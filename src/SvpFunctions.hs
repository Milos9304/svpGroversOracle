-- @Milos Prokop, 2024, free to use and modify
--
-- implements svpOracle
--      which generates the circuit for the grover's oracle for SVP and is used in the paper to estimate the quantum resource requirements
--
-- implements adderCircuit and multiplicationCircuit which are builing blocks of svpOracle
--
-- implements testAdderCircuit, testMultiplicationCircuit and testSubtractionCircuit that output (2-bit input) representations of the circuits used
--
-- other implemented functions are the helper functions and their role is explained

{-# LANGUAGE BlockArguments #-}

module SvpFunctions(
  svpOracle,
  adderCircuit,
  testAdderCircuit,
  testMultiplicationCircuit,
  testSubtractionCircuit
) where

import Quipper
import Quipper.Libraries.Arith
import Quipper.Utils.Auxiliary
import Control.Monad (foldM, zipWithM, replicateM)
import Quipper.Libraries.Qureg

import System.Random

import Quipper.Internal.Monad
import Quipper.Internal.Control

-- wrapper for a loop used many times in the implementations below
foreach2:: Monad m => [a] -> (a -> m b) -> m [b]
foreach2 l f = mapM f l

-- use quipper implementation of in-place adder
adderCircuit :: QDInt -> QDInt -> Circ (QDInt, QDInt)
adderCircuit a b = q_add_in_place a b

-- | Low-level implementation of 'q_add_in_place': represents integers
-- as big-headian qubit lists.
q_add_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_add_in_place_qulist [] [] = return ([], [])
q_add_in_place_qulist [x0] [y0] = do
  y0 <- qnot y0 `controlled` x0
  return ([x0], [y0])
q_add_in_place_qulist x y = do
  let (x0:x_higher) = reverse x  
      (y0:y_higher) = reverse y

  y0 <- qnot y0 `controlled` x0

  ((x0,y0),(x_higher,y_higher)) <- with_computed_fun (x0,y0)
    (\(x0,y0) -> do
      c <- qinit False
      c <- qnot c `controlled` (x0 .==. 1) .&&. (y0 .==. 0)
      return (x0,y0,c))
    (\(x0,y0,c) -> do
      (x_higher,y_higher,c) <- q_add_aux (x_higher) (y_higher) c
      return ((x0,y0,c),(x_higher,y_higher)))
  return (reverse (x0:x_higher), reverse (y0:y_higher))
  where
  -- Aux: add two LITTLE-endian bit strings, and an optional extra 1.
    q_add_aux :: [Qubit] -> [Qubit] -> Qubit -> Circ ([Qubit],[Qubit],Qubit)
    q_add_aux [] [] c = return ([],[],c)
    q_add_aux [x0] [y0] c = do
      y0 <- qnot y0 `controlled` x0
      y0 <- qnot y0 `controlled` c
      return ([x0],[y0],c)
    q_add_aux (x0:xs) (y0:ys) c = do
      y0 <- qnot y0 `controlled` x0
      y0 <- qnot y0 `controlled` c
      ((x0,y0,c),(xs,ys)) <- with_computed_fun (x0,y0,c)
        (\(x0,y0,c) -> do
          c' <- qinit False
          c' <- qnot c' `controlled` (x0 .==. 1) .&&. (y0 .==. 0)
          c' <- qnot c' `controlled` (x0 .==. 1) .&&. (c .==. 1)
          c' <- qnot c' `controlled` (y0 .==. 0) .&&. (c .==. 1)
          return (x0,y0,c,c'))
        
        (\(x0,y0,c,c') -> do
          (xs,ys,c') <- q_add_aux xs ys c'
          return ((x0,y0,c,c'),(xs,ys)))
      return (x0:xs,y0:ys,c)
    q_add_aux _ _ _ = error "q_add_in_place: cannot add integers of different sizes."

-- | Multiply two 'QDInt's into a fresh third.  Arguments are assumed to be of equal size.  /O/(#[sup 2]).
q_mult_qdint2 :: QDInt -> QDInt -> Circ (QDInt,QDInt,QDInt)
q_mult_qdint2 x y = do
  let x' = list_of_xint_bh x
  let y' = list_of_xint_bh y
  (x', y', z') <- q_mult_qulist2 x' y'
  let x = xint_of_list_bh x'
  let y = xint_of_list_bh y'
  let z = xint_of_list_bh z'
  return (x, y, z)

-- | Low-level implementation of 'q_mult_qdint': represents integers as
-- big-headian qubit lists.
q_mult_qulist2 :: [Qubit] -> [Qubit] -> Circ ([Qubit],[Qubit],[Qubit])
q_mult_qulist2 [] [] = return ([], [], [])
q_mult_qulist2 xs ys = do
  let x0 = last xs 
      x_higher = init xs
      y_high = head ys
      y_lower = tail ys
  (x_higher, y_lower, p_higher)
    <- q_mult_qulist2 x_higher y_lower
  p0 <- qinit False
  let y = y_high:y_lower
      p = p_higher ++ [p0]
  (y,p) <- q_add_in_place_qulist y p `controlled` x0
  return (x_higher ++ [x0], y, p)

--create multiplication circuit from the explicit formula
multiplicationCircuit :: QDInt -> QDInt -> Circ (QDInt, QDInt, QDInt)
multiplicationCircuit a b = q_mult_qdint2 a b

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- constructs the SVP oracle for Grover's algorithm (without uncomputation), resources for uncomputation are retrieved by calculating the uncomputation of the constructed oracle
-- n,m: lattice basis dimensions
-- num_bits_per_b: numer of qubits used per dimension. default: if n=m, use log2(n) qubits per dimension
-- the function is documented with reference to steps outlined in Section V of Grover’s oracle for the Shortest Vector Problem and its application in hybrid classical-quantum solvers in A) ANALYTICAL ANALYSIS OF ORACLE RESOURCE REQUIREMENTS

svpOracle :: Int -> Int -> Int -> Circ ()--Circ ([QDInt])
svpOracle n m num_bits_per_b = do
  comment "ENTER: svpOracle"
  --define the enumeration coefficients and their bounds [-lbs+1,lbs]
  let b = map (\x -> num_bits_per_b) (replicate n 0)
      lbs = map (\x -> (-2^(x-1) + 1)) b

  --init qubits that are used as an input of the n times m lattice basis B
  lattice_qs2 <- foreach2 [0,1..n] $ \y -> do
    res <- foreach2 [0,1..m] $ \x -> do
        a <- qinit $ intm (num_bits_per_b+1) 0 -- +1 to hold the query
        label a $ "B" ++ (show x) ++ (show y)
        return a
    endfor
    return res
  endfor

  shifted_enum_coeffs <- foreach2 (zip (zip (zip b lbs) lattice_qs2) [0,1..]) $ \(((bound, alpha), bRow),index) -> do
    -- each enumeration coefficient in the interval [0,2*lb] is to be combined with a constant c so that its value becomes [-lb+1,lb]. Step 1. 
    c <- qinit $ intm (bound+1) 0 -- +1 is to hold the carry 
    label c $ "c_" ++ show index 
    let cs = c : cs

    lb <- qinit $ intm (bound+1) (-alpha)
    label lb $ "lb_" ++ show index
    
    (res1,res2) <- adderCircuit c lb --use adder to shift the interval of enumeration coefficient. Step 1.
    comment $ show $ (head bRow)
    k <- foreach2 (zip bRow [0,1..]) $ \(x,i) -> do
        (r1,r2,r3) <- multiplicationCircuit res2 x --then multiply each enumeration coefficient with the row of the lattice basis matrix it corresponds to. Step 2.
        label r3 "R3"
        return r3
    endfor
   
    return (k)
  endfor

  vect_elems <-foreach2 (transpose shifted_enum_coeffs) $ \k_row -> do
    vect_elems_nonsquared <- foreach2 (zip (init k_row) (tail k_row)) $ \(a,b) -> do
        (a,b) <- adderCircuit a b --add the multiplied rows by their corresponding enumeration coefficients row-wise. Step 3.
        return b
    endfor
    (_,_,res) <- multiplicationCircuit (last vect_elems_nonsquared) (last vect_elems_nonsquared) --given the vector that is the result of adding the multiplied rows with their corresponding enumeration coefficients row-wise, take square of each of its element to proceed with calculation of the Euclidean norm. Step 4.
    label res "multRes"
    return res
  endfor

  final <- foreach2 (zip (init vect_elems) (tail vect_elems)) $ \(a,b) -> do
    (_,r) <-adderCircuit a b --sum the squared vector elements. the squared length of the resulting vector is calculated. Step 5.
    return r
  endfor

  label (last final) "OUTPUT"
  q_sub_param_in_place 1000 (last final) --subtract a constant to decide if the vector is below a threshold to be considered as a solution. Step 6.

  comment "EXIT: algorithm_R"

-- tests adding two 2-bit integers
testAdderCircuit :: Circ ()
testAdderCircuit = do
  comment "testAdderCircuit\n"
  let b = map (\x -> 1) (replicate 2 0)
      lbs = map (\x -> (-2^(x-1) + 1)) b

  lattice_qs2 <- foreach2 [0,1] $ \y -> do
    res <- foreach2 [0] $ \x -> do
        a <- qinit $ intm (2) 0 -- +1 to hold the query
        label a $ "B" ++ (show x) ++ (show y)
        return a
    endfor
    return res
  endfor

  (res1,res2) <- adderCircuit (head $ head lattice_qs2) (head $ head $ tail lattice_qs2)
  label res1 "res a"
  label res2 "res b"

-- tests multiplication of two 2-bit integers
testMultiplicationCircuit :: Circ ()
testMultiplicationCircuit = do
  comment "testMultiplicationCircuit\n"
  let b = map (\x -> 1) (replicate 2 0)
      lbs = map (\x -> (-2^(x-1) + 1)) b

  lattice_qs2 <- foreach2 [0,1] $ \y -> do
    res <- foreach2 [0] $ \x -> do
        a <- qinit $ intm (2) 0 -- +1 to hold the query
        label a $ "B" ++ (show x) ++ (show y)
        return a
    endfor
    return res
  endfor

  (_,_,res) <- multiplicationCircuit (head $ head lattice_qs2) (head $ head $ tail lattice_qs2)
  label res "res"

-- tests subtraction in place by a constant n
-- for this test n must be a 2-bit integer
testSubtractionCircuit :: Int -> Circ ()
testSubtractionCircuit n = do
  comment "testSubtrationCircuit\n"
  let b = map (\x -> 1) (replicate 2 0)
      lbs = map (\x -> (-2^(x-1) + 1)) b

  lattice_qs2 <- foreach2 [0] $ \y -> do
    res <- foreach2 [0] $ \x -> do
        a <- qinit $ intm (2) 0 -- +1 to hold the query
        label a $ "B" ++ (show x) ++ (show y)
        return a
    endfor
    return res
  endfor
  
  res <- q_sub_param_in_place (intm 2 (toInteger n)) (head $ head lattice_qs2)
  label res "res a"
