module Simulator(
    test_two_input_circ
)
where

import Quipper

type AdderType = [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])

--g <- newStdGen
--print $ run_generic g(0.0::Double) (test_two_input_circ qft_add_in_place) ([[False,False,False],[False,False,False]])

test_two_input_circ :: AdderType -> [[Qubit]] -> Circ ([Qubit], [Qubit])
test_two_input_circ circ qs | length qs == 2 = circ (head qs) (head(tail qs))
                            | otherwise = error "exactly 2 input lists expected"
