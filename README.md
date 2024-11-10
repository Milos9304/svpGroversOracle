# svpGroversOracle

Supplementary code to Grover's oracle for the Shortest Vector Problem and its application in hybrid classical-quantum solvers by Milos Prokop, Petros Wallden and David Joseph

https://doi.org/10.48550/arXiv.2402.13895

Builds a circuit for Grover's oracle for the Shortest Vector Problem and calculates the needed quantum resources.

# Requirements
1. **Qiskit**: An open-source quantum computing framework.  
   - **Install**: Run `pip install qiskit` (requires Python 3.8 or later).  
   - **Verify**: `python -c "import qiskit; print('Qiskit version:', qiskit.__version__)"`

2. **GHC (Glasgow Haskell Compiler)**: The primary Haskell compiler.  
   - **Install**: Use a package manager, e.g., `sudo apt-get install ghc` on Debian/Ubuntu, or `brew install ghc` on macOS.  
   - **Verify**: Run `ghc --version` to check the installation.

3. **Quipper**: A quantum programming language for circuit generation.  
   - **Install**: Clone and build from source:
     ```bash
     git clone https://github.com/thephoeron/quipper-language
     cd Quipper
     make
     ```
   - **Verify**: Run `quipper --version` (assuming the binary is in your PATH).

---

# Build and compile
```console
cd src
quipper svp.hs
cd ..
```

This generates executable **svp** inside folder __src/__

# Run options

Use -h option to see the argument list
```console
./src/svp -h
```
outputs

```
Usage: svp [OPTION...]
  -h             --help                 print usage info and exit
  -f <format>    --format=<format>      output format for circuits (default: ascii)
  -g <gatebase>  --gatebase=<gatebase>  type of gates to decompose into (default: logical)
  -n <n>         --n=<n>                parameter n (default: 2)
  -m <m>         --m=<m>                parameter m (default: 2)
  -b <b>         --b=<b>                parameter b (default: [4,4]) = n-length list of bounds
Possible values for format are: eps, pdf, ps, postscript, ascii, preview, gatecount.
Possible values for gatebase are: logical, binary, toffoli, cliffordt_old, cliffordt, cliffordt_keepphase, standard, strict, approximate, approximate_keepphase, exact, trimcontrols.
```

## Running the experiments 

If ./extract_resource_counts.sh is not executable, run
```console
chmod +x extract_resource_counts.sh
```

Run
```console
./extract_resource_counts.sh start_dim finish_dim qubits_per_coeff
```
where resources are calculated for SVPs of dimensions from *start_dim* to *finish_dim* where *qubits_per_coeff* qubits per enumeration coefficients are used.

## To test
Because even the smallest SVP instance of dimension 2 results in 29 qubit requirement, the functionality can be tested only piece-wise, i.e. the test is performed if all building blocks of the oracle specified in **src/SvpFunctions.hs** work as expected. The tests are for the 2 qubit input operands.

If ./test/test.sh is not executable, run
```console
chmod +x test/test.sh
```
Next compile quipper-qasm:

```console
cd test/quipper-qasm
ghc QasmPrinting.hs
cd ../..
```

This generates executable QasmPrinting inside folder test/quipper-qasm

```console
cd test/
quipper test.hs
```

This generates executable **test** inside folder _test/_

Run
```console
./test.sh
```

This produces QASM representations of adder, multiplication and subtraction circuits: adderCircuit.qasm, multiplicationCircuit.qasm, subtractionCircuit_v{0,1,2,3}.qasm
These files are loaded by **test_circuits.py** and are also included in repo in case the above instructions are problematic to execute:

```console
python test_circuits.py
```
which tests for all possible inputs to the arithmetic circuits and validates the outputs. If everything is correct, the output writes

```console
adderCircuit test succesful
multiplicationCircuit test succesful
subtratctionCircuit test with constant 0 succesful
subtratctionCircuit test with constant 1 succesful
subtratctionCircuit test with constant 2 succesful
subtratctionCircuit test with constant 3 succesful
```

# License

This project is licensed under the GNU General Public License v2.0 - see the [LICENSE](LICENSE) file for details.
