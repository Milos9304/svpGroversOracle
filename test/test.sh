#!/bin/bash

#Test Adder Circuit
./test -n 1 > adderCircuit_ascii.txt
./quipper-qasm/QasmPrinting adderCircuit_ascii.txt > adderCircuit.qasm

#Test Multiplication Circuit
./test -n 2 > multiplicationCircuit_ascii.txt
./quipper-qasm/QasmPrinting multiplicationCircuit_ascii.txt > multiplicationCircuit.qasm

#Test Subtraction Circuit
./test -n 3 > subtractionCircuit_ascii_v0.txt
./quipper-qasm/QasmPrinting subtractionCircuit_ascii_v0.txt > subtractionCircuit_v0.qasm
./test -n 4 > subtractionCircuit_ascii_v1.txt
./quipper-qasm/QasmPrinting subtractionCircuit_ascii_v1.txt > subtractionCircuit_v1.qasm
./test -n 5 > subtractionCircuit_ascii_v2.txt
./quipper-qasm/QasmPrinting subtractionCircuit_ascii_v2.txt > subtractionCircuit_v2.qasm
./test -n 6 > subtractionCircuit_ascii_v3.txt
./quipper-qasm/QasmPrinting subtractionCircuit_ascii_v3.txt > subtractionCircuit_v3.qasm

