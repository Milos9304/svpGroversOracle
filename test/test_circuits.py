# @Milos Prokop, 2024, free to use and modify

from qiskit import QuantumCircuit, Aer, execute, ClassicalRegister
import sys

# testAdderCircuit
def testAdderCircuit():
    
    qc_adder = QuantumCircuit.from_qasm_file("adderCircuit.qasm")

    for a in range(4):
        for b in range(4):
           
            init_bin = list('00000')
            init_bin[2]=format(b, f'0{2}b')[0]
            init_bin[1]=format(b, f'0{2}b')[1]

            init_bin[4]=format(a, f'0{2}b')[0]
            init_bin[3]=format(a, f'0{2}b')[1]

            init_bin = ''.join(init_bin)
            #print(init_bin)
            
            qc = QuantumCircuit(qc_adder.num_qubits, qc_adder.num_qubits)
            qc.initialize(init_bin)

            qc.append(qc_adder, range(qc_adder.num_qubits))

            qc.barrier()

            qc.measure(0,0)
            qc.measure(1,1)
            qc.measure(2,2)
            qc.measure(3,3)
            qc.measure(4,4)

            #print(qc.decompose())

            # Run the circuit on the Qiskit Aer simulator
            simulator = Aer.get_backend('qasm_simulator')
            result = execute(qc, simulator, shots=1).result()

            # Retrieve and print the counts of measurements, focusing on the last qubit
            #print(result)
            counts = result.get_counts(qc)
            res=list(counts)[0][1:3][::-1]
            #print(a, "+", b, "=", int(res,2),"  ",counts)
            assert(int(format(a+b, f'0{3}b')[1:],2) == int(res,2))

    print("adderCircuit test succesful")

# testMultiplicationCircuit

def testMultiplicationCircuit():
    
    qc_mult = QuantumCircuit.from_qasm_file("multiplicationCircuit.qasm")

    for a in range(4):
        for b in range(4):
           
            init_bin = list('0000000')
            init_bin[4]=format(b, f'0{2}b')[0]
            init_bin[3]=format(b, f'0{2}b')[1]

            init_bin[6]=format(a, f'0{2}b')[0]
            init_bin[5]=format(a, f'0{2}b')[1]

            init_bin = ''.join(init_bin)
            #print(init_bin)
            
            qc = QuantumCircuit(qc_mult.num_qubits, qc_mult.num_qubits)
            qc.initialize(init_bin)

            qc.append(qc_mult, range(qc_mult.num_qubits))

            qc.barrier()

            qc.measure(0,0)
            qc.measure(1,1)
            qc.measure(2,2)
            qc.measure(3,3)
            qc.measure(4,4)
            qc.measure(5,5)
            qc.measure(6,6)

            #print(qc.decompose())

            # Run the circuit on the Qiskit Aer simulator
            simulator = Aer.get_backend('qasm_simulator')
            result = execute(qc, simulator, shots=1).result()

            # Retrieve and print the counts of measurements, focusing on the last qubit
            #print(result)
            counts = result.get_counts(qc)
            res=list(counts)[0][1:3][::-1]
            #print(a, "*", b, "=", int(res,2),"  ", counts)
            assert(int(format(a*b, f'0{3}b')[1:],2) == int(res,2))

    print("multiplicationCircuit test succesful")

# testSubtractionCircuit
# in place subtraction of a constant 1

def testSubtractionCircuit(v):
    
    qc_subtraction = QuantumCircuit.from_qasm_file("subtractionCircuit_v"+str(v)+".qasm")

    for a in range(4):
            
        init_bin = list('000')
        init_bin[2]=format(a, f'0{2}b')[0]
        init_bin[1]=format(a, f'0{2}b')[1]

        init_bin = ''.join(init_bin)
        #print(init_bin)
        
        qc = QuantumCircuit(qc_subtraction.num_qubits, qc_subtraction.num_qubits)
        qc.initialize(init_bin)

        qc.append(qc_subtraction, range(qc_subtraction.num_qubits))

        qc.barrier()

        qc.measure(0,0)
        qc.measure(1,1)
        qc.measure(2,2)

        #print(qc.decompose())

        # Run the circuit on the Qiskit Aer simulator
        simulator = Aer.get_backend('qasm_simulator')
        result = execute(qc, simulator, shots=1).result()

        # Retrieve and print the counts of measurements, focusing on the last qubit
        #print(result)
        counts = result.get_counts(qc)
        res=list(counts)[0][1:3][::-1]
        assert((a-v) % 4 == int(res,2))

    print("subtratctionCircuit test with constant", v, "succesful")


def main():
    testAdderCircuit() 
    testMultiplicationCircuit()

    # Test 4 different constants
    for v in range(4):
        testSubtractionCircuit(v)

if __name__ == '__main__':
    main()

