OPENQASM 2.0;
include "qelib1.inc";


//// testMultiplicationCircuit
qreg anc0[1];
qreg anc1[1];
////  anc0[0],anc1[0];
qreg anc2[1];
qreg anc3[1];
////  anc2[0],anc3[0];
qreg anc4[1];
ccx anc0[0],anc3[0],anc4[0];
qreg anc5[1];
ccx anc1[0],anc3[0],anc5[0];
qreg anc6[1];
x anc5[0];
ccx anc3[0],anc5[0],anc6[0];
x anc5[0];
ccx anc1[0],anc2[0],anc4[0];
ccx anc1[0],anc6[0],anc4[0];
x anc5[0];
ccx anc3[0],anc5[0],anc6[0];
x anc5[0];
reset anc6[0];
////  anc4[0],anc5[0];

