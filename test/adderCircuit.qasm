OPENQASM 2.0;
include "qelib1.inc";


//// testAdderCircuit
qreg anc0[1];
qreg anc1[1];
////  anc0[0],anc1[0];
qreg anc2[1];
qreg anc3[1];
////  anc2[0],anc3[0];
cx anc1[0],anc3[0];
qreg anc4[1];
x anc3[0];
ccx anc1[0],anc3[0],anc4[0];
x anc3[0];
cx anc0[0],anc2[0];
cx anc4[0],anc2[0];
x anc3[0];
ccx anc1[0],anc3[0],anc4[0];
x anc3[0];
reset anc4[0];
////  anc0[0],anc1[0];
////  anc2[0],anc3[0];

