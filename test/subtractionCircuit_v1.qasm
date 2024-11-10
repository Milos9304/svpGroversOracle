OPENQASM 2.0;
include "qelib1.inc";


//// testSubtrationCircuit
qreg anc0[1];
qreg anc1[1];
////  anc0[0],anc1[0];
x anc1[0];
qreg anc2[1];
x anc1[0];
cx anc1[0],anc2[0];
x anc1[0];
x anc0[0];
cx anc2[0],anc0[0];
x anc1[0];
cx anc1[0],anc2[0];
x anc1[0];
reset anc2[0];
////  anc0[0],anc1[0];

