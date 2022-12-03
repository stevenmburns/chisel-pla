# Chisel Examples

A number of examples showing of Chisel's advantanges as a Hardware Construction Language.

## Quick Start

To run the tests:
```
sbt test
```

To print out the generated verilog:
```
sbt run
```
and interactively chose the driver to run. I currently get this in response to `sbt run`:
```
Multiple main classes detected. Select one to run:
 [1] GCD.BinaryGCDDriver
 [2] GCD.BinaryGCDNoBigShifterDriver
 [3] GCD.BinaryGCDSimpleDriver
 [4] GCD.EuclidGCDDriver
 [5] alu.MainAlu
 [6] alu.MainAluMMX
 [7] med.CellArray1616Driver
 [8] med.CellArray33Driver
 [9] med.CellArray44Driver
 [10] med.CellArray55Driver
 [11] med.CellArrayRetimed1616Driver
 [12] med.CellArrayRetimed44Driver
 [13] pla.MainMaj
 [14] pla.MainXor

Enter number: 
```
Enter "6" (without the quotes) to see the verilog for the optimized ALU.


