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

## Install Chisel

If you need this two work instead Intel, you'll need to do the following steps once:

- Find a work disk. Set the environment variable `WORK` to its full path. Then issue:

```
mkdir $WORK/.sbt
mkdir $WORK/.ivy2
mkdir $WORK/.cache
```

- Make sure `~/.sbt`, `~/.ivy2`, and `~/.cache` are empty. Then issue:
```
rm -rf ~/.sbt
rm -rf ~/.ivy2
```
(If `~/.cache` is not empty, either remove it similarly or move the contents to `$WORK/.cache`)

- Use links in the home directory to the work disk so you don’t fill up your home directory.
```
cd ~
ln -s $WORK/.sbt .
ln -s $WORK/.ivy2 .
ln -s $WORK/.cache .
```

- Set proxies
```
setenv HTTPS_PROXY http://proxy-us.intel.com:912
setenv HTTP_PROXY http://proxy-us.intel.com:912
setenv http_proxy http://proxy-us.intel.com:912
setenv https_proxy http://proxy-us.intel.com:912

```

