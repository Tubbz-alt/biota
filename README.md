# TIIA
-----
Fortran 90 source code and an example problem for the Toolkit for Integrated Impact Assessments (TIIA).

TIIA is a suite of computer tools to simultaneously estimate the effects of inorganic, organic, and radionuclide contaminants present in a ecosystem in terms of body burdens or risk to ecological species and dose or risk to humans. Both aquatic and terrestrial species are modeled. The TIIA consists of two major modules (ECEM and HUMAN), two supporting modules for accumulation of contaminants in the environment (SOIL and RIPSAC), and a suite of 10 utility codes. Codes in the TIIA communicate through shared data files.

## Compilation
The components of the TIAA are written in standard Fortran 90. Any Fortran compiler can be used. Each component of the modeling system is provided in a separate directory. All of the components also need one or more Fortran source files in the TIIA_Common_Source directory. The README file in each directory identifies the required source files.

For example, to compile the ECEM component into an executable named `ecem.x` on a Linux-type system using the [GFortran](https://gcc.gnu.org/fortran/) compiler, assuming all of the supporting utility source files are in subdirectory named ./util, use the following compiler commands:
```
gfortran -c ./util/u-prterr.f90
gfortran -c ./util/u-chars.f90
gfortran -c ./util/u-ecda.f90
gfortran -c ./util/u-fcda-ecem.f90
gfortran -c ./util/u-fcda.f90
gfortran -c ./util/u-getunit.f90
gfortran -c ./util/u-qa.f90
gfortran -c ./util/u-rdblk.f90
gfortran -c ./util/u-sort.f90
gfortran -c ./util/u-stats.f90
gfortran -c ecem.f90
gfortran ecem.o u-prterr.o u-chars.o u-ecda.o u-fcda-ecem.o u-fcda.o u-getunit.o u-qa.o u-rdblk.o u-sort.o u-stats.o -oecem.x
```

## User Instructions
A complete set of [user instructions](./UserGuide/PNWD-4357_Rev1_TIIA_User_Guide.pdf) is provided.

## Test Case
A test case for many of the codes is provided in the [TestCase](./TestCase) directory. This test case was created for a machine running the Windows operating system. The paths in the `Run.bat` file must be edited before running the test case.

## Citations
If you would like to reference the TIAA in an academic paper, we ask you include both of the following references:
* Eslinger, Paul W., and Terri B. Miley. 2012. User Instructions for the Computer Codes of the Toolkit for Integrated Impact Assessments (TIIA), Version1, PNWD-4357 Rev. 1, Battelle Memorial Institute, Columbus, Ohio.
* TIIA, Version 1.0 [http://github.com/pnnl/Biota] (acessed Oct 2018)
