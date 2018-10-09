rem echo off
REM Run sequence for the Small case
REM   Paul W. Eslinger
REM   July 12, 2012
REM
REM ---- Create the ECDA files
mkdir ecda
D:\Fortran\TIIA\Ecda\x64\Release\ecda.exe Small_ESD.kwd
copy ecda.rpt .\ecda\ecda.rpt
del ecda.rpt
REM
REM ---- Put concentrations in the ECDA files
D:\Fortran\TIIA\FillEcda\x64\Release\fillecda.exe Small_FILLECDA.kwd
REM
REM ---- Compute soil concentrations at upland locations
D:\Fortran\TIIA\Soil\x64\Release\\soil.exe Small_SOIL.kwd
del soil.done
REM
REM ---- Compute soil concentrations at riparian locations
D:\Fortran\TIIA\RipSac\x64\Release\ripsac.exe Small_RIPSAC.kwd
del ripsac.done
REM
REM ---- Option: Convert an example binary concentration file to ASCII text
D:\Fortran\TIIA\Ecda_Ascii\x64\Release\ecda_ascii.exe <Small_ECDA_ASCII_Answers.txt
REM
REM ---- Option: Demonstrate the HIGHMEDIA code
mkdir Media
D:\Fortran\TIIA\HighMedia\x64\Release\highmedia.exe Small_HIGHMEDIA.kwd
copy Small_HIGHMEDIA.rpt .\Media\Small_HIGHMEDIA.rpt
REM
REM ---- Convert food web matrix to CONSUME keywords for Ecem
D:\Fortran\TIIA\Consume\x64\Release\consume.exe Small_Consume.Dat
REM
REM ---- Compute concentrations in food products
mkdir Foods
mkdir Ecem
D:\Fortran\TIIA\Ecem\x64\Release\ecem.exe Small_ECEM.kwd
REM
REM ---- Option: Convert an example binary food concentration file to ASCII text
D:\Fortran\TIIA\Fcda_Ascii\x64\Release\fcda_ascii.exe <Small_FCDA_ASCII_TURTLE_Answers.txt
D:\Fortran\TIIA\Fcda_Ascii\x64\Release\fcda_ascii.exe <Small_FCDA_ASCII_UEGG_Answers.txt
REM
REM ---- Option: Demonstrate the HIGHDOSE code
D:\Fortran\TIIA\HighDose\x64\Release\highdose.exe Small_HIGHDOSE.kwd
copy Small_HIGHDOSE.rpt .\Ecem\Small_HIGHDOSE.rpt
REM
REM ---- Option: Put concentrations in the Food files
D:\Fortran\TIIA\FillFcda\x64\Release\fillfcda.exe Small_FILLFCDA.kwd
REM
REM ---- Calculate human impacts
mkdir Human_RF
D:\Fortran\TIIA\Human\x64\Release\human.exe Small_HUMAN_RF.kwd
copy Small_Human_RF.rpt .\Human_RF\Small_Human_RF.rpt
del Small_Human_RF.rpt
REM
REM ---- Calculate human impacts
mkdir Human_UR
D:\Fortran\TIIA\Human\x64\Release\human.exe Small_HUMAN_UR.kwd
copy Small_Human_UR.rpt .\Human_UR\Small_Human_UR.rpt
del Small_Human_UR.rpt
REM
REM ---- Option: Demonstrate the HIGHIMPACT code
D:\Fortran\TIIA\HighImpact\x64\Release\\highimpact.exe Small_HIGHIMPACT.kwd
copy Small_HIGHIMPACT.rpt .\Human_RF\Small_HIGHIMPACT.rpt
