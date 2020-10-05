
@echo off
setlocal

:: map working network drive
net use Z: "\\fld5filer\agrim\DATA\RSGA\Yield_Model\ParcelLevel"

:: ########################################################
:: get folder containing this script
set thisFolder=%~dp0
set thisFolder=%thisFolder:~0,-1%

:: get task name
for %%f in (%thisFolder%) do set task=%%~nf

:: ########################################################
:: add R bin and Anaaconda folder to system path 
::PATH=C:\Program Files\R\R-3.5.1\bin;C:\Program Files\Anaconda;%PATH%

:: defining folder names
set codeDIR=%thisFolder%\code
set dataDIR=Z:\methodology2020\500-data\input
set outROOT=Z:\methodology2020\600-temp\%username%\%task%\output

:: make output root directory if not already exists
if not exist %outROOT%\code mkdir %outROOT%\code

:: copy this batch script to output root
copy %0 %outROOT%\code
:: copy code folder to output root
xcopy /E /I /Y %codeDIR% %outROOT%\code

:: ########################################################
:: R command
set Rversion=3.6.3
::set Rversion=4.0.2
set PATH=F:\work\software\R\instances\R-%Rversion%\bin;%PATH%
Rscript %codeDIR%\runall.R %dataDIR% %codeDIR% %outROOT% 1> %outROOT%\stdout.R.runall 2> %outROOT%\stderr.R.runall

:: unmap network drives
net use Z: /delete /yes
