
@echo off
setlocal

:: map working network drive
net use Z: "\\fld6filer\DScD\Data Science Hub\DSCoE\300-projects\2020-2021\010-crop-yield"

:: ########################################################
:: get folder containing this script
set thisFolder=%~dp0
set thisFolder=%thisFolder:~0,-1%

:: get task name
for %%f in (%thisFolder%) do set task=%%~nf

:: ########################################################
:: defining folder names
:: set codeDIR=%thisFolder%\code
set codeDIR=..\101-package-stcCropYield\code
set dataDIR=Z:\500-data\input
set outROOT=Z:\600-temp\%username%\%task%\output

:: make output root directory if not already exists
if not exist %outROOT%\code mkdir %outROOT%\code

:: copy this batch script to output root
copy %0 %outROOT%\code
:: copy code folder to output root
xcopy /E /I /Y %codeDIR% %outROOT%\code

:: ########################################################
:: R command
set PATH=F:\work\software\R\instances\R-3.6.2\bin;C:\Program Files\Anaconda;%PATH%
Rscript %codeDIR%\main-make-output-rwFV.R %dataDIR% %codeDIR% %outROOT% 1> %outROOT%\stdout.R.main-make-output-rwFV 2> %outROOT%\stderr.R.main-make-output-rwFV

:: unmap network drives
net use Z: /delete /yes
