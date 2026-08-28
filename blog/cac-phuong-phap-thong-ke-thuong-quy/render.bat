@echo off

rem Set the path to the Rscript executable
set RSCRIPT="C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

rem Set the path to the R script to execute
set RSCRIPT_FILEB="E:\GITHUB\vedothi\RENDER_MASTER.R"

rem Execute the R script
%RSCRIPT% %RSCRIPT_FILEB%

rem Pause so the user can see the output
exit