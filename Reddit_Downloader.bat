@echo off
title Reddit Media Downloader
color 0A

echo.
echo  ========================================
echo       REDDIT MEDIA DOWNLOADER
echo  ========================================
echo.
echo  Starting application...
echo  Your browser will open automatically.
echo.

:: Find R path
set R_PATH="C:\Program Files\R\R-4.5.2\bin\Rscript.exe"

:: Check if R exists
if not exist %R_PATH% (
    echo  [ERROR] R not found!
    echo  Please make sure R is installed.
    pause
    exit /b 1
)

:: Kill any previous processes on port 8888
echo  Closing previous instances...
taskkill /F /IM "Rscript.exe" >nul 2>&1
timeout /t 2 /nobreak >nul

:: Start Shiny app (random port)
cd /d "%~dp0"
%R_PATH% -e "shiny::runApp('app.R', launch.browser=TRUE)"

echo.
echo  Application stopped
pause
