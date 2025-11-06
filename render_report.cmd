@echo off
setlocal
set SCRIPT=%~dp0render_report.ps1

if not exist "%SCRIPT%" (
  echo Cannot find render_report.ps1 next to this script.
  exit /b 1
)

REM Forward all args to PowerShell script with ExecutionPolicy bypass
powershell -NoProfile -ExecutionPolicy Bypass -File "%SCRIPT%" %*
exit /b %ERRORLEVEL%

