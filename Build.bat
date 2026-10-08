@echo off
setlocal

REM Works out whether this batch was started from Explorer, in which case FROM_EXPLORER is set to 1 instead of 0.
set FROM_EXPLORER=0
echo %cmdcmdline% | find /i "%~nx0" > NUL
if not errorlevel 1 (
  REM This batch was started by a program and not from a command prompt window
  echo %cmdcmdline% | find /i "call " > NUL
  if errorlevel 1 (
    REM This batch was started from Explorer and not from another batch or PowerShell script
    set FROM_EXPLORER=1
  )
)

if "%FROM_EXPLORER%" == "1" (
  echo.
  echo ###############################################################################
  echo Building Markdown Editor and Shell Extensions ^(Win32 and Win64^) with Delphi 13 ^(Config=Release^)
  echo ###############################################################################
  echo.
  echo Press ENTER to continue, or close this window to abort...
  pause > NUL
)

set BDS=C:\BDS\Studio\37.0
if exist "c:\program files (x86)\embarcadero\studio\37.0" set BDS=c:\program files (x86)\embarcadero\studio\37.0

if not exist "%BDS%\bin\rsvars.bat" (
  echo.
  echo ERROR: Delphi 13 not found
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

call "%BDS%\bin\rsvars.bat"

echo.
echo **************************************************************************
echo Building MDShellExtensions ^(Delphi13/Win64/Release^)
echo **************************************************************************
echo.
msbuild.exe "Source\MDShellExtensions.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=Release /v:minimal /p:DCC_Hints=false
if errorlevel 1 (
  echo.
  echo ERROR building MDShellExtensions ^(Delphi13/Win64/Release^)
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo **************************************************************************
echo Building MDShellExtensions32 ^(Delphi13/Win32/Release^)
echo **************************************************************************
echo.
msbuild.exe "Source\MDShellExtensions32.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=Release /v:minimal /p:DCC_Hints=false
if errorlevel 1 (
  echo.
  echo ERROR building MDShellExtensions32 ^(Delphi13/Win32/Release^)
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo **************************************************************************
echo Building MDTextEditor ^(Delphi13/Win64/Release^)
echo **************************************************************************
echo.
msbuild.exe "Source\MDTextEditor.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=Release /v:minimal /p:DCC_Hints=false
if errorlevel 1 (
  echo.
  echo ERROR building MDTextEditor ^(Delphi13/Win64/Release^)
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo **************************************************************************
echo Building MDTextEditor ^(Delphi13/Win32/Release^)
echo **************************************************************************
echo.
msbuild.exe "Source\MDTextEditor.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=Release /v:minimal /p:DCC_Hints=false
if errorlevel 1 (
  echo.
  echo ERROR building MDTextEditor ^(Delphi13/Win32/Release^)
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo Adding the digital signature to: D:\ETHEA\MarkDownShellExtensions\Bin64\MDTextEditor.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkDownShellExtensions\Bin64\MDTextEditor.exe
if errorlevel 1 (
  echo.
  echo ERROR signing: D:\ETHEA\MarkDownShellExtensions\Bin64\MDTextEditor.exe
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to continue...
    pause > NUL
  )
)

echo.
echo Adding the digital signature to: D:\ETHEA\MarkDownShellExtensions\Bin32\MDTextEditor.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkDownShellExtensions\Bin32\MDTextEditor.exe
if errorlevel 1 (
  echo.
  echo ERROR signing: D:\ETHEA\MarkDownShellExtensions\Bin32\MDTextEditor.exe
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to continue...
    pause > NUL
  )
)

echo.
echo **********************************************************************************************
echo Building "D:\ETHEA\MarkDownShellExtensions\Setup\MDShellExtensions.iss" with InnoSetup
echo **********************************************************************************************
echo.
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\MarkDownShellExtensions\Setup\MDShellExtensions.iss"
if errorlevel 1 (
  echo.
  echo ERROR building "D:\ETHEA\MarkDownShellExtensions\Setup\MDShellExtensions.iss" with InnoSetup
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to exit...
    pause > NUL
  )
  exit /b 1
)

echo.
echo Adding the digital signature to: D:\ETHEA\MarkDownShellExtensions\Setup\Output\MDShellExtensionsSetup.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkDownShellExtensions\Setup\Output\MDShellExtensionsSetup.exe
if errorlevel 1 (
  echo.
  echo ERROR signing: D:\ETHEA\MarkDownShellExtensions\Setup\Output\MDShellExtensionsSetup.exe
  if "%FROM_EXPLORER%" == "1" (
    echo.
    echo Press ENTER to continue...
    pause > NUL
  )
)

echo.
echo Done.
if "%FROM_EXPLORER%" == "1" (
  echo.
  echo Press ENTER to exit...
  pause > NUL
)
exit /b 0
