call "C:\BDS\Studio\21.0\bin\rsvars.bat"
msbuild.exe "Source\MDShellExtensions.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\MDShellExtensions32.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
msbuild.exe "Source\MDTextEditor.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\MDTextEditor.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release

:INNO
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\MDShellExtensions\Setup\MDShellExtensions.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO END
pause
EXIT

:END
pause
