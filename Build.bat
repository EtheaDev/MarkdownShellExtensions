call "C:\BDS\Studio\23.0\bin\rsvars.bat"
msbuild.exe "Source\MDShellExtensions.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\MDShellExtensions32.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release
msbuild.exe "Source\MDTextEditor.dproj" /target:Clean;Build /p:Platform=Win64 /p:config=release
msbuild.exe "Source\MDTextEditor.dproj" /target:Clean;Build /p:Platform=Win32 /p:config=release

call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkdownShellExtensions\Bin32\MDTextEditor.exe
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkdownShellExtensions\Bin64\MDTextEditor.exe

:INNO
"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "D:\ETHEA\MarkdownShellExtensions\Setup\MDShellExtensions.iss"
set INNO_STATUS=%ERRORLEVEL%
if %INNO_STATUS%==0 GOTO SIGNSETUP
pause
EXIT

:SIGNSETUP
call D:\ETHEA\Certificate\SignFileWithSectico.bat D:\ETHEA\MarkdownShellExtensions\Setup\Output\MDShellExtensionsSetup.exe

:END
pause
