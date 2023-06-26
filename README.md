# Markdown Editor and Shell Extensions [![License](https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg)](https://opensource.org/licenses/Apache-2.0)

**Latest Version 1.5.1 - 26 Jun 2023**

**A collection of tools for markdown files, to edit and view content:**

- A **Markdown Text Editor**[2] to manually edit a markdown file, with instant preview of the output in a HTML Viewer.

- A **Preview handler**[1] which allows you to see the content of the markdown file without open it, in the "Preview Panel", integrated into Windows Explorer.

### Features

- No need to built the project with Delphi: an easy Setup is provided.

- Supports Windows Vista, 7, 8, 10 and 11 (for 32 bits and 64 bits).

- Themes (Dark and Light) according to user preferences of Windows Theme

- Integrated also with other Shell Applications (like XYplorer)

### Delphi: integration with MarkDown Help Viewer

- For Delphi developers: use this tool to prepare and integrate a Help System in your application, using the ["MarkDown Help Viewer"](https://github.com/EtheaDev/MarkdownHelpViewer) project.
- Use the **Markdown Text Editor** to edit your help files and use the **Markdown Help Viewer** to show them.

### Setup using the Installer

Click to download the [MDShellExtensionsSetup.exe][3] located also in the Release area. The Installer works both for 32 and 64 bit system.

![Markdown Setup_Program](./Images/Setup.png)

***For a clean Setup close all the windows explorer instances which have the preview handler active or the preview handler was used (remember the dll remains in memory until the windows explorer was closed).***

### Preview Panel in action ###

In Windows 10 with Dark theme:

![Preview Panel Dark](./Images/PreviewPanelDark.png)

### Markdown Text Editor

A useful Text editor with instant preview of Markdown formatted content:

![Markdown Text Editor Dark](./Images/MDTextEditorLight.png)

### Manual Build and Installation (only for Delphi developers) ###

If you have Delphi 10.4 Sydney or 11.3 Alexandria, you can manually build the project:

***Warning: To simplyfy build of the projects, some third parties libraries are located into ext folder***

To manually install the SVGShellExtensions.dll follow these steps:

1. Close all the windows explorer instances which have the preview handler active or the preview handler was used (remember the dll remains in memory until the windows explorer was closed).
  
2. If you have already used the installer uninstall the components from system.
     
3. To install manually the dll run the `Unregister_Register.cmd` (run-as-administrator): notice that you must change the path into cmd file.

## Release Notes ##

26 Jun 2023: ver. 1.5.1
- Fixed Preview of Markdown files with extensions different from .markdown

24 Jun 2023: ver. 1.5.0
- Autoload local markdown files when clicked into Preview
- The editor can open all markdown extensions: .md, .mkd, .mdwn, .mdown, .mdtxt, .mdtext, .markdown, .txt, .text'
- Stopped image rendering during editing to speed-up
- Added useful close button on Tabs
- Hint full filename on Tabs
- Save/Discard messaged default response inverted
- Removed Settings "Search In Folder"

1 Nov 2022: ver. 1.4.3
- Fixed loading images into Preview
- Fixed the setting for local loading

11 Set 2022: ver. 1.4.1
- Added Combobox for easy selection Markdown Dialect
- Added Windows 11 light and dark styles
- Fixed load file with blanks in content menu
- Fixed loading ANSI files
- Fixed AV in Settings with opened files

20 Jan 2022: ver. 1.3.0
- Added Support for Windows 11
- Fixed resize content

04 Sep 2021: ver. 1.2.0
- Added Support for Delphi 11
- Updated Image32 Lib

20 Jul 2021: ver. 1.1.0
- Added Image32 Lib to best rendering of SVG Images

06 Jul 2021
- First public release with setup

## Credits

Many thanks to **Rodrigo Ruz V.** (author of [theroadtodelphi.com][7] Blog) for his wonderful work on [delphi-preview-handler][8] from which this project has used a lot of code and inspiration.

## License

Licensed under the [Apache License, Version 2.0][9] (the "License");
Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

# External projects

***This project uses some External Projects, located into Ext Path***

- [SVGIconImageList]

- [Synedit]

- [VCL-Style-Utils]

- [delphi-markdown]

  ***To simpilfy compilation of projects they are added into ext folder***

1: https://github.com/EtheaDev/SVGIconImageList

2: https://github.com/SynEdit/SynEdit

3: https://github.com/RRUZ/vcl-styles-utils

4: https://github.com/grahamegrieve/delphi-markdown

![Delphi 11 Alexandria Support](/Setup/SupportingDelphi.jpg)

Related links: [embarcadero.com][10] - [learndelphi.org][11]

[1]: https://docs.microsoft.com/en-us/windows/win32/shell/preview-handlers

[2]: https://github.com/EtheaDev/MarkdownShellExtensions/

[3]: https://github.com/EtheaDev/MarkdownShellExtensions/releases/latest/download/MDShellExtensionsSetup.exe

[4]: https://github.com/EtheaDev/SVGIconImageList

[5]: https://github.com/SynEdit/SynEdit

[6]: https://github.com/RRUZ/vcl-styles-utils

[7]: https://theroadtodelphi.com/

[8]: https://github.com/RRUZ/delphi-preview-handler

[9]: https://opensource.org/licenses/Apache-2.0

[10]: https://www.embarcadero.com/

[11]: https://learndelphi.org/
