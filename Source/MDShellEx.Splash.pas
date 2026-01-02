{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2026 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{                                                                              }
{       https://github.com/EtheaDev/MarkdownShellExtensions                    }
{                                                                              }
{******************************************************************************}
{                                                                              }
{  Licensed under the Apache License, Version 2.0 (the "License");             }
{  you may not use this file except in compliance with the License.            }
{  You may obtain a copy of the License at                                     }
{                                                                              }
{      http://www.apache.org/licenses/LICENSE-2.0                              }
{                                                                              }
{  Unless required by applicable law or agreed to in writing, software         }
{  distributed under the License is distributed on an "AS IS" BASIS,           }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    }
{  See the License for the specific language governing permissions and         }
{  limitations under the License.                                              }
{                                                                              }
{******************************************************************************}
unit MDShellEx.Splash;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls, jpeg, SVGIconImage, Vcl.Imaging.pngimage;

type
  TSplashForm = class(TForm)
    Image1: TImage;
    SVGIconImage: TSVGIconImage;
    lbVersion: TLabel;
    lbLoad: TLabel;
    procedure FormShow(Sender: TObject);
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.dfm}

uses
  MDShellEx.About;

procedure TSplashForm.FormShow(Sender: TObject);
begin
  lbVersion.Caption := GetCurrentVersion(Application.ExeName);
end;

end.
