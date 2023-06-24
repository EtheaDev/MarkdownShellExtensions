{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021-2023 (Ethea S.r.l.)                                 }
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
{                                                                              }
{  The Initial Developer of the Original Code is Rodrigo Ruz V.                }
{  Portions created by Rodrigo Ruz V. are Copyright 2011-2021 Rodrigo Ruz V.   }
{  All Rights Reserved.                                                        }
{******************************************************************************}
unit MDShellEx.PreviewHandler;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  SysUtils,
  uCommonPreviewHandler,
  uStreamPreviewHandler,
  uPreviewHandler;

  type
    TMDPreviewHandler = class(TBasePreviewHandler)
  public
    constructor Create(AParent: TWinControl); override;
  end;

const
  MyMD_PreviewHandlerGUID_64: TGUID = '{2417D607-9848-4D35-9178-DC3FA975D174}';
  MyMD_PreviewHandlerGUID_32: TGUID = '{76D62F81-1833-4761-BF9B-076ACD75106D}';

implementation

Uses
  uLogExcept,
  SynEdit,
  Windows,
  Forms,
  MDShellEx.Misc;

type
  TWinControlClass = class(TWinControl);

constructor TMDPreviewHandler.Create(AParent: TWinControl);
begin
  TLogPreview.Add('TMDPreviewHandler.Create');
  inherited Create(AParent);
  TLogPreview.Add('TMDPreviewHandler Done');
end;

end.
