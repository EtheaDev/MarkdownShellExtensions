{******************************************************************************}
{                                                                              }
{       StyledToolbar: a Toolbar with TStyledAnimatedToolButtons inside        }
{       Based on TStyledToolbar and animations using Skia4Delphi               }
{                                                                              }
{       Copyright (c) 2022-2023 (Ethea S.r.l.)                                 }
{       Author: Carlo Barazzetta                                               }
{       Contributors:                                                          }
{                                                                              }
{       https://github.com/EtheaDev/StyledComponents                           }
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
unit Vcl.StyledAnimatedToolbar;

interface

{$INCLUDE StyledComponents.inc}

uses
  Vcl.StyledToolbar
  , Vcl.Controls
(*
  , Vcl.StdCtrls
  , Vcl.ComCtrls
  , Vcl.ImgList
  , System.UITypes
  , System.SysUtils
  , System.Classes
  , System.Math
  , Vcl.ToolWin
  , Vcl.ExtCtrls
  , Vcl.Themes
  , Vcl.ActnList
  , Vcl.Menus
  , Winapi.Messages
  , Winapi.Windows
  , Vcl.ButtonStylesAttributes
*)
  , Vcl.SkAnimatedImageHelper
  ;

type
  TStyledAnimatedToolbar = class;
  TStyledAnimatedToolButton = class;

  TAnimatedButtonProc = reference to procedure (Button: TStyledAnimatedToolButton);
  TControlProc = reference to procedure (Control: TControl);

  TStyledAnimatedToolButton = class(TStyledToolButton)
  private
  protected
  public
  published
  end;

  TStyledAnimatedToolbar = class(TStyledToolbar)
  private
  protected
    function GetStyledToolButtonClass: TStyledToolButtonClass; override;
  public
  published
  end;

implementation

uses
  Vcl.Consts
  ;

{ TStyledAnimatedToolbar }

function TStyledAnimatedToolbar.GetStyledToolButtonClass: TStyledToolButtonClass;
begin
  Result := TStyledAnimatedToolButton;
end;

end.
