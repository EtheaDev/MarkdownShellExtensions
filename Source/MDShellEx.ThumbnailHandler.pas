{******************************************************************************}
{                                                                              }
{       MarkDown Shell extensions                                              }
{       (Preview Panel, Thumbnail Icon, MD Text Editor)                        }
{                                                                              }
{       Copyright (c) 2021 (Ethea S.r.l.)                                      }
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
unit MDShellEx.ThumbnailHandler;


interface

uses
  Classes,
  Controls,
  ComObj,
  ShlObj,
  Windows,
  Winapi.PropSys,
  System.Generics.Collections,
  MDShellEx.ThumbnailResources,
  SVGInterfaces,
  SVGCommon,
  ActiveX;

type
  TFEThumbnailProvider = class abstract
  public
    class function GetComClass: TComClass; virtual;
    class procedure RegisterThumbnailProvider(const AClassID: TGUID;
      const AName, ADescription: string);
  end;

  TThumbnailHandlerClass = class of TFEThumbnailProvider;

{$EXTERNALSYM IThumbnailProvider}
  IThumbnailProvider = interface(IUnknown)
    ['{E357FCCD-A995-4576-B01F-234630154E96}']
    function GetThumbnail(cx : uint; out hBitmap : HBITMAP; out bitmapType : dword): HRESULT; stdcall;
  end;

const
  {$EXTERNALSYM IID_IThumbnailProvider}
  ThumbnailProviderGUID = '{E357FCCD-A995-4576-B01F-234630154E96}';
  IID_IThumbnailProvider: TGUID = ThumbnailProviderGUID;

  MyMD_ThumbNailProviderGUID: TGUID = '{0F1BB887-2D58-4808-B64D-418299FDCB40}';

type
  TComFEThumbnailProvider = class(TComObject, IInitializeWithStream, IThumbnailProvider)
    function IInitializeWithStream.Initialize = IInitializeWithStream_Initialize;
    function IInitializeWithStream_Initialize(const pstream: IStream; grfMode: Cardinal): HRESULT; stdcall;
    function GetThumbnail(cx : uint; out hBitmap : HBITMAP; out bitmapType : dword): HRESULT; stdcall;
    function Unload: HRESULT; stdcall;
  private
    FThumbnailHandlerClass: TThumbnailHandlerClass;
    FIStream: IStream;
    FThumbnailResources: TdmThumbnailResources;
    FMode: Cardinal;
    FSVG: ISVG;
    FLightTheme: Boolean;
  protected
    property Mode: Cardinal read FMode write FMode;
    property IStream: IStream read FIStream write FIStream;
  public
    property ThumbnailHandlerClass: TThumbnailHandlerClass read FThumbnailHandlerClass write FThumbnailHandlerClass;
  end;

implementation

uses
  ComServ,
  Types,
  SysUtils,
  Graphics,
  ExtCtrls,
  uMisc,
  uREgistry,
  uLogExcept,
  uStreamAdapter,
  WinAPI.GDIPObj,
  WinAPI.GDIPApi,
  MDShellEx.ThumbnailHandlerRegister,
  SVGIconUtils;

{ TComFEThumbnailProvider }

function TComFEThumbnailProvider.GetThumbnail(cx: uint; out hBitmap: HBITMAP;
  out bitmapType: dword): HRESULT;
const
  WTSAT_ARGB = 2;
var
  AStream: TIStreamAdapter;
  LBitmap: TBitmap;
  LAntiAliasColor: TColor;
  LSVGText: string;
begin
  try
    TLogPreview.Add('TComFEThumbnailProvider.GetThumbnail start');
    hBitmap := 0;
    if (cx = 0) then
    begin
      Result := S_FALSE;
      Exit;
    end;
    bitmapType := WTSAT_ARGB;
    AStream := TIStreamAdapter.Create(FIStream);
    try
      TLogPreview.Add('TComFEThumbnailProvider.GetThumbnail LoadFromStream');
      LSVGText := FThumbnailResources.GetSVGText(AStream);
      if Pos('*Error*', LSVGText) > 0 then
        LSVGText := FThumbnailResources.GetDefaultXMLThumbnail;
      FSVG.Source := LSVGText;
      TLogPreview.Add('TComFEThumbnailProvider.FSVG.Source '+FSVG.Source);
      LBitmap := TBitmap.Create;
      LBitmap.PixelFormat := pf32bit;
      if FLightTheme then
        LAntiAliasColor := clWhite
      else
        LAntiAliasColor := clWebDarkSlategray;
      LBitmap.Canvas.Brush.Color := ColorToRGB(LAntiAliasColor);
      LBitmap.SetSize(cx, cx);
      TLogPreview.Add('TComFEThumbnailProvider.PaintTo start');
      FSVG.PaintTo(LBitmap.Canvas.Handle, TRectF.Create(0, 0, cx, cx));
      TLogPreview.Add('TComFEThumbnailProvider.PaintTo end');
      hBitmap := LBitmap.Handle;
    finally
      AStream.Free;
    end;
    Result := S_OK;
  except
    on E: Exception do
    begin
      Result := E_FAIL;
      TLogPreview.Add(Format('Error in TComFEThumbnailProvider.GetThumbnail - Message: %s: Trace %s',
        [E.Message, E.StackTrace]));
    end;
  end;
end;

function TComFEThumbnailProvider.IInitializeWithStream_Initialize(
  const pstream: IStream; grfMode: Cardinal): HRESULT;
begin
  TLogPreview.Add('TComFEThumbnailProvider.IInitializeWithStream_Initialize Init');
  Initialize_GDI;
  FIStream := pstream;
  FThumbnailResources := TdmThumbnailResources.Create(nil);
  //FMode := grfMode;
  Result := S_OK;
  //Result := E_NOTIMPL;
  if Result = S_OK then
  begin
    FSVG := GlobalSVGFactory.NewSvg;
    FLightTheme := IsWindowsAppThemeLight;
  end;
  TLogPreview.Add('TComFEThumbnailProvider.IInitializeWithStream_Initialize done');
end;

function TComFEThumbnailProvider.Unload: HRESULT;
begin
  TLogPreview.Add('TComFEThumbnailProvider.Unload Init');
  FThumbnailResources.free;
  Finalize_GDI;
  result := S_OK;
  TLogPreview.Add('TComFEThumbnailProvider.Unload Done');
end;

{ TFEThumbnailProvider }

class function TFEThumbnailProvider.GetComClass: TComClass;
begin
  Result := TComFEThumbnailProvider;
end;

class procedure TFEThumbnailProvider.RegisterThumbnailProvider(
  const AClassID: TGUID; const AName, ADescription: string);
begin
  TLogPreview.Add('TFEThumbnailProvider.RegisterThumbnailProvider Init ' + AName);
  TThumbnailHandlerRegister.Create(Self, AClassID, AName, ADescription);
  TLogPreview.Add('TFEThumbnailProvider.RegisterThumbnailProvider Done ' + AName);
end;

end.


