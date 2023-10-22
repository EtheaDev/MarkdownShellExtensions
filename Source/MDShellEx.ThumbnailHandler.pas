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
  SVGInterfaces,
  ActiveX;

type
  TMDThumbnailProvider = class abstract
  public
    class function GetComClass: TComClass; virtual;
    class procedure RegisterThumbnailProvider(const AClassID: TGUID;
      const AName, ADescription: string);
  end;

  TThumbnailHandlerClass = class of TMDThumbnailProvider;

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
  TComMDThumbnailProvider = class(TComObject, IInitializeWithStream, IThumbnailProvider)
    function IInitializeWithStream.Initialize = IInitializeWithStream_Initialize;
    function IInitializeWithStream_Initialize(const pstream: IStream; grfMode: Cardinal): HRESULT; stdcall;
    function GetThumbnail(cx : uint; out hBitmap : HBITMAP; out bitmapType : dword): HRESULT; stdcall;
    function Unload: HRESULT; stdcall;
  private
    FThumbnailHandlerClass: TThumbnailHandlerClass;
    FIStream: IStream;
    FMode: Cardinal;
    FSVG: ISVG;
    FLightTheme: Boolean;
    function GetSVGIcon: string;
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
  MDShellEx.Misc,
  uREgistry,
  uLogExcept,
  uStreamAdapter,
  WinAPI.GDIPObj,
  WinAPI.GDIPApi,
  MDShellEx.ThumbnailHandlerRegister,
  SVGIconUtils;

{ TComMDThumbnailProvider }

function TComMDThumbnailProvider.GetSVGIcon: string;
begin
  Result :=
    '<?xml version="1.0" standalone="no"?>'+
    '<svg width="100" height="100" version="1.1">'+
    ' <path d="m 87.77,23.809 v 71.504 c 0,2.598 -2.09,4.688 -4.687,4.688 H 17.457 c -2.598,0 -4.687,-2.09 -4.687,-4.687 V 4.688 C 12.77,2.09 14.86,0 17.457,0 h 46.485 z" fill="#b3b3b3" id="path5" style="opacity:0.995"/>'+
    ' <path d="M63.942 23.809L87.77 47.637V23.809z" opacity=".4" id="path7"/>'+
    ' <path d="M86.403 20.508L67.262 1.367C66.383.488 65.192 0 63.942 0v23.809H87.77c0-1.23-.488-2.422-1.367-3.301z" fill="#e6e6e6" id="path9"/>'+
    ' <path d="M30.115 8.758l-6.859 76.489-10.158 11.586 7.18-12.687 7.027-79.097c.756.362 2.979 1.672 2.81 3.71z" opacity=".4" id="path11"/>'+
    ' <g transform="matrix(0.060535,-0.050795,0.050795,0.060535,37.661356,29.863397)" id="g35" style="display:inline">'+
    '   <path d="m -718.984,276.544 610.062,-610.062 110.369,108.822 -609.236,609.253 c -111.192,42.832 -101.445,5.504 -68.658,-83.839 -25.462,9.266 -56.817,13.497 -42.536,-24.174 z" fill="#fde061" id="path13"/>'+
    '   <path d="M -763.684,397.59 -719,276.602 c -1.657,13.479 -2.446,20.206 -0.331,21.593 5.138,3.37 33.879,-2.455 42.896,2.512 4.273,12.189 -11.973,42.504 -5.24,49.225 6.311,6.298 '+
    '52.395,-5.271 58.6,-1.353 0.745,36.525 -15.633,38.854 8.027,38.672 l -117.154,43.776 c -10.08,-9.097 -25.571,-15.664 -15.174,-34.334 z" fill="#fcb180" id="path15"/>'+
    '   <path d="m -623.079,348.575 c -55.782,18.405 -78.519,8.198 -53.357,-47.868 l 601.109,-601.109 50.974,50.251 z" fill="#fdce24" id="path17"/>'+
    '   <path d="m -623.076,348.578 598.726,-598.726 25.814,25.469 -609.245,609.245 -22.619,8.405 z" fill="#f4b729" id="path19"/>'+
    '   <path d="m -727.27,416.026 104.195,-67.448 c 1.828,6.341 -5.376,33.067 -4.644,36.381 0.357,1.618 5.052,2.035 12.671,2.292 l -119.574,44.927 z" fill="#df8040" id="path21"/>'+
    '   <path d="m -676.436,300.706 c 3.464,6.731 -6.175,25.521 -7.071,42.295 -0.434,8.124 3.69,8.584 11.805,8.483 24.037,-0.876 42.898,-6.382 48.626,-2.906 l -104.195,67.448 c -23.115,-2.147 -21.762,5.937 -20.107,-19.333 z" fill="#f3975a" id="path23"/>'+
    '   <path d="m -782.898,448.649 19.411,-51.731 c 0.396,-1.055 4.025,-1.309 8.734,-1.309 8.875,0 9.314,0.576 7.711,10.134 -0.606,3.615 -0.877,7.179 -0.602,7.92 '+
    '0.352,0.947 3.511,1.347 10.625,1.347 9.076,0 10.124,0.203 10.124,1.964 0,2.768 -2.786,13.421 -4.728,14.138 l -50.1,18.525 c -1.24,0.459 -1.488,-0.16 -1.177,-0.988 z" fill="#0f0d0b" id="path25"/>'+
    '   <path d="m -49.377,-393.062 47.509,-47.509 c 18.819,-22.203 131.38,87.037 109.602,109.605 l -47.508,47.508 A 1113.83,1113.83 0 0 1 -49.377,-393.062 Z" fill="#ec9190" id="path27"/>'+
    '   <path d="m -17.664,-358.065 63.904,-63.904 c 12.902,9.508 38.58,34.333 50.759,52.774 l -62.53,62.562 c -18.736,-17.49 -33.461,-31.859 -52.133,-51.432 z" fill="#e86c6a" id="path29"/>'+
    '   <path d="m 34.48,-306.635 62.538,-62.538 c 3.42,5.268 18.844,26.616 11.53,37.393 l -48.323,48.316 c -8.826,-7.679 -17.496,-15.67 -25.745,-23.171 z" fill="#d6514f" id="path31"/>'+
    '   <path d="m -108.926,-333.514 59.549,-59.549 c 33.136,36.997 70.994,73.559 109.604,109.604 L 0.678,-223.91 c -38.49,-34.283 -77.593,-72.389 -109.604,-109.604 z" fill="#dbdbdb" id="path33"/>'+
    ' </g>'+
    ' <path id="rect848-7" style="display:inline;opacity:0.998;fill:#000000;fill-opacity:1;stroke-width:0.877845" d="m 27.942844,50.666547 c -2.303773,0 -4.158449,1.322392 -4.158449,2.964998 V 80.04582 c 0,1.642609 1.854676,2.965001 4.158449,2.965001 '+
    'h 53.383617 c 2.303777,0 4.158443,-1.322392 4.158443,-2.965001 V 53.631545 c 0,-1.642606 -1.854666,-2.964998 -4.158443,-2.964998 z m 1.413223,4.140575 h 6.521935 l 6.523962,8.847226 6.523967,-8.847226 h 6.523967 V 78.870246 H 48.925931 V 65.070252 '+
    'l -6.523967,8.845777 -6.523962,-8.845777 v 13.799994 h -6.521935 z m 37.509243,0 h 6.523966 v 12.385539 h 6.523965 l -9.784935,11.677585 -9.78493,-11.677585 h 6.521934 z"/>'+
    ' <path d="M 31.989488,84.201831 V 51.738706 h 6.275035 l 6.275038,11.934973 6.275033,-11.934973 h 6.275037 V 84.201831 H 50.814594 V 65.583274 L 44.539561,77.518246 38.264523,65.583274 v 18.618557 z m 39.218975,0 -9.412559,-15.754162 h 6.275037 '+
    'V 51.738706 h 6.275038 v 16.708963 h 6.275037 z" id="path851" style="display:none;opacity:0.996;fill:#000000;stroke-width:0.387021"/>'+
    ' <path style="display:none;opacity:0.995;fill:#000000;fill-opacity:1;stroke-width:0.340447" d="m 29.105718,82.325946 v -39.51766 h 9.429855 L 47.96543,57.336839 57.395282,42.808286 H 66.82514 V 82.326 H 71.4335 L 62.234711,94.337 53.0345,82.326 h '+
    '4.360782 V 59.661406 L 47.96543,74.189959 38.535573,59.661406 V 82.326 Z" id="path943"/>'+
    '</svg>';
end;

function TComMDThumbnailProvider.GetThumbnail(cx: uint; out hBitmap: HBITMAP;
  out bitmapType: dword): HRESULT;
const
  WTSAT_ARGB = 2;
var
  AStream: TIStreamAdapter;
  LBitmap: TBitmap;
  LAntiAliasColor: TColor;
begin
  try
    TLogPreview.Add('TComMDThumbnailProvider.GetThumbnail start');
    hBitmap := 0;
    if (cx = 0) then
    begin
      Result := S_FALSE;
      Exit;
    end;
    bitmapType := WTSAT_ARGB;
    AStream := TIStreamAdapter.Create(FIStream);
    try
      TLogPreview.Add('TComMDThumbnailProvider.GetThumbnail LoadFromStream');
      FSVG.Source := GetSVGIcon;
      TLogPreview.Add('TComMDThumbnailProvider.FSVG.Source '+FSVG.Source);
      LBitmap := TBitmap.Create;
      LBitmap.PixelFormat := pf32bit;
      if FLightTheme then
        LAntiAliasColor := clWhite
      else
        LAntiAliasColor := clWebDarkSlategray;
      LBitmap.Canvas.Brush.Color := ColorToRGB(LAntiAliasColor);
      LBitmap.SetSize(cx, cx);
      TLogPreview.Add('TComMDThumbnailProvider.PaintTo start');
      FSVG.PaintTo(LBitmap.Canvas.Handle, TRectF.Create(0, 0, cx, cx));
      TLogPreview.Add('TComMDThumbnailProvider.PaintTo end');
      hBitmap := LBitmap.Handle;
    finally
      AStream.Free;
    end;
    Result := S_OK;
  except
    on E: Exception do
    begin
      Result := E_FAIL;
      TLogPreview.Add(Format('Error in TComMDThumbnailProvider.GetThumbnail - Message: %s: Trace %s',
        [E.Message, E.StackTrace]));
    end;
  end;
end;

function TComMDThumbnailProvider.IInitializeWithStream_Initialize(
  const pstream: IStream; grfMode: Cardinal): HRESULT;
begin
  TLogPreview.Add('TComMDThumbnailProvider.IInitializeWithStream_Initialize Init');
  Initialize_GDI;
  FIStream := pstream;
  //FMode := grfMode;
  Result := S_OK;
  //Result := E_NOTIMPL;
  if Result = S_OK then
  begin
    FSVG := GlobalSVGFactory.NewSvg;
    FLightTheme := IsWindowsAppThemeLight;
  end;
  TLogPreview.Add('TComMDThumbnailProvider.IInitializeWithStream_Initialize done');
end;

function TComMDThumbnailProvider.Unload: HRESULT;
begin
  TLogPreview.Add('TComMDThumbnailProvider.Unload Init');
  Finalize_GDI;
  result := S_OK;
  TLogPreview.Add('TComMDThumbnailProvider.Unload Done');
end;

{ TMDThumbnailProvider }

class function TMDThumbnailProvider.GetComClass: TComClass;
begin
  Result := TComMDThumbnailProvider;
end;

class procedure TMDThumbnailProvider.RegisterThumbnailProvider(
  const AClassID: TGUID; const AName, ADescription: string);
begin
  TLogPreview.Add('TMDThumbnailProvider.RegisterThumbnailProvider Init ' + AName);
  TThumbnailHandlerRegister.Create(Self, AClassID, AName, ADescription);
  TLogPreview.Add('TMDThumbnailProvider.RegisterThumbnailProvider Done ' + AName);
end;

end.

