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
unit MDShellEx.ThumbnailResources;

interface

uses
  SysUtils
  , Classes
  , Xml.xmldom
  , Xml.XMLIntf
  , Xml.Win.msxmldom
  , Xml.XMLDoc
  ;

type
  TdmThumbnailResources = class(TDataModule)
    DefaultTemplate: TXMLDocument;
    SourceXML: TXMLDocument;
    EditingTemplate: TXMLDocument;
  private
    function Parse: string;
  public
    StylesheetName: string;
    function GetSVGText(const AXMLLines: TStrings): string; overload;
    function GetSVGText(const AStream: TStream): string; overload;
    function GetDefaultXMLThumbnail: string;
  public
    const SVG_DEFAULT_XSLT = 'Default';
    const SVG_EDITING_XSLT = 'Editing';
  end;

implementation

{$R *.dfm}

uses
  Windows
  , System.StrUtils
  , System.IOUtils
  , System.NetEncoding
  , WinAPI.ShellAPI
  , MDShellEx.Resources
  ;

{ TThumbnail }

function TdmThumbnailResources.Parse: string;
var
  LXSLTOutput: WideString;

  procedure Transform(AXMLDoc: TXMLDocument);
  begin
    AXMLDoc.Active := True;
    SourceXML.Node.TransformNode(AXMLDoc.DocumentElement, LXSLTOutput);
  end;

begin
  Result := '';
  LXSLTOutput := '';
  SourceXML.Active := True;
  try
    if StylesheetName = SVG_EDITING_XSLT then
      Transform(EditingTemplate)
    else
      Transform(DefaultTemplate);
  finally
    SourceXML.Active := False;
  end;
  Result := LXSLTOutput;
end;

function TdmThumbnailResources.GetDefaultXMLThumbnail: string;
begin
  Result :=
    '<?xml version="1.0" encoding="utf-8"?>'+
    '   <svg version="1.1" id="Capa_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"'+
    '      viewBox="0 0 56 56" style="enable-background:new 0 0 56 56;" xml:space="preserve">'+
    '   <g>'+
    '     <path style="fill:#E9E9E0;" d="M36.985,0H7.963C7.155,0,6.5,0.655,6.5,1.926V55c0,0.345,0.655,1,1.463,1h40.074'+
    '       c0.808,0,1.463-0.655,1.463-1V12.978c0-0.696-0.093-0.92-0.257-1.085L37.607,0.257C37.442,0.093,37.218,0,36.985,0z"/>'+
    '     <polygon style="fill:#D9D7CA;" points="37.5,0.151 37.5,12 49.349,12 	"/>'+
    '     <path style="fill:#F29C1F;" d="M48.037,56H7.963C7.155,56,6.5,55.345,6.5,54.537V39h43v15.537C49.5,55.345,48.845,56,48.037,56z"/>'+
    '     <g>'+
    '       <path style="fill:#FFFFFF;" d="M19.379,48.105L21.936,53h-1.9l-1.6-3.801h-0.137L16.576,53h-1.9l2.557-4.895l-2.721-5.182h1.873'+
    '         l1.777,4.102h0.137l1.928-4.102H22.1L19.379,48.105z"/>'+
    '       <path style="fill:#FFFFFF;" d="M31.998,42.924h1.668V53h-1.668v-6.932l-2.256,5.605h-1.449l-2.27-5.605V53h-1.668V42.924h1.668'+
    '         l2.994,6.891L31.998,42.924z"/>'+
    '       <path style="fill:#FFFFFF;" d="M37.863,42.924v8.832h4.635V53h-6.303V42.924H37.863z"/>'+
    '     </g>'+
    '     <path style="fill:#F29C1F;" d="M15.5,24c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.391-1.023,0-1.414l6-6'+
    '       c0.391-0.391,1.023-0.391,1.414,0s0.391,1.023,0,1.414l-6,6C16.012,23.902,15.756,24,15.5,24z"/>'+
    '     <path style="fill:#F29C1F;" d="M21.5,30c-0.256,0-0.512-0.098-0.707-0.293l-6-6c-0.391-0.391-0.391-1.023,0-1.414'+
    '       s1.023-0.391,1.414,0l6,6c0.391,0.391,0.391,1.023,0,1.414C22.012,29.902,21.756,30,21.5,30z"/>'+
    '     <path style="fill:#F29C1F;" d="M33.5,30c-0.256,0-0.512-0.098-0.707-0.293c-0.391-0.391-0.391-1.023,0-1.414l6-6'+
    '       c0.391-0.391,1.023-0.391,1.414,0s0.391,1.023,0,1.414l-6,6C34.012,29.902,33.756,30,33.5,30z"/>'+
    '     <path style="fill:#F29C1F;" d="M39.5,24c-0.256,0-0.512-0.098-0.707-0.293l-6-6c-0.391-0.391-0.391-1.023,0-1.414'+
    '       s1.023-0.391,1.414,0l6,6c0.391,0.391,0.391,1.023,0,1.414C40.012,23.902,39.756,24,39.5,24z"/>'+
    '     <path style="fill:#F29C1F;" d="M24.5,32c-0.11,0-0.223-0.019-0.333-0.058c-0.521-0.184-0.794-0.755-0.61-1.276l6-17'+
    '       c0.185-0.521,0.753-0.795,1.276-0.61c0.521,0.184,0.794,0.755,0.61,1.276l-6,17C25.298,31.744,24.912,32,24.5,32z"/>'+
    '   </g>'+
    '   </svg>';
end;

function TdmThumbnailResources.GetSVGText(const AStream: TStream): string;
var
  LOutStream: TStringStream;
begin
  LOutStream := TStringStream.Create('', TEncoding.UTF8);
  try
    //LoadFromStream(AStream, LOutStream);
    //Inizializza il Documento XML
    SourceXML.LoadFromStream(LOutStream, xetUTF_8);
  finally
    LOutStream.Free;
  end;
  Result := Parse;
end;

function TdmThumbnailResources.GetSVGText(const AXMLLines: TStrings): string;
begin
  //Inizializza il Documento XML
  SourceXML.LoadFromXML(AXMLLines.Text);
  //SourceXML.XML.Assign(AXMLLines);
  Result := Parse;
  if Result = '' then
    raise Exception.Create('Errore nella trasformazione in SVG dell''anteprima dell''icona');
end;

end.
