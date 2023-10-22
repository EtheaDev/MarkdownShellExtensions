unit u_dzNullableExtended;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  SysUtils,
  Math,
  Variants,
  u_dzTranslator,
  u_dzVariantUtils,
  u_dzConvertUtils,
  u_dzStringUtils,
  u_dzNullableTypesUtils;

{$DEFINE __DZ_NULLABLE_NUMBER_TEMPLATE__}
type
  _NULLABLE_TYPE_BASE_ = Extended;
const
  _NULLABLE_TYPE_NAME_ = 'TNullableExtended';
{$INCLUDE 't_NullableNumber.tpl'}

type
  TNullableExtended = _NULLABLE_NUMBER_;
  TdzNullableExtended = TNullableExtended deprecated;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

{$INCLUDE 't_NullableNumber.tpl'}

{$ENDIF DELPHI2007_UP}

end.
