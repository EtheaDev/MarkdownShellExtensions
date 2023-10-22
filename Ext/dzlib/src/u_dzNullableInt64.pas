unit u_dzNullableInt64;

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
  _NULLABLE_TYPE_BASE_ = Int64;
const
  _NULLABLE_TYPE_NAME_ = 'TNullableInt64';
{$INCLUDE 't_NullableNumber.tpl'}

type
  TNullableInt64 = _NULLABLE_NUMBER_;
  TdzNullableInt64 = TNullableInt64 deprecated;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

{$INCLUDE 't_NullableNumber.tpl'}

{$ENDIF DELPHI2007_UP}

end.
