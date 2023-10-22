{.GXFormatter.config=twm}
///<summary> Add this unit to ignore some database properties from translation </summary>
unit u_dzTranslatorBDE;

{$INCLUDE 'dzlib.inc'}

{$IFDEF BDE_IS_DEPRECATED}
{$IFNDEF NO_BDE_HINT}
{$MESSAGE HINT 'The BDE has been deprecated for a long time'}
{$ENDIF}
{$ENDIF}

interface

{$IFNDEF BDE_IS_DEPRECATED}
uses
  SysUtils,
  Classes;
{$ENDIF ~BDE_IS_DEPRECATED}

implementation

{$IFNDEF BDE_IS_DEPRECATED}
uses
  DB,
  DBTables,
  DBCtrls,
  u_dzTranslator,
  u_dzTranslatorDB;

initialization

  // Database Controls
  TP_GlobalIgnoreClassProperty(TDBComboBox, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBCheckBox, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBEdit, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBImage, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBListBox, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBLookupControl, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBLookupControl, 'KeyField');
  TP_GlobalIgnoreClassProperty(TDBLookupControl, 'ListField');
  TP_GlobalIgnoreClassProperty(TDBMemo, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBRadioGroup, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBRichEdit, 'DataField');
  TP_GlobalIgnoreClassProperty(TDBText, 'DataField');

  // Borland Database Engine  (BDE)
  TP_TryGlobalIgnoreClass(TSession);
  TP_TryGlobalIgnoreClass(TDatabase);
{$ENDIF ~BDE_IS_DEPRECATED}
end.

