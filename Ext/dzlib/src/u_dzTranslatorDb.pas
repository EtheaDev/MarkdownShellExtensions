{.GXFormatter.config=twm}
///<summary> Add this unit to ignore some database properties from translation </summary>
unit u_dzTranslatorDb;

interface

uses
  SysUtils,
  Classes;

implementation

uses
  DB,
  u_dzTranslator;

initialization

  // Ignore Database properties
  TP_GlobalIgnoreClassProperty(TField, 'DefaultExpression');
  TP_GlobalIgnoreClassProperty(TField, 'FieldName');
  TP_GlobalIgnoreClassProperty(TField, 'KeyFields');
  TP_GlobalIgnoreClassProperty(TField, 'DisplayName');
  TP_GlobalIgnoreClassProperty(TField, 'LookupKeyFields');
  TP_GlobalIgnoreClassProperty(TField, 'LookupResultField');
  TP_GlobalIgnoreClassProperty(TField, 'Origin');
  TP_TryGlobalIgnoreClass(TParam);

end.

