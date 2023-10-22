{.GXFormatter.config=twm}
///<summary> Add this unit to ignore some FastReport properties from translation </summary>
unit u_dzTranslatorFastReport;

interface

uses
  SysUtils,
  Classes;

implementation

uses
  frxClass,
  u_dzTranslator;

initialization

  // Ignore FastReport properties
//  TP_GlobalIgnoreClassProperty(TField, 'Origin');

  // Ignore FastReport classes
  TP_TryGlobalIgnoreClass(TfrxReport);
  TP_TryGlobalIgnoreClass(TfrxUserDataSet);

end.

