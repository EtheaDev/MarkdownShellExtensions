unit u_dzTranslatorReportBuilder;

interface

implementation

uses
  { TODO -otwm : todo: add report builder units for these classes }
  u_dzTranslator;

initialization
  { TODO -otwm -ccheck : This should not always be called because it links in all ReportBuilder units. }
  // Report-Builder Components / Properties to ignore
  TP_GlobalIgnoreClassProperty(TppField, 'FieldAlias');
  TP_GlobalIgnoreClassProperty(TppField, 'FieldName');
  TP_GlobalIgnoreClassProperty(TppField, 'DisplayFormat');
  TP_GlobalIgnoreClassProperty(TppField, 'SortExpression');
  TP_GlobalIgnoreClassProperty(TppField, 'TableAlias');
  TP_GlobalIgnoreClassProperty(TppField, 'TableName');
  TP_GlobalIgnoreClassProperty(TppBackgroundPrintSettings, 'BinName');
  TP_GlobalIgnoreClassProperty(TppBackgroundPrintSettings, 'PrinterName');
  TP_GlobalIgnoreClassProperty(TppBackgroundPrintSettings, 'PaperName');

  TP_GlobalIgnoreClassProperty(TppChildReport, 'DeviceType');
  TP_GlobalIgnoreClassProperty(TppChildReport, 'Version');

  TP_GlobalIgnoreClassProperty(TPsRBExportComponent, 'Version');
end.

