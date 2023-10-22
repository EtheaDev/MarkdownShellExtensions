unit u_dzTranslatorAdo;
///<summary> Add this unit to ignore some ADO database properties from translation </summary>

interface

uses
  ADODB,
  u_dzTranslator,
  u_dzTranslatorDB;

implementation

initialization
  // ADO components
  TP_TryGlobalIgnoreClass(TADOConnection);
  TP_GlobalIgnoreClassProperty(TADOQuery, 'CommandText');
  TP_GlobalIgnoreClassProperty(TADOQuery, 'ConnectionString');
  TP_GlobalIgnoreClassProperty(TADOQuery, 'DatasetField');
  TP_GlobalIgnoreClassProperty(TADOQuery, 'Filter');
  TP_GlobalIgnoreClassProperty(TADOQuery, 'IndexFieldNames');
  TP_GlobalIgnoreClassProperty(TADOQuery, 'IndexName');
  TP_GlobalIgnoreClassProperty(TADOQuery, 'MasterFields');
  TP_GlobalIgnoreClassProperty(TADOTable, 'IndexFieldNames');
  TP_GlobalIgnoreClassProperty(TADOTable, 'IndexName');
  TP_GlobalIgnoreClassProperty(TADOTable, 'MasterFields');
  TP_GlobalIgnoreClassProperty(TADOTable, 'TableName');
  TP_GlobalIgnoreClassProperty(TADODataset, 'CommandText');
  TP_GlobalIgnoreClassProperty(TADODataset, 'ConnectionString');
  TP_GlobalIgnoreClassProperty(TADODataset, 'DatasetField');
  TP_GlobalIgnoreClassProperty(TADODataset, 'Filter');
  TP_GlobalIgnoreClassProperty(TADODataset, 'IndexFieldNames');
  TP_GlobalIgnoreClassProperty(TADODataset, 'IndexName');
  TP_GlobalIgnoreClassProperty(TADODataset, 'MasterFields');
end.

