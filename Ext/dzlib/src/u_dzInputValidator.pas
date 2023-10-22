///<summary>
/// Usage:
/// 1. Get an IdzInputValidator interface for each form you want to validate by calling the
///    InputValidator function.
/// 2. Get a control specific validator interface for each control by calling one of the
///    overloaded ControlValidator functions (note: There are additional ControlValidator functions
///    in specialized u_dzInputValidatorXXX units, e.g. u_dzInputValidatorJVCL), passing it
///    the IdzInputValidator interface created above.
/// 3. Call any of the the ControlValidator's ValidateXxxx methods.
///    These methods check whether the control contains valid data. If yes, they return true.
///    If not, they return false, set the control to the error colour and also notify the
///    IdzInputValidator interface that there is an error in the data.
/// 4. After validating the input of all your controls, call the IdzInputValidator.GetResult
///    method. There are two overloaded versions of this method, one simply returns true, if all
///    the contained valid data, false otherwise. In addition to that, the other one also returns
///    an error message for displaying e.g. in a status bar.
///
/// Example:
/// var
///   iv: IdzInputValidator;
///   ErrMsg: string;
/// begin
///   iv := InputValidator;
///   ControlValidator(ed_ForANumber, iv).AsInteger.IsValid;
///   // ...
///   b_Ok.Enabled := iv.GetResult(ErrMsg);
///   TheStatusBar.SimpleText := ErrMsg;
/// end;
///
/// Note that IdzInputValidator has several overloaded Check methods that also return control
/// specific validator interfaces. These are depreacted will be phased out in the future.
/// The same applies to the
///</summary>
unit u_dzInputValidator;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  CheckLst,
  u_dzTranslator;

type
  IdzInputValidator = interface;

  ///<summary>
  /// This is the ancestor for many of the TXxxxValidator classes. It usually doesn't make sense
  /// to instantiate it. </summary>
  TControlValidator = class(TInterfacedObject)
  protected
    type
      TControlHack = class(TCustomControl);
  protected
    FControl: TControl;
    FValidator: IdzInputValidator;
    function Control: TControlHack;
    function Validate(_Condition: Boolean): Boolean; overload; virtual;
    function Validate(_Condition: Boolean; const _Message: string): Boolean; overload; virtual;
    function ValidateTrue: Boolean;
    function ValidateFalse(const _ErrorMessage: string): Boolean;
    procedure SetColor(_OK: Boolean);
    procedure doSetColor(_Color: TColor); virtual;
  public
    constructor Create(_Ctrl: TControl; _Validator: IdzInputValidator);
  end;

  ///<summary>
  /// An aggregate control (e.g. built with ccpack) can provide this interface and pass it
  /// to ControlValidator(<an embedded TEdit>, <InputValidator>, <pass the interface here>)
  /// to create a IdzEditValidator if the text to check is not the TEdit.Text property
  /// but needs to preprocessed, e.g. it could be a hex value without '$' prefix so the
  /// prefix would need to be added in the GetText method:
  /// TEdit.Text = 'abcd'
  /// GetText returns '$abcd' the following call would be possible:
  /// EditValidator.AsInteger.IsValid </summary>
  IdzControlTextSource = interface ['{A5C61A11-87B4-47C3-AAA2-13A59A2E840E}']
    function GetText: string;
  end;

  ///<summary>
  /// This is the ancestor for control validators that use the above mentioned method
  /// for providing an interface IdzControlTextSource for access to the edit text. </summary>
  TTextControlValidator = class(TControlValidator)
  protected
    FContainer: IdzControlTextSource;
    function doGetText: string; virtual;
    procedure doFocusControl;
  public
    constructor Create(_Ctrl: TControl; _Validator: IdzInputValidator;
      _Container: IdzControlTextSource = nil);
  end;

  IdzTextControlIntegerValidator = interface ['{8F794F3E-FC5A-42B1-8E9D-9C33ECC96378}']
    ///<summary> Checks whether the edit control contains an integer and colors it accordingly </summary>
    function IsValid: Boolean; overload;
    function IsValid(out _Value: Integer): Boolean; overload;
    ///<summary> Checks whether the edit control contains an integer
    ///          or is empty and colors it accordingly </summary>
    function IsValidOrEmpty: Boolean;
    ///<summary> Checks whether the edit control contains an integer in the given range and colors it
    ///          accordingly </summary>
    function IsBetween(_MinValue, _MaxValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer in the given range or empty
    ///          and colors it accordingly </summary>
    function IsBetweenOrEmpty(_MinValue, _MaxValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is less than the
    ///          given maximum and colors it accordingly </summary>
    function IsLessThan(_MaxValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is less than the
    ///          given maximum or empty and colors it accordingly </summary>
    function IsLessThanOrEmpty(_MaxValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is less or equal the
    ///          given maximum and colors it accordingly </summary>
    function IsLessThanOrEqual(_MaxValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is greater than the
    ///          given minimum and colors it accordingly </summary>
    function IsGreaterThan(_MinValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is greater than the
    ///          given minimum or empty and colors it accordingly </summary>
    function IsGreaterThanOrEmpty(_MinValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is greater or equal the
    ///          given minimum and colors it accordingly </summary>
    function IsGreaterOrEqual(_MinValue: Integer): Boolean;
    ///<summary> Checks whether the edit control contains an integer that is greater or equal the
    ///          given minimum or empty and colors it accordingly </summary>
    function IsGreaterOrEqualOrEmpty(_MinValue: Integer): Boolean;
    ///<summary> Converts the edit control's content to an integer. If it cannot be converted, it
    ///          raises an exception. </summary>
    function GetValue(_FocusControl: Boolean = True): Integer;
    ///<summary> Tries to convert the edit control's content to an integer. Returns false if it
    ///          cannot be converted. </summary>
    function TryGetValue(out _Value: Integer): Boolean;
  end;
  IdzEditIntegerValidator = IdzTextControlIntegerValidator deprecated;

  IdzTextControlFloatValidator = interface ['{C11C7D9E-09BD-425B-9F7F-7D251B9AC4E3}']
    ///<summary> Checks whether the edit control contains a float or is empty and colors it accordingly </summary>
    function IsValidOrEmtpy: Boolean; deprecated; // typo!
    ///<summary> Checks whether the edit control contains a float or is empty and colors it accordingly </summary>
    function IsValidOrEmpty: Boolean;
    ///<summary> Checks whether the edit control contains a float and colors it accordingly </summary>
    function IsValid: Boolean;
    ///<summary> Checks whether the edit control contains a float that is greater than the
    ///          given minimum and colors it accordingly </summary>
    function IsGreaterThan(_MinValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float that is greater than the
    ///          given minimum or empty and colors it accordingly </summary>
    function IsGreaterThanOrEmpty(_MinValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float that is greater or equal the
    ///          given minimum and colors it accordingly </summary>
    function IsGreaterOrEqual(_MinValue: Extended): Boolean;
        ///<summary> Checks whether the edit control contains a float that is greater or equal the
    ///          given minimum or empty and colors it accordingly </summary>
    function IsGreaterOrEqualOrEmpty(_MinValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float that is less than the
    ///          given minimum and colors it accordingly </summary>
    function IsLessThan(_MaxValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float that is less than the
    ///          given minimum or empty and colors it accordingly </summary>
    function IsLessThanOrEmpty(_MaxValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float that is less or equal the
    ///          given minimum and colors it accordingly </summary>
    function IsLessOrEqual(_MaxValue: Extended): Boolean;
        ///<summary> Checks whether the edit control contains a float that is less or equal the
    ///          given minimum or empty and colors it accordingly </summary>
    function IsLessOrEqualOrEmpty(_MaxValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float in the given range and colors it
    ///          accordingly </summary>
    function IsBetween(_MinValue, _MaxValue: Extended): Boolean;
    ///<summary> Checks whether the edit control contains a float in the given range or is empty
    ///          and colors it accordingly </summary>
    function IsBetweenOrEmpty(_MinValue, _MaxValue: Extended): Boolean;
    ///<summary> Converts the edit control's content to a float. If it cannot be converted, it
    ///          raises an exception. </summary>
    function GetValue(_FocusControl: Boolean = True): Extended;
    ///<summary> Tries to convert the edit control's content to a float. Returns false if it
    ///          cannot be converted. </summary>
    function TryGetValue(out _Value: Extended): Boolean; overload;
    function TryGetValue(out _Value: Extended; out _s: string): Boolean; overload;
    ///<summary> Tries to convert the edit control's content to a float. Returns false if it
    ///          cannot be converted. </summary>
    function TryGetValue(out _Value: Double): Boolean; overload;
    function TryGetValue(out _Value: Double; out _s: string): Boolean; overload;
    ///<summary> Tries to convert the edit control's content to a float. Returns false if it
    ///          cannot be converted. </summary>
    function TryGetValue(out _Value: Single): Boolean; overload;
    function TryGetValue(out _Value: Single; out _s: string): Boolean; overload;
  end;
  IdzEditFloatValidator = IdzTextControlFloatValidator deprecated;

  IdzTextControlTimeValidator = interface ['{7B716723-DE12-4EC5-A549-3D3A08E00DC9}']
    ///<summary> Checks whether the TEdit's text is a valid time of day and colors it accordingly. </summary>
    function IsValid: Boolean;
    ///<summary> Checks whether the TEdit's text is a valid time of day or empty and colors it accordingly. </summary>
    function IsValidOrEmpty: Boolean;
    ///<summary> Checks whether the TEdit's text is a valid time of day and between the given
    ///          limits. </summary>
    function IsBetween(_MinTime, _MaxTime: TDateTime): Boolean;
    ///<summary> Tries to convert the TEdit's text to a time of day. If it is not a valid
    ///          time of day it sets the focus to the control if FocusControl is true and
    ///          @raises EConvertError </summary>
    function GetValue(_FocusControl: Boolean = True): TDate;
    ///<summary> Tries to convert the TEdit's text to a time of day. If it is not a valid date it
    ///          @param Value is the time of day, if it can be converted, only valid it Result is true
    ///          @returns true, if it is a valid time of day, false otherwise
    function TryGetValue(out _Value: TDateTime): Boolean;
  end;

  IdzTextControlDateValidator = interface ['{80C45A30-6137-4AA7-9AD0-198FFF36908A}']
    ///<summary> Checks whether the TEdit's text is a valid date and colors it accordingly. </summary>
    function IsValid: Boolean; overload;
    ///<summary>
    /// Checks whether the TEdit's text is a valid date and colors it accordingly.
    /// @param Value will contain the date, if it was valid. </summary>
    function IsValid(out _Value: TDate): Boolean; overload;
    ///<summary> Checks whether the TEdit's text is a valid date or empty and colors it accordingly. </summary>
    function IsValidOrEmpty: Boolean;
    ///<summary> Tries to convert the TEdit's text to a date. If it is not a valid date it
    ///          sets the focus to the control if FocusControl is true and
    ///          @raises EConvertError </summary>
    function GetValue(_FocusControl: Boolean = True): TDate;
    ///<summary> Tries to convert the TEdit's text to a date.
    ///          @param Value is the date, if it can be converted, only valid it Result is true
    ///          @returns true, if it is a valid date, false otherwise
    function TryGetValue(out _Value: TDate): Boolean; overload;
    function TryGetValue(out _Value: TDate; out _s: string): Boolean; overload;
    function IsBetween(_MinDate, _MaxDate: TDate): Boolean;
    function IsBetweenOrEmpty(_MinDate, _MaxDate: TDate): Boolean;
  end;

  IdzTextControlIpV4Validator = interface ['{ADC68B27-96CC-4341-B4BF-60929EFF0A45}']
    ///<summary> Checks whether the control's text is a valid IPv4 address and colors it accordingly. </summary>
    function IsValid: Boolean;
    ///<summary> Checks whether the TEdit's text is a valid IPv4 address or empty and colors it accordingly. </summary>
    function IsValidOrEmpty: Boolean;
  end;

  IdzTextControlTextValidator = interface ['{DDCC86C2-C2F6-4C6B-8F8F-37FF4F55BDDB}']
    ///<summary> Checks whether the TEdit's text is not empty and colors it accordingly </summary>
    function IsValid(_MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean; overload;
    ///<summary> Checks whether the TEdit's text is not empty and colors it accordingly,
    ///          returns the content in Text. </summary>
    function IsValid(out _Text: string; _MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean; overload;
  end;

  IdzEditValidator = interface ['{0FAA1AA1-484C-4132-9667-84893AA2BE3F}']
    ///<summary> Sets the color according to the Condition parameter </summary>
    procedure SetColor(_OK: Boolean); deprecated; // use Validate instead
    ///<summary> Sets the color according to the Condition parameter
    ///          @returns the value of Condition (for chaining) </summary>
    function Validate(_Condition: Boolean): Boolean; overload;
    function Validate(_Condition: Boolean; const _Message: string): Boolean; overload;
    ///<summary> Returns an IdzEditIntegerValidator that handles TEdits containing integer values </summary>
    function AsInteger: IdzTextControlIntegerValidator;
    ///<summary> Returns an IdzEditFloatValidator that handles TEdits containing float values </summary>
    function AsFloat: IdzTextControlFloatValidator;
    ///<summary> Returns a IdzEditDateValidator that handles TEdits containing date values </summary>
    function AsDate: IdzTextControlDateValidator;
    ///<summary> Returns a IdzEditTimeValidator that handles TEdits containing time of day values </summary>
    function AsTime: IdzTextControlTimeValidator;
    ///<summary> Returns a IdzEditTimeValidator that handles TEdits containing IPv4 adresses </summary>
    function AsIpv4: IdzTextControlIpV4Validator;
    ///<summary> Returs a IdzEditTextValidator that handles TEdits containing just text </summary>
    function AsText: IdzTextControlTextValidator;
    ///<summary> Checks whether the edit control contains a float or is empty and colors it accordingly </summary>
    function ValidateFloatOrEmtpy: Boolean; deprecated; // use AsFloat.IsValidOrEmpty instead
    ///<summary> Checks whether the edit control contains a float and colors it accordingly </summary>
    function ValidateFloat: Boolean; deprecated; // use AsFloat.IsValid instead
    ///<summary> Checks whether the edit control contains a float in the given range and colors it
    ///          accordingly </summary>
    function ValidateFloatBetween(_MinValue, _MaxValue: Extended): Boolean; deprecated; // use AsFloat.IsBetween instead
    ///<summary> Converts the edit control's content to a float. If it cannot be converted, it
    ///          raises an exception. </summary>
    function ToFloat(_FocusControl: Boolean = True): Extended; deprecated; // use AsFloat.GetValue instead
    ///<summary> Tries to convert the edit control's content to a float. Returns false if it
    ///          cannot be converted. </summary>
    function TryToFloat(out _Value: Extended): Boolean; overload; deprecated; // use AsFloat.TryGetValue instead
    ///<summary> Tries to convert the edit control's content to a float. Returns false if it
    ///          cannot be converted. </summary>
    function TryToFloat(out _Value: Double): Boolean; overload; deprecated; // use AsFloat.TryGetValue instead
    ///<summary> Tries to convert the edit control's content to a float. Returns false if it
    ///          cannot be converted. </summary>
    function TryToFloat(out _Value: Single): Boolean; overload; deprecated; // use AsFloat.TryGetValue instead
    ///<summary> Checks whether the edit control contains an integer and colors it accordingly </summary>
    function ValidateInteger: Boolean; deprecated; // use AsInteger.IsValid instead
    ///<summary> Checks whether the edit control contains an integer or is empty and colors it accordingly </summary>
    function ValidateIntegerOrEmpty: Boolean; deprecated; // use AsInteger.IsValidOrEmpty instead
    ///<summary> Checks whether the edit control contains an integer in the given range and colors it
    ///          accordingly </summary>
    function ValidateIntegerBetween(_MinValue, _MaxValue: Integer): Boolean; deprecated; // use AsInteger.IsBetween instead
    ///<summary> Converts the edit control's content to an integer. If it cannot be converted, it
    ///          raises an exception. </summary>
    function ToInteger(_FocusControl: Boolean = True): Integer; deprecated; // use AsInteger.GetValue instead
    ///<summary> Tries to convert the edit control's content to an integer. Returns false if it
    ///          cannot be converted. </summary>
    function TryToInteger(out _Value: Integer): Boolean; deprecated; // use AsInteger.TryGetValue instead
    ///<summary> Checks whether the TEdit's text is not empty and colors it accordingly </summary>
    function ValidateHasText(_MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean; deprecated; // use AsText.IsValid instead
    ///<summary> Checks whether the TEdit's text is a valid date and colors it accordingly. </summary>
    function ValidateDate: Boolean; deprecated; // use AsDate.IsValid instead
    ///<summary> Checks whether the TEdit's text is a valid date or empty and colors it accordingly. </summary>
    function ValidateDateOrEmpty: Boolean; deprecated; // use AsDate.IsValidOrEmpty instead
  end;

  IdzMemoValidator = interface ['{A4619443-0905-4A07-B905-465247A76FDA}']
    function ValidateLineCount(_Minimum: Integer = 1): Boolean;
    function Validate(_Condition: Boolean): Boolean; overload;
    function Validate(_Condition: Boolean; const _Message: string): Boolean; overload;
    function ValidateTrue: Boolean;
    function ValidateFalse(const _ErrorMessage: string): Boolean;
  end;

  IdzComboboxValidator = interface ['{B443303D-CEC3-4B1C-885C-6A8B76695FD9}']
    ///<summary> Sets the color according to the Condition parameter
    ///          @returns the value of Condition (for chaining) </summary>
    function Validate(_Condition: Boolean): Boolean; overload;
    function Validate(_Condition: Boolean; const _Message: string): Boolean; overload;
    ///<summary> Checks whether the combobox's text is not empty and colors it accordingly </summary>
    function ValidateHasText: Boolean; overload;
    ///<summary> checks the combobox's text </summary>
    function AsText: IdzTextControlTextValidator;
    ///<summary> treats the combobox's text as integer </summary>
    function AsInteger: IdzTextControlIntegerValidator;
    ///<summary> treats the combobox's text as float </summary>
    function AsFloat: IdzTextControlFloatValidator;
    ///<summary> treats the combobox's text as time </summary>
    function AsTime: IdzTextControlTimeValidator;
    ///<summary> treats the combobox's text as date </summary>
    function AsDate: IdzTextControlDateValidator;
    ///<summary> treats the combobox's text as IPv4 </summary>
    function AsIpv4: IdzTextControlIpV4Validator;
    ///<summary> Checks whether the combobox's text is not empty and colors it accordingly
    ///          @param Text returns the content of the Text property </summary>
    function ValidateHasText(out _Text: string): Boolean; overload;
    ///<summary> Checks whether any item of the combobox is selected and colors it accordingly </summary>
    function ValidateAnyItemSelected: Boolean; overload;
    ///<summary> Checks whether any item of the combobox is selected and colors it accordingly
    ///          In addition, returns the text of the selected item.
    ///          @param Text is the text of the selected item, only valid if Result = true </summary>
    function ValidateAnyItemSelected(out _Text: string): Boolean; overload;
    ///<summary> Checks whether any item of the combobox is selected and colors it accordingly
    ///          In addition, returns the index of the selected item.
    ///          @param Idx is the index of the selected item, only valid if Result = true </summary>
    function ValidateAnyItemSelected(out _Idx: Integer): Boolean; overload;
    ///<summary> Checks whether any item of the combobox is selected and colors it accordingly
    ///          In addition, returns the object associated with the selected item.
    ///          @param Obj is the object associated with the selected item, only valid if Result = true </summary>
    function ValidateAnyItemSelected(out _Obj: Pointer): Boolean; overload;
  end;

  IdzCheckboxValidator = interface ['{5B2974A5-A433-4874-BD3E-CC86E7489AA3}']
    function ValidateIsChecked(_Message: string = ''): Boolean;
    function ValidateIsUnchecked(_Message: string = ''): Boolean;
    function ValidateIsNotGrayed(_Message: string = ''): Boolean;
    function Validate(_Condition: Boolean; const _Message: string): Boolean; overload;
    function Validate(_Value: Boolean): Boolean; overload;
  end;

  IdzCheckListBoxValidator = interface ['{C06F9B2F-8F4E-46DA-A6D8-2B420B8CCCDD}']
    function ValidateHasCheckedItems(_Items: TStrings = nil): Boolean;
    function Validate(_Value: Boolean): Boolean;
    function AtLeastOneChecked: Boolean;
  end;

  IdzInputValidator = interface ['{34A614E5-D47F-4FDB-A2A1-8A8E6C0C0410}']
    procedure Reset;
    function Check(_ed: TCustomEdit): IdzEditValidator; overload;
    function Check(_cmb: TCustomComboBox): IdzComboboxValidator; overload;
    function Check(_Chk: TCustomCheckBox): IdzCheckboxValidator; overload;
    function Check(_Value: Boolean): TColor; overload;
    ///<summary>
    /// Checks whether Value is true, if not, sets the internal state to false and
    /// sets the internal error message.
    /// Returns the value of the parameter Value. </summary>
    function Check(_Value: Boolean; const _Message: string): Boolean; overload;
    function GetResult: Boolean; overload;
    function GetResult(out _ErrorMessage: string): Boolean; overload;
    procedure SetErrorMessage(const _Msg: string); overload;
    procedure SetErrorMessage(_Ctrl: TControl; const _Message: string); overload;
    procedure EnableMessages(_Enabled: Boolean);
    function GetColor(_OK: Boolean): TColor;
    function OkColor: TColor;
    function ErrorColor: TColor;
  end;

function InputValidator(_OkColor: TColor = clWindow; _ErrColor: TColor = clYellow): IdzInputValidator;

function ControlValidator(_m: TMemo; _iv: IdzInputValidator;
  _Container: IdzControlTextSource = nil): IdzMemoValidator; overload;

function ControlValidator(_ed: TCustomEdit; _iv: IdzInputValidator;
  _Container: IdzControlTextSource = nil): IdzEditValidator; overload;

function ControlValidator(_cmb: TCustomComboBox; _iv: IdzInputValidator): IdzComboboxValidator; overload;

function ControlValidator(_Chk: TCustomCheckBox; _iv: IdzInputValidator): IdzCheckboxValidator; overload;

function ControlValidator(_clb: TCheckListBox; _iv: IdzInputValidator): IdzCheckListBoxValidator; overload;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

uses
  ExtCtrls,
  Math,
  u_dzVclUtils,
  u_dzStringUtils,
  u_dzDateUtils,
  u_dzConvertUtils,
  u_dzInputValidatorMarkerList;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

type
  TInputValidator = class(TInterfacedObject, IdzInputValidator)
  strict private
    FOkColor: TColor;
    FErrColor: TColor;
    FMessagesEnabled: Boolean;
    FResult: Boolean;
    FInputValidatorShapeList: TInputValidatorShapeList;
  private
    FErrorMessage: string;
  protected
    procedure Reset;
    function Check(_ed: TCustomEdit): IdzEditValidator; overload;
    function Check(_cmb: TCustomComboBox): IdzComboboxValidator; overload;
    function Check(_Chk: TCustomCheckBox): IdzCheckboxValidator; overload;
    function Check(_Value: Boolean): TColor; overload;
    function Check(_Value: Boolean; const _Message: string): Boolean; overload;
    function GetResult: Boolean; overload;
    function GetResult(out _ErrorMessage: string): Boolean; overload;
    procedure SetErrorMessage(_Ctrl: TControl; const _Message: string); overload;
    procedure SetErrorMessage(const _Msg: string); overload;
    function GetColor(_OK: Boolean): TColor;
    function OkColor: TColor;
    function ErrorColor: TColor;
    procedure EnableMessages(_Enabled: Boolean);
  public
    constructor Create(_OkColor, _ErrColor: TColor);
    destructor Destroy; override;
  end;

type
  TEditValidator = class(TTextControlValidator, IdzEditValidator)
  private
    function AsInteger: IdzTextControlIntegerValidator;
    function AsFloat: IdzTextControlFloatValidator;
    function AsTime: IdzTextControlTimeValidator;
    function AsDate: IdzTextControlDateValidator;
    function AsIpv4: IdzTextControlIpV4Validator;
    function AsText: IdzTextControlTextValidator;
    function ValidateFloat: Boolean;
    function ValidateFloatOrEmtpy: Boolean;
    function ValidateFloatBetween(_MinValue, _MaxValue: Extended): Boolean;
    function ToFloat(_FocusControl: Boolean = True): Extended;
    function TryToFloat(out _Value: Extended): Boolean; overload;
    function TryToFloat(out _Value: Double): Boolean; overload;
    function TryToFloat(out _Value: Single): Boolean; overload;
    function ValidateInteger: Boolean;
    function ValidateIntegerBetween(_MinValue, _MaxValue: Integer): Boolean;
    function ValidateIntegerOrEmpty: Boolean;
    function ToInteger(_FocusControl: Boolean = True): Integer;
    function TryToInteger(out _Value: Integer): Boolean;
    function ValidateHasText(_MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean;
    function ValidateDate: Boolean;
    function ValidateDateOrEmpty: Boolean;
  public
    constructor Create(_ed: TCustomEdit; _Validator: IdzInputValidator;
      _Container: IdzControlTextSource = nil);
  end;

type
  TMemoValidator = class(TTextControlValidator, IdzMemoValidator)
  private
    function ValidateLineCount(_Minimum: Integer = 1): Boolean;
  protected
    function doGetText: string; override;
  public
    constructor Create(_m: TMemo; _Validator: IdzInputValidator; _Container: IdzControlTextSource);
  end;

type
  TTextControlDateValidator = class(TTextControlValidator, IdzTextControlDateValidator)
  private
    function IsValid: Boolean; overload;
    function IsValid(out _Value: TDate): Boolean; overload;
    function IsValidOrEmpty: Boolean;
    function GetValue(_FocusControl: Boolean = True): TDate;
    function TryGetValue(out _Value: TDate): Boolean; overload;
    function TryGetValue(out _Value: TDate; out _s: string): Boolean; overload;
    function IsBetween(_MinDate, _MaxDate: TDate): Boolean;
    function IsBetweenOrEmpty(_MinDate, _MaxDate: TDate): Boolean;
  end;

type
  TTextControlTimeValidator = class(TTextControlValidator, IdzTextControlTimeValidator)
  private
    function IsValid: Boolean;
    function IsValidOrEmpty: Boolean;
    function GetValue(_FocusControl: Boolean = True): TDate;
    function TryGetValue(out _Value: TDateTime): Boolean; overload;
    function TryGetValue(out _Value: TDateTime; out _s: string): Boolean; overload;
    function IsBetween(_MinTime, _MaxTime: TDateTime): Boolean;
  end;

type
  TTextControlFloatValidator = class(TTextControlValidator, IdzTextControlFloatValidator)
  private
    function GetValue(_FocusControl: Boolean = True): Extended;
    function IsBetween(_MinValue: Extended; _MaxValue: Extended): Boolean;
    function IsBetweenOrEmpty(_MinValue, _MaxValue: Extended): Boolean;
    function IsValid: Boolean;
    function IsValidOrEmtpy: Boolean;
    function IsValidOrEmpty: Boolean;
    function IsGreaterThanOrEmpty(_MinValue: Extended): Boolean;
    function IsGreaterOrEqual(_MinValue: Extended): Boolean;
    function IsGreaterThan(_MinValue: Extended): Boolean;
    function IsGreaterOrEqualOrEmpty(_MinValue: Extended): Boolean;
    function IsLessOrEqual(_MaxValue: Extended): Boolean;
    function IsLessOrEqualOrEmpty(_MaxValue: Extended): Boolean;
    function IsLessThan(_MaxValue: Extended): Boolean;
    function IsLessThanOrEmpty(_MaxValue: Extended): Boolean;
    function TryGetValue(out _Value: Extended): Boolean; overload;
    function TryGetValue(out _Value: Extended; out _s: string): Boolean; overload;
    function TryGetValue(out _Value: Double): Boolean; overload;
    function TryGetValue(out _Value: Double; out _s: string): Boolean; overload;
    function TryGetValue(out _Value: Single): Boolean; overload;
    function TryGetValue(out _Value: Single; out _s: string): Boolean; overload;
  public
  end;

type
  TTextControlIntegerValidator = class(TTextControlValidator, IdzTextControlIntegerValidator)
  private
    function GetValue(_FocusControl: Boolean = True): Integer;
    function TryGetValue(out _Value: Integer): Boolean; overload;
    function TryGetValue(out _Value: Integer; out _s: string): Boolean; overload;
    function IsValid: Boolean; overload;
    function IsValid(out _Value: Integer): Boolean; overload;
    function IsValidOrEmpty: Boolean;
    function IsBetween(_MinValue: Integer; _MaxValue: Integer): Boolean;
    function IsBetweenOrEmpty(_MinValue, _MaxValue: Integer): Boolean;
    function IsLessThan(_MaxValue: Integer): Boolean;
    function IsLessThanOrEmpty(_MaxValue: Integer): Boolean;
    function IsLessThanOrEqual(_MaxValue: Integer): Boolean;
    function IsGreaterThan(_MinValue: Integer): Boolean;
    function IsGreaterThanOrEmpty(_MinValue: Integer): Boolean;
    function IsGreaterOrEqual(_MinValue: Integer): Boolean;
    function IsGreaterOrEqualOrEmpty(_MinValue: Integer): Boolean;
  public
  end;

type
  TTextControlIpV4Validator = class(TTextControlValidator, IdzTextControlIpV4Validator)
  private
    function IsValid: Boolean;
    function IsValidOrEmpty: Boolean;
    function TryGetValue(out _Value: string): Boolean; overload;
  end;

type
  TTextControlTextValidator = class(TTextControlValidator, IdzTextControlTextValidator)
  private
    function IsValid(_MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean; overload;
    function IsValid(out _Text: string; _MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean; overload;
  end;

type
  TComboBoxHack = class(TCustomComboBox);

type
  TComboboxValidator = class(TTextControlValidator, IdzComboboxValidator)
  private
    function Combo: TComboBoxHack;
    function AsText: IdzTextControlTextValidator;
    ///<summary> treats the combobox's text as integer </summary>
    function AsInteger: IdzTextControlIntegerValidator;
    ///<summary> treats the combobox's text as float </summary>
    function AsFloat: IdzTextControlFloatValidator;
    ///<summary> treats the combobox's text as time </summary>
    function AsTime: IdzTextControlTimeValidator;
    ///<summary> treats the combobox's text as date </summary>
    function AsDate: IdzTextControlDateValidator;
    ///<summary> treats the combobox's text as IPv4 </summary>
    function AsIpv4: IdzTextControlIpV4Validator;
    function ValidateHasText: Boolean; overload;
    function ValidateHasText(out _Text: string): Boolean; overload;
    function ValidateAnyItemSelected: Boolean; overload;
    function ValidateAnyItemSelected(out _Text: string): Boolean; overload;
    function ValidateAnyItemSelected(out _Idx: Integer): Boolean; overload;
    function ValidateAnyItemSelected(out _Obj: Pointer): Boolean; overload;
  public
    constructor Create(_cmb: TCustomComboBox; _Validator: IdzInputValidator;
      _Container: IdzControlTextSource = nil);
  end;

  TCheckBoxHack = class(TCustomCheckBox)
  end;
  TCheckboxValidator = class(TControlValidator, IdzCheckboxValidator)
  private
    function Checkbox: TCheckBoxHack;
    function Validate(_Condition: Boolean): Boolean; override;
    function Validate(_Condition: Boolean; const _Message: string): Boolean; override;
    function ValidateIsChecked(_Message: string = ''): Boolean;
    function ValidateIsUnchecked(_Message: string = ''): Boolean;
    function ValidateIsNotGrayed(_Message: string = ''): Boolean;
  public
    constructor Create(_Chk: TCustomCheckBox; _Validator: IdzInputValidator);
  end;

  TCheckListBoxValidator = class(TControlValidator, IdzCheckListBoxValidator)
  private
    function ValidateHasCheckedItems(_Items: TStrings = nil): Boolean;
    function CheckListBox: TCheckListBox;
    function AtLeastOneChecked: Boolean;
  protected
    procedure doSetColor(_Color: TColor); override;
  public
    constructor Create(_clb: TCheckListBox; _Validator: IdzInputValidator);
  end;

{ TInputValidator }

constructor TInputValidator.Create(_OkColor, _ErrColor: TColor);
begin
  inherited Create;
  FOkColor := _OkColor;
  FErrColor := _ErrColor;
  FMessagesEnabled := False;
  FInputValidatorShapeList := TInputValidatorShapeList.Create;
  FResult := True;
end;

destructor TInputValidator.Destroy;
begin
  FreeAndNil(FInputValidatorShapeList);
  inherited;
end;

procedure TInputValidator.EnableMessages(_Enabled: Boolean);
var
  i: Integer;
begin
  FMessagesEnabled := _Enabled;
  if not FMessagesEnabled then begin
    for i := 0 to FInputValidatorShapeList.Count - 1 do
      FInputValidatorShapeList[i].Image.Free;
    FInputValidatorShapeList.Clear;
  end;
end;

procedure TInputValidator.Reset;
var
  i: Integer;
begin
  FResult := True;
  FErrorMessage := '';
  for i := 0 to FInputValidatorShapeList.Count - 1 do begin
    FInputValidatorShapeList[i].Image.Visible := False;
  end;
end;

function TInputValidator.GetColor(_OK: Boolean): TColor;
begin
  if _OK then
    Result := FOkColor
  else
    Result := FErrColor;
end;

function TInputValidator.OkColor: TColor;
begin
  Result := FOkColor;
end;

function TInputValidator.ErrorColor: TColor;
begin
  Result := FErrColor;
end;

function TInputValidator.GetResult(out _ErrorMessage: string): Boolean;
begin
  Result := FResult;
  _ErrorMessage := FErrorMessage;
end;

function TInputValidator.GetResult: Boolean;
begin
  Result := FResult;
end;

function TInputValidator.Check(_Value: Boolean): TColor;
begin
  if _Value then
    Result := FOkColor
  else begin
    FResult := False;
    Result := FErrColor
  end;
end;

function TInputValidator.Check(_Value: Boolean; const _Message: string): Boolean;
begin
  Result := _Value;
  if not _Value then begin
    FResult := False;
    SetErrorMessage(_Message);
  end;
end;

procedure TInputValidator.SetErrorMessage(_Ctrl: TControl; const _Message: string);
const
  bmp = '424DC60400000000000036040000280000000C00'
    + '00000C000000010008000000000090000000C30E0000C30E0000000100000000'
    + '000000000000000080000080000000808000800000008000800080800000C0C0'
    + 'C000C0DCC000F0CAA6000020400000206000002080000020A0000020C0000020'
    + 'E00000400000004020000040400000406000004080000040A0000040C0000040'
    + 'E00000600000006020000060400000606000006080000060A0000060C0000060'
    + 'E00000800000008020000080400000806000008080000080A0000080C0000080'
    + 'E00000A0000000A0200000A0400000A0600000A0800000A0A00000A0C00000A0'
    + 'E00000C0000000C0200000C0400000C0600000C0800000C0A00000C0C00000C0'
    + 'E00000E0000000E0200000E0400000E0600000E0800000E0A00000E0C00000E0'
    + 'E00040000000400020004000400040006000400080004000A0004000C0004000'
    + 'E00040200000402020004020400040206000402080004020A0004020C0004020'
    + 'E00040400000404020004040400040406000404080004040A0004040C0004040'
    + 'E00040600000406020004060400040606000406080004060A0004060C0004060'
    + 'E00040800000408020004080400040806000408080004080A0004080C0004080'
    + 'E00040A0000040A0200040A0400040A0600040A0800040A0A00040A0C00040A0'
    + 'E00040C0000040C0200040C0400040C0600040C0800040C0A00040C0C00040C0'
    + 'E00040E0000040E0200040E0400040E0600040E0800040E0A00040E0C00040E0'
    + 'E00080000000800020008000400080006000800080008000A0008000C0008000'
    + 'E00080200000802020008020400080206000802080008020A0008020C0008020'
    + 'E00080400000804020008040400080406000804080008040A0008040C0008040'
    + 'E00080600000806020008060400080606000806080008060A0008060C0008060'
    + 'E00080800000808020008080400080806000808080008080A0008080C0008080'
    + 'E00080A0000080A0200080A0400080A0600080A0800080A0A00080A0C00080A0'
    + 'E00080C0000080C0200080C0400080C0600080C0800080C0A00080C0C00080C0'
    + 'E00080E0000080E0200080E0400080E0600080E0800080E0A00080E0C00080E0'
    + 'E000C0000000C0002000C0004000C0006000C0008000C000A000C000C000C000'
    + 'E000C0200000C0202000C0204000C0206000C0208000C020A000C020C000C020'
    + 'E000C0400000C0402000C0404000C0406000C0408000C040A000C040C000C040'
    + 'E000C0600000C0602000C0604000C0606000C0608000C060A000C060C000C060'
    + 'E000C0800000C0802000C0804000C0806000C0808000C080A000C080C000C080'
    + 'E000C0A00000C0A02000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0'
    + 'E000C0C00000C0C02000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0'
    + 'A000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF'
    + 'FF00F6F6F6F6F9F9F9F9F6F6F6F6F6F6F9F9F9F9F9F9F9F9F6F6F6F9F9F9F9FF'
    + 'FFF9F9F9F9F6F6F9F9F9F9FFFFF9F9F9F9F6F9F9F9F9F9F9F9F9F9F9F9F9F9F9'
    + 'F9F9F9F9F9F9F9F9F9F9F9F9F9F9F9FFFFF9F9F9F9F9F9F9F9F9F9FFFFF9F9F9'
    + 'F9F9F6F9F9F9F9FFFFF9F9F9F9F6F6F9F9F9F9FFFFF9F9F9F9F6F6F6F9F9F9F9'
    + 'F9F9F9F9F6F6F6F6F6F6F9F9F9F9F6F6F6F6';
var
  im: TImage;
  Item: TControlShapeLink;
begin
  SetErrorMessage(_Message);
  if not FMessagesEnabled then
    Exit;
  if FInputValidatorShapeList.Find(_Ctrl.Name, Item) then begin
    im := Item.Image;
  end else begin
    im := TImage.Create(_Ctrl.Owner);
    with im do begin
      im.Name := '';
      im.Parent := _Ctrl.Parent;
      TBitmap_LoadFromString(im.Picture.Bitmap, bmp, False);
      im.TRANSPARENT := True;
      im.AutoSize := True;
      im.Left := _Ctrl.Left + _Ctrl.Width;
      im.Top := _Ctrl.Top + Round(_Ctrl.Height / 2 - im.Height / 2);
      im.ShowHint := True;
    end;
    FInputValidatorShapeList.Add(TControlShapeLink.Create(_Ctrl.Name, im));
  end;
  im.Hint := _Message;
  im.Visible := (_Message <> '');
end;

function TInputValidator.Check(_ed: TCustomEdit): IdzEditValidator;
begin
  Result := TEditValidator.Create(_ed, Self);
end;

function TInputValidator.Check(_cmb: TCustomComboBox): IdzComboboxValidator;
begin
  Result := TComboboxValidator.Create(_cmb, Self);
end;

function TInputValidator.Check(_Chk: TCustomCheckBox): IdzCheckboxValidator;
begin
  Result := TCheckboxValidator.Create(_Chk, Self);
end;

procedure TInputValidator.SetErrorMessage(const _Msg: string);
begin
  if FErrorMessage = '' then
    FErrorMessage := _Msg;
end;

{ TControlValidator }

constructor TControlValidator.Create(_Ctrl: TControl; _Validator: IdzInputValidator);
begin
  inherited Create;
  FControl := _Ctrl;
  FValidator := _Validator;
end;

function TControlValidator.ValidateTrue: Boolean;
begin
  Result := Validate(True, '');
end;

function TControlValidator.ValidateFalse(const _ErrorMessage: string): Boolean;
begin
  Result := Validate(False, _ErrorMessage);
end;

procedure TControlValidator.doSetColor(_Color: TColor);
begin
  Control.Color := _Color;
end;

function TControlValidator.Control: TControlHack;
begin
  Result := TControlHack(FControl);
end;

procedure TControlValidator.SetColor(_OK: Boolean);
begin
  Validate(_OK);
end;

function TControlValidator.Validate(_Condition: Boolean; const _Message: string): Boolean;
var
  TheColor: TColor;
begin
  TheColor := FValidator.Check(_Condition);
  doSetColor(TheColor);

  if _Condition then begin
    Control.Hint := '';
    Control.ShowHint := False;
  end else begin
    Control.Hint := _Message;
    Control.ShowHint := True;
    FValidator.SetErrorMessage(Control, _Message);
  end;

  Result := _Condition;
end;

function TControlValidator.Validate(_Condition: Boolean): Boolean;
begin
  doSetColor(FValidator.Check(_Condition));
  if _Condition then begin
    Control.Hint := '';
    Control.ShowHint := False;
  end;
  Result := _Condition;
end;

{ TTextControlValidator }

constructor TTextControlValidator.Create(_Ctrl: TControl; _Validator: IdzInputValidator;
  _Container: IdzControlTextSource);
begin
  inherited Create(_Ctrl, _Validator);
  FContainer := _Container;
end;

procedure TTextControlValidator.doFocusControl;
begin
  TWinControl_SetFocus(FControl as TWinControl);
end;

type
  TEditHack = class(TCustomEdit);

function TTextControlValidator.doGetText: string;
begin
  if Assigned(FContainer) then
    Result := FContainer.GetText
  else begin
    if FControl is TCustomEdit then
      Result := TEditHack(FControl).Text
    else if FControl is TCustomComboBox then
      Result := TComboBoxHack(FControl).Text
    else
      Result := '';
  end;
end;

{ TEditValidator }

function TEditValidator.AsDate: IdzTextControlDateValidator;
begin
  Result := TTextControlDateValidator.Create(Control, FValidator, FContainer);
end;

function TEditValidator.AsFloat: IdzTextControlFloatValidator;
begin
  Result := TTextControlFloatValidator.Create(Control, FValidator, FContainer);
end;

function TEditValidator.AsInteger: IdzTextControlIntegerValidator;
begin
  Result := TTextControlIntegerValidator.Create(Control, FValidator, FContainer);
end;

function TEditValidator.AsIpv4: IdzTextControlIpV4Validator;
begin
  Result := TTextControlIpV4Validator.Create(Control, FValidator, FContainer);
end;

function TEditValidator.AsText: IdzTextControlTextValidator;
begin
  Result := TTextControlTextValidator.Create(Control, FValidator, FContainer);
end;

function TEditValidator.AsTime: IdzTextControlTimeValidator;
begin
  Result := TTextControlTimeValidator.Create(Control, FValidator, FContainer);
end;

constructor TEditValidator.Create(_ed: TCustomEdit; _Validator: IdzInputValidator;
  _Container: IdzControlTextSource = nil);
var
  TheContainer: IdzControlTextSource;
begin
  if Assigned(_Container) then
    TheContainer := _Container
  else
    TheContainer := nil;
  inherited Create(_ed, _Validator, TheContainer);
end;

function TEditValidator.ValidateHasText(_MinLen: Integer = 1; _MaxLen: Integer = MaxInt): Boolean;
begin
  Result := AsText.IsValid(_MinLen, _MaxLen);
end;

function TEditValidator.ValidateDate: Boolean;
begin
  Result := AsDate.IsValid;
end;

function TEditValidator.ValidateDateOrEmpty: Boolean;
begin
  Result := AsDate.IsValidOrEmpty;
end;

function TEditValidator.ValidateFloat: Boolean;
begin
  Result := AsFloat.IsValid;
end;

function TEditValidator.ValidateFloatBetween(_MinValue, _MaxValue: Extended): Boolean;
begin
  Result := AsFloat.IsBetween(_MinValue, _MaxValue);
end;

function TEditValidator.ValidateFloatOrEmtpy: Boolean;
begin
  Result := AsFloat.IsValidOrEmpty;
end;

function TEditValidator.ValidateInteger: Boolean;
begin
  Result := AsInteger.IsValid;
end;

function TEditValidator.ValidateIntegerBetween(_MinValue, _MaxValue: Integer): Boolean;
begin
  Result := AsInteger.IsBetween(_MinValue, _MaxValue);
end;

function TEditValidator.ValidateIntegerOrEmpty: Boolean;
begin
  Result := AsInteger.IsValidOrEmpty;
end;

function TEditValidator.ToFloat(_FocusControl: Boolean): Extended;
begin
  Result := AsFloat.GetValue(_FocusControl);
end;

function TEditValidator.ToInteger(_FocusControl: Boolean): Integer;
begin
  Result := AsInteger.GetValue(_FocusControl);
end;

function TEditValidator.TryToFloat(out _Value: Extended): Boolean;
begin
  Result := AsFloat.TryGetValue(_Value);
end;

function TEditValidator.TryToFloat(out _Value: Double): Boolean;
begin
  Result := AsFloat.TryGetValue(_Value);
end;

function TEditValidator.TryToFloat(out _Value: Single): Boolean;
begin
  Result := AsFloat.TryGetValue(_Value);
end;

function TEditValidator.TryToInteger(out _Value: Integer): Boolean;
begin
  Result := AsInteger.TryGetValue(_Value);
end;

{ TEditDateValidator }

function TTextControlDateValidator.GetValue(_FocusControl: Boolean): TDate;
var
  s: string;
begin
  if not TryGetValue(Result, s) then begin
    if _FocusControl then
      doFocusControl;
    raise EConvertError.CreateFmt(_('"%s" is not a valid date value.'), [s]);
  end;
end;

function TTextControlDateValidator.IsBetween(_MinDate, _MaxDate: TDate): Boolean;
var
  Value: TDate;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (Value >= _MinDate) and (Value <= _MaxDate);
  Validate(Result, Format(_('Value must be between %s and %s.'),
    [DateToStr(_MinDate), DateToStr(_MaxDate)]));
end;

function TTextControlDateValidator.IsBetweenOrEmpty(_MinDate, _MaxDate: TDate): Boolean;
var
  Value: TDate;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (Value >= _MinDate) and (Value <= _MaxDate)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be between %s and %s or empty.'),
    [DateToStr(_MinDate), DateToStr(_MaxDate)]));
end;

function TTextControlDateValidator.IsValid: Boolean;
var
  Value: TDate;
begin
  Result := TryGetValue(Value);
  Validate(Result, _('Value must be a date.'));
end;

function TTextControlDateValidator.IsValid(out _Value: TDate): Boolean;
begin
  Result := TryGetValue(_Value);
  Validate(Result, _('Value must be a date.'));
end;

function TTextControlDateValidator.IsValidOrEmpty: Boolean;
var
  Value: TDate;
  s: string;
begin
  Result := TryGetValue(Value, s) or (s = '');
  Validate(Result, _('Value must be a date or empty.'));
end;

function TTextControlDateValidator.TryGetValue(out _Value: TDate; out _s: string): Boolean;
var
  dt: TDateTime;
begin
  _s := doGetText;
  Result := TryStr2Date(_s, dt);
  if Result then
    _Value := Trunc(dt);
end;

function TTextControlDateValidator.TryGetValue(out _Value: TDate): Boolean;
var
  s: string;
begin
  Result := TryGetValue(_Value, s);
end;

{ TEditTimeValidator }

function TTextControlTimeValidator.GetValue(_FocusControl: Boolean): TDate;
var
  s: string;
  dt: TDateTime;
begin
  if TryGetValue(dt, s) then begin
    Result := Int(dt);
  end else begin
    if _FocusControl then
      doFocusControl;
    raise EConvertError.CreateFmt(_('"%s" is not a valid time of day value.'), [s]);
  end;
end;

function TTextControlTimeValidator.IsBetween(_MinTime, _MaxTime: TDateTime): Boolean;
var
  Value: TDateTime;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (Value >= _MinTime) and (Value <= _MaxTime);
  Validate(Result, Format(_('Value must be between %s and %s.'),
    [TimeToStr(_MinTime), TimeToStr(_MaxTime)]));
end;

function TTextControlTimeValidator.IsValid: Boolean;
var
  dt: TDateTime;
begin
  Result := TryGetValue(dt);
  Validate(Result, _('Value must be a time of day.'));
end;

function TTextControlTimeValidator.IsValidOrEmpty: Boolean;
var
  dt: TDateTime;
  s: string;
begin
  Result := TryGetValue(dt, s) or (s = '');
  Validate(Result, _('Value must be a time of day or empty.'));
end;

function TTextControlTimeValidator.TryGetValue(out _Value: TDateTime): Boolean;
var
  s: string;
begin
  Result := TryGetValue(_Value, s);
end;

function TTextControlTimeValidator.TryGetValue(out _Value: TDateTime; out _s: string): Boolean;
var
  dt: TDateTime;
begin
  _s := doGetText;
  Result := TryStrToTime(_s, dt);
  if Result then
    _Value := Frac(dt);
end;

{ TEditFloatValidator }

function TTextControlFloatValidator.GetValue(_FocusControl: Boolean): Extended;
var
  s: string;
begin
  if not TryGetValue(Result, s) then begin
    if _FocusControl then begin
      doFocusControl;
    end;
    raise EConvertError.CreateFmt(_('"%s" is not a valid number.'), [s]);
  end;
end;

function TTextControlFloatValidator.IsBetween(_MinValue, _MaxValue: Extended): Boolean;
var
  Value: Extended;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (CompareValue(Value, _MinValue) >= 0) and (CompareValue(Value, _MaxValue) <= 0);
  Validate(Result, Format(_('Value must be between %s and %s.'),
    [Format('%.2f', [_MinValue]), Format('%.2f', [_MaxValue])]));
end;

function TTextControlFloatValidator.IsBetweenOrEmpty(_MinValue, _MaxValue: Extended): Boolean;
var
  Value: Extended;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (CompareValue(Value, _MinValue) >= 0) and (CompareValue(Value, _MaxValue) <= 0)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be between %s and %s or empty.'),
    [Format('%.2f', [_MinValue]), Format('%.2f', [_MaxValue])]));
end;

function TTextControlFloatValidator.IsGreaterOrEqual(_MinValue: Extended): Boolean;
var
  Value: Extended;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (CompareValue(Value, _MinValue) >= 0);
  Validate(Result, Format(_('Value must be greater than or equal %s.'),
    [Format('%.2f', [_MinValue])]));
end;

function TTextControlFloatValidator.IsGreaterOrEqualOrEmpty(_MinValue: Extended): Boolean;
var
  Value: Extended;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (CompareValue(Value, _MinValue) >= 0)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be greater than or equal %s or empty.'),
    [Format('%.2f', [_MinValue])]));
end;

function TTextControlFloatValidator.IsGreaterThan(_MinValue: Extended): Boolean;
var
  Value: Extended;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (CompareValue(Value, _MinValue) > 0);
  Validate(Result, Format(_('Value must be greater than %s.'),
    [Format('%.2f', [_MinValue])]));
end;

function TTextControlFloatValidator.IsGreaterThanOrEmpty(_MinValue: Extended): Boolean;
var
  Value: Extended;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (CompareValue(Value, _MinValue) > 0)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be greater than %s or empty.'),
    [Format('%.2f', [_MinValue])]));
end;

function TTextControlFloatValidator.IsLessOrEqual(_MaxValue: Extended): Boolean;
var
  Value: Extended;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (CompareValue(Value, _MaxValue) <= 0);
  Validate(Result, Format(_('Value must be smaller than or equal %s.'),
    [Format('%.2f', [_MaxValue])]));
end;

function TTextControlFloatValidator.IsLessOrEqualOrEmpty(_MaxValue: Extended): Boolean;
var
  Value: Extended;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (CompareValue(Value, _MaxValue) <= 0)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be smaller than or equal %s or empty.'),
    [Format('%.2f', [_MaxValue])]));
end;

function TTextControlFloatValidator.IsLessThan(_MaxValue: Extended): Boolean;
var
  Value: Extended;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (CompareValue(Value, _MaxValue) < 0);
  Validate(Result, Format(_('Value must be smaller than %s.'),
    [Format('%.2f', [_MaxValue])]));
end;

function TTextControlFloatValidator.IsLessThanOrEmpty(_MaxValue: Extended): Boolean;
var
  Value: Extended;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (CompareValue(Value, _MaxValue) < 0)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be smaller than %s or empty.'),
    [Format('%.2f', [_MaxValue])]));
end;

function TTextControlFloatValidator.IsValid: Boolean;
var
  Value: Extended;
begin
  Result := TryGetValue(Value);
  Validate(Result, _('Value must be a real number.'));
end;

function TTextControlFloatValidator.IsValidOrEmpty: Boolean;
var
  Value: Extended;
  s: string;
begin
  Result := TryGetValue(Value, s) or (s = '');
  Validate(Result, _('Value must be a real number or empty.'));
end;

function TTextControlFloatValidator.IsValidOrEmtpy: Boolean;
begin
  Result := IsValidOrEmpty;
end;

function TTextControlFloatValidator.TryGetValue(out _Value: Extended): Boolean;
var
  s: string;
begin
  Result := TryGetValue(_Value, s);
end;

function TTextControlFloatValidator.TryGetValue(out _Value: Extended; out _s: string): Boolean;
begin
  _s := doGetText;
  Result := TryStrToFloat(_s, _Value);
end;

function TTextControlFloatValidator.TryGetValue(out _Value: Double): Boolean;
var
  s: string;
begin
  Result := TryGetValue(_Value, s);
end;

function TTextControlFloatValidator.TryGetValue(out _Value: Double; out _s: string): Boolean;
begin
  _s := doGetText;
  Result := TryStrToFloat(_s, _Value);
end;

function TTextControlFloatValidator.TryGetValue(out _Value: Single): Boolean;
var
  s: string;
begin
  Result := TryGetValue(_Value, s);
end;

function TTextControlFloatValidator.TryGetValue(out _Value: Single; out _s: string): Boolean;
begin
  _s := doGetText;
  Result := TryStrToFloat(_s, _Value);
end;

{ TEditIntegerValidator }

//function TEditIntegerValidator.GetText;
//begin
//  if FControl is TCustomEdit then
//    Result := TEditHack(FControl).Text
//  else if FControl is TCustomComboBox then
//    Result := TComboBoxHack(FControl).Text;
//end;

function TTextControlIntegerValidator.GetValue(_FocusControl: Boolean): Integer;
var
  s: string;
begin
  if not TryGetValue(Result, s) then begin
    if _FocusControl then begin
      Control.SetFocus;
    end;
    raise EConvertError.CreateFmt(_('"%s" is not a valid integer value.'), [s]);
  end;
end;

function TTextControlIntegerValidator.TryGetValue(out _Value: Integer): Boolean;
var
  s: string;
begin
  Result := TryGetValue(_Value, s);
end;

function TTextControlIntegerValidator.TryGetValue(out _Value: Integer; out _s: string): Boolean;
begin
  _s := doGetText;
  Result := TryStrToInt(_s, _Value);
end;

function TTextControlIntegerValidator.IsValid(out _Value: Integer): Boolean;
begin
  Result := TryGetValue(_Value);
  Validate(Result, _('Value must be an integer number.'));
end;

function TTextControlIntegerValidator.IsValid: Boolean;
var
  Value: Integer;
begin
  Result := IsValid(Value);
end;

function TTextControlIntegerValidator.IsBetween(_MinValue, _MaxValue: Integer): Boolean;
var
  Value: Integer;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (Value >= _MinValue) and (Value <= _MaxValue);
  Validate(Result, Format(_('Value must be between %s and %s.'),
    [IntToStr(_MinValue), IntToStr(_MaxValue)]));
end;

function TTextControlIntegerValidator.IsBetweenOrEmpty(_MinValue, _MaxValue: Integer): Boolean;
var
  Value: Integer;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (Value >= _MinValue) and (Value <= _MaxValue)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be between %s and %s or empty.'),
    [IntToStr(_MinValue), IntToStr(_MaxValue)]));
end;

function TTextControlIntegerValidator.IsLessThan(_MaxValue: Integer): Boolean;
var
  Value: Integer;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (Value < _MaxValue);
  Validate(Result, Format(_('Value must be smaller than %s.'),
    [IntToStr(_MaxValue)]));
end;

function TTextControlIntegerValidator.IsLessThanOrEmpty(_MaxValue: Integer): Boolean;
var
  Value: Integer;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (Value < _MaxValue)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be smaller than %s or empty.'),
    [IntToStr(_MaxValue)]));
end;

function TTextControlIntegerValidator.IsLessThanOrEqual(_MaxValue: Integer): Boolean;
var
  Value: Integer;
begin
  Result := TryGetValue(Value);
  if Result then
    Result := (Value < _MaxValue);
  Validate(Result, Format(_('Value must be smaller than or equal %s.'),
    [IntToStr(_MaxValue)]));
end;

function TTextControlIntegerValidator.IsGreaterThan(_MinValue: Integer): Boolean;
var
  Value: Integer;
begin
  Result := TryGetValue(Value);
  if not Result then
    Validate(False, _('Value must be an integer number.'))
  else begin
    begin
      Result := (Value > _MinValue);
      Validate(Result, Format(_('Value must be greater than %s.'),
        [IntToStr(_MinValue)]));
    end;
  end;
end;

function TTextControlIntegerValidator.IsGreaterThanOrEmpty(_MinValue: Integer): Boolean;
var
  Value: Integer;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (Value > _MinValue)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be greater than %s or empty.'),
    [IntToStr(_MinValue)]));
end;

function TTextControlIntegerValidator.IsGreaterOrEqual(_MinValue: Integer): Boolean;
var
  Value: Integer;
begin
  Result := TryGetValue(Value);
  if not Result then
    Validate(False, _('Value must be an integer number.'))
  else begin
    Result := (Value >= _MinValue);
    Validate(Result, Format(_('Value must be greater than or equal %s.'),
      [IntToStr(_MinValue)]));
  end;
end;

function TTextControlIntegerValidator.IsGreaterOrEqualOrEmpty(_MinValue: Integer): Boolean;
var
  Value: Integer;
  s: string;
begin
  Result := TryGetValue(Value, s);
  if Result then
    Result := (Value >= _MinValue)
  else if s = '' then
    Result := True;
  Validate(Result, Format(_('Value must be greater than or equal %s or empty.'),
    [IntToStr(_MinValue)]));
end;

function TTextControlIntegerValidator.IsValidOrEmpty: Boolean;
var
  Value: Integer;
  s: string;
begin
  Result := TryGetValue(Value, s) or (s = '');
  Validate(Result, _('Value must be an integer number or empty.'));
end;

{ TTextControlTextValidator }

function TTextControlTextValidator.IsValid(out _Text: string; _MinLen, _MaxLen: Integer): Boolean;
var
  Len: Integer;
  s: string;
begin
  _Text := doGetText;
  Len := Length(_Text);
  Result := (Len >= _MinLen) and (Len <= _MaxLen);
  if Result then
    Validate(True)
  else begin
    s := '';
    if _MinLen > 0 then begin
      s := Format(_('Text is too short (min. %d characters)'), [_MinLen]);
    end else if _MaxLen <> MaxInt then begin
      s := Format(_('Text is too long (max. %d characters)'), [_MaxLen]);
    end;
    Validate(False, s);
  end;
end;

function TTextControlTextValidator.IsValid(_MinLen, _MaxLen: Integer): Boolean;
var
  s: string;
begin
  Result := IsValid(s, _MinLen, _MaxLen);
end;

{ TComboboxValidator }

constructor TComboboxValidator.Create(_cmb: TCustomComboBox; _Validator: IdzInputValidator;
  _Container: IdzControlTextSource = nil);
var
  TheContainer: IdzControlTextSource;
begin
  if Assigned(_Container) then
    TheContainer := _Container
  else
    TheContainer := nil;
  inherited Create(_cmb, _Validator, TheContainer);
end;

function TComboboxValidator.Combo: TComboBoxHack;
begin
  Result := TComboBoxHack(FControl as TCustomComboBox);
end;

function TComboboxValidator.AsIpv4: IdzTextControlIpV4Validator;
begin
  Result := TTextControlIpV4Validator.Create(FControl, FValidator, FContainer);
end;

function TComboboxValidator.AsText: IdzTextControlTextValidator;
begin
  Result := TTextControlTextValidator.Create(FControl, FValidator, FContainer);
end;

function TComboboxValidator.AsTime: IdzTextControlTimeValidator;
begin
  Result := TTextControlTimeValidator.Create(FControl, FValidator, FContainer);
end;

function TComboboxValidator.AsDate: IdzTextControlDateValidator;
begin
  Result := TTextControlDateValidator.Create(FControl, FValidator, FContainer);
end;

function TComboboxValidator.AsFloat: IdzTextControlFloatValidator;
begin
  Result := TTextControlFloatValidator.Create(FControl, FValidator, FContainer);
end;

function TComboboxValidator.AsInteger: IdzTextControlIntegerValidator;
begin
  Result := TTextControlIntegerValidator.Create(Combo, FValidator, FContainer);
end;

function TComboboxValidator.ValidateHasText: Boolean;
begin
  Result := Combo.Text <> '';
  Validate(Result, _('Value must not be empty.'));
end;

function TComboboxValidator.ValidateHasText(out _Text: string): Boolean;
begin
  _Text := Combo.Text;
  Result := (_Text <> '');
  Validate(Result, _('Value must not be empty.'));
end;

function TComboboxValidator.ValidateAnyItemSelected(out _Text: string): Boolean;
var
  Idx: Integer;
begin
  Result := ValidateAnyItemSelected(Idx);
  if Result then
    _Text := Combo.Items[Idx];
end;

function TComboboxValidator.ValidateAnyItemSelected(out _Idx: Integer): Boolean;
var
  s: string;
begin
  _Idx := Combo.ItemIndex;
  if _Idx = -1 then begin
    // It is possible that the ComboBox' text is set but the control didn't realize it is
    // also in its Items list, so we check for this here. This happens e.g. if
    // the Text was already set and the Items list was changed afterwards.
    s := Combo.Text;
    _Idx := Combo.Items.IndexOf(s);
    TComboBox_SelectWithoutChangeEvent(Combo, _Idx);
  end;
  Result := (_Idx <> -1);
  Validate(Result, _('No entry selected.'));
end;

function TComboboxValidator.ValidateAnyItemSelected(out _Obj: Pointer): Boolean;
var
  Idx: Integer;
begin
  Result := ValidateAnyItemSelected(Idx);
  if Result then
    _Obj := Combo.Items.Objects[Idx];
end;

function TComboboxValidator.ValidateAnyItemSelected: Boolean;
var
  Idx: Integer;
begin
  Result := ValidateAnyItemSelected(Idx);
end;

{ TCheckboxValidator }

constructor TCheckboxValidator.Create(_Chk: TCustomCheckBox; _Validator: IdzInputValidator);
begin
  inherited Create(_Chk, _Validator);
end;

function TCheckboxValidator.Checkbox: TCheckBoxHack;
begin
  Result := TCheckBoxHack(FControl as TCustomCheckBox);
end;

function TCheckboxValidator.Validate(_Condition: Boolean): Boolean;
begin
  Result := inherited Validate(_Condition);
  if Result then
    Checkbox.ParentColor := True;
end;

function TCheckboxValidator.Validate(_Condition: Boolean; const _Message: string): Boolean;
begin
  Result := inherited Validate(_Condition, _Message);
  if Result then
    Checkbox.ParentColor := True;
end;

function TCheckboxValidator.ValidateIsChecked(_Message: string = ''): Boolean;
begin
  if _Message = '' then
    _Message := _('Must be checked.');
  Result := Validate(Checkbox.Checked, _Message);
end;

function TCheckboxValidator.ValidateIsNotGrayed(_Message: string = ''): Boolean;
begin
  if _Message = '' then
    _Message := _('Must not be grayed.');
  Result := Validate(Checkbox.State <> cbGrayed, _Message);
end;

function TCheckboxValidator.ValidateIsUnchecked(_Message: string = ''): Boolean;
begin
  if _Message = '' then
    _Message := _('Must not be checked.');
  Result := Validate(not Checkbox.Checked, _Message);
end;

{ TCheckListBoxValidator }

constructor TCheckListBoxValidator.Create(_clb: TCheckListBox; _Validator: IdzInputValidator);
begin
  inherited Create(_clb, _Validator);
  SendMessage(_clb.Handle, WM_PRINTCLIENT, 0, 0);
end;

function TCheckListBoxValidator.CheckListBox: TCheckListBox;
begin
  Result := FControl as TCheckListBox;
end;

function TCheckListBoxValidator.AtLeastOneChecked: Boolean;
begin
  Result := (TCheckListBox_GetCheckedCount(CheckListBox) > 0);
  Validate(Result, _('At least one item must be checked.'));
 // SendMessage(CheckListBox.Handle, WM_PRINTCLIENT, 0, 0);
end;

function TCheckListBoxValidator.ValidateHasCheckedItems(_Items: TStrings): Boolean;
begin
  Result := Validate(TCheckListBox_GetChecked(CheckListBox, _Items) > 0);
end;

procedure TCheckListBoxValidator.doSetColor(_Color: TColor);
var
  ctl: TCheckListBox;
begin
  ctl := CheckListBox;
{$IFNDEF DELPHIX_BERLIN_UP}
  // I'm not sure when this was fixed. In Delphi 2007 the background is not redrawn
  // In Delphi 10.2 it is. The change is in TCustomListbox.WindowProc
  InvalidateRect(ctl.Handle, nil, True);
{$ENDIF}
  ctl.Color := _Color;
end;

function InputValidator(_OkColor: TColor = clWindow; _ErrColor: TColor = clYellow): IdzInputValidator;
begin
  Result := TInputValidator.Create(_OkColor, _ErrColor);
end;

function ControlValidator(_ed: TCustomEdit; _iv: IdzInputValidator;
  _Container: IdzControlTextSource = nil): IdzEditValidator;
begin
  Result := TEditValidator.Create(_ed, _iv, _Container);
end;

function ControlValidator(_m: TMemo; _iv: IdzInputValidator;
  _Container: IdzControlTextSource = nil): IdzMemoValidator;
begin
  Result := TMemoValidator.Create(_m, _iv, _Container);
end;

function ControlValidator(_cmb: TCustomComboBox; _iv: IdzInputValidator): IdzComboboxValidator;
begin
  Result := TComboboxValidator.Create(_cmb, _iv);
end;

function ControlValidator(_Chk: TCustomCheckBox; _iv: IdzInputValidator): IdzCheckboxValidator;
begin
  Result := TCheckboxValidator.Create(_Chk, _iv);
end;

function ControlValidator(_clb: TCheckListBox; _iv: IdzInputValidator): IdzCheckListBoxValidator;
begin
  Result := TCheckListBoxValidator.Create(_clb, _iv);
end;

{ TTextControlIpV4Validator }

function TTextControlIpV4Validator.TryGetValue(out _Value: string): Boolean;
var
  i: Integer;
  IntValue: Integer;
  Text: string;
  s: string;
begin
  Result := False;
  _Value := doGetText;

  Text := _Value;
  for i := 1 to Length(Text) do
    if not (CharInSet(Text[i], ['0'..'9', '.'])) then
      Exit;
  for i := 1 to 4 do begin //FI:W528
    s := ExtractStr(Text, '.');
    if (Length(s) > 3) or not TryStrToInt(s, IntValue) or (IntValue < 0) or (IntValue > 255) then
      Exit;
  end;
  // Text must now be empty
  Result := (Text = '');
end;

function TTextControlIpV4Validator.IsValid: Boolean;
var
  Value: string;
begin
  Result := TryGetValue(Value) and (Value <> '');
  Validate(Result, _('Value must be an IPv4 address.'));
end;

function TTextControlIpV4Validator.IsValidOrEmpty: Boolean;
var
  Value: string;
begin
  Result := TryGetValue(Value) or (Value = '');
  Validate(Result, _('Value must be an IPv4 address or empty.'));
end;

{ TMemoValidator }

constructor TMemoValidator.Create(_m: TMemo; _Validator: IdzInputValidator;
  _Container: IdzControlTextSource);
begin
  inherited Create(_m, _Validator, _Container);
end;

function TMemoValidator.doGetText: string;
begin
  Result := (FControl as TMemo).Lines.Text;
end;

function TMemoValidator.ValidateLineCount(_Minimum: Integer): Boolean;
begin
  Result := ((FControl as TMemo).Lines.Count >= _Minimum);
  Validate(Result, Format(_('Value must have at least %d lines.'), [_Minimum]));
end;

{$ENDIF DELPHI2007_UP}

end.
