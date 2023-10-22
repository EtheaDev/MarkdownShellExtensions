unit u_dzICalParser;

{$INCLUDE 'dzlib.inc'}

{$IFNDEF SUPPORTS_GENERICS}
{$IFNDEF NO_GENERICS_HINT}
{$MESSAGE hint 'This unit requires a compiler with generics support (Delphi >= 2009)'}
{$ENDIF}
{$ENDIF}

interface

{$IFDEF SUPPORTS_GENERICS}
uses
  SysUtils,
  Classes,
  u_dzTranslator,
  u_dzICalendar,
  u_dzNullableDateTime,
  u_dzICalDuration;

type
  TOnParserEvent = procedure(_Sender: TObject; _sl: TStrings; var _i: Integer) of object;
  TOnEvent = procedure(_Sender: TObject; _Event: TStrings) of object;

type
  ///<summary>
  /// This is a very simple parser for ICal-Files. There are two different ways to use it
  /// 1. Assign a TdzICalendar to its ICal property and call ParseFile or ParseStrings
  /// 2. Assign the OnParseXxxx events and do it the rough way.
  ///</summary>
  TDzIcalendarParser = class
  private
    FIdx: Integer;
    FLines: TStrings;
    FICalendar: TdzICalendar;
    FOnParseEndVCalendar: TOnParserEvent;
    FOnParseBeginVCalendar: TOnParserEvent;
    FOnParseBeginVEvent: TOnParserEvent;
    FOnParseEndVEvent: TOnParserEvent;
    FOnEvent: TOnEvent;
    procedure doOnParseBeginVCalendar(_sl: TStrings; var _i: Integer);
    procedure doOnParseEndVCalendar(_sl: TStrings; var _i: Integer);
    procedure doOnParseBeginVEvent(_sl: TStrings; var _i: Integer);
    procedure doOnParseEndVEvent(_sl: TStrings; var _i: Integer);

    procedure doOnEvent(_Event: TStrings);

    procedure ParseVCalendar;
    procedure ParseVEvent;
    function MatchesProperty(const _Name: string; const _Line: string; var _Value: string): boolean;
    function GetNextLine(var _Line: string): boolean;
    procedure ParseVAlarm;
    function Str2DateTime(const _s: string): TdzNullableDateTime;
    function Str2Duration(const _s: string): TdzNullableDuration;
    function UnEscape(const _s: string): string;
  public
    procedure ParseFile(const _Filename: string);
    procedure ParseStrings(_sl: TStrings);
    property ICalendar: TdzICalendar read FICalendar write FICalendar;
    property OnParseBeginVCalendar: TOnParserEvent read FOnParseBeginVCalendar write FOnParseBeginVCalendar;
    property OnParseEndVCalendar: TOnParserEvent read FOnParseEndVCalendar write FOnParseEndVCalendar;
    property OnParseBeginVEvent: TOnParserEvent read FOnParseBeginVEvent write FOnParseBeginVEvent;
    property OnParseEndVEvent: TOnParserEvent read FOnParseEndVEvent write FOnParseEndVEvent;
    property OnEvent: TOnEvent read FOnEvent write FOnEvent;
  end;
{$ENDIF}

implementation

{$IFDEF SUPPORTS_GENERICS}
uses
  StrUtils,
  u_dzStringUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TDzIcalendarParser }

procedure TDzIcalendarParser.doOnParseBeginVCalendar(_sl: TStrings; var _i: Integer);
begin
  if Assigned(FOnParseBeginVCalendar) then
    FOnParseBeginVCalendar(Self, _sl, _i);
end;

procedure TDzIcalendarParser.doOnParseEndVCalendar(_sl: TStrings; var _i: Integer);
begin
  if Assigned(FOnParseEndVCalendar) then
    FOnParseEndVCalendar(Self, _sl, _i);
end;

procedure TDzIcalendarParser.doOnParseBeginVEvent(_sl: TStrings; var _i: Integer);
begin
  if Assigned(FOnParseBeginVEvent) then
    FOnParseBeginVEvent(Self, _sl, _i);
end;

procedure TDzIcalendarParser.doOnParseEndVEvent(_sl: TStrings; var _i: Integer);
begin
  if Assigned(FOnParseEndVEvent) then
    FOnParseEndVEvent(Self, _sl, _i);
end;

procedure TDzIcalendarParser.doOnEvent(_Event: TStrings);
var
  Event: TdzICalendarEvent;
begin
  if Assigned(FOnEvent) then
    FOnEvent(Self, _Event);

  if Assigned(FICalendar) then begin
    Event := FICalendar.Add;
    Event.Summary := _Event.Values['Summary'];
    Event.Description := _Event.Values['Description'];
    Event.DTStart := Str2DateTime(_Event.Values['DTStart']);
    Event.DTEnd := Str2DateTime(_Event.Values['DTEnd']);
    Event.Duration := Str2Duration(_Event.Values['Duration']);
  end;
end;

function TDzIcalendarParser.Str2Duration(const _s: string): TdzNullableDuration;
var
  s: string;
  p: Integer;
  TimeStr: string;
  DateStr: string;
  Value: Integer;
  Code: Integer;
begin
  Result.Invalidate;
  if _s = '' then
    exit;
  if _s[1] <> 'P' then
    raise Exception.Create(_('Duration must start with a "P"'));
  s := Copy(_s, 2);
  p := Pos('T', s);
  if p = 0 then begin
    DateStr := s;
    TimeStr := '';
  end else begin
    DateStr := Copy(s, 1, p - 1);
    TimeStr := Copy(s, p + 1);
  end;
  while DateStr <> '' do begin
    Val(DateStr, Value, Code);
    if (Code = 0) or (Code > Length(DateStr)) then
      raise Exception.CreateFmt(_('"%s" is not a valid duration string.'), [_s]);
    if DateStr[Code] = 'W' then
      Result.Weeks := Value
    else if DateStr[Code] = 'D' then
      Result.Days := Value
    else
      raise Exception.CreateFmt(_('"%s" is not a valid duration string.'), [_s]);
    DateStr := Copy(DateStr, Code + 1);
  end;
  while TimeStr <> '' do begin
    Val(TimeStr, Value, Code);
    if (Code = 0) or (Code > Length(TimeStr)) then
      raise Exception.CreateFmt(_('"%s" is not a valid duration string.'), [_s]);
    if TimeStr[Code] = 'H' then
      Result.Hours := Value
    else if TimeStr[Code] = 'M' then
      Result.Minutes := Value
    else if TimeStr[Code] = 'S' then
      Result.Seconds := Value
    else
      raise Exception.CreateFmt(_('"%s" is not a valid duration string.'), [_s]);
    TimeStr := Copy(TimeStr, Code + 1);
  end;
end;

function TDzIcalendarParser.Str2DateTime(const _s: string): TdzNullableDateTime;
// This function does a very simple Date-Time parsing.
// It expects a date time value in one of the following formats:
// <date>[T<time>[Z]]
// where <date> is an 8 character string consisting of
// <4 digit year><2 digit month><2 digit day>
// and <time> is an 6 character string consisting of
// <2 digit hour><2 digit minute><2 digit second>
// apart from T and Z, no delimiters are allowed.
// (This is not quite what RFC 2445 says.)
var
  DateStr: string;
  TimeStr: string;
  p: Integer;
begin
  Result.Invalidate;
  if Length(_s) = 0 then
    exit;

  p := Pos('T', _s);
  if p = 0 then begin
    DateStr := _s;
    TimeStr := '';
  end else begin
    DateStr := Copy(_s, 1, p - 1);
    TimeStr := Copy(_s, p + 1);
    p := Length(TimeStr);
    if TimeStr[p] = 'Z' then
      TimeStr := Copy(TimeStr, 1, p - 1);
    if Length(TimeStr) <> 6 then
      raise Exception.CreateFmt(_('"%s" is not a valid time value.'), [TimeStr]);
  end;
  if Length(DateStr) <> 8 then
    raise Exception.CreateFmt(_('"%s" is not a valid date value.'), [DateStr]);

  Result := EncodeDate(StrToInt(Copy(DateStr, 1, 4)), StrToInt(Copy(DateStr, 5, 2)), StrToInt(Copy(DateStr, 7, 2)));
  if TimeStr <> '' then begin
    Result.AssignTime(EncodeTime(StrToInt(Copy(TimeStr, 1, 2)), StrToInt(Copy(TimeStr, 3, 2)), StrToInt(Copy(TimeStr, 5, 2)), 0));
  end;
end;

function TDzIcalendarParser.GetNextLine(var _Line: string): boolean;
begin
  Result := FIdx < FLines.Count;
  if Result then begin
    _Line := FLines[FIdx];
    Inc(FIdx);
    while (FIdx < FLines.Count) and (LeftStr(FLines[FIdx], 1) = ' ') do begin
      _Line := _Line + TailStr(FLines[FIdx], 2);
      Inc(FIdx);
    end;
  end;
end;

procedure TDzIcalendarParser.ParseFile(const _Filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename, TEncoding.UTF8);
    ParseStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDzIcalendarParser.ParseStrings(_sl: TStrings);
var
  Line: string;
begin
  Assert(Assigned(_sl));

  FLines := _sl;
  FIdx := 0;
  while GetNextLine(Line) do begin
    if SameText('BEGIN:VCALENDAR', Line) then begin
      ParseVCalendar;
    end;
  end;
end;

procedure TDzIcalendarParser.ParseVCalendar;
//BEGIN:VCALENDAR
//VERSION:2.0
//PRODID:-//hacksw/handcal//NONSGML v1.0//EN
//  <entries>
//END:VCALENDAR
var
  Line: string;
begin
  doOnParseBeginVCalendar(FLines, FIdx);
  while GetNextLine(Line) do begin
    if SameText('END:VCALENDAR', Line) then begin
      doOnParseEndVCalendar(FLines, FIdx);
      exit; // ==>
    end;
    if SameText('BEGIN:VEVENT', Line) then
      ParseVEvent;
  end;
  raise Exception.CreateFmt(_('Unexpected end of ICalendar within VCalendar in line %d'), [FIdx]);
end;

function TDzIcalendarParser.MatchesProperty(const _Name: string; const _Line: string; var _Value: string): boolean;
var
  p: Integer;
begin
  if StartsText(_Name + ':', _Line) then begin
    p := Length(_Name);
    _Value := Copy(_Line, p + 2);
    Result := true;
  end else if StartsText(_Name + ';', _Line) then begin
    p := Pos(':', _Line);
    if p = 0 then
      raise Exception.CreateFmt(_('%s is not a valid ICalendar line.'), [_Line]);
    _Value := Copy(_Line, p + 1);
    Result := true;
  end else
    Result := False;
end;

function TDzIcalendarParser.UnEscape(const _s: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  i := 1;
  while i <= Length(_s) do begin
    c := _s[i];
    if c = '\' then begin
      Inc(i);
      c := _s[i];
      if (c = 'n') or (c = 'N') then
        Result := Result + #13#10
      else begin
        // this is not quite correct since \ is supposed to only escape crlf, '\', ',' and ';'
        Result := Result + c;
      end;
    end else
      Result := Result + c;
    Inc(i);
  end;
end;

procedure TDzIcalendarParser.ParseVEvent;
//BEGIN:VEVENT
//UID:uid1@example.com
//DTSTAMP:19970714T170000Z
//ORGANIZER;CN=John Doe:MAILTO:john.doe@example.com
//DTSTART:19970714T170000Z
//DTEND:19970715T035959Z
//SUMMARY:Bastille Day Party
//END:VEVENT
var
  Line: string;
  s: string;
  Event: TStrings;
begin
  Event := TStringList.Create;
  try
    doOnParseBeginVEvent(FLines, FIdx);
    while GetNextLine(Line) do begin
      if SameText('END:VEVENT', Line) then begin
        doOnEvent(Event);
        doOnParseEndVEvent(FLines, FIdx);
        exit; // ==>
      end;
      if SameText('BEGIN:VALARM', Line) then begin
        ParseVAlarm;
      end else if MatchesProperty('UID', Line, s) then begin
        Event.Values['UID'] := s;
      end else if MatchesProperty('DTSTAMP', Line, s) then begin
        Event.Values['DTSTAMP'] := s;
      end else if MatchesProperty('ORGANIZER', Line, s) then begin
        Event.Values['ORGANIZER'] := s;
      end else if MatchesProperty('DTSTART', Line, s) then begin
        Event.Values['DTSTART'] := s;
      end else if MatchesProperty('DTEND', Line, s) then begin
        Event.Values['DTEND'] := s;
      end else if MatchesProperty('DURATION', Line, s) then begin
        Event.Values['DURATION'] := s;
      end else if MatchesProperty('SUMMARY', Line, s) then begin
        Event.Values['SUMMARY'] := UnEscape(s);
      end else if MatchesProperty('DESCRIPTION', Line, s) then begin
        Event.Values['DESCRIPTION'] := UnEscape(s);
      end;
    end;
  finally
    FreeAndNil(Event);
  end;
  raise Exception.CreateFmt(_('Unexpected end of ICalendar within VEvent in line %d'), [FIdx]);
end;

procedure TDzIcalendarParser.ParseVAlarm;
var
  Line: string;
begin
  while GetNextLine(Line) do begin
    if SameText('END:VALARM', Line) then begin
      exit; // ==>
    end;
  end;
  raise Exception.CreateFmt(_('Unexpected end of ICalendar within VAlarm in line %d'), [FIdx]);
end;
{$ENDIF ~SUPPORTS_GENERICS}

end.
