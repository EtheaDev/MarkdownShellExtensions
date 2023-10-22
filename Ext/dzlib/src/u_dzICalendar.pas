unit u_dzICalendar;

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
  Generics.Defaults,
  Generics.Collections,
  u_dzTranslator,
  u_dzNullableDateTime,
  u_dzICalDuration;

type
  TdzICalendar = class;

  TdzICalendarEvent = class
  private
  public
//DTSTART:19970714T170000Z
    DTStart: TdzNullableDateTime;
//DTEND:19970715T035959Z
    DTEnd: TdzNullableDateTime;
    Duration: TdzNullableDuration;
    Summary: string;
    Description: string;
    constructor Create(_Owner: TdzICalendar);
    destructor Destroy; override;
  end;

  ///<summary> This is a very simple class for storing ICalendar data.
  ///          For reading this from a file, see TdzICalendarParser.
  ///</summary>
  TdzICalendar = class
  private
    FEvents: TObjectList<TdzICalendarEvent>;
    function GetEvents(_Idx: integer): TdzICalendarEvent;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TdzICalendarEvent;
    procedure Delete(_Idx: integer);
    function Count: integer;
    procedure Sort;
    property Events[_Idx: integer]: TdzICalendarEvent read GetEvents; default;
  end;
{$ENDIF}

implementation

{$IFDEF SUPPORTS_GENERICS}
uses
  StrUtils,
  DateUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

{ TdzICalendar }

constructor TdzICalendar.Create;
begin
  inherited Create;
  FEvents := TObjectList<TdzICalendarEvent>.Create;
end;

destructor TdzICalendar.Destroy;
begin
  FreeAndNil(FEvents);
  inherited;
end;

function TdzICalendar.Count: integer;
begin
  Result := FEvents.Count;
end;

function TdzICalendar.GetEvents(_Idx: integer): TdzICalendarEvent;
begin
  Result := FEvents[_Idx];
end;

type
  TEventComparer = TComparer<TdzICalendarEvent>;

procedure TdzICalendar.Sort;
begin
  FEvents.Sort(TEventComparer.Construct(
    function(const L, R: TdzICalendarEvent): integer
    begin
      if L.DTStart > R.DTStart then
        Result := 1
      else if L.DTStart = R.DTStart then
        Result := 0
      else
        Result := -1;
    end
    ));
end;

function TdzICalendar.Add: TdzICalendarEvent;
begin
  Result := TdzICalendarEvent.Create(Self);
  FEvents.Add(Result);
end;

procedure TdzICalendar.Delete(_Idx: integer);
begin
  FEvents.Delete(_Idx);
end;

{ TdzICalendarItem }

constructor TdzICalendarEvent.Create(_Owner: TdzICalendar);
begin
  inherited Create;
end;

destructor TdzICalendarEvent.Destroy;
begin

  inherited;
end;
{$ENDIF}

end.
