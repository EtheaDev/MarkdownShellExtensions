unit u_dzAngle;

{$INCLUDE 'dzlib.inc'}

interface

{$IFNDEF DELPHI2007_UP}
{$IFNDEF NO_DELPHI2007UP_HINT}
{$MESSAGE HINT 'Delphi <2007 not supported'}
{$ENDIF}
{$ELSE}

uses
  Math;

type
  ///<summary>
  /// Stores an angle, either in degrees or in radians. Automatically converts it to the
  /// other unit when it is required. </summary>
  TAngle = record
  private
    FRadians: Extended;
    FDegrees: Extended;
  public
    class function DegreesToPercent(_Degrees: Extended): Extended; static;
    class function RadiansToDegrees(const _Value: Extended): Extended; static;
    class function DegreesToRadians(const _Value: Extended): Extended; static;
    class function FromRadians(const _Value: Extended): TAngle; static;
    class function FromDegrees(const _Value: Extended): TAngle; static;
    function InRadians: Extended;
    function InDegrees: Extended;
    function InPercent: Extended;
    procedure AssignRadians(const _Value: Extended);
    procedure AssignDegrees(const _Value: Extended);
  end;

{$ENDIF DELPHI2007_UP}

implementation

{$IFDEF DELPHI2007_UP}

{ TAngle }

procedure TAngle.AssignDegrees(const _Value: Extended);
begin
  FDegrees := _Value;
  FRadians := NaN;
end;

procedure TAngle.AssignRadians(const _Value: Extended);
begin
  FRadians := _Value;
  FDegrees := NaN;
end;

class function TAngle.DegreesToPercent(_Degrees: Extended): Extended;
begin
  Result := Tan(DegToRad(_Degrees)) * 100;
end;

class function TAngle.DegreesToRadians(const _Value: Extended): Extended;
begin
  Result := DegToRad(_Value);
end;

class function TAngle.FromDegrees(const _Value: Extended): TAngle;
begin
  Result.AssignDegrees(_Value);
end;

class function TAngle.FromRadians(const _Value: Extended): TAngle;
begin
  Result.AssignRadians(_Value);
end;

function TAngle.InDegrees: Extended;
begin
  if IsNan(FDegrees) then
    FDegrees := RadiansToDegrees(FRadians);
  Result := FDegrees;
end;

function TAngle.InPercent: Extended;
begin
  Result := DegreesToPercent(InDegrees);
end;

function TAngle.InRadians: Extended;
begin
  if IsNan(FRadians) then
    FRadians := DegreesToRadians(FDegrees);
  Result := FRadians;
end;

class function TAngle.RadiansToDegrees(const _Value: Extended): Extended;
begin
  Result := RadToDeg(_Value);
end;

{$ENDIF DELPHI2007_UP}

end.
