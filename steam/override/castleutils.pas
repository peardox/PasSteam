unit CastleUtils;

interface

uses Classes, SysUtils, CastleLog;

type
  EInternalError = class(Exception);

procedure Check(TrueValue: boolean; const ErrMessage: string = 'Check failed');
function GetMem(const Size: Integer): Pointer;

implementation

procedure Check(TrueValue: boolean; const ErrMessage: string = 'Check failed');
begin
  if not TrueValue then
    raise Exception.Create(ErrMessage);
end;

function GetMem(const Size: Integer): Pointer;
begin
  System.GetMem(Result, Size);
end;


end.
