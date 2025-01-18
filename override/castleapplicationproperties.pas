unit castleapplicationproperties;

interface

uses SysUtils, Generics.Collections, Contnrs, Classes;

type
  TNotifyEventList = class({$ifdef FPC}specialize{$endif} TList<TNotifyEvent>)
  public
    { Call all (assigned) Items, from first to last. }
    procedure ExecuteAll(Sender: TObject);

    { Call all (assigned) Items, from first to last. }
    procedure ExecuteForward(Sender: TObject);

    { Call all (assigned) Items, from last to first. }
    procedure ExecuteBackward(Sender: TObject);
  end;

  TProcedureList = class({$ifdef FPC}specialize{$endif} TList<TProcedure>)
  public
    { Call all (assigned) Items, from first to last. }
    procedure ExecuteAll;
  end;

  TWarningEvent = procedure (const Category, Message: String) of object;
  TLogEvent = procedure (const Message: String) of object;

  TWarningEventList = class({$ifdef FPC}specialize{$endif} TList<TWarningEvent>)
  end;

  TLogEventList = class({$ifdef FPC}specialize{$endif} TList<TLogEvent>)
  end;

  { Events and properties of the Castle Game Engine application,
    usually accessed through the @link(ApplicationProperties) singleton.

    These members work regardless if you use CastleWindow or CastleControl.
    For more fine-grained application control,
    see TCastleApplication (in case you use CastleWindow)
    or Lazarus (LCL) TApplication (in case you use CastleControl). }
  TCastleApplicationProperties = class
  private
    FOnUpdate: TNotifyEventList;
    FOnInitializeDebug: TProcedureList;
    FPendingToFree: TComponentList;
    procedure _Update;
    procedure DoPendingFree;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoUpdate(Sender: TObject);
    property OnUpdate: TNotifyEventList read FOnUpdate;
  end;

var
  FApplicationProperties: TCastleApplicationProperties;

function ApplicationProperties(const CreateIfNotExisting: boolean = true): TCastleApplicationProperties;

implementation

{ TCastleApplicationProperties }

function ApplicationProperties(const CreateIfNotExisting: boolean): TCastleApplicationProperties;
begin
  if (FApplicationProperties = nil) and CreateIfNotExisting then
    FApplicationProperties := TCastleApplicationProperties.Create;
  Result := FApplicationProperties;
end;


procedure TCastleApplicationProperties.DoUpdate(Sender: TObject);
begin
  _Update;
end;

constructor TCastleApplicationProperties.Create;
begin
  FOnUpdate := TNotifyEventList.Create;
  FOnInitializeDebug := TProcedureList.Create;
end;

destructor TCastleApplicationProperties.Destroy;
begin
  FreeAndNil(FPendingToFree);
  FreeAndNil(FOnUpdate);
  FreeAndNil(FOnInitializeDebug);

  inherited;
end;

procedure TCastleApplicationProperties._Update;
begin
  DoPendingFree;
  FOnUpdate.ExecuteAll(Self);
end;

procedure TCastleApplicationProperties.DoPendingFree;
var
  I: Integer;
begin
  if FPendingToFree <> nil then
    for I := FPendingToFree.Count - 1 downto 0 do
      if I < FPendingToFree.Count then
        FPendingToFree[I].Free; // this will remove it from children, and from FPendingToFree
end;

{ TNotifyEventList  ------------------------------------------------------ }

procedure TNotifyEventList.ExecuteAll(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do

    { TODO: The test "I < Count" is a quick fix for the problem that list
      may be modified during iteration.
      E.g. network/remote_logging/gameloghandler.pas in HttpPostFinish
      sets FreeSender, which means that Application.OnUpdate list
      is modified while we iterate over it.

      We should introduce a reliable way to handle this, but for now the test
      at least prevents a crash in this case. }

    if (I < Count) and Assigned(Items[I]) then
      Items[I](Sender);
end;

procedure TNotifyEventList.ExecuteForward(Sender: TObject);
begin
  ExecuteAll(Sender);
end;

procedure TNotifyEventList.ExecuteBackward(Sender: TObject);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do

    { TODO: The test "I < Count" is a quick fix for the problem that when
      TCastleApplicationProperties._GLContextClose calls
      FOnGLContextCloseObject.ExecuteBackward(Self),
      some "on close" callbacks modify the FOnGLContextCloseObject list.

      We should introduce a reliable way to handle this, but for now the test
      at least prevents a crash in this case. }

    if (I < Count) and Assigned(Items[I]) then
      Items[I](Sender);
end;

{ TProcedureList ------------------------------------------------------ }

procedure TProcedureList.ExecuteAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    { See TNotifyEventList.ExecuteAll for explanation why the "I < Count"
      adds a little safety here. }
    if (I < Count) and Assigned(Items[I]) then
      Items[I]();
end;


initialization
finalization
  FreeAndNil(FApplicationProperties);

end.
