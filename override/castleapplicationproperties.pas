unit castleapplicationproperties;

interface

uses SysUtils, Generics.Collections, Contnrs, Classes;

type
  TNotifyEventList = class({$ifdef FPC}specialize{$endif} TList<TNotifyEvent>)
  end;

  TProcedureList = class({$ifdef FPC}specialize{$endif} TList<TProcedure>)
  end;

  TGLContextEvent = procedure;

  TGLContextEventList = class({$ifdef FPC}specialize{$endif} TList<TGLContextEvent>)
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
    FIsGLContextOpen, FFileAccessSafe: boolean;
    FOnGLContextEarlyOpen, FOnGLContextOpen, FOnGLContextClose: TGLContextEventList;
    FOnUpdate, FOnInitializeJavaActivity,
      FOnGLContextOpenObject, FOnGLContextCloseObject,
      FOnPause, FOnResume: TNotifyEventList;
    FOnWarning: TWarningEventList;
    FOnLog: TLogEventList;
    FOnInitializeDebug: TProcedureList;
    FVersion: String;
    FTouchDevice: boolean;
    FLimitFPS: Single;
    FShowUserInterfaceToQuit: Boolean;
    FCaption: String;
    // Maybe we will expose them as public read-only properties in the future.
    FInitializedDebug: Boolean;
    FInitializedRelease: Boolean;
    FPendingToFree: TComponentList;
    function GetApplicationName: String;
    procedure SetApplicationName(const Value: String);
    procedure DoPendingFree;
  public
    const
      DefaultLimitFPS = 120.0;

      { Some platforms do not support Application.ProcessMessage, which means you
        cannot just write a function like MessageYesNo that waits until user clicks
        something.
        You *have* to implement modal boxes then using views,
        e.g. using CastleDialogViews or your own TCastleView descendants. }
      PlatformAllowsModalRoutines = {$if defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH)} false {$else} true {$endif};

    constructor Create;
    destructor Destroy; override;

    { Application short name.
      Used e.g. by @link(InitializeLog) to name the log file.

      When compiled with FPC, this returns and sets the same thing
      as standard SysUtils.ApplicationName.
      When setting this, we automatically set SysUtils.OnGetApplicationName. }
    property ApplicationName: String read GetApplicationName write SetApplicationName;
  end;

var
  FApplicationProperties: TCastleApplicationProperties;



implementation

{ TCastleApplicationProperties }

constructor TCastleApplicationProperties.Create;
begin

end;

destructor TCastleApplicationProperties.Destroy;
begin

  inherited;
end;

procedure TCastleApplicationProperties.DoPendingFree;
begin

end;

function TCastleApplicationProperties.GetApplicationName: String;
begin

end;

procedure TCastleApplicationProperties.SetApplicationName(const Value: String);
begin

end;

end.
