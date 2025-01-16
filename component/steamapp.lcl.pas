unit steamapp.lcl;

interface

uses
  System.Classes, CastleSteam, CastleApplicationProperties, ExtCtrls;

type
  TSteamApp = Class(TCastleSteam)
  strict private
    FAppTimer: TTimer;
    FUpdateCount: Cardinal;
    FInterval: Cardinal;
    FDoUpdate: TNotifyEvent;
    procedure SetInterval(const AValue: Cardinal);
    procedure AppUpdate(Sender: TObject);
  public
    constructor Create(const AAppId: TAppId); reintroduce;
    destructor Destroy; override;
    property UpdateCount: Cardinal Read FUpdateCount;
    property Interval: Cardinal Read FInterval Write SetInterval;
    property OnAppUpdate: TNotifyEvent read FDoUpdate write FDoUpdate;
  end;

implementation

{ TSteamApp }

constructor TSteamApp.Create(const AAppId: TAppId);
begin
  inherited Create(AAppId);

  ApplicationProperties();

  FUpdateCount := 0;
  FAppTimer := TTimer.Create(Nil);
  FAppTimer.OnTimer := Nil;
  FAppTimer.Interval := 0;
  FAppTimer.Enabled := False;
end;

destructor TSteamApp.Destroy;
begin

  inherited;
end;

procedure TSteamApp.SetInterval(const AValue: Cardinal);
begin
  if(AValue > 0) then
    begin
      FInterval := AValue;
      FAppTimer.OnTimer := AppUpdate;
      FAppTimer.Interval := FInterval;
      FAppTimer.Enabled := True;
    end
  else
    begin
      FInterval := AValue;
      FAppTimer.OnTimer := Nil;
      FAppTimer.Interval := FInterval;
      FAppTimer.Enabled := False;
    end;
end;

procedure TSteamApp.AppUpdate(Sender: TObject);
begin
  FUpdateCount := FUpdateCount + 1;
  ApplicationProperties.DoUpdate(Self);
  if Assigned(FDoUpdate) and UserStatsReceived then
    FDoUpdate(Self);
end;



end.
