unit SteamApp.fmx;

interface

uses
  System.Classes, CastleSteam, CastleApplicationProperties, fmx.Types,
  fmx.Graphics;

type
  TSteamApp = Class(TCastleSteam)
  strict private
    FAppTimer: TTimer;
    FUpdateCount: Cardinal;
    FInterval: Cardinal;
    FDoUpdate: TNotifyEvent;
    FAvatar: TBitmap;
    procedure SetInterval(const AValue: Cardinal);
    procedure AppUpdate(Sender: TObject);
    function GetAvatar: TBitmap;
  public
    constructor Create(const AAppId: TAppId); reintroduce;
    destructor Destroy; override;
    property UpdateCount: Cardinal Read FUpdateCount;
    property Interval: Cardinal Read FInterval Write SetInterval;
    property OnAppUpdate: TNotifyEvent read FDoUpdate write FDoUpdate;
    property Avatar: TBitmap read GetAvatar;
  end;

implementation

{ TSteamApp }

uses SysUtils, System.UITypes, FMX.Utils;

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

function TSteamApp.GetAvatar: TBitmap;
var
  SteamImage: TSteamBitmap;
  B: TBitmap;
  x, y: integer;
  Src, Dst: TBitmapData;
  S, D: Pointer;
begin
  Result := nil;
  if UserStatsReceived then
    begin
      SteamImage := GetFriendImage(UserId);
      if SteamImage.IsValid then
        begin
          B := TBitmap.Create(SteamImage.Width, SteamImage.Height);
          if B.BytesPerPixel <> SteamImage.BPP then
            Raise Exception.Create('Mis-matched Bytes Per Pixel');
          if B.Map(TMapAccess.Write, Dst) then
            begin
              Src := TBitmapData.Create(SteamImage.Width, SteamImage.Height, TPixelFormat.RGBA);
              Src.Data := SteamImage.Image;
              AlphaColorToScanline(@PAlphaColorArray(Src.Data)[0],
                Dst.Data,
                SteamImage.Width * SteamImage.Height,
                TPixelFormat.RGBA);

              B.Unmap(Dst);
              Result := B;
              SteamImage.Free;
            end;
        end;
    end;
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
