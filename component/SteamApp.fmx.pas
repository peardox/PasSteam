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
    procedure SetInterval(const AValue: Cardinal);
    procedure AppUpdate(Sender: TObject);
    function GetAvatar: UInt32;
  public
    procedure ConvertSteamImage(const AHandle: UInt32; const ABitmap: TBitmap);
    function SteamBitmapToTBitmap(ABitmap: TSteamBitmap): TBitmap;
    function SteamImageToTBitmap(const AHandle: UInt32):TBitmap;
    constructor Create(const AAppId: TAppId); reintroduce;
    destructor Destroy; override;
    property UpdateCount: Cardinal Read FUpdateCount;
    property Interval: Cardinal Read FInterval Write SetInterval;
    property OnAppUpdate: TNotifyEvent read FDoUpdate write FDoUpdate;
    property Avatar: UInt32 read GetAvatar;
  end;

implementation

{ TSteamApp }

uses SysUtils, System.UITypes, FMX.Utils;

function TSteamApp.SteamImageToTBitmap(const AHandle: UInt32):TBitmap;
var
  SteamBitmap: TSteamBitmap;
  ABitmap: TBitmap;
  Dst: TBitmapData;
  SR, DR: Pointer;
  Row: Integer;
begin
  Result := nil;
  SteamBitmap := GetSteamBitmap(AHandle);
  try
    if Assigned(SteamBitmap) and SteamBitmap.IsValid then
      begin
        ABitmap := TBitmap.Create(SteamBitmap.Width, SteamBitmap.Height);
        if ABitmap.BytesPerPixel <> SteamBitmap.BPP then
          Raise Exception.Create('Mis-matched Bytes Per Pixel');
        if ABitmap.Map(TMapAccess.Write, Dst) then
              begin
                for Row := 0 to SteamBitmap.Height - 1 do
                  begin
                    DR := Dst.GetScanline(Row);
                    SR := Pointer(NativeInt(SteamBitmap.Image) +
                          (SizeOf(TAlphaColor) * Row * SteamBitmap.Width) );
                    AlphaColorToScanline(@PAlphaColorArray(SR)[0],
                      DR,
                      SteamBitmap.Width,
                      TPixelFormat.RGBA);
                  end;

            ABitmap.Unmap(Dst);
            Result := ABitmap;
          end;
      end;
  finally
    FreeAndNil(SteamBitmap);
  end;

end;

procedure TSteamApp.ConvertSteamImage(const AHandle: UInt32; const ABitmap: TBitmap);
var
  SteamBitmap: TSteamBitmap;
  Dst: TBitmapData;
  SR, DR: Pointer;
  Row: Integer;
begin
  SteamBitmap := GetSteamBitmap(AHandle);
  try
    if Assigned(SteamBitmap) and SteamBitmap.IsValid then
      begin
        ABitmap.Resize(SteamBitmap.Width, SteamBitmap.Height);
        if ABitmap.BytesPerPixel <> SteamBitmap.BPP then
          Raise Exception.Create('Mis-matched Bytes Per Pixel');
        if ABitmap.Map(TMapAccess.Write, Dst) then
              begin
                for Row := 0 to SteamBitmap.Height - 1 do
                  begin
                    DR := Dst.GetScanline(Row);
                    SR := Pointer(NativeInt(SteamBitmap.Image) +
                          (SizeOf(TAlphaColor) * Row * SteamBitmap.Width) );
                    AlphaColorToScanline(@PAlphaColorArray(SR)[0],
                      DR,
                      SteamBitmap.Width,
                      TPixelFormat.RGBA);
                  end;

            ABitmap.Unmap(Dst);
          end;
      end;
  finally
    FreeAndNil(SteamBitmap);
  end;

end;

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
  FAppTimer.Enabled := False;
  FreeAndNil(FAppTimer);
  inherited;
end;

function TSteamApp.GetAvatar: UInt32;
begin
  Result := 0;
  if UserStatsReceived then
     Result := GetFriendImageHandle(UserId);
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

function TSteamApp.SteamBitmapToTBitmap(ABitmap: TSteamBitmap): TBitmap;
var
  B: TBitmap;
  Dst: TBitmapData;
  SR, DR: Pointer;
  Row: Integer;
begin
  Result := nil;
  if Assigned(ABitmap) and ABitmap.IsValid then
    begin
      B := TBitmap.Create(ABitmap.Width, ABitmap.Height);
      if B.BytesPerPixel <> ABitmap.BPP then
        Raise Exception.Create('Mis-matched Bytes Per Pixel');
      if B.Map(TMapAccess.Write, Dst) then
            begin
              for Row := 0 to ABitmap.Height - 1 do
                begin
                  DR := Dst.GetScanline(Row);
                  SR := Pointer(NativeInt(ABitmap.Image) +
                        (SizeOf(TAlphaColor) * Row * ABitmap.Width) );
                  AlphaColorToScanline(@PAlphaColorArray(SR)[0],
                    DR,
                    ABitmap.Width,
                    TPixelFormat.RGBA);
                end;

          B.Unmap(Dst);
          Result := B;
        end;
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
