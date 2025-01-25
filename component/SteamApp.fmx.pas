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
    function SteamBitmapToTBitmap(ABitmap: TSteamBitmap): TBitmap;
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

  FAvatar := nil;
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
  FreeAndNil(FAvatar);
  inherited;
end;

function RGBAArrayToBitmap(const RGBAArray: TArray<Byte>; Width, Height: Integer): TBitmap;
var
  Bitmap: TBitmap;
  x, y: Integer;
  PixelColor: TAlphaColor;
  BitmapData: TBitmapData;
begin
  Result := nil;
  // Ensure the array size matches the expected size for the given dimensions
  if Length(RGBAArray) <> Width * Height * 4 then
    raise Exception.Create('Invalid RGBA array size.');

  // Create a new TBitmap with the given dimensions
  Bitmap := TBitmap.Create(Width, Height);

  // Lock the bitmap data for writing
  if Bitmap.Map(TMapAccess.Write, BitmapData) then
  try
    // Iterate through each pixel
    for y := 0 to Height - 1 do
    begin
      for x := 0 to Width - 1 do
      begin
        // Calculate the index in the RGBA array
        // Each pixel has 4 bytes (R, G, B, A)
        PixelColor := TAlphaColor(
          (RGBAArray[(y * Width + x) * 4 + 3] shl 24) or // A
          (RGBAArray[(y * Width + x) * 4 + 0] shl 16) or // R
          (RGBAArray[(y * Width + x) * 4 + 1] shl 8) or  // G
          (RGBAArray[(y * Width + x) * 4 + 2] shl 0)     // B
        );

        // Set the pixel color in the bitmap
        BitmapData.SetPixel(x, y, PixelColor);
      end;
    end;
  finally
    // Unlock the bitmap data
    Bitmap.Unmap(BitmapData);
    Result := Bitmap;
  end;
end;

function TSteamApp.GetAvatar: TBitmap;
var
  SteamImage: TSteamBitmap;
  B: TBitmap;
  Dst: TBitmapData;
  S, D, SR, DR: Pointer;
  Row: Integer;
begin
  Result := nil;
  if UserStatsReceived then
    begin
      SteamImage := GetFriendImage(UserId);
      if SteamImage.IsValid then
        Result := SteamBitmapToTBitmap(SteamImage);
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

function TSteamApp.SteamBitmapToTBitmap(ABitmap: TSteamBitmap): TBitmap;
var
  B: TBitmap;
  Dst: TBitmapData;
  S, D, SR, DR: Pointer;
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
