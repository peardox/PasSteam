unit BasicProjectMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,
  SteamApp.fmx, CastleSteam, CastleLog;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure UserStatsReceived(Sender: TObject);
    procedure AppUpdate(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  Steam: TSteamApp;
  AppId: Integer = 2275430; // 2478970;// 2060130; // 316790;

implementation

{$R *.fmx}

procedure TForm2.FormCreate(Sender: TObject);
begin
  OutLog := Memo1.Lines;
  Steam := TSteamApp.Create(AppId);
  if Assigned(Steam) and Steam.Enabled then
    begin
      Steam.Interval := 17; // 60 calls per second
      Steam.OnUserStatsReceived := UserStatsReceived;
      Steam.OnAppUpdate := AppUpdate;
      AppID := Steam.Utils.GetSteamAppID;
    end;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if Assigned(Steam) then
    if Steam.Enabled then
      begin
        FreeAndNil(Steam);
      end;
end;

procedure TForm2.AppUpdate(Sender: TObject);
begin

end;

procedure TForm2.UserStatsReceived(Sender: TObject);
var
  I: Integer;
begin
  Steam.Input.Enabled := True;
  for I := 0 to Steam.UserStats.Achievements.Count - 1 do
    WriteLnLog(Steam.UserStats.Achievements[I]);
end;

end.
