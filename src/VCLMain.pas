unit VCLMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  SteamApp.vcl;

type
  TAchievementLine = Class(TPanel)
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure UserStatsReceived(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Steam: TSteamApp;

const
  AppId: Integer = 316790;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Steam := TSteamApp.Create(AppId);
  Steam.Interval := 17;
  Steam.OnUserStatsReceived := UserStatsReceived;
  Steam.OnAppUpdate := UserStatsReceived;
  if Steam.Enabled then
    Label1.Caption := 'Steam loaded'
  else
    Label1.Caption := 'Steam not loaded';
end;

procedure TForm1.UserStatsReceived(Sender: TObject);
var
  AchievementCount: Integer;
begin
  if Assigned(Steam) then
    begin
      AchievementCount := Steam.Achievements.Count;
      Label1.Caption := 'Steam - User Stats Received - ' + IntToStr(AchievementCount) + ' Achievements available - Update : ' + IntToStr(Steam.UpdateCount);
    end;
end;

end.
