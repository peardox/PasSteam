unit FMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation,
  CastleSteam, CastleApplicationProperties;

type
  TAchievementLine = Class(TPanel)
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure UserStatsReceived(Sender: TObject);
//    procedure AppUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Steam: TCastleSteam;

const
  AppId: Integer = 480;

implementation


{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ApplicationProperties();
  ApplicationProperties.Interval := 17;
  Steam := TCastleSteam.Create(AppId);
  Steam.OnUserStatsReceived := UserStatsReceived;
  if Steam.Enabled then
    Label1.Text := 'Steam loaded'
  else
    Label1.Text := 'Steam not loaded';
end;

procedure TForm1.UserStatsReceived(Sender: TObject);
var
  AchievementCount: Integer;
begin
  if Assigned(Steam) then
    begin
      AchievementCount := Steam.Achievements.Count;
      Label1.Text := 'Steam - User Stats Received - ' + IntToStr(AchievementCount) + ' Achievements available';
    end;
end;

end.
