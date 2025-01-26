unit lazmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SteamApp.LCL, CastleLog;

type
  TAchievementLine = Class(TPanel)
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure UserStatsReceived(Sender: TObject);
    procedure AppUpdate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Steam: TSteamApp;

const
  AppId: Integer = 316790;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Clear;
 // OutLog := Memo1.Lines;
  Steam := TSteamApp.Create(AppId);
  Steam.Interval := 17;
  Steam.OnUserStatsReceived := @UserStatsReceived;
  Steam.OnAppUpdate := @AppUpdate;
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
      Label1.Paint();
    end;
end;

procedure TForm1.AppUpdate(Sender: TObject);
var
  AchievementCount: Integer;
begin
  if Assigned(Steam) then
    begin
      AchievementCount := Steam.Achievements.Count;
      Label1.Caption := 'Steam - User Stats Received - ' + IntToStr(AchievementCount) + ' Achievements available - Update : ' + IntToStr(Steam.UpdateCount);
      Label1.Paint();
    end;
end;

end.

