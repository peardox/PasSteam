unit FMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation,
  SteamApp.fmx, FMX.ListBox, FMX.Layouts,
  CastleLog, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl;

type
  TAchievementLine = Class(TPanel)
  end;

  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    TabItem2: TTabItem;
    Memo1: TMemo;
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
  Steam: TSteamApp;

const
  AppId: Integer = 316790;

implementation


{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutLog := Memo1.Lines;
  Steam := TSteamApp.Create(AppId);
  Steam.Interval := 17;
  Steam.OnUserStatsReceived := UserStatsReceived;
  Steam.OnAppUpdate := UserStatsReceived;
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
      Label1.Text := 'Steam - User Stats Received - ' + IntToStr(AchievementCount) + ' Achievements available - Update : ' + IntToStr(Steam.UpdateCount);
    end;
end;


end.
