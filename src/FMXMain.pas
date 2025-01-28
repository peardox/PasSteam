unit FMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation,
  SteamApp.fmx, CastleSteam, FMX.ListBox, FMX.Layouts,
  CastleLog, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  FMX.Objects;

type
  TOnItemChange = procedure(Sender: TObject; AItemIndex: Integer);

  TAchievementItem = class(TLayout)
    FOwner: TObject;
    FApiID: TLabel;
    FAchName: TLabel;
    FAchDesc: TLabel;
    FImage: TImage;
    FAchieved: TCheckbox;
    FHidden: TCheckbox;
    FLine: TLine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddData(const AItem: TSteamAchievement; const BadImage: TBitmap = nil);

  end;



  TForm1 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    TabItem2: TTabItem;
    Memo1: TMemo;
    Image1: TImage;
    Layout1: TLayout;
    Avatar: TImage;
    vsbAch: TFramedVertScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure UserStatsReceived(Sender: TObject);
    procedure AppUpdate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    UpCall: Integer;
    procedure DoAchieved(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Steam: TSteamApp;

const
  AppId: Integer = 316790; {BG - 228280}  {}

implementation


{$R *.fmx}

uses rtti;

procedure TForm1.DoAchieved(Sender: TObject);
var
  Cbx: TCheckbox;
  Ach: TSteamAchievement;
begin
  if not(Sender is TCheckbox) then
    Exit;
  Cbx := TCheckbox(Sender);
  if not(Cbx.TagObject is TSteamAchievement) then
    Exit;
  Ach := TSteamAchievement(Cbx.TagObject);
  Ach.Achieved := Cbx.IsChecked;
  if Cbx.IsChecked then
    WriteLnLog('Event ==> ', 'Set %s', [Ach.ApiId])
  else
    WriteLnLog('Event ==> ', 'Cleared %s', [Ach.ApiId]);
end;

procedure TForm1.AppUpdate(Sender: TObject);
var
  AchievementCount: Integer;
begin
  if Assigned(Steam) then
    begin
      AchievementCount := Steam.Achievements.Count;
      Label1.Text := 'Steam (' + Steam.Country + ') - User Stats Received - ' + IntToStr(AchievementCount) + ' Achievements available - Update : ' + IntToStr(Steam.UpdateCount);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OutLog := Memo1.Lines;
  Label1.Text := 'Steam - Not Loaded';
  Steam := TSteamApp.Create(AppId);
  Steam.Interval := 17; // 60 calls per second
  Steam.OnUserStatsReceived := UserStatsReceived;
  Steam.OnAppUpdate := AppUpdate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Steam);
end;

procedure TForm1.UserStatsReceived(Sender: TObject);
var
  I: Integer;
  AchItem: TAchievementItem;
begin
  if Assigned(Steam) then
    begin
      Steam.ConvertSteamImage(Steam.Avatar, Avatar.Bitmap);
      Inc(UpCall);

      vsbAch.BeginUpdate;
      for I := 0 to Steam.Achievements.Count - 1 do
        begin

          AchItem := TAchievementItem.Create(vsbAch);
          AchItem.AddData(Steam.Achievements[I], Image1.Bitmap);
          vsbAch.AddObject(AchItem);
        end;
      vsbAch.EndUpdate;
    end;
end;


{ TAchievementItem }

constructor TAchievementItem.Create(AOwner: TComponent);
begin
  inherited;

  Parent := TFmxObject(AOwner);
  Align := TAlignLayout.Top;
  Height := 116;
  Width := 640;

  FApiID := TLabel.Create(Self);
  FApiID.Parent := Self;
  FApiID.Width := 500;
  FApiID.Position.Point := PointF(8, 8);

  FAchName := TLabel.Create(Self);
  FAchName.Parent := Self;
  FAchName.Width := 500;
  FAchName.Position.Point := PointF(8, 28);

  FAchDesc := TLabel.Create(Self);
  FAchDesc.Parent := Self;
  FAchDesc.Width := 500;
  FAchDesc.Position.Point := PointF(8, 48);

  FHidden := TCheckbox.Create(Self);
  FHidden.Parent := Self;
  FHidden.Enabled := False;
  FHidden.Width := 500;
  FHidden.Position.Point := PointF(8, 68);

  FAchieved := TCheckbox.Create(Self);
  FAchieved.Parent := Self;
  FAchieved.OnChange := TForm1(AOwner).DoAchieved;
  FAchieved.Width := 500;
  FAchieved.Position.Point := PointF(8, 88);

  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Height := 100;
  FImage.Width := 100;
  FImage.Position.Point := PointF(520, 8);

  FLine := TLine.Create(Self);
  FLine.Parent := Self;
  FLine.Width := 610;
  FLine.Height := 1;
  FLine.Position.Point := PointF(8, 116);
end;

destructor TAchievementItem.Destroy;
begin
  inherited;
end;

procedure TAchievementItem.AddData(const AItem: TSteamAchievement; const BadImage: TBitmap);
begin
  FApiID.Text := AItem.ApiId;
  FAchName.Text := AItem.Name;
  FAchDesc.Text := AItem.Desc;
  FHidden.IsChecked := AItem.Hidden;
  FHidden.Text := 'Hidden';
  FAchieved.IsChecked := AItem.Achieved;
  if AItem.Achieved then
    FAchieved.Text := 'Achieved on ' + DateTimeToStr(AItem.DoneDate)
  else
    FAchieved.Text := 'Not Achieved Yet';

  if AItem.Icon <> 0 then
    begin
      Steam.ConvertSteamImage(AItem.Icon, FImage.Bitmap);
      if not Assigned(FImage.Bitmap) then
        FImage.Bitmap := BadImage;
    end
  else
    FImage.Bitmap := BadImage;

end;

end.
