unit FMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation,
  SteamApp.fmx, FMX.ListBox, FMX.Layouts,
  CastleLog, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  FMX.Objects;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label1: TLabel;
    ListBox1: TListBox;
    TabItem2: TTabItem;
    Memo1: TMemo;
    Image1: TImage;
    Layout1: TLayout;
    Image2: TImage;
    procedure FormCreate(Sender: TObject);
    procedure UserStatsReceived(Sender: TObject);
    procedure AppUpdate(Sender: TObject);
  private
    { Private declarations }
    FAvatar: TBitmap;
    UpCall: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Steam: TSteamApp;

const
  AppId: Integer = 2275430; //316790; {BG - 228280}  {}

implementation


{$R *.fmx}

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

procedure TForm1.UserStatsReceived(Sender: TObject);
var
  Item: TListBoxItem;
  I: Integer;
  Bitmap: TBitmap;
begin
  if Assigned(Steam) then
    begin
      Image2.Bitmap := Steam.Avatar;
      Inc(UpCall);
      ListBox1.BeginUpdate;
      ListBox1.Items.Clear;
      for I := 0 to Steam.Achievements.Count - 1 do
        begin
          Item := TListBoxItem.Create(nil);

          Item.Parent := ListBox1;
          Item.StyleLookup := 'CustomItem';
          Item.Text := '';
          Item.StylesData['LabelApiKey'] := Steam.Achievements[I].ApiId;
          Item.StylesData['LabelName'] := Steam.Achievements[I].Name;
          Item.StylesData['LabelDesc'] := Steam.Achievements[I].Desc;
          Item.StylesData['SwitchProgress'] := Steam.Achievements[I].Hidden;
          Item.StylesData['SwitchDone'] := Steam.Achievements[I].Done;
          Item.StylesData['LabelDoneDate'] := Steam.Achievements[I].DoneDate;

          Bitmap := Steam.SteamBitmapToTBitmap(Steam.Achievements[I].IconAchieved);
          if Assigned(Bitmap) then
            Item.StylesData['IconImage'] := Bitmap
          else
            Item.StylesData['IconImage'] := Image1.Bitmap;
        end;
      ListBox1.EndUpdate;
    end;
end;


end.
