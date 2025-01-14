unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
//  FMX.Controls.Presentation, Fmx.CastleControl,
  CastleSteam, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Steam: TCastleSteam;

const
  AppId: Integer = 2275430;

implementation


{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Steam := TCastleSteam.Create(AppId);

  if Steam.Enabled then
    Label1.Text := 'Steam loaded'
  else
    Label1.Text := 'Steam not loaded';
end;

end.
