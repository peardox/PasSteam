program FMX3D;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'src\Unit1.pas' {Form1},
  steamtypes in '..\steam\steamtypes.pas',
  SteamSubsystem in '..\steam\SteamSubsystem.pas',
  castlelog in '..\steam\override\castlelog.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
