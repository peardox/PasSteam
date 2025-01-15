program SteamFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXMain in 'src\FMXMain.pas' {Form1},
  castlesteam in 'steam\castlesteam.pas',
  castleinternalsteamapi in 'steam\castleinternalsteamapi.pas',
  ctypes in 'steam\ctypes.pas',
  castledynlib in 'steam\castledynlib.pas',
  castleutils in 'steam\castleutils.pas',
  castlelog in 'override\castlelog.pas',
  castlefilesutils in 'override\castlefilesutils.pas',
  castlestringutils in 'override\castlestringutils.pas',
  castleapplicationproperties in 'override\castleapplicationproperties.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
