program SteamFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXMain in 'src\FMXMain.pas' {Form1},
  castlesteam in 'steam\castlesteam.pas',
  castleinternalsteamapi in 'steam\castleinternalsteamapi.pas',
  castledynlib in 'steam\castledynlib.pas',
  SteamApp.fmx in 'component\SteamApp.fmx.pas',
  ctypes in 'steam\delphi\ctypes.pas',
  castleapplicationproperties in 'steam\override\castleapplicationproperties.pas',
  castlefilesutils in 'steam\override\castlefilesutils.pas',
  castlelog in 'steam\override\castlelog.pas',
  castlestringutils in 'steam\override\castlestringutils.pas',
  castleutils in 'steam\override\castleutils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
