program BasicProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  BasicProjectMain in 'src\BasicProjectMain.pas' {Form2},
  ctypes in '..\steam\delphi\ctypes.pas',
  castleapplicationproperties in '..\steam\override\castleapplicationproperties.pas',
  castlefilesutils in '..\steam\override\castlefilesutils.pas',
  castlelog in '..\steam\override\castlelog.pas',
  castlestringutils in '..\steam\override\castlestringutils.pas',
  castleutils in '..\steam\override\castleutils.pas',
  steamapps in '..\steam\subsystems\steamapps.pas',
  steamfriends in '..\steam\subsystems\steamfriends.pas',
  steaminput in '..\steam\subsystems\steaminput.pas',
  SteamSubsystem in '..\steam\subsystems\SteamSubsystem.pas',
  steamuser in '..\steam\subsystems\steamuser.pas',
  steamuserstats in '..\steam\subsystems\steamuserstats.pas',
  steamutils in '..\steam\subsystems\steamutils.pas',
  castledynlib in '..\steam\castledynlib.pas',
  castleinternalsteamapi in '..\steam\castleinternalsteamapi.pas',
  castlesteam in '..\steam\castlesteam.pas',
  steamtypes in '..\steam\steamtypes.pas',
  SteamApp.fmx in '..\component\SteamApp.fmx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
