program SteamVCL;

uses
  Vcl.Forms,
  VCLMain in 'src\VCLMain.pas' {Form1},
  castleapplicationproperties in 'override\castleapplicationproperties.pas',
  castlefilesutils in 'override\castlefilesutils.pas',
  castlelog in 'override\castlelog.pas',
  castlestringutils in 'override\castlestringutils.pas',
  castledynlib in 'steam\castledynlib.pas',
  castleinternalsteamapi in 'steam\castleinternalsteamapi.pas',
  castlesteam in 'steam\castlesteam.pas',
  castleutils in 'steam\castleutils.pas',
  ctypes in 'steam\ctypes.pas',
  jprse.mediatimer in 'override\jprse.mediatimer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
