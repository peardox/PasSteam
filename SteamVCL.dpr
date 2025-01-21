program SteamVCL;

uses
  Vcl.Forms,
  VCLMain in 'src\VCLMain.pas' {Form1},
  SteamApp.vcl in 'component\SteamApp.vcl.pas',
  castledynlib in 'steam\castledynlib.pas',
  castleinternalsteamapi in 'steam\castleinternalsteamapi.pas',
  castlesteam in 'steam\castlesteam.pas',
  ctypes in 'steam\delphi\ctypes.pas',
  castleapplicationproperties in 'steam\override\castleapplicationproperties.pas',
  castlefilesutils in 'steam\override\castlefilesutils.pas',
  castlelog in 'steam\override\castlelog.pas',
  castlestringutils in 'steam\override\castlestringutils.pas',
  castleutils in 'steam\override\castleutils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
