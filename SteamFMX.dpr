program SteamFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$ifdef MSWINDOWS}shellapi, FMX.Platform.Win,{$endif}
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
  castleutils in 'steam\override\castleutils.pas',
  steamtypes in 'steam\steamtypes.pas';

{$R *.res}


begin
  SystemRestart := False;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  var MainForm := Form1;
  Application.Run;
{$ifdef MSWINDOWS}
  if SystemRestart then
    begin
      ShellExecute(FMX.Platform.Win.FormToHWND(MainForm), nil, PWideChar(ParamStr(0)), nil, nil, 1);
//      Application.Terminate; // or, if this is the main form, simply Close;
    end;
{$endif}

end.
