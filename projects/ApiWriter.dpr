program ApiWriter;

uses
  System.StartUpCopy,
  FMX.Forms,
  ApiWriterMain in 'src\ApiWriterMain.pas' {ApiWriterForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TApiWriterForm, ApiWriterForm);
  Application.Run;
end.
