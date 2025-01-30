unit ApiWriterMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  System.JSON, System.Generics.Collections, IOUtils;

type
  TApiWriterForm = class(TForm)
    Layout1: TLayout;
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure RecurseJsonObject(const Obj: TJSONObject);
    procedure RecurseJsonArray(const Obj: TJSONArray);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  jsonsrc_test = '../../../data/redistributable_testing_bin/steam_api.json';
  jsonsrc_live = '../../../data/redistributable_live_bin/steam_api.json';

var
  ApiWriterForm: TApiWriterForm;

implementation

{$R *.fmx}



procedure TApiWriterForm.Button1Click(Sender: TObject);
var
  JsonText: String;
  ValRoot: TJSONValue;
  Pair: TJSONPair;
  I: Integer;
begin
  JsonText := TFile.ReadAllText(jsonsrc_test);
  ValRoot := TJSONObject.ParseJSONValue(JsonText);
  if ValRoot is TJSONObject then
    begin
      Memo1.Lines.Add(Format('ValRoot is TJSONObject [%d]',[TJSONObject(ValRoot).Count]));
      // "callback_structs", "consts", "enums", "interfaces", "structs", "typedefs"
      for I := 0 to TJSONObject(ValRoot).Count - 1 do
        begin
          Pair := TJSONObject(ValRoot).Pairs[I];
          Memo1.Lines.Add(Format('%d = %s',[I, Pair.JsonString.Value]));
          if Pair.JsonValue is TJSONArray then
            begin
              Memo1.Lines.Add(Format('%s is TJSONArray [%d]',[Pair.JsonString.Value, TJSONArray(Pair.JsonValue).Count]));
              if Pair.JsonValue is TJSONArray then
                RecurseJsonArray(TJSONArray(Pair.JsonValue))
              else if Pair.JsonValue is TJSONObject then
                RecurseJsonObject(TJSONObject(Pair.JsonValue))
              else
                Memo1.Lines.Add('Unhandled Pair');
            end
          else
          if Pair.JsonValue is TJSONObject then
            begin
              Memo1.Lines.Add(Format('%s is TJSONObject [%d]',[Pair.JsonString.Value, TJSONObject(Pair.JsonValue).Count]));
              if Pair.JsonValue is TJSONObject then
                RecurseJsonObject(TJSONObject(Pair.JsonValue))
              else
                Memo1.Lines.Add('Unhandled Pair');
            end
          else
            Memo1.Lines.Add('Unhandled Pair');

        end;

    end
  else
    Memo1.Lines.Add('Unhandled ValRoot');
end;

procedure TApiWriterForm.RecurseJsonArray(const Obj: TJSONArray);
var
  Pair: TJSONPair;
  I,J: Integer;
begin
  for I := 0 to Obj.Count - 1 do
    begin
      if Obj[I] is TJSONObject then
        begin
          for J := 0 to TJSONObject(Obj[I]).Count - 1 do
            begin
              Pair := TJSONObject(Obj[I]).Pairs[J];
              Memo1.Lines.Add(Format('%d = %s ==> %s [%d]',[I, Pair.JsonString.Value, Pair.JsonValue.ClassName, TJSONObject(Obj[I]).Count]));
              if Pair.JsonValue is TJSONArray then
                RecurseJsonArray(Pair.JsonValue.AsType<TJSONArray>)
              else if Pair.JsonValue is TJSONObject then
                RecurseJsonObject(Pair.JsonValue.AsType<TJSONObject>)
              else if Pair.JsonValue is TJSONNumber then
                Memo1.Lines.Add(Format('    [%d]',[Pair.JsonValue.AsType<Int64>]))
              else if Pair.JsonValue is TJSONString then
                Memo1.Lines.Add(Format('    "%s"',[Pair.JsonValue.Value]));
            end;
        end
      else if Obj[I] is TJSONArray then
        begin
          Memo1.Lines.Add(Format('Array Obj[%d]', [I]));
        end
      else
        Memo1.Lines.Add('Unhandled Obj');
    end;
end;

procedure TApiWriterForm.RecurseJsonObject(const Obj: TJSONObject);
var
  Pair: TJSONPair;
  I: Integer;
begin
  if Obj is TJSONObject then
    begin
      for I := 0 to Obj.Count - 1 do
        begin
          Pair := TJSONObject(Obj).Pairs[I];
          Memo1.Lines.Add(Format('%d = %s',[I, Pair.JsonString.Value]));
          if Pair.JsonValue is TJSONArray then
            begin
              Memo1.Lines.Add(Format('%s is TJSONArray [%d]',[Pair.JsonString.Value, TJSONArray(Pair.JsonValue).Count]));
            end
          else
          if Pair.JsonValue is TJSONObject then
            begin
              Memo1.Lines.Add(Format('%s is TJSONObject [%d]',[Pair.JsonString.Value, TJSONObject(Pair.JsonValue).Count]));
            end
          else
            Memo1.Lines.Add('Unhandled Pair');
        end;
    end
  else
    Memo1.Lines.Add('Unhandled Obj');
end;

end.
