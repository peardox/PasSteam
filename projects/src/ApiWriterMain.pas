unit ApiWriterMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  System.JSON, System.Generics.Collections, IOUtils;

type

  TEnumValue = class
    name: String;
    value: String;
  end;
  TEnumValues = Array of TEnumValue;

  TCallbackConst = class
    constname: String;
    consttype: String;
    constval: String;
  end;
  TCallbackConsts = Array of TCallbackConst;

  TLongEnum = class
    enumname: String;
    fqname: String;
    values: TEnumValues;
    destructor Destroy; override;
    procedure BuildValues(Ary: TJSONArray);
  end;
  TCallbackEnums = Array of TLongEnum;
  TInterfaceEnums = Array of TLongEnum;

  TSteamField = class
    fieldname: String;
    fieldtype: String;
  end;
  TCallbackFieldArray = Array of TSteamField;
  TInterfaceFieldArray = Array of TSteamField;

  TCallbackStruct = class
    callback_id: Int32;
    fields: TCallbackFieldArray;
    struct: String;
    enums: TCallbackEnums;
    consts: TCallbackConsts;
    procedure BuildEnums(Ary: TJSONArray);
    procedure BuildConsts(Ary: TJSONArray);
    procedure BuildFields(Ary: TJSONArray);
    destructor Destroy; override;
  end;
  TCallbackList = TObjectList<TCallbackStruct>;

  TTypeDef = class
    typedef: String;
    typedeftype: String;
  end;
  TTYpeDefList = TObjectList<TTypeDef>;


  TAccessor = class
    kind: String;
    name: String;
    name_flat: String;
  end;
  TAccessors = Array of TAccessor;

  TParam = class
    paramname: String;
    paramtype: String;
    paramtype_flat: String;
    array_count: String;
    buffer_count: String;
    desc: String;
    out_struct: String;
    out_array_call: String;
    out_string_count: String;
    out_string: String;
    out_array_count: String;
    out_buffer_count: String;
  end;
  TParams = Array of TParam;

  TMethod = class
    methodname: String;
    methodname_flat: String;
    params: TParams;
    returntype: String;
    returntype_flat: String;
    callresult: String;
    callback: String;
    desc: String;
    procedure BuildParams(Ary: TJSONArray);
    destructor Destroy; override;
  end;
  TMethods = Array of TMethod;

  TInterface = class
    accessors: TAccessors;
    classname: String;
    fields: TInterfaceFieldArray;
    methods: TMethods;
    enums: TInterfaceEnums;
    version_string: String;
    procedure BuildAccessors(Ary: TJSONArray);
    procedure BuildEnums(Ary: TJSONArray);
    procedure BuildMethods(Ary: TJSONArray);
    destructor Destroy; override;
  end;
  TInterfaceList = TObjectList<TInterface>;

  TApiWriterForm = class(TForm)
    Layout1: TLayout;
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    CallbackList: TCallbackList;
    TypeDefList: TTypeDefList;
    InterfaceList: TInterfaceList;
    procedure JSONSections(Value: TJSONValue);
    procedure SkipArray(Value: TJSONArray);
    procedure HandleArray(AList: TInterfaceList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TInterfaceList; Value: TJSONValue); overload;
    procedure HandleArray(AList: TCallbackList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TCallbackList; Value: TJSONValue); overload;
    procedure HandleArray(AList: TTypeDefList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TTypeDefList; Value: TJSONValue); overload;
    { Private declarations }
  public
    { Public declarations }
  end;

const
  jsonsrc_test = '../../../data/redistributable_testing_bin/steam_api.json';
  jsonsrc_live = '../../../data/redistributable_live_bin/steam_api.json';
  json_api = 'C:\work\steamworks_sdk_148a\sdk\public\steam\steam_api.json';
var
  Sections: TStringList;

  ApiWriterForm: TApiWriterForm;

implementation

{$R *.fmx}


procedure TApiWriterForm.FormCreate(Sender: TObject);
begin
  Sections := TStringList.Create;
  Sections.Duplicates := dupError;
  Sections.Sorted := True;
  Sections.Add('callback_structs'); // Done
  Sections.Add('consts');
  Sections.Add('enums');
  Sections.Add('interfaces'); // Done
  Sections.Add('structs');
  Sections.Add('typedefs'); // Done
  CallbackList := TCallbackList.Create;
  TypeDefList := TTypeDefList.Create;
  InterfaceList := TInterfaceList.Create;
end;

procedure TApiWriterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(InterfaceList);
  FreeAndNil(TypeDefList);
  FreeAndNil(CallbackList);
  FreeAndNil(Sections);
end;

procedure TApiWriterForm.HandleArrayItem(AItem: TCallbackList; Value: TJSONValue);
var
  Pair: TJSONPair;
  I: Integer;
  AKey: String;
  O: TCallbackStruct;
begin
  if Value is TJSONObject then
    begin
      O := TCallbackStruct.Create;
      try
        for I := 0 to TJSONObject(Value).Count - 1 do
          begin
            Pair := TJSONObject(Value).Pairs[I];
            AKey := Pair.JsonString.Value;
//            Memo1.Lines.Add(Format('Item : %s',[AKey]));
            if AKey = 'callback_id' then
              begin
                if Pair.JsonValue is TJSONNumber then
                  O.callback_id := Pair.JsonValue.AsType<Int32>
                else
                  Raise Exception.Create('CallbackList - callback_id is non-numeric');
              end
            else if AKey = 'struct' then
              begin
                if Pair.JsonValue is TJSONString then
                  O.struct := Pair.JsonValue.AsType<String>
                else
                  Raise Exception.Create('CallbackList - struct is non-string');
              end
            else if AKey = 'fields' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildFields(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.Create('CallbackList - fields is non-array');
              end
            else if AKey = 'enums' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildEnums(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.Create('CallbackList - enums is non-array');
              end
            else if AKey = 'consts' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildConsts(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.Create('CallbackList - consts is non-array');
              end
            else
              Raise Exception.CreateFmt('CallbackList - Unknown Key : %s for %d', [AKey, O.callback_id]);
          end;
      finally
        AItem.Add(O);
      end;
    end
  else
    Raise Exception.Create('Error in CallbackList');

end;

procedure TApiWriterForm.HandleArray(AList: TCallbackList; Value: TJSONArray);
var
  I: Integer;
begin
  for I := 0 to Value.Count - 1 do
    HandleArrayItem(AList, Value.Items[I]);
end;

procedure TApiWriterForm.Button1Click(Sender: TObject);
var
  JsonText: String;
  Value: TJSONValue;
begin
  JsonText := TFile.ReadAllText(jsonsrc_test);
//  JsonText := TFile.ReadAllText(jsonsrc_live);
//  JsonText := TFile.ReadAllText(json_api);
  Value := TJSONObject.ParseJSONValue(JsonText);
  Memo1.Lines.Add(Format('TJSONValue is %s',[Value.ClassName]));
  if Value is TJSONObject then
    JSONSections(Value);
  FreeAndNil(Value);
end;

procedure TApiWriterForm.JSONSections(Value: TJSONValue);
var
  Pair: TJSONPair;
  I, IDX: Integer;
  AKey: String;
  AValue: TJSONValue;
begin
  if Value is TJSONObject then
    begin
      for I := 0 to TJSONObject(Value).Count - 1 do
        begin
          Pair := TJSONObject(Value).Pairs[I];
          AKey := Pair.JsonString.Value;
          IDX := Sections.IndexOf(AKey);
          if IDX = -1 then
            Raise Exception.CreateFmt('Section %s not found in pre-defined list',[Akey]);
          if Pair.JsonValue is TJSONArray then
            begin
            Memo1.Lines.Add(Format('Pair %s is %s (%d)',[AKey, Pair.JsonValue.ClassName, TJSONArray(Pair.JsonValue).Count]));
            case IDX of
              0: // callback_structs
//                HandleArray(CallbackList, TJSONArray(Pair.JsonValue));
                SkipArray(TJSONArray(Pair.JsonValue));
              1: // consts
                SkipArray(TJSONArray(Pair.JsonValue));
              2: // enums
                SkipArray(TJSONArray(Pair.JsonValue));
              3: // interfaces
                HandleArray(InterfaceList, TJSONArray(Pair.JsonValue));
//                SkipArray(TJSONArray(Pair.JsonValue));
              4: // structs
                SkipArray(TJSONArray(Pair.JsonValue));
              5: // typedefs
//                HandleArray(TypeDefList, TJSONArray(Pair.JsonValue));
                SkipArray(TJSONArray(Pair.JsonValue));
              else
                Raise Exception.Create('Impossible Section');
              end;
            end
          else
            Raise Exception.CreateFmt('Section %s should be an array',[Akey]);
        end;
    end;
end;

procedure TApiWriterForm.SkipArray(Value: TJSONArray);
begin

end;

{ TCallbackStruct }

destructor TCallbackStruct.Destroy;
var
  I: Integer;
begin
  for I := Length(enums) - 1 downto 0 do
    FreeAndNil(enums[I]);
  for I := Length(consts) - 1 downto 0 do
    FreeAndNil(consts[I]);
  for I := Length(fields) - 1 downto 0 do
    FreeAndNil(fields[I]);
  inherited;
end;

procedure TCallbackStruct.BuildConsts(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TCallbackConst;
  AKey: String;
begin
  SetLength(consts, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TCallbackConst.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'constname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.constname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback constname %s is non-string', [AKey]);
                end
              else if AKey = 'consttype' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.consttype := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback consttype %s is non-string', [AKey]);
                end
              else if AKey = 'constval' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.constval := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback constval %s is non-string', [AKey]);
                end
              else
                Raise Exception.CreateFmt('Callback const %s is unhandled', [AKey]);
            end;
          consts[I] := F;
        end
      else
        Raise Exception.CreateFmt('CallbackList - const %d of %d', [I, callback_id]);
    end;
end;

procedure TCallbackStruct.BuildEnums(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TLongEnum;
  AKey: String;
begin
  SetLength(enums, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TLongEnum.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'enumname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.enumname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback enum %s is non-string', [AKey]);
                end
              else if AKey = 'fqname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.fqname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback enum %s is non-string', [AKey]);
                end
              else if AKey = 'values' then
                begin
                  if Pair.JsonValue is TJSONArray then
                    F.BuildValues(TJSONArray(Pair.JsonValue))
                  else
                    Raise Exception.CreateFmt('Callback enum %s is non-array', [AKey]);
                end
              else
                Raise Exception.CreateFmt('Callback enum %s is unhandled', [AKey]);
            end;
          enums[I] := F;
        end
      else
        Raise Exception.CreateFmt('CallbackList - enum %d of %d', [I, callback_id]);

    end;
end;

procedure TCallbackStruct.BuildFields(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TSteamField;
  AKey: String;
begin
  SetLength(fields, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TSteamField.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'fieldname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.fieldname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback Field %s is non-string', [AKey]);
                end
              else if AKey = 'fieldtype' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.fieldtype := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Callback Field %s is non-string', [AKey]);
                end
              else
                Raise Exception.CreateFmt('Callback Field %s is unhandled', [AKey]);
            end;
          fields[I] := F;
        end
      else
        Raise Exception.CreateFmt('CallbackList - Field %d of %d', [I, callback_id]);
    end;
end;

{ TCallbackEnum }

procedure TLongEnum.BuildValues(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TEnumValue;
  AKey: String;
begin
  SetLength(values, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TEnumValue.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'name' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.name := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Enaum Value Name %s is non-string', [AKey]);
                end
              else if AKey = 'value' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.value := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('Enum Value Value %s is non-string', [AKey]);
                end
              else
                Raise Exception.CreateFmt('Enum Value %s is unhandled', [AKey]);
            end;
          values[I] := F;
        end
      else
        Raise Exception.Create('Value - Not an object');
    end;
end;

destructor TLongEnum.Destroy;
var
  I: Integer;
begin
  for I := Length(values) - 1 downto 0 do
    FreeAndNil(values[I]);
  inherited;
end;

procedure TApiWriterForm.HandleArray(AList: TTypeDefList; Value: TJSONArray);
var
  I: Integer;
begin
  for I := 0 to Value.Count - 1 do
    HandleArrayItem(AList, Value.Items[I]);
end;

procedure TApiWriterForm.HandleArray(AList: TInterfaceList; Value: TJSONArray);
var
  I: Integer;
begin
  for I := 0 to Value.Count - 1 do
    HandleArrayItem(AList, Value.Items[I]);
end;

procedure TApiWriterForm.HandleArrayItem(AItem: TTypeDefList;
  Value: TJSONValue);
var
  Pair: TJSONPair;
  I: Integer;
  AKey: String;
  O: TTypeDef;
begin
  if Value is TJSONObject then
    begin
      O := TTypeDef.Create;
      try
        for I := 0 to TJSONObject(Value).Count - 1 do
          begin
            Pair := TJSONObject(Value).Pairs[I];
            AKey := Pair.JsonString.Value;
//            Memo1.Lines.Add(Format('Item : %s',[AKey]));
            if AKey = 'typedef' then
              begin
                if Pair.JsonValue is TJSONString then
                  O.typedef := Pair.JsonValue.AsType<String>
                else
                  Raise Exception.Create('TypeDefList - typedef is non-string');
              end
            else if AKey = 'type' then
              begin
                if Pair.JsonValue is TJSONString then
                  O.typedeftype := Pair.JsonValue.AsType<String>
                else
                  Raise Exception.Create('TypeDefList - type is non-string');
              end
            else
              Raise Exception.CreateFmt('TypeDefList - Unknown Key : %s for %s', [AKey, O.typedef]);
          end;
      finally
        AItem.Add(O);
      end;
    end
  else
    Raise Exception.Create('Error in TypeDefList');

end;

procedure TApiWriterForm.HandleArrayItem(AItem: TInterfaceList;
  Value: TJSONValue);
var
  Pair: TJSONPair;
  I: Integer;
  AKey: String;
  O: TInterface;
begin
  if Value is TJSONObject then
    begin
      O := TInterface.Create;
      try
        for I := 0 to TJSONObject(Value).Count - 1 do
          begin
            Pair := TJSONObject(Value).Pairs[I];
            AKey := Pair.JsonString.Value;
            Memo1.Lines.Add(Format('Item : %s',[AKey]));
            if AKey = 'accessors' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildAccessors(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.Create('InterfaceList - accessor is non-array');
              end
            else if AKey = 'fields' then
              begin
                if Pair.JsonValue is TJSONArray then
                  begin
                  if TJSONArray(Pair.JsonValue).Count = 0 then
                    O.fields := Nil
                  else
                    Raise Exception.CreateFmt('InterfaceList - method %s has $d fields. Not able to handle until data seen',[O.methods, TJSONArray(Pair.JsonValue).Count]);
                  end
                else
                  Raise Exception.Create('InterfaceList - fields is non-array');
              end
            else if AKey = 'methods' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildMethods(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.Create('InterfaceList - methods is non-array');
              end
            else if AKey = 'enums' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildEnums(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.Create('InterfaceList - enums is non-array');
              end
            else if AKey = 'classname' then
              begin
                if Pair.JsonValue is TJSONString then
                  O.classname := Pair.JsonValue.AsType<String>
                else
                  Raise Exception.Create('InterfaceList - classname is non-string');
              end
            else if AKey = 'version_string' then
              begin
                if Pair.JsonValue is TJSONString then
                  O.version_string := Pair.JsonValue.AsType<String>
                else
                  Raise Exception.Create('InterfaceList - version_string is non-string');
              end
            else
              Raise Exception.CreateFmt('InterfaceList - Unknown Key : %s for %s', [AKey, O.classname]);
          end;
      finally
        AItem.Add(O);
      end;
    end
  else
    Raise Exception.Create('Error in InterfaceList');

end;

{ TInterface }

procedure TInterface.BuildAccessors(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TAccessor;
  AKey: String;
begin
  SetLength(accessors, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TAccessor.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'kind' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.kind := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-string', [AKey]);
                end
              else if AKey = 'name' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.name := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-string', [AKey]);
                end
              else if AKey = 'name_flat' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.name_flat := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-string', [AKey]);
                end
              else
                Raise Exception.CreateFmt('InterfaceList enum %s is unhandled', [AKey]);
            end;
          accessors[I] := F;
        end
      else
        Raise Exception.CreateFmt('InterfaceList - enum %d', [I]);

    end;
end;

procedure TInterface.BuildEnums(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TLongEnum;
  AKey: String;
begin
  SetLength(enums, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TLongEnum.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'enumname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.enumname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-string', [AKey]);
                end
              else if AKey = 'fqname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.fqname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-string', [AKey]);
                end
              else if AKey = 'values' then
                begin
                  if Pair.JsonValue is TJSONArray then
                    F.BuildValues(TJSONArray(Pair.JsonValue))
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-array', [AKey]);
                end
              else
                Raise Exception.CreateFmt('InterfaceList enum %s is unhandled', [AKey]);
            end;
          enums[I] := F;
        end
      else
        Raise Exception.CreateFmt('InterfaceList - enum %d', [I]);

    end;
end;

procedure TInterface.BuildMethods(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TMethod;
  AKey: String;
begin
  SetLength(methods, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TMethod.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'methodname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.methodname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList methodname %s is non-string', [AKey]);
                end
              else if AKey = 'methodname_flat' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.methodname_flat := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList methodname_flat %s is non-string', [AKey]);
                end
              else if AKey = 'returntype' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.returntype := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList returntype %s is non-string', [AKey]);
                end
              else if AKey = 'returntype_flat' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.returntype_flat := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList returntype %s is non-string', [AKey]);
                end
              else if AKey = 'callresult' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.callresult := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList returntype %s is non-string', [AKey]);
                end
              else if AKey = 'callback' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.callback := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList returntype %s is non-string', [AKey]);
                end
              else if AKey = 'desc' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.desc := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList returntype %s is non-string', [AKey]);
                end
                {
              else if AKey = 'ignore' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.ignore := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList returntype %s is non-string', [AKey]);
                end
                }
              else if AKey = 'params' then
                begin
                  if Pair.JsonValue is TJSONArray then
                    f.BuildParams(TJSONArray(Pair.JsonValue))
                  else
                    Raise Exception.CreateFmt('InterfaceList enum %s is non-array', [AKey]);
                end
              else
                Raise Exception.CreateFmt('InterfaceList %s is unhandled', [AKey]);
            end;
          methods[I] := F;
        end
      else
        Raise Exception.CreateFmt('InterfaceList - enum %d', [I]);

    end;
end;

destructor TInterface.Destroy;
var
  I: Integer;
begin
  for I := Length(enums) - 1 downto 0 do
    FreeAndNil(enums[I]);
  for I := Length(accessors) - 1 downto 0 do
    FreeAndNil(accessors[I]);
  for I := Length(methods) - 1 downto 0 do
    FreeAndNil(methods[I]);
  inherited;
end;

{ TMethod }

procedure TMethod.BuildParams(Ary: TJSONArray);
var
  Pair: TJSONPair;
  I, P: Integer;
  F: TParam;
  AKey: String;
begin
  SetLength(params, Ary.Count);
  for I := 0 to Ary.Count - 1 do
    begin
      if Ary.Items[I] is TJSONObject then
        begin
          F := TParam.Create;
          for P := 0 to TJSONObject(Ary.Items[I]).Count - 1 do
            begin
              Pair := TJSONObject(Ary.Items[I]).Pairs[P];
              AKey := Pair.JsonString.Value;
              if AKey = 'paramname' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.paramname := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'paramtype' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.paramtype := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'paramtype_flat' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.paramtype_flat := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'out_struct' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.out_struct := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'out_array_call' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.out_array_call := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'array_count' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.array_count := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'out_string_count' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.out_string_count := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'out_array_count' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.out_array_count := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'out_buffer_count' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.out_buffer_count := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'buffer_count' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.buffer_count := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'out_string' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.out_string := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else if AKey = 'desc' then
                begin
                  if Pair.JsonValue is TJSONString then
                    F.desc := Pair.JsonValue.AsType<String>
                  else
                    Raise Exception.CreateFmt('InterfaceList param %s is non-string', [AKey]);
                end
              else
                Raise Exception.CreateFmt('InterfaceList param %s is unhandled', [AKey]);
            end;
          params[I] := F;
        end
      else
        Raise Exception.CreateFmt('InterfaceList - param %d', [I]);

    end;
end;

destructor TMethod.Destroy;
var
  I: Integer;
begin
  for I := Length(params) - 1 downto 0 do
    FreeAndNil(params[I]);
  inherited;
end;

end.
