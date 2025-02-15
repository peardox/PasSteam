unit ApiWriterMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  System.JSON, System.Generics.Defaults, System.Generics.Collections, IOUtils;

type
  TTypes = record
    AClassName: String;
    APointerName: String;
  end;

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

  TShortEnum = class
    enumname: String;
    values: TEnumValues;
    destructor Destroy; override;
    procedure BuildValues(Ary: TJSONArray);
  end;
  TEnumList = TObjectList<TShortEnum>;

  TLongEnum = class(TShortEnum)
    fqname: String;
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
  TMethods = TObjectList<TMethod>; // was Array of

  TInterface = class
    accessors: TAccessors;
    cmethod: String;
    fields: TInterfaceFieldArray;
    methods: TMethods;
    enums: TInterfaceEnums;
    version_string: String;
    constructor Create;
    procedure BuildAccessors(Ary: TJSONArray);
    procedure BuildEnums(Ary: TJSONArray);
    procedure BuildMethods(Ary: TJSONArray);
    destructor Destroy; override;
  end;
  TInterfaceList = TObjectList<TInterface>;
  TCTypeToPas = TDictionary<String, String>;
  TTypeToPas = TDictionary<String, String>;

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
    EnumList: TEnumList;
    // consts, enums, structs
    procedure MakePasTypes;
    procedure MakeCPasTypes;
    procedure JSONSections(Value: TJSONValue);
    procedure SkipArray(Value: TJSONArray);
    procedure HandleArray(AList: TEnumList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TEnumList; Value: TJSONValue); overload;
    procedure HandleArray(AList: TInterfaceList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TInterfaceList; Value: TJSONValue); overload;
    procedure HandleArray(AList: TCallbackList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TCallbackList; Value: TJSONValue); overload;
    procedure HandleArray(AList: TTypeDefList; Value: TJSONArray); overload;
    procedure HandleArrayItem(AItem: TTypeDefList; Value: TJSONValue); overload;
    procedure ShowCode;
    procedure ProcessJSON;
    { Private declarations }
  public
    { Public declarations }
  end;

const
  jsonsrc_test = '../../../data/redistributable_testing_bin/steam_api.json';
  jsonsrc_live = '../../../data/redistributable_live_bin/steam_api.json';
  json_api = 'C:\work\steamworks_sdk_148a\sdk\public\steam\steam_api.json';
var
  ApiWriterForm: TApiWriterForm;
  Sections: TStringList;
  TypeToPas: TTypeToPas;
  CTypeToPas: TCTypeToPas;
  NeedType: TStringList;

implementation

{$R *.fmx}

function ConvertType(const AType: String): String;
begin
  if not CTypeToPas.TryGetValue(AType, Result) then
    begin
      if NeedType.IndexOf(AType) = -1 then
        NeedType.Add(AType);

      Result := '{ Unconverted } ' + AType;
    end;
end;

function MangleTypes(const AName: String):TTypes;
begin
  if AName.EndsWith('_t') then
    begin
      Result.AClassName := 'T' + AName.Substring(0, Length(AName)-2);
      Result.APointerName := 'P' + AName.Substring(0, Length(AName)-2);
    end
  else
    begin
      Result.AClassName := AName;
      Result.APointerName := AName;
    end;
end;

procedure TApiWriterForm.Button1Click(Sender: TObject);
begin
  ProcessJSON;
end;

procedure TApiWriterForm.ProcessJSON;
var
  JsonText: String;
  Value: TJSONValue;
begin
  Memo1.Lines.Clear;
  JsonText := TFile.ReadAllText(jsonsrc_test);
//  JsonText := TFile.ReadAllText(jsonsrc_live);
//  JsonText := TFile.ReadAllText(json_api);
  Value := TJSONObject.ParseJSONValue(JsonText);
//  Memo1.Lines.Add(Format('TJSONValue is %s',[Value.ClassName]));
  if Value is TJSONObject then
    JSONSections(Value);
  FreeAndNil(Value);
end;

procedure TApiWriterForm.FormCreate(Sender: TObject);
begin
  NeedType := TStringList.Create;
  NeedType.Duplicates := dupError;
  NeedType.Sorted := True;

  Sections := TStringList.Create;
  Sections.Duplicates := dupError;
  Sections.Sorted := True;
  Sections.Add('callback_structs'); // Done
  Sections.Add('consts');
  Sections.Add('enums'); // done
  Sections.Add('interfaces'); // Done
  Sections.Add('structs');
  Sections.Add('typedefs'); // Done

  MakePasTypes;
  MakeCPasTypes;

  CallbackList := TCallbackList.Create;
  TypeDefList := TTypeDefList.Create;
  InterfaceList := TInterfaceList.Create;
  EnumList := TEnumList.Create;

  ProcessJSON;
end;

procedure TApiWriterForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EnumList);
  FreeAndNil(InterfaceList);
  FreeAndNil(TypeDefList);
  FreeAndNil(CallbackList);
  FreeAndNil(Sections);
  FreeAndNil(CTypeToPas);
  FreeAndNil(TypeToPas);
  FreeAndNil(NeedType);
end;

procedure TApiWriterForm.JSONSections(Value: TJSONValue);
var
  Pair: TJSONPair;
  I, IDX: Integer;
  AKey: String;
  AValue: TJSONValue;
begin
  CallbackList.Clear;
  TypeDefList.Clear;
  InterfaceList.Clear;
  EnumList.Clear;

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
//            Memo1.Lines.Add(Format('Pair %s is %s (%d)',[AKey, Pair.JsonValue.ClassName, TJSONArray(Pair.JsonValue).Count]));
            case IDX of
              0: // callback_structs
                HandleArray(CallbackList, TJSONArray(Pair.JsonValue));
//                SkipArray(TJSONArray(Pair.JsonValue));
              1: // consts
                SkipArray(TJSONArray(Pair.JsonValue));
              2: // enums
//                HandleArray(EnumList, TJSONArray(Pair.JsonValue));
                SkipArray(TJSONArray(Pair.JsonValue));
              3: // interfaces
                HandleArray(InterfaceList, TJSONArray(Pair.JsonValue));
//                SkipArray(TJSONArray(Pair.JsonValue));
              4: // structs
                SkipArray(TJSONArray(Pair.JsonValue));
              5: // typedefs
                HandleArray(TypeDefList, TJSONArray(Pair.JsonValue));
//                SkipArray(TJSONArray(Pair.JsonValue));
              else
                Raise Exception.Create('Impossible Section');
              end;
            end
          else
            Raise Exception.CreateFmt('Section %s should be an array',[Akey]);
        end;

       CallbackList.Sort(TComparer<TCallbackStruct>.Construct(
          function (const L, R: TCallbackStruct): integer
          begin
            if L.callback_id = R.callback_id then
              Result := 0
            else if L.callback_id < R.callback_id then
              Result := -1
            else
              Result := 1;
          end
          ));

       InterfaceList.Sort(TComparer<TInterface>.Construct(
          function (const L, R: TInterface): integer
          begin
              // Force ISteamClient to start
              if CompareText(L.classname, 'ISteamClient') = 0 then
                Result := -1
              else if CompareText(R.classname, 'ISteamClient') = 0 then
                Result := 1
              else
              // The rest just alpha-sort
                Result := CompareText(L.classname, R.classname);
          end
          ));

        ShowCode;
    end;
end;

procedure TApiWriterForm.MakePasTypes;
begin
  TypeToPas := TTypeToPas.Create;
  TypeToPas.Add('HSteamPipe',     'Int32');
  TypeToPas.Add('HSteamUser',     'Int32');
  TypeToPas.Add('CSteamId',       'UInt64');
  TypeToPas.Add('CGameID',        'UInt64');
  TypeToPas.Add('CUserID',        'UInt64');
  TypeToPas.Add('EResult',        'UInt32');
  TypeToPas.Add('TAppId',         'UInt32');
  TypeToPas.Add('PSteamErrMsg',   'PChar');
  TypeToPas.Add('SteamAPICall_t', 'UInt64');

  TypeToPas.Add('TArraySingle50', 'Array[0..49] of Single');
  TypeToPas.Add('TArrayWord8',    'Array[0..7] of Word');
  TypeToPas.Add('TArrayDWord50',  'Array[0..49] of DWord');
  TypeToPas.Add('TArrayByte8',    'Array[0..7] of Byte');
  TypeToPas.Add('TArrayByte2560', 'Array[0..2559] of Byte');

  TypeToPas.Add('TAnsiChar8000',  'Array[0..7999] of AnsiChar');
  TypeToPas.Add('TAnsiChar2048',  'Array[0..2047] of AnsiChar');
  TypeToPas.Add('TAnsiChar1025',  'Array[0..1024] of AnsiChar');
  TypeToPas.Add('TAnsiChar1024',  'Array[0..1023] of AnsiChar');
  TypeToPas.Add('TAnsiChar512',   'Array[0..511] of AnsiChar');
  TypeToPas.Add('TAnsiChar260',   'Array[0..259] of AnsiChar');
  TypeToPas.Add('TAnsiChar256',   'Array[0..255] of AnsiChar');
  TypeToPas.Add('TAnsiChar240',   'Array[0..239] of AnsiChar');
  TypeToPas.Add('TAnsiChar129',   'Array[0..128] of AnsiChar');
  TypeToPas.Add('TAnsiChar128',   'Array[0..127] of AnsiChar');
  TypeToPas.Add('TAnsiChar64',    'Array[0..63] of AnsiChar');
  TypeToPas.Add('TAnsiChar32',    'Array[0..31] of AnsiChar');
  TypeToPas.Add('TAnsiChar4',     'Array[0..3] of AnsiChar');
end;

procedure TApiWriterForm.MakeCPasTypes;
begin
  CTypeToPas := TCTypeToPas.Create;
  // Simple Types
  CTypeToPas.Add('unsigned long long', 'UInt64');
  CTypeToPas.Add('long long', 'Int64');
  CTypeToPas.Add('unsigned int', 'UInt32');
  CTypeToPas.Add('int', 'Int32');
  CTypeToPas.Add('int32', 'Int32');
  CTypeToPas.Add('short', 'Int16');
  CTypeToPas.Add('unsigned short', 'UInt16');
  CTypeToPas.Add('int8', 'Int8');
  CTypeToPas.Add('uint8', 'UInt8');
  CTypeToPas.Add('uint16', 'UInt16');
  CTypeToPas.Add('uint32', 'UInt32');
  CTypeToPas.Add('uint64', 'UInt64');
  CTypeToPas.Add('float', 'Single');
  CTypeToPas.Add('bool', 'LongBool');
  CTypeToPas.Add('double', 'Double');
  CTypeToPas.Add('const char *', 'PChar');

  // Types needed in TypeToPas
  CTypeToPas.Add('float [50]', 'TArraySingle50');
  CTypeToPas.Add('uint16 [8]', 'TArrayWord8');
  CTypeToPas.Add('uint32 [50]', 'TArrayDWord50');
  CTypeToPas.Add('uint8 [20]', 'TArrayByte8');
  CTypeToPas.Add('uint8 [2560]', 'TArrayByte2560');

  CTypeToPas.Add('char [8000]', 'TAnsiChar8000');
  CTypeToPas.Add('char [2048]', 'TAnsiChar2048');
  CTypeToPas.Add('char [1025]', 'TAnsiChar1025');
  CTypeToPas.Add('char [1024]', 'TAnsiChar1024');
  CTypeToPas.Add('char [512]',  'TAnsiChar512');
  CTypeToPas.Add('char [260]',  'TAnsiChar260');
  CTypeToPas.Add('char [256]',  'TAnsiChar256');
  CTypeToPas.Add('char [240]',  'TAnsiChar240');
  CTypeToPas.Add('char [129]',  'TAnsiChar129');
  CTypeToPas.Add('char [128]',  'TAnsiChar128');
  CTypeToPas.Add('char [64]',   'TAnsiChar64');
  CTypeToPas.Add('char [32]',   'TAnsiChar32');
  CTypeToPas.Add('char [4]',    'TAnsiChar4');
end;

function DecodeParams(const Params: TParams): String;
begin
  Result := '';
end;

procedure TApiWriterForm.ShowCode;
var
  I, M, V: Integer;
  ATypes: TTypes;
  S: String;
begin
  // TypeToPas
  if TypeToPas.Count > 0 then
    begin
      Memo1.Lines.Add(Format('  { TypeToPas (%d) }',[TypeToPas.Count]));
      Memo1.Lines.Add('');
      Memo1.Lines.Add('type');
      for S in TypeToPas.Keys do
        Memo1.Lines.Add(Format('  %s: %s;',[S, TypeToPas[S]]));
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  { == End TypeToPas ========================= }');
      Memo1.Lines.Add('');
    end;

  // TypeDefList
  if TypeDefList.Count > 0 then
    begin
      Memo1.Lines.Add(Format('  { TypeDefList (%d) }',[TypeDefList.Count]));
      for I := 0 to TypeDefList.Count - 1 do
        begin
          Memo1.Lines.Add(Format('    %s = %s',[TypeDefList[I].typedef, TypeDefList[I].typedeftype]));
        end;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  { == End TypeDefList ======================= }');
      Memo1.Lines.Add('');
    end;

  // CallbackList
  if CallbackList.Count > 0 then
    begin
      Memo1.Lines.Add(Format('  { CallbackList (%d) }',[CallbackList.Count]));
      for I := 0 to CallbackList.Count - 1 do
        begin
          ATypes := MangleTypes(CallbackList[I].struct);
          Memo1.Lines.Add(Format('  %s = record',[ATypes.AClassName]));
          Memo1.Lines.Add('  const');
          Memo1.Lines.Add(Format('    k_iCallback = %d;',[CallbackList[I].callback_id]));
          if Length(CallbackList[I].fields) > 0 then
            begin
              Memo1.Lines.Add('  var');
              for V := 0 to Length(CallbackList[I].fields) - 1 do
                Memo1.Lines.Add(Format('    %s: %s;',[CallbackList[I].fields[V].fieldname, ConvertType(CallbackList[I].fields[V].fieldtype)]));
            end;
          Memo1.Lines.Add('  end;');
          Memo1.Lines.Add(Format('  %s = ^%s;',[ATypes.APointerName, ATypes.AClassName]));
          Memo1.Lines.Add('');
        end;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  { == End CallbackList ====================== }');
      Memo1.Lines.Add('');
    end;

  // InterfaceList
  if InterfaceList.Count > 0 then
    begin
      Memo1.Lines.Add(Format('  { InterfaceList (%d) }',[InterfaceList.Count]));
      for I := 0 to InterfaceList.Count - 1 do
        begin
          Memo1.Lines.Add(Format('  // %s',[InterfaceList[I].classname]));
          for M := 0 to InterfaceList[I].methods.Count - 1 do
            if InterfaceList[I].methods[M].returntype = 'void' then
              Memo1.Lines.Add(Format('  %s: procedure(%s: Pointer%s); CDecl;', [InterfaceList[I].methods[M].methodname_flat, InterfaceList[I].cmethod, DecodeParams(InterfaceList[I].methods[M].params)]))
            else
              Memo1.Lines.Add(Format('  %s: function(%s: Pointer%s); %s; CDecl;', [InterfaceList[I].methods[M].methodname_flat, InterfaceList[I].cmethod, DecodeParams(InterfaceList[I].methods[M].params), ConvertType(InterfaceList[I].methods[M].returntype)]));
          Memo1.Lines.Add('');
        end;
      Memo1.Lines.Add('');
      Memo1.Lines.Add('  { == End InterfaceList ===================== }');
      Memo1.Lines.Add('');
    end;

  // ENumList
  if EnumList.Count > 0 then
    begin
      Memo1.Lines.Add(Format('  { ENumList (%d) }',[ENumList.Count]));
      for I := 0 to EnumList.Count - 1 do
        begin
          Memo1.Lines.Add(Format('%s',[EnumList[I].enumname]));
          for V := 0 to Length(EnumList[I].values) - 1 do
            Memo1.Lines.Add(Format('    %s = %s',[EnumList[I].values[V].name, EnumList[I].values[V].value]));
          Memo1.Lines.Add('');
        end;
      Memo1.Lines.Add('  { == End EnumList ========================== }');
    end;

  // NeedType
  if NeedType.Count > 0 then
    begin
      Memo1.Lines.Add(Format('  { NeedTypes (%d) }',[NeedType.Count]));
      for I := 0 to NeedType.Count - 1 do
        Memo1.Lines.Add(Format('  %s',[NeedType[I]]));
      Memo1.Lines.Add('  { ======================================= }');
    end;
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

procedure TShortEnum.BuildValues(Ary: TJSONArray);
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

destructor TShortEnum.Destroy;
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

procedure TApiWriterForm.HandleArray(AList: TEnumList; Value: TJSONArray);
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

procedure TApiWriterForm.HandleArrayItem(AItem: TEnumList; Value: TJSONValue);
var
  Pair: TJSONPair;
  I: Integer;
  AKey: String;
  O: TShortEnum;
begin
  if Value is TJSONObject then
    begin
      O := TShortEnum.Create;
      try
        for I := 0 to TJSONObject(Value).Count - 1 do
          begin
            Pair := TJSONObject(Value).Pairs[I];
            AKey := Pair.JsonString.Value;
//            Memo1.Lines.Add(Format('Item : %s',[AKey]));
            if AKey = 'enumname' then
              begin
                if Pair.JsonValue is TJSONString then
                  O.enumname := Pair.JsonValue.AsType<String>
                else
                  Raise Exception.CreateFmt('EnumList enumname %s is non-string', [AKey]);
              end
            else if AKey = 'values' then
              begin
                if Pair.JsonValue is TJSONArray then
                  O.BuildValues(TJSONArray(Pair.JsonValue))
                else
                  Raise Exception.CreateFmt('EnumList value %s is non-array', [AKey]);
              end
            else
              Raise Exception.CreateFmt('EnumList %s is unhandled', [AKey]);
        end
      finally
        AItem.Add(O);
      end;
    end
  else
    Raise Exception.Create('Error in EnumList');
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
//            Memo1.Lines.Add(Format('Item : %s',[AKey]));
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
                  O.cmethod := Pair.JsonValue.AsType<String>
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
//  SetLength(methods, Ary.Count); - now TOcjectList
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
          methods.Add(F);
        end
      else
        Raise Exception.CreateFmt('InterfaceList - enum %d', [I]);

    end;

  methods.Sort(TComparer<TMethod>.Construct(
    function (const L, R: TMethod): integer
    begin
        Result := CompareText(L.methodname, R.methodname);
    end
    ));
end;

constructor TInterface.Create;
begin
  methods := TObjectList<TMethod>.Create;
end;

destructor TInterface.Destroy;
var
  I: Integer;
begin
  for I := Length(enums) - 1 downto 0 do
    FreeAndNil(enums[I]);
  for I := Length(accessors) - 1 downto 0 do
    FreeAndNil(accessors[I]);
  for I := methods.Count - 1 downto 0 do
    FreeAndNil(methods[I]);
  FreeAndNil(methods);
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
