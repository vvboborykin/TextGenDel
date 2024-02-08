unit tgdFastScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdScriptEngineUnit,
  tgdReportUnit, fs_iinterpreter, fs_ipascal, fs_iclassesrtti, fs_idbrtti,
  fs_iformsrtti, fs_iinirtti;

type
  TtgdFastScriptEngine = class(TInterfacedObject, ItgdScriptEngine)
  private
    FNestCount: Integer;
    FReport: TtgdReport;
    FResultLines: TStrings;
    FScript: TfsScript;
    procedure AddScriptCodeLine(ALine: string; AScript: TStrings);
    procedure AddScriptMacroLine(ALine: string; AScript: TStrings);
    function CallAddLine(Instance: TObject; ClassType: TClass; const MethodName:
      string; var Params: Variant): Variant;
    procedure ConvertTemplateToScript(AScript: TStrings); stdcall;
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
    procedure GenerateScriptLine(ALine: string; AScript: TStrings);
    procedure Init(AReport: TtgdReport); stdcall;
    procedure RaiseCompileError;
    procedure RegisterContext;
    procedure RegisterFunctions;
    procedure RegisterVariables;
    function ReplaceMacroses(ALine: string): string;
    procedure ValidateScript(AScript: TStrings); stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  tgdCollectionsUnit;

constructor TtgdFastScriptEngine.Create;
begin
  inherited Create;
  FScript := TfsScript.Create(nil);
  FScript.Parent := fsGlobalUnit;
end;

destructor TtgdFastScriptEngine.Destroy;
begin
  FreeAndNil(FScript);
  inherited Destroy;
end;

procedure TtgdFastScriptEngine.AddScriptCodeLine(ALine: string; AScript: TStrings);
var
  vText: string;
begin
  vText := StringReplace(ALine, FReport.CodeBeginMarker, '', [rfReplaceAll]);
  vText := StringReplace(vText, FReport.CodeEndMarker, '', [rfReplaceAll]);
  AScript.Add(vText);
end;

procedure TtgdFastScriptEngine.AddScriptMacroLine(ALine: string; AScript: TStrings);
var
  vText: string;
begin
  vText := ReplaceMacroses(ALine);
  vText := AnsiQuotedStr(vText, '''');
  if AnsiStartsText(''' + ', vText) then
    vText := MidStr(vText, 5, MaxInt);
  if AnsiEndsText(' + ''', vText) then
    vText := LeftStr(vText, Length(vText) - 4);

  AScript.Add(FReport.AddLineFunctionName + '(' + vText + ');');
end;

function TtgdFastScriptEngine.CallAddLine(Instance: TObject; ClassType: TClass;
  const MethodName: string; var Params: Variant): Variant;
begin
  Result := FResultLines.Add(VarToStr(Params[0]));
end;

procedure TtgdFastScriptEngine.ConvertTemplateToScript(AScript: TStrings);
var
  I: Integer;
begin
  FNestCount := 0;
  for I := 0 to AScript.Count - 1 do
    GenerateScriptLine(AScript[I], AScript);
end;

procedure TtgdFastScriptEngine.ExecuteScript(AScript, AResultLines: TStrings);
begin
  FResultLines := AResultLines;
  FScript.Lines.Assign(AScript);
  if FScript.Compile() then
    FScript.Execute()
  else
  begin
    FResultLines := nil;
    RaiseCompileError();
  end;
end;

procedure TtgdFastScriptEngine.GenerateScriptLine(ALine: string; AScript: TStrings);
begin
  if AnsiStartsText(FReport.CodeBeginMarker, Trim(ALine)) then
    Inc(FNestCount);

  if FNestCount > 0 then
    AddScriptCodeLine(ALine, AScript)
  else
    AddScriptMacroLine(ALine, AScript);

  if AnsiEndsText(FReport.CodeEndMarker, Trim(ALine)) then
    Dec(FNestCount);
end;

procedure TtgdFastScriptEngine.Init(AReport: TtgdReport);
begin
  FReport := AReport;
  RegisterContext();
  RegisterVariables();
  RegisterFunctions();
end;

procedure TtgdFastScriptEngine.RaiseCompileError;
begin
  raise Exception.CreateFmt('Compilation error %s at %s', [FScript.ErrorMsg,
    FScript.ErrorPos]);
end;

procedure TtgdFastScriptEngine.RegisterContext;
var
  I: Integer;
  vItem: TtgdReportContextItem;
begin
  for I := 0 to FReport.Context.Count - 1 do
  begin
    vItem := FReport.Context.Items[I];
    FScript.AddForm(vItem.Component);
  end;

  if FReport.UseOwnerAsContext then
    FScript.AddForm(FReport.Owner);
end;

procedure TtgdFastScriptEngine.RegisterFunctions;
var
  I: Integer;
  vItem: TtgdReportFunction;
begin
  for I := 0 to FReport.Functions.Count - 1 do
  begin
    vItem := FReport.Functions.Items[I];
    FScript.AddMethod(vItem.Declaration, FReport.DoCallMethod, 'TextGen');
  end;

  FScript.AddMethod('function ' + FReport.AddLineFunctionName +
    '(ALine: String): Integer;', CallAddLine);
end;

procedure TtgdFastScriptEngine.RegisterVariables;
var
  I: Integer;
  vItem: TtgdReportVariable;
begin
  for I := 0 to FReport.Variables.Count - 1 do
  begin
    vItem := FReport.Variables.Items[I];
    FScript.AddVariable(vItem.Name, 'Variant', vItem.Value);
  end;
end;

function TtgdFastScriptEngine.ReplaceMacroses(ALine: string): string;
begin
  Result := ALine;
  Result := StringReplace(Result, FReport.MacroBeginMarker, ''' + ', [rfReplaceAll]);
  Result := StringReplace(Result, FReport.MacroEndMarker, ' + ''', [rfReplaceAll]);
end;

procedure TtgdFastScriptEngine.ValidateScript(AScript: TStrings);
begin
  FScript.Lines.Assign(AScript);
  if not FScript.Compile then
    RaiseCompileError;
end;

function CreateFastScriptEngine(): ItgdScriptEngine;
begin
  Result := TtgdFastScriptEngine.Create;
end;

initialization
  CreateScriptEngine := CreateFastScriptEngine;

end.

