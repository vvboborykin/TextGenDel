unit tgdFastScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdScriptEngineUnit,
  tgdReportUnit, fs_iinterpreter;

type
  TtgdFastScriptEngine = class(TInterfacedObject, ItgdScriptEngine)
  private
    FReport: TtgdReport;
    FScript: TfsScript;
    procedure ConvertTemplateToScript(AScript: TStrings); stdcall;
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
    procedure Init(AReport: TtgdReport); stdcall;
    procedure RegisterContext;
    procedure RegisterFunctions;
    procedure RegisterVariables;
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

procedure TtgdFastScriptEngine.ConvertTemplateToScript(AScript: TStrings);
begin
  // TODO -cMM: TtgdFastScriptEngine.ConvertTemplateToScript default body inserted
end;

procedure TtgdFastScriptEngine.ExecuteScript(AScript, AResultLines: TStrings);
begin
  // TODO -cMM: TtgdFastScriptEngine.ExecuteScript default body inserted
end;

procedure TtgdFastScriptEngine.Init(AReport: TtgdReport);
begin
  FReport := AReport;
  RegisterContext();
  RegisterVariables();
  RegisterFunctions();
end;

procedure TtgdFastScriptEngine.RegisterContext;
var
  I: Integer;
  vItem: TtgdReportContextItem;
begin
  for I := 0 to FReport.Context.Count-1 do
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
  for I := 0 to FReport.Functions.Count-1 do
  begin
    vItem := FReport.Functions.Items[I];
    FScript.AddMethod(vItem.Declaration, FReport.DoCallMethod, 'TextGen');
  end;
end;

procedure TtgdFastScriptEngine.RegisterVariables;
var
  I: Integer;
  vItem: TtgdReportVariable;
begin
  for I := 0 to FReport.Variables.Count-1 do
  begin
    vItem := FReport.Variables.Items[I];
    FScript.AddVariable(vItem.Name, 'Variant', vItem.Value);
  end;
end;

procedure TtgdFastScriptEngine.ValidateScript(AScript: TStrings);
begin
  FScript.Lines.Assign(AScript);
  if not FScript.Compile then
    raise Exception.CreateFmt('Compilation error %s at %s',
      [FScript.ErrorMsg, FScript.ErrorPos]);
end;

function CreateFastScriptEngine(): ItgdScriptEngine;
begin
  Result := TtgdFastScriptEngine.Create;
end;

initialization
  CreateScriptEngine := CreateFastScriptEngine;

end.
