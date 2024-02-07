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
begin
  // TODO -cMM: TtgdFastScriptEngine.RegisterContext default body inserted
end;

procedure TtgdFastScriptEngine.RegisterFunctions;
begin
  // TODO -cMM: TtgdFastScriptEngine.RegisterFunctions default body inserted
end;

procedure TtgdFastScriptEngine.RegisterVariables;
begin
  // TODO -cMM: TtgdFastScriptEngine.RegisterVariables default body inserted
end;

procedure TtgdFastScriptEngine.ValidateScript(AScript: TStrings);
begin
  // TODO -cMM: TtgdFastScriptEngine.ValidateScript default body inserted
end;

end.
