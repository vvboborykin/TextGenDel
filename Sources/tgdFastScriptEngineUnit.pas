{*******************************************************
* Project: TextGenDelTest
* Unit: tgdFastScriptEngineUnit.pas
* Description: Implimentation of Script Engine Interface Based On FastScript library
* 
* Created: 08.02.2024 10:50:19
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdFastScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdScriptEngineUnit,
  tgdReportUnit, fs_iinterpreter, fs_ipascal, fs_iclassesrtti, fs_idbrtti,
  fs_iformsrtti, fs_iinirtti;

type
  /// <summary>TtgdFastScriptEngine
  /// Implimentation of Script Engine Interface Based On FastScript library
  /// </summary>
  TtgdFastScriptEngine = class(TInterfacedObject, ItgdScriptEngine)
  private
    FNestCount: Integer;
    FReport: TtgdReport;
    FResultLines: TStrings;
    FScript: TfsScript;
    procedure AddChildScriptElement(AItem: TfsItemList; AChildren:
        TtgdScriptElementList);
    procedure AddComponentRegistration(vComponent: TComponent);
    procedure AddScriptCodeLine(ALine: string; AScript: TStrings);
    procedure AddScriptMacroLine(ALine: string; AScript: TStrings);
    function CallAddLine(Instance: TObject; ClassType: TClass; const MethodName:
      string; var Params: Variant): Variant;
    procedure CheckInitCompleted;
    procedure ConvertTemplateToScript(AScript: TStrings; ATemplateLines: TStrings =
        nil); stdcall;
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
    procedure GenerateScriptLine(ALine: string; AScript: TStrings);
    procedure GetChildrenScriptElements(AParent: TObject; AChildren:
        TtgdScriptElementList); stdcall;
    procedure Init(AReport: TtgdReport); stdcall;
    procedure LoadRootElements(AChildren: TtgdScriptElementList);
    procedure LoadSubElements(AParent: TObject; AChildren: TtgdScriptElementList);
    procedure RaiseCompileError;
    procedure RegisterComponentClass(AClass: TClass);
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
  tgdCollectionsUnit, Forms;

resourcestring
  SAReportIsNil = 'AReport is nil';
  SInitMetodNotExecuted = 'Init metod not executed';
  SCompilationErrorAt = 'Compilation error "%s" at %s';

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

procedure TtgdFastScriptEngine.AddChildScriptElement(AItem: TfsItemList;
    AChildren: TtgdScriptElementList);
begin
  // TODO -cMM: TtgdFastScriptEngine.AddChildScriptElement default body inserted
end;

procedure TtgdFastScriptEngine.AddComponentRegistration(vComponent: TComponent);
begin
  RegisterComponentClass(vComponent.ClassType);
  FScript.AddComponent(vComponent);
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
  vText := '''' + vText + '''';

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

procedure TtgdFastScriptEngine.CheckInitCompleted;
begin
  if FReport = nil then
    raise Exception.Create(SInitMetodNotExecuted);
end;

procedure TtgdFastScriptEngine.ConvertTemplateToScript(AScript: TStrings;
    ATemplateLines: TStrings = nil);
var
  I: Integer;
begin
  CheckInitCompleted();
  FNestCount := 0;
  
  if ATemplateLines = nil then
    ATemplateLines := FReport.TemplateLines;

  for I := 0 to ATemplateLines.Count - 1 do
    GenerateScriptLine(ATemplateLines[I], AScript);

  if (AScript.Count = 0) or (Trim(AScript[AScript.Count - 1]) <> 'end.') then
  begin
    AScript.Insert(0, 'begin');
    AScript.Append('end.');
  end;
end;

procedure TtgdFastScriptEngine.ExecuteScript(AScript, AResultLines: TStrings);
begin
  CheckInitCompleted;

  FResultLines := AResultLines;
  FScript.Lines.Assign(AScript);

  if FScript.Compile() then
  begin
    FScript.Execute();
    FResultLines := nil;
  end
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

procedure TtgdFastScriptEngine.GetChildrenScriptElements(AParent: TObject;
    AChildren: TtgdScriptElementList);
begin
  if AParent = nil then
    LoadRootElements(AChildren)
  else
    LoadSubElements(AParent, AChildren);
end;

procedure TtgdFastScriptEngine.Init(AReport: TtgdReport);
begin
  if AReport = nil then
    raise Exception.Create(SAReportIsNil);

  FReport := AReport;
  RegisterContext();
  RegisterVariables();
  RegisterFunctions();
end;

procedure TtgdFastScriptEngine.LoadRootElements(AChildren:
    TtgdScriptElementList);
var
  I: Integer;
  vItem: TfsItemList;
begin
  for I := 0 to FScript.Count-1 do
  begin
    vItem := FScript.Items[I];
    AddChildScriptElement(vItem, AChildren);
  end;
end;

procedure TtgdFastScriptEngine.LoadSubElements(AParent: TObject; AChildren:
    TtgdScriptElementList);
var
  I: Integer;
  vClass: TfsClassVariable;
  vCustVar: TfsCustomVariable;
  vParentItem: TfsItemList;
begin
  if AParent <> nil then
  begin
    vParentItem := TfsItemList(AParent);
    vClass := nil;

    if vParentItem is TfsClassVariable then
      vClass := (vParentItem as TfsClassVariable)
    else
    if vParentItem is TfsCustomVariable then
    begin
      vCustVar := vParentItem as TfsCustomVariable;
      if (vCustVar.RefItem <> nil) and (vCustVar.RefItem is TfsClassVariable) then
        vClass := (vCustVar.RefItem as TfsClassVariable)
      else;
    end;

    if vClass <> nil then
    begin
      for I := 0 to vClass.Count-1 do
        AddChildScriptElement(vClass.Members[I], AChildren);
    end;
  end;
end;

procedure TtgdFastScriptEngine.RaiseCompileError;
begin
  raise Exception.CreateFmt(SCompilationErrorAt, [FScript.ErrorMsg, FScript.ErrorPos]);
end;

procedure TtgdFastScriptEngine.RegisterComponentClass(AClass: TClass);
var
  vFsClass: TfsClassVariable;
begin
  vFsClass := FScript.FindClass(AClass.ClassName);
  if vFsClass = nil then
  begin
    if (AClass.ClassParent <> nil) then
      RegisterComponentClass(AClass.ClassParent);
  end;
  FScript.AddClass(AClass, AClass.ClassParent.ClassName);
end;

procedure TtgdFastScriptEngine.RegisterContext;
var
  I: Integer;
  vComponent: TComponent;
  vItem: TtgdReportContextItem;
begin
  for I := 0 to FReport.Context.Count - 1 do
  begin
    vItem := FReport.Context.Items[I];
    FScript.AddForm(vItem.Component);
  end;

  if FReport.UseOwnerAsContext and (FReport.Owner <> nil) then
  begin
    for I := 0 to FReport.Owner.ComponentCount - 1 do
    begin
      vComponent := FReport.Owner.Components[I];
      if (vComponent is TFrame) or (vComponent is TForm)
        or (vComponent is TDataModule) then
          FScript.AddForm(vComponent)
      else
        AddComponentRegistration(vComponent);
    end;
  end;
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
  CheckInitCompleted;

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

finalization
  CreateScriptEngine := nil;

end.

