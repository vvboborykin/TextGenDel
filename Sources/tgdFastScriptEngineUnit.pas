{*******************************************************
* Project: TextGenDelTest
* Unit: tgdFastScriptEngineUnit.pas
* Description: Implimentation of Script Engine Interface Based On FastScript library
*
* Created: 08.02.2024 10:50:19
* Copyright (C) 2024 ��������� �.�. (bpost@yandex.ru)
*******************************************************}
unit tgdFastScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdBaseScriptEngineUnit,
  tgdScriptElementsUnit, tgdReportUnit, fs_iinterpreter, fs_ipascal,
  fs_iclassesrtti;

type
  /// <summary>TtgdFastScriptEngine
  /// Implimentation of Script Engine Interface Based On FastScript library
  /// </summary>
  TtgdFastScriptEngine = class(TtgdBaseScriptEngine, ItgdScriptEngine)
  private
    FResultLines: TStrings;
    FScript: TfsScript;
    procedure AddComponentRegistration(vComponent: TComponent);
    procedure AddScriptElementToList(AItem: TfsItemList; AChildren:
      TtgdScriptElementList);
    function CallAddLine(Instance: TObject; ClassType: TClass; const MethodName:
      string; var Params: Variant): Variant;
    procedure FindRootItemByName(AName: string; var vParentItem: TfsItemList);
    procedure FindSubItemByName(AName: string; var vParentItem: TfsItemList);
    procedure GetSubItemByTokenName(AName: string; var vParentItem: TfsItemList);
    procedure GetSubitemsStartedWithName(AItem: TfsItemList; ANameStart: string;
      AItems, AInserts: TStrings);
    procedure LoadRootElements(AChildren: TtgdScriptElementList);
    procedure LoadSubElements(AParent: TObject; AChildren: TtgdScriptElementList);
    procedure RaiseCompileError;
    procedure RegisterComponentClass(AClass: TClass);
    procedure RegisterContext;
    procedure RegisterFunctions;
    procedure RegisterVariables;
  protected
    /// <summary>ItgdScriptEngine.GetCompletionItems
    /// Get autocompletion itetms for text
    /// </summary>
    /// <param name="AText"> (string) </param>
    /// <param name="AItems"> (TStrings) </param>
    /// <param name="AInserts"> (TStrings) </param>
    procedure GetCompletionItems(AText: string; AItems, AInserts: TStrings); stdcall;
    /// <summary>ItgdScriptEngine.GetChildrenScriptElements
    /// Get script children elements
    /// </summary>
    /// <param name="AParent"> (TObject) </param>
    /// <param name="AChildren"> (TObjectList) </param>
    procedure GetChildrenScriptElements(AParent: TObject; AChildren:
      TtgdScriptElementList); stdcall;
    /// <summary>ItgdScriptEngine.ValidateScript
    /// Validate script
    /// </summary>
    /// <param name="AScript"> (TStrings) Script lines</param>
    procedure ValidateScript(AScript: TStrings); stdcall;
    /// <summary>ItgdScriptEngine.Init
    /// Initialize script engine before use
    /// </summary>
    /// <param name="AReport"> (TtgdReport) Report context</param>
    procedure Init(AReport: TtgdReport); override; stdcall;
    /// <summary>ItgdScriptEngine.ExecuteScript
    /// Execute script (generate text)
    /// </summary>
    /// <param name="AScript"> (TStrings) Script source to generate text</param>
    /// <param name="AResultLines"> (TStrings) Result text lines recipient</param>
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
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
  FScript := TfsScript.Create(fsGlobalUnit);
  FScript.Parent := fsGlobalUnit;
end;

destructor TtgdFastScriptEngine.Destroy;
begin
  FreeAndNil(FScript);
  inherited Destroy;
end;

procedure TtgdFastScriptEngine.AddComponentRegistration(vComponent: TComponent);
begin
  RegisterComponentClass(vComponent.ClassType);
  FScript.AddComponent(vComponent);
end;

procedure TtgdFastScriptEngine.AddScriptElementToList(AItem: TfsItemList;
  AChildren: TtgdScriptElementList);
var
  vClassName: string;
  vCustVar: TfsCustomVariable;
  vFsClass: TfsClassVariable;
  vHasChildren: Boolean;
  vName: string;
  vScriptItem: TtgdScriptElement;
begin
  vScriptItem := nil;

  vClassName := AItem.ClassName;
  if vClassName <> '' then
    vClassName := vClassName;

  if AItem is TfsCustomVariable then
  begin
    vCustVar := (AItem as TfsCustomVariable);

    vName := vCustVar.Name;
    if vName = '' then
      vName := vCustVar.TypeName;

    vFsClass := FScript.FindClass(vCustVar.GetFullTypeName);
    vHasChildren := vFsClass <> nil;

    if AItem is TfsClassVariable then
      vScriptItem := TtgdScriptClass.Create(AItem, vName, vCustVar.Count > 0)
    else if AItem is TfsTypeVariable then
      vScriptItem := TtgdScriptType.Create(AItem, vName, vCustVar.Count > 0)
    else if AItem is TfsVariable then
      vScriptItem := TtgdScriptVariable.Create(AItem, vName, vHasChildren)
    else if AItem is TfsEventHelper then
      vScriptItem := TtgdScriptEvent.Create(AItem, vName, vHasChildren)
    else if AItem is TfsPropertyHelper then
      vScriptItem := TtgdScriptProperty.Create(AItem, vName, vHasChildren)
    else if AItem is TfsMethodHelper then
    begin
      if (AItem as TfsMethodHelper).ParentRef <> nil then
        vScriptItem := TtgdScriptMethod.Create(AItem, vName, vHasChildren)
      else
        vScriptItem := TtgdScriptFunction.Create(AItem, vName, vHasChildren);
    end
  end;

  if (vScriptItem <> nil) then
  begin
    if AChildren.IsClassAllowed(vScriptItem.ClassType) and not AChildren.ContainsScriptElement
      (vScriptItem) then
      AChildren.Add(vScriptItem)
    else
      vScriptItem.Free;
  end;
end;

function TtgdFastScriptEngine.CallAddLine(Instance: TObject; ClassType: TClass;
  const MethodName: string; var Params: Variant): Variant;
begin
  Result := FResultLines.Add(VarToStr(Params[0]));
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

procedure TtgdFastScriptEngine.FindRootItemByName(AName: string; var vParentItem:
  TfsItemList);
var
  I: Integer;
  vCustomVar: TfsCustomVariable;
begin
  for I := 0 to FScript.Count - 1 do
  begin
    if FScript.Items[I] is TfsCustomVariable then
    begin
      vCustomVar := FScript.Items[I] as TfsCustomVariable;
      if AnsiSameText(AName, vCustomVar.Name) then
      begin
        vParentItem := vCustomVar;
        Break;
      end;
    end;
  end;
end;

procedure TtgdFastScriptEngine.FindSubItemByName(AName: string; var vParentItem:
  TfsItemList);
var
  I: Integer;
  vChildren: TtgdScriptElementList;
  vClass: TfsClassVariable;
begin
  vClass := nil;
  if vParentItem is TfsClassVariable then
    vClass := vParentItem as TfsClassVariable
  else if vParentItem is TfsCustomVariable then
    vClass := FScript.FindClass((vParentItem as TfsCustomVariable).TypeName);

  if vClass <> nil then
  begin
    vChildren := TtgdScriptElementList.Create(True);
    try
      GetChildrenScriptElements(vClass, vChildren);
      for I := 0 to vChildren.Count - 1 do
      begin
        if vChildren[I].SourceObject is TfsCustomVariable then
        begin
          if AnsiSameText((vChildren[I].SourceObject as TfsCustomVariable).Name,
            AName) then
          begin
            vParentItem := vChildren[I].SourceObject as TfsItemList;
            Break;
          end;
        end;
      end;
    finally
      vChildren.Free;
    end;
  end;
end;

procedure TtgdFastScriptEngine.GetChildrenScriptElements(AParent: TObject;
  AChildren: TtgdScriptElementList);
begin
  if AParent = nil then
    LoadRootElements(AChildren)
  else
    LoadSubElements(AParent, AChildren);
end;

procedure TtgdFastScriptEngine.GetCompletionItems(AText: string; AItems,
  AInserts: TStrings);
var
  vName: string;
  vParentItem: TfsItemList;
  vTokens: TStrings;
begin
  AItems.Clear;
  AInserts.Clear;
  vTokens := TStringList.Create;
  try
    vTokens.Delimiter := '.';
    vTokens.DelimitedText := AText;
    vParentItem := nil;
    while vTokens.Count > 1 do
    begin
      if Trim(vTokens[0]) <> '' then
        GetSubItemByTokenName(vTokens[0], vParentItem);
      vTokens.Delete(0);
    end;

    vName := '';

    if vTokens.Count > 0 then
      vName := vTokens[vTokens.Count - 1];

    GetSubitemsStartedWithName(vParentItem, vName, AItems, AInserts);
  finally
    vTokens.Free;
  end;
end;

procedure TtgdFastScriptEngine.GetSubItemByTokenName(AName: string; var
  vParentItem: TfsItemList);
begin
  if vParentItem = nil then
    FindRootItemByName(AName, vParentItem)
  else
    FindSubItemByName(AName, vParentItem);
end;

procedure TtgdFastScriptEngine.GetSubitemsStartedWithName(AItem: TfsItemList;
  ANameStart: string; AItems, AInserts: TStrings);
var
  I: Integer;
  vChildren: TtgdScriptElementList;
  vCustVar: TfsCustomVariable;
  vObject: TObject;
begin
  vChildren := TtgdScriptElementList.Create(True);
  try
    GetChildrenScriptElements(AItem, vChildren);
    for I := 0 to vChildren.Count - 1 do
    begin
      vObject := vChildren[I].SourceObject;
      if vObject is TfsCustomVariable then
      begin
        vCustVar := (vObject as TfsCustomVariable);
        if AnsiStartsText(ANameStart, vCustVar.Name) then
        begin
          AItems.AddObject(vCustVar.Name, TObject(vChildren[I].ClassType));
          AInserts.AddObject(vCustVar.Name, TObject(vChildren[I].ClassType));
        end;
      end;
    end;
  finally
    vChildren.Free;
  end;
end;

procedure TtgdFastScriptEngine.Init(AReport: TtgdReport);
begin
  inherited Init(AReport);
  RegisterContext();
  RegisterVariables();
  RegisterFunctions();
end;

procedure TtgdFastScriptEngine.LoadRootElements(AChildren: TtgdScriptElementList);
var
  I: Integer;
  vItem: TfsItemList;
begin
  for I := 0 to FScript.Count - 1 do
  begin
    vItem := FScript.Items[I];
    AddScriptElementToList(vItem, AChildren);
  end;
end;

procedure TtgdFastScriptEngine.LoadSubElements(AParent: TObject; AChildren:
  TtgdScriptElementList);
var
  I: Integer;
  vClass, vParentClass: TfsClassVariable;
  vClassName: string;
  vCustVar: TfsCustomVariable;
  vParentItem: TfsItemList;
begin
  if AParent <> nil then
  begin
    vParentItem := AParent as TfsItemList;

    vClassName := vParentItem.ClassName;
    if vClassName <> '' then
      vClassName := vClassName;

    vClass := nil;

    if vParentItem is TfsClassVariable then
      vClass := (vParentItem as TfsClassVariable)
    else if vParentItem is TfsCustomVariable then
    begin
      vCustVar := vParentItem as TfsCustomVariable;
      vClass := FScript.FindClass(vCustVar.TypeName);
    end;

    if vClass <> nil then
    begin
      vParentClass := FScript.FindClass(vClass.TypeName);
      repeat
        if vParentClass <> nil then
        begin
          for I := 0 to vParentClass.MembersCount - 1 do
            AddScriptElementToList(vParentClass.Members[I], AChildren);
          vParentClass := FScript.FindClass(vParentClass.Ancestor);
        end;
      until vParentClass = nil;
      for I := 0 to vClass.MembersCount - 1 do
        AddScriptElementToList(vClass.Members[I], AChildren);
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

  function FindParentClass(ACurrent: TClass): TfsClassVariable;
  begin
    Result := FScript.FindClass(ACurrent.ClassName);
    if Result = nil then
      Result := FindParentClass(ACurrent.ClassParent);
  end;

begin
  vFsClass := FScript.FindClass(AClass.ClassName);
  if vFsClass = nil then
  begin
    vFsClass := FindParentClass(AClass);
    FScript.AddClass(AClass, vFsClass.TypeName);
  end;
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
      if (vComponent is TFrame) or (vComponent is TForm) or (vComponent is
        TDataModule) then
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

end.

