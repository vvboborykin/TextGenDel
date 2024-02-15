{*******************************************************
* Project: TextGenDelTest
* Unit: tgdReportUnit.pas
* Description: Template generator component
* 
* Created: 08.02.2024 10:43:28
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdReportUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdCollectionsUnit,
  fs_iinterpreter;

type
  /// <summary>TtgdReport
  /// Template generator component
  /// </summary>
  TtgdReport = class(TComponent)
  private
    FAddLineFunctionName: String;
    FCodeBeginMarker: string;
    FCodeEndMarker: string;
    FContext: TtgdReportContext;
    FFunctions: TtgdReportFunctions;
    FMacroBeginMarker: string;
    FMacroEndMarker: string;
    FTemplateLines: TStringList;
    FUseOwnerAsContext: Boolean;
    FVariables: TtgdReportVariables;
  protected
    procedure SetAddLineFunctionName(const Value: String);
    procedure SetCodeBeginMarker(const Value: string);
    procedure SetCodeEndMarker(const Value: string);
    procedure SetContext(const Value: TtgdReportContext);
    procedure SetFunctions(const Value: TtgdReportFunctions);
    procedure SetMacroBeginMarker(const Value: string);
    procedure SetMacroEndMarker(const Value: string);
    procedure SetTemplateLines(const Value: TStringList);
    procedure SetUseOwnerAsContext(const Value: Boolean);
    procedure SetVariables(const Value: TtgdReportVariables);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GenerateText(AResultLines: TStrings);
    procedure GenerateScript(AResultLines: TStrings);
    procedure ValidateTemplate;
    procedure SetDefaultMarkers;
    function DoCallMethod(Instance: TObject; ClassType: TClass; const MethodName:
        String; var Params: Variant): Variant;
    procedure Assign(Source: TPersistent); override;
  published
    /// <summary>TtgdReport.AddLineFunctionName
    /// The Name Of Custom function for Script appending New Line in Generation results
    /// </summary>
    /// type:String
    property AddLineFunctionName: String read FAddLineFunctionName write
        SetAddLineFunctionName;
    /// <summary>TtgdReport.Context
    /// Collection of components to use as Script variables
    /// </summary>
    /// type:TtgdReportContext
    property Context: TtgdReportContext read FContext write SetContext;
    /// <summary>TtgdReport.TemplateLines
    /// Template Lines (text)
    /// </summary>
    /// type:TStringList
    property TemplateLines: TStringList read FTemplateLines write SetTemplateLines;
    /// <summary>TtgdReport.UseOwnerAsContext
    /// Flag of usage owner component in Script
    /// </summary>
    /// type:Boolean
    property UseOwnerAsContext: Boolean read FUseOwnerAsContext write
        SetUseOwnerAsContext;
    /// <summary>TtgdReport.MacroBeginMarker
    /// The Marker of macro Expression start
    /// </summary>
    /// type:string
    property MacroBeginMarker: string read FMacroBeginMarker write
        SetMacroBeginMarker;
    /// <summary>TtgdReport.MacroEndMarker
    /// The Marker of macro Expression end
    /// </summary>
    /// type:string
    property MacroEndMarker: string read FMacroEndMarker write SetMacroEndMarker;
    /// <summary>TtgdReport.CodeBeginMarker
    /// The Marker of code Block Start
    /// </summary>
    /// type:string
    property CodeBeginMarker: string read FCodeBeginMarker write SetCodeBeginMarker;
    /// <summary>TtgdReport.CodeEndMarker
    /// The Marker of Block End
    /// </summary>
    /// type:string
    property CodeEndMarker: string read FCodeEndMarker write SetCodeEndMarker;
    /// <summary>TtgdReport.Functions
    /// Collection of Custom Script function declarations
    /// </summary>
    /// type:TtgdReportFunctions
    property Functions: TtgdReportFunctions read FFunctions write SetFunctions;
    /// <summary>TtgdReport.Variables
    /// Script Custom varibles
    /// </summary>
    /// type:TtgdReportVariables
    property Variables: TtgdReportVariables read FVariables write SetVariables;
  end;

implementation

uses
  tgdGeneratorUnit;

const
  SAResultLinesIsNil = 'AResultLines is nil';

constructor TtgdReport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTemplateLines := TStringList.Create();
  FUseOwnerAsContext := True;
  FContext := TtgdReportContext.Create(Self);
  FVariables := TtgdReportVariables.Create(Self);
  FFunctions := TtgdReportFunctions.Create(Self);
  SetDefaultMarkers();
  FAddLineFunctionName := 'AddLine';
end;

destructor TtgdReport.Destroy;
begin
  FreeAndNil(FFunctions);
  FreeAndNil(FVariables);
  FreeAndNil(FContext);
  FreeAndNil(FTemplateLines);
  inherited Destroy;
end;

procedure TtgdReport.Assign(Source: TPersistent);
var
  vReport: TtgdReport;
begin
  if Source is TtgdReport then
  begin
    vReport := Source as TtgdReport;
    TemplateLines.Assign(vReport.TemplateLines);
    UseOwnerAsContext := vReport.UseOwnerAsContext;
    MacroBeginMarker := vReport.MacroBeginMarker;
    MacroEndMarker := vReport.MacroEndMarker;
    CodeBeginMarker := vReport.CodeBeginMarker;
    CodeEndMarker := vReport.CodeEndMarker;
    Functions.Assign(vReport.Functions);
    Variables.Assign(vReport.Variables);
    Context.Assign(vReport.Context);
  end
  else
    inherited Assign(Source);
end;

function TtgdReport.DoCallMethod(Instance: TObject; ClassType: TClass; const
    MethodName: String; var Params: Variant): Variant;
var
  vItem: TtgdNamedCollectionItem;
begin
  Result := Unassigned;
  if FFunctions.TryFindName(MethodName, vItem) then
    Result := TtgdReportFunction(vItem).DoExecute(MethodName, Params);
end;

procedure TtgdReport.GenerateScript(AResultLines: TStrings);
var
  vGenerator: TtgdGenerator;
begin
  if AResultLines = nil then
    raise Exception.Create(SAResultLinesIsNil);

  vGenerator := TtgdGenerator.Create(Self);
  try
    vGenerator.GenerateScript(AResultLines);
  finally
    vGenerator.Free;
  end;
end;

procedure TtgdReport.GenerateText(AResultLines: TStrings);
var
  vGenerator: TtgdGenerator;
begin
  if AResultLines = nil then
    raise Exception.Create(SAResultLinesIsNil);

  vGenerator := TtgdGenerator.Create(Self);
  try
    vGenerator.GenerateText(AResultLines);
  finally
    vGenerator.Free;
  end;
end;

procedure TtgdReport.SetAddLineFunctionName(const Value: String);
begin
  if FAddLineFunctionName <> Value then
  begin
    FAddLineFunctionName := Value;
  end;
end;

procedure TtgdReport.SetCodeBeginMarker(const Value: string);
begin
  if FCodeBeginMarker <> Value then
  begin
    FCodeBeginMarker := Value;
  end;
end;

procedure TtgdReport.SetCodeEndMarker(const Value: string);
begin
  if FCodeEndMarker <> Value then
  begin
    FCodeEndMarker := Value;
  end;
end;

procedure TtgdReport.SetContext(const Value: TtgdReportContext);
begin
  FContext.Assign(Value);
end;

procedure TtgdReport.SetDefaultMarkers;
begin
  FCodeBeginMarker := '{{';
  FCodeEndMarker := '}}';
  FMacroBeginMarker := '{=';
  FMacroEndMarker := '=}'
end;

procedure TtgdReport.SetFunctions(const Value: TtgdReportFunctions);
begin
  FFunctions.Assign(Value);
end;

procedure TtgdReport.SetMacroBeginMarker(const Value: string);
begin
  if FMacroBeginMarker <> Value then
  begin
    FMacroBeginMarker := Value;
  end;
end;

procedure TtgdReport.SetMacroEndMarker(const Value: string);
begin
  if FMacroEndMarker <> Value then
  begin
    FMacroEndMarker := Value;
  end;
end;

procedure TtgdReport.SetTemplateLines(const Value: TStringList);
begin
  if Value = nil then
    FTemplateLines.Clear
  else
    FTemplateLines.AddStrings(Value);
end;

procedure TtgdReport.SetUseOwnerAsContext(const Value: Boolean);
begin
  if FUseOwnerAsContext <> Value then
  begin
    FUseOwnerAsContext := Value;
  end;
end;

procedure TtgdReport.SetVariables(const Value: TtgdReportVariables);
begin
  FVariables.Assign(Value);
end;

procedure TtgdReport.ValidateTemplate;
var
  vGenerator: TtgdGenerator;
begin
  vGenerator := TtgdGenerator.Create(Self);
  try
    vGenerator.ValidateTemplate();
  finally
    vGenerator.Free;
  end;
end;

end.
