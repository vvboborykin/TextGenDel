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
  tgdScriptElementsUnit;


type
  TtgdReport = class;

  /// <summary>ItgdScriptEngine
  /// Scrip Engine interface declaration
  /// </summary>
  ItgdScriptEngine = interface
  ['{E42CD0D9-F515-4EAA-9A38-7E55F648CA11}']
    /// <summary>ItgdScriptEngine.ConvertTemplateToScript
    /// Convert report`s TemplateLines to script lines
    /// </summary>
    /// <param name="AScript"> (TStrings) Result script lines</param>
    procedure ConvertTemplateToScript(AScript: TStrings; ATemplateLines: TStrings =
        nil); stdcall;
    /// <summary>ItgdScriptEngine.ExecuteScript
    /// Execute script (generate text)
    /// </summary>
    /// <param name="AScript"> (TStrings) Script source to generate text</param>
    /// <param name="AResultLines"> (TStrings) Result text lines recipient</param>
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
    /// <summary>ItgdScriptEngine.GetChildrenScriptElements
    /// Get script children elements
    /// </summary>
    /// <param name="AParent"> (TObject) </param>
    /// <param name="AChildren"> (TObjectList) </param>
    procedure GetChildrenScriptElements(AParent: TObject; AChildren:
        TtgdScriptElementList); stdcall;
    /// <summary>ItgdScriptEngine.GetCompletionItems
    /// Get autocompletion itetms for text
    /// </summary>
    /// <param name="AText"> (string) </param>
    /// <param name="AItems"> (TStrings) </param>
    /// <param name="AInserts"> (TStrings) </param>
    procedure GetCompletionItems(AText: string; AItems, AInserts: TStrings);
        stdcall;
    /// <summary>ItgdScriptEngine.Init
    /// Initialize script engine before use
    /// </summary>
    /// <param name="AReport"> (TtgdReport) Report context</param>
    procedure Init(AReport: TtgdReport); stdcall;
    /// <summary>ItgdScriptEngine.ValidateScript
    /// Validate script
    /// </summary>
    /// <param name="AScript"> (TStrings) Script lines</param>
    procedure ValidateScript(AScript: TStrings); stdcall;
  end;

  //1 Script engine factory abstract class
  TtgdScriptEngineFactory = class(TComponent)
  public
    /// <summary>ItgdScriptEngineFactory.CreateScriptEngine
    /// Create new instance of ItgdScriptEngine
    /// </summary>
    /// <returns> ItgdScriptEngine
    /// </returns>
    function CreateScriptEngine: ItgdScriptEngine; virtual; abstract;
  end;

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
    FScriptEngineFactory: TtgdScriptEngineFactory;
    FSyntaxName: String;
    FTemplateLines: TStringList;
    FUseOwnerAsContext: Boolean;
    FVariables: TtgdReportVariables;
    procedure SetScriptEngineFactory(const Value: TtgdScriptEngineFactory);
    procedure SetSyntaxName(const Value: String);
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
  protected
    /// <summary>TtgdReport.Notification
    /// Process notification of linked components deletion
    /// </summary>
    /// <param name="AComponent"> (TComponent) </param>
    /// <param name="Operation"> (TOperation) </param>
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    /// <summary>TtgdReport.SetDefaultMarkers
    /// Initiate marker properties with default values
    /// </summary>
    procedure SetDefaultMarkers; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>TtgdReport.GenerateText
    /// Generate text for TemplateLines in current context
    /// </summary>
    /// <param name="AResultLines"> (TStrings) Result text lines</param>
    procedure GenerateText(AResultLines: TStrings);
    /// <summary>TtgdReport.GenerateScript Convert TemplateLines to script lines
    /// </summary>
    /// <param name="AResultLines"> (TStrings) Result script lines</param>
    procedure GenerateScript(AResultLines: TStrings);
    /// <summary>TtgdReport.ValidateTemplate
    /// Check TemplateLines for correct template syntax
    /// </summary>
    procedure ValidateTemplate;
    /// <summary>TtgdReport.DoCallMethod
    /// Execute script function
    /// </summary>
    /// <returns> Variant
    /// </returns>
    /// <param name="Instance"> (TObject) Object owned method</param>
    /// <param name="ClassType"> (TClass) Class owned method</param>
    /// <param name="MethodName"> (String) The name of method or function or
    /// procedure</param>
    /// <param name="Params"> (Variant) Array of parameter values</param>
    function DoCallMethod(Instance: TObject; ClassType: TClass; const MethodName:
        String; var Params: Variant): Variant;
    /// <summary>TtgdReport.Assign
    /// Copy properties values from source objects
    /// </summary>
    /// <param name="Source"> (TPersistent) Source object</param>
    procedure Assign(Source: TPersistent); override;
    /// <summary>TtgdReport.CreateScriptEngine
    /// Create script engine
    /// </summary>
    /// <returns> ItgdScriptEngine
    /// </returns>
    function CreateScriptEngine: ItgdScriptEngine;
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
    /// <summary>TtgdReport.ScriptEngineFactory
    /// ScriptEngine factory reference
    /// </summary>
    /// type:ItgdScriptEngineFactory
    property ScriptEngineFactory: TtgdScriptEngineFactory read FScriptEngineFactory
        write SetScriptEngineFactory;
    /// <summary>TtgdReport.SyntaxName Template syntax for component editor
    /// </summary> type:String
    property SyntaxName: String read FSyntaxName write SetSyntaxName;
    /// <summary>TtgdReport.Variables
    /// Script Custom varibles
    /// </summary>
    /// type:TtgdReportVariables
    property Variables: TtgdReportVariables read FVariables write SetVariables;
  end;

procedure Register;

const
  STextGenDelComponentPage = 'TextGenDel';

implementation

uses
  tgdGeneratorUnit;

resourcestring
  SScriptEngineFactoryPropertyIsNil = 'ScriptEngineFactory property is nil';

{$R TextGenDel.dcr}  

procedure Register;
begin
  RegisterComponents(STextGenDelComponentPage, [TtgdReport]);
end;


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
    ScriptEngineFactory := vReport.ScriptEngineFactory;
  end
  else
    inherited Assign(Source);
end;

function TtgdReport.CreateScriptEngine: ItgdScriptEngine;
begin
  if FScriptEngineFactory = nil then
    raise Exception.Create(SScriptEngineFactoryPropertyIsNil);
  Result := FScriptEngineFactory.CreateScriptEngine;
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

procedure TtgdReport.Notification(AComponent: TComponent; Operation:
    TOperation);
begin
  if (Operation = opRemove) and (AComponent = FScriptEngineFactory) then
  begin
    FScriptEngineFactory := nil;
  end;
  inherited;
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

procedure TtgdReport.SetScriptEngineFactory(const Value:
    TtgdScriptEngineFactory);
begin
  if FScriptEngineFactory <> Value then
  begin
    FScriptEngineFactory := Value;
  end;
end;

procedure TtgdReport.SetSyntaxName(const Value: String);
begin
  if FSyntaxName <> Value then
  begin
    FSyntaxName := Value;
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
