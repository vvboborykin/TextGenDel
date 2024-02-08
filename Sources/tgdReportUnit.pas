unit tgdReportUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdCollectionsUnit,
  fs_iinterpreter;

type
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
    procedure SetDefaultMarkers;
    function DoCallMethod(Instance: TObject; ClassType: TClass; const MethodName:
        String; var Params: Variant): Variant;
  published
    property AddLineFunctionName: String read FAddLineFunctionName write
        SetAddLineFunctionName;
    property Context: TtgdReportContext read FContext write SetContext;
    property TemplateLines: TStringList read FTemplateLines write SetTemplateLines;
    property UseOwnerAsContext: Boolean read FUseOwnerAsContext write
        SetUseOwnerAsContext;
    property MacroBeginMarker: string read FMacroBeginMarker write
        SetMacroBeginMarker;
    property MacroEndMarker: string read FMacroEndMarker write SetMacroEndMarker;
    property CodeBeginMarker: string read FCodeBeginMarker write SetCodeBeginMarker;
    property CodeEndMarker: string read FCodeEndMarker write SetCodeEndMarker;
    property Functions: TtgdReportFunctions read FFunctions write SetFunctions;
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

function TtgdReport.DoCallMethod(Instance: TObject; ClassType: TClass; const
    MethodName: String; var Params: Variant): Variant;
var
  vItem: TtgdNamedCollectionItem;
begin
  Result := Unassigned;
  if FFunctions.TryFindName(MethodName, vItem) then
    Result := TtgdReportFunction(vItem).DoExecute(MethodName, Params);
end;

procedure TtgdReport.GenerateText(AResultLines: TStrings);
begin
  if AResultLines = nil then
    raise Exception.Create(SAResultLinesIsNil);
  with TtgdGenerator.Create(Self) do
  begin
    try
      GenerateText(AResultLines);
    finally
      Free;
    end;
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

end.
