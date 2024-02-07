unit tgdReportUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdCollectionsUnit;

type
  TtgdReport = class(TComponent)
  private
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
  published
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
end;

destructor TtgdReport.Destroy;
begin
  FreeAndNil(FFunctions);
  FreeAndNil(FVariables);
  FreeAndNil(FContext);
  FreeAndNil(FTemplateLines);
  inherited Destroy;
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
