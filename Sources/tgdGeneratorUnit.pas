unit tgdGeneratorUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, Contnrs, tgdReportUnit;

type
  TtgdGenerator = class
  private
    FReport: TtgdReport;
  protected
    procedure SetReport(const Value: TtgdReport);
  public
    constructor Create(AReport: TtgdReport);
    procedure GenerateText(AResultLines: TStrings);
    property Report: TtgdReport read FReport write SetReport;
  end;

implementation

const
  SAReportIsNil = 'AReport is nil';

constructor TtgdGenerator.Create(AReport: TtgdReport);
begin
  if AReport = nil then
    raise Exception.Create(SAReportIsNil);

  inherited Create;
  FReport := AReport;
end;

procedure TtgdGenerator.GenerateText(AResultLines: TStrings);
begin

end;

procedure TtgdGenerator.SetReport(const Value: TtgdReport);
begin
  if FReport <> Value then
  begin
    FReport := Value;
  end;
end;

end.
