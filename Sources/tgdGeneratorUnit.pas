{*******************************************************
* Project: TextGenDelTest
* Unit: tgdGeneratorUnit.pas
* Description: Template generator
*
* Created: 08.02.2024 10:40:00
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdGeneratorUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, Contnrs, tgdReportUnit;

type
  /// <summary>TtgdGenerator
  /// Template generator
  /// </summary>
  TtgdGenerator = class
  private
    FReport: TtgdReport;
  protected
    procedure SetReport(const Value: TtgdReport);
  public
    /// <summary>TtgdGenerator.Create
    /// Initializing constructor
    /// </summary>
    /// <param name="AReport"> (TtgdReport) </param>
    constructor Create(AReport: TtgdReport);
    /// <summary>TtgdGenerator.GenerateText Generate script and execute it for report
    /// TemplateLines property
    /// </summary>
    /// <param name="AResultLines"> (TStrings) Result text recipient</param>
    procedure GenerateText(AResultLines: TStrings);
    procedure GenerateScript(AResultLines: TStrings);
    procedure ValidateTemplate;
    /// <summary>TtgdGenerator.Report
    /// Owner
    /// </summary>
    /// type:TtgdReport
    property Report: TtgdReport read FReport write SetReport;
  end;

implementation

uses
  tgdScriptEngineUnit;

const
  SAReportIsNil = 'AReport is nil';

constructor TtgdGenerator.Create(AReport: TtgdReport);
begin
  if AReport = nil then
    raise Exception.Create(SAReportIsNil);

  inherited Create;
  FReport := AReport;
end;

procedure TtgdGenerator.GenerateScript(AResultLines: TStrings);
var
  vEngine: ItgdScriptEngine;
begin
  vEngine := CreateScriptEngine;
  vEngine.Init(FReport);
  vEngine.ConvertTemplateToScript(AResultLines);
end;

procedure TtgdGenerator.GenerateText(AResultLines: TStrings);
var
  vEngine: ItgdScriptEngine;
  vScript: TStringList;
begin
  vEngine := CreateScriptEngine;
  vScript := TStringList.Create();
  try
    vEngine.Init(FReport);
    vEngine.ConvertTemplateToScript(vScript);
    vEngine.ExecuteScript(vScript, AResultLines);
  finally
    vScript.Free;
  end;
end;

procedure TtgdGenerator.SetReport(const Value: TtgdReport);
begin
  if FReport <> Value then
  begin
    FReport := Value;
  end;
end;

procedure TtgdGenerator.ValidateTemplate;
var
  vEngine: ItgdScriptEngine;
  vScript: TStringList;
begin
  vEngine := CreateScriptEngine;
  vScript := TStringList.Create();
  try
    vEngine.Init(FReport);
    vEngine.ConvertTemplateToScript(vScript);
    vEngine.ValidateScript(vScript);
  finally
    vScript.Free;
  end;
end;

end.
