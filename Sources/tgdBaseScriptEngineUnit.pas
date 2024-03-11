{*******************************************************
* Project: TextGenDelD7
* Unit: tgdBaseScriptEngineUnit.pas
* Description: Base class for script engine implementations
* 
* Created: 09.03.2024 11:40:48
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdBaseScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, 
  tgdScriptElementsUnit, tgdReportUnit;


type
  /// <summary>TtgdBaseScriptEngine
  /// Base class for script engine implementations
  /// </summary>
  TtgdBaseScriptEngine = class(TInterfacedObject)
  private
  protected
    /// <summary>TtgdBaseScriptEngine.FNestCount
    /// Nest level of parse
    /// </summary>
    /// type:Integer
    FNestCount: Integer;
    /// <summary>TtgdBaseScriptEngine.FReport
    /// TextGenDel report - engine context
    /// </summary>
    /// type:TtgdReport
    FReport: TtgdReport;
    /// <summary>TtgdBaseScriptEngine.AddScriptCodeLine Add line of script code to
    /// script
    /// </summary>
    /// <param name="ALine"> (string) </param>
    /// <param name="AScript"> (TStrings) </param>
    procedure AddScriptCodeLine(ALine: string; AScript: TStrings); virtual;
    /// <summary>TtgdBaseScriptEngine.AddScriptMacroLine
    /// Add line having macro definitions to script
    /// </summary>
    /// <param name="ALine"> (string) </param>
    /// <param name="AScript"> (TStrings) </param>
    procedure AddScriptMacroLine(ALine: string; AScript: TStrings); virtual;
    /// <summary>TtgdBaseScriptEngine.CheckInitCompleted Verify for generator was
    /// properly initiated before use
    /// </summary>
    procedure CheckInitCompleted; virtual;
    /// <summary>TtgdBaseScriptEngine.GenerateScriptLine
    /// Convert line from template format to script  format
    /// </summary>
    /// <param name="ALine"> (string) </param>
    /// <param name="AScript"> (TStrings) </param>
    procedure GenerateScriptLine(ALine: string; AScript: TStrings); virtual;
    /// <summary>TtgdBaseScriptEngine.ReplaceMacroses
    /// Replace macro definitions with script
    /// </summary>
    /// <returns> string
    /// </returns>
    /// <param name="ALine"> (string) </param>
    function ReplaceMacroses(ALine: string): string; virtual;
  protected
    /// <summary>TtgdBaseScriptEngine.Init
    /// Initiate generator  before usage
    /// </summary>
    /// <param name="AReport"> (TtgdReport) </param>
    procedure Init(AReport: TtgdReport); virtual; stdcall;
    /// <summary>TtgdBaseScriptEngine.ConvertTemplateToScript
    /// Convert template lines to script lines
    /// </summary>
    /// <param name="AScript"> (TStrings) </param>
    /// <param name="ATemplateLines"> (TStrings) </param>
    procedure ConvertTemplateToScript(AScript: TStrings; ATemplateLines: TStrings =
        nil); virtual; stdcall;
  end;

resourcestring
  SAReportIsNil = 'AReport is nil';
  SInitMetodNotExecuted = 'Init metod not executed';

implementation

procedure TtgdBaseScriptEngine.AddScriptCodeLine(ALine: string; AScript:
    TStrings);
var
  vText: string;
begin
  vText := StringReplace(ALine, FReport.CodeBeginMarker, '', [rfReplaceAll]);
  vText := StringReplace(vText, FReport.CodeEndMarker, '', [rfReplaceAll]);
  AScript.Add(vText);
end;

procedure TtgdBaseScriptEngine.AddScriptMacroLine(ALine: string; AScript:
    TStrings);
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

procedure TtgdBaseScriptEngine.CheckInitCompleted;
begin
  if FReport = nil then
    raise Exception.Create(SInitMetodNotExecuted);
end;

procedure TtgdBaseScriptEngine.ConvertTemplateToScript(AScript: TStrings;
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

procedure TtgdBaseScriptEngine.GenerateScriptLine(ALine: string; AScript:
    TStrings);
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

procedure TtgdBaseScriptEngine.Init(AReport: TtgdReport);
begin
  if (AReport = nil) then
    raise Exception.Create(SAReportIsNil);

  FReport := AReport;
end;

function TtgdBaseScriptEngine.ReplaceMacroses(ALine: string): string;
begin
  Result := ALine;
  Result := StringReplace(Result, FReport.MacroBeginMarker, ''' + ', [rfReplaceAll]);
  Result := StringReplace(Result, FReport.MacroEndMarker, ' + ''', [rfReplaceAll]);
end;

end.
 