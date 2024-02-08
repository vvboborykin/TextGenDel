{*******************************************************
* Project: TextGenDelTest
* Unit: tgdScriptEngineUnit.pas
* Description: Scrip Engine interface declaration
*
* Created: 08.02.2024 9:41:02
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdReportUnit;

type
  /// <summary>ItgdScriptEngine
  /// Scrip Engine interface declaration
  /// </summary>
  ItgdScriptEngine = interface
  ['{E42CD0D9-F515-4EAA-9A38-7E55F648CA11}']
    /// <summary>ItgdScriptEngine.ConvertTemplateToScript
    /// Convert report`s TemplateLines to script lines
    /// </summary>
    /// <param name="AScript"> (TStrings) Result script lines</param>
    procedure ConvertTemplateToScript(AScript: TStrings); stdcall;
    /// <summary>ItgdScriptEngine.ExecuteScript
    /// Execute script (generate text)
    /// </summary>
    /// <param name="AScript"> (TStrings) Script source to generate text</param>
    /// <param name="AResultLines"> (TStrings) Result text lines recipient</param>
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
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


  /// <summary>TtgdCreateScriptEngineFunc
  /// Factory method type
  /// </summary>
  /// type:ItgdScriptEngine
  TtgdCreateScriptEngineFunc = function: ItgdScriptEngine;

var
  /// <summary>CreateScriptEngine
  /// Factory method, creates ItgdScriptEngine instance using current
  /// implementation class. Assigned by implementation class module
  /// </summary>
  CreateScriptEngine: TtgdCreateScriptEngineFunc;

implementation

end.
