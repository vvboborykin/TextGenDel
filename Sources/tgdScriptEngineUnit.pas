unit tgdScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, tgdReportUnit;

type
  ItgdScriptEngine = interface
  ['{E42CD0D9-F515-4EAA-9A38-7E55F648CA11}']
    procedure ConvertTemplateToScript(AScript: TStrings); stdcall;
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
    procedure Init(AReport: TtgdReport); stdcall;
    procedure ValidateScript(AScript: TStrings); stdcall;
  end;


  TtgdCreateScriptEngineFunc = function(): ItgdScriptEngine;

var
  CreateScriptEngine: TtgdCreateScriptEngineFunc;

implementation

end.
