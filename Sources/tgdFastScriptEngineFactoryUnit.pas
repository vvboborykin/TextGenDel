{*******************************************************
* Project: TextGenDelTest
* Unit: tgdFastScriptEngineFactoryUnit.pas
* Description: Implementation of ItgdScriptEngineFactory interface for FastScript
* 
* Created: 08.02.2024 10:28:54
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdFastScriptEngineFactoryUnit;

interface

uses
  SysUtils, Classes, tgdReportUnit;

type
  /// <summary>TtgdFastScriptEngineFactory
  /// Implementation of Script Engine Factory for FastScript
  /// </summary>
  /// <summary>TtgdFastScriptEngineFactory
  /// </summary>
  TtgdFastScriptEngineFactory = class(TtgdScriptEngineFactory)
  public
    function CreateScriptEngine: ItgdScriptEngine; override;
  end;

procedure Register;

implementation

{$R TextDelGenFastScript.dcr}

uses
  tgdFastScriptEngineUnit;

procedure Register;
begin
  RegisterComponents('Tgd', [TtgdFastScriptEngineFactory]);
end;

function TtgdFastScriptEngineFactory.CreateScriptEngine: ItgdScriptEngine;
begin
  Result := TtgdFastScriptEngine.Create;
end;

end.
