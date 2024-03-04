{*******************************************************
* Project: FastScriptDemo
* Unit: FastScriptDemoMainFormUnit.pas
* Description: Main form for FastScript engine demo application
*
* Created: 04.03.2024 16:19:25
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit FastScriptDemoMainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, fs_idbrtti, fs_iinirtti, fs_iextctrlsrtti, fs_iformsrtti,
  fs_iclassesrtti, tgdReportUnit, DB, DBClient, StdCtrls,
  tgdFastScriptEngineFactoryUnit, DemoBaseMainFormUnit, ActnList;

type
  /// <summary>TFastScriptDemoMainForm
  /// Main form for FastScript engine demo application
  /// </summary>
  TFastScriptDemoMainForm = class(TDemoBaseMainForm)
    fsClassesRTTI1: TfsClassesRTTI;
    fsFormsRTTI1: TfsFormsRTTI;
    fsExtCtrlsRTTI1: TfsExtCtrlsRTTI;
    fsIniRTTI1: TfsIniRTTI;
    fsDBRTTI1: TfsDBRTTI;
    fseScriptEngine: TtgdFastScriptEngineFactory;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FastScriptDemoMainForm: TFastScriptDemoMainForm;

implementation

{$R *.dfm}

end.
