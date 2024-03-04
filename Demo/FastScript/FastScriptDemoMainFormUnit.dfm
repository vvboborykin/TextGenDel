inherited FastScriptDemoMainForm: TFastScriptDemoMainForm
  Left = 952
  Top = 172
  Caption = 'FastScript Demo Main Form'
  PixelsPerInch = 96
  TextHeight = 19
  inherited tgrReport: TtgdReport
    ScriptEngineFactory = fseScriptEngine
  end
  object fsClassesRTTI1: TfsClassesRTTI
    Left = 296
    Top = 32
  end
  object fsFormsRTTI1: TfsFormsRTTI
    Left = 296
    Top = 80
  end
  object fsExtCtrlsRTTI1: TfsExtCtrlsRTTI
    Left = 296
    Top = 128
  end
  object fsIniRTTI1: TfsIniRTTI
    Left = 296
    Top = 176
  end
  object fsDBRTTI1: TfsDBRTTI
    Left = 296
    Top = 224
  end
  object fseScriptEngine: TtgdFastScriptEngineFactory
    Left = 136
    Top = 136
  end
end
