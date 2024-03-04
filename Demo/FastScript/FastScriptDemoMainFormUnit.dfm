inherited FastScriptDemoMainForm: TFastScriptDemoMainForm
  Left = 952
  Top = 172
  Caption = 'FastScript Demo Main Form'
  PixelsPerInch = 96
  TextHeight = 19
  inherited pgcMain: TPageControl
    inherited tsData: TTabSheet
      inherited navData: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited tsText: TTabSheet
      inherited mmoResult: TMemo
        Height = 516
      end
    end
  end
  inherited tgrReport: TtgdReport
    ScriptEngineFactory = fseScriptEngine
  end
  object fsClassesRTTI1: TfsClassesRTTI [8]
    Left = 296
    Top = 32
  end
  object fsFormsRTTI1: TfsFormsRTTI [9]
    Left = 296
    Top = 80
  end
  object fsExtCtrlsRTTI1: TfsExtCtrlsRTTI [10]
    Left = 296
    Top = 128
  end
  object fsIniRTTI1: TfsIniRTTI [11]
    Left = 296
    Top = 176
  end
  object fsDBRTTI1: TfsDBRTTI [12]
    Left = 296
    Top = 224
  end
  object fseScriptEngine: TtgdFastScriptEngineFactory [13]
    Left = 136
    Top = 136
  end
end
