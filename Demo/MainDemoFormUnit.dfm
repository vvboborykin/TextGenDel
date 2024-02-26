object MainDemoForm: TMainDemoForm
  Left = 487
  Top = 146
  Width = 377
  Height = 373
  Caption = 'Main Demo Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnGenerateXml: TButton
    Left = 16
    Top = 16
    Width = 129
    Height = 25
    Caption = 'Generate Xml'
    TabOrder = 0
    OnClick = btnGenerateXmlClick
  end
  object tgrReport: TtgdReport
    AddLineFunctionName = 'AddLine'
    Context = <>
    TemplateLines.Strings = (
      '<?xml version="1.0" encoding="utf-8"?>'
      '<information>'
      '{{'
      '  cdsData.First;'
      '  while not cdsData.Eof do begin'
      '    if cdsDataIsValid.AsBoolean then begin'
      '}}'
      
        '  <RecordData Id="{= cdsDataRecordId.AsString =}" Code="{= cdsDa' +
        'taCode.AsString =}">{= cdsDataName.AsString =}</RecordData>   '
      '{{'
      '    end;'
      '    cdsData.Next;'
      '  end;   '
      '}}'
      '</information>')
    UseOwnerAsContext = True
    MacroBeginMarker = '{='
    MacroEndMarker = '=}'
    CodeBeginMarker = '{{'
    CodeEndMarker = '}}'
    Functions = <>
    Variables = <>
    Left = 40
    Top = 56
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
  object cdsData: TClientDataSet
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'RecordId'
        DataType = ftInteger
      end
      item
        Name = 'Name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'Code'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'IsValid'
        DataType = ftBoolean
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 40
    Top = 112
    Data = {
      6D0000009619E0BD0100000018000000040000000000030000006D0008526563
      6F726449640400010000000000044E616D650200490000000100055749445448
      02000200FF0004436F6465010049000000010005574944544802000200140007
      497356616C696402000300000000000000}
    object cdsDataRecordId: TIntegerField
      FieldName = 'RecordId'
    end
    object cdsDataName: TStringField
      FieldName = 'Name'
      Size = 255
    end
    object cdsDataCode: TStringField
      FieldName = 'Code'
    end
    object cdsDataIsValid: TBooleanField
      FieldName = 'IsValid'
    end
  end
end
