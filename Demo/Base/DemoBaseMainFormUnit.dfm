object DemoBaseMainForm: TDemoBaseMainForm
  Left = 438
  Top = 277
  Width = 885
  Height = 539
  Caption = 'Demo Base Main Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    869
    500)
  PixelsPerInch = 96
  TextHeight = 19
  object btnGenerateXml: TButton
    Left = 448
    Top = 8
    Width = 185
    Height = 30
    Action = actGenerateXml
    TabOrder = 0
  end
  object btnEditTemplate: TButton
    Left = 16
    Top = 8
    Width = 217
    Height = 30
    Action = actEditTemplate
    TabOrder = 1
  end
  object btnExit: TButton
    Left = 671
    Top = 7
    Width = 185
    Height = 30
    Action = actExit
    Anchors = [akTop, akRight]
    Cancel = True
    TabOrder = 2
  end
  object mmoResult: TMemo
    Left = 16
    Top = 48
    Width = 840
    Height = 441
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 3
    WantTabs = True
    WordWrap = False
  end
  object btnGenerateScript: TButton
    Left = 248
    Top = 8
    Width = 185
    Height = 30
    Action = actGenerateScript
    TabOrder = 4
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
    Left = 72
    Top = 72
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
    SyntaxName = 
      'XML Files (*.xml;*.xsd;*.xsl;*.xslt;*.dtd)|*.xml;*.xsd;*.xsl;*.x' +
      'slt;*.dtd'
    Variables = <>
    Left = 136
    Top = 72
  end
  object aclMain: TActionList
    Left = 200
    Top = 72
    object actEditTemplate: TAction
      Caption = 'Edit Template'
      OnExecute = actEditTemplateExecute
    end
    object actGenerateXml: TAction
      Caption = 'Generate Xml'
      OnExecute = actGenerateXmlExecute
    end
    object actExit: TAction
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
    object actGenerateScript: TAction
      Caption = 'Generate Script'
      OnExecute = actGenerateScriptExecute
    end
  end
end
