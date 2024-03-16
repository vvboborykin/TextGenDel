object DemoBaseMainForm: TDemoBaseMainForm
  Left = 668
  Top = 163
  Caption = 'Demo Base Main Form'
  ClientHeight = 625
  ClientWidth = 869
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    869
    625)
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
    Left = 720
    Top = 7
    Width = 136
    Height = 30
    Action = actExit
    Anchors = [akTop, akRight]
    Cancel = True
    TabOrder = 2
  end
  object btnGenerateScript: TButton
    Left = 248
    Top = 8
    Width = 185
    Height = 30
    Action = actGenerateScript
    TabOrder = 3
  end
  object pgcMain: TPageControl
    Left = 16
    Top = 48
    Width = 838
    Height = 558
    ActivePage = tsData
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
    object tsData: TTabSheet
      Caption = 'Data'
      object grdData: TDBGrid
        Left = 0
        Top = 25
        Width = 830
        Height = 499
        Align = alClient
        DataSource = dsData
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -16
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'RecordId'
            Width = 76
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Name'
            Width = 394
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'Code'
            Width = 181
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'IsValid'
            Width = 100
            Visible = True
          end>
      end
      object navData: TDBNavigator
        Left = 0
        Top = 0
        Width = 830
        Height = 25
        DataSource = dsData
        Align = alTop
        TabOrder = 1
      end
    end
    object tsText: TTabSheet
      Caption = 'Text'
      ImageIndex = 1
      object mmoResult: TMemo
        Left = 0
        Top = 0
        Width = 830
        Height = 524
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
        WordWrap = False
      end
    end
  end
  object cdsData: TClientDataSet
    PersistDataPacket.Data = {
      6D0000009619E0BD0100000018000000040000000000030000006D0008526563
      6F726449640400010000000000044E616D650200490000000100055749445448
      02000200FF0004436F6465010049000000010005574944544802000200140007
      497356616C696402000300000000000000}
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
    Top = 192
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
  object dsData: TDataSource
    DataSet = cdsData
    Left = 68
    Top = 246
  end
end
