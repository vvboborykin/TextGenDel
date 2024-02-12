object TestMainForm: TTestMainForm
  Left = 311
  Top = 204
  Width = 1073
  Height = 578
  Caption = 'TextGenDel test application'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1057
    539)
  PixelsPerInch = 96
  TextHeight = 13
  object btnGenerateScript: TButton
    Left = 8
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Generate script'
    TabOrder = 0
    OnClick = btnGenerateScriptClick
  end
  object pgc1: TPageControl
    Left = 8
    Top = 48
    Width = 1041
    Height = 481
    ActivePage = tsTemplate
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object tsTemplate: TTabSheet
      Caption = 'Template'
      object mmoTemplate: TMemo
        Left = 0
        Top = 0
        Width = 1033
        Height = 453
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pm1
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
        OnKeyUp = mmoTemplateKeyUp
      end
    end
    object tsScript: TTabSheet
      Caption = 'Script'
      ImageIndex = 1
      object mmoScript: TMemo
        Left = 0
        Top = 0
        Width = 1033
        Height = 453
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pm1
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
      end
    end
    object tsResult: TTabSheet
      Caption = 'Result'
      ImageIndex = 2
      object mmoResult: TMemo
        Left = 0
        Top = 0
        Width = 1033
        Height = 453
        Align = alClient
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        PopupMenu = pm1
        ScrollBars = ssBoth
        TabOrder = 0
        WantTabs = True
      end
    end
  end
  object btnLoad: TButton
    Left = 512
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 2
    OnClick = btnLoadClick
  end
  object btnSave: TButton
    Left = 592
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object btnGenerateText: TButton
    Left = 312
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Generate text'
    TabOrder = 4
    OnClick = btnGenerateTextClick
  end
  object btnValidateTemplate: TButton
    Left = 160
    Top = 8
    Width = 137
    Height = 25
    Caption = 'Validate template'
    TabOrder = 5
    OnClick = btnValidateTemplateClick
  end
  object btnEdit: TButton
    Left = 696
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 6
    OnClick = btnEditClick
  end
  object dlgLoad: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 412
    Top = 136
  end
  object dlgSave: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 488
    Top = 136
  end
  object aclMain: TActionList
    Left = 92
    Top = 120
    object EditCut1: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 0
      ShortCut = 16472
    end
    object EditCopy1: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 1
      ShortCut = 16451
    end
    object EditPaste1: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 2
      ShortCut = 16470
    end
    object EditSelectAll1: TEditSelectAll
      Category = 'Edit'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
    end
    object EditUndo1: TEditUndo
      Category = 'Edit'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 3
      ShortCut = 16474
    end
    object EditDelete1: TEditDelete
      Category = 'Edit'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 5
      ShortCut = 46
    end
  end
  object pm1: TPopupMenu
    Left = 100
    Top = 184
    object Copy1: TMenuItem
      Action = EditCopy1
    end
    object Cut1: TMenuItem
      Action = EditCut1
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Paste1: TMenuItem
      Action = EditPaste1
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object SelectAll1: TMenuItem
      Action = EditSelectAll1
    end
    object Delete1: TMenuItem
      Action = EditDelete1
    end
    object Undo1: TMenuItem
      Action = EditUndo1
    end
  end
  object vtData: TVirtualTable
    Active = True
    Constraints = <>
    FieldDefs = <
      item
        Name = 'Name'
        DataType = ftString
        Size = 255
      end
      item
        Name = 'Code'
        DataType = ftInteger
      end
      item
        Name = 'Date'
        DataType = ftDateTime
      end
      item
        Name = 'Summa'
        DataType = ftFloat
      end>
    Left = 236
    Top = 248
    Data = {
      0400040004004E616D650100FF00000000000400436F64650300000000000000
      0400446174650B00000000000000050053756D6D610600000000000000000003
      0000000C000000C8ECFF20FDEBE5ECE5EDF2E004000000010000000800000000
      0054383F08CD42080000009A9999999909694008000000CDEEE2FBE920333304
      0000000200000008000000000018DF1008CD4208000000AE47E17A14AEF33F06
      000000CBEEE4E420300400000003000000080000000000FEF94004CD42080000
      009A99999999990B40}
    object vtDataName: TStringField
      FieldName = 'Name'
      Size = 255
    end
    object vtDataCode: TIntegerField
      FieldName = 'Code'
    end
    object vtDataDate: TDateTimeField
      FieldName = 'Date'
    end
    object vtDataSumma: TFloatField
      FieldName = 'Summa'
    end
  end
end
