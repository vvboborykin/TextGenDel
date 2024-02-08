unit MainTestFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DialogsX, StdCtrls, ComCtrls, tgdReportUnit, Menus, StdActns,
  ActnList;

type
  TTestMainForm = class(TForm)
    btnGenerateScript: TButton;
    pgc1: TPageControl;
    tsTemplate: TTabSheet;
    mmoTemplate: TMemo;
    tsScript: TTabSheet;
    mmoScript: TMemo;
    tsResult: TTabSheet;
    mmoResult: TMemo;
    btnLoad: TButton;
    btnSave: TButton;
    dlgLoad: TFileOpenDialog;
    dlgSave: TFileSaveDialog;
    btnGenerateText: TButton;
    btnValidateTemplate: TButton;
    aclMain: TActionList;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    pm1: TPopupMenu;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    SelectAll1: TMenuItem;
    Undo1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGenerateScriptClick(Sender: TObject);
    procedure btnGenerateTextClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnValidateTemplateClick(Sender: TObject);
    procedure mmoTemplateKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FReport: TtgdReport;
  public
    { Public declarations }
  end;

var
  TestMainForm: TTestMainForm;

implementation

{$R *.dfm}

procedure TTestMainForm.FormDestroy(Sender: TObject);
begin
  FReport.Free;
end;

procedure TTestMainForm.FormCreate(Sender: TObject);
begin
  FReport := TtgdReport.Create(Self);
end;

procedure TTestMainForm.btnGenerateScriptClick(Sender: TObject);
begin
  FReport.TemplateLines.Assign(mmoTemplate.Lines);
  mmoScript.Lines.Clear;
  FReport.GenerateScript(mmoScript.Lines);
  pgc1.ActivePageIndex := 1;
end;

procedure TTestMainForm.btnGenerateTextClick(Sender: TObject);
begin
  FReport.TemplateLines.Assign(mmoTemplate.Lines);
  mmoResult.Lines.Clear;
  FReport.GenerateText(mmoResult.Lines);
  pgc1.ActivePageIndex := 2;
end;

procedure TTestMainForm.btnLoadClick(Sender: TObject);
begin
  if dlgLoad.Execute then
    TMemo(pgc1.ActivePage.Controls[0]).Lines.LoadFromFile(dlgLoad.FileName);
end;

procedure TTestMainForm.btnSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
    TMemo(pgc1.ActivePage.Controls[0]).Lines.SaveToFile(dlgSave.FileName);
end;

procedure TTestMainForm.btnValidateTemplateClick(Sender: TObject);
begin
  FReport.TemplateLines.Assign(mmoTemplate.Lines);
  try
    FReport.ValidateTemplate;
    ShowMessage('Template is valid');
  except
    on E: Exception do
      ShowMessage('Error: ' + E.Message);
  end;
end;

procedure TTestMainForm.mmoTemplateKeyUp(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  vMemo: TMemo;
begin
  if Key = 9 then
  begin
    vMemo := TMemo(Sender);
    vMemo.Lines[vMemo.CaretPos.Y] := StringReplace(vMemo.Lines[vMemo.CaretPos.Y], #9, '  ', [rfReplaceAll]);
  end;
end;

end.
