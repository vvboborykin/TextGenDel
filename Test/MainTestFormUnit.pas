unit MainTestFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DialogsX, StdCtrls, ComCtrls, tgdReportUnit, Menus, StdActns,
  ActnList, DB, MemDS, VirtualTable, tgdCollectionsUnit, fs_idbrtti,
  fs_iinirtti, fs_imenusrtti, fs_idialogsrtti, fs_iextctrlsrtti,
  fs_iformsrtti, fs_igraphicsrtti, fs_iclassesrtti;

type
  TTestMainForm = class(TForm)
    btnGenerateScript: TButton;
    pgc1: TPageControl;
    tsTemplate: TTabSheet;
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
    mmoTemplate: TMemo;
    vtData: TVirtualTable;
    vtDataName: TStringField;
    vtDataCode: TIntegerField;
    vtDataDate: TDateTimeField;
    vtDataSumma: TFloatField;
    btnEdit: TButton;
    fsClassesRTTI1: TfsClassesRTTI;
    fsGraphicsRTTI1: TfsGraphicsRTTI;
    fsFormsRTTI1: TfsFormsRTTI;
    fsExtCtrlsRTTI1: TfsExtCtrlsRTTI;
    fsDialogsRTTI1: TfsDialogsRTTI;
    fsMenusRTTI1: TfsMenusRTTI;
    fsIniRTTI1: TfsIniRTTI;
    fsDBRTTI1: TfsDBRTTI;
    procedure btnEditClick(Sender: TObject);
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
    function SayTime(Sender: TtgdReportFunction; AParams: Variant): Variant;
  public
    { Public declarations }
  end;

var
  TestMainForm: TTestMainForm;

implementation

uses
  tgdReportEditorFormUnit;

{$R *.dfm}

procedure TTestMainForm.btnEditClick(Sender: TObject);
begin
  TtgdReportEditorForm.ShowEditor(FReport);
end;

procedure TTestMainForm.FormDestroy(Sender: TObject);
begin
  FReport.Free;
end;

procedure TTestMainForm.FormCreate(Sender: TObject);
begin
  FReport := TtgdReport.Create(Self);
  FReport.Name := 'TtgdReport1';
  FReport.Variables.Add('CurrentTimeText', DateTimeToStr(Now));
  FReport.Functions.Add('function SayTime(): String').OnExecute := Self.SayTime;
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

function TTestMainForm.SayTime(Sender: TtgdReportFunction; AParams: Variant):
    Variant;
begin
  Result := TimeToStr(Now);
end;

end.
