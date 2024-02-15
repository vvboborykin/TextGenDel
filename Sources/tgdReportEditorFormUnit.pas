unit tgdReportEditorFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, SynMemo, StdCtrls,
  ImgList, ComCtrls, ExtCtrls, SynEditMiscClasses, SynEditSearch,
  SynCompletionProposal, ActnList, SynHighlighterMulti, Buttons,
  tgdScriptEngineUnit, SynHighlighterXML, SynHighlighterJSON, tgdReportUnit,
  tdgScriptElementsUnit, DialogsX, frxUnicodeUtils, SynUnicode;

type
  TtgdReportEditorForm = class(TForm)
    synpPascalSyntax: TSynPasSyn;
    synsSearch: TSynEditSearch;
    pnlClient: TPanel;
    splVert: TSplitter;
    tvContext: TTreeView;
    pnlBttons: TPanel;
    synmTemplate: TSynMemo;
    ilTree: TImageList;
    aclMain: TActionList;
    symMain: TSynMultiSyn;
    syxXmlSyntax: TSynXMLSyn;
    actSave: TAction;
    actCancel: TAction;
    actLoad: TAction;
    actOk: TAction;
    btnSave: TBitBtn;
    btnLoadFromFile: TBitBtn;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnValidate: TBitBtn;
    actValidate: TAction;
    synjJsonSyntax: TSynJSONSyn;
    dlgLoadTemplate: TFileOpenDialog;
    dlgSaveTemplate: TFileSaveDialog;
    SynCompletionProposal1: TSynCompletionProposal;
    actExecuteReport: TAction;
    btnExecuteReport: TBitBtn;
    dlgSaveReport: TFileSaveDialog;
    statTempl: TStatusBar;
    pnlTemplate: TPanel;
    procedure actCancelExecute(Sender: TObject);
    procedure actExecuteReportExecute(Sender: TObject);
    procedure actLoadExecute(Sender: TObject);
    procedure actOkExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actValidateExecute(Sender: TObject);
    procedure SynCompletionProposal1Execute(Kind: SynCompletionType; Sender:
      TObject; var CurrentInput: WideString; var x, y: Integer; var CanExecute: Boolean);
    procedure synmTemplateChange(Sender: TObject);
    procedure synmTemplateKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvContextEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure tvContextExpanding(Sender: TObject; Node: TTreeNode; var
      AllowExpansion: Boolean);
    procedure tvContextMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
  private
    FReport: TtgdReport;
    FEngine: ItgdScriptEngine;
    procedure AddChildNode(AParentNode: TTreeNode; AScriptElement: TtgdScriptElement);
    procedure AddImagesToProposalList(AProposalList: TUnicodeStrings; AItems:
        TStrings);
    function GetImageIndexOfScriptElement(AScriptElement: TtgdScriptElement): Integer;
    function GetNodeChainText(ANode: TTreeNode): string;
    function GetRandomName: string;
    function GetScriptElementClassImage(AClass: TClass): Integer;
    function GetTextLeftOfCaret: string;
    procedure Init(AReport: TtgdReport);
    procedure InsertSelectedTreeNodeToTemplate;
    procedure LoadAutoCompleteList(ANestLevel: Integer = 4);
    procedure LoadTopTreeNodes;
    procedure LoadTreeNodes(ANode: TTreeNode; AScriptElementClass: array of TClass);
    procedure UpdateStatusPanel;
  public
    class function ShowEditor(AReport: TtgdReport): Boolean;
  end;

implementation

uses
  StrUtils, ComObj, shellapi;

resourcestring
  STemplateIsValid = 'Template is valid';
  SAReportIsNil = 'AReport is nil';

{$R *.dfm}

procedure TtgdReportEditorForm.actCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TtgdReportEditorForm.actExecuteReportExecute(Sender: TObject);
var
  I: Integer;
  vReport: TtgdReport;
  vResultLines: TStrings;
begin
  vResultLines := TStringList.Create;
  vReport := TtgdReport.Create(FReport.Owner);
  vReport.Name := 'TtgdReport' + GetRandomName();
  try
    vReport.Assign(FReport);
    vReport.TemplateLines.Clear;
    for I := 0 to synmTemplate.Lines.Count-1 do
      vReport.TemplateLines.Add(synmTemplate.Lines[I]);
    vReport.GenerateText(vResultLines);
    if dlgSaveReport.Execute then
    begin
      vResultLines.SaveToFile(dlgSaveReport.FileName);
      ShellExecute(0, 'open', PChar(dlgSaveReport.FileName), nil, nil, 0);
    end;
  finally
    vReport.Free;
    vResultLines.Free;
  end;
end;

procedure TtgdReportEditorForm.actLoadExecute(Sender: TObject);
begin
  if dlgLoadTemplate.Execute then
  begin
    synmTemplate.Lines.LoadFromFile(dlgLoadTemplate.FileName);
    if AnsiSameText('.xml', ExtractFileExt(dlgLoadTemplate.FileName)) then
      symMain.DefaultHighlighter := syxXmlSyntax
    else if AnsiSameText('.json', ExtractFileExt(dlgLoadTemplate.FileName)) then
      symMain.DefaultHighlighter := synjJsonSyntax
    else
      symMain.DefaultHighlighter := nil;
  end;
end;

procedure TtgdReportEditorForm.actOkExecute(Sender: TObject);
begin
  FReport.TemplateLines.Assign(synmTemplate.Lines);
end;

procedure TtgdReportEditorForm.actSaveExecute(Sender: TObject);
begin
  if dlgSaveTemplate.Execute then
    synmTemplate.Lines.SaveToFile(dlgSaveTemplate.FileName);
end;

procedure TtgdReportEditorForm.actValidateExecute(Sender: TObject);
var
  I: Integer;
  vLines: TStrings;
  vScript: TStringList;
  vScriptEngine: ItgdScriptEngine;
begin
  vLines := TStringList.Create;
  vScript := TStringList.Create();
  try
    for I := 0 to synmTemplate.Lines.Count-1 do
      vLines.Add(synmTemplate.Lines[I]);
    vScriptEngine := CreateScriptEngine();
    vScriptEngine.Init(FReport);
    vScriptEngine.ConvertTemplateToScript(vScript, vLines);
    vScriptEngine.ValidateScript(vScript);
    ShowMessage(STemplateIsValid);
  finally
    vScript.Free;
    vLines.Free;
  end;
end;

procedure TtgdReportEditorForm.AddChildNode(AParentNode: TTreeNode;
  AScriptElement: TtgdScriptElement);
var
  vNode: TTreeNode;
begin
  vNode := tvContext.Items.AddChild(AParentNode, AScriptElement.Name);
  vNode.HasChildren := AScriptElement.HasChildren;
  vNode.ImageIndex := GetImageIndexOfScriptElement(AScriptElement);
  vNode.SelectedIndex := vNode.ImageIndex;
  vNode.Data := Pointer(Integer(AScriptElement.SourceObject));
end;

procedure TtgdReportEditorForm.AddImagesToProposalList(AProposalList:
    TUnicodeStrings; AItems: TStrings);
var
  I: Integer;
  vImageIndex: Integer;
  vString: string;
begin
  for I := 0 to AProposalList.Count-1 do
  begin
    vString := AProposalList[I];
    vImageIndex := GetScriptElementClassImage(TClass(AItems.Objects[I]));
    vString := Format('\IMAGE{%d}', [vImageIndex]) + vString;
    AProposalList[I] := vString;
  end;
end;

function TtgdReportEditorForm.GetImageIndexOfScriptElement(AScriptElement:
  TtgdScriptElement): Integer;
begin
  Result := 0;
  if AScriptElement is TtgdScriptVariable then
    Result := 6
  else if AScriptElement is TtgdScriptClass then
    Result := 1
  else if AScriptElement is TtgdScriptFunction then
    Result := 4
  else if AScriptElement is TtgdScriptProperty then
    Result := 0
  else if AScriptElement is TtgdScriptEvent then
    Result := 3
  else if AScriptElement is TtgdScriptMethod then
    Result := 2
  else if AScriptElement is TtgdScriptConstant then
    Result := 5
  else if AScriptElement is TtgdScriptConstant then
    Result := 6
  else
end;

function TtgdReportEditorForm.GetNodeChainText(ANode: TTreeNode): string;
begin
  Result := '';
  if ANode.Parent <> nil then
    Result := GetNodeChainText(ANode.Parent);
  Result := Result + IfThen(Result = '', '', '.') + ANode.Text
end;

function TtgdReportEditorForm.GetRandomName: string;
begin
  Result := CreateClassID;
  Result := MidStr(Result, 2, Length(Result)-2);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
end;

function TtgdReportEditorForm.GetScriptElementClassImage(AClass: TClass):
    Integer;
begin
  Result := 0;
  if AClass = TtgdScriptVariable then
    Result := 6
  else if AClass = TtgdScriptClass then
    Result := 1
  else if AClass = TtgdScriptFunction then
    Result := 4
  else if AClass = TtgdScriptProperty then
    Result := 0
  else if AClass = TtgdScriptEvent then
    Result := 3
  else if AClass = TtgdScriptMethod then
    Result := 2
  else if AClass = TtgdScriptConstant then
    Result := 5
end;

function TtgdReportEditorForm.GetTextLeftOfCaret: string;
const
  TokenChars =['a'..'z', 'A'..'Z', '_', '.'];
var
  I: Integer;
  vLine: string;
  vChar: Char;
begin
  Result := '';
  with synmTemplate do
  begin
    if Lines.Count > 0 then
    begin
      vLine := Lines[CaretY - 1];
      I := CaretX - 1;
      while I >= 1 do
      begin
        vChar := vLine[I];
        if vChar in TokenChars then
        begin
          Result := vChar + Result;
          Dec(I)
        end
        else
          Break;
      end;
    end;
  end;
end;

procedure TtgdReportEditorForm.Init(AReport: TtgdReport);
begin
  FReport := AReport;
  FEngine := CreateScriptEngine();
  FEngine.Init(FReport);

  synmTemplate.Lines.Assign(AReport.TemplateLines);
  LoadTopTreeNodes();
  LoadAutoCompleteList();
  UpdateStatusPanel();
end;

procedure TtgdReportEditorForm.InsertSelectedTreeNodeToTemplate;
var
  vNodeText: string;
  vText: string;
begin
  if tvContext.Selected <> nil then
  begin
    with synmTemplate do
    begin
      if Lines.Count < 1 then
        Lines.Add('');
      vText := Lines[CaretY - 1];
      vNodeText := ' ' + GetNodeChainText(tvContext.Selected) + ' ';
      Insert(vNodeText, vText, CaretX);
      Lines[CaretY - 1] := vText;
    end;
    synmTemplate.SetFocus;
  end;
end;

procedure TtgdReportEditorForm.LoadAutoCompleteList(ANestLevel: Integer = 4);
begin

end;

procedure TtgdReportEditorForm.LoadTopTreeNodes;
begin
  LoadTreeNodes(nil, [TtgdScriptVariable, TtgdScriptFunction, TtgdScriptType]);
end;

procedure TtgdReportEditorForm.LoadTreeNodes(ANode: TTreeNode;
  AScriptElementClass: array of TClass);
var
  I: Integer;
  vChildren: TtgdScriptElementList;
  vParentObject: TObject;
begin
  vChildren := TtgdScriptElementList.Create(True);

  if Length(AScriptElementClass) > 0 then
  begin
    for I := Low(AScriptElementClass) to High(AScriptElementClass) do
      vChildren.AllowedClasses.Add(Pointer(AScriptElementClass[I]));
  end;

  try
    vParentObject := nil;

    if ANode <> nil then
      vParentObject := TObject(Integer(ANode.Data));

    FEngine.GetChildrenScriptElements(vParentObject, vChildren);
    vChildren.SortByName();

    for I := 0 to vChildren.Count - 1 do
      AddChildNode(ANode, vChildren[I]);
  finally
    vChildren.Free;
  end;
end;

class function TtgdReportEditorForm.ShowEditor(AReport: TtgdReport): Boolean;
var
  vForm: TtgdReportEditorForm;
begin
  if AReport = nil then
    raise Exception.Create(SAReportIsNil);

  vForm := TtgdReportEditorForm.Create(nil);
  try
    vForm.Init(AReport);
    Result := IsPositiveResult(vForm.ShowModal);
  finally
    vForm.Free;
  end;
end;

procedure TtgdReportEditorForm.SynCompletionProposal1Execute(Kind:
  SynCompletionType; Sender: TObject; var CurrentInput: WideString; var x, y:
  Integer; var CanExecute: Boolean);
var
  vInserts: TStringList;
  vItems: TStringList;
  vText: string;
begin
  vText := GetTextLeftOfCaret();
  vInserts := TStringList.Create();
  try
    vItems := TStringList.Create;
    try
      FEngine.GetCompletionItems(vText, vItems, vInserts);

      SynCompletionProposal1.ItemList.Clear;
      SynCompletionProposal1.ItemList.AddStrings(vItems);
      AddImagesToProposalList(SynCompletionProposal1.ItemList, vItems);

      SynCompletionProposal1.InsertList.Clear;
      SynCompletionProposal1.InsertList.AddStrings(vInserts);
    finally
      vItems.Free;
    end;
  finally
    vInserts.Free;
  end;
end;

procedure TtgdReportEditorForm.synmTemplateChange(Sender: TObject);
begin
  UpdateStatusPanel;
end;

procedure TtgdReportEditorForm.synmTemplateKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    InsertSelectedTreeNodeToTemplate();
  UpdateStatusPanel;
end;

procedure TtgdReportEditorForm.tvContextEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := False;
end;

procedure TtgdReportEditorForm.tvContextExpanding(Sender: TObject; Node:
  TTreeNode; var AllowExpansion: Boolean);
begin
  AllowExpansion := Node.HasChildren;

  if AllowExpansion and (Node.Count = 0) then
    LoadTreeNodes(Node, []);
end;

procedure TtgdReportEditorForm.tvContextMouseUp(Sender: TObject; Button:
  TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vNode: TTreeNode;
begin
  if Button in [mbRight, mbMiddle] then
  begin
    vNode := tvContext.GetNodeAt(X, Y);
    if vNode <> nil then
    begin
      vNode.Selected := True;
      InsertSelectedTreeNodeToTemplate();
    end;
  end;
end;

procedure TtgdReportEditorForm.UpdateStatusPanel;
begin
  statTempl.SimpleText := Format('Pos: %d, %d',
    [synmTemplate.CaretY, synmTemplate.CaretX]);
end;

end.

