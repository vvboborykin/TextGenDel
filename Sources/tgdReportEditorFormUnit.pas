unit tgdReportEditorFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, SynMemo, StdCtrls,
  ImgList, ComCtrls, ExtCtrls, SynEditMiscClasses, SynEditSearch,
  SynCompletionProposal, ActnList, SynHighlighterMulti, Buttons,
  SynHighlighterXML, SynHighlighterJSON, tgdReportUnit,
  tgdScriptElementsUnit, DialogsX, frxUnicodeUtils, SynUnicode,
  SynHighlighterGeneral, SynHighlighterIni, SynHighlighterSQL,
  SynHighlighterHtml, Menus, StdActns;

type
  TtgdReportEditorForm = class(TForm)
    synPascalSyntax: TSynPasSyn;
    synsSearch: TSynEditSearch;
    pnlClient: TPanel;
    splVert: TSplitter;
    tvContext: TTreeView;
    pnlBttons: TPanel;
    synmTemplate: TSynMemo;
    ilTree: TImageList;
    aclMain: TActionList;
    symMain: TSynMultiSyn;
    synXmlSyntax: TSynXMLSyn;
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
    synJsonSyntax: TSynJSONSyn;
    dlgLoadTemplate: TFileOpenDialog;
    dlgSaveTemplate: TFileSaveDialog;
    sypComplete: TSynCompletionProposal;
    actExecuteReport: TAction;
    btnExecuteReport: TBitBtn;
    dlgSaveReport: TFileSaveDialog;
    statTempl: TStatusBar;
    pnlTemplate: TPanel;
    synText: TSynGeneralSyn;
    synSQLSyntax: TSynSQLSyn;
    synIniSyntax: TSynIniSyn;
    synHTMLSyntax: TSynHTMLSyn;
    aclEdit: TActionList;
    ActionFileOpen: TAction;
    ActionSearch: TAction;
    ActionSearchNext: TAction;
    ActionSearchPrev: TAction;
    ActionSearchReplace: TAction;
    ilEdit: TImageList;
    dlgOpenFile: TOpenDialog;
    synMacro: TSynGeneralSyn;
    pmEdit: TPopupMenu;
    Find1: TMenuItem;
    Findnext1: TMenuItem;
    Findprevious1: TMenuItem;
    Replace1: TMenuItem;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    EditDelete1: TEditDelete;
    N1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Delete1: TMenuItem;
    Paste1: TMenuItem;
    SelectAll1: TMenuItem;
    Undo1: TMenuItem;
    cbbTemplateSyntax: TComboBox;
    procedure actCancelExecute(Sender: TObject);
    procedure actExecuteReportExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSearchNextExecute(Sender: TObject);
    procedure ActionSearchPrevExecute(Sender: TObject);
    procedure ActionSearchReplaceExecute(Sender: TObject);
    procedure ActionSearchReplaceUpdate(Sender: TObject);
    procedure actLoadExecute(Sender: TObject);
    procedure actOkExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSearchUpdate(Sender: TObject);
    procedure actValidateExecute(Sender: TObject);
    procedure cbbTemplateSyntaxChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sypCompleteExecute(Kind: SynCompletionType; Sender:
      TObject; var CurrentInput: WideString; var x, y: Integer; var CanExecute: Boolean);
    procedure SynEditorReplaceText(Sender: TObject; const ASearch, AReplace:
        UnicodeString; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure synmTemplateChange(Sender: TObject);
    procedure synmTemplateDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure synmTemplateDragOver(Sender, Source: TObject; X, Y: Integer; State:
        TDragState; var Accept: Boolean);
    procedure synmTemplateKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tvContextEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure tvContextExpanding(Sender: TObject; Node: TTreeNode; var
      AllowExpansion: Boolean);
    procedure tvContextMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
  private
    FModifiedFlag: Boolean;
    FReport: TtgdReport;
    FEngine: ItgdScriptEngine;
    FInitCompleted: Boolean;
    FSearchFromCaret: Boolean;
    procedure AddChildNode(AParentNode: TTreeNode; AScriptElement: TtgdScriptElement);
    procedure AddImagesToProposalList(AProposalList: TUnicodeStrings; AItems:
        TStrings);
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure ExecuteReportForCurrentTemplate;
    function FileNameMatchesHighliterMask(vFileName: string; vHighliter:
        TSynCustomHighlighter): Boolean;
    procedure FillTemplateSyntaxComboBoxList;
    function GetHighlighterForFileName(AFileName: string): TSynCustomHighlighter;
    function GetImageIndexOfScriptElement(AScriptElement: TtgdScriptElement): Integer;
    function GetNodeChainText(ANode: TTreeNode): string;
    function GetRandomName: string;
    function GetScriptElementClassImage(AClass: TClass): Integer;
    function GetTextLeftOfCaret: string;
    procedure Init(AReport: TtgdReport);
    procedure InsertIntoTemplateCaretPos(AText: string);
    procedure InsertSelectedTreeNodeToTemplate;
    procedure LoadAutoCompleteList(ANestLevel: Integer = 4);
    procedure LoadLines;
    procedure LoadTemplateFromFile;
    procedure LoadTemplateSyntax;
    procedure LoadTopTreeNodes;
    procedure LoadTreeNodes(ANode: TTreeNode; AScriptElementClass: array of TClass);
    procedure MarkModified;
    procedure SaveChangesToReport;
    procedure SaveTemplateToFile;
    procedure SelectReportTemplateSyntaxInComboBox;
    procedure SetTemplateSyntax(AHighliter: TSynCustomHighlighter);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure UpdateStatusPanel;
    procedure ValidateTemplate;
  public
    class function ShowEditor(AReport: TtgdReport): Boolean;
  end;


implementation

uses
  StrUtils, ComObj, shellapi, Mask, Masks,
  dlgSearchText, dlgReplaceText, dlgConfirmReplace, plgSearchHighlighter,
  SynEditTypes, SynEditMiscProcs, DesignIntf;

  // options - to be saved to the registry
var
  gbSearchBackwards: Boolean;
  gbSearchCaseSensitive: Boolean;
  gbSearchFromCaret: Boolean;
  gbSearchSelectionOnly: Boolean;
  gbSearchTextAtCaret: Boolean;
  gbSearchWholeWords: Boolean;
  gbSearchRegex: Boolean;

  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

resourcestring
  STheTemplateHasBeenChangedSaveCha = 'The template has been changed. Save changes ?';
  STextNotFound = 'Text not found';


resourcestring
  STemplateIsValid = 'Template is valid';
  SAReportIsNil = 'AReport is nil';

{$R *.dfm}



type
  TStringArray = array of string;

function SplitString(AString: String; Delimiter: Char): TStringArray;
var
  I: Integer;
  vList: TStringList;
begin
  vList := TStringList.Create;
  try
    vList.Delimiter := Delimiter;
    vList.DelimitedText := AString;
    SetLength(Result, vList.Count);
    for I := 0 to vList.Count-1 do
      Result[I] := vList[I];
  finally
    vList.Free;
  end;
end;


procedure TtgdReportEditorForm.actCancelExecute(Sender: TObject);
begin
  Close;
end;

procedure TtgdReportEditorForm.actExecuteReportExecute(Sender: TObject);
begin
  ExecuteReportForCurrentTemplate;
end;

{ event handler }

procedure TtgdReportEditorForm.ActionFileOpenExecute(Sender: TObject);
begin
  if dlgOpenFile.Execute then begin
    synmTemplate.Lines.LoadFromFile(dlgOpenFile.FileName);
    synmTemplate.ReadOnly := ofReadOnly in dlgOpenFile.Options;
  end;
end;

procedure TtgdReportEditorForm.ActionSearchExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(FALSE);
end;

procedure TtgdReportEditorForm.ActionSearchNextExecute(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, FALSE);
end;

procedure TtgdReportEditorForm.ActionSearchPrevExecute(Sender: TObject);
begin
  DoSearchReplaceText(FALSE, TRUE);
end;

procedure TtgdReportEditorForm.ActionSearchReplaceExecute(Sender: TObject);
begin
  ShowSearchReplaceDialog(TRUE);
end;

procedure TtgdReportEditorForm.ActionSearchReplaceUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (gsSearchText <> '')
    and not synmTemplate.ReadOnly;
end;

procedure TtgdReportEditorForm.actLoadExecute(Sender: TObject);
begin
  LoadTemplateFromFile;
end;

procedure TtgdReportEditorForm.actOkExecute(Sender: TObject);
begin
  SaveChangesToReport;
end;

procedure TtgdReportEditorForm.actSaveExecute(Sender: TObject);
begin
  SaveTemplateToFile;
end;

procedure TtgdReportEditorForm.actSearchUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := gsSearchText <> '';
end;

procedure TtgdReportEditorForm.actValidateExecute(Sender: TObject);
begin
  ValidateTemplate;
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

procedure TtgdReportEditorForm.cbbTemplateSyntaxChange(Sender: TObject);
begin
  SetTemplateSyntax(cbbTemplateSyntax.Items.Objects[cbbTemplateSyntax.ItemIndex] as TSynCustomHighlighter);
end;

procedure TtgdReportEditorForm.DoSearchReplaceText(AReplace: Boolean;
    ABackwards: Boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not FSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  synmTemplate.SearchEngine := synsSearch;
  if synmTemplate.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      synmTemplate.BlockEnd := synmTemplate.BlockBegin
    else
      synmTemplate.BlockBegin := synmTemplate.BlockEnd;
    synmTemplate.CaretXY := synmTemplate.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

procedure TtgdReportEditorForm.ExecuteReportForCurrentTemplate;
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

function TtgdReportEditorForm.FileNameMatchesHighliterMask(vFileName: string;
    vHighliter: TSynCustomHighlighter): Boolean;
var
  I: Integer;
  J: Integer;
  vFilters: TStringArray;
  vMasks: TStringArray;
begin
  Result := False;
  vFilters := SplitString(vHighliter.DefaultFilter, '|');
  for I := Low(vFilters) to High(vFilters) do
  begin
    if (I mod 2) = 1 then
    begin
      vMasks := SplitString(vFilters[I], ';');
      for J := Low(vMasks) to High(vMasks) do
      begin
        if MatchesMask(vFileName, vMasks[J]) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
    if Result then
      Break;
  end;
end;

procedure TtgdReportEditorForm.FillTemplateSyntaxComboBoxList;
var
  I: Integer;
  vSyntaxName: string;
begin
  for I := 0 to ComponentCount-1 do
  begin
    if Components[I] is TSynCustomHighlighter then
    begin
      vSyntaxName := (Components[I] as TSynCustomHighlighter).DefaultFilter;
      if vSyntaxName <> '' then
        cbbTemplateSyntax.Items.AddObject(vSyntaxName, Components[I]);
    end;
  end;
end;

procedure TtgdReportEditorForm.FormCloseQuery(Sender: TObject; var CanClose:
    Boolean);
var
  vAnswer: Integer;
begin
  if FModifiedFlag and not IsPositiveResult(ModalResult) then
  begin
    vAnswer := MessageDlg(STheTemplateHasBeenChangedSaveCha, mtConfirmation, mbYesNoCancel, 0);
    case vAnswer of
      mrYes: SaveChangesToReport();
      mrCancel: CanClose := False;
      mrNone: CanClose := True;
    end;
  end;
end;

function TtgdReportEditorForm.GetHighlighterForFileName(AFileName: string):
    TSynCustomHighlighter;
var
  I: Integer;
  vFileName: string;
  vHighliter: TSynCustomHighlighter;
begin
  Result := nil;
  vFileName := ExtractFileName(AFileName);

  for I := 0 to ComponentCount-1 do
    if Components[I] is TSynCustomHighlighter then
    begin
      vHighliter := Components[I] as TSynCustomHighlighter;
      if FileNameMatchesHighliterMask(vFileName, vHighliter) then
      begin
        Result := vHighliter;
        Break;
      end;
    end;

  if Result = nil then
    Result := synXmlSyntax;
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
  FInitCompleted := False;
  
  FReport := AReport;
  FEngine := AReport.CreateScriptEngine;
  FEngine.Init(FReport);

  synmTemplate.Lines.Assign(AReport.TemplateLines);
  LoadTopTreeNodes();
  LoadAutoCompleteList();
  LoadTemplateSyntax();
  UpdateStatusPanel();

  FInitCompleted := True;
end;

procedure TtgdReportEditorForm.InsertIntoTemplateCaretPos(AText: string);
var
  vText: string;
begin
  if synmTemplate.Lines.Count = 0 then
    synmTemplate.Lines.Add(AText)
  else
  begin
    vText := synmTemplate.Lines[synmTemplate.CaretY - 1];
    Insert(AText, vText, synmTemplate.CaretX);
    synmTemplate.Lines[synmTemplate.CaretY - 1] := vText;
    synmTemplate.CaretX := synmTemplate.CaretX + 3;
  end;
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

procedure TtgdReportEditorForm.LoadLines;
var
  vLines: TStrings;
begin
  vLines := TStringList.Create;
  try
    vLines.LoadFromFile(dlgLoadTemplate.FileName);
    synmTemplate.Lines.Clear;
    synmTemplate.Lines.AddStrings(vLines);
  finally
    vLines.Free;
  end;
end;

procedure TtgdReportEditorForm.LoadTemplateFromFile;
begin
  if dlgLoadTemplate.Execute then
  begin
    LoadLines;
    SetTemplateSyntax(GetHighlighterForFileName(dlgLoadTemplate.FileName));
  end;
end;

procedure TtgdReportEditorForm.LoadTemplateSyntax;
begin
  FillTemplateSyntaxComboBoxList();
  SelectReportTemplateSyntaxInComboBox();
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

procedure TtgdReportEditorForm.MarkModified;
begin
  if FInitCompleted and not FModifiedFlag then
  begin
    FModifiedFlag := True;
    Caption := Caption + ' *';
  end;
end;

procedure TtgdReportEditorForm.SaveChangesToReport;
begin
  FReport.TemplateLines.Assign(synmTemplate.Lines);
  FReport.SyntaxName := cbbTemplateSyntax.Text;
end;

procedure TtgdReportEditorForm.SaveTemplateToFile;
var
  vLines: TStrings;
  vText: string;
begin
  if dlgSaveTemplate.Execute then
  begin
    vLines := TStringList.Create;
    try
      vText := synmTemplate.Lines.Text;
      vLines.Text := vText;
      vLines.SaveToFile(dlgSaveTemplate.FileName);
    finally
      vLines.Free;
    end;
  end;
end;

procedure TtgdReportEditorForm.SelectReportTemplateSyntaxInComboBox;
var
  vIndex: Integer;
  vSyntaxName: string;
begin
  vSyntaxName := FReport.SyntaxName;
  vIndex := cbbTemplateSyntax.Items.IndexOf(vSyntaxName);
  if vIndex < 0 then
    vIndex := cbbTemplateSyntax.Items.IndexOfObject(synXmlSyntax);
  cbbTemplateSyntax.ItemIndex := vIndex;
  SetTemplateSyntax(cbbTemplateSyntax.Items.Objects[vIndex] as TSynCustomHighlighter);
end;

procedure TtgdReportEditorForm.SetTemplateSyntax(AHighliter:
    TSynCustomHighlighter);
begin
  symMain.DefaultHighlighter := AHighliter;
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

procedure TtgdReportEditorForm.ShowSearchReplaceDialog(AReplace: Boolean);
var
  dlg: TTextSearchDialog;
begin
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then begin
      // if something is selected search for that text
      if synmTemplate.SelAvail and (synmTemplate.BlockBegin.Line = synmTemplate.BlockEnd.Line)
      then
        SearchText := synmTemplate.SelText
      else
        SearchText := synmTemplate.GetWordAtRowCol(synmTemplate.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then with dlg as TTextReplaceDialog do begin
        gsReplaceText := ReplaceText;
        gsReplaceTextHistory := ReplaceTextHistory;
      end;
      FSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        FSearchFromCaret := TRUE;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TtgdReportEditorForm.sypCompleteExecute(Kind:
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

      sypComplete.ItemList.Clear;
      sypComplete.ItemList.AddStrings(vItems);
      AddImagesToProposalList(sypComplete.ItemList, vItems);

      sypComplete.InsertList.Clear;
      sypComplete.InsertList.AddStrings(vInserts);
    finally
      vItems.Free;
    end;
  finally
    vInserts.Free;
  end;
end;

procedure TtgdReportEditorForm.SynEditorReplaceText(Sender: TObject; const
    ASearch, AReplace: UnicodeString; Line, Column: Integer; var Action:
    TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else begin
    APos := synmTemplate.ClientToScreen(
      synmTemplate.RowColumnToPixels(
      synmTemplate.BufferToDisplayPos(
        BufferCoord(Column, Line) ) ) );
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if ConfirmReplaceDialog = nil then
      ConfirmReplaceDialog := TConfirmReplaceDialog.Create(Application);
    ConfirmReplaceDialog.PrepareShow(EditRect, APos.X, APos.Y,
      APos.Y + synmTemplate.LineHeight, ASearch);
    case ConfirmReplaceDialog.ShowModal of
      mrYes: Action := raReplace;
      mrYesToAll: Action := raReplaceAll;
      mrNo: Action := raSkip;
      else Action := raCancel;
    end;
  end;
end;

procedure TtgdReportEditorForm.synmTemplateChange(Sender: TObject);
begin
  MarkModified;
  UpdateStatusPanel;
end;

procedure TtgdReportEditorForm.synmTemplateDragDrop(Sender, Source: TObject; X,
    Y: Integer);
begin
  InsertSelectedTreeNodeToTemplate();
end;

procedure TtgdReportEditorForm.synmTemplateDragOver(Sender, Source: TObject; X,
    Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = tvContext;
end;

procedure TtgdReportEditorForm.synmTemplateKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    InsertSelectedTreeNodeToTemplate();
  UpdateStatusPanel;
  if (Key = 219) then
  begin
    if (ssCtrl in Shift) and (ssShift in Shift) then
      InsertIntoTemplateCaretPos(FReport.CodeBeginMarker + '  ' + FReport.CodeEndMarker)
    else
    if (ssCtrl in Shift) and (ssAlt in Shift) then
      InsertIntoTemplateCaretPos(FReport.MacroBeginMarker + '  ' + FReport.MacroEndMarker)
  end;
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
  statTempl.Panels[0].Text := Format('Pos: %d, %d',
    [synmTemplate.CaretY, synmTemplate.CaretX]);
end;

procedure TtgdReportEditorForm.ValidateTemplate;
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
    vScriptEngine := FReport.CreateScriptEngine;
    vScriptEngine.Init(FReport);
    vScriptEngine.ConvertTemplateToScript(vScript, vLines);
    vScriptEngine.ValidateScript(vScript);
    ShowMessage(STemplateIsValid);
  finally
    vScript.Free;
    vLines.Free;
  end;
end;

end.

