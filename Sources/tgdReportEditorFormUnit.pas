unit tgdReportEditorFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, SynMemo, StdCtrls,
  ImgList, ComCtrls, ExtCtrls, SynEditMiscClasses, SynEditSearch,
  SynCompletionProposal, ActnList, SynHighlighterMulti, Buttons,
  tgdScriptEngineUnit, SynHighlighterXML, SynHighlighterJSON, tgdReportUnit,
  DialogsX;

type
  TtgdReportEditorForm = class(TForm)
    synpPascalSyntax: TSynPasSyn;
    syaAutoComplete: TSynAutoComplete;
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
    dlgLoad: TFileOpenDialog;
    dlgSave: TFileSaveDialog;
    procedure actLoadExecute(Sender: TObject);
    procedure actOkExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actValidateExecute(Sender: TObject);
  private
    FReport: TtgdReport;
    FEngine: ItgdScriptEngine;
    procedure AddChildNode(AParentNode: TTreeNode; AScriptElement:
        TtgdScriptElement);
    function GetImageIndexOfScriptElement(AScriptElement: TtgdScriptElement):
        Integer;
    procedure Init(AReport: TtgdReport);
    procedure LoadTopTreeNodes;
    procedure LoadTreeNodes(ANode: TTreeNode);
  public
    class function ShowEditor(AReport: TtgdReport): Boolean;
  end;

implementation

resourcestring
  STemplateIsValid = 'Template is valid';
  SAReportIsNil = 'AReport is nil';

{$R *.dfm}

procedure TtgdReportEditorForm.actLoadExecute(Sender: TObject);
begin
  if dlgLoad.Execute then
  begin
    synmTemplate.Lines.LoadFromFile(dlgLoad.FileName);
    if AnsiSameText('.xml', ExtractFileExt(dlgLoad.FileName)) then
      symMain.DefaultHighlighter := syxXmlSyntax
    else if AnsiSameText('.json', ExtractFileExt(dlgLoad.FileName)) then
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
  if dlgSave.Execute then
    synmTemplate.Lines.SaveToFile(dlgSave.FileName);
end;

procedure TtgdReportEditorForm.actValidateExecute(Sender: TObject);
var
  vScript: TStringList;
  vScriptEngine: ItgdScriptEngine;
begin
  vScript := TStringList.Create();
  try
    vScriptEngine := CreateScriptEngine();
    vScriptEngine.Init(FReport);
    vScriptEngine.ConvertTemplateToScript(vScript);
    vScriptEngine.ValidateScript(vScript);
    ShowMessage(STemplateIsValid);
  finally
    vScript.Free;
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
end;

function TtgdReportEditorForm.GetImageIndexOfScriptElement(AScriptElement:
    TtgdScriptElement): Integer;
begin
  Result := 0;
  if AScriptElement is TtgdScriptVariable then
    Result := 1
  else
  if AScriptElement is TtgdScriptClass then
    Result := 1
  else
  if AScriptElement is TtgdScriptFunction then
    Result := 1
  else
  if AScriptElement is TtgdScriptProperty then
    Result := 1
  else
  if AScriptElement is TtgdScriptEvent then
    Result := 1
  else
end;

procedure TtgdReportEditorForm.Init(AReport: TtgdReport);
begin
  FReport := AReport;
  FEngine := CreateScriptEngine();
  synmTemplate.Lines.Assign(AReport.TemplateLines);
  LoadTopTreeNodes();
end;

procedure TtgdReportEditorForm.LoadTopTreeNodes;
begin
  LoadTreeNodes(nil);
end;

procedure TtgdReportEditorForm.LoadTreeNodes(ANode: TTreeNode);
var
  I: Integer;
  vChildren: TtgdScriptElementList;
  vParentObject: TObject;
begin
  vChildren := TtgdScriptElementList.Create(False);
  try
    vParentObject := nil;
    if ANode <> nil then
      vParentObject := TObject(ANode.Data);
    FEngine.GetChildrenScriptElements(vParentObject, vChildren);
    for I := 0 to vChildren.Count-1 do
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

end.

