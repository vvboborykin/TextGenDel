unit tgdReportEditorFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEditHighlighter, SynHighlighterPas, SynEdit, SynMemo,
  StdCtrls, ImgList, ComCtrls, ExtCtrls, SynEditMiscClasses, SynEditSearch,
  SynCompletionProposal, ActnList, SynHighlighterMulti, Buttons,
  SynHighlighterXML;

type
  TtgdReportEditorForm = class(TForm)
    synpsyn1: TSynPasSyn;
    syntcmplt1: TSynAutoComplete;
    syndtsrch1: TSynEditSearch;
    pnlClient: TPanel;
    spl1: TSplitter;
    tv1: TTreeView;
    pnl1: TPanel;
    synm1: TSynMemo;
    il16: TImageList;
    actlst1: TActionList;
    synmltsyn1: TSynMultiSyn;
    synxmlsyn2: TSynXMLSyn;
    actSave: TAction;
    actCancel: TAction;
    actLoad: TAction;
    actOk: TAction;
    btnSave1: TBitBtn;
    btnLoadFromFile: TBitBtn;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnValidate: TBitBtn;
    actValidate: TAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  tgdReportEditorForm: TtgdReportEditorForm;

implementation

{$R *.dfm}

end.
