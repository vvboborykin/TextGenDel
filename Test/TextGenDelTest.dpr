program TextGenDelTest;

uses
  Forms,
  MainTestFormUnit in 'MainTestFormUnit.pas' {TestMainForm},
  tgdReportUnit in '..\Sources\tgdReportUnit.pas',
  tgdGeneratorUnit in '..\Sources\tgdGeneratorUnit.pas',
  tgdCollectionsUnit in '..\Sources\tgdCollectionsUnit.pas',
  tgdScriptEngineUnit in '..\Sources\tgdScriptEngineUnit.pas',
  tgdFastScriptEngineUnit in '..\Sources\tgdFastScriptEngineUnit.pas',
  tgdReportEditorFormUnit in '..\Sources\tgdReportEditorFormUnit.pas' {tgdReportEditorForm},
  tdgScriptElementsUnit in '..\Sources\tdgScriptElementsUnit.pas',
  dlgSearchText in '..\Sources\dlgSearchText.pas' {TextSearchDialog},
  dlgConfirmReplace in '..\Sources\dlgConfirmReplace.pas' {ConfirmReplaceDialog},
  dlgReplaceText in '..\Sources\dlgReplaceText.pas' {TextReplaceDialog},
  plgSearchHighlighter in '..\Sources\plgSearchHighlighter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestMainForm, TestMainForm);
  Application.Run;
end.
