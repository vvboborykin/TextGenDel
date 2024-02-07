program TextGenDelTest;

uses
  Forms,
  MainTestFormUnit in 'MainTestFormUnit.pas' {TestMainForm},
  tgdReportUnit in '..\Sources\tgdReportUnit.pas',
  tgdGeneratorUnit in '..\Sources\tgdGeneratorUnit.pas',
  tgdCollectionsUnit in '..\Sources\tgdCollectionsUnit.pas',
  tgdScriptEngineUnit in '..\Sources\tgdScriptEngineUnit.pas',
  tgdFastScriptEngineUnit in '..\Sources\tgdFastScriptEngineUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestMainForm, TestMainForm);
  Application.Run;
end.
