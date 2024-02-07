program TextGenDelTest;

uses
  Forms,
  MainTestFormUnit in 'MainTestFormUnit.pas' {TestMainForm},
  tgdReportUnit in '..\Sources\tgdReportUnit.pas',
  tgdGeneratorUnit in '..\Sources\tgdGeneratorUnit.pas',
  tgdCollectionsUnit in '..\Sources\tgdCollectionsUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTestMainForm, TestMainForm);
  Application.Run;
end.
