program FastScriptDemo;

uses
  Forms,
  FastScriptDemoMainFormUnit in 'FastScriptDemoMainFormUnit.pas' {FastScriptDemoMainForm},
  DemoBaseMainFormUnit in '..\Base\DemoBaseMainFormUnit.pas' {DemoBaseMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFastScriptDemoMainForm, FastScriptDemoMainForm);
  Application.Run;
end.
