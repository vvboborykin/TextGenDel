program Demo;

uses
  Forms,
  MainDemoFormUnit in 'MainDemoFormUnit.pas' {MainDemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainDemoForm, MainDemoForm);
  Application.Run;
end.
