unit tgdReportEditorUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, DesignIntf,
  DesignEditors;

type
  TtgdReportEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  tgdReportUnit, tgdReportEditorFormUnit;

procedure Register;
begin
  RegisterComponentEditor(TtgdReport, TtgdReportEditor);
end;

{ TtgdReportEditor }

procedure TtgdReportEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  if (Component <> nil) and (Component is TtgdReport) then
  begin
    TtgdReportEditorForm.ShowEditor(Component as TtgdReport);
  end;
end;

function TtgdReportEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit report';
end;

function TtgdReportEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
