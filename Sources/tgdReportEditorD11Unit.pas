unit tgdReportEditorD11Unit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, DesignIntf,
  DesignEditors;

type
  TtgdReportEditorD11 = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

uses
  tgdReportUnit, tgdReportEditorFormD11Unit;

procedure Register;
begin
  RegisterComponentEditor(TtgdReport, TtgdReportEditorD11);
end;

{ TtgdReportEditorD11 }

procedure TtgdReportEditorD11.ExecuteVerb(Index: Integer);
begin
  inherited;
  if (Component <> nil) and (Component is TtgdReport) then
  begin
    TtgdReportEditorFormD11.ShowEditor(Component as TtgdReport);
  end;
end;

function TtgdReportEditorD11.GetVerb(Index: Integer): string;
begin
  Result := 'Edit report';
end;

function TtgdReportEditorD11.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
