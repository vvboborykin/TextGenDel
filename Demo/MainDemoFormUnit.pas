unit MainDemoFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, fs_idbrtti, fs_iinirtti, fs_iextctrlsrtti, fs_iformsrtti,
  fs_iclassesrtti, tgdReportUnit, DB, DBClient, StdCtrls;

type
  TMainDemoForm = class(TForm)
    tgrReport: TtgdReport;
    fsClassesRTTI1: TfsClassesRTTI;
    fsFormsRTTI1: TfsFormsRTTI;
    fsExtCtrlsRTTI1: TfsExtCtrlsRTTI;
    fsIniRTTI1: TfsIniRTTI;
    fsDBRTTI1: TfsDBRTTI;
    cdsData: TClientDataSet;
    cdsDataRecordId: TIntegerField;
    cdsDataName: TStringField;
    cdsDataCode: TStringField;
    cdsDataIsValid: TBooleanField;
    btnGenerateXml: TButton;
    procedure btnGenerateXmlClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AppendDataRecord(AId: Integer; ACode, AName: String; AIsValid:
        Boolean);
    procedure AppendDataRecords;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainDemoForm: TMainDemoForm;

implementation

{$R *.dfm}

procedure TMainDemoForm.AppendDataRecord(AId: Integer; ACode, AName: String;
    AIsValid: Boolean);
begin
  cdsData.Append;
  cdsDataRecordId.Value := AId;
  cdsDataCode.AsString := ACode;
  cdsDataName.AsString := AName;
  cdsDataIsValid.Value := AIsValid;
  cdsData.Post;
end;

procedure TMainDemoForm.AppendDataRecords;
begin
  AppendDataRecord(1, 'Code 001', 'First valid record', True);
  AppendDataRecord(2, 'Code 002', 'Second valid record', True);
  AppendDataRecord(3, 'Code 003', 'Third invalid record', False);
end;

procedure TMainDemoForm.btnGenerateXmlClick(Sender: TObject);
var
  vSaveDialog: TSaveDialog;
  vXmlLines: TStrings;
begin
  vSaveDialog := TSaveDialog.Create(Self);
  vXmlLines := TStringList.Create;
  try
    tgrReport.GenerateText(vXmlLines);
    if vSaveDialog.Execute then
      vXmlLines.SaveToFile(vSaveDialog.FileName);
  finally
    vXmlLines.Free;
    vSaveDialog.Free;
  end;
end;

procedure TMainDemoForm.FormCreate(Sender: TObject);
begin
  AppendDataRecords;
end;

end.
