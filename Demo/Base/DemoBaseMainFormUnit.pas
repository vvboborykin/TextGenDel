{*******************************************************
* Project: FastScriptDemo
* Unit: DemoBaseMainFormUnit.pas
* Description: Base main form for demo applications
* 
* Created: 04.03.2024 16:03:39
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit DemoBaseMainFormUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ActnList, StdCtrls, tgdReportUnit, DB, DBClient;

type
  /// <summary>TDemoBaseMainForm
  /// Base main form for demo applications
  /// </summary>
  TDemoBaseMainForm = class(TForm)
    cdsData: TClientDataSet;
    cdsDataRecordId: TIntegerField;
    cdsDataName: TStringField;
    cdsDataCode: TStringField;
    cdsDataIsValid: TBooleanField;
    tgrReport: TtgdReport;
    btnGenerateXml: TButton;
    btnEditTemplate: TButton;
    aclMain: TActionList;
    actEditTemplate: TAction;
    btnExit: TButton;
    actGenerateXml: TAction;
    actExit: TAction;
    mmoResult: TMemo;
    btnGenerateScript: TButton;
    actGenerateScript: TAction;
    procedure FormCreate(Sender: TObject);
    procedure actEditTemplateExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actGenerateScriptExecute(Sender: TObject);
    procedure actGenerateXmlExecute(Sender: TObject);
  private
    procedure AppendDataRecord(AId: Integer; ACode, AName: String; AIsValid:
        Boolean);
    procedure AppendDataRecords;
  protected
    procedure CreateData; virtual;
    procedure EditTemplate; virtual;
    procedure GenerateScript; virtual;
    procedure GenerateXml; virtual;
  public
  end;


implementation

uses
  tgdReportEditorFormUnit;

{$R *.dfm}

procedure TDemoBaseMainForm.FormCreate(Sender: TObject);
begin
  CreateData();
end;

procedure TDemoBaseMainForm.actEditTemplateExecute(Sender: TObject);
begin
  EditTemplate();
end;

procedure TDemoBaseMainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TDemoBaseMainForm.actGenerateScriptExecute(Sender: TObject);
begin
  GenerateScript();
end;

procedure TDemoBaseMainForm.actGenerateXmlExecute(Sender: TObject);
begin
  GenerateXml();
end;

procedure TDemoBaseMainForm.AppendDataRecord(AId: Integer; ACode, AName:
    String; AIsValid: Boolean);
begin
  cdsData.Append;
  cdsDataRecordId.Value := AId;
  cdsDataCode.AsString := ACode;
  cdsDataName.AsString := AName;
  cdsDataIsValid.Value := AIsValid;
  cdsData.Post;
end;

procedure TDemoBaseMainForm.AppendDataRecords;
begin
  AppendDataRecord(1, 'Code 001', 'First valid record', True);
  AppendDataRecord(2, 'Code 002', 'Second valid record', True);
  AppendDataRecord(3, 'Code 003', 'Third invalid record', False);
end;

procedure TDemoBaseMainForm.CreateData;
begin
  AppendDataRecords;
end;

procedure TDemoBaseMainForm.EditTemplate;
begin
  TtgdReportEditorForm.ShowEditor(tgrReport);
end;

procedure TDemoBaseMainForm.GenerateScript;
begin
  mmoResult.Lines.Clear;
  tgrReport.GenerateScript(mmoResult.Lines);
end;

procedure TDemoBaseMainForm.GenerateXml;
begin
  mmoResult.Lines.Clear;
  tgrReport.GenerateText(mmoResult.Lines);
end;

end.
