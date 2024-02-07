unit tgdCollectionsUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils;

type
  TtgdNamedCollectionItem = class(TCollectionItem)
  private
    FName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetName(const Value: string);
  published
    property Name: string read FName write SetName;
  end;

  TtgdNamedCollection = class(TOwnedCollection)
  protected
    function GetItems(Index: Integer): TtgdNamedCollectionItem;
    procedure SetItems(Index: Integer; const Value: TtgdNamedCollectionItem);
  public
    function ContainsName(AName: string): Boolean;
    function FindName(AName: string): TtgdNamedCollectionItem;
    function IndexOfName(AName: string): Integer;
    function TryFindName(AName: string; out AItem: TtgdNamedCollectionItem): Boolean;
    property Items[Index: Integer]: TtgdNamedCollectionItem read GetItems write
      SetItems; default;
  end;

  TtgdReportContextItem = class(TtgdNamedCollectionItem)
  private
    FComponent: TComponent;
  protected
    procedure SetComponent(const Value: TComponent);
  published
    property Component: TComponent read FComponent write SetComponent;
  end;

  TtgdReportContext = class(TtgdNamedCollection)
  protected
    function GetItems(Index: Integer): TtgdReportContextItem;
    procedure SetItems(Index: Integer; const Value: TtgdReportContextItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add(AComponent: TComponent; AName: string = ''): TtgdReportContextItem;
    property Items[Index: Integer]: TtgdReportContextItem read GetItems write
      SetItems; default;
  end;

  TtgdReportVariable = class;

  TtgdGetReportVariableEvent = procedure(Sender: TtgdReportVariable; var Value:
    Variant) of object;

  TtgdSetReportVariableEvent = procedure(Sender: TtgdReportVariable; Value:
    Variant) of object;

  TtgdReportVariable = class(TtgdNamedCollectionItem)
  private
    FOnGetValue: TtgdGetReportVariableEvent;
    FOnSetValue: TtgdSetReportVariableEvent;
    FValue: Variant;
  protected
    procedure DoGetValue(AVarName: string; var Value: Variant);
    procedure DoSetValue(AVarName: string; Value: Variant);
    procedure SetValue(const Value: Variant);
  published
    property Value: Variant read FValue write SetValue;
    property OnGetValue: TtgdGetReportVariableEvent read FOnGetValue write FOnGetValue;
    property OnSetValue: TtgdSetReportVariableEvent read FOnSetValue write FOnSetValue;
  end;

  TtgdReportVariables = class(TtgdNamedCollection)
  protected
    function GetItems(Index: Integer): TtgdReportVariable;
    procedure SetItems(Index: Integer; const Value: TtgdReportVariable);
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TtgdReportVariable read GetItems write
      SetItems; default;
  end;

  TtgdReportFunction = class;

  TtgdReportFunctionExecuteEvent = function(Sender: TtgdReportFunction; AParams:
    array of Variant): Variant of object;

  TtgdReportFunction = class(TtgdNamedCollectionItem)
  private
    FDeclaration: string;
    FOnExecute: TtgdReportFunctionExecuteEvent;
  protected
    function DoExecute(AName: string; AParams: array of Variant): Variant;
    procedure SetDeclaration(const Value: string);
  published
    property Declaration: string read FDeclaration write SetDeclaration;
    property OnExecute: TtgdReportFunctionExecuteEvent read FOnExecute write FOnExecute;
  end;

  TtgdReportFunctions = class(TtgdNamedCollection)
  protected
    function GetItems(Index: Integer): TtgdReportFunction;
    procedure SetItems(Index: Integer; const Value: TtgdReportFunction);
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TtgdReportFunction read GetItems write
      SetItems; default;
  end;

implementation

function TtgdNamedCollectionItem.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TtgdNamedCollectionItem.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
  end;
end;

function TtgdNamedCollection.ContainsName(AName: string): Boolean;
begin
  Result := IndexOfName(AName) >= 0;
end;

function TtgdNamedCollection.FindName(AName: string): TtgdNamedCollectionItem;
var
  vIndex: Integer;
begin
  Result := nil;
  vIndex := IndexOfName(AName);
  if vIndex >= 0 then
    Result := Items[vIndex];
end;

function TtgdNamedCollection.GetItems(Index: Integer): TtgdNamedCollectionItem;
begin
  Result := TtgdReportContextItem(inherited Items[Index]);
end;

function TtgdNamedCollection.IndexOfName(AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if AnsiSameText(AName, Items[I].Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TtgdNamedCollection.SetItems(Index: Integer; const Value:
  TtgdNamedCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TtgdNamedCollection.TryFindName(AName: string; out AItem:
  TtgdNamedCollectionItem): Boolean;
begin
  AItem := FindName(AName);
  Result := AItem <> nil;
end;

procedure TtgdReportContextItem.SetComponent(const Value: TComponent);
begin
  if FComponent <> Value then
  begin
    FComponent := Value;
    if (Name = '') and (FComponent <> nil) then
      Name := FComponent.Name;
  end;
end;

constructor TtgdReportContext.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtgdReportContextItem);
end;

function TtgdReportContext.Add(AComponent: TComponent; AName: string = ''):
  TtgdReportContextItem;
begin
  Result := TtgdReportContextItem.Create(Self);
  Result.Component := AComponent;
  if AName <> '' then
    Result.Name := AName;
end;

function TtgdReportContext.GetItems(Index: Integer): TtgdReportContextItem;
begin
  Result := TtgdReportContextItem(inherited Items[Index]);
end;

procedure TtgdReportContext.SetItems(Index: Integer; const Value: TtgdReportContextItem);
begin
  inherited Items[Index] := Value;
end;

constructor TtgdReportVariables.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtgdReportContextItem);
end;

function TtgdReportVariables.GetItems(Index: Integer): TtgdReportVariable;
begin
  Result := TtgdReportVariable(inherited Items[Index]);
end;

procedure TtgdReportVariables.SetItems(Index: Integer; const Value: TtgdReportVariable);
begin
  inherited Items[Index] := Value;
end;

procedure TtgdReportVariable.DoGetValue(AVarName: string; var Value: Variant);
begin
  if Assigned(FOnGetValue) then
    FOnGetValue(Self, AVarName, Value);
end;

procedure TtgdReportVariable.DoSetValue(AVarName: string; Value: Variant);
begin
  if Assigned(FOnSetValue) then
    FOnSetValue(Self, AVarName, Value);
end;

procedure TtgdReportVariable.SetValue(const Value: Variant);
begin
  if FValue <> Value then
  begin
    FValue := Value;
  end;
end;

function TtgdReportFunction.DoExecute(AName: string; AParams: array of Variant): Variant;
begin
  if Assigned(FOnExecute) then
    Result := FOnExecute(Self, AName, AParams)
  else
    Result := Unassigned;
end;

procedure TtgdReportFunction.SetDeclaration(const Value: string);
begin
  if FDeclaration <> Value then
  begin
    FDeclaration := Value;
  end;
end;

constructor TtgdReportFunctions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtgdReportContextItem);
end;

function TtgdReportFunctions.GetItems(Index: Integer): TtgdReportFunction;
begin
  Result := TtgdReportFunction(inherited Items[Index]);
end;

procedure TtgdReportFunctions.SetItems(Index: Integer; const Value: TtgdReportFunction);
begin
  inherited Items[Index] := Value;
end;

end.

