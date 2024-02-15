{*******************************************************
* Project: TextGenDelTest
* Unit: tgdCollectionsUnit.pas
* Description: Report collections
* 
* Created: 08.02.2024 10:28:54
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdCollectionsUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils;

type
  /// <summary>TtgdNamedCollectionItem
  /// Base collection item having a name
  /// </summary>
  TtgdNamedCollectionItem = class(TCollectionItem)
  private
    FName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetName(const Value: string);
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>TtgdNamedCollectionItem.Name
    /// Item name
    /// </summary>
    /// type:string
    property Name: string read FName write SetName;
  end;

  /// <summary>TtgdNamedCollection
  /// Base collection of named items
  /// </summary>
  TtgdNamedCollection = class(TOwnedCollection)
  protected
    function GetItems(Index: Integer): TtgdNamedCollectionItem;
    function GetValues(Name: String): TtgdNamedCollectionItem;
    procedure SetItems(Index: Integer; const Value: TtgdNamedCollectionItem);
  public
    procedure Assign(ASource: TPersistent); override;
    /// <summary>TtgdNamedCollection.ContainsName Is collection contains item having
    /// specified name
    /// </summary>
    /// <returns> Boolean
    /// </returns>
    /// <param name="AName"> (string) Item`s name to find</param>
    function ContainsName(AName: string): Boolean;
    /// <summary>TtgdNamedCollection.FindName
    /// Find item having specified name
    /// </summary>
    /// <returns> TtgdNamedCollectionItem
    /// nil if not found
    /// </returns>
    /// <param name="AName"> (string) The name of item to find</param>
    function FindName(AName: string): TtgdNamedCollectionItem;
    /// <summary>TtgdNamedCollection.IndexOfName
    /// The index of item having specified name in collection
    /// </summary>
    /// <returns> Integer
    /// -1 if not found
    /// </returns>
    /// <param name="AName"> (string) Name of item to find</param>
    /// <param name="APartialSearch"> (Boolean) </param>
    function IndexOfName(AName: string; APartialSearch: Boolean = False): Integer;
    /// <summary>TtgdNamedCollection.TryFindName
    /// Try to find item having specified name
    /// </summary>
    /// <returns> Boolean
    /// True is found, otherwise False
    /// </returns>
    /// <param name="AName"> (string) Name of item to find</param>
    /// <param name="AItem"> (TtgdNamedCollectionItem) Found item or nil if not
    /// found</param>
    function TryFindName(AName: string; out AItem: TtgdNamedCollectionItem): Boolean;
    /// <summary>TtgdNamedCollection.Items
    /// Collection elements
    /// </summary>
    /// type:TtgdNamedCollectionItem
    /// <param name="Index"> (Integer) Element index</param>
    property Items[Index: Integer]: TtgdNamedCollectionItem read GetItems write
        SetItems;
    /// <summary>TtgdNamedCollection.Values Elements by name
    /// </summary> type:TtgdNamedCollectionItem
    /// <param name="Name"> (String) </param>
    property Values[Name: String]: TtgdNamedCollectionItem read GetValues; default;
  end;

  /// <summary>TtgdReportContextItem Template variable component (use it as variable in
  /// template)
  /// </summary>
  TtgdReportContextItem = class(TtgdNamedCollectionItem)
  private
    FComponent: TComponent;
  protected
    procedure SetComponent(const Value: TComponent);
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>TtgdReportContextItem.Component
    /// Component instance for template variable
    /// </summary>
    /// type:TComponent
    property Component: TComponent read FComponent write SetComponent;
  end;

  /// <summary>TtgdReportContext
  /// Context components for template
  /// </summary>
  TtgdReportContext = class(TtgdNamedCollection)
  protected
    function GetItems(Index: Integer): TtgdReportContextItem;
    function GetValues(Name: String): TtgdNamedCollectionItem;
    procedure SetItems(Index: Integer; const Value: TtgdReportContextItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add(AComponent: TComponent; AName: string = ''): TtgdReportContextItem;
    property Items[Index: Integer]: TtgdReportContextItem read GetItems write
      SetItems;
    property Values[Name: String]: TtgdNamedCollectionItem read GetValues; default;
  end;

  /// <summary>TtgdReportVariable
  /// Template variable
  /// </summary>
  TtgdReportVariable = class(TtgdNamedCollectionItem)
  private
    FValue: Variant;
  protected
    procedure SetValue(const Value: Variant);
  public
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>TtgdReportVariable.Value
    /// Variable value
    /// </summary>
    /// type:Variant
    property Value: Variant read FValue write SetValue;
  end;

  /// <summary>TtgdReportVariables
  /// Custom variables of template
  /// </summary>
  TtgdReportVariables = class(TtgdNamedCollection)
  protected
    function GetItems(Index: Integer): TtgdReportVariable;
    function GetValues(Name: String): TtgdReportVariable;
    procedure SetItems(Index: Integer; const Value: TtgdReportVariable);
  public
    constructor Create(AOwner: TPersistent);
    function Add(AName: string; AValue: Variant): TtgdReportVariable;
    property Items[Index: Integer]: TtgdReportVariable read GetItems write
      SetItems;
    property Values[Name: String]: TtgdReportVariable read GetValues; default;
  end;

  TtgdReportFunction = class;

  TtgdReportFunctionExecuteEvent = function(Sender: TtgdReportFunction; AParams: Variant): Variant of object;

  /// <summary>TtgdReportFunction
  /// Additional custom function for use in template
  /// </summary>
  TtgdReportFunction = class(TtgdNamedCollectionItem)
  private
    FDeclaration: string;
    FOnExecute: TtgdReportFunctionExecuteEvent;
  protected
    procedure SetDeclaration(const Value: string);
  public
    function ExtractNameFromDeclaration(ADeclaration: String): string;
    function DoExecute(AName: string; AParams: Variant): Variant;
    procedure Assign(ASource: TPersistent); override;
  published
    /// <summary>TtgdReportFunction.Declaration
    /// Function syntax
    /// </summary>
    /// type:string
    property Declaration: string read FDeclaration write SetDeclaration;
    /// <summary>TtgdReportFunction.OnExecute
    /// Function implementation
    /// </summary>
    /// type:TtgdReportFunctionExecuteEvent
    property OnExecute: TtgdReportFunctionExecuteEvent read FOnExecute write FOnExecute;
  end;

  /// <summary>TtgdReportFunctions
  /// Custom functions for use in template
  /// </summary>
  TtgdReportFunctions = class(TtgdNamedCollection)
  protected
    function GetItems(Index: Integer): TtgdReportFunction;
    function GetValues(Name: String): TtgdReportFunction;
    procedure SetItems(Index: Integer; const Value: TtgdReportFunction);
  public
    constructor Create(AOwner: TPersistent);
    function Add(ADeclaration: String): TtgdReportFunction;
    property Items[Index: Integer]: TtgdReportFunction read GetItems write
      SetItems;
    property Values[Name: String]: TtgdReportFunction read GetValues; default;
  end;

implementation

procedure TtgdNamedCollectionItem.Assign(ASource: TPersistent);
begin
  if ASource is TtgdNamedCollectionItem then
    Name := (ASource as TtgdNamedCollectionItem).Name
  else
    inherited Assign(ASource);
end;

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

procedure TtgdNamedCollection.Assign(ASource: TPersistent);
var
  I: Integer;
  vCollection: TtgdNamedCollection;
begin
  if ASource is TtgdNamedCollection then
  begin
    Clear;
    vCollection := ASource as TtgdNamedCollection;
    for I := 0 to vCollection.Count-1 do
      Add.Assign(vCollection.Items[I]);
  end
  else
    inherited Assign(ASource);
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

function TtgdNamedCollection.GetValues(Name: String): TtgdNamedCollectionItem;
begin
  if not TryFindName(Name, Result) then
    raise Exception.CreateFmt('Item with name "%s" is not found in collection', [Name]);
end;

function TtgdNamedCollection.IndexOfName(AName: string; APartialSearch: Boolean
    = False): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
  begin
    if AnsiSameText(AName, Items[I].Name)
      or (APartialSearch and AnsiStartsText(AName, Items[I].Name)) then
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

procedure TtgdReportContextItem.Assign(ASource: TPersistent);
begin
  if ASource is TtgdReportContextItem then
    Component := (ASource as TtgdReportContextItem).Component;
  inherited Assign(ASource);
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

function TtgdReportContext.GetValues(Name: String): TtgdNamedCollectionItem;
begin
  Result := TtgdNamedCollectionItem(inherited Values[Name]);
end;

procedure TtgdReportContext.SetItems(Index: Integer; const Value: TtgdReportContextItem);
begin
  inherited Items[Index] := Value;
end;

constructor TtgdReportVariables.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtgdReportVariable);
end;

function TtgdReportVariables.Add(AName: string; AValue: Variant):
    TtgdReportVariable;
begin
  Result := TtgdReportVariable.Create(Self);
  Result.Name := AName;
  Result.Value := AValue;
end;

function TtgdReportVariables.GetItems(Index: Integer): TtgdReportVariable;
begin
  Result := TtgdReportVariable(inherited Items[Index]);
end;

function TtgdReportVariables.GetValues(Name: String): TtgdReportVariable;
begin
  Result := TtgdReportVariable(inherited Values[Name]);
end;

procedure TtgdReportVariables.SetItems(Index: Integer; const Value: TtgdReportVariable);
begin
  inherited Items[Index] := Value;
end;

procedure TtgdReportFunction.Assign(ASource: TPersistent);
begin
  if ASource is TtgdReportFunction then
  begin
    Declaration := (ASource as TtgdReportFunction).Declaration;
    OnExecute := (ASource as TtgdReportFunction).OnExecute;
  end;
  inherited;
end;

function TtgdReportFunction.DoExecute(AName: string; AParams: Variant): Variant;
begin
  if Assigned(FOnExecute) then
    Result := FOnExecute(Self, AParams)
  else
    Result := Unassigned;
end;

function TtgdReportFunction.ExtractNameFromDeclaration(ADeclaration: String):
    string;
const
  IdentChars = ['a' .. 'z', 'A' .. 'Z', '0' .. '9', '_'];
var
  I: Integer;
  vText: string;
begin
  Result := '';
  vText := MidStr(ADeclaration, Pos(' ', ADeclaration)+1, MaxInt);
  I := 1;
  while I <= Length(vText) do
  begin
    if vText[I] in IdentChars then
      Result := Result + vText[I]
    else
      Break;
    Inc(I);
  end;
end;

procedure TtgdReportFunction.SetDeclaration(const Value: string);
begin
  if FDeclaration <> Value then
  begin
    FDeclaration := Value;
    Name := ExtractNameFromDeclaration(FDeclaration);
  end;
end;

constructor TtgdReportFunctions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TtgdReportFunction);
end;

function TtgdReportFunctions.Add(ADeclaration: String): TtgdReportFunction;
begin
  Result := TtgdReportFunction.Create(Self);
  Result.Declaration := ADeclaration;
end;

function TtgdReportFunctions.GetItems(Index: Integer): TtgdReportFunction;
begin
  Result := TtgdReportFunction(inherited Items[Index]);
end;

function TtgdReportFunctions.GetValues(Name: String): TtgdReportFunction;
begin
  Result := TtgdReportFunction(inherited Values[Name]);
end;

procedure TtgdReportFunctions.SetItems(Index: Integer; const Value: TtgdReportFunction);
begin
  inherited Items[Index] := Value;
end;

procedure TtgdReportVariable.Assign(ASource: TPersistent);
begin
  if ASource is TtgdReportVariable then
    Value := (ASource as TtgdReportVariable).Value;
  inherited;
end;

procedure TtgdReportVariable.SetValue(const Value: Variant);
begin
  if FValue <> Value then
  begin
    FValue := Value;
  end;
end;

end.

