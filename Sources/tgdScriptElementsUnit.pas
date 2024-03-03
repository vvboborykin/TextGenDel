{*******************************************************
* Project: TextGenDelTest
* Unit: tdgScriptElementsUnit.pas
* Description: Script element classes and list
* 
* Created: 13.02.2024 9:57:35
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdScriptElementsUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, Contnrs;

type
  /// <summary>TtgdScriptElement
  /// Abstract script element
  /// </summary>
  TtgdScriptElement = class
  private
    FHasChildren: Boolean;
    FName: string;
    FSourceObject: TObject;
    procedure SetName(const Value: string);
    procedure SetSourceObject(const Value: TObject);
  protected
    procedure SetHasChildren(const Value: Boolean);
  public
    constructor Create(ASourceObject: TObject; AName: string; AHasChildren:
        Boolean);
    property HasChildren: Boolean read FHasChildren write SetHasChildren;
    property Name: string read FName write SetName;
    property SourceObject: TObject read FSourceObject write SetSourceObject;
  end;

  /// <summary>TtgdScriptVariable
  /// Script variable
  /// </summary>
  TtgdScriptVariable = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptClass
  /// Script class
  /// </summary>
  TtgdScriptClass = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptFunction
  /// Script function or procedure
  /// </summary>
  TtgdScriptFunction = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptMethod
  /// Script class method
  /// </summary>
  TtgdScriptMethod = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptEvent
  /// Script event type
  /// </summary>
  TtgdScriptEvent = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptProperty
  /// Script class property
  /// </summary>
  TtgdScriptProperty = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptConstant
  /// Script constant
  /// </summary>
  TtgdScriptConstant = class(TtgdScriptElement)
  end;

  /// <summary>TtgdScriptType
  /// Script type declaration
  /// </summary>
  TtgdScriptType = class(TtgdScriptElement)
  end;


  /// <summary>TtgdScriptElementList
  /// List of script element
  /// </summary>
  TtgdScriptElementList = class(TObjectList)
  private
    FAllowedClasses: TList;
  protected
    function GetItems(Index: Integer): TtgdScriptElement;
    procedure SetItems(Index: Integer; const Value: TtgdScriptElement);
  public
    constructor Create(AOwnsObjects: Boolean); overload;
    destructor Destroy; override;
    function ContainsName(AName: string): Boolean;
    function ContainsScriptElement(AElement: TtgdScriptElement): Boolean;
    function IsClassAllowed(AClass: TClass): Boolean;
    procedure SortByName;
    property AllowedClasses: TList read FAllowedClasses;
    /// <summary>TtgdScriptElementList.Items
    /// Elements in list
    /// </summary>
    /// type:TtgdScriptElement
    /// <param name="Index"> (Integer) </param>
    property Items[Index: Integer]: TtgdScriptElement read GetItems write SetItems;
        default;
  end;

implementation

constructor TtgdScriptElement.Create(ASourceObject: TObject; AName: string;
    AHasChildren: Boolean);
begin
  inherited Create;
  FHasChildren := AHasChildren;
  FName := AName;
  FSourceObject := ASourceObject;
end;

procedure TtgdScriptElement.SetHasChildren(const Value: Boolean);
begin
  if FHasChildren <> Value then
  begin
    FHasChildren := Value;
  end;
end;

procedure TtgdScriptElement.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    FName := Value;
  end;
end;

procedure TtgdScriptElement.SetSourceObject(const Value: TObject);
begin
  if FSourceObject <> Value then
  begin
    FSourceObject := Value;
  end;
end;

constructor TtgdScriptElementList.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FAllowedClasses := TList.Create();
end;

destructor TtgdScriptElementList.Destroy;
begin
  FreeAndNil(FAllowedClasses);
  inherited Destroy;
end;

function TtgdScriptElementList.ContainsName(AName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count-1  do
  begin
    if AnsiSameText(AName, Items[I].Name) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TtgdScriptElementList.ContainsScriptElement(AElement:
    TtgdScriptElement): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count-1 do
  begin
    Result := AnsiSameText(Items[I].Name, AElement.Name);
    if Result then
      Break;
  end;
end;

function TtgdScriptElementList.GetItems(Index: Integer): TtgdScriptElement;
begin
  Result := inherited Items[Index] as TtgdScriptElement;
end;

function TtgdScriptElementList.IsClassAllowed(AClass: TClass): Boolean;
begin
  Result := (AllowedClasses.Count = 0) or (AllowedClasses.IndexOf(Pointer(AClass)) >= 0);
end;

procedure TtgdScriptElementList.SetItems(Index: Integer; const Value:
    TtgdScriptElement);
begin
  inherited Items[Index] := Value
end;

function SortNameFunc(Item1, Item2: Pointer): Integer;
var
  vItem1, vItem2: TtgdScriptElement;
begin
  vItem1 := TtgdScriptElement(Item1);
  vItem2 := TtgdScriptElement(Item2);
  Result := CompareStr(vItem1.Name, vItem2.Name);
end;

procedure TtgdScriptElementList.SortByName;
begin
  Sort(SortNameFunc);
end;

end.


