unit tdgScriptElementsUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, Contnrs;

type
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
    destructor Destroy; override;
    property HasChildren: Boolean read FHasChildren write SetHasChildren;
    property Name: string read FName write SetName;
    property SourceObject: TObject read FSourceObject write SetSourceObject;
  end;

  TtgdScriptVariable = class(TtgdScriptElement)
  end;

  TtgdScriptClass = class(TtgdScriptElement)
  end;

  TtgdScriptFunction = class(TtgdScriptElement)
  end;

  TtgdScriptMethod = class(TtgdScriptElement)
  end;

  TtgdScriptEvent = class(TtgdScriptElement)
  end;

  TtgdScriptProperty = class(TtgdScriptElement)
  end;

  TtgdScriptConstant = class(TtgdScriptElement)
  end;

  TtgdScriptType = class(TtgdScriptElement)
  end;


  TtgdScriptElementList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TtgdScriptElement;
    procedure SetItems(Index: Integer; const Value: TtgdScriptElement);
  public
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

destructor TtgdScriptElement.Destroy;
begin
  inherited Destroy;
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

function TtgdScriptElementList.GetItems(Index: Integer): TtgdScriptElement;
begin
  Result := inherited Items[Index] as TtgdScriptElement;
end;

procedure TtgdScriptElementList.SetItems(Index: Integer; const Value:
    TtgdScriptElement);
begin
  inherited Items[Index] := Value
end;

end.
