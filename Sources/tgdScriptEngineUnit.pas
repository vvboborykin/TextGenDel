{*******************************************************
* Project: TextGenDelTest
* Unit: tgdScriptEngineUnit.pas
* Description: Scrip Engine interface declaration
*
* Created: 08.02.2024 9:41:02
* Copyright (C) 2024 Боборыкин В.В. (bpost@yandex.ru)
*******************************************************}
unit tgdScriptEngineUnit;

interface

uses
  SysUtils, Classes, Variants, StrUtils, DateUtils, Contnrs, tgdReportUnit;

type
  TtgdScriptElement = class
  private
    FHasChildren: Boolean;
    FName: string;
    FSourceObject: TObjectList;
    procedure SetName(const Value: string);
    procedure SetSourceObject(const Value: TObjectList);
  protected
    procedure SetHasChildren(const Value: Boolean);
  public
    property HasChildren: Boolean read FHasChildren write SetHasChildren;
    property Name: string read FName write SetName;
    property SourceObject: TObjectList read FSourceObject write SetSourceObject;
  end;

  TtgdScriptVariable = class(TtgdScriptElement)
  end;

  TtgdScriptClass = class(TtgdScriptElement)
  end;

  TtgdScriptFunction = class(TtgdScriptElement)
  end;

  TtgdScriptEvent = class(TtgdScriptElement)
  end;

  TtgdScriptProperty = class(TtgdScriptElement)
  end;


  TtgdScriptElementList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TtgdScriptElement;
    procedure SetItems(Index: Integer; const Value: TtgdScriptElement);
  public
    property Items[Index: Integer]: TtgdScriptElement read GetItems write SetItems;
        default;
  end;

  /// <summary>ItgdScriptEngine
  /// Scrip Engine interface declaration
  /// </summary>
  ItgdScriptEngine = interface
  ['{E42CD0D9-F515-4EAA-9A38-7E55F648CA11}']
    /// <summary>ItgdScriptEngine.ConvertTemplateToScript
    /// Convert report`s TemplateLines to script lines
    /// </summary>
    /// <param name="AScript"> (TStrings) Result script lines</param>
    procedure ConvertTemplateToScript(AScript: TStrings; ATemplateLines: TStrings =
        nil); stdcall;
    /// <summary>ItgdScriptEngine.ExecuteScript
    /// Execute script (generate text)
    /// </summary>
    /// <param name="AScript"> (TStrings) Script source to generate text</param>
    /// <param name="AResultLines"> (TStrings) Result text lines recipient</param>
    procedure ExecuteScript(AScript, AResultLines: TStrings); stdcall;
    /// <summary>ItgdScriptEngine.GetChildrenScriptElements
    /// Get script children elements
    /// </summary>
    /// <param name="AParent"> (TObject) </param>
    /// <param name="AChildren"> (TObjectList) </param>
    procedure GetChildrenScriptElements(AParent: TObject; AChildren:
        TtgdScriptElementList); stdcall;
    /// <summary>ItgdScriptEngine.Init
    /// Initialize script engine before use
    /// </summary>
    /// <param name="AReport"> (TtgdReport) Report context</param>
    procedure Init(AReport: TtgdReport); stdcall;
    /// <summary>ItgdScriptEngine.ValidateScript
    /// Validate script
    /// </summary>
    /// <param name="AScript"> (TStrings) Script lines</param>
    procedure ValidateScript(AScript: TStrings); stdcall;
  end;


  /// <summary>TtgdCreateScriptEngineFunc
  /// Factory method type
  /// </summary>
  /// type:ItgdScriptEngine
  TtgdCreateScriptEngineFunc = function: ItgdScriptEngine;

var
  /// <summary>CreateScriptEngine
  /// Factory method, creates ItgdScriptEngine instance using current
  /// implementation class. Assigned by implementation class module
  /// </summary>
  CreateScriptEngine: TtgdCreateScriptEngineFunc;

implementation

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

procedure TtgdScriptElement.SetSourceObject(const Value: TObjectList);
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
