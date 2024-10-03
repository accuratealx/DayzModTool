unit BuilderItemUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Graphics,
  Language;

type
  TBuilderItemFrame = class(TFrame)
    btnBuild: TSpeedButton;
    ilCollapse: TImageList;
    imgCollapse: TImage;
    imgIcon: TImage;
    lblTitle: TLabel;
    pnlCollapse: TPanel;
    procedure FrameClick(Sender: TObject);
    procedure pnlCollapseClick(Sender: TObject);
    procedure pnlCollapsePaint(Sender: TObject);
  private
    FLanguage: TLanguage;

    FIcon: TIcon;

    FCollapsed: Boolean;
    FHighlight: Boolean;
    FSelected: Boolean;

    FOnHeightChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;

    function  GetTotalFrameHeight: Integer;

    procedure SetTitle(ATitle: String);
    function  GetTitle: String;
    procedure SetSelected(ASelected: Boolean);
    procedure SetHighlight(AHighlight: Boolean);
    procedure SetCollapsed(ACollapsed: Boolean);

    procedure DoOnSelect;
    procedure DoHeightChange;
  public
    constructor Create; reintroduce;
    constructor Create(ASettings: String);
    destructor  Destroy; override;

    procedure ValueFromString(const AValue: String);
    function  ValueToString: String;
    procedure ChangeLanguage(Language: TLanguage);

    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Selected: Boolean read FSelected write SetSelected;
    property Highlight: Boolean read FHighlight write SetHighlight;
    property Title: String read GetTitle write SetTitle;

    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TBuilderItemFrameList = array of TBuilderItemFrame;


implementation

{$R *.lfm}

const
  SEPARATOR = ';;;';


procedure TBuilderItemFrame.pnlCollapseClick(Sender: TObject);
begin
  if not FSelected then
  begin
    Selected := True;
    Exit;
  end;

  Collapsed := not Collapsed;
end;


procedure TBuilderItemFrame.pnlCollapsePaint(Sender: TObject);
begin
  if FSelected then
  begin
    pnlCollapse.Canvas.Pen.Color := clHighlight;
    pnlCollapse.Canvas.Pen.Style := psSolid;
    pnlCollapse.Canvas.Pen.Width := 5;
    pnlCollapse.Canvas.Line(0, 0, 0, pnlCollapse.Height);
  end;
end;


procedure TBuilderItemFrame.FrameClick(Sender: TObject);
begin
  Selected := True;
end;


function TBuilderItemFrame.GetTotalFrameHeight: Integer;
begin
  Result := 200;
end;


procedure TBuilderItemFrame.SetTitle(ATitle: String);
begin
  lblTitle.Caption := ATitle;
end;


function TBuilderItemFrame.GetTitle: String;
begin
  Result := Trim(lblTitle.Caption);
end;


procedure TBuilderItemFrame.SetSelected(ASelected: Boolean);
begin
  if FSelected = ASelected then
    Exit;

  FSelected := ASelected;

  pnlCollapse.Repaint;

  if FSelected then
    DoOnSelect;
end;


procedure TBuilderItemFrame.SetHighlight(AHighlight: Boolean);
begin
  if FHighlight = AHighlight then
    Exit;

  FHighlight := AHighlight;

  Self.ParentColor := not FHighlight;
  Self.ParentBackground := not FHighlight;

  if FHighlight then
    Self.Color := cl3DLight
  else
    Self.Color := Parent.Color;

  //Почему-то панелька меняет свой цвет после изменения цвета родителя
  pnlCollapse.ParentBackground := True;
  pnlCollapse.Color := Self.Color;
end;


procedure TBuilderItemFrame.SetCollapsed(ACollapsed: Boolean);
begin
  FCollapsed := ACollapsed;

  if FCollapsed then
  begin
    Height := 45;
    imgCollapse.ImageIndex := 0;
  end
  else
  begin
    Height := GetTotalFrameHeight;
    imgCollapse.ImageIndex := 1;
  end;

  //Вызвать обработчик изменения высоты
  DoHeightChange;
end;


procedure TBuilderItemFrame.DoOnSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;


procedure TBuilderItemFrame.DoHeightChange;
begin
  if Assigned(FOnHeightChange) then
    FOnHeightChange(Self);
end;


constructor TBuilderItemFrame.Create;
begin
  inherited Create(nil);
end;


constructor TBuilderItemFrame.Create(ASettings: String);
begin
  Create;

  ValueFromString(ASettings);
end;


destructor TBuilderItemFrame.Destroy;
begin

  FIcon.Free;
  inherited Destroy;
end;


procedure TBuilderItemFrame.ValueFromString(const AValue: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := SEPARATOR;
  List.Text := AValue;
  try

    //TODO: Сделать безосную загрузку параметров
    Collapsed := StrToBool(List.Strings[0]);
    SetTitle(List.Strings[1]);

  finally
    List.Free;
  end;
end;


function TBuilderItemFrame.ValueToString: String;
begin
  Result :=
    BoolToStr(FCollapsed) + SEPARATOR +
    lblTitle.Caption;
end;


procedure TBuilderItemFrame.ChangeLanguage(Language: TLanguage);
begin
  FLanguage := Language;

  //Элементы управления

end;



end.

