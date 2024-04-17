unit ColumnAdjustUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids,
  StdCtrls, Buttons, LMessages;

type
  TColumnAdjustForm = class(TForm)
    btnSelectAll: TSpeedButton;
    btnUnselectAll: TSpeedButton;
    ContentPanel: TPanel;
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnUnselectAllClick(Sender: TObject);
    procedure CheckBoxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FGrid: TStringGrid;

    procedure SetChecked(Checked: Boolean);
    procedure PrepareInterface;

    procedure OnActivate(var msg: TLMActivate); message LM_ACTIVATE;
  public

  end;


procedure ColumnAdjustExecute(PopupPos: TPoint; Grid: TStringGrid);


implementation

{$R *.lfm}


procedure ColumnAdjustExecute(PopupPos: TPoint; Grid: TStringGrid);
begin
  with TColumnAdjustForm.Create(nil) do
  begin
    Left := PopupPos.X;
    Top := PopupPos.Y;
    FGrid := Grid;
    PrepareInterface;

    Show;
  end;
end;


procedure TColumnAdjustForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;


procedure TColumnAdjustForm.CheckBoxChange(Sender: TObject);
var
  Index: Integer;
begin
  Index := (Sender as TCheckBox).Tag;
  FGrid.Columns.Items[Index].Visible := (Sender as TCheckBox).Checked;
end;


procedure TColumnAdjustForm.btnSelectAllClick(Sender: TObject);
begin
  SetChecked(True);
end;


procedure TColumnAdjustForm.btnUnselectAllClick(Sender: TObject);
begin
  SetChecked(False);
end;


procedure TColumnAdjustForm.SetChecked(Checked: Boolean);
var
  i: Integer;
  cb: TCheckBox;
begin
  for i := 0 to ContentPanel.ControlCount - 1 do
  begin
    if not (ContentPanel.Controls[i] is TCheckBox) then
      Continue;

    cb := ContentPanel.Controls[i] as TCheckBox;
    cb.Checked := Checked;
  end;
end;


procedure TColumnAdjustForm.PrepareInterface;
const
  ITEM_HEIGHT = 19;
  PADDING_CONTENT = 10;
var
  i, Y, ItemMaxWidth, W: Integer;
  cb: TCheckBox;
begin
  Y := btnSelectAll.Top + btnSelectAll.Height + 5;
  ItemMaxWidth := 0;

  for i := 1 to FGrid.Columns.Count - 1 do
  begin
    cb := TCheckBox.Create(ContentPanel);
    cb.Parent := ContentPanel;
    cb.ParentFont := True;
    cb.Cursor := crHandPoint;
    cb.Caption := FGrid.Columns.Items[i].Title.Caption;
    cb.Checked := FGrid.Columns.Items[i].Visible;
    cb.OnChange := @CheckBoxChange;
    cb.Left := PADDING_CONTENT;
    cb.Tag := i;
    cb.Top := Y;

    //Невидимый контрол возвращает неправильную ширину, свелосипедим
    W := ContentPanel.Canvas.GetTextWidth(cb.Caption) + ITEM_HEIGHT;
    if W > ItemMaxWidth then
      ItemMaxWidth := W;

    Inc(Y, ITEM_HEIGHT + 2);
  end;

  Height := Y + PADDING_CONTENT;
  Width := ItemMaxWidth + PADDING_CONTENT * 2;
end;


procedure TColumnAdjustForm.OnActivate(var msg: TLMActivate);
begin
  inherited;
  if msg.Active = WA_INACTIVE then
    Close;
end;



end.

