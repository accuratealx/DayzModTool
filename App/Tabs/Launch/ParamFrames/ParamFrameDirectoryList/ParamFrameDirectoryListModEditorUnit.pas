unit ParamFrameDirectoryListModEditorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Buttons, StdCtrls,
  Language,
  StartParamDirectoryList;

type
  TParamFrameDirectoryListModEditorFrame = class(TFrame)
    btnSelectAll: TSpeedButton;
    btnUnselectAll: TSpeedButton;
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnUnselectAllClick(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabLaunch.Param.';

      //Костыль! TCheckBox.Height возвращает разную высоту когда wincontrol не виден на экране
      CHECK_BOX_HEIGHT = 19;
  private
    FItems: TStartParamDirectoryListItemArray;
    FCheckBoxList: array of TCheckBox;

    procedure SetCheckStatus(AStatus: Boolean);
    procedure cbClick(Sender: TObject);

    procedure ClearCheckBoxList;
    procedure CreateCheckBoxList(AItems: TStartParamDirectoryListItemArray);
    procedure CorrectFrameHeight;
  public
    constructor Create(AItems: TStartParamDirectoryListItemArray); reintroduce;
    destructor  Destroy; override;

    procedure UpdateInterface;

    procedure ChangeLanguage(Language: TLanguage);

    property Items: TStartParamDirectoryListItemArray read FItems;
  end;


implementation

{$R *.lfm}


procedure TParamFrameDirectoryListModEditorFrame.btnSelectAllClick(Sender: TObject);
begin
  SetCheckStatus(True);
end;


procedure TParamFrameDirectoryListModEditorFrame.btnUnselectAllClick(Sender: TObject);
begin
  SetCheckStatus(False);
end;


procedure TParamFrameDirectoryListModEditorFrame.SetCheckStatus(AStatus: Boolean);
var
  i, c: Integer;
begin
  c := Length(FCheckBoxList) - 1;
  for i := 0 to c do
    FCheckBoxList[i].Checked := AStatus;
end;


procedure TParamFrameDirectoryListModEditorFrame.cbClick(Sender: TObject);
var
  Cb: TCheckBox absolute Sender;
begin
  FItems.Item[Cb.Tag].Checked := Cb.Checked;
end;


procedure TParamFrameDirectoryListModEditorFrame.ClearCheckBoxList;
var
  i, c: Integer;
begin
  c := Length(FCheckBoxList) - 1;
  for i := 0 to c do
    FCheckBoxList[i].Free;
  SetLength(FCheckBoxList, 0);
end;


procedure TParamFrameDirectoryListModEditorFrame.CreateCheckBoxList(AItems: TStartParamDirectoryListItemArray);

  procedure Add(cb: TCheckBox);
  var
    c: Integer;
  begin
    c := Length(FCheckBoxList);
    SetLength(FCheckBoxList, c + 1);
    FCheckBoxList[c] := cb;
  end;

const
  X_OFFSET = 5;
  Y_OFFSET = 5;
var
  i, c, Y: Integer;
  cb: TCheckBox;
begin
  ClearCheckBoxList;

  c := AItems.Count - 1;
  Y := Y_OFFSET;

  for i := 0 to c do
  begin
    cb := TCheckBox.Create(nil);
    cb.Cursor := crHandPoint;
    cb.Caption := Trim(AItems.Item[i].Value);
    cb.Checked := AItems.Item[i].Checked;
    cb.OnClick := @cbClick;
    cb.Tag := i;
    cb.Left := btnSelectAll.Left + btnSelectAll.Width + X_OFFSET;
    cb.Top := Y;
    cb.Parent := Self;

    Add(cb);

    Inc(Y, CHECK_BOX_HEIGHT);
  end;
end;


procedure TParamFrameDirectoryListModEditorFrame.CorrectFrameHeight;
var
  c, ItemHeight, MinHeight, CbHeight: Integer;
begin
  MinHeight := btnUnselectAll.Top + btnUnselectAll.Height;

  c := Length(FCheckBoxList);

  if c > 0 then
    ItemHeight := CHECK_BOX_HEIGHT
  else
    ItemHeight := 0;

  CbHeight := c * ItemHeight + 5;

  if CbHeight > MinHeight then
    Height := CbHeight
  else
    Height := MinHeight;
end;


constructor TParamFrameDirectoryListModEditorFrame.Create(AItems: TStartParamDirectoryListItemArray);
begin
  inherited Create(nil);

  FItems := AItems;
  CorrectFrameHeight;
end;


destructor TParamFrameDirectoryListModEditorFrame.Destroy;
begin
  ClearCheckBoxList;

  inherited Destroy;
end;


procedure TParamFrameDirectoryListModEditorFrame.UpdateInterface;
begin
  CreateCheckBoxList(FItems);
  CorrectFrameHeight;
end;


procedure TParamFrameDirectoryListModEditorFrame.ChangeLanguage(Language: TLanguage);
begin
  btnSelectAll.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'SelectAll', 'Выбрать все элементы');
  btnUnselectAll.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'UnselectAll', 'Снять выбор со всех элементов');
end;



end.

