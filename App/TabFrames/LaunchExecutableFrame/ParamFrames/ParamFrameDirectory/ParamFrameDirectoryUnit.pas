unit ParamFrameDirectoryUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  StartParamSimple, StartParamDirectory,
  ParamFrameSimpleUnit;

type
  TParamFrameDirectoryFrame = class(TParamFrameSimpleFrame)
    btnClearValue: TSpeedButton;
    btnSelectDirectory: TSpeedButton;
    btnOpenDirectory: TSpeedButton;
    edValue: TEdit;
    procedure btnClearValueClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnSelectDirectoryClick(Sender: TObject);
  private
    procedure SetValue(AValue: String);
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  public

  end;


implementation

{$R *.lfm}

uses
  DayZUtils, SelectDirectoryDialogUnit;


procedure TParamFrameDirectoryFrame.btnSelectDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(edValue.Text);
  if SelectDirectoryDialogExecute('Выберите каталог', Dir) then
    SetValue(Dir);
end;


procedure TParamFrameDirectoryFrame.SetValue(AValue: String);
begin
  (FItem as TStartParamDirectory).Value := AValue;
  edValue.Text := AValue;
end;


procedure TParamFrameDirectoryFrame.btnClearValueClick(Sender: TObject);
begin
  SetValue('');
end;


procedure TParamFrameDirectoryFrame.btnOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := Trim((FItem as TStartParamDirectory).Value);
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TParamFrameDirectoryFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamDirectory).Value;
end;



end.

