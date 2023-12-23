unit ParamFrameFileUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  StartParamSimple, StartParamFile,
  ParamFrameSimpleUnit;

type
  TParamFrameFileFrame = class(TParamFrameSimpleFrame)
    btnClearValue: TSpeedButton;
    btnOpenDirectory: TSpeedButton;
    btnSelectFile: TSpeedButton;
    edValue: TEdit;
    OpenDialog: TOpenDialog;
    procedure btnClearValueClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
  private
    procedure SetValue(AValue: String);
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  public

  end;


implementation

{$R *.lfm}

uses
  DayZUtils;


procedure TParamFrameFileFrame.btnSelectFileClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(edValue.Text);

  OpenDialog.InitialDir := Dir;
  if OpenDialog.Execute then
  begin
    SetValue(OpenDialog.FileName);
  end;
end;


procedure TParamFrameFileFrame.SetValue(AValue: String);
begin
  (FItem as TStartParamFile).Value := AValue;
  edValue.Text := AValue;
end;


procedure TParamFrameFileFrame.btnClearValueClick(Sender: TObject);
begin
  SetValue('');
end;


procedure TParamFrameFileFrame.btnOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(Trim((FItem as TStartParamFile).Value));
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TParamFrameFileFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamFile).Value;
end;



end.

