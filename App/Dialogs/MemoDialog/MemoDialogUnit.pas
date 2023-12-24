unit MemoDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DialogCommonUnit;

type
  TMemoDialogForm = class(TDialogCommonForm)
    mContent: TMemo;
    btnClose: TSpeedButton;
    btnCopyContent: TSpeedButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyContentClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FContent: String;
    procedure PrepareInterface(const ACaption: String; const AContent: String);
  public
    constructor Create(const ACaption: String; const AContent: String); reintroduce;
  end;


procedure MemoDialogExecute(const Caption: String; const Content: String);


implementation

{$R *.lfm}

uses
  Clipbrd;


procedure MemoDialogExecute(const Caption: String; const Content: String);
begin
  with TMemoDialogForm.Create(Caption, Content) do
  begin
    ShowModal;
    Free;
  end;
end;


procedure TMemoDialogForm.FormResize(Sender: TObject);
begin
  btnClose.Left := (pnlButton.Width - btnClose.Width) div 2;
end;


procedure TMemoDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMemoDialogForm.btnCopyContentClick(Sender: TObject);
begin
  Clipboard.AsText := FContent;
end;


procedure TMemoDialogForm.PrepareInterface(const ACaption: String; const AContent: String);
begin
  Caption := ACaption;
  mContent.Text := AContent;
end;


constructor TMemoDialogForm.Create(const ACaption: String; const AContent: String);
begin
  inherited Create(nil);

  FContent := AContent;
  PrepareInterface(ACaption, AContent);
end;



end.

