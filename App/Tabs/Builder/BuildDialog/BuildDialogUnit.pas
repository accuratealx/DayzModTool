unit BuildDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  windows, Language, DialogCommonUnit;

type
  TBuildDialogForm = class(TDialogCommonForm)
    ilResult: TImageList;
    mContent: TMemo;
    btnClose: TSpeedButton;
    btnCopyContent: TSpeedButton;
    Timer: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyContentClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabBuilder.BuildDialog.';

    function  IsOperationSucceed: Boolean;
    procedure Build;
  protected
    procedure SetLanguage; override;
  end;


procedure BuildDialogExecute(Language: TLanguage; const AppFile, AParams: String);


implementation

{$R *.lfm}

uses
  Clipbrd, process, LazUTF8, Forms,
  DialogParameters;

type
  TBuildDialogParameters = class(TDialogParameters)
    AppFile: String;
    Params: String;
  end;


procedure BuildDialogExecute(Language: TLanguage; const AppFile, AParams: String);
var
  Params: TBuildDialogParameters;
begin
  Params := TBuildDialogParameters.Create;
  Params.Language := Language;
  Params.AppFile := AppFile;
  Params.Params := AParams;
  try
    with TBuildDialogForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TBuildDialogForm.FormResize(Sender: TObject);
begin
  btnClose.Left := (pnlButton.Width - btnClose.Width) div 2;
end;


procedure TBuildDialogForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;

  //Запустим сборку
  Build;

  //Поправим результат операции
  btnClose.ImageIndex := Ord(not IsOperationSucceed);

  //Разблокируем кнопку
  btnClose.Enabled := True;
end;


function TBuildDialogForm.IsOperationSucceed: Boolean;
begin
  Result := Pos('Build Successful', mContent.Text) > 0
end;


procedure TBuildDialogForm.Build;
const
  BUF_SIZE = 1024 * 64;
var
  Buffer: string = '';
  BytesRead: LongInt = 0;
  Params: TBuildDialogParameters;
  Proc: TProcess;
begin
  Params := FParameters as TBuildDialogParameters;

  Proc := TProcess.Create(nil);
  try
    //Настроим параметры
    Proc.Options := [poUsePipes];
    Proc.ShowWindow := swoHIDE;
    Proc.Executable := Params.AppFile;
    Proc.Parameters.Add(Params.Params);

    //Запустим процесс
    Proc.Execute;

    //Выбираем данные из потока
    repeat

      mContent.Lines.BeginUpdate;
      try
        SetLength(Buffer, BUF_SIZE);
        BytesRead := Proc.Output.Read(Buffer[1], Length(Buffer));

        if BytesRead > 0 then
        begin
          SetLength(Buffer, BytesRead);
          mContent.Append(Buffer);
        end;

      finally
        mContent.Lines.EndUpdate;
        mContent.SelStart := UTF8Length(mContent.Text);
      end;

      Sleep(50);
      Application.ProcessMessages;

    until BytesRead = 0;


  finally
    Proc.Free;
  end;
end;


procedure TBuildDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TBuildDialogForm.btnCopyContentClick(Sender: TObject);
begin
  Clipboard.AsText := mContent.Text;
end;


procedure TBuildDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Title', 'Рузультат сборки');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  btnCopyContent.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CopyContent', 'Скопировать текст в буфер обмена');
end;



end.

