unit TranslationWorker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Windows, SysUtils,
  Translator, StringTableItem;

const
  TRANSLATOR_MESSAGE = WM_USER + 1;
  TRANSLATOR_WPARAM_START = 0;
  TRANSLATOR_WPARAM_FINISH = 1;
  TRANSLATOR_WPARAM_ERROR = 2;
  TRANSLATOR_WPARAM_LANGUAGE_ID = 3;

type
  //Метод передачи строки с ошибкой
  TTranslateErrorHandler = procedure(msg: String) of object;


  TTranslateWorker = class(TThread)
  private
    FCallbackHandler: HWND;
    FTableItem: TStringTableItem;
    FOriginalLng: TStringTableLanguageTypes;
    FMsg: String;

    FOnErrorHandler: TTranslateErrorHandler;
  public
    constructor Create(CallbackHandler: HWND; ErrorHandler: TTranslateErrorHandler; TableItem: TStringTableItem; OriginalLng: TStringTableLanguageTypes; Msg: String);

    procedure Execute; override;
  end;

implementation


constructor TTranslateWorker.Create(CallbackHandler: HWND; ErrorHandler: TTranslateErrorHandler; TableItem: TStringTableItem; OriginalLng: TStringTableLanguageTypes; Msg: String);
begin
  inherited Create(False);
  FreeOnTerminate := True;

  FCallbackHandler := CallbackHandler;
  FTableItem := TableItem;
  FOriginalLng := OriginalLng;
  FMsg := Msg;

  FOnErrorHandler := ErrorHandler;
end;


procedure TTranslateWorker.Execute;
var
  i: TStringTableLanguageTypes;
begin
  try
    //Старт перевода
    PostMessage(FCallbackHandler, TRANSLATOR_MESSAGE, TRANSLATOR_WPARAM_START, 0);


    for i := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
    begin
      //Пропуск текущего языка
      if i = FOriginalLng then
      begin
        //Заполнить оригинальный текст
        FTableItem.LocalizedText[i] := FMsg;
        Continue;
      end;

      //Отправить сообщение с номером языка для перевода
      PostMessage(FCallbackHandler, TRANSLATOR_MESSAGE, TRANSLATOR_WPARAM_LANGUAGE_ID, Ord(i));


      //Перевести элемент
      FTableItem.LocalizedText[i] := Translate_GoogleApp(TTranslatorLanguages(FOriginalLng), TTranslatorLanguages(i), FMsg);
    end;

    //Отправить сообщение что все в порядке
    PostMessage(FCallbackHandler, TRANSLATOR_MESSAGE, TRANSLATOR_WPARAM_FINISH, 0);

  except
    //Передать сообщение об ошибке
    if Assigned(FOnErrorHandler) then
      FOnErrorHandler(Exception(ExceptObject).Message);

    //Отправить сообщение что произошла ошибка
    PostMessage(FCallbackHandler, TRANSLATOR_MESSAGE, TRANSLATOR_WPARAM_ERROR, 0);
  end;
end;


end.

