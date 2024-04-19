unit Translator;

{$mode ObjFPC}{$H+}

interface

type
  TTranslatorLanguages = (
    tlEnglish,      //Английский
    tlCzech,        //Чешский
    tlGerman,       //Немецкий
    tlRussian,      //Русский
    tlPolish,       //Польский
    tlHungarian,    //Венгерский
    tlItalian,      //Итальянский
    tlSpanish,      //Эспанский
    tlFrench,       //Французский
    tlChinese,      //Китайский
    tlJapanese,     //Японский
    tlPortuguese,   //Португальский
    tlChineseSimp   //Китайский (упрощенный)
  );

const
  TranslatorLanguagesNames: array[TTranslatorLanguages] of String = (
    'en',
    'cs',
    'de',
    'ru',
    'pl',
    'hu',
    'it',
    'es',
    'fr',
    'zh-TW',
    'ja',
    'pt',
    'zh-CN'
  );


function Translate_GoogleApp(SrcLng: TTranslatorLanguages; DstLng: TTranslatorLanguages; Msg: String): String;


implementation

uses
  SysUtils, Classes, LazUTF8,
  fphttpclient, opensslsockets, fpjson, jsonparser, jsonscanner, httpprotocol;


function Translate_GoogleApp(SrcLng: TTranslatorLanguages; DstLng: TTranslatorLanguages; Msg: String): String;

  function GetResult(JSONStr: String): String;
  var
    JSonParser: TJSONParser;
    JSONArr: TJSONArray;
    JSonBody, J: TJSONObject;
  begin
    Result := '';

    JSonBody := nil;
    JSonParser := TJSONParser.Create(JSONStr, DefaultOptions);
    JSonBody := JsonParser.Parse as TJSONObject;
    try

      JSONArr := JSonBody.Find('sentences', jtArray) as TJSONArray;
      if JSONArr <> nil then
      begin
        if JSONArr.Count > 0 then
        begin
          J := JSONArr.Items[0] as TJSONObject;
          if J <> nil then
            Result := J.Get('trans');
        end;
      end;

    finally
      JSonBody.Free;
      JSonParser.Free;
    end;
  end;

const
  URL = 'https://translate.google.com/translate_a/single?client=at&dt=t&dt=ld&dt=qca&dt=rm&dt=bd&dj=1&hl=uk-RU&ie=UTF-8&oe=UTF-8&inputm=2&otf=2&iid=1dd3b944-fa62-4b55-b330-74909a99969e';
var
  Connector: TFPHTTPClient;
  Response: TRawByteStringStream;
  Str, src, dst: String;
begin
  Result := '';

  //Подготовить тело запроса
  src := TranslatorLanguagesNames[SrcLng];
  dst := TranslatorLanguagesNames[DstLng];
  Str := Format('sl=%s&tl=%s&q=%s', [src, dst, EncodeURLElement(Msg)]);

  //Создать коннектор
  Connector := TFPHTTPClient.Create(nil);
  Response := TRawByteStringStream.Create;
  try
    //Добавить заголовки
    Connector.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    Connector.AddHeader('User-Agent', 'AndroidTranslate/5.3.0.RC02.130475354-53000263 5.1 phone TRANSLATE_OPM5_TEST_1');
    //Connector.AddHeader('Accept-Encoding', '');
    Connector.AddHeader('Connection', 'close');

    //Установить тело
    Connector.RequestBody := TRawByteStringStream.Create(Str);

    //Выполнить запрос
    Connector.Post(URL, Response);

    //Получить ответ
    Result := GetResult(Response.DataString);

  finally
    Connector.RequestBody.Free;
    Response.Free;
    Connector.Free;
  end;
end;


end.

