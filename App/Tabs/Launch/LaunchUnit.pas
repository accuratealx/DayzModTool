unit LaunchUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniFiles,
  TabParameters, TabCommonUnit, LaunchItemUnit;

type
  TLaunchFrame = class(TTabCommonFrame)
    sbLaunch: TScrollBox;
    procedure sbLaunchResize(Sender: TObject);
  private
    FItems: TLaunchItemFrameList;
    FDataDir: String;               //Каталог с данными
    FSettingsDir: String;           //Каатлог с настройками
    FLaunchListFile: String;        //Файл настроек приложений

    procedure AddLaunchFrame(AFrame: TLaunchItemFrame);
    procedure CreateLaunchFrames(const LaunchFile: String);
    procedure DestroyLaunchFrame;
    procedure ArrangeLaunchTabItems;
    procedure OnChangeLaunchContentHeight(Sender: TObject);
  public
    constructor Create(Parameters: TTabParameters); reintroduce;
    destructor  Destroy; override;

    procedure FindExecutables;
    procedure ApplyLanguage; override;
    procedure SetCollapset(ACollapsed: Boolean);

    property Items: TLaunchItemFrameList read FItems;
  end;


implementation

{$R *.lfm}


procedure TLaunchFrame.sbLaunchResize(Sender: TObject);
begin
  ArrangeLaunchTabItems;
end;


procedure TLaunchFrame.AddLaunchFrame(AFrame: TLaunchItemFrame);
var
  c: Integer;
begin
  c := Length(FItems);
  SetLength(FItems, c + 1);
  FItems[c] := AFrame;
end;


procedure TLaunchFrame.CreateLaunchFrames(const LaunchFile: String);
var
  F: TIniFile;
  SectionList: TStringList;
  i: Integer;
  FrameCaption, FrameParams, FrameSettings, FrameIconName, FrameRelativeFilename, FrameLocaleID: String;
  Frame: TLaunchItemFrame;
  FrameIcon: TIcon;
begin
  F := TIniFile.Create(LaunchFile);
  SectionList := TStringList.Create;
  FrameIcon := TIcon.Create;
  try

    //Прочитать список секций
    F.ReadSections(SectionList);

    //Создать фреймы запуска
    for i := 0 to SectionList.Count - 1 do
    begin
      //Прочитать параметры фрейма
      FrameCaption := SectionList.Strings[i];
      FrameParams := FDataDir + F.ReadString(FrameCaption, 'ParamFile', '');
      FrameSettings := FSettingsDir + F.ReadString(FrameCaption, 'SettingsFile', '');
      FrameIconName := FDataDir + F.ReadString(FrameCaption, 'Icon', '');
      FrameRelativeFilename := F.ReadString(FrameCaption, 'RelativeFileName', '');
      FrameLocaleID := F.ReadString(FrameCaption, 'LocaleId', '');

      //Если нет файла с настройками параметров, то пропуск
      if not FileExists(FrameParams) then
        Continue;

      //Грузим иконку
      try
        if FileExists(FrameIconName) then
          FrameIcon.LoadFromFile(FrameIconName)
        else
          FrameIcon.Assign(Application.Icon);
      except
        FrameIcon.Assign(Application.Icon);
      end;

      //Создать фрейм
      Frame := TLaunchItemFrame.Create(FrameCaption, FrameIcon, FrameParams, FrameSettings, FrameRelativeFilename, FrameLocaleID);

      //Настроить выделение
      Frame.Higlight := not Odd(i);

      //Установить обработчик изменения высоты
      Frame.OnHeightChange := @OnChangeLaunchContentHeight;

      //Прикрепить фрейм к закладке
      Frame.Parent := sbLaunch;

      //Добавить в список
      AddLaunchFrame(Frame);
    end;

  finally
    FrameIcon.Free;
    SectionList.Free;
    F.Free;
  end;
end;


procedure TLaunchFrame.DestroyLaunchFrame;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do
    FItems[i].Free;
  SetLength(FItems, 0);
end;


procedure TLaunchFrame.ArrangeLaunchTabItems;
var
  i, Y: Integer;
  Frame: TLaunchItemFrame;
begin
  Y := 0;

  for i := 0 to Length(FItems) - 1 do
  begin
    Frame := sbLaunch.Controls[i] as TLaunchItemFrame;
    Frame.Left := 0;
    Frame.Width := sbLaunch.ClientWidth;
    Frame.Top := Y;

    Inc(Y, Frame.Height);
  end;
end;


procedure TLaunchFrame.OnChangeLaunchContentHeight(Sender: TObject);
begin
  ArrangeLaunchTabItems;
end;


constructor TLaunchFrame.Create(Parameters: TTabParameters);
begin
  inherited Create(Parameters);

  //Подготовить каталог данных
  FDataDir := FParams.DataDirectory + 'Launch\';
  ForceDirectories(FDataDir);

  //Определить файл настроек фреймов
  FLaunchListFile := FDataDir + '\List.ini';

  //Подготовить каталог хранения параметров фреймов
  FSettingsDir := FParams.SettingsDirectory + 'Launch\';
  ForceDirectories(FSettingsDir);

  //Создать фреймы
  CreateLaunchFrames(FLaunchListFile);
end;


destructor TLaunchFrame.Destroy;
begin
  DestroyLaunchFrame;

  inherited Destroy;
end;


procedure TLaunchFrame.FindExecutables;
var
  i: Integer;
begin
  //В каждом фрейме вызвать поиск приложения
  for i := 0 to Length(FItems) - 1 do
    FItems[i].FindExecutable;
end;


procedure TLaunchFrame.ApplyLanguage;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do
    FItems[i].ChangeLanguage(FParams.Language);
end;


procedure TLaunchFrame.SetCollapset(ACollapsed: Boolean);
var
  i: Integer;
begin
  for i := Length(FItems) - 1 downto 0 do
    FItems[i].Collapsed := ACollapsed;
end;



end.

