program DayZModTool;

{$mode objfpc}{$H+}

uses
  AdvancedSingleInstance, Windows,
  Interfaces, Forms, MainUnit, BuilderItemFrameCommonUnit;

{$R *.res}

var
  Wnd: HWND;

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  //Не понимаю как это работает, но кнопка на панели задач перестала показываться
  Application.MainFormOnTaskBar := True;

  //Запустим режим запуска только одной копии
  Application.SingleInstanceEnabled := true;
  Application.SingleInstance.Start;

  if Application.SingleInstance.IsServer then
  begin
    //Создадим приложение
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
  end
  else
  begin
    //Покажем уже открытое приложение
    Wnd := FindWindow(nil, PChar(GetAppTitle));
    if Wnd <> 0 then
      SetForegroundWindow(Wnd);
  end;
end.

