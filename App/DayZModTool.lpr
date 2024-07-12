program DayZModTool;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, MainUnit, TrashCleanerUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;

  //Не понимаю как это работает, но кнопка на панели задач перестала показываться
  Application.MainFormOnTaskBar := True;

  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

