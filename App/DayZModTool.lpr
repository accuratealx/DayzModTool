program DayZModTool;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, MainUnit, TranslationWorker;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

