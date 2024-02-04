unit DialogUtils;

{$mode ObjFPC}{$H+}

interface

uses
  StdCtrls, Classes;


procedure GetTextSize(Lbl: TLabel; const Str: String; out AWidth, AHeight: Integer);

implementation


procedure GetTextSize(Lbl: TLabel; const Str: String; out AWidth, AHeight: Integer);
var
  List: TStringList;
  tw, th, i: Integer;
begin
  AWidth := 0;
  AHeight := 0;

  th := Lbl.Canvas.GetTextHeight('W');

  List := TStringList.Create;
  List.LineBreak := sLineBreak;
  List.Text := Str;

  for i := 0 to List.Count - 1 do
  begin
    tw := Lbl.Canvas.GetTextWidth(List.Strings[i]);
    if tw > AWidth then
      AWidth := tw;
  end;

  AHeight := th * List.Count;

  List.Free;
end;



end.

