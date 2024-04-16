unit DosDevices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;


procedure MapDrive(const Letter: Char; const Path: String);   //Назначить виртуальный диск
procedure UnMapDrive(const Letter: Char);                     //Удалить виртуальный диск
function  GetDrivePath(const Letter: Char): String;           //Получить путь виртуального диска
procedure GetAvailableDriveLetters(ResultList: TStringList);  //Получить список свободных букв дисководов


implementation

uses
  Windows, SysUtils, LazUTF8;


procedure MapDrive(const Letter: Char; const Path: String);
var
  R: WINBOOL;
  L: PChar;
begin
  SetLastError(ERROR_SUCCESS);
  L := StrAlloc(2);
  try
    StrPCopy(L, Letter + ':');
    R := DefineDosDevice(0, L, PChar(Path));
    if not R then
      RaiseLastOSError;

  finally
    StrDispose(L);
  end;
end;


procedure UnMapDrive(const Letter: Char);
var
  L: PChar;
begin
  SetLastError(ERROR_SUCCESS);
  L := StrAlloc(2);
  try
    StrPCopy(L, Letter + ':');
    if not DefineDosDevice(DDD_REMOVE_DEFINITION, L, nil) then
      RaiseLastOSError;

  finally
    StrDispose(L);
  end;
end;


function GetDrivePath(const Letter: Char): String;
var
  L, Buffer: PChar;
  Len: DWORD;
begin
  Result := '';

  Buffer := StrAlloc(MAX_PATH + 1);
  L := StrAlloc(2);
  try
    StrPCopy(L, Letter + ':');
    Len := QueryDosDevice(L, Buffer, MAX_PATH);
    if Len > 0 then
    begin
      //Отсеить все кроме виртуальных
      if Pos('\Device\', Buffer) = 0 then
        Result := Copy(StrPas(Buffer), 5, Len - 4);
    end;

  finally
    StrDispose(L);
    StrDispose(Buffer);
  end;
end;


procedure GetAvailableDriveLetters(ResultList: TStringList);
var
  Buff: PChar;
  CurrDrive: PChar;
  BuffLen: Integer;
  ErrCode: Cardinal;
begin
  ResultList.Clear;

  SetLastError(ERROR_SUCCESS);
  BuffLen := GetLogicalDriveStrings(0, nil);
  Buff := StrAlloc(BuffLen);
  try
    ErrCode := GetLogicalDriveStrings(BuffLen, Buff);

    if ErrCode <> 0 then
    begin
      CurrDrive := Buff;
      repeat
        ResultList.Add(CurrDrive[0]);
        CurrDrive := StrEnd(CurrDrive) + 1;
      until CurrDrive^ = #0;
    end;

  finally
    StrDispose(Buff);
  end;
end;



end.

