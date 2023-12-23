unit DayZUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, ShellApi;


procedure OpenFolderInExplorer(const Folder: String);


implementation


procedure OpenFolderInExplorer(const Folder: String);
begin
  ShellExecute(0, 'explore', pChar(Folder), nil, nil, SW_SHOWNORMAL);
end;


end.

