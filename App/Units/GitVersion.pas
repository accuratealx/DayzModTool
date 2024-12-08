unit GitVersion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  fphttpclient,
  fpjson, jsonparser, jsonscanner;

type
  TGitHubVersion = record
    Version: String;
    URL: String;
  end;

function GetGithubVersion: TGitHubVersion;


implementation


function GetGithubVersion: TGitHubVersion;
const
  GITHUB_URL = 'https://api.github.com/repos/accuratealx/DayzModTool/releases/latest';
var
  s: String;
  Parser: TJSONParser;
  Obj: TJSONObject;
  Client: TFPHTTPClient;
begin
  Result.Version := '';
  Result.URL := '';

  Client := TFPHTTPClient.Create(nil);
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := True;
  try
    s := Client.Get(GITHUB_URL)
  except
    s := '{}';
    FreeAndNil(Client);
  end;
  Client.Free;

  try
    Parser := TJSONParser.Create(s, [joUTF8]);
    Obj := Parser.Parse as TJSONObject;
  except
    Parser.Free;
    Exit;
  end;

  Result.Version := Obj.Get('tag_name', '');
  Result.URL := Obj.Get('html_url', '');
end;



end.

