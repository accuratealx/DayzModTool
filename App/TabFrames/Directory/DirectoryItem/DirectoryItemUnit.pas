unit DirectoryItemUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Graphics;

type
  TDirectoryItemFrame = class(TFrame)
    imgIcon: TImage;
    lblPath: TLabel;
    lblCaption: TLabel;
    procedure FrameClick(Sender: TObject);
    procedure FrameDblClick(Sender: TObject);
  private
    FIconDirectory: String;
    FCaption: String;
    FPath: String;
    FIconName: String;
    FSelected: Boolean;

    FOnSelect: TNotifyEvent;

    function  GetIcon: TIcon;
    procedure SetCaption(ACaption: String);
    procedure SetPath(APath: String);
    procedure SetIconName(AFileName: String);
    procedure SetSelected(ASelected: Boolean);

    procedure DoOnSelect;
  public
    constructor Create(const AIconDirectory: String);
    constructor Create(const AIconDirectory, ACaption, APath, AIconName: String);
    constructor Create(const AIconDirectory, ASettingsString: String);
    destructor  Destroy; override;

    procedure ValueFromString(const AValue: String);
    function  ValueToString: String;

    procedure OpenDirectory;
    function  IsPathCorrect: Boolean;

    property Icon: TIcon read GetIcon;
    property Caption: String read FCaption write SetCaption;
    property Path: String read FPath write SetPath;
    property IconName: String read FIconName write SetIconName;
    property Selected: Boolean read FSelected write SetSelected;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  TDirectoryItemFrameArray = array of TDirectoryItemFrame;


implementation

{$R *.lfm}

uses
  DayZUtils;

const
  SEPARATOR = ';;;';


procedure TDirectoryItemFrame.FrameClick(Sender: TObject);
begin
  Selected := True;
end;


procedure TDirectoryItemFrame.FrameDblClick(Sender: TObject);
begin
  OpenDirectory;
end;


function TDirectoryItemFrame.GetIcon: TIcon;
begin
  Result := imgIcon.Picture.Icon;
end;


procedure TDirectoryItemFrame.SetCaption(ACaption: String);
begin
  FCaption := ACaption;
  lblCaption.Caption := FCaption;
end;


procedure TDirectoryItemFrame.SetPath(APath: String);
begin
  FPath := APath;
  lblPath.Caption := FPath;

  lblCaption.Enabled := IsPathCorrect;
end;


procedure TDirectoryItemFrame.SetIconName(AFileName: String);
var
  AIcon: TIcon;
begin
  if FileExists(FIconDirectory + AFileName) then
  begin
    FIconName := AFileName;

    AIcon := TIcon.Create;
    try
      AIcon.LoadFromFile(FIconDirectory + FIconName);
      imgIcon.Picture.Assign(AIcon);

    finally
      AIcon.Free;
    end;
  end;
end;


procedure TDirectoryItemFrame.SetSelected(ASelected: Boolean);
begin
  FSelected := ASelected;

  //Изменить фоновый цвет
  Self.ParentColor := not FSelected;
  Self.ParentBackground := not FSelected;

  if FSelected then
    Color := cl3DLight
  else
    Color := clDefault;

  if FSelected then
    DoOnSelect;
end;


procedure TDirectoryItemFrame.DoOnSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;


constructor TDirectoryItemFrame.Create(const AIconDirectory: String);
begin
  inherited Create(nil);

  FIconDirectory := IncludeTrailingBackslash(AIconDirectory);
end;


constructor TDirectoryItemFrame.Create(const AIconDirectory, ACaption, APath, AIconName: String);
begin
  Create(AIconDirectory);

  SetCaption(ACaption);
  SetPath(APath);
  SetIconName(AIconName);
end;


constructor TDirectoryItemFrame.Create(const AIconDirectory, ASettingsString: String);
begin
  Create(AIconDirectory);

  ValueFromString(ASettingsString);
end;


destructor TDirectoryItemFrame.Destroy;
begin
  inherited Destroy;
end;


procedure TDirectoryItemFrame.ValueFromString(const AValue: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := SEPARATOR;
  List.Text := AValue;
  try
    //Caption
    if List.Count > 0 then
      SetCaption(Trim(List.Strings[0]));

    //Path
    if List.Count > 1 then
      SetPath(Trim(List.Strings[1]));

    //IconName
    if List.Count > 2 then
      SetIconName(Trim(List.Strings[2]));

  finally
    List.Free;
  end;

end;


function TDirectoryItemFrame.ValueToString: String;
begin
  Result := FCaption + SEPARATOR + FPath + SEPARATOR + FIconName;
end;


procedure TDirectoryItemFrame.OpenDirectory;
begin
  OpenFolderInExplorer(FPath);
end;


function TDirectoryItemFrame.IsPathCorrect: Boolean;
begin
  Result := DirectoryExists(FPath);
end;



end.

