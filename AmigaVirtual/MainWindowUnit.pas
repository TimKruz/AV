unit MainWindowUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  SplashScreenUnit, Vcl.ExtCtrls, Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.Menus,
  System.ImageList, Vcl.ImgList, HelpWindowUnit, System.Win.TaskbarCore,
  Vcl.Taskbar, System.UITypes, IniFiles, Vcl.Tabs, Vcl.ComCtrls, Vcl.TabNotBk,
  Winapi.CommCtrl, Vcl.OleCtrls, SHDocVw, Vcl.ToolWin, ModuleOperationsUnit,
  Vcl.Buttons, System.StrUtils, Vcl.CheckLst, ModulesIOThreadsUnit, ShellApi,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  System.Zip, IdURI;

type
  TContentCategory = array [0 .. 2] of String;

const
  UserID = -1;
  ProgramVersion = 1;
  Category: TContentCategory = ('modules', 'virtuals', 'documents');

type

  TTabSheet = class(Vcl.ComCtrls.TTabSheet)
  private
    FColor: TColor;
    procedure SetColor(Value: TColor);
    procedure WMEraseBkGnd(var Msg: TWMEraseBkGnd); message WM_ERASEBKGND;
  public
    constructor Create(aOwner: TComponent); override;
    property Color: TColor read FColor write SetColor;
  end;

  TAccount = record
    Name, AuthToken: String;
    Authorized: Boolean;
    AuthMode: (forget, auto, guest, offline);
  end;

  TVirtual = record
    Name: String;
    Loaded: Boolean;
  end;

  TIntArray = array of Integer;

  TMainForm = class(TForm)
    MainTimer: TTimer;
    FormBackground: TImage;
    FormCaption: TLabel;

    ButtonImage: TImageList;
    TitleButtons: TPanel;
    CloseButton: TImage;
    HelpButton: TImage;
    SleepButton: TImage;
    LanguageButton: TImage;
    MenuButton: TImage;

    MainMenu: TPopupMenu;
    LanguageMenu: TPopupMenu;

    ChatBox: TMemo;

    MainPages: TPageControl;
    AuthorizationTab: TTabSheet;
    VirtualsTab: TTabSheet;
    ChatTab: TTabSheet;
    ExchangeCenterTab: TTabSheet;
    ForumTab: TTabSheet;
    SettingsTab: TTabSheet;

    TabImage: TImageList;

    ForumToolBar: TToolBar;
    ToolButtonBack: TToolButton;
    ToolButtonHome: TToolButton;
    ToolButtonAddress: TToolButton;
    ToolButtonBookmarks: TToolButton;
    ToolButtonSendFile: TToolButton;

    RegPanel: TPanel;
    AuthPanel: TPanel;
    AuthLabel: TLabel;
    RegLabel: TLabel;
    UsernameAuthEdit: TLabeledEdit;
    PasswordAuthEdit: TLabeledEdit;
    AuthVariantsGroup: TRadioGroup;
    UsernameRegEdit: TLabeledEdit;
    Password1RegEdit: TLabeledEdit;
    EmailRegEdit: TLabeledEdit;
    RegButton: TButton;
    RulesButton: TButton;
    AuthButton: TButton;
    RulesCheckBox: TCheckBox;
    AdulthoodCheckBox: TCheckBox;
    InfoProcAgrCheckBox: TCheckBox;

    ModulesMenu: TMenuItem;

    SplashScreenSettings: TRadioGroup;
    SettingsScrollBox: TScrollBox;
    SettingsToolBar: TToolBar;
    DefaultSettingsButton: TToolButton;
    SaveSettingsButton: TToolButton;
    LanguageNotice: TLabel;
    ChatEnterSettings: TRadioGroup;
    ShowLogo3secCheckBox: TCheckBox;

    ChatUserEnterBox: TEdit;

    VirtualInfoMemo: TMemo;
    InfoGroupBox: TGroupBox;
    VirtualsList: TListBox;
    ModulesGroupBox: TGroupBox;
    LoadVButton: TButton;
    VirtualsGroupBox: TGroupBox;
    RenameVButton: TButton;
    CreateVButton: TButton;
    DeleteVButton: TButton;
    SettingsVButton: TButton;
    DeleteModuleButton: TButton;
    ModuleManagerButton: TButton;

    ModuleMenu: TPopupMenu;
    ModuleInfoBtn: TMenuItem;
    ModuleStatisticsBtn: TMenuItem;
    ModuleHelpBtn: TMenuItem;
    ModuleStartBtn: TMenuItem;
    ModuleSleepBtn: TMenuItem;
    ModuleOpenWindowBtn: TMenuItem;
    ModuleSaveBtn: TMenuItem;
    ModuleResetBtn: TMenuItem;
    ModuleSettingsBtn: TMenuItem;
    ModulesList: TCheckListBox;
    N1: TMenuItem;
    StopVirtualBtn: TMenuItem;
    PoolShowBtn: TMenuItem;
    AuthSuccessPanel: TPanel;
    AuthSuccessLabel: TLabel;
    LogOffBtn: TButton;
    WebBrowser: TWebBrowser;
    BookmarksMenu: TPopupMenu;
    AddBookmarkButton: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    WebsiteLink: TMenuItem;
    DontKnowCheckBtn: TMenuItem;
    SaveVirtualInfoBtn: TButton;
    IdHTTP: TIdHTTP;
    UpdateProgramGroup: TGroupBox;
    CurrentProgramVersion: TLabel;
    ActualProgramVersion: TLabel;
    UpdateProgramButton: TButton;
    ContentInfoGroup: TGroupBox;
    ContentInfoMemo: TMemo;
    DownloadSourceButton: TButton;
    DownloadFilesButton: TButton;
    ContentListGroup: TGroupBox;
    ContentCategoryBox: TComboBox;
    ContentList: TListBox;
    FilterEdit: TLabeledEdit;
    SearchButton: TButton;
    PublishGroup: TGroupBox;
    PublishButton: TButton;
    CloudGroup: TGroupBox;
    LanguageBox: TGroupBox;
    CloudProvider: TComboBox;
    AutoCloudSaveCheck: TCheckBox;
    AutoCloudLoadCheck: TCheckBox;
    CloudUsername: TEdit;
    CloudLoginBtn: TButton;
    CloudSaveNow: TButton;
    CloudLoadNow: TButton;
    CloudPassword: TEdit;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure FormBackgroundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormBackgroundMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormBackgroundMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);

    procedure TitleBarButtonMouseEnter(Sender: TObject);
    procedure TitleBarButtonMouseLeave(Sender: TObject);
    procedure TitleBarButtonMouseClick(Sender: TObject);
    procedure TitleBarButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TitleBarButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure SetStatusMessage(Status: String; StatLife: Integer = 30);

    procedure MainTimerTimer(Sender: TObject);

    procedure LanguageMenuItemClick(Sender: TObject);

    procedure FormBackgroundClick(Sender: TObject);

    procedure MainPagesDrawTab(Control: TCustomTabControl; TabIndex: Integer;
      const Rect: TRect; Active: Boolean);

    procedure RegistrationCheckBoxClick(Sender: TObject);
    function AllRegistrationCheckBoxesChecked: Boolean;

    procedure SaveSettingsButtonClick(Sender: TObject);
    procedure DefaultSettingsButtonClick(Sender: TObject);
    procedure SplashScreenSettingsClick(Sender: TObject);
    procedure ChatEnterSettingsClick(Sender: TObject);

    procedure ChatUserEnterBoxExit(Sender: TObject);
    procedure ChatUserEnterBoxEnter(Sender: TObject);
    procedure ChatUserEnterBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ChatUserEnterBoxChange(Sender: TObject);
    procedure ShowLogo3secCheckBoxClick(Sender: TObject);

    procedure MainPagesChange(Sender: TObject);

    procedure CreateVButtonClick(Sender: TObject);
    procedure DeleteVButtonClick(Sender: TObject);
    procedure RenameVButtonClick(Sender: TObject);
    procedure VirtualsListClick(Sender: TObject);

    procedure DeleteModuleButtonClick(Sender: TObject);

    procedure ModuleMenuBtnClick(Sender: TObject);
    procedure ModulesListClick(Sender: TObject);
    procedure ModulesListClickCheck(Sender: TObject);
    procedure LoadVButtonClick(Sender: TObject);
    procedure StopVirtualBtnClick(Sender: TObject);
    procedure PoolShowBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AuthButtonClick(Sender: TObject);
    procedure LogOffBtnClick(Sender: TObject);
    procedure ToolButtonHomeClick(Sender: TObject);
    procedure ToolButtonBackClick(Sender: TObject);
    procedure ToolButtonAddressClick(Sender: TObject);
    procedure ToolButtonBookmarksClick(Sender: TObject);
    procedure WebsiteLinkClick(Sender: TObject);
    procedure DontKnowCheckBtnClick(Sender: TObject);
    procedure SaveVirtualInfoBtnClick(Sender: TObject);
    procedure IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure IdHTTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure UpdateProgramButtonClick(Sender: TObject);
    procedure ContentCategoryBoxChange(Sender: TObject);
    procedure ContentListClick(Sender: TObject);
    procedure DownloadFilesButtonClick(Sender: TObject);
    procedure VirtualsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ModulesListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ContentListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private const
    DefaultLanguage = 'Russian';
    Silent = true;
    LanguageFileExtension = '.language';
    SettingsFileName = 'Settings.ini';
    LangFileMinSize = 182;
    ModulesDir = 'Modules';
    LangFilesDir = 'Languages';
    VirtualsDir = 'Virtuals';
    ZipsDir = 'Archives';
    VirtualExt = '.info';
    SiteProtocol = 'http://';
    OfficialWebsite = 'avirtual.ru';
    ExchangeCenterPage = '/exchange/';
    ForumMainPage = 'http://www.avirtual.ru/forum/';
    VersionFile = 'av.version';

    TabColor: array [0 .. 6] of TColor = ($00C9AEFF, $00E8DCFF, $00DCF5FF,
      $00DCFFE6, $00F7FFDC, $00FFEADC, $00FFDCF8);
    ListColor: TColor = $00101010;

  var
    Dragging: Boolean;
    OldX, OldY: Integer;

    Starting, SleepMode: Boolean;

    StatusMessage: String;
    StatusMsgLife: Integer;

    EnterTextByChatMemo: Boolean;

    ModuleMenuButtons: array of TMenuItem;
    SelectedModule: TModule;
    SelectedModuleID: Integer;

    ForumBrowserFlags: OLEVariant;

    ProgramPath: String;

    NewVersion: Integer;

    WebBrowserFirstStart, ExchangeCenterFirstStart: Boolean;

    Downloading: Boolean;

    procedure ChangeChatEnterMode;
    procedure ChatInputTextChange;

    procedure LoadModules;

    procedure LoadLanguagesList;
    procedure DeployDefaultLanguages;
    procedure ChangeLanguageTo(Language: String; Silent: Boolean = false);

    procedure ResetButtonsImages;

    procedure SleepModeChange;

    procedure ResetFormCaption;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure ModulesMenuClick(Sender: TObject);

    procedure UpdateVirtualsList;
    procedure LoadVirtualInfo;

    procedure UpdateModulesList;

    procedure SendTextToPool(Text: String);

    procedure StopVirtual;

    procedure DownloadFile(From, SaveTo: String);
    procedure UnzipFiles(ArchiveName, UnzipTo: String);
    function NeedUpdateProgram: Boolean;
    procedure ShowVersionsInfo;
    procedure UpdateProgram;

    procedure UpdateContentList;
    procedure UpdateContentInfo;

    procedure OnWMMouseWheel(var Msg: TWMMOUSEWHEEL); message WM_MOUSEWHEEL;
  public
    LanguageData: array of String;
    SavedLanguage: String;
    SavedSplashScreenState: Integer;
    SavedShowLogo3sec: Boolean;
    SavedUserName: String;
    User: TAccount;
    AVirtual: TVirtual;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

function CutLastSymbols(str: String; SymNum: Integer = 4): String;
begin
  Result := Copy(str, 1, Length(str) - SymNum);
end;

function ExtractDLLName(s: String): String;
var
  i: Integer;
begin
  i := Pos('(', s);
  Result := Copy(s, i + 1, Length(s) - i - 1);
end;

{ TMainForm }

function TMainForm.AllRegistrationCheckBoxesChecked: Boolean;
begin
  Result := AdulthoodCheckBox.Checked and InfoProcAgrCheckBox.Checked and
    RulesCheckBox.Checked;
end;

procedure TMainForm.AuthButtonClick(Sender: TObject);
begin
  case AuthVariantsGroup.ItemIndex of
    0:
      ;
    1:
      ;
    2:
      begin
        if UsernameAuthEdit.Text = '' then
          User.Name := LanguageData[135]
        else
          User.Name := UsernameAuthEdit.Text;
        User.AuthToken := '';
        User.AuthMode := guest;
        User.Authorized := true;
        AuthSuccessPanel.Visible := true;
        AuthPanel.Visible := false;
        RegPanel.Visible := false;
        SetStatusMessage(LanguageData[144] + ' ' + User.Name);
      end;
    3:
      ;
  end;
  SavedUserName := User.Name;
end;

procedure TMainForm.ChangeChatEnterMode;
begin
  if EnterTextByChatMemo then
  begin
    ChatBox.Height := ChatTab.Height;
    ChatBox.ReadOnly := false;
    ChatUserEnterBox.Visible := false;
  end
  else
  begin
    ChatBox.Height := ChatTab.Height - ChatUserEnterBox.Height;
    ChatBox.ReadOnly := true;
    ChatUserEnterBox.Visible := true;
    ChatInputTextChange;
  end;
end;

procedure TMainForm.ChangeLanguageTo;
  function LanguageDataLoaded: Boolean;
  var
    T: TextFile;
    B: RawByteString;
  begin
    SetCurrentDir(ProgramPath + LangFilesDir);
    if FileExists(Language + LanguageFileExtension) then
    begin
      AssignFile(T, Language + LanguageFileExtension);
      Reset(T);
      SetLength(LanguageData, 1);
      LanguageData[0] := Language;
      while not eof(T) do
      begin
        SetLength(LanguageData, Length(LanguageData) + 1);
        ReadLn(T, B);
        LanguageData[Length(LanguageData) - 1] := UTF8ToWideString(B);
      end;
      CloseFile(T);
      if Length(LanguageData) - 1 >= LangFileMinSize then
        Result := true
      else
        Result := false;
    end
    else
      Result := false;
  end;

  procedure SetCaptions;
  begin
    with HelpForm do
    begin
      LoadHelpTexts;
      if CurrentTopic < HelpLast then
        OpenTopic(CurrentTopic)
      else
        OpenTopic(0);
    end;
    AddModulesHelpToMainProgramHelp;
    ChangeModulesLanguageToProgramLanguage;

    AuthorizationTab.Caption := LanguageData[18];
    VirtualsTab.Caption := LanguageData[19];
    ChatTab.Caption := LanguageData[20];
    ExchangeCenterTab.Caption := LanguageData[21];
    ForumTab.Caption := LanguageData[22];
    SettingsTab.Caption := LanguageData[23];

    AuthLabel.Caption := LanguageData[24];
    UsernameAuthEdit.EditLabel.Caption := LanguageData[25];
    UsernameRegEdit.EditLabel.Caption := LanguageData[25];
    PasswordAuthEdit.EditLabel.Caption := LanguageData[26];
    Password1RegEdit.EditLabel.Caption := LanguageData[26];
    EmailRegEdit.EditLabel.Caption := LanguageData[28];
    AuthVariantsGroup.Items.Clear;
    AuthVariantsGroup.Caption := LanguageData[29];
    AuthVariantsGroup.Items.Add(LanguageData[30]);
    AuthVariantsGroup.Items.Add(LanguageData[31]);
    AuthVariantsGroup.Items.Add(LanguageData[32]);
    AuthVariantsGroup.Items.Add(LanguageData[33]);
    AuthVariantsGroup.ItemIndex := 2;
    AuthButton.Caption := LanguageData[34];
    RulesButton.Caption := LanguageData[35];
    AdulthoodCheckBox.Caption := LanguageData[27];
    InfoProcAgrCheckBox.Caption := LanguageData[39];
    RulesCheckBox.Caption := LanguageData[36];
    RegButton.Caption := LanguageData[37];
    RegLabel.Caption := LanguageData[38];

    ModulesMenu.Caption := LanguageData[60];

    LanguageBox.Caption := LanguageData[61];
    LanguageNotice.Caption := LanguageData[0] + ' ' + LanguageData[62];

    SplashScreenSettings.Caption := LanguageData[63];
    with SplashScreenSettings.Items do
    begin
      Clear;
      Add(LanguageData[64]);
      Add(LanguageData[65]);
      Add(LanguageData[66]);
    end;
    SplashScreenSettings.ItemIndex := SavedSplashScreenState;
    ShowLogo3secCheckBox.Caption := LanguageData[75];
    ShowLogo3secCheckBox.Checked := SavedShowLogo3sec;

    DefaultSettingsButton.Caption := LanguageData[67];
    SaveSettingsButton.Caption := LanguageData[68];

    ChatUserEnterBox.Text := LanguageData[71];
    ChatEnterSettings.Caption := LanguageData[72];
    with ChatEnterSettings.Items do
    begin
      Clear;
      Add(LanguageData[73]);
      Add(LanguageData[74]);
    end;
    ChatEnterSettings.ItemIndex := Integer(not EnterTextByChatMemo);
    ChangeChatEnterMode;

    VirtualsGroupBox.Caption := LanguageData[82];
    ModulesGroupBox.Caption := LanguageData[83];
    InfoGroupBox.Caption := LanguageData[84];
    CreateVButton.Caption := LanguageData[85];
    LoadVButton.Caption := LanguageData[86];
    RenameVButton.Caption := LanguageData[87];
    DeleteVButton.Caption := LanguageData[88];
    SettingsVButton.Caption := LanguageData[89];
    LoadVirtualInfo;

    DeleteModuleButton.Caption := LanguageData[99];
    ModuleManagerButton.Caption := LanguageData[100];

    SaveVirtualInfoBtn.Caption := LanguageData[158];

    ModuleInfoBtn.Caption := LanguageData[105];
    ModuleStatisticsBtn.Caption := LanguageData[106];
    ModuleHelpBtn.Caption := LanguageData[107];
    ModuleStartBtn.Caption := LanguageData[108];
    ModuleSleepBtn.Caption := LanguageData[109];
    ModuleOpenWindowBtn.Caption := LanguageData[110];
    ModuleSaveBtn.Caption := LanguageData[111];
    ModuleResetBtn.Caption := LanguageData[112];
    ModuleSettingsBtn.Caption := LanguageData[113];

    StopVirtualBtn.Caption := LanguageData[129];
    PoolShowBtn.Caption := LanguageData[130];
    DontKnowCheckBtn.Caption := LanguageData[157];

    AuthSuccessLabel.Caption := LanguageData[137];
    LogOffBtn.Caption := LanguageData[138];

    ToolButtonBack.Caption := LanguageData[145];
    ToolButtonHome.Caption := LanguageData[146];
    ToolButtonAddress.Caption := LanguageData[147];
    ToolButtonBookmarks.Caption := LanguageData[148];
    ToolButtonSendFile.Caption := LanguageData[149];

    AddBookmarkButton.Caption := LanguageData[151];

    WebsiteLink.Caption := LanguageData[152] + ' ' + OfficialWebsite;

    ShowVersionsInfo;
    UpdateProgramButton.Caption := LanguageData[161];

    ContentInfoGroup.Caption := LanguageData[162];
    DownloadSourceButton.Caption := LanguageData[163];
    DownloadFilesButton.Caption := LanguageData[164];

    PublishGroup.Caption := LanguageData[165];
    PublishButton.Caption := LanguageData[166];

    ContentListGroup.Caption := LanguageData[167];
    with ContentCategoryBox.Items do
    begin
      Clear;
      Add(LanguageData[168]);
      Add(LanguageData[169]);
      Add(LanguageData[170]);
    end;
    ContentCategoryBox.ItemIndex := 0;
    UpdateContentList;

    FilterEdit.EditLabel.Caption := LanguageData[171];

    CloudGroup.Caption := LanguageData[174] + ' ' + LanguageData[1];
    CloudProvider.Text := LanguageData[175];
    CloudUsername.Text := LanguageData[176];
    CloudPassword.Text := LanguageData[177];
    CloudLoginBtn.Caption := LanguageData[178];
    AutoCloudSaveCheck.Caption := LanguageData[179];
    AutoCloudLoadCheck.Caption := LanguageData[180];
    CloudSaveNow.Caption := LanguageData[181];
    CloudLoadNow.Caption := LanguageData[182];
  end;

  procedure CheckMenuItem;
  var
    Item: TMenuItem;
  begin
    for Item in LanguageMenu.Items do
      if Item.Name = LanguageData[0] + 'Lang' then
        Item.Checked := true;
  end;

begin
  if LanguageDataLoaded then
  begin
    if not Silent then
      SetStatusMessage(LanguageData[2] + ' ' + LanguageData[0])
    else
      FormCaption.Caption := LanguageData[1];
    SetCaptions;
  end
  else
  begin
    if Language <> SavedLanguage then
      ChangeLanguageTo(SavedLanguage)
    else
      ChangeLanguageTo(DefaultLanguage);
    MessageDlg(LanguageData[3] + #13 + LanguageData[2] + ' ' + LanguageData[0] +
      '.', mtError, [mbOk], 0);
  end;
  CheckMenuItem;
end;

procedure TMainForm.ChatEnterSettingsClick(Sender: TObject);
begin
  case ChatEnterSettings.ItemIndex of
    0:
      EnterTextByChatMemo := true;
    1:
      EnterTextByChatMemo := false;
  end;
  ChangeChatEnterMode;
end;

procedure TMainForm.ChatInputTextChange;
begin
  with ChatUserEnterBox do
  begin
    if Text = LanguageData[71] then
      Font.Color := clGray
    else
      Font.Color := clBlack;
  end;
end;

procedure TMainForm.ChatUserEnterBoxChange(Sender: TObject);
begin
  ChatInputTextChange;
end;

procedure TMainForm.ChatUserEnterBoxEnter(Sender: TObject);
begin
  if ChatUserEnterBox.Text = LanguageData[71] then
    ChatUserEnterBox.Clear;
end;

procedure TMainForm.ChatUserEnterBoxExit(Sender: TObject);
begin
  if ChatUserEnterBox.Text = '' then
  begin
    ChatUserEnterBox.Text := LanguageData[71];
    ChangeChatEnterMode;
  end;
end;

procedure TMainForm.SendTextToPool(Text: String);
begin
  Pool.AddRecord(Text, UserID);
end;

procedure TMainForm.ChatUserEnterBoxKeyPress(Sender: TObject; var Key: Char);
begin
  with ChatUserEnterBox do
    if Key = #13 then
    begin
      SendTextToPool(Text);
      Clear;
      Key := #0;
    end;
end;

procedure TMainForm.ContentCategoryBoxChange(Sender: TObject);
begin
  UpdateContentList;
end;

procedure TMainForm.ContentListClick(Sender: TObject);
begin
  UpdateContentInfo;
end;

procedure TMainForm.ContentListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox, Canvas do
  begin
    Brush.Color := TabColor[4] - ListColor * ((Index + 1) mod 2);
    FillRect(Rect);
    Font.Color := TColor(clBlack);
    TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TMainForm.CreateVButtonClick(Sender: TObject);
var
  s: String;
  e: Integer;
begin
  s := InputBox(LanguageData[76], LanguageData[77], '');
  if s <> '' then
  begin
    SetCurrentDir(ProgramPath);
    if not DirectoryExists(VirtualsDir) then
      CreateDir(VirtualsDir);
    SetCurrentDir(VirtualsDir);
    FileClose(FileCreate(s + VirtualExt));
    e := GetLastError;
    if e > 0 then
      MessageDlg(LanguageData[78] + #13 + LanguageData[79] + ' ' + IntToStr(e),
        mtError, [mbOk], 0)
    else
      SetStatusMessage(LanguageData[80] + ' ' + s + ' ' + LanguageData[81]);
  end;
  UpdateVirtualsList;
  LoadVirtualInfo;
end;

procedure TMainForm.DefaultSettingsButtonClick(Sender: TObject);
begin
  SavedLanguage := 'Russian';
  ChangeLanguageTo(SavedLanguage);
  SplashScreenSettings.ItemIndex := 0;
  ChatEnterSettings.ItemIndex := 1;
  SaveSettings;
end;

procedure TMainForm.DeleteModuleButtonClick(Sender: TObject);
var
  i: Integer;
  MF: String;
  M: TModule;
  MustUpdate: Boolean;
begin
  MustUpdate := false;
  SetCurrentDir(ProgramPath + ModulesDir);
  with ModulesList do
    if ItemIndex = -1 then
      MessageDlg(LanguageData[101], mtInformation, [mbOk], 0)
    else
      for i := 0 to Items.Count - 1 do
        if Selected[i] then
          if MessageDlg(LanguageData[102] + ' ' + Items[i] + '?',
            mtConfirmation, [mbYes, mbNo], 0) = mrYes then
          begin
            MF := ExtractDLLName(Items[i]);
            if FileExists(MF) then
            begin
              M := FindModuleByFileName(MF);
              if M.WindowState <> opened then
                FreeModule(M);
              if DeleteFile(ExtractDLLName(Items[i])) then
                MustUpdate := true
              else
                MessageDlg(LanguageData[103] + ' ' + MF + #13 + LanguageData
                  [70], mtError, [mbOk], 0);
            end
            else
              MessageDlg(MF + ' ' + LanguageData[104], mtError, [mbOk], 0);
          end;
  if MustUpdate then
    UpdateModulesList;
end;

procedure TMainForm.DeleteVButtonClick(Sender: TObject);
  procedure ClearFiles(Name: String);
  var
    R: Integer;
    SR: TSearchRec;
  begin
    SetCurrentDir(ProgramPath + VirtualsDir);
    R := FindFirst(Name + '.*', 0, SR);
    while R = 0 do
    begin
      DeleteFile(SR.Name);
      R := FindNext(SR);
    end;
    FindClose(SR);
  end;

begin
  if (VirtualsList.Items.Count > 0) and (VirtualsList.ItemIndex > -1) then
  begin
    if MessageDlg(LanguageData[90] + ' ' + VirtualsList.Items
      [VirtualsList.ItemIndex] + '?', mtConfirmation, [mbYes, mbNo], 0) = mrYes
    then
    begin
      ClearFiles(VirtualsList.Items[VirtualsList.ItemIndex]);
      UpdateVirtualsList;
      LoadVirtualInfo;
    end;
  end
  else
    MessageDlg(LanguageData[91], mtWarning, [mbOk], 0);
end;

procedure TMainForm.DeployDefaultLanguages;
  procedure DeployLanguage(LanguageName: String);
  var
    ResHandle, MemHandle: THandle;
    MemStream: TMemoryStream;
    ResPtr: PByte;
    ResSize: Longint;
    ResName: String;
    i: Integer;
  begin
    ResName := '';
    for i := 1 to Length(LanguageName) do
      ResName := ResName + UpCase(LanguageName[i]);
    ResName := ResName + '_LP';
    ResHandle := FindResource(HInstance, PWideChar(ResName), RT_RCDATA);
    if ResHandle = 0 then
    begin
      ShowMessage('Default language "' + LanguageName + '" not found. (' +
        ResName + ')');
      exit;
    end;
    MemHandle := LoadResource(HInstance, ResHandle);
    ResPtr := LockResource(MemHandle);
    MemStream := TMemoryStream.Create;
    ResSize := SizeOfResource(HInstance, ResHandle);
    MemStream.SetSize(ResSize);
    MemStream.Write(ResPtr^, ResSize);
    MemStream.Seek(0, 0);
    MemStream.SaveToFile(LangFilesDir + '/' + LanguageName +
      LanguageFileExtension);
    FreeResource(MemHandle);
    MemStream.Destroy;
  end;

begin
  if not DirectoryExists(LangFilesDir) then
    CreateDir(LangFilesDir);
  DeployLanguage('Russian');
  DeployLanguage('English');
end;

procedure TMainForm.DontKnowCheckBtnClick(Sender: TObject);
begin
  DontKnowCheckBtn.Checked := not DontKnowCheckBtn.Checked;
end;

procedure TMainForm.DownloadFile(From, SaveTo: String);
var
  LoadStream: TMemoryStream;
begin
  Downloading := true;
  LoadStream := TMemoryStream.Create;
  IdHTTP.Get(TIdURI.URLEncode(From), LoadStream);
  LoadStream.SaveToFile(SaveTo);
  LoadStream.Free;
  Downloading := false;
  SplashScreen.Close;
end;

procedure TMainForm.DownloadFilesButtonClick(Sender: TObject);
var
  Dir, FileName: String;
begin
  SetCurrentDir(ProgramPath);
  Dir := Category[ContentCategoryBox.ItemIndex];
  Dir := UpCase(Dir[1]) + Copy(Dir, 2, Length(Dir));
  if not DirectoryExists(Dir) then
    CreateDir(Dir);
  SetCurrentDir(Dir);
  FileName := ContentList.Items[ContentList.ItemIndex];
  if FileExists(FileName) then
    MessageDlg(LanguageData[172], mtInformation, [mbOk], 0)
  else
  begin
    DownloadFile(SiteProtocol + OfficialWebsite + ExchangeCenterPage + '?c=' +
      Category[ContentCategoryBox.ItemIndex] + '&f=' + FileName + '&l=' +
      LanguageData[0], FileName);
    if Copy(FileName, Length(FileName) - 2, 3) = 'zip' then
      UnzipFiles(FileName, GetCurrentDir);
    case ContentCategoryBox.ItemIndex of
      0:
        UpdateModulesList;
      1:
        UpdateVirtualsList;
    end;
    SetStatusMessage(LanguageData[173] + ' ' + ProgramPath + Dir + '\');
  end;
end;

procedure TMainForm.FormBackgroundClick(Sender: TObject);
begin
  MainPages.Visible := true;
end;

procedure TMainForm.FormBackgroundMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    OldX := X;
    OldY := Y;
    Dragging := true;
  end;
end;

procedure TMainForm.FormBackgroundMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Dragging then
  begin
    Left := Left + X - OldX;
    Top := Top + Y - OldY;
    HelpForm.Repaint;
  end;
end;

procedure TMainForm.FormBackgroundMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Dragging := false;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveSettings;
  if AVirtual.Loaded then
    StopVirtual;
  CloseAllWindows;
  FreeModules;
  CanClose := true;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Starting := true;
  SleepMode := false;
end;

procedure TMainForm.FormShow(Sender: TObject);
  procedure OfferAssistanceForTheFirstStart;
  begin
    SetCurrentDir(ProgramPath);
    if not FileExists(SettingsFileName) then
      if MessageDlg('RUSSIAN' + #13 + 'Похоже, это твой первый запуск прораммы.'
        + #13 + 'Хочешь для начала ознакомиться со справкой?' + #13 + '---' +
        #13 + 'ENGLISH' + #13 +
        'Seems your are starting program for the first time.' + #13 +
        'Do you want read the help file before beginning the work?',
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        MessageDlg('RUSSIAN' + #13 +
          'Ты всегда можешь сменить язык справки, нажав на ЗЕЛЁНУЮ кнопку, ' +
          #13 + 'расположенную на заголовке основного окна программы.' + #13 +
          '---' + #13 + 'ENGLISH' + #13 +
          'You always can change the language of the help file then press ' +
          'GREEN button,' + #13 +
          'located on the header of the main program window.', mtInformation,
          [mbOk], 0);
        HelpForm.Show;
      end;
  end;

  procedure SetUpTabs;
  begin
    MainPages.Brush.Color := TabColor[0];
    AuthorizationTab.Color := TabColor[1];
    VirtualsTab.Color := TabColor[2];
    ChatTab.Color := TabColor[3];
    ExchangeCenterTab.Color := TabColor[4];
    ForumTab.Color := TabColor[5];
    SettingsTab.Color := TabColor[6];
    MainPages.ActivePage := AuthorizationTab;
  end;

  procedure CheckUpdates;
  begin
    if NeedUpdateProgram then
      if MessageDlg('RUSSIAN' + #13 +
        'Программе требуется обновление. Обновить сейчас?' + #13 +
        'Текущая версия - ' + IntToStr(ProgramVersion) + #13 + 'Новая версия - '
        + IntToStr(NewVersion) + #13 + '---' + #13 + 'ENGLISH' + #13 +
        'This program requires an update. Update now?' + #13 +
        'Current version - ' + IntToStr(ProgramVersion) + #13 + 'New Version - '
        + IntToStr(NewVersion), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        UpdateProgram;
  end;

begin
  if Starting then
  begin
    ProgramPath := ExtractFilePath(Application.ExeName);
    AVirtual.Loaded := false;
    User.Authorized := false;
    WebBrowserFirstStart := true;
    ExchangeCenterFirstStart := true;
    ForumBrowserFlags := 4;
    LoadSettings;
    UsernameAuthEdit.Text := SavedUserName;
    if SavedSplashScreenState < 2 then
      SplashScreen.Show;
    DeployDefaultLanguages;
    LoadLanguagesList;
    ChangeLanguageTo(SavedLanguage, Silent);
    FindModules(ProgramPath + ModulesDir);
    LoadModules;
    AddModulesHelpToMainProgramHelp;
    ResetButtonsImages;
    SplashScreen.SetProgress(100);
    if SavedShowLogo3sec and (SavedSplashScreenState < 2) then
      Sleep(3000);
    SplashScreen.Close;
    Starting := false;
    SetUpTabs;
    CheckUpdates;
    OfferAssistanceForTheFirstStart;
    Randomize;
    SetStatusMessage(LanguageData[Random(5) + 13]);
  end;
end;

procedure TMainForm.IdHTTPWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if Downloading then
    if SavedSplashScreenState < 2 then
      IncAndUpdateProgress(AWorkCount);
end;

procedure TMainForm.IdHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  if Downloading then
    if SavedSplashScreenState < 2 then
    begin
      ProgressSteps := AWorkCountMax;
      ProgressInt := 0;
      SplashScreen.Show;
    end;
end;

procedure TMainForm.IdHTTPWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if Downloading then
    if SavedSplashScreenState < 2 then
      SplashScreen.Close;
end;

procedure TMainForm.LoadLanguagesList;
var
  LangMenuItem: TMenuItem;
  R: Integer;
  SR: TSearchRec;

  procedure AddButton(BtnName: String);
  begin
    LangMenuItem := TMenuItem.Create(LanguageMenu);
    LangMenuItem.Caption := BtnName;
    LangMenuItem.Name := BtnName + 'Lang';
    LangMenuItem.OnClick := LanguageMenuItemClick;
    LangMenuItem.RadioItem := true;
    LanguageMenu.Items.Add(LangMenuItem);
  end;

begin
  LanguageMenu.Items.Clear;
  SetCurrentDir('Languages');
  R := FindFirst('*' + LanguageFileExtension, 0, SR);
  while R = 0 do
  begin
    AddButton(CutLastSymbols(SR.Name, Length(LanguageFileExtension)));
    R := FindNext(SR);
  end;
  FindClose(SR);
end;

procedure TMainForm.LoadModules;
var
  M: TModule;

  procedure AddButton(BtnCap, BtnName: String);
  var
    Btn: TMenuItem;
  begin
    Btn := TMenuItem.Create(ModulesMenu);
    Btn.Caption := BtnCap + ' (' + BtnName + ')';
    Btn.Name := CutLastSymbols(BtnName) + 'MBtn';
    Btn.OnClick := ModulesMenuClick;
    ModulesMenu.Add(Btn);
    SetLength(ModuleMenuButtons, Length(ModuleMenuButtons) + 1);
    ModuleMenuButtons[Length(ModuleMenuButtons) - 1] := Btn;
    ModulesList.Items.Add(BtnCap + ' (' + BtnName + ')');
  end;

  procedure ClearMenus;
  var
    i: Integer;
  begin
    for i := 0 to Length(ModuleMenuButtons) - 1 do
      ModuleMenuButtons[i].Free;
    SetLength(ModuleMenuButtons, 0);
    ModulesList.Clear;
  end;

begin
  ModuleOperationsUnit.LoadModules;
  ClearMenus;
  for M in Module do
    if M.Handle > 0 then
      AddButton('[' + M.ControlCodeFormated + '] ' + M.Name, M.FileName);
  ModulesList.CheckAll(cbChecked);
end;

procedure TMainForm.LoadSettings;
var
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(ProgramPath + SettingsFileName);
  SavedLanguage := Settings.ReadString('Main Settings', 'Language',
    DefaultLanguage);
  SavedSplashScreenState := Settings.ReadInteger('Main Settings',
    'Splash Screen Mode', 0);
  SavedShowLogo3sec := Settings.ReadBool('Main Settings',
    'Show Logo 3sec', true);
  EnterTextByChatMemo := Settings.ReadBool('Main Settings',
    'Chat Enter Mode', false);
  SavedUserName := Settings.ReadString('Account settings', 'Last nickname', '');
  Settings.Free;
end;

procedure TMainForm.LoadVButtonClick(Sender: TObject);
type
  CResult = (no_collision, collision);

  function CheckModulesCollision: CResult;
  var
    i, j: Integer;
    CC: String;
  begin
    Result := no_collision;
    with ModulesList do
      for i := 0 to Items.Count - 1 do
        if Checked[i] then
        begin
          CC := FindModuleByFileName(ExtractDLLName(Items[i])).ControlCode;
          for j := 0 to Items.Count - 1 do
            if Checked[j] and (i <> j) and
              (CC = FindModuleByFileName(ExtractDLLName(Items[j])).ControlCode)
            then
              Result := collision;
        end;
  end;

  function GetSelectedModules: TIntArray;
  var
    i, ID: Integer;
    FN: String;
    M: TModule;
  begin
    SetLength(Result, 0);
    SetLength(ModuleSelected, 0);
    with ModulesList do
      for i := 0 to Items.Count - 1 do
        if Checked[i] then
        begin
          FN := ExtractDLLName(Items[i]);
          M := FindModuleByFileName(FN);
          if not(M.MType in [erroneous_or_empty, parser]) then
          begin
            ID := Length(Result);
            SetLength(Result, ID + 1);
            Result[ID] := FindModuleIDByFileName(FN);

            ID := Length(ModuleSelected);
            SetLength(ModuleSelected, ID + 1);
            ModuleSelected[ID] := M;
          end;
        end;
  end;

  function NothingSelected: Boolean;
  var
    i: Integer;
  begin
    Result := true;
    for i := 0 to ModulesList.Items.Count - 1 do
      if ModulesList.Checked[i] then
      begin
        Result := false;
        exit;
      end;
  end;

begin
  if VirtualsList.Items.Count = 0 then
  begin
    MessageDlg(LanguageData[143], mtInformation, [mbOk], 0);
    exit;
  end;
  if ModulesList.Items.Count = 0 then
  begin
    MessageDlg(LanguageData[142], mtInformation, [mbOk], 0);
    exit;
  end;
  if NothingSelected then
  begin
    MessageDlg(LanguageData[141], mtInformation, [mbOk], 0);
    exit;
  end;
  if CheckModulesCollision = collision then
    if MessageDlg(LanguageData[134], mtInformation, [mbOk, mbIgnore], 0)
      in [mrOk, mrCancel] then
      exit;
  AVirtual.Loaded := true;
  AVirtual.Name := VirtualsList.Items[VirtualsList.ItemIndex];
  StopVirtualBtn.Visible := true;
  PoolShowBtn.Visible := true;
  DontKnowCheckBtn.Visible := true;
  StartSelectedModules(GetSelectedModules);
  CountOutputModules;
  Pool := TPool.Create;
  SetStatusMessage(LanguageData[153] + ' ' + AVirtual.Name + '. ' + LanguageData
    [60] + ' - ' + IntToStr(Length(ModuleSelected)) + ' ' + LanguageData[154] +
    ' ' + LanguageData[139] + ' - ' + IntToStr(StartThreads) + ' ' +
    LanguageData[154]);
  MainPages.ActivePageIndex := 2;
end;

procedure TMainForm.LoadVirtualInfo;
begin
  VirtualInfoMemo.Clear;
  with VirtualsList do
    if (Items.Count > 0) and (ItemIndex > -1) then
      VirtualInfoMemo.Lines.LoadFromFile(ExtractFileDir(Application.ExeName) +
        '/' + VirtualsDir + '/' + Items[ItemIndex] + VirtualExt)
    else
      VirtualInfoMemo.Lines.Add(LanguageData[94]);
end;

procedure TMainForm.LogOffBtnClick(Sender: TObject);
begin
  AuthSuccessPanel.Visible := false;
  AuthPanel.Visible := true;
  RegPanel.Visible := true;
  User.Name := '';
  User.Authorized := false;
end;

procedure TMainForm.MainPagesChange(Sender: TObject);
begin
  if (MainPages.ActivePageIndex in [1 .. 5]) and not User.Authorized then
  begin
    MainPages.ActivePageIndex := 0;
    MessageDlg(LanguageData[136], mtInformation, [mbOk], 0);
  end
  else
    case MainPages.ActivePageIndex of
      1:
        if AVirtual.Loaded then
          MainPages.ActivePageIndex := 2
        else
        begin
          UpdateVirtualsList;
          LoadVirtualInfo;
        end;
      2:
        if not AVirtual.Loaded then
          MainPages.ActivePageIndex := 1;
      3:
        if ExchangeCenterFirstStart then
        begin
          UpdateContentList;
          ExchangeCenterFirstStart := false;
        end;
      4:
        if WebBrowserFirstStart then
        begin
          WebBrowser.Navigate(ForumMainPage, ForumBrowserFlags);
          WebBrowserFirstStart := false;
        end;
    end;
end;

procedure TMainForm.MainPagesDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  AText: String;
  APoint: TPoint;
  W, H: Integer;
  Index: Integer;
begin
  Index := TabIndex;
  with (Control as TPageControl), Canvas do
  begin
    Brush.Color := TTabSheet(Pages[Index]).Color;
    FillRect(Rect);
    AText := Pages[Index].Caption;
    H := Rect.bottom - Rect.Top;
    W := Rect.right - Rect.Left;
    APoint.X := W div 2 - TextWidth(AText) div 2 + 8;
    APoint.Y := H div 2 - TextHeight(AText) div 2;
    TextRect(Rect, Rect.Left + APoint.X, Rect.Top + APoint.Y, AText);
    TabImage.Draw(Canvas, Rect.Left + 1, Rect.Top + 1, Index);
  end;
end;

procedure TMainForm.MainTimerTimer(Sender: TObject);
begin
  if StatusMsgLife > 0 then
    Dec(StatusMsgLife)
  else if StatusMessage <> '' then
  begin
    Delete(StatusMessage, 1, 1);
    SetStatusMessage(StatusMessage, 0);
  end
  else
    ResetFormCaption;
end;

procedure TMainForm.ModulesListClick(Sender: TObject);
var
  X: Integer;
begin
  X := ModulesList.ItemIndex;
  if X > -1 then
    if ModulesList.State[X] = cbChecked then
      ModulesList.State[X] := cbUnchecked
    else
      ModulesList.State[X] := cbChecked
  else
    ModulesList.CheckAll(cbChecked);
end;

procedure TMainForm.ModulesListClickCheck(Sender: TObject);
var
  X: Integer;
begin
  X := ModulesList.ItemIndex;
  if ModulesList.State[X] = cbChecked then
    ModulesList.State[X] := cbUnchecked
  else
    ModulesList.State[X] := cbChecked;
end;

procedure TMainForm.ModulesListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TCheckListBox, Canvas do
  begin
    Brush.Color := TabColor[2] - ListColor * ((Index + 1) mod 2);
    FillRect(Rect);
    Font.Color := TColor(clBlack);
    TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TMainForm.ModulesMenuClick(Sender: TObject);
  procedure HideUnavailableButtons(M: TModule);
  begin
    ModuleStatisticsBtn.Visible := not M.Statistics.Empty;
    ModuleHelpBtn.Visible := M.HelpTopicID > 0;
    ModuleStartBtn.Visible := @M.Start <> nil;
    ModuleSleepBtn.Visible := ((@M.Sleep <> nil) and (M.ModuleState = working))
      or (@M.WakeUp <> nil) and (M.ModuleState = sleeping);
    ModuleOpenWindowBtn.Visible :=
      ((@M.OpenWindow <> nil) and (M.WindowState = closed)) or
      (@M.CloseWindow <> nil) and (M.WindowState = opened);
    ModuleSaveBtn.Visible := @M.SaveData <> nil;
    ModuleResetBtn.Visible := @M.Reset <> nil;
    ModuleSettingsBtn.Visible := false;
  end;

  procedure RenameButtons(M: TModule);
  begin
    ModuleSleepBtn.Caption := IfThen(M.ModuleState = working, LanguageData[109],
      LanguageData[115]);
    ModuleOpenWindowBtn.Caption := IfThen(M.WindowState = closed,
      LanguageData[110], LanguageData[116]);
  end;

var
  B: TMenuItem;
begin
  B := TMenuItem(Sender);
  SelectedModule := FindModuleByFileName(CutLastSymbols(B.Name) + '.dll');
  SelectedModuleID := FindModuleIDByFileName(CutLastSymbols(B.Name) + '.dll');
  HideUnavailableButtons(SelectedModule);
  RenameButtons(SelectedModule);
  ModuleMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

function TMainForm.NeedUpdateProgram: Boolean;
var
  s: String;
begin
  try
    s := IdHTTP.Get(SiteProtocol + OfficialWebsite + '/' + VersionFile);
  except
    on e: Exception do
    begin
      MessageDlg('RUSSIAN' + #13 + 'Не удаётся подключиться к серверу.' + #13 +
        '---' + #13 + 'ENGLISH' + #13 + 'Can''t connect to server.', mtError,
        [mbOk], 0);
      s := IntToStr(ProgramVersion);
    end;
  end;
  NewVersion := StrToInt(s);
  if NewVersion > ProgramVersion then
    Result := true
  else
    Result := false;
  ShowVersionsInfo;
  UpdateProgramButton.Enabled := Result;
end;

procedure TMainForm.OnWMMouseWheel(var Msg: TWMMOUSEWHEEL);
{ var
  zDelta: Integer; }
begin
  { inherited;
    if MainPages.ActivePageIndex = 5 then
    begin
    if Msg.WheelDelta > 0 then
    zDelta := 10
    else
    zDelta := -10;
    with SettingsScrollBox do
    begin
    if ((VertScrollBar.Position = 0) and (zDelta > 0)) or
    ((VertScrollBar.Position = VertScrollBar.Range - ClientHeight) and
    (zDelta < 0)) then
    exit;
    ScrollBy(0, zDelta);
    VertScrollBar.Position := VertScrollBar.Position - zDelta;
    end;
    end; }
end;

procedure TMainForm.PoolShowBtnClick(Sender: TObject);
begin
  PoolShowBtn.Checked := not PoolShowBtn.Checked;
end;

procedure TMainForm.ModuleMenuBtnClick(Sender: TObject);
var
  B: TMenuItem;
begin
  B := TMenuItem(Sender);
  if B.Name = 'ModuleInfoBtn' then
    HelpForm.OpenTopic(HelpForm.AddTopic(LanguageData[117] + ' "' +
      SelectedModule.Name + '"', GetInfoAbout(SelectedModule)), false)
  else if B.Name = 'ModuleStatisticsBtn' then
    HelpForm.OpenTopic(HelpForm.AddTopic(LanguageData[118] + ' "' +
      SelectedModule.Name + '"', GetStatisticsSummary(SelectedModule)), false)
  else if B.Name = 'ModuleHelpBtn' then
    HelpForm.OpenTopic(SelectedModule.HelpTopicID, false)
  else if B.Name = 'ModuleStartBtn' then
    StartModule(SelectedModuleID)
  else if B.Name = 'ModuleSleepBtn' then
    ToogleModuleSleepMode(SelectedModuleID)
  else if B.Name = 'ModuleOpenWindowBtn' then
    ToogleModuleWindow(SelectedModuleID)
  else if B.Name = 'ModuleSaveBtn' then
    SelectedModule.SaveData
  else if B.Name = 'ModuleResetBtn' then
    SelectedModule.Reset
  else if B.Name = 'ModuleSettingsBtn' then
    MainPages.TabIndex := 5;
end;

procedure TMainForm.RegistrationCheckBoxClick(Sender: TObject);
var
  CB: TCheckBox;
begin
  CB := TCheckBox(Sender);
  if CB.Name = 'AdulthoodCheckBox' then
    InfoProcAgrCheckBox.Enabled := true
  else if CB.Name = 'InfoProcAgrCheckBox' then
    RulesButton.Enabled := true
  else if CB.Name = 'RulesButton' then
  begin
    RulesCheckBox.Enabled := true;
    HelpForm.ShowWithStrings(HelpForm.GenerateHelpTextForMemo(40, 59));
  end;
  RegButton.Enabled := AllRegistrationCheckBoxesChecked;
end;

procedure TMainForm.RenameVButtonClick(Sender: TObject);
  procedure RenameFiles(OldName, NewName: String);
  var
    R: Integer;
    SR: TSearchRec;

    function GetExtension(FileName: String): String;
    var
      buf: String;
      ps: Integer;
    begin
      buf := FileName;
      ps := Pos('.', buf);
      Delete(buf, 1, ps - 1);
      Result := buf;
    end;

  begin
    SetCurrentDir(ProgramPath + VirtualsDir);
    R := FindFirst(OldName + '.*', 0, SR);
    while R = 0 do
    begin
      RenameFile(SR.Name, NewName + GetExtension(SR.Name));
      R := FindNext(SR);
    end;
    FindClose(SR);
  end;

var
  NewName: String;
begin
  if (VirtualsList.Items.Count > 0) and (VirtualsList.ItemIndex > -1) then
  begin
    NewName := InputBox(LanguageData[92], LanguageData[93] + ' ' +
      VirtualsList.Items[VirtualsList.ItemIndex] + ':',
      VirtualsList.Items[VirtualsList.ItemIndex]);
    if NewName <> '' then
    begin
      RenameFiles(VirtualsList.Items[VirtualsList.ItemIndex], NewName);
      UpdateVirtualsList;
      LoadVirtualInfo;
    end;
  end
  else
    MessageDlg(LanguageData[91], mtWarning, [mbOk], 0);
end;

procedure TMainForm.ResetButtonsImages;
begin
  ButtonImage.Draw(MenuButton.Canvas, 0, 0, 0);
  ButtonImage.Draw(LanguageButton.Canvas, 0, 0, 1);
  ButtonImage.Draw(SleepButton.Canvas, 0, 0, 2);
  ButtonImage.Draw(HelpButton.Canvas, 0, 0, 3);
  ButtonImage.Draw(CloseButton.Canvas, 0, 0, 4);
  MenuButton.Repaint;
  LanguageButton.Repaint;
  SleepButton.Repaint;
  HelpButton.Repaint;
  CloseButton.Repaint;
end;

procedure TMainForm.ResetFormCaption;
begin
  FormCaption.Caption := LanguageData[1];
end;

procedure TMainForm.SaveSettings;
var
  Settings: TIniFile;
begin
  Settings := TIniFile.Create(ProgramPath + SettingsFileName);
  Settings.WriteString('Main Settings', 'Language', LanguageData[0]);
  Settings.WriteInteger('Main Settings', 'Splash Screen Mode',
    SavedSplashScreenState);
  Settings.WriteBool('Main Settings', 'Show Logo 3sec', SavedShowLogo3sec);
  Settings.WriteBool('Main Settings', 'Chat Enter Mode', EnterTextByChatMemo);
  Settings.WriteString('Account settings', 'Last nickname', SavedUserName);
  Settings.UpdateFile;
  Settings.Free;
end;

procedure TMainForm.SaveSettingsButtonClick(Sender: TObject);
begin
  SaveSettings;
end;

procedure TMainForm.SaveVirtualInfoBtnClick(Sender: TObject);
begin
  SetCurrentDir(ProgramPath + VirtualsDir);
  VirtualInfoMemo.Lines.SaveToFile(VirtualsList.Items[VirtualsList.ItemIndex] +
    VirtualExt);
end;

procedure TMainForm.SetStatusMessage;
begin
  StatusMessage := Status;
  StatusMsgLife := StatLife;
  FormCaption.Caption := LanguageData[1] + ': ' + StatusMessage;
end;

procedure TMainForm.ShowLogo3secCheckBoxClick(Sender: TObject);
begin
  SavedShowLogo3sec := ShowLogo3secCheckBox.Checked;
end;

procedure TMainForm.ShowVersionsInfo;
begin
  CurrentProgramVersion.Caption := LanguageData[159] + ' ' +
    IntToStr(ProgramVersion);
  ActualProgramVersion.Caption := LanguageData[160] + ' ' +
    IntToStr(NewVersion);
end;

procedure TMainForm.SleepModeChange;
begin
  SleepMode := not SleepMode;
  if SleepMode then
  begin
    MainForm.Height := 27;
    SetStatusMessage(LanguageData[11]);
    HelpForm.Close;
  end
  else
  begin
    MainForm.Height := 400;
    SetStatusMessage(LanguageData[12]);
  end;
end;

procedure TMainForm.SplashScreenSettingsClick(Sender: TObject);
begin
  SavedSplashScreenState := SplashScreenSettings.ItemIndex;
  if SavedSplashScreenState = 2 then
    ShowLogo3secCheckBox.Enabled := false
  else
    ShowLogo3secCheckBox.Enabled := true;
end;

procedure TMainForm.StopVirtual;
begin
  FreeThreads;
  Pool.Free;
  AVirtual.Loaded := false;
end;

procedure TMainForm.StopVirtualBtnClick(Sender: TObject);
begin
  StopVirtual;
  if MainPages.ActivePageIndex = 2 then
    MainPages.ActivePageIndex := 1;
  StopVirtualBtn.Visible := false;
  PoolShowBtn.Visible := false;
  DontKnowCheckBtn.Visible := false;
  CloseAllWindows;
  SaveAllData;
  ChatBox.Lines.Add('---');
  SetStatusMessage(LanguageData[155]);
end;

procedure TMainForm.TitleBarButtonMouseClick(Sender: TObject);
var
  Button: TImage;
begin
  Button := TImage(Sender);
  if Button.Name = 'MenuButton' then
    MainMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  if Button.Name = 'LanguageButton' then
    LanguageMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  if Button.Name = 'SleepButton' then
    SleepModeChange;
  if Button.Name = 'HelpButton' then
    HelpForm.Show;
  if Button.Name = 'CloseButton' then
    if AVirtual.Loaded or not AreWindowsClosed then
      MessageDlg(LanguageData[140], mtInformation, [mbOk], 0)
    else
      Close;
end;

procedure TMainForm.TitleBarButtonMouseDown;
var
  Btn: TImage;
begin
  Btn := TImage(Sender);
  if Btn.Name = 'MenuButton' then
    ButtonImage.Draw(Btn.Canvas, 0, 0, 10);
  if Btn.Name = 'LanguageButton' then
    ButtonImage.Draw(Btn.Canvas, 0, 0, 11);
  if Btn.Name = 'SleepButton' then
    ButtonImage.Draw(Btn.Canvas, 0, 0, 12);
  if Btn.Name = 'HelpButton' then
    ButtonImage.Draw(Btn.Canvas, 0, 0, 13);
  if Btn.Name = 'CloseButton' then
    ButtonImage.Draw(Btn.Canvas, 0, 0, 14);
  Btn.Repaint;
end;

procedure TMainForm.TitleBarButtonMouseEnter(Sender: TObject);
var
  Button: TImage;
begin
  Button := TImage(Sender);
  if Button.Name = 'MenuButton' then
    ButtonImage.Draw(Button.Canvas, 0, 0, 5);
  if Button.Name = 'LanguageButton' then
    ButtonImage.Draw(Button.Canvas, 0, 0, 6);
  if Button.Name = 'SleepButton' then
    ButtonImage.Draw(Button.Canvas, 0, 0, 7);
  if Button.Name = 'HelpButton' then
    ButtonImage.Draw(Button.Canvas, 0, 0, 8);
  if Button.Name = 'CloseButton' then
    ButtonImage.Draw(Button.Canvas, 0, 0, 9);
  Button.Repaint;
end;

procedure TMainForm.TitleBarButtonMouseLeave(Sender: TObject);
begin
  ResetButtonsImages;
end;

procedure TMainForm.TitleBarButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ResetButtonsImages;
end;

procedure TMainForm.ToolButtonAddressClick(Sender: TObject);
begin
  WebBrowser.Navigate(InputBox(LanguageData[150], LanguageData[147] + ':',
    ForumMainPage), ForumBrowserFlags);
end;

procedure TMainForm.ToolButtonBackClick(Sender: TObject);
begin
  WebBrowser.GoBack;
end;

procedure TMainForm.ToolButtonBookmarksClick(Sender: TObject);
begin
  with Mouse.CursorPos do
    BookmarksMenu.Popup(X, Y);
end;

procedure TMainForm.ToolButtonHomeClick(Sender: TObject);
begin
  WebBrowser.Navigate(ForumMainPage);
end;

function GetStrings(Text: String; sep: Char = '/'): TStrings;
var
  SL: TStringList;
  s: String;
  n: Integer;
begin
  s := Text;
  SL := TStringList.Create;
  while Length(s) > 1 do
  begin
    n := Pos(sep, s);
    if n > 0 then
    begin
      SL.Add(Copy(s, 1, n - 1));
      Delete(s, 1, n);
    end
    else
    begin
      SL.Add(s);
      s := '';
    end;
  end;
  Result := SL;
end;

procedure TMainForm.UnzipFiles(ArchiveName, UnzipTo: String);
var
  Zip: TZipFile;
begin
  if not DirectoryExists(UnzipTo) then
    CreateDir(UnzipTo);
  Zip := TZipFile.Create;
  try
    Zip.ExtractZipFile(ArchiveName, UnzipTo);
  finally
    Zip.Free;
  end;
  DeleteFile(ArchiveName);
end;

procedure TMainForm.UpdateContentInfo;
begin
  with ContentInfoMemo.Lines do
  begin
    Clear;
    if ContentList.Items.Count > 0 then
    begin
      AddStrings(GetStrings(IdHTTP.Get(TIdURI.URLEncode(SiteProtocol +
        OfficialWebsite + ExchangeCenterPage + '?c=' + Category
        [ContentCategoryBox.ItemIndex] + '&f=' + ContentList.Items
        [ContentList.ItemIndex] + '&i=i' + '&l=' + LanguageData[0]))));
      DownloadFilesButton.Enabled := true;
    end
    else
      DownloadFilesButton.Enabled := false;
  end;
end;

procedure TMainForm.UpdateContentList;
begin
  with ContentList.Items do
  begin
    Clear;
    AddStrings(GetStrings(IdHTTP.Get(TIdURI.URLEncode(SiteProtocol +
      OfficialWebsite + ExchangeCenterPage + '?c=' + Category
      [ContentCategoryBox.ItemIndex]))));
  end;
  ContentList.ItemIndex := 0;
  UpdateContentInfo;
end;

procedure TMainForm.UpdateModulesList;
begin
  if SavedSplashScreenState < 2 then
    SplashScreen.Show;
  FreeModules;
  FindModules(ProgramPath + ModulesDir);
  LoadModules;
  HelpForm.LoadHelpTexts;
  AddModulesHelpToMainProgramHelp;
  ChangeModulesLanguageToProgramLanguage;
  SplashScreen.Close;
end;

procedure TMainForm.UpdateProgram;
  procedure DeployBAT;
  var
    bat: TextFile;
  begin
    if not FileExists(ProgramPath + 'update.bat') then
    begin
      AssignFile(bat, ProgramPath + 'update.bat');
      Rewrite(bat);
      WriteLn(bat, 'taskkill /im av.exe');
      WriteLn(bat, 'sleep 1'); // Windows XP
      WriteLn(bat, 'timeout /t 1 /nobreak'); // Windows 7+
      WriteLn(bat, 'del av.exe');
      WriteLn(bat, 'move ' + ZipsDir + '\av.exe %1');
      WriteLn(bat, 'del /S /Q ' + ZipsDir);
      WriteLn(bat, 'start av.exe');
      // WriteLn(bat, 'pause');
      CloseFile(bat);
    end;
  end;

begin
  DownloadFile(SiteProtocol + OfficialWebsite + '/av.zip',
    ProgramPath + 'av.zip');
  UnzipFiles(ProgramPath + 'av.zip', ProgramPath + ZipsDir);
  DeployBAT;
  SetCurrentDir(ProgramPath);
  ShellExecute(Handle, nil, 'update.bat', PChar(ProgramPath), nil, SW_SHOW);
end;

procedure TMainForm.UpdateProgramButtonClick(Sender: TObject);
begin
  UpdateProgram;
end;

procedure TMainForm.UpdateVirtualsList;
var
  R: Integer;
  SR: TSearchRec;
begin
  VirtualsList.Clear;
  if not DirectoryExists(ProgramPath + VirtualsDir) then
    CreateDir(VirtualsDir);
  SetCurrentDir(ProgramPath + VirtualsDir);
  R := FindFirst('*' + VirtualExt, 0, SR);
  while R = 0 do
  begin
    VirtualsList.AddItem(CutLastSymbols(SR.Name, Length(VirtualExt)), nil);
    R := FindNext(SR);
  end;
  FindClose(SR);
  VirtualsList.ItemIndex := 0;
end;

procedure TMainForm.VirtualsListClick(Sender: TObject);
begin
  LoadVirtualInfo;
end;

procedure TMainForm.VirtualsListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Control as TListBox, Canvas do
  begin
    Brush.Color := TabColor[2] - ListColor * ((Index + 1) mod 2);
    FillRect(Rect);
    Font.Color := TColor(clBlack);
    TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TMainForm.WebsiteLinkClick(Sender: TObject);
begin
  ShellExecute(Handle, nil, 'http://' + OfficialWebsite, nil, nil, SW_SHOW);
end;

procedure TMainForm.LanguageMenuItemClick(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  if Sender is TMenuItem then
  begin
    MenuItem := TMenuItem(Sender);
    ChangeLanguageTo(CutLastSymbols(MenuItem.Name));
  end;
end;

{ TTabSheet }

constructor TTabSheet.Create(aOwner: TComponent);
begin
  inherited;
  FColor := clBtnFace;
end;

procedure TTabSheet.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TTabSheet.WMEraseBkGnd(var Msg: TWMEraseBkGnd);
begin
  if FColor = clBtnFace then
    inherited
  else
  begin
    Brush.Color := FColor;
    Winapi.Windows.FillRect(Msg.dc, ClientRect, Brush.Handle);
    Msg.Result := 1;
  end;
end;

end.
