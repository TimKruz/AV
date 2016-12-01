unit ModuleOperationsUnit;

interface

uses System.Types, Winapi.Windows, System.SysUtils, SplashScreenUnit,
  System.Classes, TypInfo;

const
  ProcCountPerModule = 21;
  ControlCodeMarker = '>';
  StartAll = -1;

type
  TModuleType = (input_and_output, only_input, only_output,
    no_input_and_no_output, parser, undetermined, erroneous_or_empty);
  TWindowState = (closed, opened, window_does_not_exist);
  TModuleState = (working, sleeping, module_cant_sleep);

  TModuleTypeRussian = (ввод_и_вывод, только_ввод, только_вывод,
    без_ввода_и_без_вывода, парсер, неопределён, ошибочен_или_пуст);
  TWindowStateRussian = (закрыто, открыто, окно_не_существует);
  TModuleStateRussian = (работает, спит, модуль_не_может_спать);

  TModuleStatistics = record
    Empty: boolean;
    TimesLoaded: Integer;
  end;

  TModule = record
    Handle: THandle;
    Name, FileName, ControlCode, ControlCodeFormated: String;
    MType: TModuleType;
    WindowState: TWindowState;
    ModuleState: TModuleState;
    HelpTopicID: Integer;
    Statistics: TModuleStatistics;

    SetLanguage: function(Language: PChar): boolean; stdcall;
    SendLanguageData: function(Data: array of PChar): boolean; stdcall;
    GetName: function: PChar; stdcall;
    GetHelp: function: PChar; stdcall;
    Start: procedure; stdcall;
    Sleep: function: boolean; stdcall;
    WakeUp: procedure; stdcall;
    OpenWindow: procedure; stdcall;
    CloseWindow: procedure; stdcall;
    SetVirtual: procedure(Name: PChar); stdcall;
    SaveData: procedure; stdcall;
    LoadData: procedure; stdcall;
    Reset: procedure; stdcall;
    SetNewMainWindow: procedure(Position, Size: TPoint); stdcall;
    GetTimerInterval: function: Integer; stdcall;
    SendData: procedure(Data: PChar); stdcall;
    GetData: function: PChar; stdcall;

    SetSource: procedure(SourcePath: PChar); stdcall;
    NextData: function: PChar; stdcall;
    Progress: function: Real; stdcall;
    RestartParsing: procedure; stdcall;
  end;

  TModules = array of TModule;

var
  Module, ModuleSelected: TModules;

  ProgessFloat: Single;
  ProgressInt: Int64;
  ProgressSteps: Int64;

  PathToModules: String;

procedure FindModules(Path: String);
procedure LoadModules;
function LoadModule(var M: TModule): boolean;
procedure AddNewModule(FileName: String);
procedure FreeModules;
procedure FreeModule(var M: TModule);
function DetermineModuleType(var M: TModule): TModuleType;
procedure IncAndUpdateProgress(val: Int64 = 1);
function FindModuleByFileName(FN: String): TModule;
function FindModuleIDByFileName(FN: String): Integer;
procedure AddModulesHelpToMainProgramHelp;
procedure ChangeModulesLanguageToProgramLanguage;
function GetInfoAbout(M: TModule): TStrings;
function GetStatisticsSummary(M: TModule): TStrings;
procedure ToogleModuleWindow(ID: Integer);
procedure ToogleModuleSleepMode(ID: Integer);
procedure StartModule(ID: Integer);
procedure StartSelectedModules(IDs: array of Integer);
procedure StartAllModules;
procedure CloseAllWindows;
function AreWindowsClosed: boolean;
procedure SaveAllData;

implementation

uses MainWindowUnit, HelpWindowUnit;

procedure FindModules(Path: String);
var
  R: Integer;
  SR: TSearchRec;
begin
  SetLength(Module, 0);
  SetCurrentDir(Path);
  R := FindFirst('*.dll', 0, SR);
  while R = 0 do
  begin
    SetLength(Module, Length(Module) + 1);
    Module[Length(Module) - 1].FileName := SR.Name;
    Module[Length(Module) - 1].Handle := 0;
    R := FindNext(SR);
  end;
  FindClose(SR);
  PathToModules := Path;
  ProgressSteps := Length(Module) * ProcCountPerModule;
  ProgressInt := 0;
end;

procedure LoadModules;
var
  i: Integer;
begin
  for i := 0 to Length(Module) - 1 do
  begin
    if Module[i].Handle > 0 then
      FreeModule(Module[i]);
    if LoadModule(Module[i]) then
      DetermineModuleType(Module[i]);
  end;
end;

function LoadModule(var M: TModule): boolean;
  procedure LoadStatistics;
  begin
    M.Statistics.Empty := true;
  end;

  function FormatControlCode(Code: String): String;
  var
    i: Integer;
    R: String;
  begin
    R := '';
    for i := 1 to Length(Code) do
      R := R + '#' + IntToStr(Ord(Code[i]));
    Result := R;
  end;

var
  B: String;
  C: Integer;
begin
  with M do
  begin
    Handle := LoadLibrary(PWideChar(FileName));
    if Handle > 0 then
    begin
      @SetLanguage := GetProcAddress(Handle, 'SetLanguage');
      IncAndUpdateProgress;
      @SendLanguageData := GetProcAddress(Handle, 'SendLanguageData');
      IncAndUpdateProgress;
      @GetName := GetProcAddress(Handle, 'GetName');
      IncAndUpdateProgress;
      @GetHelp := GetProcAddress(Handle, 'GetHelp');
      IncAndUpdateProgress;
      @Start := GetProcAddress(Handle, 'Start');
      IncAndUpdateProgress;
      @Sleep := GetProcAddress(Handle, 'Sleep');
      IncAndUpdateProgress;
      @WakeUp := GetProcAddress(Handle, 'WakeUp');
      IncAndUpdateProgress;
      @OpenWindow := GetProcAddress(Handle, 'OpenWindow');
      IncAndUpdateProgress;
      @CloseWindow := GetProcAddress(Handle, 'CloseWindow');
      IncAndUpdateProgress;
      @SetVirtual := GetProcAddress(Handle, 'SetVirtual');
      IncAndUpdateProgress;
      @SaveData := GetProcAddress(Handle, 'SaveData');
      IncAndUpdateProgress;
      @LoadData := GetProcAddress(Handle, 'LoadData');
      IncAndUpdateProgress;
      @Reset := GetProcAddress(Handle, 'Reset');
      IncAndUpdateProgress;
      @SetNewMainWindow := GetProcAddress(Handle, 'SetNewMainWindow');
      IncAndUpdateProgress;
      @GetTimerInterval := GetProcAddress(Handle, 'GetTimerInterval');
      IncAndUpdateProgress;
      @SendData := GetProcAddress(Handle, 'SendData');
      IncAndUpdateProgress;
      @GetData := GetProcAddress(Handle, 'GetData');
      IncAndUpdateProgress;
      @SetSource := GetProcAddress(Handle, 'SetSource');
      IncAndUpdateProgress;
      @NextData := GetProcAddress(Handle, 'NextData');
      IncAndUpdateProgress;
      @Progress := GetProcAddress(Handle, 'Progress');
      IncAndUpdateProgress;
      @RestartParsing := GetProcAddress(Handle, 'RestartParsing');
      IncAndUpdateProgress;

      if @GetName <> nil then
      begin
        B := GetName;
        C := Pos(ControlCodeMarker, B);
        if C > 0 then
        begin
          Name := Copy(B, C + 1, Length(B));
          ControlCode := Copy(B, 1, C - 1);
          ControlCodeFormated := FormatControlCode(ControlCode);
        end
        else
        begin
          Name := B;
          ControlCode := MainForm.LanguageData[133];
        end;
      end
      else
      begin
        Name := MainForm.LanguageData[114];
        ControlCode := MainForm.LanguageData[133];
      end;

      if (@OpenWindow <> nil) and (@CloseWindow <> nil) then
        WindowState := closed
      else
        WindowState := window_does_not_exist;

      if (@Sleep <> nil) and (@WakeUp <> nil) then
        ModuleState := working
      else
        ModuleState := module_cant_sleep;

      LoadStatistics;
      Result := true;
    end
    else
      Result := false;
  end;
end;

procedure AddNewModule(FileName: String);
begin
  SetLength(Module, Length(Module) + 1);
  Module[Length(Module) - 1].FileName := FileName;
  LoadModule(Module[Length(Module) - 1]);
end;

procedure FreeModules;
var
  i: Integer;
begin
  for i := 0 to Length(Module) - 1 do
    FreeModule(Module[i]);
  SetLength(Module, 0);
end;

procedure FreeModule(var M: TModule);
begin
  with M do
  begin
    FreeLibrary(Handle);

    Name := '';
    FileName := '';
    MType := undetermined;
    WindowState := window_does_not_exist;
    ModuleState := module_cant_sleep;
    HelpTopicID := 0;

    @SetLanguage := nil;
    @SendLanguageData := nil;
    @GetName := nil;
    @GetHelp := nil;
    @Start := nil;
    @Sleep := nil;
    @WakeUp := nil;
    @OpenWindow := nil;
    @CloseWindow := nil;
    @SetVirtual := nil;
    @SaveData := nil;
    @LoadData := nil;
    @Reset := nil;
    @SetNewMainWindow := nil;
    @GetTimerInterval := nil;
    @SendData := nil;
    @GetData := nil;
    @SetSource := nil;
    @NextData := nil;
    @Progress := nil;
    @RestartParsing := nil;
  end;
end;

function DetermineModuleType(var M: TModule): TModuleType;
begin
  with M do
  begin
    MType := undetermined;

    if (@SetLanguage = nil) and (@SendLanguageData = nil) and (@GetName = nil)
      and (@GetHelp = nil) and (@Start = nil) and (@Sleep = nil) and
      (@WakeUp = nil) and (@OpenWindow = nil) and (@CloseWindow = nil) and
      (@SetVirtual = nil) and (@SaveData = nil) and (@LoadData = nil) and
      (@Reset = nil) and (@SetNewMainWindow = nil) and (@GetTimerInterval = nil)
      and (@SendData = nil) and (@GetData = nil) and (@SetSource = nil) and
      (@NextData = nil) and (@Progress = nil) and (@RestartParsing = nil) then
      MType := erroneous_or_empty
    else
    begin
      if (@SetSource <> nil) or (@NextData <> nil) or (@Progress <> nil) or
        (@RestartParsing <> nil) then
        MType := parser
      else
      begin
        if (@GetData <> nil) and (@SendData <> nil) then
          MType := input_and_output
        else if (@GetData <> nil) and (@SendData = nil) then
          MType := only_input
        else if (@GetData = nil) and (@SendData <> nil) then
          MType := only_output
        else if (@GetData = nil) and (@SendData = nil) then
          MType := no_input_and_no_output;
      end;
    end;

    Result := MType;
  end;
end;

procedure IncAndUpdateProgress(val: Int64 = 1);
  procedure ShowProgess;
  begin
    ProgessFloat := ProgressInt / ProgressSteps * 100;
    SplashScreen.SetProgress(ProgessFloat);
  end;

begin
  inc(ProgressInt, val);
  case MainForm.SavedSplashScreenState of
    0:
      ShowProgess;
    1:
      if ProgressInt mod ProcCountPerModule = 0 then
        ShowProgess;
    2:
      ;
  end;
end;

function FindModuleByFileName(FN: String): TModule;
var
  M: TModule;
begin
  for M in Module do
    if M.FileName = FN then
      Result := M;
end;

function FindModuleIDByFileName(FN: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Module) - 1 do
    if Module[i].FileName = FN then
      Result := i;
end;

procedure AddModulesHelpToMainProgramHelp;
  function GetStrings(ID: Integer): TStrings;
    function Slice(var S: String; C: Char): String;
    var
      P: Integer;
    begin
      P := Pos(C, S);
      if P > 0 then
      begin
        Result := Copy(S, 1, P - 1);
        Delete(S, 1, P);
      end
      else
      begin
        Result := S;
        S := '';
      end;
    end;

  var
    HT: String;
  begin
    Result := TStringList.Create;
    HT := String(Module[ID].GetHelp);
    while HT <> '' do
      Result.Add(Slice(HT, #13));
  end;

var
  i: Integer;
begin
  for i := 0 to Length(Module) - 1 do
    if @Module[i].GetHelp <> nil then
      Module[i].HelpTopicID := HelpForm.AddTopic(MainForm.LanguageData[69] +
        ' "' + Module[i].Name + '"', GetStrings(i));
end;

procedure ChangeModulesLanguageToProgramLanguage;
var
  M: TModule;
begin
  for M in Module do
    if @M.SetLanguage <> nil then
      M.SetLanguage(PChar(MainForm.LanguageData[0]))
    else if @M.SendLanguageData <> nil then
      M.SendLanguageData(PChar(MainForm.LanguageData));
end;

function GetInfoAbout(M: TModule): TStrings;
  function UnderlineToSpace(str: String): String;
  var
    n: Integer;
    S: String;
  begin
    S := str;
    n := Pos('_', S);
    while n > 0 do
    begin
      S[n] := #32;
      n := Pos('_', S);
    end;
    Result := S;
  end;

begin
  Result := TStringList.Create;
  with Result, M, MainForm do
  begin
    Add(LanguageData[119] + #32 + IntToStr(M.Handle));
    Add(LanguageData[120] + #32 + M.Name);
    Add(LanguageData[121] + #32 + FileName);
    Add(LanguageData[132] + #32 + ControlCodeFormated);
    if LanguageData[0] = 'Russian' then
    begin
      Add(LanguageData[122] + #32 + UnderlineToSpace
        (GetEnumName(TypeInfo(TModuleTypeRussian), Ord(MType))));
      Add(LanguageData[123] + #32 + UnderlineToSpace
        (GetEnumName(TypeInfo(TWindowStateRussian), Ord(M.WindowState))));
      Add(LanguageData[124] + #32 + UnderlineToSpace
        (GetEnumName(TypeInfo(TModuleStateRussian), Ord(ModuleState))));
    end
    else
    begin
      Add(LanguageData[122] + #32 + UnderlineToSpace
        (GetEnumName(TypeInfo(TModuleType), Ord(MType))));
      Add(LanguageData[123] + #32 + UnderlineToSpace
        (GetEnumName(TypeInfo(TWindowState), Ord(M.WindowState))));
      Add(LanguageData[124] + #32 + UnderlineToSpace
        (GetEnumName(TypeInfo(TModuleState), Ord(ModuleState))));
    end;
    Add(LanguageData[125] + #32 + IntToStr(HelpTopicID));
    if Statistics.Empty then
      Add(LanguageData[126])
    else
      Add(LanguageData[127]);
    Add('');

    Add(LanguageData[128]);
    if @SetLanguage <> nil then
      Add('SetLanguage');
    if @SendLanguageData <> nil then
      Add('SendLanguageData');
    if @GetName <> nil then
      Add('GetName');
    if @GetHelp <> nil then
      Add('GetHelp');
    if @Start <> nil then
      Add('Start');
    if @Sleep <> nil then
      Add('Sleep');
    if @WakeUp <> nil then
      Add('WakeUp');
    if @OpenWindow <> nil then
      Add('OpenWindow');
    if @CloseWindow <> nil then
      Add('CloseWindow');
    if @SetVirtual <> nil then
      Add('SetVirtual');
    if @SaveData <> nil then
      Add('SaveData');
    if @LoadData <> nil then
      Add('LoadData');
    if @Reset <> nil then
      Add('Reset');
    if @SetNewMainWindow <> nil then
      Add('SetNewMainWindow');
    if @GetTimerInterval <> nil then
      Add('GetTimerInterval');
    if @SendData <> nil then
      Add('SendData');
    if @GetData <> nil then
      Add('GetData');
    if @SetSource <> nil then
      Add('SetSource');
    if @NextData <> nil then
      Add('NextData');
    if @Progress <> nil then
      Add('Progress');
    if @RestartParsing <> nil then
      Add('RestartParsing');
  end;
end;

function GetStatisticsSummary(M: TModule): TStrings;
begin
  Result := TStringList.Create;
  with Result, M, MainForm do
  begin
    if Statistics.Empty then
      Add(LanguageData[126])
    else
    begin
      Add(LanguageData[106] + ': ');
    end;
  end;
end;

procedure ToogleModuleWindow(ID: Integer);
begin
  with Module[ID] do
    case WindowState of
      closed:
        begin
          OpenWindow;
          WindowState := opened;
        end;
      opened:
        begin
          CloseWindow;
          WindowState := closed;
        end;
      window_does_not_exist:
        ;
    end;
end;

procedure ToogleModuleSleepMode(ID: Integer);
begin
  with Module[ID] do
    case ModuleState of
      working:
        begin
          ModuleState := sleeping;
          Sleep;
        end;
      sleeping:
        begin
          ModuleState := working;
          WakeUp;
        end;
      module_cant_sleep:
        ;
    end;
end;

procedure StartModule(ID: Integer);
begin
  with Module[ID] do
    if @Start <> nil then
    begin
      Start;
      if WindowState = closed then
        ToogleModuleWindow(ID);
      if @SetVirtual <> nil then
        SetVirtual(PChar(MainForm.AVirtual.Name));
      if @LoadData <> nil then
        LoadData;
    end;
end;

procedure StartSelectedModules(IDs: array of Integer);
  function Selected(ID: Integer): boolean;
  var
    i: Integer;
  begin
    Result := false;
    for i := 0 to Length(IDs) - 1 do
      if IDs[i] = ID then
      begin
        Result := true;
        exit;
      end;
  end;

var
  i: Integer;
begin
  for i := 0 to Length(Module) - 1 do
    if Selected(i) or (IDs[0] = StartAll) then
      StartModule(i);
end;

procedure StartAllModules;
begin
  StartSelectedModules([StartAll]);
end;

procedure CloseAllWindows;
var
  i: Integer;
begin
  for i := 0 to Length(Module) - 1 do
    if Module[i].WindowState = opened then
      ToogleModuleWindow(i);
end;

function AreWindowsClosed: boolean;
var
  M: TModule;
begin
  Result := true;
  for M in Module do
    if M.WindowState = opened then
    begin
      Result := false;
      exit;
    end;
end;

procedure SaveAllData;
var
  M: TModule;
begin
  for M in ModuleSelected do
    if @M.SaveData <> nil then
      M.SaveData;
end;

end.
