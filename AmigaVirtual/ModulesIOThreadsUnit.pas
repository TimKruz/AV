unit ModulesIOThreadsUnit;

interface

uses System.Classes, Vcl.Dialogs, System.SysUtils, ModuleOperationsUnit,
  SystemControlMessagesUnit;

type
  TIOThread = class(TThread)
  public const
    SleepTime = 1000;

  var
    Module: TModule;
    SelfID: Integer;
    constructor Create(M: TModule; ID: Integer);
  protected
    procedure Execute; override;
  end;

  TIOThreads = array of TIOThread;

  TPoolRecord = record
    Text: String;
    AuthorID: Integer;
    ModuleGot: array of Boolean;
  end;

  TPool = class
    Records: array of TPoolRecord;
    Empty: Boolean;
    procedure AddRecord(RecordText: String; RecordAuthor: Integer);
    procedure CheckAndDeleteOddRecords;
    constructor Create;
    procedure Show;
  end;

var
  IOThread: TIOThreads;
  Pool: TPool;

function StartThreads: Integer;
procedure FreeThreads;
procedure CountOutputModules;

implementation

uses
  MainWindowUnit, HelpWindowUnit;

var
  OutputModulesCount: Integer;

procedure CountOutputModules;
var
  M: TModule;
begin
  OutputModulesCount := 0;
  for M in ModuleSelected do
    if @M.SendData <> nil then
      Inc(OutputModulesCount);
end;

procedure FreeThreads;
var
  T: TIOThread;
begin
  if Length(IOThread) > 0 then
    for T in IOThread do
      T.Free;
  SetLength(IOThread, 0);
end;

function StartThreads: Integer;
var
  M: TModule;
  ID, R: Integer;
begin
  FreeThreads;
  R := 0;
  for M in ModuleSelected do
    if M.MType in [only_input, only_output, input_and_output] then
    begin
      ID := Length(IOThread);
      SetLength(IOThread, ID + 1);
      IOThread[ID] := TIOThread.Create(M, ID);
      if IOThread[ID].Handle <> 0 then
        Inc(R);
    end;
  Result := R;
end;

{ TIOThread }

constructor TIOThread.Create(M: TModule; ID: Integer);
begin
  Module := M;
  SelfID := ID;

  inherited Create(false);
end;

procedure TIOThread.Execute;
  procedure GetData;
  var
    M: String;
  begin
    M := String(Module.GetData);
    if M <> SCM_No_Message then
      Synchronize(
        procedure
        begin
          Pool.AddRecord(M, SelfID);
        end);
  end;

  procedure SendData;
  var
    i: Integer;
  begin
    with Pool do
      if not Empty then
      begin
        for i := 0 to Length(Records) - 1 do
          with Records[i] do
            if not ModuleGot[SelfID] and (AuthorID <> SelfID) then
            begin
              Module.SendData(PChar(Text));
              ModuleGot[SelfID] := true;
            end;
        Synchronize(
          procedure
          begin
            CheckAndDeleteOddRecords;
          end);
      end;
  end;

begin
  inherited;
  while not Terminated do
  begin
    case Module.MType of
      only_input:
        GetData;
      only_output:
        SendData;
      input_and_output:
        begin
          SendData;
          GetData;
        end;
    end;
    Sleep(SleepTime);
  end;
end;

{ TPool }

procedure TPool.AddRecord(RecordText: String; RecordAuthor: Integer);
var
  i, RL: Integer;
begin
  RL := Length(Records);
  SetLength(Records, RL + 1);
  with Records[RL] do
  begin
    Text := RecordText;
    AuthorID := RecordAuthor;
    SetLength(ModuleGot, OutputModulesCount);
    for i := 0 to OutputModulesCount - 1 do
      if i = AuthorID then
        ModuleGot[i] := true
      else
        ModuleGot[i] := false;
  end;
  with MainForm, MainForm.ChatBox.Lines do
    case RecordAuthor of
      - 1:
        Add(User.Name + ': ' + RecordText);
    else
      if RecordText = SCM_Dont_Know_Answer then
      begin
        if DontKnowCheckBtn.Checked then
          Add(LanguageData[156]);
      end
      else
        Add(AVirtual.Name + ': ' + RecordText);
    end;
  Empty := false;
end;

procedure TPool.CheckAndDeleteOddRecords;
  function ItsOdd(ID: Integer): Boolean;
  var
    i: Integer;
  begin
    ItsOdd := true;
    with Records[ID] do
      for i := 0 to Length(ModuleGot) - 1 do
        if not ModuleGot[i] then
        begin
          ItsOdd := false;
          exit;
        end;
  end;

  procedure DeleteRecord(ID: Integer);
  var
    i: Integer;
  begin
    for i := ID to Length(Records) - 2 do
      Records[i] := Records[i + 1];
    SetLength(Records, Length(Records) - 1);
  end;

var
  i: Integer;
begin
  if not Empty then
  begin
    for i := Length(Records) - 1 downto 0 do
      if ItsOdd(i) then
        DeleteRecord(i);
    if Length(Records) = 0 then
      Empty := true;
  end;
  if MainForm.PoolShowBtn.Checked then
    Show;
end;

constructor TPool.Create;
begin
  Empty := true;
end;

procedure TPool.Show;
var
  b: String;
  i: Integer;
begin
  b := MainForm.LanguageData[131];
  with Pool do
    for i := 0 to Length(Records) - 1 do
      with Records[i] do
        b := b + #13 + IntToStr(i) + '.' + Text + ' Автор: ' +
          IntToStr(AuthorID);
  HelpForm.ShowWithText(b);
end;

end.
