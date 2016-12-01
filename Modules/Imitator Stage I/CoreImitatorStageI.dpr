library CoreImitatorStageI;

uses
  System.SysUtils,
  System.Classes,
  SystemControlMessagesUnit
    in '..\..\AmigaVirtual\SystemControlMessagesUnit.pas',
  MainFormUnit in 'MainFormUnit.pas',
  StatesPoolUnit;

const
  ControlCode = #1#1#1;
  Name = ControlCode + '>Core Imitator Stage I';
  Help = 'help' + #13 + 'helpy' + #13 + 'helpest';

  DefaultSyllableLength = 3;
  ContextMaxLength = 10;

  DataExt = '.data';
  DataRelationsExt = '.relations';
  VirtualsDirectory = '../Virtuals/';

type
  TID = UInt64;

  PNode = ^TNode;

  TContext = array of PNode;

  TLink = record
    Address: PNode;
    Context: TContext;
    Weight: Int64;
  end;

  TNode = record
    Data: String;
    ID: TID;
    Next: array of TLink;
  end;

  TPNodeArray = array of PNode;

var
  FormState: (closed, opened);
  Buffer, VirtualName: String;
  NewMessageGot: Boolean;

  Node: TPNodeArray;
  LastNode: PNode;
  // CurrentContext: TContext;
  LearningThreshold: Single;
  NoAnswer: Boolean;

function GetName: PChar; stdcall;
begin
  Result := PChar(Name);
end;

function GetHelp: PChar; stdcall;
begin
  Result := PChar(Help);
end;

procedure OpenWindow; stdcall;
begin
  if MainForm = nil then
    MainForm := TMainForm.Create(nil);
  MainForm.Show;
  FormState := opened;
end;

procedure CloseWindow; stdcall;
begin
  if FormState = opened then
  begin
    MainForm.Close;
    FormState := closed;
    MainForm.Release;
    MainForm := nil;
  end;
end;

function CalculateSimilarity(A, B: String;
  SyllableLength: Byte = DefaultSyllableLength): Real;

  function SyllableCount(S: String; SylLength: Integer): Integer;
  begin
    Result := Length(S) - SylLength + 1;
  end;

var
  MatchCount: Integer;
  i: Integer;
  C, D: String;
begin
  MatchCount := 0;

  if A = B then
  begin
    Result := 1;
    exit;
  end;

  if Length(A) > Length(B) then
  begin
    C := A;
    D := B;
  end
  else
  begin
    C := B;
    D := A;
  end;

  if Length(D) < SyllableLength then
  begin
    C := C + Copy(C, 1, Length(D) - 1);
    if Pos(D, C) > 0 then
      MatchCount := 1;
    Result := MatchCount / SyllableCount(C, Length(D));
    exit;
  end;

  C := C + Copy(C, 1, SyllableLength - 1);
  D := D + Copy(D, 1, SyllableLength - 1);

  for i := 1 to Length(C) - SyllableLength + 1 do
    if Pos(Copy(C, i, SyllableLength), D) > 0 then
      inc(MatchCount);

  Result := MatchCount / SyllableCount(C, SyllableLength);
end;

function CompareContexts(A, B: TContext): Real;
var
  TotalSim: Real;
  P1, P2: PNode;
begin
  TotalSim := 0;
  for P1 in A do
    for P2 in B do
      TotalSim := TotalSim + CalculateSimilarity(P1^.Data, P2^.Data);
  Result := TotalSim / (Length(A) * Length(B));
end;

function SliceFirstWord(var from: String): String;
var
  i: Integer;
  C: Char;
begin
  C := from[1];
  i := 1;
  while (C <> ' ') and (i < Length(from) + 1) do
  begin
    inc(i);
    C := from[i];
  end;
  SliceFirstWord := Copy(from, 1, i - 1);
  Delete(from, 1, i);
end;

procedure AddNode(Data: String);
var
  ID: TID;
begin
  ID := Length(Node);
  SetLength(Node, ID + 1);
  New(Node[ID]);
  Node[ID]^.Data := Data;
  Node[ID]^.ID := ID;
  if LastNode <> nil then
  begin
    SetLength(LastNode^.Next, 1);
    LastNode^.Next[0].Address := Node[ID];
  end;
  LastNode := Node[ID];
end;

procedure LoadWholeData;
var
  DataFile, RelationsFile: Textfile;
  Buffer: String;
  i: TID;
begin
{$I-}
  AssignFile(DataFile, VirtualsDirectory + VirtualName + DataExt);
  Reset(DataFile);
  if IOResult = 0 then
    while not Eof(DataFile) do
    begin
      ReadLn(DataFile, Buffer);
      MainForm.Memo.Lines.Add(Buffer);
      AddNode(Buffer);
    end
  else
    Rewrite(DataFile);
  Close(DataFile);

  AssignFile(RelationsFile, VirtualsDirectory + VirtualName + DataRelationsExt);
  Reset(RelationsFile);
  if IOResult = 0 then
  begin
    i := 0;
    while not Eof(RelationsFile) do
    begin
      ReadLn(RelationsFile, Buffer);
      Node[i]^.Next[0].Address := PNode(Buffer);
      inc(i);
    end;
  end
  else
    Rewrite(RelationsFile);
  Close(RelationsFile);
{$I+}
end;

procedure SaveWholeData;
var
  DataFile, RelationsFile: Textfile;
//  Buffer: String;
  NP: PNode;
begin
{$I-}
  AssignFile(DataFile, VirtualsDirectory + VirtualName + DataExt);
  Rewrite(DataFile);
  for NP in Node do
    WriteLn(DataFile, NP^.Data);
  Close(DataFile);

  AssignFile(RelationsFile, VirtualsDirectory + VirtualName + DataRelationsExt);
  Rewrite(RelationsFile);
  {for NP in Node do
    WriteLn(RelationsFile, NP^.Next[0].Address^.ID);}
  Close(RelationsFile);
{$I+}
end;

procedure PrintDataBase(from: PNode);
var
  P: PNode;
  S: String;
begin
  S := '';
  P := from;
  while Length(P^.Next) > 0 do
  begin
    S := S + (P^.Data + '->');
    P := P^.Next[0].Address;
  end;
  S := S + (P^.Data + '$');
  MainForm.Memo.Lines.Add(S);
end;

function FindNearestNode(Data: String; var ID: TID): Boolean;
var
  N: PNode;
  Sim, MaxSim: Real;
begin
  MaxSim := 0;
  for N in Node do
  begin
    Sim := CalculateSimilarity(N^.Data, Data);
    if Sim > MaxSim then
    begin
      MaxSim := Sim;
      ID := N^.ID;
    end;
  end;
  LearningThreshold := MainForm.LearningThresholdTrackBar.Position / 100;
  MainForm.Memo.Lines.Add(FloatToStr(MaxSim * 100) + '%');
  if MaxSim > LearningThreshold then
    Result := true
  else
    Result := false;
end;

procedure PrintNodes;
var
  S: String;
  N: PNode;
begin
  S := '';
  for N in Node do
  begin
    S := S + N^.Data + '(' + IntToStr(N^.ID) + ')';
    if Length(N^.Next) > 0 then
      S := S + ' -> ' + N^.Next[0].Address^.Data;
    S := S + '; ';
  end;
  MainForm.Memo.Lines.Add(S);
end;

function GetAnswer(QID: TID): String;
begin
  with Node[QID]^ do
  begin
    if Length(Next) > 0 then
    begin
      LastNode := Next[0].Address;
      Result := LastNode^.Data;
      NoAnswer := false;
    end
    else
    begin
      NoAnswer := true;
    end;
  end;
end;

procedure SendData(Data: PChar); stdcall;
var
  ID: TID;
begin
  NoAnswer := true;
  Buffer := Data;
  MainForm.Memo.Lines.Add(Buffer);
  if FindNearestNode(Buffer, ID) then
    Buffer := GetAnswer(ID)
  else
    AddNode(Buffer);
  PrintNodes;
  NewMessageGot := true;
  if NoAnswer then
    Buffer := SCM_Dont_Know_Answer;
end;

function GetData: PChar; stdcall;
begin
  if NewMessageGot then
  begin
    Result := PChar(Buffer);
    NewMessageGot := false;
  end
  else
    Result := PChar(SCM_No_Message);
end;

procedure Start; stdcall;
begin
  NewMessageGot := false;
  LastNode := nil;
  if MainForm = nil then
    MainForm := TMainForm.Create(nil);
  LearningThreshold := MainForm.LearningThresholdTrackBar.Position / 100;
end;

procedure SetVirtual(Name: PChar); stdcall;
begin
  VirtualName := Name;
  MainForm.Caption := VirtualName;
end;

procedure LoadData; stdcall;
begin
  LoadWholeData;
end;

procedure SaveData; stdcall;
begin
  SaveWholeData;
end;

exports GetName, GetHelp, OpenWindow, CloseWindow, SendData, GetData, Start,
  SetVirtual, LoadData, SaveData;

begin

end.
