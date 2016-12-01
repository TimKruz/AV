unit AVMainWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus;

const
  SysName = '[SYSTEM] ';
  CorrectSym = (['A' .. 'Z', 'a' .. 'z',
                 'А' .. 'Я', 'а' .. 'я', 'Ё', 'ё',
                 ' ', '-', '.', ',', '!', '?']);
  Answer_Is_BAD = -1;
  Answer_Is_NOTHING = 0;
  Answer_Is_GOOD = 1;

  SM_CHAT = 0;
  SM_CHANGE_AV = 1;

  SH_ABOUT = 0;
  SH_FAQ = 1;

type
  TAVMainForm = class(TForm)
    Memo: TMemo;
    Timer: TTimer;
    MainMenu: TPopupMenu;
    HelpGrp: TMenuItem;
    EditGrp: TMenuItem;
    CopyBtn: TMenuItem;
    ClearBtn: TMenuItem;
    MenuPanel: TPanel;
    VirtList: TListBox;
    LoadBtn: TButton;
    NewBtn: TButton;
    DelBtn: TButton;
    RenBtn: TButton;
    About: TMenuItem;
    SettingsGrp: TMenuItem;
    ChgAvatarBtn: TMenuItem;
    CloseAVBtn: TMenuItem;
    QuitBtn: TMenuItem;
    SaveAVBtn: TMenuItem;
    DontSaveBtn: TMenuItem;
    FAQbtn: TMenuItem;
    procedure FormCreate (Sender: TObject);
    procedure TimerTimer (Sender: TObject);
    procedure FormCloseQuery (Sender: TObject; var CanClose: Boolean);
    procedure WriteS (s: string);
    procedure WriteC (c: string; add: boolean = false);
    procedure MemoKeyPress (Sender: TObject; var Key: Char);
    procedure CopyBtnClick (Sender: TObject);
    procedure ClearBtnClick (Sender: TObject);
    procedure FormMouseDown (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy (Sender: TObject);
    procedure FormMouseUp (Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove (Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DelBtnClick (Sender: TObject);
    procedure NewBtnClick (Sender: TObject);
    procedure RenBtnClick (Sender: TObject);
    procedure LoadBtnClick (Sender: TObject);
    procedure AboutClick (Sender: TObject);
    procedure QuitBtnClick (Sender: TObject);
    procedure CloseAVBtnClick (Sender: TObject);
    procedure SaveAVBtnClick (Sender: TObject);
    procedure DontSaveBtnClick (Sender: TObject);
    procedure ChgAvatarBtnClick (Sender: TObject);
    procedure FAQbtnClick (Sender: TObject);
    procedure MemoExit (Sender: TObject);
  private
    Dragging: boolean;
    OldX, OldY: integer;
    Menu: byte;
    procedure SetMenu (x:byte);
    procedure ResetChat;
    procedure ChangeAvatar;
  end;

  TWords = array of string;

  TNextWord = record
    ID: word;
    Importance: smallint;
  end;
  TKeySum = record
    KeySum: longword;
    Importance: smallint;
  end;
  TDBWord = record
    //ID: word;
    Text: string;
    Next: array of TNextWord;
  end;
  TDBPhrase = record
    Sum: longword;
    KeySums: array of TKeySum;
  end;

  TWordDB = array of TDBWord;
  TPhraseDB = array of TDBPhrase;

  TFuncGetAvatar = function (params: array of byte; var ResImg: TBitmap): byte;

var
  AVMainForm: TAVMainForm;

  AlphaDelta: shortint = 0;

  Hidding: byte = 0;
  Hidded: boolean = false;
  Closing: boolean = false;

  Words: TWords;
  WordDB: TWordDB;
  PhraseDB: TPhraseDB;

  DB: array of string;

  AppCaption: string = 'Amiga Virtual';
  CapBuf: string;
  AVName: string = 'AV';
  UserName: string = 'Вы: ';
  UserIn: string = '';

  FrameImg, FrameBuf: TBitmap;
  CheckAnswer: boolean = false;
  CapChanged: boolean = true;

  AvatarLib: HMODULE;
  AvatarImg: TBitmap;
  AvatarCode: cardinal;

  OutTime: cardinal;

implementation

{$R *.dfm}
{$R Images.res}

function CalculateRatio (A, B: string): single;
type
  TBigramm = string[2];
  TBigramms = array of TBigramm;
var
  ASet, BSet: TBigramms;
  I, Match: byte;

  function ExplodeString (str: string): TBigramms;
  var
    res: TBigramms;
    I: byte;
  begin
    for I := 1 to Length (str) - 1 do
    begin
      SetLength (res, Length (res) + 1);
      res[Length (res) - 1] := Copy (str, I, 2);
    end;
    Result := res;
  end;

  function BigrammIsInSet (Bigramm: TBigramm; BigrammSet: TBigramms): boolean;
  var
    I: byte;
    res: boolean;
  begin
    res := false;
    if Length (BigrammSet) > 0 then
      for I := 0 to Length (BigrammSet) - 1 do
        if Bigramm = BigrammSet[I] then
          res := true;
    Result := res;
  end;

begin
  Match := 0;
  SetLength (ASet, 0);
  SetLength (BSet, 0);
  if Length (A) > 1 then
  begin
    ASet := ExplodeString (A);
    BSet := ExplodeString (B);
    for I := 0 to Length (ASet) - 1 do
      if BigrammIsInSet (ASet[I], BSet) then
        Inc (Match);
    Result := Match / (Length (ASet) + Length (BSet) - Match);
  end; {else
    if Length (B) > 0 then
      if A[1] = B[1] then
        Result := 1
      else
        Result := 0
    else
      Result := 0;}
end;

function FindMaxRatio (X: string): cardinal;
var
  I: word;
  Max, Ratio: single;
begin
  Max := 0;
  if Length (DB) > 0 then
    for I := 0 to Length (DB) - 1 do
    begin
      Ratio := CalculateRatio (X, DB[I]);
      if (Ratio > Max) {and (Ratio <= 1)} then
      begin
        Max := Ratio;
        Result := I;
      end;
    end;
end;

function GetMaxRatio (X: string): single;
var
  I: word;
  Max, Ratio: single;
begin
  Max := 0;
  if Length (DB) > 0 then
    for I := 0 to Length (DB) - 1 do
    begin
      Ratio := CalculateRatio (X, DB[I]);
      if (Ratio > Max) {and (Ratio <= 1)} then
      begin
        Max := Ratio;
        Result := Max;
      end;
    end;
end;

procedure AnalysePhrase (Phrase: string);
begin
  SetLength (DB, Length (DB) + 1);
  if GetMaxRatio (Phrase) < 0.5 then
    DB[Length(DB) - 1] := Phrase;
end;

function GenPhrase: string;
var
  PhraseID: cardinal;
begin
  PhraseID := FindMaxRatio (UserIn);
  if PhraseID < Length (DB) - 1 then
    Result := DB[PhraseID + 1];
end;

// Опустить регистр строки и очистить от знаков
procedure StringDown (var s: string);
var
  I: byte;
begin
  for I := 1 to Length (s) do
  begin
    case s[i] of
    'A'..'Z':
      s[i]:=chr(ord(s[i])+32);
    'А'..'Я':
      s[i]:=chr(ord(s[i])+32);
    'Ё':
      s[i]:='ё';
    '-','.',',','!','?':
      Delete(s,i,1);
    end;
  end;
end;

// Проверка, пуста ли строка
function StringEmpty (s: string): boolean;
var
  I: word;
begin
  I := 1;
  while I <= Length (s) do
  begin
    if S[I] = #32 then
      Delete (s,I,1)
    else
      Inc (I);
  end;
  if Length (s) > 0 then
    result := false
  else
    result := true;
end;

procedure ChangeCap (s: string);
begin
  AppCaption := s;
  Application.Title := s;
  CapChanged := true;
end;

procedure SetUpFrame;
begin
  bitblt(FrameImg.Canvas.Handle, 0, 0, 600, 300,
         FrameBuf.Canvas.Handle, 0, 0, SRCCOPY);
  FrameImg.Canvas.Font.Name := 'Courier New';
  FrameImg.Canvas.Font.Size := 10;
  FrameImg.Canvas.TextOut (18, 3, AppCaption);
  if AvatarLib > 0 then
    bitblt(FrameImg.Canvas.Handle, 374, 20, 224, 241,
           AvatarImg.Canvas.Handle, 0, 0, SRCCOPY);
  CapChanged := false;
end;

procedure ShowCheckBtns (Show: boolean = false);
begin
  if Show then
  begin
    AVMainForm.Height := 300;
    CheckAnswer := true;
  end
  else
    if CheckAnswer then
    begin
      AVMainForm.Height := 263;
      CheckAnswer := false;
    end;
end;

// Установка правильности ответа
procedure AnswerIs (Value: shortint);
begin
  case Value of
  Answer_Is_BAD:
    ;
  Answer_Is_NOTHING:
    ;
  Answer_Is_GOOD:
    ;
  end;
  ShowCheckBtns;
end;

function CreateRgnFromBMP (RGNBitmap: TBitmap): HRGN;
var
  TransColor: TColor;
  I, J: Integer;
  I_Width, I_Height: Integer;
  I_Left, I_Right: Integer;
  RectRGN: HRGN;
begin
  Result := 0;
  I_Width := RGNBitmap.Width;
  I_Height := RGNBitmap.Height;
  TransColor := RGNBitmap.Canvas.Pixels[0, 0];
  for I := 0 to I_Height - 1 do
  begin
    I_Left := -1;
    for J := 0 to I_Width - 1 do
    begin
      if I_Left < 0 then
      begin
        if RGNBitmap.Canvas.Pixels[J, I] <> TransColor
        then I_Left := j;
      end else
      if RGNBitmap.Canvas.Pixels[J, I] = TransColor then
      begin
        I_Right := J;
        RectRGN := CreateRectRGN (I_Left, I, I_Right, I + 1);
        if Result = 0 then Result := RectRGN else
        begin
          CombineRGN (Result, Result, RectRGN, RGN_OR);
          DeleteObject (RectRGN);
        end;
        I_Left := -1;
      end;
    end;
    if I_Left >= 0 then
    begin
      RectRGN := CreateRectRGN (I_Left, I, I_Width, I + 1);
      if Result = 0 then Result := RectRGN else
      begin
        CombineRGN (Result, Result, RectRGN, RGN_OR);
        DeleteObject (RectRGN);
      end;
    end;
  end;
end;

procedure FindVirtuals;
var
  R: integer;
  SR: TSearchRec;
  S: string;
begin
  AVMainForm.VirtList.Clear;
  R := FindFirst ('*.av', 0, SR);
  while R = 0 do
  begin
    S := Copy (SR.Name, 1, Length (SR.Name) - 3);
    if FileExists (S + '.db') then
      AVMainForm.VirtList.AddItem (S, nil);
    R := FindNext(SR);
  end;
  FindClose (SR);
  AVMainForm.VirtList.ItemIndex := 0;
end;

procedure TAVMainForm.FormCreate(Sender: TObject);
var
  WindowRgn: HRGN;
begin
  if not DirectoryExists ('virtuals') then
  begin
    CreateDirectory ('virtuals', nil);
    SetCurrentDir (GetCurrentDir + '\virtuals');
  end else
  begin
    SetCurrentDir (GetCurrentDir + '\virtuals');
    FindVirtuals;
  end;

  Application.Title := AppCaption;
  Dragging := false;
  SetMenu (SM_CHANGE_AV);

  FrameBuf := TBitmap.Create;
  FrameBuf.Width := 600;
  FrameBuf.Height := 300;
  FrameBuf.LoadFromResourceName (HInstance, 'WNDFRAME');
  FrameImg := TBitmap.Create;
  with FrameImg do
  begin
    Width := 600;
    Height := 300;
    Canvas.Font.Color := clWhite;
    Canvas.Font.Style := [fsBold];
    Canvas.Brush.Color := clPurple;
  end;
  SetUpFrame;
  CapChanged := true;
  AvatarLib := LoadLibrary ('avatar.dll');
  if AvatarLib <> 0 then
  begin
    AvatarImg := TBitmap.Create;
    AvatarImg.Width := 224;
    AvatarImg.Height := 241;
    AvatarImg.Canvas.Brush.Color := clBtnFace;
    AvatarImg.Canvas.Rectangle (-1, -1, 225, 242);
  end;

  windowRgn := CreateRgnFromBMP (FrameBuf);
  SetWindowRgn (Handle, WindowRgn, True);
  Top := (Screen.Height - ClientHeight) div 2;
  Left := (Screen.Width - ClientWidth) div 2;
  AnimateWindow (Handle, 50, AW_VER_POSITIVE);

  Timer.Enabled := true;
end;

procedure TAVMainForm.TimerTimer (Sender: TObject);
const
  MoveStep = 30;
  ResizeStep = 30;
begin
  if not Dragging then
  begin
    if ((Mouse.CursorPos.X > Left) and (Mouse.CursorPos.X < (Left + Width))
       and (Mouse.CursorPos.Y > Top) and (Mouse.CursorPos.Y < (Top + Height)))
    then
    begin
      AlphaDelta := 20;
      OutTime := 0;
    end else
    begin
      if OutTime=0 then OutTime := GetTickCount;
      if GetTickCount - OutTime > 1000 then
      begin
        AlphaDelta := -20;
        OutTime := 0;
      end;
    end;
  end;
  
  if ((AlphaDelta > 0) and (AlphaBlendValue < 255)) or
     ((AlphaDelta < 0) and (AlphaBlendValue > 95))
  then
    AlphaBlendValue := AlphaBlendValue + AlphaDelta;

  if not Dragging then
  begin
    if Top < 0 then
      if -Top > MoveStep then
        Top := Top + MoveStep
      else
        Top := 0;
    if left < 0 then
      if -left > MoveStep then
        Left := Left + MoveStep
      else
        Left := 0;
    if Top + Height > Screen.Height then
      if Top + Height - Screen.Height > MoveStep then
        Top := Top - MoveStep
      else
        Top := Screen.Height - height;
    if Left + Width > Screen.Width then
      if Left + Width - Screen.Width > MoveStep then
        Left := Left - MoveStep
      else
        Left := Screen.Width - Width;
  end;

  if Closing then
  begin
    if Height > 0 then
      Height := Height - ResizeStep
    else
      Close;
  end else
    case Hidding of
    1:
      if Height > 25 then
        Height := Height - ResizeStep
      else
      begin
        Hidding := 0;
        Height := 19;
      end;
    2:
      if Height < 240 then
        Height := Height + ResizeStep
      else
      begin
        Hidding := 0;
        if CheckAnswer then
          Height := 300
        else
          Height := 263;
      end;
    end;

  if CapChanged then SetUpFrame;
  bitblt(MainFrm.Canvas.Handle, 0, 0, 600, 300,
         FrameImg.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure SleepMode;
begin
  if not Closing then
  begin
    if not Hidded then Hidding := 1
    else Hidding := 2;
    case Hidding of
    1:
      begin
        CapBuf := AppCaption;
        ChangeCap ('Zzzz...');
        MainFrm.Memo.Enabled := false;
        Hidded := true;
      end;
    2:
      begin
        ChangeCap (CapBuf);
        if MainFrm.Menu = 0 then
        begin
          MainFrm.Memo.Enabled := true;
          MainFrm.Memo.SetFocus;
        end;
        Hidded:=false;
      end;
    end;
  end;
end;

procedure TMainFrm.FormCloseQuery (Sender: TObject; var CanClose: Boolean);
begin
  if not Closing then
  begin
    CanClose := false;
    Closing := true;
    Memo.Enabled := false;
  end else
    CanClose := true;
end;

procedure TMainFrm.WriteS (s: string);
begin
  Memo.Lines.Add(s);
end;

procedure TMainFrm.WriteC (c: string; add: boolean = false);
begin
  if not Add then
    Memo.Lines.Strings[memo.Lines.Count - 1] := c
  else
    Memo.Lines.Strings[memo.Lines.Count - 1] :=
      Memo.Lines.Strings[memo.Lines.Count-1] + c;
end;

procedure TMainFrm.MemoKeyPress (Sender: TObject; var Key: Char);
begin
  if (key in CorrectSym) and (Length (UserIn) < 41) then
  begin
    UserIn := UserIn + Key;
    WriteC (Key, true);
  end;
  if (Ord (Key) = 8) then
  begin
    Delete (UserIn, Length (UserIn), 1);
    WriteC (Username + UserIn);
  end;
  if (Key = #13) and (not StringEmpty (UserIn)) and
     (Length (UserIn) > 0) and (not Hidded) then
  begin
    AnalysePhrase (UserIn);
    WriteS (AVName + ': ' + GenPhrase);
    Memo.Lines.Add ('');
    WriteC (Username);
    UserIn := '';
    //ShowCheckBtns (true);
  end;
end;

procedure ShowHelp (n: byte);
var
  s: string;
begin
  case n of
  SH_ABOUT:
    s := '"Amiga Virtual"' + #13 + #13 + '©2012-2015, Денис Тимошин (TimKruz)';
  SH_FAQ:
    s := '';
  end;
  MessageDlg (s, mtInformation, [mbOk], 0);
end;

procedure TMainFrm.CopyBtnClick (Sender: TObject);
begin
  Memo.SelectAll;
  Memo.CopyToClipboard;
end;

procedure TMainFrm.ClearBtnClick (Sender: TObject);
begin
  Memo.Clear;
  Memo.Lines.Add ('');
  WriteC (Username);
  UserIn := '';
end;

procedure TMainFrm.FormMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  WantDrag: boolean;
begin
  if Button = mbLeft then
  begin
    WantDrag := false;
    if (y > 1) and (y < 17) then
    begin
      if (x > 530) and (x < 546) then ShowHelp (SH_ABOUT) else
      if (x > 548) and (x < 564) then SleepMode else
      if (x > 566) and (x < 582) then Close
      else WantDrag := true;
    end else
    if (y > 262) and CheckAnswer then
    begin
      if (x < 165) then AnswerIs (Answer_Is_BAD) else
      if (x > 165) and (x < 219) then AnswerIs (Answer_Is_NOTHING) else
      if (x > 219) then AnswerIs (Answer_Is_GOOD)
      else WantDrag := true;
    end else WantDrag := true;
    if WantDrag and (y < 19) then
    begin
      OldX := X;
      OldY := Y;
      Dragging := true;
    end;
  end;
end;

procedure TMainFrm.FormDestroy (Sender: TObject);
begin
  FrameImg.Free;
  FrameBuf.Free;
  if AvatarLib > 0 then
  begin
    AvatarImg.Free;
    FreeLibrary (AvatarLib);
  end;
end;

procedure TMainFrm.FormMouseUp (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Dragging := false;
end;

procedure TMainFrm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Dragging then
  begin
    Left := Left + X - OldX;
    Top := Top + Y - OldY;
  end;
end;

procedure TMainFrm.DelBtnClick (Sender: TObject);
begin
  if (VirtList.Items.Count > 0) and (VirtList.ItemIndex > -1) then
  begin
    if MessageDlg ('Вы уверены, что хотите удалить "' +
                   VirtList.Items[VirtList.ItemIndex] + '"?',
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      DeleteFile (VirtList.Items[VirtList.ItemIndex] + '.av');
      DeleteFile (VirtList.Items[VirtList.ItemIndex] + '.db');
      FindVirtuals;
    end;
  end else MessageDlg ('Список пуст или ничего не выбрано.',
                       mtWarning, [mbOk], 0);
end;

procedure TMainFrm.NewBtnClick (Sender: TObject);
var
  s: string;
begin
  s := InputBox ('Создание новой AV', 'Введите имя новой AV:', '');
  if not StringEmpty (s) then
  begin
    FileClose (FileCreate (s + '.av'));
    FileClose (FileCreate (s + '.db'));
    FindVirtuals;
  end;
end;

procedure TMainFrm.RenBtnClick(Sender: TObject);
var
  s: string;
begin
  if (VirtList.Items.Count > 0) and (VirtList.ItemIndex > -1) then
  begin
    s := InputBox ('Переименование AV', 'Введите новое имя для "'+
                   VirtList.Items[VirtList.ItemIndex] + '":',
                   VirtList.Items[VirtList.ItemIndex]);
    if not StringEmpty (s) then
    begin
      RenameFile (VirtList.Items[VirtList.ItemIndex] + '.av', s + '.av');
      RenameFile (VirtList.Items[VirtList.ItemIndex] + '.db', s + '.db');
      FindVirtuals;
    end;
  end else MessageDlg ('Список пуст или ничего не выбрано.',
                       mtWarning, [mbOk], 0);
end;

procedure TMainFrm.LoadBtnClick (Sender: TObject);
begin
  if (VirtList.Items.Count > 0) and (VirtList.ItemIndex > -1) then
  begin
    SetMenu (SM_CHAT);
    AVName := VirtList.Items[VirtList.ItemIndex];
    //LoadDB;
    ChangeCap (caption + ': ' + AVName);
    //ChangeAvatar;
  end else MessageDlg('Список пуст или ничего не выбрано.',
                      mtWarning, [mbOk], 0);
end;

procedure TMainFrm.SetMenu (x: byte);
begin
  Menu := x;
  case x of
  SM_CHAT:
    begin
      MenuPanel.Visible := false;
      Memo.Enabled := true;
      Memo.Visible := true;
      EditGrp.Enabled := true;
      SettingsGrp.Enabled := true;
      ResetChat;
      Memo.SetFocus;
    end;
  SM_CHANGE_AV:
    begin
      MenuPanel.Visible := true;
      Memo.Enabled := false;
      Memo.Visible := false;
      EditGrp.Enabled := false;
      SettingsGrp.Enabled := false;
      ChangeCap (Caption);
      if CheckAnswer then ShowCheckBtns;
    end;
  end;
end;

procedure TMainFrm.AboutClick (Sender: TObject);
begin
  ShowHelp (SH_ABOUT);
end;

procedure TMainFrm.QuitBtnClick (Sender: TObject);
begin
  Close;
end;

procedure TMainFrm.CloseAVBtnClick (Sender: TObject);
begin
  //SaveDB;
  SetMenu (SM_CHANGE_AV);
end;

procedure TMainFrm.SaveAVBtnClick (Sender: TObject);
begin
  //SaveDB;
end;

procedure TMainFrm.DontSaveBtnClick (Sender: TObject);
begin
  SetMenu (SM_CHANGE_AV);
end;

procedure TMainFrm.ChgAvatarBtnClick (Sender: TObject);
begin
  ChangeAvatar;
end;

procedure TMainFrm.FAQbtnClick (Sender: TObject);
begin
  ShowHelp (SH_FAQ);
end;

procedure TMainFrm.ResetChat;
begin
  Memo.Clear;
  WriteS (sysname + 'Система готова к работе.');
  Memo.Lines.Add ('');
  WriteC (Username);
end;

procedure TMainFrm.MemoExit (Sender: TObject);
begin
  if Memo.Enabled then Memo.SetFocus;
end;

procedure TMainFrm.ChangeAvatar;
var
  GetAvatar: TFuncGetAvatar;
begin
  @GetAvatar := GetProcAddress (AvatarLib, 'GetAvatar');
  if @GetAvatar = nil then
  begin
    MessageDlg ('Ошибка загрузки DLL!', mtError, [mbOk], 0);
    exit;
  end else
  begin
    GetAvatar ([1, 1], AvatarImg);
    CapChanged := true;
  end;
end;

end.
