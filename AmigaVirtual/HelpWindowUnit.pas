unit HelpWindowUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  HelpIndexName = (Title, First, Last);

const
  HelpLast = 3;
  HelpIndex: array [1 .. HelpLast, HelpIndexName] of cardinal = ((6, 7, 10),
    (40, 40, 59), (95, 96, 98));

type
  THelpForm = class(TForm)
    HelpMemo: TMemo;
    FormCaption: TLabel;
    CloseButton: TImage;
    HelpMenu: TComboBox;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowWithText(Text: String);
    procedure ShowWithStrings(Text: TStrings);
    function GenerateHelpTextForMemo(First, Last: Integer): TStrings;

    procedure CloseButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CloseButtonMouseEnter(Sender: TObject);
    procedure CloseButtonMouseLeave(Sender: TObject);
    procedure CloseButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LoadHelpTexts;
    procedure HelpMenuChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Dragging: boolean;
    OldX, OldY, SavedX, SavedY: Integer;

    HelpTexts: array of TStrings;

    procedure ResetButtonsImages;
  public
    CurrentTopic: Integer;

    procedure OpenTopic(ID: Integer; Silence: boolean = true);
    function AddTopic(Name: String; Text: TStrings): Integer; overload;
    function AddTopic(Name: String; Text: String): Integer; overload;
  end;

var
  HelpForm: THelpForm;

implementation

uses
  MainWindowUnit;

{$R *.dfm}

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

function THelpForm.AddTopic(Name: String; Text: TStrings): Integer;
begin
  HelpMenu.Items.Add(Name);
  SetLength(HelpTexts, Length(HelpTexts) + 1);
  HelpTexts[Length(HelpTexts) - 1] := Text;
  Result := Length(HelpTexts) - 1;
end;

function THelpForm.AddTopic(Name, Text: String): Integer;
begin
  HelpMenu.Items.Add(Name);
  SetLength(HelpTexts, Length(HelpTexts) + 1);
  while Text <> '' do
    HelpMemo.Lines.Add(Slice(Text, #13));
  Result := Length(HelpTexts) - 1;
end;

procedure THelpForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure THelpForm.CloseButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MainForm.ButtonImage.Draw(CloseButton.Canvas, 0, 0, 14);
  CloseButton.Repaint;
end;

procedure THelpForm.CloseButtonMouseEnter(Sender: TObject);
begin
  MainForm.ButtonImage.Draw(CloseButton.Canvas, 0, 0, 9);
  CloseButton.Repaint;
end;

procedure THelpForm.CloseButtonMouseLeave(Sender: TObject);
begin
  ResetButtonsImages;
  CloseButton.Repaint;
end;

procedure THelpForm.CloseButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ResetButtonsImages;
  CloseButton.Repaint;
end;

procedure THelpForm.FormShow(Sender: TObject);
begin
  SavedX := 0;
  SavedY := MainForm.Width;
  ResetButtonsImages;
end;

function THelpForm.GenerateHelpTextForMemo(First, Last: Integer): TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := First to Last - 1 do
  begin
    Result.Add(MainForm.LanguageData[i]);
    Result.Add('');
  end;
  Result.Add(MainForm.LanguageData[Last]);
end;

procedure THelpForm.HelpMenuChange(Sender: TObject);
begin
  OpenTopic(HelpMenu.ItemIndex);
end;

procedure THelpForm.LoadHelpTexts;
var
  i: Integer;
begin
  with MainForm do
  begin
    HelpForm.FormCaption.Caption := LanguageData[4];
    HelpMenu.Clear;
    HelpMenu.Text := LanguageData[5];
    SetLength(HelpTexts, HelpLast);
    for i := 1 to HelpLast do
    begin
      HelpMenu.Items.Add(LanguageData[HelpIndex[i, Title]]);
      HelpTexts[i - 1] := GenerateHelpTextForMemo(HelpIndex[i, First],
        HelpIndex[i, Last]);
    end;
  end;
end;

procedure THelpForm.OpenTopic(ID: Integer; Silence: boolean = true);
begin
  HelpMemo.Clear;
  HelpMemo.Lines.AddStrings(HelpTexts[ID]);
  HelpMenu.ItemIndex := ID;
  CurrentTopic := ID;
  if not Silence then
    HelpForm.Show;
end;

procedure THelpForm.ResetButtonsImages;
begin
  MainForm.ButtonImage.Draw(CloseButton.Canvas, 0, 0, 4);
  CloseButton.Repaint;
end;

procedure THelpForm.ShowWithStrings(Text: TStrings);
begin
  HelpMemo.Clear;
  HelpMemo.Lines.AddStrings(Text);
  Show;
end;

procedure THelpForm.ShowWithText(Text: String);
begin
  HelpMemo.Clear;
  while Text <> '' do
    HelpMemo.Lines.Add(Slice(Text, #13));
  Show;
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  SavedX := 0;
  SavedY := MainForm.Width;
  CurrentTopic := 0;
end;

procedure THelpForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    OldX := X;
    OldY := Y;
    Dragging := true;
  end;
end;

procedure THelpForm.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Dragging then
  begin
    Left := Left + X - OldX;
    Top := Top + Y - OldY;
    SavedX := Top - MainForm.Top;
    SavedY := Left - MainForm.Left;
  end;
end;

procedure THelpForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Dragging := false;
end;

procedure THelpForm.FormPaint(Sender: TObject);
begin
  Top := MainForm.Top + SavedX;
  Left := MainForm.Left + SavedY;
end;

end.
