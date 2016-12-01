unit MainFormUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.Menus;

type
  TMode = (off, on);

  TMainForm = class(TForm)
    EmotarImage: TImage;
    GenerateBtn: TButton;
    PopupMenu: TPopupMenu;
    HeadTypeBtn: TBitBtn;
    HairTypeBtn: TBitBtn;
    MouthTypeBtn: TBitBtn;
    EyesTypeBtn: TBitBtn;
    PupilsTypeBtn: TBitBtn;
    EyebrowsTypeBtn: TBitBtn;
    SignTypeBtn: TBitBtn;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    FormCaption: TLabel;
    Timer: TTimer;
    procedure ButtonClick(Sender: TObject);
    procedure GenerateBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private const
    GMOnWidth = 200;
    GMOffWidth = 100;
    
  var
    GenerateMode: TMode;
    procedure GenerateEmotion;
    procedure ToogleGenerateMode;
    procedure SetGenerateMode(M: TMode);
  public
    CurrentEmotion: String;
    procedure DisplayEmotion(EmoCode: String);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.ButtonClick(Sender: TObject);
var
  B: TBitBtn;
begin
  B := TBitBtn(Sender);
  with Mouse.CursorPos do
    PopupMenu.Popup(X, Y);
end;

procedure TMainForm.DisplayEmotion(EmoCode: String);
begin
//
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetGenerateMode(off);
  CurrentEmotion := '';
end;

procedure TMainForm.GenerateBtnClick(Sender: TObject);
begin
  ToogleGenerateMode;
end;

procedure TMainForm.GenerateEmotion;
begin
  //
end;

procedure TMainForm.SetGenerateMode(M: TMode);
begin
  case M of
    off:
      Width := GMOffWidth;
    on:
      Width := GMOnWidth;
  end;
  GenerateMode := M;
end;

procedure TMainForm.ToogleGenerateMode;
begin
  case GenerateMode of
    off:
      begin
        Width := GMOnWidth;
        GenerateMode := on;
      end;
    on:
      begin
        Width := GMOffWidth;
        GenerateMode := off;
        GenerateEmotion;
      end;
  end;
end;

end.
