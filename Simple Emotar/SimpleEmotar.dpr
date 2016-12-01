library SimpleEmotar;

{$R *.dres}

uses
  System.SysUtils,
  System.Classes,
  MainFormUnit in 'MainFormUnit.pas' {MainForm} ,
  SystemControlMessagesUnit
    in '..\..\AmigaVirtual\SystemControlMessagesUnit.pas';

const
  ControlCode = #2#1#1;
  Name = ControlCode + '>Simple Emotar';
  Help = 'HERE HELP COMES!' + #13 + 'IT''S SUPER EFFECTIVE!!!';

var
  FormState: (closed, opened);
  Buffer: String;

{$R *.res}

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

procedure SendData(Data: PChar); stdcall;
begin
  Buffer := Data;
  MainForm.DisplayEmotion(Buffer);
end;

function GetData: PChar; stdcall;
begin
  with MainForm do
    if CurrentEmotion <> '' then
    begin
      Buffer := CurrentEmotion;
      Result := PChar(Buffer);
      CurrentEmotion := '';
    end
    else
      Result := PChar(SCM_No_Message);
end;

procedure Start; stdcall;
begin
  if MainForm = nil then
    MainForm := TMainForm.Create(nil);
end;

exports GetName, GetHelp, OpenWindow, CloseWindow, SendData, GetData, Start;

begin

end.
