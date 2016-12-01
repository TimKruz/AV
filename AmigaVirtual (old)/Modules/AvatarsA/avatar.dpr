library avatar;

uses Graphics;

{$R *.res}
{$R AvatarPieces.res}

function IntToStr(x:integer):string;
begin
  str(x,result);
end;

function GetAvatar(params:array of byte;var ResImg:TBitmap):byte;
var buf:tbitmap;
begin
  result:=0;
  buf:=tbitmap.Create;
  buf.Width:=224;
  buf.Height:=241;
  //Разбиваем code на ID требуемых компонентов.
  //Рисуем нужные компоненты в битмап.
  ResImg.LoadFromResourceName(HInstance,'FACE'+inttostr(params[0]));
  buf.LoadFromResourceName(HInstance,'EYES'+inttostr(params[1]));
  buf.Free;
end;

exports GetAvatar;

end.
