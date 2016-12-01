unit SplashScreenUnit;

interface

uses
  Windows, Dialogs, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  Vcl.Imaging.PNGImage, Graphics, Types;

type
  TSplashScreen = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure RenderForm;
    procedure SetProgress(Progress: Single);
  private
    BMPBack, BMPGray, BMPLogo, BMPLogoPart: TBitmap;
    LoadingProgress: Single;
  public
  end;

var
  SplashScreen: TSplashScreen;

implementation

{$R *.dfm}

uses MainWindowUnit;

procedure BuildPNG2BMP(PNGID: string; var BMP: TBitmap);
const
  MaxPixelCountA = MaxInt div SizeOf(TRGBQuad);
type
  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array [0 .. MaxPixelCountA - 1] of TRGBQuad;
var
  i1, i2: Integer;
  PNG: TPngImage;
  PRGBArr: PRGBAArray;
  pByteArr: pByteArray;
begin
  PNG := TPngImage.Create;
  try
    PNG.LoadFromResourceName(HInstance, PNGID);
    PNG.CreateAlpha;
    BMP.Assign(PNG);
    BMP.PixelFormat := pf32bit;
    for i2 := 0 to BMP.Height - 1 do
    begin
      PRGBArr := BMP.ScanLine[i2];
      pByteArr := PNG.AlphaScanline[i2];
      for i1 := 0 to BMP.Width - 1 do
      begin
        PRGBArr[i1].rgbReserved := pByteArr[i1];
      end;
    end;
  finally
    PNG.free;
  end;
end;

procedure Multed(BMPout: TBitmap);
const
  MaxPixelCountA = MaxInt div SizeOf(TRGBQuad);
type
  PRGBAArray = ^TRGBAArray;
  TRGBAArray = array [0 .. MaxPixelCountA - 1] of TRGBQuad;
var
  x, y: Integer;
  RowOut: PRGBAArray;
begin
  for y := 0 to BMPout.Height - 1 do
  begin
    RowOut := BMPout.ScanLine[y];
    for x := 0 to BMPout.Width - 1 do
    begin
      RowOut[x].rgbBlue :=
        byte(trunc(RowOut[x].rgbBlue * RowOut[x].rgbReserved / 255));
      RowOut[x].rgbGreen :=
        byte(trunc(RowOut[x].rgbGreen * RowOut[x].rgbReserved / 255));
      RowOut[x].rgbRed :=
        byte(trunc(RowOut[x].rgbRed * RowOut[x].rgbReserved / 255));
    end;
  end;
end;

procedure TSplashScreen.FormCreate(Sender: TObject);
begin
  LoadingProgress := -5.0;

  BMPLogo := TBitmap.Create;
  BuildPNG2BMP('LOGO', BMPLogo);
  BMPBack := TBitmap.Create;
  BuildPNG2BMP('SSBACK', BMPBack);

  BMPLogoPart := TBitmap.Create;
  BMPLogoPart.Width := BMPLogo.Width;
  BMPLogoPart.Height := BMPLogo.Height;

  BMPGray := TBitmap.Create;
  BMPGray.Width := BMPBack.Width;
  BMPGray.Height := BMPBack.Height;
  BMPGray.Assign(BMPBack);

  Multed(BMPBack);
  SetProgress(0);
  RenderForm;
end;

procedure TSplashScreen.RenderForm;
var
  bf: TBlendFunction;
  TopLeft, pnt: TPoint;
  sz: TSize;
begin
  SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or
    WS_EX_LAYERED);

  sz.cx := BMPBack.Width;
  sz.cy := BMPBack.Height;
  pnt := Point(0, 0);

  with bf do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    AlphaFormat := AC_SRC_ALPHA;
    SourceConstantAlpha := 255;
  end;
  TopLeft := BoundsRect.TopLeft;

  UpdateLayeredWindow(Handle, GetDC(0), @TopLeft, @sz, BMPBack.Canvas.Handle,
    @pnt, 0, @bf, ULW_ALPHA);
end;

procedure TSplashScreen.SetProgress(Progress: Single);
var
  h: Integer;
begin
  LoadingProgress := Progress / 100 * 105 - 5;
  with BMPLogoPart.Canvas do
  begin
    Draw(0, 0, BMPLogo);
    Brush.Color := clBlack;
    h := BMPLogo.Height - Round(BMPLogo.Height / 100 * LoadingProgress);
    Rectangle(0, 0, BMPLogo.Width, h);
    BMPBack.Assign(BMPGray);
    BitBlt(BMPBack.Canvas.Handle, 188, 70, BMPLogo.Width, BMPLogo.Height,
      Handle, 0, 0, SRCPAINT);
  end;
  RenderForm;
end;

end.
