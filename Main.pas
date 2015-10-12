unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  Core;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
    _MouseS :Byte;
    /////
    procedure DrawPoin( const Center_:TPointF; const Size_:Single; const Color_:TAlphaColor );
    function GetMapColor( const P_:TPointF ) :TAlphaColor;
  public
    { public 宣言 }
    _BMP   :TBitmap;
    _MAP   :TBitmapData;
    _Trans :THomography;
    /////
    procedure ShowFrame;
    procedure Cutout;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math, System.Threading;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TForm1.DrawPoin( const Center_:TPointF; const Size_:Single; const Color_:TAlphaColor );
var
   R0, R1 :Single;
begin
     R0 := Size_ / 2;
     R1 := R0 + 2;

     with Image1.Bitmap.Canvas do
     begin
          Fill.Color := TAlphaColorRec.White;

          with Center_ do FillEllipse( TRectF.Create( X-R1, Y-R1, X+R1, Y+R1 ), 1 );

          Fill.Color := Color_;

          with Center_ do FillEllipse( TRectF.Create( X-R0, Y-R0, X+R0, Y+R0 ), 1 );
     end;
end;

function TForm1.GetMapColor( const P_:TPointF ) :TAlphaColor;
var
   X, Y :Integer;
begin
     X := LoopMod( Floor( P_.X ), 512 );
     Y := LoopMod( Floor( P_.Y ), 512 );

     Result := _MAP.GetPixel( X, Y );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

procedure TForm1.ShowFrame;
begin
     with Image1.Bitmap.Canvas do
     begin
          BeginScene;

          DrawBitmap( _BMP, TRectF.Create( 0, 0, 512, 512 ), TRectF.Create( 0, 0, 512, 512 ), 1 );

          Stroke.Thickness := 2;
          Stroke.Color     := TAlphaColorRec.White;
          Stroke.Join      := TStrokeJoin.Round;

          DrawPolygon( [ _Trans.Poin00, _Trans.Poin01, _Trans.Poin11, _Trans.Poin10, _Trans.Poin00 ], 1 );

          DrawPoin( _Trans.Poin00, 10, $FF00AA00 );
          DrawPoin( _Trans.Poin01, 10, $FFFD531D );
          DrawPoin( _Trans.Poin10, 10, $FF00AFFF );
          DrawPoin( _Trans.Poin11, 10, $FFA88F00 );

          EndScene;
     end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Cutout;
var
   B :TBitmapData;
begin
     Image2.Bitmap.Map( TMapAccess.Write, B );

     TParallel.For( 0, 512-1, procedure( Y:Integer )
     var
        X :Integer;
        P, S :TPointF;
        C :PAlphaColor;
     begin
          P.Y := ( Y + 0.5 ) / 512;

          C := B.GetScanline( Y );

          for X := 0 to 512-1 do
          begin
               P.X := ( X + 0.5 ) / 512;

               if _Trans.ToScreen( P, S ) then C^ := GetMapColor( S )
                                          else C^ := TAlphaColorRec.Null;

               Inc( C );
          end;
     end );

     Image2.Bitmap.Unmap( B );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     Image1.Bitmap.SetSize( 512, 512 );
     Image2.Bitmap.SetSize( 512, 512 );

     Image1.AutoCapture := True;

     _BMP := TBitmap.Create;

     _MouseS := 0;

     _BMP.LoadFromFile( '..\..\» DATA\Perspective.png' );
     _BMP.Map( TMapAccess.Read, _MAP );

     _Trans := THomography.Create(
          TPointF.Create( 270, 176 ), TPointF.Create( 488, 203 ),
          TPointF.Create(  66, 302 ), TPointF.Create( 229, 433 ) );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Trans.Free;

     _BMP.Unmap( _MAP );
     _BMP.Free;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     ShowFrame;
     Cutout;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
   P :TPointF;
   L, D :Single;
begin
     P := TPointF.Create( X, Y );

     _MouseS := 0;

     L := Single.MaxValue;

     D := P.Distance( _Trans.Poin00 );  if D < L then begin  _MouseS := 1;  L := D;  end;
     D := P.Distance( _Trans.Poin01 );  if D < L then begin  _MouseS := 2;  L := D;  end;
     D := P.Distance( _Trans.Poin10 );  if D < L then begin  _MouseS := 3;  L := D;  end;
     D := P.Distance( _Trans.Poin11 );  if D < L then begin  _MouseS := 4;           end;

     Image1MouseMove( Sender, Shift, X, Y );
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
     case _MouseS of
       1: _Trans.Poin00 := TPointF.Create( X, Y );
       2: _Trans.Poin01 := TPointF.Create( X, Y );
       3: _Trans.Poin10 := TPointF.Create( X, Y );
       4: _Trans.Poin11 := TPointF.Create( X, Y );
     end;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     Image1MouseMove( Sender, Shift, X, Y );

     _MouseS := 0;
end;

end. //######################################################################### ■
