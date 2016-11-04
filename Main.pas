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
    procedure FormDestroy(Sender: TObject);
    procedure Image1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
    _MouseS :Byte;
    _BMP    :TBitmapData;
    /////
    procedure DrawPoin( const Canvas_:TCanvas;
                        const Center_:TPointF;
                        const Radius_:Single;
                        const Color_ :TAlphaColor );
    function GetColor( const P_:TPointF ) :TAlphaColor;
  public
    { public 宣言 }
    _Trans :THomography;
    /////
    procedure Cutout;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

uses System.Math, System.Threading;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure TForm1.DrawPoin( const Canvas_:TCanvas;
                           const Center_:TPointF;
                           const Radius_:Single;
                           const Color_ :TAlphaColor );
var
   R1 :Single;
begin
     R1 := Radius_ + 2;

     with Canvas_ do
     begin
          Fill.Color := TAlphaColorRec.White;

          with Center_ do FillEllipse( TRectF.Create( X-R1, Y-R1, X+R1, Y+R1 ), 1 );

          Fill.Color := Color_;

          with Center_ do FillEllipse( TRectF.Create( X-Radius_, Y-Radius_,
                                                      X+Radius_, Y+Radius_ ), 1 );
     end;
end;

function TForm1.GetColor( const P_:TPointF ) :TAlphaColor;
var
   X, Y :Integer;
begin
     X := LoopMod( Floor( P_.X ), 512 );
     Y := LoopMod( Floor( P_.Y ), 512 );

     Result := _BMP.GetPixel( X, Y );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

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

               if _Trans.ToScreen( P, S ) then C^ := GetColor( S )
                                          else C^ := TAlphaColorRec.Null;

               Inc( C );
          end;
     end,
     _ThreadPool_ );

     Image2.Bitmap.Unmap( B );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _MouseS := 0;

     Image1.AutoCapture := True;

     Image1.Bitmap.LoadFromFile( '..\..\_DATA\Perspective.png' );
     Image1.Bitmap.Map( TMapAccess.Read, _BMP );

     _Trans := THomography.Create(
          TPointF.Create( 270, 176 ), TPointF.Create( 488, 203 ),
          TPointF.Create(  66, 302 ), TPointF.Create( 229, 433 ) );

     Image2.Bitmap.SetSize( 512, 512 );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Trans.Free;

     Image1.Bitmap.Unmap( _BMP );
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Image1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
     with Canvas do
     begin
          with Stroke do
          begin
               Kind      := TBrushKind.Solid;
               Join      := TStrokeJoin.Round;
               Thickness := 2;
               Color     := TAlphaColorRec.White;
          end;

          DrawPolygon( [ _Trans.Poin00, _Trans.Poin01,
                         _Trans.Poin11, _Trans.Poin10 ], 1 );
     end;

     DrawPoin( Canvas, _Trans.Poin00, 5, $FF00AA00 );
     DrawPoin( Canvas, _Trans.Poin01, 5, $FFFD531D );
     DrawPoin( Canvas, _Trans.Poin10, 5, $FF00AFFF );
     DrawPoin( Canvas, _Trans.Poin11, 5, $FFA88F00 );
end;

//------------------------------------------------------------------------------

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

//------------------------------------------------------------------------------

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     Cutout;

     Image1.Repaint;
end;

end. //######################################################################### ■
