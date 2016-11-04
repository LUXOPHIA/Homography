unit Core;

interface //#################################################################### ■

uses System.Types, System.Threading;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% THomography

     THomography = class
     private
       T00, T01, T10 :Single;
       A00, A01, A10 :TPointF;
       ///// メソッド
       procedure Calc;
     protected
       _Poin00 :TPointF;
       _Poin01 :TPointF;
       _Poin10 :TPointF;
       _Poin11 :TPointF;
       ///// アクセス
       procedure SetPoin00( const Poin00_:TPointF );
       procedure SetPoin01( const Poin01_:TPointF );
       procedure SetPoin10( const Poin10_:TPointF );
       procedure SetPoin11( const Poin11_:TPointF );
     public
       constructor Create( const P00_,P01_,P10_,P11_:TPointF );
       ///// プロパティ
       property Poin00 :TPointF read _Poin00 write SetPoin00;
       property Poin01 :TPointF read _Poin01 write SetPoin01;
       property Poin10 :TPointF read _Poin10 write SetPoin10;
       property Poin11 :TPointF read _Poin11 write SetPoin11;
       ///// メソッド
       function ToScreen( const P_:TPointF; var S_:TPointF ) :Boolean;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

    _ThreadPool_ :TThreadPool;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function LoopMod( const X_,Range_:Integer ) :Integer;

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% THomography

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

procedure THomography.Calc;
var
   E01, E10, S :TPointF;
begin
     E01 := _Poin01 - _Poin11;
     E10 := _Poin10 - _Poin11;

	   T00 := E01.X * E10.Y - E01.Y * E10.X;

     A00 := T00 * _Poin00;

	   S := _Poin00 - _Poin01 - _Poin10 + _Poin11;

     T10 := S.Y * E01.X - S.X * E01.Y;
     T01 := S.X * E10.Y - S.Y * E10.X;

     A01 := ( T00 + T01 ) * _Poin01 - A00;
     A10 := ( T00 + T10 ) * _Poin10 - A00;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure THomography.SetPoin00( const Poin00_:TPointF );
begin
     _Poin00 := Poin00_;  Calc;
end;

procedure THomography.SetPoin01( const Poin01_:TPointF );
begin
     _Poin01 := Poin01_;  Calc;
end;

procedure THomography.SetPoin10( const Poin10_:TPointF );
begin
     _Poin10 := Poin10_;  Calc;
end;

procedure THomography.SetPoin11( const Poin11_:TPointF );
begin
     _Poin11 := Poin11_;  Calc;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor THomography.Create( const P00_,P01_,P10_,P11_:TPointF );
begin
     _Poin00 := P00_;  _Poin01 := P01_;
     _Poin10 := P10_;  _Poin11 := P11_;

     Calc;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function THomography.ToScreen( const P_:TPointF; var S_:TPointF ) :Boolean;
var
   T :Single;
begin
     T := T01 * P_.X + T10 * P_.Y + T00;

     Result := ( Abs( T ) > 1E-6 );

     if Result then S_ := ( A01 * P_.X + A10 * P_.Y + A00 ) / T;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function LoopMod( const X_,Range_:Integer ) :Integer;
begin
     Result := X_ mod Range_;  if Result < 0 then Inc( Result, Range_ );
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

     _ThreadPool_ := TThreadPool.Create;

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

     _ThreadPool_.Free;

end. //######################################################################### ■
