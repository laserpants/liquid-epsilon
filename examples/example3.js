// Main
var $Main=
 ($Main ? $Main : {});
$Main.$_24okUNQ7=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["input"]);
          var $__3=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__4=
           new _A_($Main.$_24okUNQ14,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Main.$_24okUNQ14=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Main.$__5__17NEW3,[$_24x,$_24x2]);
          var $__4=
           new _A_($Util.$DOM.$_7e_3e,[$_24x2,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);});
$Main.$__5__17NEW3=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["input"]);
          var $__4=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__5=
           new _A_($Main.$_24okUNQ21,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Main.$_24okUNQ21=
 new _F_(function($_24x,$_24x2,$_24x3)
         {var $__=
           new _A_($Util.$FRP.$inputValue,[$_24x2]);
          var $__5=
           new _A_($Util.$DOM.$setValue,[$_24x3]);
          var $__6=
           new _A_($UHC.$Base.$fmap,[$Util.$FRP.$Functor__DCT18__0__0,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_24,[$__6,$__]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$Util.$FRP.$attach,$__7]);
          var $__9=
           new _A_($Util.$DOM.$_7e_3e,[$_24x3,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__8]);});
$Main.$__5__6=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$DOM.$documentBody,$Main.$_24okUNQ7]);}),[]);
$Main.$__5__4=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Util.$Router.$wrap,$Main.$__5__6]);}),[]);
$Main.$main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Main.$__5__4,$Util.$Router.$onReady]);}),[]);
var $main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Run.$ehcRunMain,[$Main.$main]);}),[]);
_e_(new _A_($main,[[]]));
