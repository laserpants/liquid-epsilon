// Util.Base
var $Util=
 ($Util ? $Util : {});
$Util.$Base=
 ($Util.$Base ? $Util.$Base : {});
$Util.$Base.$addOneUNQ8=
 new _F_(function($body,$src)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["script"]);
          var $__4=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__5=
           new _A_($Util.$Base.$_24okUNQ14,[$body,$src]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Util.$Base.$_24okUNQ14=
 new _F_(function($body,$src,$_24x)
         {var $__=
           new _A_($Util.$DOM.$_7e_3e,[$_24x,$body]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["src"]);
          var $__6=
           new _A_($Util.$DOM.$setAttribute,[$_24x,$__5,$src]);
          var $__7=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,["text/javascript"]);
          var $__9=
           new _A_($UHC.$Base.$packedStringToString,["type"]);
          var $__10=
           new _A_($Util.$DOM.$setAttribute,[$_24x,$__9,$__8]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__7]);});
$Util.$Base.$addScripts=
 new _F_(function($xs)
         {var $__=
           new _A_($UHC.$Base.$flip,[$UHC.$Base.$map,$xs]);
          var $__3=
           new _A_($UHC.$Base.$_2e,[$__,$Util.$Base.$addOneUNQ8]);
          var $__4=
           new _A_($UHC.$Base.$sequence,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__5=
           new _A_($UHC.$Base.$_2e,[$__4,$__3]);
          var $__6=
           new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$DOM.$documentBody,$__5]);
          var $__7=
           new _A_($Util.$Router.$wrap,[$__6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$Util.$Router.$onReady]);});
$Util.$Base.$addScript=
 new _F_(function($s)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$s,$UHC.$Base.$_5b_5d]);
          return new _A_($Util.$Base.$addScripts,[$__]);});
