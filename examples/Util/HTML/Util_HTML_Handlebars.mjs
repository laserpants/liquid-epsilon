// Util.HTML.Handlebars
var $Util=
 ($Util ? $Util : {});
$Util.$HTML=
 ($Util.$HTML ? $Util.$HTML : {});
$Util.$HTML.$Handlebars=
 ($Util.$HTML.$Handlebars ? $Util.$HTML.$Handlebars : {});
$Util.$HTML.$Handlebars.$each=
 new _F_(function($i)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["{{/each}}"]);
          var $__3=
           new _A_($UHC.$Base.$packedStringToString,["}}"]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["{{#each "]);
          var $__5=
           new _A_($UHC.$Base.$_2b_2b,[$__4,$i]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          return new _A_($Util.$HTML.$parent,[$__6,$__5,$__3,$__]);});
$Util.$HTML.$Handlebars.$_24okUNQ15=
 new _F_(function($__,$obj,$_24x)
         {var $__4=
           new _A_($Util.$Handlebars.$render,[$__,$_24x,$obj]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$Util.$HTML.$Handlebars.$_24okUNQ19]);});
$Util.$HTML.$Handlebars.$_24okUNQ19=
 new _F_(function($_24x)
         {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);});
$Util.$HTML.$Handlebars.$renderWithHtml=
 new _F_(function($__,$htm,$obj)
         {var $__4=
           new _A_($Util.$HTML.$renderHtml,[$htm]);
          var $__5=
           new _A_($UHC.$Base.$_24,[$Util.$Handlebars.$compile,$__4]);
          var $__6=
           new _A_($Util.$HTML.$Handlebars.$_24okUNQ15,[$__,$obj]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__6]);});
$Util.$HTML.$Handlebars.$handlebar=
 new _F_(function($s)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["}}"]);
          var $__3=
           new _A_($UHC.$Base.$packedStringToString,["{{"]);
          var $__4=
           new _A_($UHC.$Base.$_2b_2b,[$__3,$s]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          return new _A_($Util.$HTML.$leaf,[$__5,$__4,$__]);});
$Util.$HTML.$Handlebars.$_24_3f=
 new _F_(function($a)
         {return new _A_($UHC.$Base.$_2e,[$a,$Util.$HTML.$Handlebars.$handlebar]);});
