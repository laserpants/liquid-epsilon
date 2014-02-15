// Main
var $Main=
 ($Main ? $Main : {});
$Main.$fUNQ34=
 new _F_(function($str,$x)
         {var $__=
           new _A_($UHC.$Base.$Just__,[$str]);
          var $__4=
           new _A_($Control.$Applicative.$_3c_24_3e,[$UHC.$Base.$Functor__DCT74__404__0,$Util.$DOM.$setInnerHtml,$x]);
          return new _A_($Control.$Applicative.$_3c_2a_3e,[$Control.$Applicative.$Applicative__DCT181__2__0,$__4,$__]);});
$Main.$out=
 new _F_(function($str)
         {var $__=
           new _A_($Main.$fUNQ34,[$str]);
          var $__3=
           new _A_($UHC.$Base.$_2e,[$Data.$Maybe.$maybeToList,$__]);
          var $__4=
           new _A_($UHC.$Base.$sequence__,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__5=
           new _A_($UHC.$Base.$_2e,[$__4,$__3]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["myStdOut"]);
          var $__7=
           new _A_($Util.$DOM.$getElementById,[$__6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__5]);});
$Main.$byUser=
 new _F_(function($n)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["<br>"]);
          var $__3=
           new _A_($UHC.$Base.$_2b_2b,[$n,$__]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["show posts by user "]);
          var $__5=
           new _A_($UHC.$Base.$_2b_2b,[$__4,$__3]);
          return new _A_($UHC.$Base.$_24,[$Main.$out,$__5]);});
$Main.$showPost=
 new _F_(function($pid)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["<br>"]);
          var $__3=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$pid]);
          var $__4=
           new _A_($UHC.$Base.$_2b_2b,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["show post #"]);
          var $__6=
           new _A_($UHC.$Base.$_2b_2b,[$__5,$__4]);
          return new _A_($UHC.$Base.$_24,[$Main.$out,$__6]);});
$Main.$_24okUNQ65=
 new _F_(function($_24x)
         {var $__=
           new _A_($Main.$byUser,[$_24x]);
          var $__3=
           new _A_($UHC.$Base.$_24,[$Util.$Router.$go,$__]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["please"]);
          var $__5=
           new _A_($Util.$Router.$atom,[$__4]);
          return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$__5,$__3]);});
$Main.$__14__58=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Util.$Router.$Monad__DCT4__0__0,$Util.$Router.$str,$Main.$_24okUNQ65]);}),[]);
$Main.$__14__57=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["user"]);}),[]);
$Main.$__14__56=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__57]);}),[]);
$Main.$__14__53=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__56,$Main.$__14__58]);}),[]);
$Main.$__14__52=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["post"]);}),[]);
$Main.$__14__51=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__52]);}),[]);
$Main.$postsByUserRoute=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__51,$Main.$__14__53]);}),[]);
$Main.$_24okUNQ58=
 new _F_(function($_24x)
         {var $__=
           new _A_($Main.$showPost,[$_24x]);
          return new _A_($UHC.$Base.$_24,[$Util.$Router.$go,$__]);});
$Main.$__14__83=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Util.$Router.$Monad__DCT4__0__0,$Util.$Router.$num,$Main.$_24okUNQ58]);}),[]);
$Main.$__14__82=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["show"]);}),[]);
$Main.$__14__81=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__82]);}),[]);
$Main.$__14__78=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__81,$Main.$__14__83]);}),[]);
$Main.$__14__77=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["post"]);}),[]);
$Main.$__14__76=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__77]);}),[]);
$Main.$showPostRoute=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__76,$Main.$__14__78]);}),[]);
$Main.$__14__98=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["<br>"]);}),[]);
$Main.$__14__97=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["create new post"]);}),[]);
$Main.$__14__95=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$Main.$__14__97,$Main.$__14__98]);}),[]);
$Main.$newPost=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Main.$out,$Main.$__14__95]);}),[]);
$Main.$__14__110=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$go,[$Main.$newPost]);}),[]);
$Main.$__14__109=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["new"]);}),[]);
$Main.$__14__108=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__109]);}),[]);
$Main.$__14__105=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__108,$Main.$__14__110]);}),[]);
$Main.$__14__104=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["post"]);}),[]);
$Main.$__14__103=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__104]);}),[]);
$Main.$newPostRoute=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__103,$Main.$__14__105]);}),[]);
$Main.$threeNumbers=
 new _F_(function($a,$b,$c)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["<br>"]);
          var $__5=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$c]);
          var $__6=
           new _A_($UHC.$Base.$_2b_2b,[$__5,$__]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToString,[", "]);
          var $__8=
           new _A_($UHC.$Base.$_2b_2b,[$__7,$__6]);
          var $__9=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$b]);
          var $__10=
           new _A_($UHC.$Base.$_2b_2b,[$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToString,[", "]);
          var $__12=
           new _A_($UHC.$Base.$_2b_2b,[$__11,$__10]);
          var $__13=
           new _A_($UHC.$Base.$show,[$UHC.$Base.$Show__DCT74__128__0,$a]);
          var $__14=
           new _A_($UHC.$Base.$_2b_2b,[$__13,$__12]);
          return new _A_($UHC.$Base.$_24,[$Main.$out,$__14]);});
$Main.$_24okUNQ41=
 new _F_(function($_24x)
         {var $__=
           new _A_($Main.$_24okUNQ47,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Util.$Router.$Monad__DCT4__0__0,$Util.$Router.$num,$__]);});
$Main.$_24okUNQ47=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Main.$_24okUNQ52,[$_24x,$_24x2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$Util.$Router.$Monad__DCT4__0__0,$Util.$Router.$num,$__]);});
$Main.$_24okUNQ52=
 new _F_(function($_24x,$_24x2,$_24x3)
         {var $__=
           new _A_($Main.$threeNumbers,[$_24x,$_24x2,$_24x3]);
          return new _A_($UHC.$Base.$_24,[$Util.$Router.$go,$__]);});
$Main.$__14__145=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$Util.$Router.$Monad__DCT4__0__0,$Util.$Router.$num,$Main.$_24okUNQ41]);}),[]);
$Main.$__14__144=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["read"]);}),[]);
$Main.$__14__143=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Router.$atom,[$Main.$__14__144]);}),[]);
$Main.$anotherRoute=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e,[$Util.$Router.$Monad__DCT4__0__0,$Main.$__14__143,$Main.$__14__145]);}),[]);
$Main.$__14__176=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["<br>"]);}),[]);
$Main.$__14__175=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["Home"]);}),[]);
$Main.$__14__173=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2b_2b,[$Main.$__14__175,$Main.$__14__176]);}),[]);
$Main.$__14__171=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Main.$out,$Main.$__14__173]);}),[]);
$Main.$homeRoute=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Util.$Router.$go,$Main.$__14__171]);}),[]);
$Main.$_24okUNQ71=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["div"]);
          var $__3=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__4=
           new _A_($Main.$_24okUNQ79,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Main.$_24okUNQ79=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Main.$__14__200NEW80,[$_24x]);
          var $__4=
           new _A_($Util.$DOM.$_7e_3e,[$_24x2,$_24x]);
          var $__5=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["<ul><li><a href=\"#post/show/1\">#post/show/1</a></li><li><a href=\"#post/new\">#post/new</a></li><li><a href=\"#post/user/bob/please\">#post/user/bob/please</a></li><li><a href=\"#read/1/2/3\">#read/1/2/3</a></li><li><a href=\"#/\">#/</a></li></ul>"]);
          var $__7=
           new _A_($Util.$DOM.$setInnerHtml,[$_24x2,$__6]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__5]);});
$Main.$__14__200NEW80=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["div"]);
          var $__3=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__4=
           new _A_($Main.$_24okUNQ86,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Main.$_24okUNQ86=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$Main.$homeRoute,$UHC.$Base.$_5b_5d]);
          var $__4=
           new _A_($UHC.$Base.$_3a,[$Main.$anotherRoute,$__]);
          var $__5=
           new _A_($UHC.$Base.$_3a,[$Main.$postsByUserRoute,$__4]);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$Main.$newPostRoute,$__5]);
          var $__7=
           new _A_($UHC.$Base.$_3a,[$Main.$showPostRoute,$__6]);
          var $__8=
           new _A_($Util.$Router.$setMap,[$__7]);
          var $__9=
           new _A_($Util.$DOM.$_7e_3e,[$_24x2,$_24x]);
          var $__10=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$packedStringToString,["myStdOut"]);
          var $__12=
           new _A_($UHC.$Base.$packedStringToString,["id"]);
          var $__13=
           new _A_($Util.$DOM.$setAttribute,[$_24x2,$__12,$__11]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__13,$__10]);});
$Main.$__14__183=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$DOM.$documentBody,$Main.$_24okUNQ71]);}),[]);
$Main.$__14__181=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Util.$Router.$wrap,$Main.$__14__183]);}),[]);
$Main.$main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Main.$__14__181,$Util.$Router.$onReady]);}),[]);
var $main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Run.$ehcRunMain,[$Main.$main]);}),[]);
_e_(new _A_($main,[[]]));
