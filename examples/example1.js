// Main
var $Main=
 ($Main ? $Main : {});
$Main.$____offsetTop=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.offsetTop;});
$Main.$menuLink=
 new _F_(function($title,$item)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["#"]);
          var $__4=
           new _A_($UHC.$Base.$_2b_2b,[$__,$item]);
          var $__5=
           new _A_($Util.$HTML.$Attributes.$href,[$__4]);
          var $__6=
           new _A_($Util.$HTML.$_21,[$Util.$HTML.$Attributable__DCT8__8__0,$Util.$HTML.$Elements.$a,$__5]);
          return new _A_($Util.$HTML.$_24_3c,[$__6,$title]);});
$Main.$_24okUNQ43=
 new _F_(function($es,$_24x)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($UHC.$Base.$map,[$UHC.$Base.$snd,$es]);
          var $__5=
           new _A_($UHC.$Base.$_24,[$UHC.$Base.$reverse,$__4]);
          var $__6=
           new _A_($UHC.$Base.$flip,[$Util.$DOM.$_7e_3e,$_24x]);
          var $__7=
           new _A_($UHC.$Base.$map,[$__6]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$__7,$__5]);
          var $__9=
           new _A_($UHC.$Base.$sequence,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__10=
           new _A_($UHC.$Base.$_24,[$__9,$__8]);
          var $__11=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__]);
          var $__12=
           new _A_($UHC.$Base.$packedStringToString,["top-menu"]);
          var $__13=
           new _A_($UHC.$Base.$packedStringToString,["id"]);
          var $__14=
           new _A_($Util.$DOM.$setAttribute,[$_24x,$__13,$__12]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__14,$__11]);});
$Main.$menuHtml=
 new _F_(function($es)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["ul"]);
          var $__3=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__4=
           new _A_($Main.$_24okUNQ43,[$es]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Main.$__13__62=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["<p>Smorgosbord!</p>"]);}),[]);
$Main.$__13__61=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$repeat,[$Main.$__13__62]);}),[]);
$Main.$__13__60=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$take,[7]);}),[]);
$Main.$__13__58=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Main.$__13__60,$Main.$__13__61]);}),[]);
$Main.$blah=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$UHC.$Base.$concat,$Main.$__13__58]);}),[]);
$Main.$css=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["body { font-family: Helvetica, Arial; } #top-menu { list-style: none; position: fixed; z-index: 1; background: white; left: 250px; right: 0; top: 0; } #top-menu li { float: left; } #top-menu a { display: block; padding: 5px 25px 7px 25px; -webkit-transition: 1s all ease; -moz-transition: 1s all ease; transition: 1s all ease; border-top: 3px solid white; color: #666; text-decoration: none; } #top-menu a:hover { color: #000; } #top-menu li.active a { border-top: 3px solid #333; color: #333; font-weight: bold; }"]);}),[]);
$Main.$_24okUNQ97=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["div"]);
          var $__3=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__4=
           new _A_($Main.$_24okUNQ106,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Main.$_24okUNQ106=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Main.$__13__112NEW32,[$_24x]);
          var $__4=
           new _A_($Util.$DOM.$_7e_3e,[$_24x2,$_24x]);
          var $__5=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);
          var $__6=
           new _A_($UHC.$Base.$repeat,[$Main.$blah]);
          var $__7=
           new _A_($UHC.$Base.$take,[5]);
          var $__8=
           new _A_($UHC.$Base.$_24,[$__7,$__6]);
          var $__9=
           new _A_($UHC.$Base.$_24,[$UHC.$Base.$concat,$__8]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToString,["<h1 id=\"item-5\">Item 5</h1>"]);
          var $__11=
           new _A_($UHC.$Base.$_2b_2b,[$__10,$__9]);
          var $__12=
           new _A_($UHC.$Base.$_2b_2b,[$Main.$blah,$__11]);
          var $__13=
           new _A_($UHC.$Base.$packedStringToString,["<h1 id=\"item-4\">Item 4</h1>"]);
          var $__14=
           new _A_($UHC.$Base.$_2b_2b,[$__13,$__12]);
          var $__15=
           new _A_($UHC.$Base.$_2b_2b,[$Main.$blah,$__14]);
          var $__16=
           new _A_($UHC.$Base.$packedStringToString,["<h1 id=\"item-3\">Item 3</h1>"]);
          var $__17=
           new _A_($UHC.$Base.$_2b_2b,[$__16,$__15]);
          var $__18=
           new _A_($UHC.$Base.$_2b_2b,[$Main.$blah,$__17]);
          var $__19=
           new _A_($UHC.$Base.$packedStringToString,["<h1 id=\"item-2\">Item 2</h1>"]);
          var $__20=
           new _A_($UHC.$Base.$_2b_2b,[$__19,$__18]);
          var $__21=
           new _A_($UHC.$Base.$_2b_2b,[$Main.$blah,$__20]);
          var $__22=
           new _A_($UHC.$Base.$packedStringToString,["<h1 id=\"item-1\">Item 1</h1>"]);
          var $__23=
           new _A_($UHC.$Base.$_2b_2b,[$__22,$__21]);
          var $__24=
           new _A_($Util.$DOM.$setInnerHtml,[$_24x2]);
          var $__25=
           new _A_($UHC.$Base.$_24,[$__24,$__23]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__25,$__5]);});
$Main.$__13__112NEW32=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["style"]);
          var $__3=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__4=
           new _A_($Main.$_24okUNQ114,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Main.$_24okUNQ114=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$_24x]);
          var $__4=
           new _A_($Util.$DOM.$_7e_3e,[$_24x2,$_24x]);
          var $__5=
           new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__]);
          var $__6=
           new _A_($Util.$DOM.$setInnerHtml,[$_24x2,$Main.$css]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__5]);});
$Main.$setup=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$DOM.$documentBody,$Main.$_24okUNQ97]);}),[]);
$Main.$fUNQ48=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          var $__5=
           new _A_($UHC.$Base.$packedStringToString,["active"]);
          var $__6=
           new _A_($Util.$DOM.$getElementsByClassName,[$__5]);
          var $__7=
           new _A_($Main.$_24okUNQ62,[$__2[1]]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__7]);});
$Main.$_24okUNQ62=
 new _F_(function($e,$_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["active"]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["class"]);
          var $__5=
           new _A_($Util.$DOM.$setAttribute,[$e,$__4,$__]);
          var $__6=
           new _A_($UHC.$Base.$flip,[$UHC.$Base.$map,$_24x]);
          var $__7=
           new _A_($UHC.$Base.$_24,[$__6,$Main.$__13__159__0]);
          var $__8=
           new _A_($UHC.$Base.$sequence,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__9=
           new _A_($UHC.$Base.$_24,[$__8,$__7]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__9,$__5]);});
$Main.$__13__159__0=
 new _F_(function($x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[""]);
          var $__3=
           new _A_($UHC.$Base.$packedStringToString,["class"]);
          return new _A_($Util.$DOM.$setAttribute,[$x,$__3,$__]);});
$Main.$setClassAttribute=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($UHC.$Base.$fmap,[$UHC.$Base.$Functor__DCT74__404__0,$Main.$fUNQ48]);
                  var $__2=
                   new _A_($UHC.$Base.$_2e,[$Data.$Maybe.$maybeToList,$__]);
                  var $__3=
                   new _A_($UHC.$Base.$sequence__,[$UHC.$Base.$Monad__DCT74__339__0]);
                  return new _A_($UHC.$Base.$_2e,[$__3,$__2]);}),[]);
$Main.$__13__192=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Main.$____offsetTop,$UHC.$Base.$fst]);}),[]);
$Main.$__13__191=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$negate,[$UHC.$Base.$Num__DCT74__101__0]);}),[]);
$Main.$__13__189=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Main.$__13__191,$Main.$__13__192]);}),[]);
$Main.$__13__188=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$compare,[$UHC.$Base.$Ord__DCT74__91__0]);}),[]);
$Main.$__13__187=
 new _A_(new _F_(function()
                 {return new _A_($Data.$Function.$on,[$Main.$__13__188]);}),[]);
$Main.$sortedElements=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Data.$List.$sortBy,$Main.$__13__187,$Main.$__13__189]);}),[]);
$Main.$__13__203__0=
 new _F_(function($y,$__)
         {var $__3=
           _e_($__);
          var $__6=
           new _A_($UHC.$Base.$_2b,[$UHC.$Base.$Num__DCT74__101__0,$y,5]);
          var $__7=
           new _A_($Main.$____offsetTop,[$__3[0]]);
          var $__8=
           new _A_($UHC.$Base.$_3c,[$UHC.$Base.$Ord__DCT74__91__0,$__7,$__6]);
          return $__8;});
$Main.$findFirstElement=
 new _F_(function($es,$y)
         {var $__=
           new _A_($UHC.$Base.$flip,[$Data.$List.$find,$es]);
          var $__4=
           new _A_($Main.$__13__203__0,[$y]);
          return new _A_($UHC.$Base.$_24,[$__,$__4]);});
$Main.$fUNQ81=
 new _F_(function($it,$e)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["li"]);
          var $__4=
           new _A_($Util.$DOM.$createElement,[$__]);
          var $__5=
           new _A_($Main.$_24okUNQ87,[$it,$e]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__4,$__5]);});
$Main.$_24okUNQ87=
 new _F_(function($it,$e,$_24x)
         {var $__=
           [$e,$_24x];
          var $__5=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$__]);
          var $__6=
           new _A_($UHC.$Base.$flip,[$Main.$menuLink,$it]);
          var $__7=
           new _A_($UHC.$Base.$_2e,[$Util.$HTML.$renderHtml,$__6]);
          var $__8=
           new _A_($Util.$DOM.$setInnerHtml,[$_24x]);
          var $__9=
           new _A_($UHC.$Base.$_2e,[$__8,$__7]);
          var $__10=
           new _A_($Util.$DOM.$innerHtml,[$e]);
          var $__11=
           new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__10,$__9]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__11,$__5]);});
$Main.$build=
 new _F_(function($it)
         {var $__=
           new _A_($Main.$fUNQ81,[$it]);
          var $__3=
           new _A_($UHC.$Base.$fmap,[$UHC.$Base.$Functor__DCT74__404__0,$__]);
          var $__4=
           new _A_($UHC.$Base.$_2e,[$Data.$Maybe.$maybeToList,$__3]);
          var $__5=
           new _A_($UHC.$Base.$sequence,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__6=
           new _A_($UHC.$Base.$_2e,[$__5,$__4]);
          var $__7=
           new _A_($Util.$DOM.$getElementById,[$it]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__6]);});
$Main.$buildElementList=
 new _F_(function($xs)
         {var $__=
           new _A_($UHC.$Base.$_2e,[$Main.$sortedElements,$UHC.$Base.$concat]);
          var $__3=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__4=
           new _A_($UHC.$Base.$_2e,[$__3,$__]);
          var $__5=
           new _A_($UHC.$Base.$map,[$Main.$build,$xs]);
          var $__6=
           new _A_($UHC.$Base.$sequence,[$UHC.$Base.$Monad__DCT74__339__0]);
          var $__7=
           new _A_($UHC.$Base.$_24,[$__6,$__5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__7,$__4]);});
$Main.$_24okUNQ120=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,["item-5"]);
          var $__3=
           new _A_($UHC.$Base.$_3a,[$__,$UHC.$Base.$_5b_5d]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["item-4"]);
          var $__5=
           new _A_($UHC.$Base.$_3a,[$__4,$__3]);
          var $__6=
           new _A_($UHC.$Base.$packedStringToString,["item-3"]);
          var $__7=
           new _A_($UHC.$Base.$_3a,[$__6,$__5]);
          var $__8=
           new _A_($UHC.$Base.$packedStringToString,["item-2"]);
          var $__9=
           new _A_($UHC.$Base.$_3a,[$__8,$__7]);
          var $__10=
           new _A_($UHC.$Base.$packedStringToString,["item-1"]);
          var $__11=
           new _A_($UHC.$Base.$_3a,[$__10,$__9]);
          var $__12=
           new _A_($Main.$buildElementList,[$__11]);
          var $__13=
           new _A_($Main.$_24okUNQ128,[$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__12,$__13]);});
$Main.$_24okUNQ128=
 new _F_(function($_24x,$_24x2)
         {var $__=
           new _A_($Main.$__13__293NEW125,[$_24x2]);
          var $__4=
           new _A_($Util.$DOM.$appendChild,[$_24x]);
          var $__5=
           new _A_($Main.$menuHtml,[$_24x2]);
          var $__6=
           new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__5,$__4]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Main.$__13__293NEW125=
 new _F_(function($_24x)
         {var $__=
           new _A_($Main.$findFirstElement,[$_24x]);
          var $f=
           new _A_($UHC.$Base.$_2e,[$Main.$setClassAttribute,$__]);
          var $__4=
           new _A_($UHC.$Base.$fmap,[$Util.$FRP.$Functor__DCT18__0__0,$f,$Util.$FRP.$scrollYSignal]);
          return new _A_($UHC.$Base.$_24,[$Util.$FRP.$attach,$__4]);});
$Main.$__13__279=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Main.$setup,$Main.$_24okUNQ120]);}),[]);
$Main.$__13__277=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_24,[$Util.$Router.$wrap,$Main.$__13__279]);}),[]);
$Main.$main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Main.$__13__277,$Util.$Router.$onReady]);}),[]);
var $main=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Run.$ehcRunMain,[$Main.$main]);}),[]);
_e_(new _A_($main,[[]]));
