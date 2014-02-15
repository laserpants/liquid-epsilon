// Data.Either
var $Data=
 ($Data ? $Data : {});
$Data.$Either=
 ($Data.$Either ? $Data.$Either : {});
$Data.$Either.$_24okUNQ5=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW0__0;
          switch($__._tag_)
           {case 0:
             $__swJSW0__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
             $__swJSW0__0=
              $__5;
             break;}
          return $__swJSW0__0;});
$Data.$Either.$rights=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$concatMap,[$Data.$Either.$_24okUNQ5,$x]);});
$Data.$Either.$_24okUNQ13=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW1__0;
          switch($__._tag_)
           {case 0:
             var $__4=
              new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
             $__swJSW1__0=
              $__4;
             break;
            case 1:
             $__swJSW1__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW1__0;});
$Data.$Either.$lefts=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$concatMap,[$Data.$Either.$_24okUNQ13,$x]);});
$Data.$Either.$leftUNQ23=
 new _F_(function($a,$__)
         {var $__3=
           _e_($__);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$a,$__3[0]]);
          var $__7=
           [$__6,$__3[1]];
          return $__7;});
$Data.$Either.$rightUNQ24=
 new _F_(function($a,$__)
         {var $__3=
           _e_($__);
          var $__6=
           new _A_($UHC.$Base.$_3a,[$a,$__3[1]]);
          var $__7=
           [$__3[0],$__6];
          return $__7;});
$Data.$Either.$partitionEithers=
 new _A_(new _F_(function()
                 {var $__=
                   [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];
                  var $__2=
                   new _A_($UHC.$Base.$either,[$Data.$Either.$leftUNQ23,$Data.$Either.$rightUNQ24]);
                  return new _A_($UHC.$Base.$foldr,[$__2,$__]);}),[]);
