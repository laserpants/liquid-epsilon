// Data.List
var $Data=
 ($Data ? $Data : {});
$Data.$List=
 ($Data.$List ? $Data.$List : {});
$Data.$List.$elem__by=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW0__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($Data.$List.$elem__by,[$x1,$x2,$x34._2]);
             var $__8=
              new _A_($x1,[$x2,$x34._1]);
             var $__9=
              new _A_($UHC.$Base.$_7c_7c,[$__8,$__]);
             $__swJSW0__0=
              $__9;
             break;
            case 1:
             $__swJSW0__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW0__0;});
$Data.$List.$nubBy_27UNQ103=
 new _F_(function($eq,$x1,$x2)
         {var $x14=
           _e_($x1);
          var $__swJSW1__0;
          switch($x14._tag_)
           {case 0:
             var $__=
              new _A_($Data.$List.$elem__by,[$eq,$x14._1,$x2]);
             var $__8=
              _e_($__);
             var $__swJSW2__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW3__0;
                switch($__9._tag_)
                 {case 0:
                   $__swJSW3__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__10=
                    new _A_($UHC.$Base.$_3a,[$x14._1,$x2]);
                   var $__11=
                    new _A_($Data.$List.$nubBy_27UNQ103,[$eq,$x14._2,$__10]);
                   var $__12=
                    new _A_($UHC.$Base.$_3a,[$x14._1,$__11]);
                   $__swJSW3__0=
                    $__12;
                   break;}
                $__swJSW2__0=
                 $__swJSW3__0;
                break;
               case 1:
                var $__13=
                 new _A_($Data.$List.$nubBy_27UNQ103,[$eq,$x14._2,$x2]);
                $__swJSW2__0=
                 $__13;
                break;}
             $__swJSW1__0=
              $__swJSW2__0;
             break;
            case 1:
             $__swJSW1__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW1__0;});
$Data.$List.$nubBy=
 new _F_(function($eq,$l)
         {return new _A_($Data.$List.$nubBy_27UNQ103,[$eq,$l,$UHC.$Base.$_5b_5d]);});
$Data.$List.$__316__498__2__1NEW16UNQ127=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Data.$List.$__316__628__2__0NEW19UNQ124=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$Data.$List.$__316__770__2__0NEW22UNQ122=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$Data.$List.$__318__58__0=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $n=
           new _A_($Data.$List.$nNEW26UNQ173CCN,[$__,$__3,$x1,$x2]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3c_3d,[$__2,$x1,$__8]);
          var $__10=
           _e_($__9);
          var $__swJSW7__0;
          switch($__10._tag_)
           {case 0:
             $__swJSW7__0=
              $n;
             break;
            case 1:
             var $__11=
              [$UHC.$Base.$_5b_5d,$x2];
             $__swJSW7__0=
              $__11;
             break;}
          return $__swJSW7__0;});
$Data.$List.$nNEW26UNQ173CCN=
 new _F_(function($__,$__2,$x1,$x2)
         {var $x25=
           _e_($x2);
          var $__swJSW8__0;
          switch($x25._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__9=
              new _A_($UHC.$Base.$fromInteger,[$__2,$__8]);
             var $__10=
              new _A_($UHC.$Base.$_2d,[$__2,$x1,$__9]);
             var $__11=
              new _A_($Data.$List.$genericSplitAt,[$__,$__10,$x25._2]);
             var $xs_27_27=
              new _A_($Data.$List.$xs_27_27NEW36UNQ182,[$__11]);
             var $xs_27=
              new _A_($Data.$List.$xs_27NEW39UNQ181,[$__11]);
             var $__14=
              new _A_($UHC.$Base.$_3a,[$x25._1,$xs_27]);
             $__swJSW8__0=
              [$__14,$xs_27_27];
             break;
            case 1:
             var $__15=
              [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];
             $__swJSW8__0=
              $__15;
             break;}
          return $__swJSW8__0;});
$Data.$List.$xs_27_27NEW36UNQ182=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$xs_27NEW39UNQ181=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$genericSplitAt=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$List.$__316__498__2__1NEW16UNQ127,[$__]);
          var $__3=
           new _A_($Data.$List.$__316__628__2__0NEW19UNQ124,[$__2]);
          var $__4=
           new _A_($Data.$List.$__316__770__2__0NEW22UNQ122,[$__2]);
          return new _A_($Data.$List.$__318__58__0,[$__,$__3,$__4]);});
$Data.$List.$genericLength=
 new _F_(function($__,$x1)
         {var $__3=
           _e_($x1);
          var $__swJSW11__0;
          switch($__3._tag_)
           {case 0:
             var $__6=
              new _A_($Data.$List.$genericLength,[$__,$__3._2]);
             var $__7=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__8=
              new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
             var $__9=
              new _A_($UHC.$Base.$_2b,[$__,$__8,$__6]);
             $__swJSW11__0=
              $__9;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__11=
              new _A_($UHC.$Base.$fromInteger,[$__,$__10]);
             $__swJSW11__0=
              $__11;
             break;}
          return $__swJSW11__0;});
$Data.$List.$__316__934__2__2NEW58UNQ219=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Data.$List.$__316__934__2__1NEW61UNQ218=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$Data.$List.$__316__1093__2__0NEW64UNQ216=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$Data.$List.$__316__1040__1__0NEW67UNQ211=
 new _F_(function($__)
         {var $Eq__=
           _e_($__);
          return $Eq__._4;});
$Data.$List.$__318__131__0=
 new _F_(function($__,$__2,$__3,$__4,$x1,$x2)
         {var $__7=
           new _A_($UHC.$Base.$packedStringToString,["List.genericIndex: index too large."]);
          var $__8=
           new _A_($UHC.$Base.$error,[$__7]);
          var $x19=
           _e_($x1);
          var $__swJSW16__0;
          switch($x19._tag_)
           {case 0:
             var $x12=
              new _A_($Data.$List.$xNEW74UNQ267CCN,[$__2,$__3,$__4,$x2,$__8,$x19._2]);
             var $__13=
              new _A_($UHC.$Base.$packedStringToInteger,["0"]);
             var $__14=
              new _A_($UHC.$Base.$fromInteger,[$__4,$__13]);
             var $x215=
              _e_(new _A_($UHC.$Base.$_3d_3d,[$__,$__14,$x2]));
             var $__swJSW17__0;
             switch($x215._tag_)
              {case 0:
                $__swJSW17__0=
                 $x12;
                break;
               case 1:
                $__swJSW17__0=
                 $x19._1;
                break;}
             $__swJSW16__0=
              $__swJSW17__0;
             break;
            case 1:
             $__swJSW16__0=
              $__8;
             break;}
          return $__swJSW16__0;});
$Data.$List.$xNEW74UNQ267CCN=
 new _F_(function($__,$__2,$__3,$x2,$__5,$__6)
         {var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__3,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3e,[$__,$x2,$__8]);
          var $__10=
           _e_($__9);
          var $__swJSW18__0;
          switch($__10._tag_)
           {case 0:
             var $__11=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW19__0;
             switch($__11._tag_)
              {case 0:
                $__swJSW19__0=
                 $__5;
                break;
               case 1:
                var $__12=
                 new _A_($UHC.$Base.$packedStringToString,["List.genericIndex: negative argument."]);
                var $__13=
                 new _A_($UHC.$Base.$error,[$__12]);
                $__swJSW19__0=
                 $__13;
                break;}
             $__swJSW18__0=
              $__swJSW19__0;
             break;
            case 1:
             var $__14=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__15=
              new _A_($UHC.$Base.$fromInteger,[$__3,$__14]);
             var $__16=
              new _A_($UHC.$Base.$_2d,[$__3,$x2,$__15]);
             var $__17=
              new _A_($Data.$List.$genericIndex,[$__2,$__6,$__16]);
             $__swJSW18__0=
              $__17;
             break;}
          return $__swJSW18__0;});
$Data.$List.$genericIndex=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$List.$__316__934__2__2NEW58UNQ219,[$__]);
          var $__3=
           new _A_($Data.$List.$__316__934__2__1NEW61UNQ218,[$__2]);
          var $__4=
           new _A_($Data.$List.$__316__1093__2__0NEW64UNQ216,[$__2]);
          var $__5=
           new _A_($Data.$List.$__316__1040__1__0NEW67UNQ211,[$__3]);
          return new _A_($Data.$List.$__318__131__0,[$__5,$__4,$__,$__3]);});
$Data.$List.$__316__1202__2__1NEW96UNQ284=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Data.$List.$__316__1319__2__0NEW99UNQ280=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$Data.$List.$__316__1329__2__0NEW102UNQ279=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$Data.$List.$__318__183__0=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $n=
           new _A_($Data.$List.$nNEW106UNQ319CCN,[$__,$__2,$x1,$x2]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3c_3d,[$__3,$x1,$__8]);
          var $__10=
           _e_($__9);
          var $__swJSW23__0;
          switch($__10._tag_)
           {case 0:
             $__swJSW23__0=
              $n;
             break;
            case 1:
             $__swJSW23__0=
              $x2;
             break;}
          return $__swJSW23__0;});
$Data.$List.$nNEW106UNQ319CCN=
 new _F_(function($__,$__2,$x1,$x2)
         {var $x25=
           _e_($x2);
          var $__swJSW24__0;
          switch($x25._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__9=
              new _A_($UHC.$Base.$fromInteger,[$__,$__8]);
             var $__10=
              new _A_($UHC.$Base.$_2d,[$__,$x1,$__9]);
             var $__11=
              new _A_($Data.$List.$genericDrop,[$__2,$__10,$x25._2]);
             $__swJSW24__0=
              $__11;
             break;
            case 1:
             $__swJSW24__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW24__0;});
$Data.$List.$genericDrop=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$List.$__316__1202__2__1NEW96UNQ284,[$__]);
          var $__3=
           new _A_($Data.$List.$__316__1319__2__0NEW99UNQ280,[$__2]);
          var $__4=
           new _A_($Data.$List.$__316__1329__2__0NEW102UNQ279,[$__2]);
          return new _A_($Data.$List.$__318__183__0,[$__4,$__,$__3]);});
$Data.$List.$wrap=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$_3a,[$x,$UHC.$Base.$_5b_5d]);});
$Data.$List.$permsUNQ331=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW25__0;
          switch($x13._tag_)
           {case 0:
             var $__=
              new _A_($Data.$List.$permutations,[$x2]);
             var $__7=
              new _A_($UHC.$Base.$_3a,[$x13._1,$x2]);
             var $__8=
              new _A_($Data.$List.$permsUNQ331,[$x13._2,$__7]);
             var $__9=
              new _A_($Data.$List.$interleaveUNQ370,[$x13._1,$x13._2]);
             $__swJSW25__0=
              new _A_($UHC.$Base.$foldr,[$__9,$__8,$__]);
             break;
            case 1:
             $__swJSW25__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW25__0;});
$Data.$List.$interleave_27UNQ369=
 new _F_(function($t,$ts,$x1,$x2,$x3)
         {var $x26=
           _e_($x2);
          var $__swJSW26__0;
          switch($x26._tag_)
           {case 0:
             var $__=
              new _A_($UHC.$Base.$_3a,[$x26._1]);
             var $__10=
              new _A_($UHC.$Base.$_2e,[$x1,$__]);
             var $__11=
              new _A_($Data.$List.$interleave_27UNQ369,[$t,$ts,$__10,$x26._2,$x3]);
             var $us=
              new _A_($Data.$List.$usNEW129UNQ387,[$__11]);
             var $zs=
              new _A_($Data.$List.$zsNEW132UNQ388,[$__11]);
             var $__14=
              new _A_($UHC.$Base.$_3a,[$x26._1,$us]);
             var $__15=
              new _A_($UHC.$Base.$_3a,[$t,$__14]);
             var $__16=
              new _A_($x1,[$__15]);
             var $__17=
              new _A_($UHC.$Base.$_3a,[$__16,$zs]);
             var $__18=
              new _A_($UHC.$Base.$_3a,[$x26._1,$us]);
             $__swJSW26__0=
              [$__18,$__17];
             break;
            case 1:
             var $__=
              [$ts,$x3];
             $__swJSW26__0=
              $__;
             break;}
          return $__swJSW26__0;});
$Data.$List.$usNEW129UNQ387=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$zsNEW132UNQ388=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$interleaveUNQ370=
 new _F_(function($t,$ts,$xs,$r)
         {var $__=
           new _A_($Data.$List.$interleave_27UNQ369,[$t,$ts,$UHC.$Base.$id,$xs,$r]);
          var $zs=
           new _A_($Data.$List.$zsNEW143UNQ400,[$__]);
          return $zs;});
$Data.$List.$zsNEW143UNQ400=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$permutations=
 new _F_(function($xs0)
         {var $__=
           new _A_($Data.$List.$permsUNQ331,[$xs0,$UHC.$Base.$_5b_5d]);
          return new _A_($UHC.$Base.$_3a,[$xs0,$__]);});
$Data.$List.$__316__2136__0NEW151UNQ422CCN=
 new _F_(function($__,$x1,$x2)
         {var $__4=
           new _A_($Data.$List.$__316__2152__0NEW155UNQ423CCN,[$__,$x1,$x2]);
          var $x25=
           _e_($x2);
          var $__swJSW30__0;
          switch($x25._tag_)
           {case 0:
             $__swJSW30__0=
              $__4;
             break;
            case 1:
             $__swJSW30__0=
              $UHC.$Base.$False__;
             break;}
          return $__swJSW30__0;});
$Data.$List.$__316__2152__0NEW155UNQ423CCN=
 new _F_(function($__,$x1,$x2)
         {var $x14=
           _e_($x1);
          var $__swJSW31__0;
          switch($x14._tag_)
           {case 0:
             var $x27=
              _e_($x2);
             var $__swJSW32__0;
             switch($x27._tag_)
              {case 0:
                var $__10=
                 new _A_($Data.$List.$isPrefixOf,[$__,$x14._2,$x27._2]);
                var $__11=
                 new _A_($UHC.$Base.$_3d_3d,[$__,$x14._1,$x27._1]);
                var $__12=
                 new _A_($UHC.$Base.$_26_26,[$__11,$__10]);
                $__swJSW32__0=
                 $__12;
                break;
               case 1:
                $__swJSW32__0=
                 $UHC.$Base.$undefined;
                break;}
             $__swJSW31__0=
              $__swJSW32__0;
             break;
            case 1:
             $__swJSW31__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW31__0;});
$Data.$List.$isPrefixOf=
 new _F_(function($__,$x1,$x2)
         {var $__4=
           new _A_($Data.$List.$__316__2136__0NEW151UNQ422CCN,[$__,$x1,$x2]);
          var $x15=
           _e_($x1);
          var $__swJSW33__0;
          switch($x15._tag_)
           {case 0:
             $__swJSW33__0=
              $__4;
             break;
            case 1:
             $__swJSW33__0=
              $UHC.$Base.$True__;
             break;}
          return $__swJSW33__0;});
$Data.$List.$unfoldr=
 new _F_(function($f,$b)
         {var $__=
           new _A_($f,[$b]);
          var $__4=
           _e_($__);
          var $__swJSW34__0;
          switch($__4._tag_)
           {case 0:
             var $__6=
              _e_($__4._1);
             var $__9=
              new _A_($Data.$List.$unfoldr,[$f,$__6[1]]);
             var $__10=
              new _A_($UHC.$Base.$_3a,[$__6[0],$__9]);
             $__swJSW34__0=
              $__10;
             break;
            case 1:
             $__swJSW34__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW34__0;});
$Data.$List.$yNEW175UNQ509=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$s_27NEW178UNQ508=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$ysNEW182UNQ512=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$s_27_27NEW185UNQ511=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$mapAccumL=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW40__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x2,$x34._1]);
             var $y=
              new _A_($Data.$List.$yNEW175UNQ509,[$__]);
             var $s_27=
              new _A_($Data.$List.$s_27NEW178UNQ508,[$__]);
             var $__10=
              new _A_($Data.$List.$mapAccumL,[$x1,$s_27,$x34._2]);
             var $ys=
              new _A_($Data.$List.$ysNEW182UNQ512,[$__10]);
             var $s_27_27=
              new _A_($Data.$List.$s_27_27NEW185UNQ511,[$__10]);
             var $__13=
              new _A_($UHC.$Base.$_3a,[$y,$ys]);
             $__swJSW40__0=
              [$s_27_27,$__13];
             break;
            case 1:
             var $__=
              [$x2,$UHC.$Base.$_5b_5d];
             $__swJSW40__0=
              $__;
             break;}
          return $__swJSW40__0;});
$Data.$List.$s_27NEW193UNQ564=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$yNEW197UNQ568=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$ysNEW200UNQ565=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$s_27_27NEW203UNQ567=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$mapAccumR=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW45__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($Data.$List.$mapAccumR,[$x1,$x2,$x34._2]);
             var $s_27=
              new _A_($Data.$List.$s_27NEW193UNQ564,[$__]);
             var $__9=
              new _A_($x1,[$s_27,$x34._1]);
             var $y=
              new _A_($Data.$List.$yNEW197UNQ568,[$__9]);
             var $ys=
              new _A_($Data.$List.$ysNEW200UNQ565,[$__]);
             var $s_27_27=
              new _A_($Data.$List.$s_27_27NEW203UNQ567,[$__9]);
             var $__13=
              new _A_($UHC.$Base.$_3a,[$y,$ys]);
             $__swJSW45__0=
              [$s_27_27,$__13];
             break;
            case 1:
             var $__=
              [$x2,$UHC.$Base.$_5b_5d];
             $__swJSW45__0=
              $__;
             break;}
          return $__swJSW45__0;});
$Data.$List.$insertBy=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW46__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x2,$x34._1]);
             var $__8=
              new _A_($UHC.$Base.$_3a,[$x2,$x34]);
             var $__9=
              _e_($__);
             var $__swJSW47__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW47__0=
                 $__8;
                break;
               case 1:
                var $__10=
                 new _A_($Data.$List.$insertBy,[$x1,$x2,$x34._2]);
                var $__11=
                 new _A_($UHC.$Base.$_3a,[$x34._1,$__10]);
                $__swJSW47__0=
                 $__11;
                break;
               case 2:
                $__swJSW47__0=
                 $__8;
                break;}
             $__swJSW46__0=
              $__swJSW47__0;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$_3a,[$x2,$UHC.$Base.$_5b_5d]);
             $__swJSW46__0=
              $__;
             break;}
          return $__swJSW46__0;});
$Data.$List.$insert=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$compare,[$__]);
          return new _A_($Data.$List.$insertBy,[$__2]);});
$Data.$List.$inits=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW48__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($Data.$List.$inits,[$__._2]);
             var $__6=
              new _A_($UHC.$Base.$_3a,[$__._1]);
             var $__7=
              new _A_($UHC.$Base.$map,[$__6,$__5]);
             var $__8=
              new _A_($UHC.$Base.$_3a,[$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d]);
             var $__9=
              new _A_($UHC.$Base.$_2b_2b,[$__8,$__7]);
             $__swJSW48__0=
              $__9;
             break;
            case 1:
             var $__10=
              new _A_($UHC.$Base.$_3a,[$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d]);
             $__swJSW48__0=
              $__10;
             break;}
          return $__swJSW48__0;});
$Data.$List.$isSuffixOf=
 new _F_(function($__,$x,$y)
         {var $__4=
           new _A_($UHC.$Base.$reverse,[$y]);
          var $__5=
           new _A_($UHC.$Base.$reverse,[$x]);
          return new _A_($Data.$List.$isPrefixOf,[$__,$__5,$__4]);});
$Data.$List.$ysNEW233UNQ647=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$zsNEW236UNQ648=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$groupBy=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW51__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x23._1]);
             var $__7=
              new _A_($UHC.$Base.$span,[$__,$x23._2]);
             var $ys=
              new _A_($Data.$List.$ysNEW233UNQ647,[$__7]);
             var $zs=
              new _A_($Data.$List.$zsNEW236UNQ648,[$__7]);
             var $__10=
              new _A_($Data.$List.$groupBy,[$x1,$zs]);
             var $__11=
              new _A_($UHC.$Base.$_3a,[$x23._1,$ys]);
             $__swJSW51__0=
              new _A_($UHC.$Base.$_3a,[$__11,$__10]);
             break;
            case 1:
             $__swJSW51__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW51__0;});
$Data.$List.$__316__4080__2__1NEW242UNQ664=
 new _F_(function($__)
         {var $Real__=
           _e_($__);
          return $Real__._2;});
$Data.$List.$__316__4197__2__0NEW245UNQ659=
 new _F_(function($__)
         {var $Ord__=
           _e_($__);
          return $Ord__._2;});
$Data.$List.$__316__4207__2__0NEW248UNQ660=
 new _F_(function($__)
         {var $Num__=
           _e_($__);
          return $Num__._1;});
$Data.$List.$__318__481__0=
 new _F_(function($__,$__2,$__3,$x1,$x2)
         {var $n=
           new _A_($Data.$List.$nNEW252UNQ699CCN,[$__,$__3,$x1,$x2]);
          var $__7=
           new _A_($UHC.$Base.$packedStringToInteger,["0"]);
          var $__8=
           new _A_($UHC.$Base.$fromInteger,[$__,$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3c_3d,[$__2,$x1,$__8]);
          var $__10=
           _e_($__9);
          var $__swJSW55__0;
          switch($__10._tag_)
           {case 0:
             $__swJSW55__0=
              $n;
             break;
            case 1:
             $__swJSW55__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW55__0;});
$Data.$List.$nNEW252UNQ699CCN=
 new _F_(function($__,$__2,$x1,$x2)
         {var $x25=
           _e_($x2);
          var $__swJSW56__0;
          switch($x25._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$packedStringToInteger,["1"]);
             var $__9=
              new _A_($UHC.$Base.$fromInteger,[$__,$__8]);
             var $__10=
              new _A_($UHC.$Base.$_2d,[$__,$x1,$__9]);
             var $__11=
              new _A_($Data.$List.$genericTake,[$__2,$__10,$x25._2]);
             var $__12=
              new _A_($UHC.$Base.$_3a,[$x25._1,$__11]);
             $__swJSW56__0=
              $__12;
             break;
            case 1:
             $__swJSW56__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW56__0;});
$Data.$List.$genericTake=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$List.$__316__4080__2__1NEW242UNQ664,[$__]);
          var $__3=
           new _A_($Data.$List.$__316__4197__2__0NEW245UNQ659,[$__2]);
          var $__4=
           new _A_($Data.$List.$__316__4207__2__0NEW248UNQ660,[$__2]);
          return new _A_($Data.$List.$__318__481__0,[$__4,$__3,$__]);});
$Data.$List.$genericReplicate=
 new _F_(function($__,$n,$x)
         {var $__4=
           new _A_($UHC.$Base.$repeat,[$x]);
          return new _A_($Data.$List.$genericTake,[$__,$n,$__4]);});
$Data.$List.$__316__4622__0NEW270UNQ737CCN=
 new _F_(function($x1,$x2,$x3)
         {var $xs=
           new _A_($Data.$List.$xsNEW274UNQ738CCN,[$x1,$x2,$x3]);
          var $x35=
           _e_($x3);
          var $__swJSW57__0;
          switch($x35._tag_)
           {case 0:
             $__swJSW57__0=
              $xs;
             break;
            case 1:
             $__swJSW57__0=
              $x2;
             break;}
          return $__swJSW57__0;});
$Data.$List.$xsNEW274UNQ738CCN=
 new _F_(function($x1,$x2,$x3)
         {var $x24=
           _e_($x2);
          var $__swJSW58__0;
          switch($x24._tag_)
           {case 0:
             var $x37=
              _e_($x3);
             var $__swJSW59__0;
             switch($x37._tag_)
              {case 0:
                var $__=
                 new _A_($x1,[$x24._1,$x37._1]);
                var $__11=
                 new _A_($UHC.$Base.$_3a,[$x37._1,$x37._2]);
                var $__12=
                 new _A_($Data.$List.$merge,[$x1,$x24._2,$__11]);
                var $__13=
                 new _A_($UHC.$Base.$_3a,[$x24._1,$__12]);
                var $__14=
                 _e_($__);
                var $__swJSW60__0;
                switch($__14._tag_)
                 {case 0:
                   $__swJSW60__0=
                    $__13;
                   break;
                  case 1:
                   var $__15=
                    new _A_($UHC.$Base.$_3a,[$x24._1,$x24._2]);
                   var $__16=
                    new _A_($Data.$List.$merge,[$x1,$__15,$x37._2]);
                   var $__17=
                    new _A_($UHC.$Base.$_3a,[$x37._1,$__16]);
                   $__swJSW60__0=
                    $__17;
                   break;
                  case 2:
                   $__swJSW60__0=
                    $__13;
                   break;}
                $__swJSW59__0=
                 $__swJSW60__0;
                break;
               case 1:
                $__swJSW59__0=
                 $UHC.$Base.$undefined;
                break;}
             $__swJSW58__0=
              $__swJSW59__0;
             break;
            case 1:
             $__swJSW58__0=
              $UHC.$Base.$undefined;
             break;}
          return $__swJSW58__0;});
$Data.$List.$merge=
 new _F_(function($x1,$x2,$x3)
         {var $__=
           new _A_($Data.$List.$__316__4622__0NEW270UNQ737CCN,[$x1,$x2,$x3]);
          var $x25=
           _e_($x2);
          var $__swJSW61__0;
          switch($x25._tag_)
           {case 0:
             $__swJSW61__0=
              $__;
             break;
            case 1:
             $__swJSW61__0=
              $x3;
             break;}
          return $__swJSW61__0;});
$Data.$List.$merge__pairs=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW62__0;
          switch($x23._tag_)
           {case 0:
             var $__6=
              _e_($x23._2);
             var $__swJSW63__0;
             switch($__6._tag_)
              {case 0:
                var $__9=
                 new _A_($Data.$List.$merge__pairs,[$x1,$__6._2]);
                var $__10=
                 new _A_($Data.$List.$merge,[$x1,$x23._1,$__6._1]);
                var $__11=
                 new _A_($UHC.$Base.$_3a,[$__10,$__9]);
                $__swJSW63__0=
                 $__11;
                break;
               case 1:
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$x23._1,$UHC.$Base.$_5b_5d]);
                $__swJSW63__0=
                 $__12;
                break;}
             $__swJSW62__0=
              $__swJSW63__0;
             break;
            case 1:
             $__swJSW62__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW62__0;});
$Data.$List.$intersperse=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW64__0;
          switch($x23._tag_)
           {case 0:
             var $__6=
              new _A_($Data.$List.$intersperse,[$x1,$x23._2]);
             var $__7=
              new _A_($UHC.$Base.$_3a,[$x1,$__6]);
             var $__8=
              new _A_($UHC.$Base.$_3a,[$x23._1,$__7]);
             var $__9=
              _e_($x23._2);
             var $__swJSW65__0;
             switch($__9._tag_)
              {case 0:
                $__swJSW65__0=
                 $__8;
                break;
               case 1:
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$x23._1,$UHC.$Base.$_5b_5d]);
                $__swJSW65__0=
                 $__12;
                break;}
             $__swJSW64__0=
              $__swJSW65__0;
             break;
            case 1:
             $__swJSW64__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW64__0;});
$Data.$List.$__316__5344__0NEW305UNQ822CCN=
 new _F_(function($x1,$x2)
         {var $__=
           new _A_($Data.$List.$minByUNQ825,[$x1]);
          return new _A_($UHC.$Base.$foldl1,[$__,$x2]);});
$Data.$List.$minByUNQ825=
 new _F_(function($x1,$x,$y)
         {var $__=
           new _A_($x1,[$x,$y]);
          var $__5=
           _e_($__);
          var $__swJSW66__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW66__0=
              $x;
             break;
            case 1:
             $__swJSW66__0=
              $y;
             break;
            case 2:
             $__swJSW66__0=
              $x;
             break;}
          return $__swJSW66__0;});
$Data.$List.$minimumBy=
 new _F_(function($x1,$x2)
         {var $__=
           new _A_($Data.$List.$__316__5344__0NEW305UNQ822CCN,[$x1,$x2]);
          var $x24=
           _e_($x2);
          var $__swJSW67__0;
          switch($x24._tag_)
           {case 0:
             $__swJSW67__0=
              $__;
             break;
            case 1:
             var $__7=
              new _A_($UHC.$Base.$packedStringToString,["List.minimumBy: empty list"]);
             var $__8=
              new _A_($UHC.$Base.$error,[$__7]);
             $__swJSW67__0=
              $__8;
             break;}
          return $__swJSW67__0;});
$Data.$List.$__316__5543__0NEW315UNQ844CCN=
 new _F_(function($x1,$x2)
         {var $__=
           new _A_($Data.$List.$maxByUNQ847,[$x1]);
          return new _A_($UHC.$Base.$foldl1,[$__,$x2]);});
$Data.$List.$maxByUNQ847=
 new _F_(function($x1,$x,$y)
         {var $__=
           new _A_($x1,[$x,$y]);
          var $__5=
           _e_($__);
          var $__swJSW68__0;
          switch($__5._tag_)
           {case 0:
             $__swJSW68__0=
              $y;
             break;
            case 1:
             $__swJSW68__0=
              $x;
             break;
            case 2:
             $__swJSW68__0=
              $y;
             break;}
          return $__swJSW68__0;});
$Data.$List.$maximumBy=
 new _F_(function($x1,$x2)
         {var $__=
           new _A_($Data.$List.$__316__5543__0NEW315UNQ844CCN,[$x1,$x2]);
          var $x24=
           _e_($x2);
          var $__swJSW69__0;
          switch($x24._tag_)
           {case 0:
             $__swJSW69__0=
              $__;
             break;
            case 1:
             var $__7=
              new _A_($UHC.$Base.$packedStringToString,["List.maximumBy: empty list"]);
             var $__8=
              new _A_($UHC.$Base.$error,[$__7]);
             $__swJSW69__0=
              $__8;
             break;}
          return $__swJSW69__0;});
$Data.$List.$_24okUNQ858=
 new _F_(function($eq,$ys,$_24x)
         {var $__=
           new _A_($eq,[$_24x]);
          var $__5=
           new _A_($UHC.$Base.$any,[$__,$ys]);
          var $__6=
           _e_($__5);
          var $__swJSW70__0;
          switch($__6._tag_)
           {case 0:
             $__swJSW70__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__7=
              new _A_($UHC.$Base.$_3a,[$_24x,$UHC.$Base.$_5b_5d]);
             $__swJSW70__0=
              $__7;
             break;}
          return $__swJSW70__0;});
$Data.$List.$intersectBy=
 new _F_(function($eq,$xs,$ys)
         {var $__=
           new _A_($Data.$List.$_24okUNQ858,[$eq,$ys]);
          return new _A_($UHC.$Base.$concatMap,[$__,$xs]);});
$Data.$List.$foldl_27=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW71__0;
          switch($x34._tag_)
           {case 0:
             var $a_27=
              new _A_($x1,[$x2,$x34._1]);
             var $__=
              new _A_($Data.$List.$foldl_27,[$x1,$a_27,$x34._2]);
             $__swJSW71__0=
              new _A_($UHC.$Base.$seq,[$a_27,$__]);
             break;
            case 1:
             $__swJSW71__0=
              $x2;
             break;}
          return $__swJSW71__0;});
$Data.$List.$group=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$_3d_3d,[$__]);
          return new _A_($Data.$List.$groupBy,[$__2]);});
$Data.$List.$stripPrefix=
 new _F_(function($__,$x1,$x2)
         {var $x14=
           _e_($x1);
          var $__swJSW72__0;
          switch($x14._tag_)
           {case 0:
             var $x27=
              _e_($x2);
             var $__swJSW73__0;
             switch($x27._tag_)
              {case 0:
                var $__10=
                 new _A_($UHC.$Base.$_3d_3d,[$__,$x14._1,$x27._1]);
                var $__11=
                 _e_($__10);
                var $__swJSW74__0;
                switch($__11._tag_)
                 {case 0:
                   $__swJSW74__0=
                    $UHC.$Base.$Nothing__;
                   break;
                  case 1:
                   var $__12=
                    new _A_($Data.$List.$stripPrefix,[$__,$x14._2,$x27._2]);
                   $__swJSW74__0=
                    $__12;
                   break;}
                $__swJSW73__0=
                 $__swJSW74__0;
                break;
               case 1:
                $__swJSW73__0=
                 $UHC.$Base.$Nothing__;
                break;}
             $__swJSW72__0=
              $__swJSW73__0;
             break;
            case 1:
             var $__13=
              new _A_($UHC.$Base.$Just__,[$x2]);
             $__swJSW72__0=
              $__13;
             break;}
          return $__swJSW72__0;});
$Data.$List.$zipWith6=
 new _F_(function($x1,$x2,$x3,$x4,$x5,$x6,$x7)
         {var $x28=
           _e_($x2);
          var $__swJSW75__0;
          switch($x28._tag_)
           {case 0:
             var $x311=
              _e_($x3);
             var $__swJSW76__0;
             switch($x311._tag_)
              {case 0:
                var $x414=
                 _e_($x4);
                var $__swJSW77__0;
                switch($x414._tag_)
                 {case 0:
                   var $x517=
                    _e_($x5);
                   var $__swJSW78__0;
                   switch($x517._tag_)
                    {case 0:
                      var $x620=
                       _e_($x6);
                      var $__swJSW79__0;
                      switch($x620._tag_)
                       {case 0:
                         var $x723=
                          _e_($x7);
                         var $__swJSW80__0;
                         switch($x723._tag_)
                          {case 0:
                            var $__=
                             new _A_($Data.$List.$zipWith6,[$x1,$x28._2,$x311._2,$x414._2,$x517._2,$x620._2,$x723._2]);
                            var $__27=
                             new _A_($x1,[$x28._1,$x311._1,$x414._1,$x517._1,$x620._1,$x723._1]);
                            var $__28=
                             new _A_($UHC.$Base.$_3a,[$__27,$__]);
                            $__swJSW80__0=
                             $__28;
                            break;
                           case 1:
                            $__swJSW80__0=
                             $UHC.$Base.$_5b_5d;
                            break;}
                         $__swJSW79__0=
                          $__swJSW80__0;
                         break;
                        case 1:
                         $__swJSW79__0=
                          $UHC.$Base.$_5b_5d;
                         break;}
                      $__swJSW78__0=
                       $__swJSW79__0;
                      break;
                     case 1:
                      $__swJSW78__0=
                       $UHC.$Base.$_5b_5d;
                      break;}
                   $__swJSW77__0=
                    $__swJSW78__0;
                   break;
                  case 1:
                   $__swJSW77__0=
                    $UHC.$Base.$_5b_5d;
                   break;}
                $__swJSW76__0=
                 $__swJSW77__0;
                break;
               case 1:
                $__swJSW76__0=
                 $UHC.$Base.$_5b_5d;
                break;}
             $__swJSW75__0=
              $__swJSW76__0;
             break;
            case 1:
             $__swJSW75__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW75__0;});
$Data.$List.$__318__743__0=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6)
         {return [$__,$__2,$__3,$__4,$__5,$__6];});
$Data.$List.$zip6=
 new _A_(new _F_(function()
                 {return new _A_($Data.$List.$zipWith6,[$Data.$List.$__318__743__0]);}),[]);
$Data.$List.$zipWith7=
 new _F_(function($x1,$x2,$x3,$x4,$x5,$x6,$x7,$x8)
         {var $x29=
           _e_($x2);
          var $__swJSW81__0;
          switch($x29._tag_)
           {case 0:
             var $x312=
              _e_($x3);
             var $__swJSW82__0;
             switch($x312._tag_)
              {case 0:
                var $x415=
                 _e_($x4);
                var $__swJSW83__0;
                switch($x415._tag_)
                 {case 0:
                   var $x518=
                    _e_($x5);
                   var $__swJSW84__0;
                   switch($x518._tag_)
                    {case 0:
                      var $x621=
                       _e_($x6);
                      var $__swJSW85__0;
                      switch($x621._tag_)
                       {case 0:
                         var $x724=
                          _e_($x7);
                         var $__swJSW86__0;
                         switch($x724._tag_)
                          {case 0:
                            var $x827=
                             _e_($x8);
                            var $__swJSW87__0;
                            switch($x827._tag_)
                             {case 0:
                               var $__=
                                new _A_($Data.$List.$zipWith7,[$x1,$x29._2,$x312._2,$x415._2,$x518._2,$x621._2,$x724._2,$x827._2]);
                               var $__31=
                                new _A_($x1,[$x29._1,$x312._1,$x415._1,$x518._1,$x621._1,$x724._1,$x827._1]);
                               var $__32=
                                new _A_($UHC.$Base.$_3a,[$__31,$__]);
                               $__swJSW87__0=
                                $__32;
                               break;
                              case 1:
                               $__swJSW87__0=
                                $UHC.$Base.$_5b_5d;
                               break;}
                            $__swJSW86__0=
                             $__swJSW87__0;
                            break;
                           case 1:
                            $__swJSW86__0=
                             $UHC.$Base.$_5b_5d;
                            break;}
                         $__swJSW85__0=
                          $__swJSW86__0;
                         break;
                        case 1:
                         $__swJSW85__0=
                          $UHC.$Base.$_5b_5d;
                         break;}
                      $__swJSW84__0=
                       $__swJSW85__0;
                      break;
                     case 1:
                      $__swJSW84__0=
                       $UHC.$Base.$_5b_5d;
                      break;}
                   $__swJSW83__0=
                    $__swJSW84__0;
                   break;
                  case 1:
                   $__swJSW83__0=
                    $UHC.$Base.$_5b_5d;
                   break;}
                $__swJSW82__0=
                 $__swJSW83__0;
                break;
               case 1:
                $__swJSW82__0=
                 $UHC.$Base.$_5b_5d;
                break;}
             $__swJSW81__0=
              $__swJSW82__0;
             break;
            case 1:
             $__swJSW81__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW81__0;});
$Data.$List.$__318__811__0=
 new _F_(function($__,$__2,$__3,$__4,$__5,$__6,$__7)
         {return [$__,$__2,$__3,$__4,$__5,$__6,$__7];});
$Data.$List.$zip7=
 new _A_(new _F_(function()
                 {return new _A_($Data.$List.$zipWith7,[$Data.$List.$__318__811__0]);}),[]);
$Data.$List.$zipWith4=
 new _F_(function($x1,$x2,$x3,$x4,$x5)
         {var $x26=
           _e_($x2);
          var $__swJSW88__0;
          switch($x26._tag_)
           {case 0:
             var $x39=
              _e_($x3);
             var $__swJSW89__0;
             switch($x39._tag_)
              {case 0:
                var $x412=
                 _e_($x4);
                var $__swJSW90__0;
                switch($x412._tag_)
                 {case 0:
                   var $x515=
                    _e_($x5);
                   var $__swJSW91__0;
                   switch($x515._tag_)
                    {case 0:
                      var $__=
                       new _A_($Data.$List.$zipWith4,[$x1,$x26._2,$x39._2,$x412._2,$x515._2]);
                      var $__19=
                       new _A_($x1,[$x26._1,$x39._1,$x412._1,$x515._1]);
                      var $__20=
                       new _A_($UHC.$Base.$_3a,[$__19,$__]);
                      $__swJSW91__0=
                       $__20;
                      break;
                     case 1:
                      $__swJSW91__0=
                       $UHC.$Base.$_5b_5d;
                      break;}
                   $__swJSW90__0=
                    $__swJSW91__0;
                   break;
                  case 1:
                   $__swJSW90__0=
                    $UHC.$Base.$_5b_5d;
                   break;}
                $__swJSW89__0=
                 $__swJSW90__0;
                break;
               case 1:
                $__swJSW89__0=
                 $UHC.$Base.$_5b_5d;
                break;}
             $__swJSW88__0=
              $__swJSW89__0;
             break;
            case 1:
             $__swJSW88__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW88__0;});
$Data.$List.$__318__864__0=
 new _F_(function($__,$__2,$__3,$__4)
         {return [$__,$__2,$__3,$__4];});
$Data.$List.$zip4=
 new _A_(new _F_(function()
                 {return new _A_($Data.$List.$zipWith4,[$Data.$List.$__318__864__0]);}),[]);
$Data.$List.$zipWith5=
 new _F_(function($x1,$x2,$x3,$x4,$x5,$x6)
         {var $x27=
           _e_($x2);
          var $__swJSW92__0;
          switch($x27._tag_)
           {case 0:
             var $x310=
              _e_($x3);
             var $__swJSW93__0;
             switch($x310._tag_)
              {case 0:
                var $x413=
                 _e_($x4);
                var $__swJSW94__0;
                switch($x413._tag_)
                 {case 0:
                   var $x516=
                    _e_($x5);
                   var $__swJSW95__0;
                   switch($x516._tag_)
                    {case 0:
                      var $x619=
                       _e_($x6);
                      var $__swJSW96__0;
                      switch($x619._tag_)
                       {case 0:
                         var $__=
                          new _A_($Data.$List.$zipWith5,[$x1,$x27._2,$x310._2,$x413._2,$x516._2,$x619._2]);
                         var $__23=
                          new _A_($x1,[$x27._1,$x310._1,$x413._1,$x516._1,$x619._1]);
                         var $__24=
                          new _A_($UHC.$Base.$_3a,[$__23,$__]);
                         $__swJSW96__0=
                          $__24;
                         break;
                        case 1:
                         $__swJSW96__0=
                          $UHC.$Base.$_5b_5d;
                         break;}
                      $__swJSW95__0=
                       $__swJSW96__0;
                      break;
                     case 1:
                      $__swJSW95__0=
                       $UHC.$Base.$_5b_5d;
                      break;}
                   $__swJSW94__0=
                    $__swJSW95__0;
                   break;
                  case 1:
                   $__swJSW94__0=
                    $UHC.$Base.$_5b_5d;
                   break;}
                $__swJSW93__0=
                 $__swJSW94__0;
                break;
               case 1:
                $__swJSW93__0=
                 $UHC.$Base.$_5b_5d;
                break;}
             $__swJSW92__0=
              $__swJSW93__0;
             break;
            case 1:
             $__swJSW92__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW92__0;});
$Data.$List.$__318__914__0=
 new _F_(function($__,$__2,$__3,$__4,$__5)
         {return [$__,$__2,$__3,$__4,$__5];});
$Data.$List.$zip5=
 new _A_(new _F_(function()
                 {return new _A_($Data.$List.$zipWith5,[$Data.$List.$__318__914__0]);}),[]);
$Data.$List.$fUNQ1275=
 new _F_(function($x,$ys,$r)
         {var $__=
           new _A_($UHC.$Base.$_3a,[$x,$ys]);
          var $__5=
           new _A_($UHC.$Base.$_3a,[$__,$r]);
          return new _A_($UHC.$Base.$_3a,[$ys,$__5]);});
$Data.$List.$nonEmptySubsequences=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW97__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($Data.$List.$nonEmptySubsequences,[$__._2]);
             var $__6=
              new _A_($Data.$List.$fUNQ1275,[$__._1]);
             var $__7=
              new _A_($UHC.$Base.$foldr,[$__6,$UHC.$Base.$_5b_5d,$__5]);
             var $__8=
              new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
             $__swJSW97__0=
              new _A_($UHC.$Base.$_3a,[$__8,$__7]);
             break;
            case 1:
             $__swJSW97__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW97__0;});
$Data.$List.$_24okUNQ444=
 new _F_(function($p,$_24x)
         {var $__=
           _e_($_24x);
          var $__6=
           new _A_($p,[$__[0]]);
          var $__7=
           _e_($__6);
          var $__swJSW99__0;
          switch($__7._tag_)
           {case 0:
             $__swJSW99__0=
              $UHC.$Base.$_5b_5d;
             break;
            case 1:
             var $__8=
              new _A_($UHC.$Base.$_3a,[$__[1],$UHC.$Base.$_5b_5d]);
             $__swJSW99__0=
              $__8;
             break;}
          return $__swJSW99__0;});
$Data.$List.$findIndices=
 new _F_(function($p,$xs)
         {var $__=
           new _A_($UHC.$Base.$enumFrom,[$UHC.$Base.$Enum__DCT74__118__0,0]);
          var $__4=
           new _A_($UHC.$Base.$zip,[$xs,$__]);
          var $__5=
           new _A_($Data.$List.$_24okUNQ444,[$p]);
          return new _A_($UHC.$Base.$concatMap,[$__5,$__4]);});
$Data.$List.$findIndex=
 new _F_(function($p)
         {var $__=
           new _A_($Data.$List.$findIndices,[$p]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Maybe.$listToMaybe,$__]);});
$Data.$List.$errorEmptyList=
 new _F_(function($fun)
         {var $__=
           new _A_($UHC.$Base.$packedStringToString,[": empty list"]);
          var $__3=
           new _A_($UHC.$Base.$_2b_2b,[$fun,$__]);
          var $__4=
           new _A_($UHC.$Base.$packedStringToString,["Prelude."]);
          var $__5=
           new _A_($UHC.$Base.$_2b_2b,[$__4,$__3]);
          return new _A_($UHC.$Base.$error,[$__5]);});
$Data.$List.$foldl1_27=
 new _F_(function($x1,$x2)
         {var $x23=
           _e_($x2);
          var $__swJSW100__0;
          switch($x23._tag_)
           {case 0:
             var $__=
              new _A_($Data.$List.$foldl_27,[$x1,$x23._1,$x23._2]);
             $__swJSW100__0=
              $__;
             break;
            case 1:
             var $__=
              new _A_($UHC.$Base.$packedStringToString,["foldl1'"]);
             var $__8=
              new _A_($Data.$List.$errorEmptyList,[$__]);
             $__swJSW100__0=
              $__8;
             break;}
          return $__swJSW100__0;});
$Data.$List.$elemIndex=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$Base.$_3d_3d,[$__,$x]);
          return new _A_($Data.$List.$findIndex,[$__3]);});
$Data.$List.$intercalate=
 new _F_(function($xs,$xss)
         {var $__=
           new _A_($Data.$List.$intersperse,[$xs,$xss]);
          return new _A_($UHC.$Base.$concat,[$__]);});
$Data.$List.$find=
 new _F_(function($p)
         {var $__=
           new _A_($UHC.$Base.$filter,[$p]);
          return new _A_($UHC.$Base.$_2e,[$Data.$Maybe.$listToMaybe,$__]);});
$Data.$List.$mergesort_27=
 new _F_(function($x1,$x2)
         {var $__=
           new _A_($Data.$List.$merge__pairs,[$x1,$x2]);
          var $__4=
           new _A_($Data.$List.$mergesort_27,[$x1,$__]);
          var $x25=
           _e_($x2);
          var $__swJSW101__0;
          switch($x25._tag_)
           {case 0:
             var $__8=
              _e_($x25._2);
             var $__swJSW102__0;
             switch($__8._tag_)
              {case 0:
                $__swJSW102__0=
                 $__4;
                break;
               case 1:
                $__swJSW102__0=
                 $x25._1;
                break;}
             $__swJSW101__0=
              $__swJSW102__0;
             break;
            case 1:
             $__swJSW101__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW101__0;});
$Data.$List.$mergesort=
 new _F_(function($cmp)
         {var $__=
           new _A_($UHC.$Base.$map,[$Data.$List.$wrap]);
          var $__3=
           new _A_($Data.$List.$mergesort_27,[$cmp]);
          return new _A_($UHC.$Base.$_2e,[$__3,$__]);});
$Data.$List.$sort=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$compare,[$__]);
          return new _A_($Data.$List.$mergesort,[$__2]);});
$Data.$List.$sortBy=
 new _A_(new _F_(function()
                 {return $Data.$List.$mergesort;}),[]);
$Data.$List.$__318__1103=
 new _A_(new _F_(function()
                 {return [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];}),[]);
$Data.$List.$__318__1059__0=
 new _F_(function($a,$b,$e,$f,$c,$d,$g,$__)
         {var $as=
           new _A_($Data.$List.$asNEW438UNQ1407,[$__]);
          var $bs=
           new _A_($Data.$List.$bsNEW441UNQ1416,[$__]);
          var $cs=
           new _A_($Data.$List.$csNEW444UNQ1425,[$__]);
          var $ds=
           new _A_($Data.$List.$dsNEW447UNQ1434,[$__]);
          var $es=
           new _A_($Data.$List.$esNEW450UNQ1443,[$__]);
          var $fs=
           new _A_($Data.$List.$fsNEW453UNQ1452,[$__]);
          var $gs=
           new _A_($Data.$List.$gsNEW456UNQ1461,[$__]);
          var $__16=
           new _A_($UHC.$Base.$_3a,[$g,$gs]);
          var $__17=
           new _A_($UHC.$Base.$_3a,[$f,$fs]);
          var $__18=
           new _A_($UHC.$Base.$_3a,[$e,$es]);
          var $__19=
           new _A_($UHC.$Base.$_3a,[$d,$ds]);
          var $__20=
           new _A_($UHC.$Base.$_3a,[$c,$cs]);
          var $__21=
           new _A_($UHC.$Base.$_3a,[$b,$bs]);
          var $__22=
           new _A_($UHC.$Base.$_3a,[$a,$as]);
          return [$__22,$__21,$__20,$__19,$__18,$__17,$__16];});
$Data.$List.$asNEW438UNQ1407=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$bsNEW441UNQ1416=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$csNEW444UNQ1425=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[2];});
$Data.$List.$dsNEW447UNQ1434=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[3];});
$Data.$List.$esNEW450UNQ1443=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[4];});
$Data.$List.$fsNEW453UNQ1452=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[5];});
$Data.$List.$gsNEW456UNQ1461=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[6];});
$Data.$List.$__318__1055__0=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Data.$List.$__318__1059__0,[$__2[0],$__2[1],$__2[4],$__2[5],$__2[2],$__2[3],$__2[6]]);});
$Data.$List.$unzip7=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldr,[$Data.$List.$__318__1055__0,$Data.$List.$__318__1103]);}),[]);
$Data.$List.$__318__1155=
 new _A_(new _F_(function()
                 {return [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];}),[]);
$Data.$List.$__318__1117__0=
 new _F_(function($c,$d,$a,$b,$e,$f,$__)
         {var $as=
           new _A_($Data.$List.$asNEW471UNQ1535,[$__]);
          var $bs=
           new _A_($Data.$List.$bsNEW474UNQ1543,[$__]);
          var $cs=
           new _A_($Data.$List.$csNEW477UNQ1551,[$__]);
          var $ds=
           new _A_($Data.$List.$dsNEW480UNQ1559,[$__]);
          var $es=
           new _A_($Data.$List.$esNEW483UNQ1567,[$__]);
          var $fs=
           new _A_($Data.$List.$fsNEW486UNQ1575,[$__]);
          var $__14=
           new _A_($UHC.$Base.$_3a,[$f,$fs]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[$e,$es]);
          var $__16=
           new _A_($UHC.$Base.$_3a,[$d,$ds]);
          var $__17=
           new _A_($UHC.$Base.$_3a,[$c,$cs]);
          var $__18=
           new _A_($UHC.$Base.$_3a,[$b,$bs]);
          var $__19=
           new _A_($UHC.$Base.$_3a,[$a,$as]);
          return [$__19,$__18,$__17,$__16,$__15,$__14];});
$Data.$List.$asNEW471UNQ1535=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$bsNEW474UNQ1543=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$csNEW477UNQ1551=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[2];});
$Data.$List.$dsNEW480UNQ1559=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[3];});
$Data.$List.$esNEW483UNQ1567=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[4];});
$Data.$List.$fsNEW486UNQ1575=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[5];});
$Data.$List.$__318__1113__0=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Data.$List.$__318__1117__0,[$__2[2],$__2[3],$__2[0],$__2[1],$__2[4],$__2[5]]);});
$Data.$List.$unzip6=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldr,[$Data.$List.$__318__1113__0,$Data.$List.$__318__1155]);}),[]);
$Data.$List.$__318__1200=
 new _A_(new _F_(function()
                 {return [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];}),[]);
$Data.$List.$__318__1168__0=
 new _F_(function($c,$d,$a,$b,$e,$__)
         {var $as=
           new _A_($Data.$List.$asNEW500UNQ1637,[$__]);
          var $bs=
           new _A_($Data.$List.$bsNEW503UNQ1644,[$__]);
          var $cs=
           new _A_($Data.$List.$csNEW506UNQ1651,[$__]);
          var $ds=
           new _A_($Data.$List.$dsNEW509UNQ1658,[$__]);
          var $es=
           new _A_($Data.$List.$esNEW512UNQ1665,[$__]);
          var $__12=
           new _A_($UHC.$Base.$_3a,[$e,$es]);
          var $__13=
           new _A_($UHC.$Base.$_3a,[$d,$ds]);
          var $__14=
           new _A_($UHC.$Base.$_3a,[$c,$cs]);
          var $__15=
           new _A_($UHC.$Base.$_3a,[$b,$bs]);
          var $__16=
           new _A_($UHC.$Base.$_3a,[$a,$as]);
          return [$__16,$__15,$__14,$__13,$__12];});
$Data.$List.$asNEW500UNQ1637=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$bsNEW503UNQ1644=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$csNEW506UNQ1651=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[2];});
$Data.$List.$dsNEW509UNQ1658=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[3];});
$Data.$List.$esNEW512UNQ1665=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[4];});
$Data.$List.$__318__1164__0=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Data.$List.$__318__1168__0,[$__2[2],$__2[3],$__2[0],$__2[1],$__2[4]]);});
$Data.$List.$unzip5=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldr,[$Data.$List.$__318__1164__0,$Data.$List.$__318__1200]);}),[]);
$Data.$List.$__318__1238=
 new _A_(new _F_(function()
                 {return [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];}),[]);
$Data.$List.$__318__1212__0=
 new _F_(function($c,$d,$a,$b,$__)
         {var $as=
           new _A_($Data.$List.$asNEW525UNQ1715,[$__]);
          var $bs=
           new _A_($Data.$List.$bsNEW528UNQ1721,[$__]);
          var $cs=
           new _A_($Data.$List.$csNEW531UNQ1727,[$__]);
          var $ds=
           new _A_($Data.$List.$dsNEW534UNQ1733,[$__]);
          var $__10=
           new _A_($UHC.$Base.$_3a,[$d,$ds]);
          var $__11=
           new _A_($UHC.$Base.$_3a,[$c,$cs]);
          var $__12=
           new _A_($UHC.$Base.$_3a,[$b,$bs]);
          var $__13=
           new _A_($UHC.$Base.$_3a,[$a,$as]);
          return [$__13,$__12,$__11,$__10];});
$Data.$List.$asNEW525UNQ1715=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$bsNEW528UNQ1721=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$csNEW531UNQ1727=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[2];});
$Data.$List.$dsNEW534UNQ1733=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[3];});
$Data.$List.$__318__1208__0=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Data.$List.$__318__1212__0,[$__2[2],$__2[3],$__2[0],$__2[1]]);});
$Data.$List.$unzip4=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$foldr,[$Data.$List.$__318__1208__0,$Data.$List.$__318__1238]);}),[]);
$Data.$List.$intersect=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$_3d_3d,[$__]);
          return new _A_($Data.$List.$intersectBy,[$__2]);});
$Data.$List.$subsequences=
 new _F_(function($xs)
         {var $__=
           new _A_($Data.$List.$nonEmptySubsequences,[$xs]);
          return new _A_($UHC.$Base.$_3a,[$UHC.$Base.$_5b_5d,$__]);});
$Data.$List.$__318__1276NEW549=
 new _F_(function($xss)
         {return new _A_($UHC.$Base.$concatMap,[$Data.$List.$_24okUNQ1760,$xss]);});
$Data.$List.$_24okUNQ1760=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW129__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$__._2,$UHC.$Base.$_5b_5d]);
             $__swJSW129__0=
              $__5;
             break;
            case 1:
             $__swJSW129__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW129__0;});
$Data.$List.$__318__1264NEW556=
 new _F_(function($xss)
         {return new _A_($UHC.$Base.$concatMap,[$Data.$List.$_24okUNQ1753,$xss]);});
$Data.$List.$_24okUNQ1753=
 new _F_(function($_24x)
         {var $__=
           _e_($_24x);
          var $__swJSW130__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$__._1,$UHC.$Base.$_5b_5d]);
             $__swJSW130__0=
              $__5;
             break;
            case 1:
             $__swJSW130__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW130__0;});
$Data.$List.$transpose=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW131__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              _e_($__._1);
             var $__swJSW132__0;
             switch($__5._tag_)
              {case 0:
                var $__8=
                 new _A_($Data.$List.$__318__1276NEW549,[$__._2]);
                var $__9=
                 new _A_($UHC.$Base.$_3a,[$__5._2,$__8]);
                var $__10=
                 new _A_($Data.$List.$transpose,[$__9]);
                var $__11=
                 new _A_($Data.$List.$__318__1264NEW556,[$__._2]);
                var $__12=
                 new _A_($UHC.$Base.$_3a,[$__5._1,$__11]);
                var $__13=
                 new _A_($UHC.$Base.$_3a,[$__12,$__10]);
                $__swJSW132__0=
                 $__13;
                break;
               case 1:
                var $__14=
                 new _A_($Data.$List.$transpose,[$__._2]);
                $__swJSW132__0=
                 $__14;
                break;}
             $__swJSW131__0=
              $__swJSW132__0;
             break;
            case 1:
             $__swJSW131__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW131__0;});
$Data.$List.$tsNEW565UNQ1780=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Data.$List.$fsNEW568UNQ1784=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Data.$List.$select=
 new _F_(function($p,$x,$__)
         {var $ts=
           new _A_($Data.$List.$tsNEW565UNQ1780,[$__]);
          var $fs=
           new _A_($Data.$List.$fsNEW568UNQ1784,[$__]);
          var $__6=
           new _A_($p,[$x]);
          var $__7=
           _e_($__6);
          var $__swJSW135__0;
          switch($__7._tag_)
           {case 0:
             var $__8=
              _e_($UHC.$Base.$otherwise);
             var $__swJSW136__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 new _A_($UHC.$Base.$packedStringToString,["FAIL 315_39_0"]);
                var $__10=
                 new _A_($UHC.$Base.$error,[$__9]);
                $__swJSW136__0=
                 $__10;
                break;
               case 1:
                var $__11=
                 new _A_($UHC.$Base.$_3a,[$x,$fs]);
                var $__12=
                 [$ts,$__11];
                $__swJSW136__0=
                 $__12;
                break;}
             $__swJSW135__0=
              $__swJSW136__0;
             break;
            case 1:
             var $__13=
              new _A_($UHC.$Base.$_3a,[$x,$ts]);
             var $__14=
              [$__13,$fs];
             $__swJSW135__0=
              $__14;
             break;}
          return $__swJSW135__0;});
$Data.$List.$partition=
 new _F_(function($p)
         {var $__=
           [$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d];
          var $__3=
           new _A_($Data.$List.$select,[$p]);
          return new _A_($UHC.$Base.$foldr,[$__3,$__]);});
$Data.$List.$tails=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__swJSW137__0;
          switch($__._tag_)
           {case 0:
             var $__5=
              new _A_($Data.$List.$tails,[$__._2]);
             var $__6=
              new _A_($UHC.$Base.$_3a,[$__,$__5]);
             $__swJSW137__0=
              $__6;
             break;
            case 1:
             var $__7=
              new _A_($UHC.$Base.$_3a,[$UHC.$Base.$_5b_5d,$UHC.$Base.$_5b_5d]);
             $__swJSW137__0=
              $__7;
             break;}
          return $__swJSW137__0;});
$Data.$List.$isInfixOf=
 new _F_(function($__,$needle,$haystack)
         {var $__4=
           new _A_($Data.$List.$tails,[$haystack]);
          var $__5=
           new _A_($Data.$List.$isPrefixOf,[$__,$needle]);
          return new _A_($UHC.$Base.$any,[$__5,$__4]);});
$Data.$List.$nub_27UNQ1810=
 new _F_(function($__,$x1,$x2)
         {var $x14=
           _e_($x1);
          var $__swJSW138__0;
          switch($x14._tag_)
           {case 0:
             var $__7=
              new _A_($UHC.$Base.$elem,[$__,$x14._1,$x2]);
             var $__8=
              _e_($__7);
             var $__swJSW139__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 _e_($UHC.$Base.$otherwise);
                var $__swJSW140__0;
                switch($__9._tag_)
                 {case 0:
                   $__swJSW140__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__10=
                    new _A_($UHC.$Base.$_3a,[$x14._1,$x2]);
                   var $__11=
                    new _A_($Data.$List.$nub_27UNQ1810,[$__,$x14._2,$__10]);
                   var $__12=
                    new _A_($UHC.$Base.$_3a,[$x14._1,$__11]);
                   $__swJSW140__0=
                    $__12;
                   break;}
                $__swJSW139__0=
                 $__swJSW140__0;
                break;
               case 1:
                var $__13=
                 new _A_($Data.$List.$nub_27UNQ1810,[$__,$x14._2,$x2]);
                $__swJSW139__0=
                 $__13;
                break;}
             $__swJSW138__0=
              $__swJSW139__0;
             break;
            case 1:
             $__swJSW138__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW138__0;});
$Data.$List.$nub=
 new _F_(function($__,$l)
         {return new _A_($Data.$List.$nub_27UNQ1810,[$__,$l,$UHC.$Base.$_5b_5d]);});
$Data.$List.$deleteBy=
 new _F_(function($x1,$x2,$x3)
         {var $x34=
           _e_($x3);
          var $__swJSW141__0;
          switch($x34._tag_)
           {case 0:
             var $__=
              new _A_($x1,[$x2,$x34._1]);
             var $__8=
              _e_($__);
             var $__swJSW142__0;
             switch($__8._tag_)
              {case 0:
                var $__9=
                 new _A_($Data.$List.$deleteBy,[$x1,$x2,$x34._2]);
                var $__10=
                 new _A_($UHC.$Base.$_3a,[$x34._1,$__9]);
                $__swJSW142__0=
                 $__10;
                break;
               case 1:
                $__swJSW142__0=
                 $x34._2;
                break;}
             $__swJSW141__0=
              $__swJSW142__0;
             break;
            case 1:
             $__swJSW141__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW141__0;});
$Data.$List.$unionBy=
 new _F_(function($eq,$xs,$ys)
         {var $__=
           new _A_($Data.$List.$nubBy,[$eq,$ys]);
          var $__5=
           new _A_($Data.$List.$deleteBy,[$eq]);
          var $__6=
           new _A_($UHC.$Base.$flip,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$foldl,[$__6,$__,$xs]);
          return new _A_($UHC.$Base.$_2b_2b,[$xs,$__7]);});
$Data.$List.$union=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$_3d_3d,[$__]);
          return new _A_($Data.$List.$unionBy,[$__2]);});
$Data.$List.$deleteFirstsBy=
 new _F_(function($eq)
         {var $__=
           new _A_($Data.$List.$deleteBy,[$eq]);
          var $__3=
           new _A_($UHC.$Base.$flip,[$__]);
          return new _A_($UHC.$Base.$foldl,[$__3]);});
$Data.$List.$delete=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$_3d_3d,[$__]);
          return new _A_($Data.$List.$deleteBy,[$__2]);});
$Data.$List.$_5c_5c=
 new _F_(function($__)
         {var $__2=
           new _A_($Data.$List.$delete,[$__]);
          var $__3=
           new _A_($UHC.$Base.$flip,[$__2]);
          return new _A_($UHC.$Base.$foldl,[$__3]);});
$Data.$List.$elemIndices=
 new _F_(function($__,$x)
         {var $__3=
           new _A_($UHC.$Base.$_3d_3d,[$__,$x]);
          return new _A_($Data.$List.$findIndices,[$__3]);});
