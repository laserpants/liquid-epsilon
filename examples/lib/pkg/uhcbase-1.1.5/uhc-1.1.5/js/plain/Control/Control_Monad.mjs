// Control.Monad
var $Control=
 ($Control ? $Control : {});
$Control.$Monad=
 ($Control.$Monad ? $Control.$Monad : {});
$Control.$Monad.$zipWithM__=
 new _F_(function($__,$f,$xs,$ys)
         {var $__5=
           new _A_($UHC.$Base.$zipWith,[$f,$xs,$ys]);
          return new _A_($UHC.$Base.$sequence__,[$__,$__5]);});
$Control.$Monad.$MonadPlus__CLS116__0__0=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           {_tag_:0,_1:$UHC.$Base.$undefined,_2:$UHC.$Base.$undefined,_3:$UHC.$Base.$undefined};
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW5UNQ311DCT116__1__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           new _A_($Control.$Monad.$MonadPlus__NEW7UNQ312EVLDCT116__1__0RDC,[$MonadPlus__]);
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW7UNQ312EVLDCT116__1__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           _e_(new _A_($Control.$Monad.$MonadPlus__CLS116__0__0,[$MonadPlus__]));
          var $__6=
           {_tag_:0,_1:$UHC.$Base.$_2b_2b,_2:$UHC.$Base.$_5b_5d,_3:$UHC.$Base.$Monad__DCT74__85__0};
          return $__6;});
$Control.$Monad.$MonadPlus__UNQ311DCT116__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$MonadPlus__NEW5UNQ311DCT116__1__0RDC,[$Control.$Monad.$MonadPlus__UNQ311DCT116__1__0RDC]);}),[]);
$Control.$Monad.$MonadPlus__DCT116__1__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$MonadPlus__UNQ311DCT116__1__0RDC;}),[]);
$Control.$Monad.$MonadPlus__DCT116__4__0DFLControl_2eMonad_2emplus=
 new _F_(function($x1,$x2)
         {var $x13=
           _e_($x1);
          var $__swJSW1__0;
          switch($x13._tag_)
           {case 0:
             $__swJSW1__0=
              $x1;
             break;
            case 1:
             $__swJSW1__0=
              $x2;
             break;}
          return $__swJSW1__0;});
$Control.$Monad.$MonadPlus__NEW14UNQ317DCT116__4__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           new _A_($Control.$Monad.$MonadPlus__NEW16UNQ318EVLDCT116__4__0RDC,[$MonadPlus__]);
          return $MonadPlus__2;});
$Control.$Monad.$MonadPlus__NEW16UNQ318EVLDCT116__4__0RDC=
 new _F_(function($MonadPlus__)
         {var $MonadPlus__2=
           _e_(new _A_($Control.$Monad.$MonadPlus__CLS116__0__0,[$MonadPlus__]));
          var $__6=
           {_tag_:0,_1:$Control.$Monad.$MonadPlus__DCT116__4__0DFLControl_2eMonad_2emplus,_2:$UHC.$Base.$Nothing__,_3:$UHC.$Base.$Monad__DCT74__75__0};
          return $__6;});
$Control.$Monad.$MonadPlus__UNQ317DCT116__4__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Control.$Monad.$MonadPlus__NEW14UNQ317DCT116__4__0RDC,[$Control.$Monad.$MonadPlus__UNQ317DCT116__4__0RDC]);}),[]);
$Control.$Monad.$MonadPlus__DCT116__4__0=
 new _A_(new _F_(function()
                 {return $Control.$Monad.$MonadPlus__UNQ317DCT116__4__0RDC;}),[]);
$Control.$Monad.$__120__62__0=
 new _F_(function($__,$x1,$xs,$fax)
         {return new _A_($Control.$Monad.$foldM,[$__,$x1,$fax,$xs]);});
$Control.$Monad.$foldM=
 new _F_(function($__,$x1,$x2,$x3)
         {var $x35=
           _e_($x3);
          var $__swJSW3__0;
          switch($x35._tag_)
           {case 0:
             var $__8=
              new _A_($x1,[$x2,$x35._1]);
             var $__9=
              new _A_($Control.$Monad.$__120__62__0,[$__,$x1,$x35._2]);
             var $__10=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__8,$__9]);
             $__swJSW3__0=
              $__10;
             break;
            case 1:
             var $__11=
              new _A_($UHC.$Base.$return,[$__,$x2]);
             $__swJSW3__0=
              $__11;
             break;}
          return $__swJSW3__0;});
$Control.$Monad.$join=
 new _F_(function($__,$x)
         {return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$x,$UHC.$Base.$id]);});
$Control.$Monad.$replicateM=
 new _F_(function($__,$n,$x)
         {var $__4=
           new _A_($UHC.$Base.$replicate,[$n,$x]);
          return new _A_($UHC.$Base.$sequence,[$__,$__4]);});
$Control.$Monad.$_3e_3d_3e=
 new _F_(function($__,$f,$g,$x)
         {var $__5=
           new _A_($f,[$x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__5,$g]);});
$Control.$Monad.$_3c_3d_3c=
 new _F_(function($__)
         {var $__2=
           new _A_($Control.$Monad.$_3e_3d_3e,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$Control.$Monad.$forM=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$mapM,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$Control.$Monad.$unless=
 new _F_(function($__,$p,$s)
         {var $__4=
           _e_($p);
          var $__swJSW4__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW4__0=
              $s;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$return,[$__,[]]);
             $__swJSW4__0=
              $__5;
             break;}
          return $__swJSW4__0;});
$Control.$Monad.$_24Dict_2dMonadPlus=
 new _F_(function($x1,$x2,$x3)
         {return {_tag_:0,_1:$x1,_2:$x2,_3:$x3};});
$Control.$Monad.$mzero=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._2;});
$Control.$Monad.$when=
 new _F_(function($__,$p,$s)
         {var $__4=
           _e_($p);
          var $__swJSW6__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($UHC.$Base.$return,[$__,[]]);
             $__swJSW6__0=
              $__5;
             break;
            case 1:
             $__swJSW6__0=
              $s;
             break;}
          return $__swJSW6__0;});
$Control.$Monad.$replicateM__=
 new _F_(function($__,$n,$x)
         {var $__4=
           new _A_($UHC.$Base.$replicate,[$n,$x]);
          return new _A_($UHC.$Base.$sequence__,[$__,$__4]);});
$Control.$Monad.$mplus=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          return $x2._1;});
$Control.$Monad.$msum=
 new _F_(function($__)
         {var $__2=
           new _A_($Control.$Monad.$mzero,[$__]);
          var $__3=
           new _A_($Control.$Monad.$mplus,[$__]);
          return new _A_($UHC.$Base.$foldr,[$__3,$__2]);});
$Control.$Monad.$zipWithM=
 new _F_(function($__,$f,$xs,$ys)
         {var $__5=
           new _A_($UHC.$Base.$zipWith,[$f,$xs,$ys]);
          return new _A_($UHC.$Base.$sequence,[$__,$__5]);});
$Control.$Monad.$mapAndUnzipM=
 new _F_(function($__,$f,$xs)
         {var $__4=
           new _A_($UHC.$Base.$return,[$__]);
          var $__5=
           new _A_($UHC.$Base.$_2e,[$__4,$UHC.$Base.$unzip]);
          var $__6=
           new _A_($UHC.$Base.$map,[$f,$xs]);
          var $__7=
           new _A_($UHC.$Base.$sequence,[$__,$__6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__7,$__5]);});
$Control.$Monad.$forM__=
 new _F_(function($__)
         {var $__2=
           new _A_($UHC.$Base.$mapM__,[$__]);
          return new _A_($UHC.$Base.$flip,[$__2]);});
$Control.$Monad.$_24okUNQ137=
 new _F_(function($__,$f,$_24x)
         {var $__4=
           new _A_($f,[$_24x]);
          return new _A_($UHC.$Base.$return,[$__,$__4]);});
$Control.$Monad.$liftM=
 new _F_(function($__,$f,$m1)
         {var $__4=
           new _A_($Control.$Monad.$_24okUNQ137,[$__,$f]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__4]);});
$Control.$Monad.$forever=
 new _F_(function($__,$a)
         {var $__3=
           new _A_($Control.$Monad.$forever,[$__,$a]);
          return new _A_($UHC.$Base.$_3e_3e,[$__,$a,$__3]);});
$Control.$Monad.$foldM__=
 new _F_(function($__,$f,$a,$xs)
         {var $__5=
           new _A_($UHC.$Base.$return,[$__,[]]);
          var $__6=
           new _A_($Control.$Monad.$foldM,[$__,$f,$a,$xs]);
          return new _A_($UHC.$Base.$_3e_3e,[$__,$__6,$__5]);});
$Control.$Monad.$_24okUNQ172=
 new _F_(function($__,$f,$m2,$m3,$m4,$m5,$_24x)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ176,[$__,$f,$m3,$m4,$m5,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__8]);});
$Control.$Monad.$_24okUNQ176=
 new _F_(function($__,$f,$m3,$m4,$m5,$_24x,$_24x7)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ180,[$__,$f,$m4,$m5,$_24x,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m3,$__8]);});
$Control.$Monad.$_24okUNQ180=
 new _F_(function($__,$f,$m4,$m5,$_24x,$_24x6,$_24x7)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ184,[$__,$f,$m5,$_24x,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m4,$__8]);});
$Control.$Monad.$_24okUNQ184=
 new _F_(function($__,$f,$m5,$_24x,$_24x5,$_24x6,$_24x7)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ188,[$__,$f,$_24x,$_24x5,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m5,$__8]);});
$Control.$Monad.$_24okUNQ188=
 new _F_(function($__,$f,$_24x,$_24x4,$_24x5,$_24x6,$_24x7)
         {var $__8=
           new _A_($f,[$_24x,$_24x4,$_24x5,$_24x6,$_24x7]);
          return new _A_($UHC.$Base.$return,[$__,$__8]);});
$Control.$Monad.$liftM5=
 new _F_(function($__,$f,$m1,$m2,$m3,$m4,$m5)
         {var $__8=
           new _A_($Control.$Monad.$_24okUNQ172,[$__,$f,$m2,$m3,$m4,$m5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__8]);});
$Control.$Monad.$_24okUNQ207=
 new _F_(function($__,$f,$m2,$m3,$m4,$_24x)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ211,[$__,$f,$m3,$m4,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__7]);});
$Control.$Monad.$_24okUNQ211=
 new _F_(function($__,$f,$m3,$m4,$_24x,$_24x6)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ215,[$__,$f,$m4,$_24x,$_24x6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m3,$__7]);});
$Control.$Monad.$_24okUNQ215=
 new _F_(function($__,$f,$m4,$_24x,$_24x5,$_24x6)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ219,[$__,$f,$_24x,$_24x5,$_24x6]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m4,$__7]);});
$Control.$Monad.$_24okUNQ219=
 new _F_(function($__,$f,$_24x,$_24x4,$_24x5,$_24x6)
         {var $__7=
           new _A_($f,[$_24x,$_24x4,$_24x5,$_24x6]);
          return new _A_($UHC.$Base.$return,[$__,$__7]);});
$Control.$Monad.$liftM4=
 new _F_(function($__,$f,$m1,$m2,$m3,$m4)
         {var $__7=
           new _A_($Control.$Monad.$_24okUNQ207,[$__,$f,$m2,$m3,$m4]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__7]);});
$Control.$Monad.$_24okUNQ235=
 new _F_(function($__,$f,$m2,$m3,$_24x)
         {var $__6=
           new _A_($Control.$Monad.$_24okUNQ239,[$__,$f,$m3,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__6]);});
$Control.$Monad.$_24okUNQ239=
 new _F_(function($__,$f,$m3,$_24x,$_24x5)
         {var $__6=
           new _A_($Control.$Monad.$_24okUNQ243,[$__,$f,$_24x,$_24x5]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m3,$__6]);});
$Control.$Monad.$_24okUNQ243=
 new _F_(function($__,$f,$_24x,$_24x4,$_24x5)
         {var $__6=
           new _A_($f,[$_24x,$_24x4,$_24x5]);
          return new _A_($UHC.$Base.$return,[$__,$__6]);});
$Control.$Monad.$liftM3=
 new _F_(function($__,$f,$m1,$m2,$m3)
         {var $__6=
           new _A_($Control.$Monad.$_24okUNQ235,[$__,$f,$m2,$m3]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__6]);});
$Control.$Monad.$_24okUNQ256=
 new _F_(function($__,$f,$m2,$_24x)
         {var $__5=
           new _A_($Control.$Monad.$_24okUNQ260,[$__,$f,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m2,$__5]);});
$Control.$Monad.$_24okUNQ260=
 new _F_(function($__,$f,$_24x,$_24x4)
         {var $__5=
           new _A_($f,[$_24x,$_24x4]);
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Control.$Monad.$liftM2=
 new _F_(function($__,$f,$m1,$m2)
         {var $__5=
           new _A_($Control.$Monad.$_24okUNQ256,[$__,$f,$m2]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$m1,$__5]);});
$Control.$Monad.$ap=
 new _F_(function($__)
         {return new _A_($Control.$Monad.$liftM2,[$__,$UHC.$Base.$id]);});
$Control.$Monad.$_24okUNQ291=
 new _F_(function($__,$x1,$xs,$x,$_24x)
         {var $__6=
           new _A_($Control.$Monad.$filterM,[$__,$x1,$xs]);
          var $__7=
           new _A_($Control.$Monad.$_24okUNQ295,[$__,$x,$_24x]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__6,$__7]);});
$Control.$Monad.$_24okUNQ295=
 new _F_(function($__,$x,$_24x,$_24x4)
         {var $__5=
           new _A_($Control.$Monad.$__120__428NEW95,[$x,$_24x,$_24x4]);
          return new _A_($UHC.$Base.$return,[$__,$__5]);});
$Control.$Monad.$__120__428NEW95=
 new _F_(function($x,$_24x,$_24x3)
         {var $__=
           _e_($_24x);
          var $__swJSW8__0;
          switch($__._tag_)
           {case 0:
             $__swJSW8__0=
              $_24x3;
             break;
            case 1:
             var $__5=
              new _A_($UHC.$Base.$_3a,[$x,$_24x3]);
             $__swJSW8__0=
              $__5;
             break;}
          return $__swJSW8__0;});
$Control.$Monad.$filterM=
 new _F_(function($__,$x1,$x2)
         {var $x24=
           _e_($x2);
          var $__swJSW9__0;
          switch($x24._tag_)
           {case 0:
             var $__7=
              new _A_($x1,[$x24._1]);
             var $__8=
              new _A_($Control.$Monad.$_24okUNQ291,[$__,$x1,$x24._2,$x24._1]);
             $__swJSW9__0=
              new _A_($UHC.$Base.$_3e_3e_3d,[$__,$__7,$__8]);
             break;
            case 1:
             var $__9=
              new _A_($UHC.$Base.$return,[$__,$UHC.$Base.$_5b_5d]);
             $__swJSW9__0=
              $__9;
             break;}
          return $__swJSW9__0;});
$Control.$Monad.$__118__4630__2__0NEW105UNQ303=
 new _F_(function($__)
         {var $Monad__=
           _e_($__);
          return $Monad__._3;});
$Control.$Monad.$__120__450__0=
 new _F_(function($__,$__2,$x1)
         {var $__4=
           _e_($x1);
          var $__swJSW11__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              new _A_($Control.$Monad.$mzero,[$__]);
             $__swJSW11__0=
              $__5;
             break;
            case 1:
             var $__6=
              new _A_($UHC.$Base.$return,[$__2,[]]);
             $__swJSW11__0=
              $__6;
             break;}
          return $__swJSW11__0;});
$Control.$Monad.$guard=
 new _F_(function($__)
         {var $__2=
           new _A_($Control.$Monad.$__118__4630__2__0NEW105UNQ303,[$__]);
          return new _A_($Control.$Monad.$__120__450__0,[$__,$__2]);});
