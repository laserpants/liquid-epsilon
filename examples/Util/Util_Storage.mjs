// Util.Storage
var $Util=
 ($Util ? $Util : {});
$Util.$Storage=
 ($Util.$Storage ? $Util.$Storage : {});
$Util.$Storage.$____get=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($.jStorage.get($__3));
          return [$__2,$__4];});
$Util.$Storage.$____getTTL=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($.jStorage.getTTL($__3));
          return [$__2,$__4];});
$Util.$Storage.$____storageSize=
 new _F_(function($__)
         {var $__2=
           _e_($.jStorage.storageSize());
          return [$__,$__2];});
$Util.$Storage.$____currentBackend=
 new _F_(function($__)
         {var $__2=
           _e_($.jStorage.currentBackend());
          return [$__,$__2];});
$Util.$Storage.$____delete=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($.jStorage.deleteKey($__3));
          var $__5=
           _e_([]);
          return [$__2,$__5];});
$Util.$Storage.$____boolean=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return Boolean($__2);});
$Util.$Storage.$____flush=
 new _F_(function($__)
         {var $__2=
           _e_($.jStorage.flush());
          var $__3=
           _e_([]);
          return [$__,$__3];});
$Util.$Storage.$____setTTL=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($.jStorage.setTTL($__4,$__5));
          var $__7=
           _e_([]);
          return [$__3,$__7];});
$Util.$Storage.$____set=
 new _F_(function($__,$__2,$__3)
         {var $__4=
           _e_($__);
          var $__5=
           _e_($__2);
          var $__6=
           _e_($.jStorage.set($__4,$__5));
          var $__7=
           _e_([]);
          return [$__3,$__7];});
$Util.$Storage.$____storageAvailable=
 new _F_(function($__)
         {var $__2=
           _e_($.jStorage.storageAvailable());
          return [$__,$__2];});
$Util.$Storage.$____reInit=
 new _F_(function($__)
         {var $__2=
           _e_($.jStorage.reInit());
          var $__3=
           _e_([]);
          return [$__,$__3];});
$Util.$Storage.$____index=
 new _F_(function($__)
         {var $__2=
           _e_($.jStorage.index());
          return [$__,$__2];});
$Util.$Storage.$____indexLength=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.length;});
$Util.$Storage.$____indexElement=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3[$__4];});
$Util.$Storage.$setTTL=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Util.$Storage.$____setTTL,$Util.$String.$pack]);}),[]);
$Util.$Storage.$deleteKey=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Util.$Storage.$____delete,$Util.$String.$pack]);}),[]);
$Util.$Storage.$flush=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$____flush;}),[]);
$Util.$Storage.$__Rep0IndexPtrDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {return $UHC.$Base.$undefined;});
$Util.$Storage.$__Rep0IndexPtrDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {return $UHC.$Base.$undefined;});
$Util.$Storage.$__Rep0IndexPtrNEW47UNQ131SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Util.$Storage.$__Rep0IndexPtrNEW49UNQ132EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Util.$Storage.$__Rep0IndexPtrNEW49UNQ132EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Util.$Storage.$__Rep0IndexPtrDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Util.$Storage.$__Rep0IndexPtrDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Util.$Storage.$__Rep0IndexPtrUNQ131SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Storage.$__Rep0IndexPtrNEW47UNQ131SDCGENRepresentable0,[$Util.$Storage.$__Rep0IndexPtrUNQ131SDCGENRepresentable0]);}),[]);
$Util.$Storage.$__Rep0IndexPtrGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$__Rep0IndexPtrUNQ131SDCGENRepresentable0;}),[]);
$Util.$Storage.$getTTL_27=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$____getTTL;}),[]);
$Util.$Storage.$setTTL_27=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$____setTTL;}),[]);
$Util.$Storage.$reInit=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$____reInit;}),[]);
$Util.$Storage.$_24D__IndexPtrDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Util.Storage"]);});
$Util.$Storage.$_24D__IndexPtrDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["IndexPtr"]);});
$Util.$Storage.$_24D__IndexPtrNEW59UNQ140SDCGENDatatype=
 new _F_(function($_24D__IndexPtr)
         {var $_24D__IndexPtr2=
           new _A_($Util.$Storage.$_24D__IndexPtrNEW61UNQ141EVLSDCGENDatatype,[$_24D__IndexPtr]);
          return $_24D__IndexPtr2;});
$Util.$Storage.$_24D__IndexPtrNEW61UNQ141EVLSDCGENDatatype=
 new _F_(function($_24D__IndexPtr)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__IndexPtr]));
          var $__5=
           {_tag_:0,_1:$Util.$Storage.$_24D__IndexPtrDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Util.$Storage.$_24D__IndexPtrDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Util.$Storage.$_24D__IndexPtrUNQ140SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Util.$Storage.$_24D__IndexPtrNEW59UNQ140SDCGENDatatype,[$Util.$Storage.$_24D__IndexPtrUNQ140SDCGENDatatype]);}),[]);
$Util.$Storage.$_24D__IndexPtrGENDatatype=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$_24D__IndexPtrUNQ140SDCGENDatatype;}),[]);
$Util.$Storage.$__11__164=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);}),[]);
$Util.$Storage.$__11__162=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Util.$Storage.$__11__164,$Util.$String.$unpack]);}),[]);
$Util.$Storage.$currentBackend=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$Storage.$____currentBackend,$Util.$Storage.$__11__162]);}),[]);
$Util.$Storage.$_24okUNQ153=
 new _F_(function($_24x)
         {var $__=
           new _A_($Util.$Storage.$__11__172NEW70,[$_24x]);
          var $__3=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__3,$__]);});
$Util.$Storage.$__11__172NEW70=
 new _F_(function($_24x)
         {var $__=
           new _A_($Util.$Storage.$____indexLength,[$_24x]);
          var $__3=
           new _A_($Util.$Storage.$__9__1362__0NEW73UNQ158CCN,[$_24x,$__]);
          var $__4=
           _e_(new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,0,$__]));
          var $__swJSW2__0;
          switch($__4._tag_)
           {case 0:
             $__swJSW2__0=
              $__3;
             break;
            case 1:
             $__swJSW2__0=
              $UHC.$Base.$_5b_5d;
             break;}
          return $__swJSW2__0;});
$Util.$Storage.$__9__1362__0NEW73UNQ158CCN=
 new _F_(function($_24x,$__)
         {var $g=
           new _A_($Util.$Storage.$____indexElement,[$_24x]);
          var $__4=
           new _A_($UHC.$Base.$_2d,[$UHC.$Base.$Num__DCT74__101__0,$__,1]);
          var $__5=
           new _A_($UHC.$Base.$enumFromTo,[$UHC.$Base.$Enum__DCT74__118__0,0,$__4]);
          var $__6=
           new _A_($Util.$Storage.$_24okUNQ161,[$g]);
          return new _A_($UHC.$Base.$concatMap,[$__6,$__5]);});
$Util.$Storage.$_24okUNQ161=
 new _F_(function($g,$_24x)
         {var $__=
           new _A_($g,[$_24x]);
          return new _A_($UHC.$Base.$_3a,[$__,$UHC.$Base.$_5b_5d]);});
$Util.$Storage.$index_27=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$Storage.$____index,$Util.$Storage.$_24okUNQ153]);}),[]);
$Util.$Storage.$storageSize=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$____storageSize;}),[]);
$Util.$Storage.$_24okUNQ127=
 new _F_(function($_24x)
         {var $__=
           new _A_($UHC.$Base.$_2f_3d,[$UHC.$Base.$Eq__DCT74__88__0,$_24x,0]);
          var $__3=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__3,$__]);});
$Util.$Storage.$storageAvailable=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$Storage.$____storageAvailable,$Util.$Storage.$_24okUNQ127]);}),[]);
$Util.$Storage.$deleteKey_27=
 new _A_(new _F_(function()
                 {return $Util.$Storage.$____delete;}),[]);
$Util.$Storage.$__11__221=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$map,[$Util.$String.$unpack]);}),[]);
$Util.$Storage.$__11__220=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);}),[]);
$Util.$Storage.$__11__218=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Util.$Storage.$__11__220,$Util.$Storage.$__11__221]);}),[]);
$Util.$Storage.$index=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$Storage.$index_27,$Util.$Storage.$__11__218]);}),[]);
$Util.$Storage.$getTTL=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Util.$Storage.$____getTTL,$Util.$String.$pack]);}),[]);
$Util.$Storage.$_24okUNQ103=
 new _F_(function($__,$_24x)
         {var $__3=
           new _A_($Util.$Storage.$__11__236NEW96,[$__,$_24x]);
          var $__4=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0]);
          return new _A_($UHC.$Base.$_24,[$__4,$__3]);});
$Util.$Storage.$__11__236NEW96=
 new _F_(function($__,$_24x)
         {var $__3=
           new _A_($Util.$Storage.$____boolean,[$_24x]);
          var $__4=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,0,$__3]);
          var $__5=
           _e_($__4);
          var $__swJSW3__0;
          switch($__5._tag_)
           {case 0:
             var $__6=
              new _A_($Util.$String.$unpack,[$_24x]);
             var $__7=
              new _A_($Util.$JSON.$parse,[$Util.$JSON.$json]);
             var $__8=
              new _A_($UHC.$Base.$_24,[$__7,$__6]);
             var $__9=
              _e_($__8);
             var $__swJSW4__0;
             switch($__9._tag_)
              {case 0:
                var $__12=
                 _e_($__9._1);
                var $__15=
                 _e_($__9._2);
                var $__swJSW6__0;
                switch($__15._tag_)
                 {case 0:
                   $__swJSW6__0=
                    $UHC.$Base.$undefined;
                   break;
                  case 1:
                   var $__18=
                    new _A_($Util.$JSON.$fromJSON,[$__,$__12[0]]);
                   $__swJSW6__0=
                    $__18;
                   break;}
                $__swJSW4__0=
                 $__swJSW6__0;
                break;
               case 1:
                $__swJSW4__0=
                 $UHC.$Base.$Nothing__;
                break;}
             $__swJSW3__0=
              $__swJSW4__0;
             break;
            case 1:
             $__swJSW3__0=
              $UHC.$Base.$Nothing__;
             break;}
          return $__swJSW3__0;});
$Util.$Storage.$get_27=
 new _F_(function($__,$key)
         {var $__3=
           new _A_($Util.$Storage.$____get,[$key]);
          var $__4=
           new _A_($Util.$Storage.$_24okUNQ103,[$__]);
          return new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__3,$__4]);});
$Util.$Storage.$get=
 new _F_(function($__)
         {var $__2=
           new _A_($Util.$Storage.$get_27,[$__]);
          return new _A_($UHC.$Base.$_2e,[$__2,$Util.$String.$pack]);});
$Util.$Storage.$set_27=
 new _F_(function($__,$key,$val)
         {var $__4=
           new _A_($Util.$JSON.$toJSON,[$__,$val]);
          var $__5=
           new _A_($UHC.$Base.$_24,[$Util.$JSON.$valueToStr,$__4]);
          var $__6=
           new _A_($UHC.$Base.$_24,[$Util.$String.$pack,$__5]);
          return new _A_($Util.$Storage.$____set,[$key,$__6]);});
$Util.$Storage.$set=
 new _F_(function($__)
         {var $__2=
           new _A_($Util.$Storage.$set_27,[$__]);
          return new _A_($UHC.$Base.$_2e,[$__2,$Util.$String.$pack]);});
