// Util.FRP
var $Util=
 ($Util ? $Util : {});
$Util.$FRP=
 ($Util.$FRP ? $Util.$FRP : {});
$Util.$FRP.$____window=
 new _A_(new _F_(function()
                 {return window;}),[]);
$Util.$FRP.$____val=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.value;});
$Util.$FRP.$____hasEventProperty=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.hasOwnProperty("___e");});
$Util.$FRP.$____addEventListener=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__);
          var $__6=
           _e_($__2);
          var $__7=
           _e_($__3);
          var $__8=
           _e_($__5.addEventListener($__6,$__7,false));
          var $__9=
           _e_([]);
          return [$__4,$__9];});
$Util.$FRP.$____wrap1=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return function(v1)
                 {var res=
                   _e_(new _A_($__2,[v1,[]]));
                  _e_(res[0]);
                  return _e_(res[1]);};});
$Util.$FRP.$____prop=
 new _F_(function($__,$__2)
         {var $__3=
           _e_($__);
          var $__4=
           _e_($__2);
          return $__3[$__4];});
$Util.$FRP.$____clientX=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.___e["clientX"];});
$Util.$FRP.$____clientY=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.___e["clientY"];});
$Util.$FRP.$____document=
 new _A_(new _F_(function()
                 {return document;}),[]);
$Util.$FRP.$____set=
 new _F_(function($__,$__2,$__3,$__4)
         {var $__5=
           _e_($__);
          var $__6=
           _e_($__2);
          var $__7=
           _e_($__3);
          var $__8=
           _e_(primSetAttr($__5,$__6,$__7));
          return [$__4,$__8];});
$Util.$FRP.$_24S__pollDFLUHC_2eBase_2eselNameGENSelector=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["poll"]);});
$Util.$FRP.$_24S__pollNEW28UNQ157SDCGENSelector=
 new _F_(function($_24S__poll)
         {var $_24S__poll2=
           new _A_($Util.$FRP.$_24S__pollNEW30UNQ158EVLSDCGENSelector,[$_24S__poll]);
          return $_24S__poll2;});
$Util.$FRP.$_24S__pollNEW30UNQ158EVLSDCGENSelector=
 new _F_(function($_24S__poll)
         {var $Selector__=
           _e_(new _A_($UHC.$Base.$Selector__CLS74__351__0,[$_24S__poll]));
          var $__4=
           {_tag_:0,_1:$Util.$FRP.$_24S__pollDFLUHC_2eBase_2eselNameGENSelector};
          return $__4;});
$Util.$FRP.$_24S__pollUNQ157SDCGENSelector=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$_24S__pollNEW28UNQ157SDCGENSelector,[$Util.$FRP.$_24S__pollUNQ157SDCGENSelector]);}),[]);
$Util.$FRP.$_24S__pollGENSelector=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$_24S__pollUNQ157SDCGENSelector;}),[]);
$Util.$FRP.$_24S__eventDFLUHC_2eBase_2eselNameGENSelector=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["event"]);});
$Util.$FRP.$_24S__eventNEW36UNQ152SDCGENSelector=
 new _F_(function($_24S__event)
         {var $_24S__event2=
           new _A_($Util.$FRP.$_24S__eventNEW38UNQ153EVLSDCGENSelector,[$_24S__event]);
          return $_24S__event2;});
$Util.$FRP.$_24S__eventNEW38UNQ153EVLSDCGENSelector=
 new _F_(function($_24S__event)
         {var $Selector__=
           _e_(new _A_($UHC.$Base.$Selector__CLS74__351__0,[$_24S__event]));
          var $__4=
           {_tag_:0,_1:$Util.$FRP.$_24S__eventDFLUHC_2eBase_2eselNameGENSelector};
          return $__4;});
$Util.$FRP.$_24S__eventUNQ152SDCGENSelector=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$_24S__eventNEW36UNQ152SDCGENSelector,[$Util.$FRP.$_24S__eventUNQ152SDCGENSelector]);}),[]);
$Util.$FRP.$_24S__eventGENSelector=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$_24S__eventUNQ152SDCGENSelector;}),[]);
$Util.$FRP.$poll=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.poll;});
$Util.$FRP.$Signal__=
 new _F_(function($x1,$x2)
         {return {_tag_:0,event:$x1,poll:$x2};});
$Util.$FRP.$__Rep1SignalDFLUHC_2eBase_2eto1GENRepresentable1=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Util.$FRP.$Signal__,[$proj__3._1,$proj__3._2]);
          return $__;});
$Util.$FRP.$__Rep1SignalDFLUHC_2eBase_2efrom1GENRepresentable1=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$Rec1__,[$x2.poll]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2.event]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$Util.$FRP.$__Rep1SignalNEW58UNQ111SDCGENRepresentable1=
 new _F_(function($__)
         {var $__2=
           new _A_($Util.$FRP.$__Rep1SignalNEW60UNQ112EVLSDCGENRepresentable1,[$__]);
          return $__2;});
$Util.$FRP.$__Rep1SignalNEW60UNQ112EVLSDCGENRepresentable1=
 new _F_(function($__)
         {var $Representable1__=
           _e_(new _A_($UHC.$Base.$Representable1__CLS74__370__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Util.$FRP.$__Rep1SignalDFLUHC_2eBase_2efrom1GENRepresentable1,_2:$Util.$FRP.$__Rep1SignalDFLUHC_2eBase_2eto1GENRepresentable1};
          return $__5;});
$Util.$FRP.$__Rep1SignalUNQ111SDCGENRepresentable1=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$__Rep1SignalNEW58UNQ111SDCGENRepresentable1,[$Util.$FRP.$__Rep1SignalUNQ111SDCGENRepresentable1]);}),[]);
$Util.$FRP.$__Rep1SignalGENRepresentable1=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$__Rep1SignalUNQ111SDCGENRepresentable1;}),[]);
$Util.$FRP.$event=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2.event;});
$Util.$FRP.$_24D__SignalDFLUHC_2eBase_2emoduleNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Util.FRP"]);});
$Util.$FRP.$_24D__SignalDFLUHC_2eBase_2edatatypeNameGENDatatype=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Signal"]);});
$Util.$FRP.$_24D__SignalNEW69UNQ136SDCGENDatatype=
 new _F_(function($_24D__Signal)
         {var $_24D__Signal2=
           new _A_($Util.$FRP.$_24D__SignalNEW71UNQ137EVLSDCGENDatatype,[$_24D__Signal]);
          return $_24D__Signal2;});
$Util.$FRP.$_24D__SignalNEW71UNQ137EVLSDCGENDatatype=
 new _F_(function($_24D__Signal)
         {var $Datatype__=
           _e_(new _A_($UHC.$Base.$Datatype__CLS74__350__0,[$_24D__Signal]));
          var $__5=
           {_tag_:0,_1:$Util.$FRP.$_24D__SignalDFLUHC_2eBase_2edatatypeNameGENDatatype,_2:$Util.$FRP.$_24D__SignalDFLUHC_2eBase_2emoduleNameGENDatatype};
          return $__5;});
$Util.$FRP.$_24D__SignalUNQ136SDCGENDatatype=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$_24D__SignalNEW69UNQ136SDCGENDatatype,[$Util.$FRP.$_24D__SignalUNQ136SDCGENDatatype]);}),[]);
$Util.$FRP.$_24D__SignalGENDatatype=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$_24D__SignalUNQ136SDCGENDatatype;}),[]);
$Util.$FRP.$__Rep0SignalDFLUHC_2eBase_2efrom0GENRepresentable0=
 new _F_(function($x)
         {var $x2=
           _e_($x);
          var $__5=
           new _A_($UHC.$Base.$K1__,[$x2.poll]);
          var $__6=
           new _A_($UHC.$Base.$M1__,[$__5]);
          var $__7=
           new _A_($UHC.$Base.$K1__,[$x2.event]);
          var $__8=
           new _A_($UHC.$Base.$M1__,[$__7]);
          var $__9=
           new _A_($UHC.$Base.$_3a_2a_3a,[$__8,$__6]);
          var $__10=
           new _A_($UHC.$Base.$M1__,[$__9]);
          var $__11=
           new _A_($UHC.$Base.$M1__,[$__10]);
          return $__11;});
$Util.$FRP.$__Rep0SignalDFLUHC_2eBase_2eto0GENRepresentable0=
 new _F_(function($proj__1)
         {var $proj__3=
           _e_($proj__1);
          var $__=
           new _A_($Util.$FRP.$Signal__,[$proj__3._1,$proj__3._2]);
          return $__;});
$Util.$FRP.$__Rep0SignalNEW88UNQ86SDCGENRepresentable0=
 new _F_(function($__)
         {var $__2=
           new _A_($Util.$FRP.$__Rep0SignalNEW90UNQ87EVLSDCGENRepresentable0,[$__]);
          return $__2;});
$Util.$FRP.$__Rep0SignalNEW90UNQ87EVLSDCGENRepresentable0=
 new _F_(function($__)
         {var $Representable0__=
           _e_(new _A_($UHC.$Base.$Representable0__CLS74__369__0,[$__]));
          var $__5=
           {_tag_:0,_1:$Util.$FRP.$__Rep0SignalDFLUHC_2eBase_2efrom0GENRepresentable0,_2:$Util.$FRP.$__Rep0SignalDFLUHC_2eBase_2eto0GENRepresentable0};
          return $__5;});
$Util.$FRP.$__Rep0SignalUNQ86SDCGENRepresentable0=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$__Rep0SignalNEW88UNQ86SDCGENRepresentable0,[$Util.$FRP.$__Rep0SignalUNQ86SDCGENRepresentable0]);}),[]);
$Util.$FRP.$__Rep0SignalGENRepresentable0=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$__Rep0SignalUNQ86SDCGENRepresentable0;}),[]);
$Util.$FRP.$Functor__DCT18__0__0DFLUHC_2eBase_2efmap=
 new _F_(function($f,$__)
         {var $__3=
           _e_($__);
          var $__6=
           new _A_($Control.$Monad.$liftM,[$UHC.$Base.$Monad__DCT74__339__0,$f,$__3.poll]);
          var $__7=
           new _A_($Util.$FRP.$Signal__,[$__3.event,$__6]);
          return $__7;});
$Util.$FRP.$Functor__NEW99UNQ281DCT18__0__0RDC=
 new _F_(function($Functor__)
         {var $Functor__2=
           new _A_($Util.$FRP.$Functor__NEW101UNQ283EVLDCT18__0__0RDC,[$Functor__]);
          return $Functor__2;});
$Util.$FRP.$Functor__NEW101UNQ283EVLDCT18__0__0RDC=
 new _F_(function($Functor__)
         {var $Functor__2=
           _e_(new _A_($UHC.$Base.$Functor__CLS74__44__0,[$Functor__]));
          var $__4=
           {_tag_:0,_1:$Util.$FRP.$Functor__DCT18__0__0DFLUHC_2eBase_2efmap};
          return $__4;});
$Util.$FRP.$Functor__UNQ281DCT18__0__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$Functor__NEW99UNQ281DCT18__0__0RDC,[$Util.$FRP.$Functor__UNQ281DCT18__0__0RDC]);}),[]);
$Util.$FRP.$Functor__DCT18__0__0=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$Functor__UNQ281DCT18__0__0RDC;}),[]);
$Util.$FRP.$mouseUNQ243=
 new _F_(function($e)
         {var $__=
           new _A_($Util.$FRP.$____hasEventProperty,[$e]);
          var $__3=
           new _A_($UHC.$Base.$_3d_3d,[$UHC.$Base.$Eq__DCT74__88__0,$__,1]);
          var $__4=
           _e_($__3);
          var $__swJSW13__0;
          switch($__4._tag_)
           {case 0:
             var $__5=
              [0,0];
             $__swJSW13__0=
              $__5;
             break;
            case 1:
             var $__6=
              new _A_($Util.$FRP.$____clientY,[$e]);
             var $__7=
              new _A_($Util.$FRP.$____clientX,[$e]);
             var $__8=
              [$__7,$__6];
             $__swJSW13__0=
              $__8;
             break;}
          return $__swJSW13__0;});
$Util.$FRP.$__22__224=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$Util.$FRP.$____document]);
                  var $__2=
                   new _A_($UHC.$Base.$packedStringToString,["mousemove"]);
                  var $__3=
                   [$Util.$FRP.$____document,$__2];
                  var $__4=
                   new _A_($UHC.$Base.$_3a,[$__3,$UHC.$Base.$_5b_5d]);
                  return {_tag_:0,event:$__4,poll:$__};}),[]);
$Util.$FRP.$mouseSignal=
 new _A_(new _F_(function()
                 {var $__=
                   new _A_($UHC.$Base.$fmap,[$Util.$FRP.$Functor__DCT18__0__0,$Util.$FRP.$mouseUNQ243]);
                  return new _A_($UHC.$Base.$_24,[$__,$Util.$FRP.$__22__224]);}),[]);
$Util.$FRP.$Applicative__DCT18__1__0DFLControl_2eApplicative_2e_3c_2a_3e=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return new _A_($Util.$FRP.$__22__241__0,[$__2.poll,$__2.event]);});
$Util.$FRP.$__22__241__0=
 new _F_(function($p1,$a,$__)
         {var $__4=
           _e_($__);
          var $__7=
           new _A_($Control.$Applicative.$_3c_2a_3e,[$Control.$Applicative.$Applicative__DCT181__13__0,$p1,$__4.poll]);
          var $__8=
           new _A_($UHC.$Base.$_2b_2b,[$a,$__4.event]);
          return {_tag_:0,event:$__8,poll:$__7};});
$Util.$FRP.$Applicative__DCT18__1__0DFLControl_2eApplicative_2epure=
 new _F_(function($a)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$a]);
          return {_tag_:0,event:$UHC.$Base.$_5b_5d,poll:$__};});
$Util.$FRP.$Applicative__NEW129UNQ256DCT18__1__0RDC=
 new _F_(function($Applicative__)
         {var $Applicative__2=
           new _A_($Util.$FRP.$Applicative__NEW131UNQ259EVLDCT18__1__0RDC,[$Applicative__]);
          return $Applicative__2;});
$Util.$FRP.$Applicative__NEW131UNQ259EVLDCT18__1__0RDC=
 new _F_(function($Applicative__)
         {var $Applicative__2=
           _e_(new _A_($Control.$Applicative.$Applicative__CLS181__0__0,[$Applicative__]));
          var $__6=
           {_tag_:0,_1:$Util.$FRP.$Applicative__DCT18__1__0DFLControl_2eApplicative_2e_3c_2a_3e,_2:$Util.$FRP.$Applicative__DCT18__1__0DFLControl_2eApplicative_2epure,_3:$Util.$FRP.$Functor__DCT18__0__0};
          return $__6;});
$Util.$FRP.$Applicative__UNQ256DCT18__1__0RDC=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$Applicative__NEW129UNQ256DCT18__1__0RDC,[$Util.$FRP.$Applicative__UNQ256DCT18__1__0RDC]);}),[]);
$Util.$FRP.$Applicative__DCT18__1__0=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$Applicative__UNQ256DCT18__1__0RDC;}),[]);
$Util.$FRP.$sNEW136UNQ175=
 new _F_(function($ev,$e,$e_27)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$e_27]);
          var $__5=
           [$e,$ev];
          var $__6=
           new _A_($UHC.$Base.$_3a,[$__5,$UHC.$Base.$_5b_5d]);
          return {_tag_:0,event:$__6,poll:$__};});
$Util.$FRP.$__bindSignalOn=
 new _F_(function($ev,$prop,$e,$e_27)
         {var $s=
           new _A_($Util.$FRP.$sNEW136UNQ175,[$ev,$e,$e_27]);
          var $__=
           new _A_($Util.$String.$pack,[$prop]);
          var $__7=
           new _A_($Control.$Applicative.$pure,[$Util.$FRP.$Applicative__DCT18__1__0,$__]);
          var $__8=
           new _A_($Control.$Applicative.$_3c_24_3e,[$Util.$FRP.$Functor__DCT18__0__0,$Util.$FRP.$____prop,$s]);
          return new _A_($Control.$Applicative.$_3c_2a_3e,[$Util.$FRP.$Applicative__DCT18__1__0,$__8,$__7]);});
$Util.$FRP.$sNEW147UNQ185=
 new _F_(function($ev,$e)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$e]);
          var $__4=
           [$e,$ev];
          var $__5=
           new _A_($UHC.$Base.$_3a,[$__4,$UHC.$Base.$_5b_5d]);
          return {_tag_:0,event:$__5,poll:$__};});
$Util.$FRP.$__bindSignal=
 new _F_(function($ev,$prop,$e)
         {var $s=
           new _A_($Util.$FRP.$sNEW147UNQ185,[$ev,$e]);
          var $__=
           new _A_($Util.$String.$pack,[$prop]);
          var $__6=
           new _A_($Control.$Applicative.$pure,[$Util.$FRP.$Applicative__DCT18__1__0,$__]);
          var $__7=
           new _A_($Control.$Applicative.$_3c_24_3e,[$Util.$FRP.$Functor__DCT18__0__0,$Util.$FRP.$____prop,$s]);
          return new _A_($Control.$Applicative.$_3c_2a_3e,[$Util.$FRP.$Applicative__DCT18__1__0,$__7,$__6]);});
$Util.$FRP.$__22__331=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["scrollY"]);}),[]);
$Util.$FRP.$__22__330=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["scroll"]);}),[]);
$Util.$FRP.$scrollYSignal=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$__bindSignal,[$Util.$FRP.$__22__330,$Util.$FRP.$__22__331,$Util.$FRP.$____window]);}),[]);
$Util.$FRP.$__22__343__0=
 new _F_(function($__,$f)
         {return $f;});
$Util.$FRP.$onSignal=
 new _F_(function($s,$a)
         {var $__=
           new _A_($Control.$Applicative.$pure,[$Util.$FRP.$Applicative__DCT18__1__0,$a]);
          var $__4=
           new _A_($UHC.$Base.$fmap,[$Util.$FRP.$Functor__DCT18__0__0,$Util.$FRP.$__22__343__0,$s]);
          return new _A_($Control.$Applicative.$_3c_2a_3e,[$Util.$FRP.$Applicative__DCT18__1__0,$__4,$__]);});
$Util.$FRP.$_24C__SignalDFLUHC_2eBase_2econIsRecordGENConstructor=
 new _F_(function($x)
         {return $UHC.$Base.$True__;});
$Util.$FRP.$_24C__SignalDFLUHC_2eBase_2econNameGENConstructor=
 new _F_(function($x)
         {return new _A_($UHC.$Base.$packedStringToString,["Signal"]);});
$Util.$FRP.$_24C__SignalNEW166UNQ143SDCGENConstructor=
 new _F_(function($_24C__Signal)
         {var $_24C__Signal2=
           new _A_($Util.$FRP.$_24C__SignalNEW168UNQ144EVLSDCGENConstructor,[$_24C__Signal]);
          return $_24C__Signal2;});
$Util.$FRP.$_24C__SignalNEW168UNQ144EVLSDCGENConstructor=
 new _F_(function($_24C__Signal)
         {var $Constructor__=
           _e_(new _A_($UHC.$Base.$Constructor__CLS74__353__0,[$_24C__Signal]));
          var $__7=
           {_tag_:0,_1:$Constructor__._1,_2:$Util.$FRP.$_24C__SignalDFLUHC_2eBase_2econIsRecordGENConstructor,_3:$Constructor__._3,_4:$Util.$FRP.$_24C__SignalDFLUHC_2eBase_2econNameGENConstructor};
          return $__7;});
$Util.$FRP.$_24C__SignalUNQ143SDCGENConstructor=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$_24C__SignalNEW166UNQ143SDCGENConstructor,[$Util.$FRP.$_24C__SignalUNQ143SDCGENConstructor]);}),[]);
$Util.$FRP.$_24C__SignalGENConstructor=
 new _A_(new _F_(function()
                 {return $Util.$FRP.$_24C__SignalUNQ143SDCGENConstructor;}),[]);
$Util.$FRP.$__22__371=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["scrollX"]);}),[]);
$Util.$FRP.$__22__370=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["scroll"]);}),[]);
$Util.$FRP.$scrollXSignal=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$__bindSignal,[$Util.$FRP.$__22__370,$Util.$FRP.$__22__371,$Util.$FRP.$____window]);}),[]);
$Util.$FRP.$__bindUnitSignal=
 new _F_(function($ev,$e)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
          var $__4=
           [$e,$ev];
          var $__5=
           new _A_($UHC.$Base.$_3a,[$__4,$UHC.$Base.$_5b_5d]);
          return {_tag_:0,event:$__5,poll:$__};});
$Util.$FRP.$__22__389=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$packedStringToString,["click"]);}),[]);
$Util.$FRP.$clickSignal=
 new _A_(new _F_(function()
                 {return new _A_($Util.$FRP.$__bindUnitSignal,[$Util.$FRP.$__22__389]);}),[]);
$Util.$FRP.$sNEW182UNQ197=
 new _F_(function($e)
         {var $__=
           new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,$e]);
          var $__3=
           new _A_($UHC.$Base.$packedStringToString,["input"]);
          var $__4=
           [$e,$__3];
          var $__5=
           new _A_($UHC.$Base.$_3a,[$__4,$UHC.$Base.$_5b_5d]);
          return {_tag_:0,event:$__5,poll:$__};});
$Util.$FRP.$inputValue_27=
 new _F_(function($e)
         {var $s=
           new _A_($Util.$FRP.$sNEW182UNQ197,[$e]);
          return new _A_($UHC.$Base.$fmap,[$Util.$FRP.$Functor__DCT18__0__0,$Util.$FRP.$____val,$s]);});
$Util.$FRP.$__22__411=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$fmap,[$Util.$FRP.$Functor__DCT18__0__0,$Util.$String.$unpack]);}),[]);
$Util.$FRP.$inputValue=
 new _A_(new _F_(function()
                 {return new _A_($UHC.$Base.$_2e,[$Util.$FRP.$__22__411,$Util.$FRP.$inputValue_27]);}),[]);
$Util.$FRP.$evNEW197UNQ217=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[1];});
$Util.$FRP.$eNEW200UNQ216=
 new _F_(function($__)
         {var $__2=
           _e_($__);
          return $__2[0];});
$Util.$FRP.$__22__441__0=
 new _F_(function($v,$e,$setEventProperty,$obj)
         {var $__=
           new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$v,$UHC.$Base.$id]);
          var $__6=
           new _A_($setEventProperty,[$obj,$e]);
          return new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__6,$__]);});
$Util.$FRP.$attach=
 new _F_(function($x1)
         {var $__=
           _e_($x1);
          var $__5=
           _e_($__.event);
          var $__swJSW21__0;
          switch($__5._tag_)
           {case 0:
             var $__8=
              new _A_($UHC.$Base.$packedStringToString,["___e"]);
             var $__9=
              new _A_($Util.$String.$pack,[$__8]);
             var $setEventProperty=
              new _A_($Util.$FRP.$____set,[$__9]);
             var $__11=
              $__5._1;
             var $ev=
              new _A_($Util.$FRP.$evNEW197UNQ217,[$__11]);
             var $e=
              new _A_($Util.$FRP.$eNEW200UNQ216,[$__11]);
             var $__14=
              new _A_($Util.$FRP.$Signal__,[$__5._2,$__.poll]);
             var $__15=
              new _A_($Util.$FRP.$attach,[$__14]);
             var $__16=
              new _A_($Util.$FRP.$__22__441__0,[$__.poll,$e,$setEventProperty]);
             var $__17=
              new _A_($UHC.$Base.$_24,[$Util.$FRP.$____wrap1,$__16]);
             var $__18=
              new _A_($Util.$String.$pack,[$ev]);
             var $__19=
              new _A_($Util.$FRP.$____addEventListener,[$e,$__18]);
             var $__20=
              new _A_($UHC.$Base.$_24,[$__19,$__17]);
             $__swJSW21__0=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__20,$__15]);
             break;
            case 1:
             var $__21=
              new _A_($UHC.$Base.$return,[$UHC.$Base.$Monad__DCT74__339__0,[]]);
             var $__22=
              new _A_($UHC.$Base.$_3e_3e_3d,[$UHC.$Base.$Monad__DCT74__339__0,$__.poll,$UHC.$Base.$id]);
             var $__23=
              new _A_($UHC.$Base.$_3e_3e,[$UHC.$Base.$Monad__DCT74__339__0,$__22,$__21]);
             $__swJSW21__0=
              $__23;
             break;}
          return $__swJSW21__0;});
