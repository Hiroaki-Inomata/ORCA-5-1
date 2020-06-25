      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  Z03.
      ***  02  Z03-FIXED8.
              03  Z03-NOTEBOOK1.
                04  Z03-PAGENO  PIC S9(9)   BINARY.
                04  Z03-FIXED9.
                  05  Z03-KANANAME.
                    06  Z03-VALUE   PIC X(100).
                  05  Z03-NAME.
                    06  Z03-VALUE   PIC X(100).
                  05  Z03-TEN.
                    06  Z03-VALUE   PIC S9(8)V9(2).
                  05  Z03-OLDTEN.
                    06  Z03-VALUE   PIC S9(8)V9(2).
                  05  Z03-BYOSYOKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-YUKOSTYMD.
                    06  Z03-VALUE   PIC X(10).
                  05  Z03-YUKOEDYMD.
                    06  Z03-VALUE   PIC X(10).
                  05  Z03-TSUSOKUAGE.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KGNAGE.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-JGNAGE.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-FORMALNAME.
                    06  Z03-VALUE   PIC X(200).
                  05  Z03-SHONAME.
                    06  Z03-VALUE   PIC X(200).
                  05  Z03-INPCHKKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-HOSPSRYKBNCOMB.
                    06  Z03-HOSPSRYKBN.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-HOSPSRYKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-HOSPSRYKBN-ITEM    PIC X(30)   OCCURS  70.
                  05  Z03-TANICDCOB.
                    06  Z03-TANICD.
                      07  Z03-VALUE   PIC X(28).
                    06  Z03-TANICD-COUNT   PIC S9(9)   BINARY.
                    06  Z03-TANICD-ITEM    PIC X(28)   OCCURS 200 TIMES.
                  05  Z03-SRYKBNCOB.
                    06  Z03-SRYKBN.
                      07  Z03-VALUE   PIC X(20).
                    06  Z03-SRYKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-SRYKBN-ITEM    PIC X(20)   OCCURS  200.
                  05  Z03-TENSIKIBETUCOB.
                    06  Z03-TENSIKIBETU.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-TENSIKIBETU-COUNT   PIC S9(9)   BINARY.
                    06  Z03-TENSIKIBETU-ITEM    PIC X(30)   OCCURS  10.
                  05  Z03-OLDTENSKBCOB.
                    06  Z03-OLDTENSKB.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-OLDTENSKB-COUNT   PIC S9(9)   BINARY.
                    06  Z03-OLDTENSKB-ITEM    PIC X(30)   OCCURS  10.
                  05  Z03-SRYSYUKBNCOB.
                    06  Z03-SRYSYUKBN.
                      07  Z03-VALUE   PIC X(74).
                    06  Z03-SRYSYUKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-SRYSYUKBN-ITEM    PIC X(74)   OCCURS  200.
                  05  Z03-DATAKBNCOB.
                    06  Z03-DATAKBN.
                      07  Z03-VALUE   PIC X(16).
                    06  Z03-DATAKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-DATAKBN-ITEM    PIC X(16)   OCCURS  3 TIMES.
                  05  Z03-BYOKANRENKBNCOB.
                    06  Z03-BYOKANRENKBN.
                      07  Z03-VALUE   PIC X(50).
                    06  Z03-BYOKANRENKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-BYOKANRENKBN-ITEM    PIC X(50)   OCCURS 20.
                  05  Z03-SDOKNRYOCOB.
                    06  Z03-SDOKNRYO.
                      07  Z03-VALUE   PIC X(50).
                    06  Z03-SDOKNRYO-COUNT   PIC S9(9)   BINARY.
                    06  Z03-SDOKNRYO-ITEM    PIC X(50)   OCCURS  100.
                  05  Z03-HKNTEKKBNCOB.
                    06  Z03-HKNTEKKBN.
                      07  Z03-VALUE   PIC X(20).
                    06  Z03-HKNTEKKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-HKNTEKKBN-ITEM    PIC X(20)   OCCURS  2.
                  05  Z03-OSINKBNCOB.
                    06  Z03-OSINKBN.
                      07  Z03-VALUE   PIC X(20).
                    06  Z03-OSINKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-OSINKBN-ITEM    PIC X(20)   OCCURS  8 TIMES.
                  05  Z03-SAIKETUCOMB.
                    06  Z03-SAIKETUKBN.
                      07  Z03-VALUE   PIC X(16).
                    06  Z03-SAIKETUKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-SAIKETUKBN-ITEM    PIC X(16)   OCCURS  4.
                  05  Z03-YKZKNRKBNCOB.
                    06  Z03-YKZKNRKBN.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-YKZKNRKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-YKZKNRKBN-ITEM    PIC X(30)   OCCURS  5.
                  05  Z03-TIMEKSNKBNCOB.
                    06  Z03-TIMEKSNKBN.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-TIMEKSNKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-TIMEKSNKBN-ITEM    PIC X(30)   OCCURS  10.
                  05  Z03-SANTEIRRKKBNCOB.
                    06  Z03-SANTEIRRKKBN.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-SANTEIRRKKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-SANTEIRRKKBN-ITEM    PIC X(30)   OCCURS  5.
                  05  Z03-GAIKANRIKBNCOB.
                    06  Z03-GAIKANRIKBN.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-GAIKANRIKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-GAIKANRIKBN-ITEM    PIC X(30)   OCCURS  3.
                  05  Z03-NYUGAITEKKBNCOMB.
                    06  Z03-NYUGAITEKKBN.
                      07  Z03-VALUE   PIC X(20).
                    06  Z03-NYUGAITEKKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-NYUGAITEKKBN-ITEM    PIC X(20)   OCCURS  3.
                  05  Z03-ROUTEKKBNCOMB.
                    06  Z03-ROUTEKKBN.
                      07  Z03-VALUE   PIC X(20).
                    06  Z03-ROUTEKKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-ROUTEKKBN-ITEM    PIC X(20)   OCCURS  3.
                  05  Z03-IRYOKAN-COMB.
                    06  Z03-IRYOKAN.
                      07  Z03-VALUE   PIC X(20).
                    06  Z03-IRYOKAN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-IRYOKAN-ITEM
                            PIC X(20)   OCCURS  9 TIMES.
                  05  Z03-ZENKNSKBN-COMB.
                    06  Z03-ZENKNSKBN.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-ZENKNSKBN-COUNT   PIC S9(9)   BINARY.
                    06  Z03-ZENKNSKBN-ITEM
                            PIC X(30)   OCCURS  5 TIMES.
                  05  Z03-DPCKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-ROSAIKBN.
                    06  Z03-VALUE   PIC S9(1).
                04  Z03-FIXED10.
                  05  Z03-JITUDAY.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-DAYCNT.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-JGNCNTERR.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KZMCOMPSIKIBETU.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KZMERR.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-CHUKSNCD.
                    06  Z03-VALUE   PIC X(4).
                  05  Z03-CHUKSNTSUBAN.
                    06  Z03-VALUE   PIC X(1).
                  05  Z03-AGEKSNKGN1.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNJGN1.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNSRYCD1.
                    06  Z03-VALUE   PIC X(9).
                  05  Z03-AGEKSNKGN2.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNKGN4.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNKGN3.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNJGN4.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNJGN3.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNJGN2.
                    06  Z03-VALUE   PIC X(2).
                  05  Z03-AGEKSNSRYCD4.
                    06  Z03-VALUE   PIC X(9).
                  05  Z03-AGEKSNSRYCD3.
                    06  Z03-VALUE   PIC X(9).
                  05  Z03-AGEKSNSRYCD2.
                    06  Z03-VALUE   PIC X(9).
                  05  Z03-JGNCNT.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-JGNCNT1D.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-JGNCNT1W.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-JGNCNTETCM.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-JGNCNTETC.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-KZMTEN.
                    06  Z03-VALUE   PIC S9(8)V9(2).
                  05  Z03-KZMKGN.
                    06  Z03-VALUE   PIC S9(8).
                  05  Z03-KZMJGN.
                    06  Z03-VALUE   PIC S9(8).
                  05  Z03-KZM.
                    06  Z03-VALUE   PIC S9(8).
                  05  Z03-PLUS-JGNCNT.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-PLUS-JGNCNT1D.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-PLUS-JGNCNT1W.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-PLUS-JGNCNTERR.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-PLUS-SANTEIRRKCOMB.
                    06  Z03-PLUS-SANTEIRRK.
                      07  Z03-VALUE   PIC X(30).
                    06  Z03-PLUS-SANTEI-COUNT   PIC S9(9)   BINARY.
                    06  Z03-PLUS-SANTEI-ITEM
                            PIC X(30)   OCCURS  3 TIMES.
                  05  Z03-PLUS-JGNCNTETCM.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-PLUS-JGNCNTETC.
                    06  Z03-VALUE   PIC S9(3).
                04  Z03-FIXED11.
                  05  Z03-TUSOKUGAIKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-SHORTSTAYOPE.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-CHPGYOKOSOTIKSN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-AUTOFUNGO.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-AUTOHOUGO.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KEIBUJYUTU.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-SEKIZUISOKUTEI.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-DONORKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-CHPMESUKSN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-LASERKSN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KNSJISKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KNSJISGRPKBN.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-CHPNAISIKSN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-HOUKNSKBN.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-HOKATUTEIGENKBN.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-TEIGENKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KENTAICOMMENT.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-NYUKHNKBN.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-KANGOKSN.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-CHIIKIKSN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-NYUKHNKSNKBN.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-KIJUNTEIGENKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KIJUNTEIGENCD.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-MICMESUKSN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-GAZOOPESUP.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-MASUISKBKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-FUKUBIKUNAIKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-FUKUBIKUKOTUKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-TIMEMASUIKBN.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-HISINSYUMONKSN.
                    06  Z03-NUMDATA PIC S9(1).
                  05  Z03-TOUKETUHOZONKSN.
                    06  Z03-NUMDATA PIC S9(1).
                  05  Z03-SISIKSN.
                    06  Z03-NUMDATA PIC S9(1).
                  05  Z03-AKUSEIBYORIKSN.
                    06  Z03-NUMDATA PIC S9(1).
                  05  Z03-SOGAIKOTEIKSN.
                    06  Z03-NUMDATA PIC S9(1).
                  05  Z03-CHPSESSAKUKSN.
                    06  Z03-NUMDATA PIC S9(1).
                  05  Z03-SASINJIHEISIKBN.
                    06  Z03-NUMDATA PIC S9(1).
                04  Z03-FIXED12.
                  05  Z03-SSTKIJUNCD1.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD5.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD4.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD3.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD2.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD10.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD9.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD8.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD7.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-SSTKIJUNCD6.
                    06  Z03-VALUE   PIC S9(4).
                  05  Z03-NYUTENTTLSKB.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-GAITENTTLKBN.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-CDKBN-KOUBAN.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-CDKBN-BU.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-CDKBN-SYO.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-CDKBN-KBN.
                    06  Z03-VALUE   PIC X(1).
                  05  Z03-KOKUCHI-KBN.
                    06  Z03-VALUE   PIC X(1).
                  05  Z03-KOKUCHI-SYO.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KOKUCHI-BU.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-CDKBN-KBNNUM.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-CDKBN-KBNNUM-EDA.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-KOKUCHI-KBNNUM.
                    06  Z03-VALUE   PIC S9(3).
                  05  Z03-KOKUCHI-KBNNUM-EDA.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-KOKUCHI-KOUBAN.
                    06  Z03-VALUE   PIC S9(2).
                  05  Z03-KOKUJISKBKBN2.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KOKUJISKBKBN1.
                    06  Z03-VALUE   PIC S9(1).
                  05  Z03-KOUHYOJYUNNUM.
                    06  Z03-VALUE   PIC S9(9).
                  05  Z03-KUBUNBANGOU.
                    06  Z03-TEXTDATA    PIC X(30).
              03  Z03-SRYCDH.
                04  Z03-VALUE   PIC X(9).
              03  Z03-NAMEH.
                04  Z03-VALUE   PIC X(100).
              03  Z03-YUKOSTYMDH.
                04  Z03-VALUE   PIC X(9).
              03  Z03-YUKOEDYMDH.
                04  Z03-VALUE   PIC X(9).
              03  Z03-MIDASHI.
                04  Z03-MIDASHI-STYLE   PIC X(20).
                04  Z03-MIDASHI-DATA    PIC X(80).
              03  Z03-B03.
                04  Z03-B03-STATE   PIC S9(9)   BINARY.
              03  Z03-B10.
                04  Z03-B10-STATE   PIC S9(9)   BINARY.
              03  Z03-B12.
                04  Z03-B12-STATE   PIC S9(9)   BINARY.
