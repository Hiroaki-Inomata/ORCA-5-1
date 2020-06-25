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
         02  Z05.
      ***  02  Z05-FIXED7.
              03  Z05-JGNAGE.
                04  Z05-VALUE   PIC X(2).
              03  Z05-KGNAGE.
                04  Z05-VALUE   PIC X(2).
              03  Z05-SRYCDH.
                04  Z05-VALUE   PIC X(9).
              03  Z05-NAMEH.
                04  Z05-VALUE   PIC X(100).
              03  Z05-YUKOSTYMDH.
                04  Z05-VALUE   PIC X(9).
              03  Z05-YUKOEDYMDH.
                04  Z05-VALUE   PIC X(9).
              03  Z05-KANANAME.
                04  Z05-VALUE   PIC X(100).
              03  Z05-NAME.
                04  Z05-VALUE   PIC X(100).
              03  Z05-TEN.
                04  Z05-VALUE   PIC S9(8)V9(2).
              03  Z05-OLDTEN.
                04  Z05-VALUE   PIC S9(8)V9(2).
              03  Z05-YUKOSTYMD.
                04  Z05-VALUE   PIC X(10).
              03  Z05-YUKOEDYMD.
                04  Z05-VALUE   PIC X(10).
              03  Z05-JGNTEN.
                04  Z05-VALUE   PIC S9(7).
              03  Z05-KOUHYOJYUNNUM.
                04  Z05-VALUE   PIC S9(9).
              03  Z05-FORMALNAME.
                04  Z05-VALUE   PIC X(200).
              03  Z05-SHONAME.
                04  Z05-VALUE   PIC X(200).
              03  Z05-TENSIKIBETUCOB.
                04  Z05-TENSIKIBETU.
                  05  Z05-VALUE   PIC X(26).
                04  Z05-TENSIKIBETU-COUNT   PIC S9(9)   BINARY.
                04  Z05-TENSIKIBETU-ITEM    PIC X(26)   OCCURS  7 TIMES.
              03  Z05-OLDTENSKBCOB.
                04  Z05-OLDTENSKB.
                  05  Z05-VALUE   PIC X(26).
                04  Z05-OLDTENSKB-COUNT   PIC S9(9)   BINARY.
                04  Z05-OLDTENSKB-ITEM    PIC X(26)   OCCURS  7 TIMES.
              03  Z05-TANICDCOB.
                04  Z05-TANICD.
                  05  Z05-VALUE   PIC X(28).
                04  Z05-TANICD-COUNT   PIC S9(9)   BINARY.
                04  Z05-TANICD-ITEM    PIC X(28)   OCCURS 200 TIMES.
              03  Z05-MEIUSESKBCOB.
                04  Z05-MEIUSESKB.
                  05  Z05-VALUE   PIC X(20).
                04  Z05-MEIUSESKB-COUNT   PIC S9(9)   BINARY.
                04  Z05-MEIUSESKB-ITEM    PIC X(20)   OCCURS  3 TIMES.
              03  Z05-TOKUKIZAIAGEKSNKBNCOB.
                04  Z05-TOKUKIZAIAGEKSNKBN.
                  05  Z05-VALUE   PIC X(20).
                04  Z05-TOKUKIZAIAGEKSN-COUNT   PIC S9(9)   BINARY.
                04  Z05-TOKUKIZAIAGEKSN-ITEM    PIC X(20)   OCCURS  2.
              03  Z05-DATAKBNCOB.
                04  Z05-DATAKBN.
                  05  Z05-VALUE   PIC X(16).
                04  Z05-DATAKBN-COUNT   PIC S9(9)   BINARY.
                04  Z05-DATAKBN-ITEM    PIC X(16)   OCCURS  3 TIMES.
              03  Z05-TOKUKIZAISBT1COB.
                04  Z05-TOKUKIZAISBT1.
                  05  Z05-VALUE   PIC X(30).
                04  Z05-TOKUKIZAISBT1-COUNT   PIC S9(9)   BINARY.
                04  Z05-TOKUKIZAISBT1-ITEM    PIC X(30)   OCCURS  4.
              03  Z05-TOKUKIZAISBT2COB.
                04  Z05-TOKUKIZAISBT2.
                  05  Z05-VALUE   PIC X(30).
                04  Z05-TOKUKIZAISBT2-COUNT   PIC S9(9)   BINARY.
                04  Z05-TOKUKIZAISBT2-ITEM    PIC X(30)   OCCURS  2.
              03  Z05-SANSOKBNCOB.
                04  Z05-SANSOKBN.
                  05  Z05-VALUE   PIC X(30).
                04  Z05-SANSOKBN-COUNT   PIC S9(9)   BINARY.
                04  Z05-SANSOKBN-ITEM    PIC X(30)   OCCURS  7 TIMES.
              03  Z05-KEIKASOCHIYMD.
                04  Z05-VALUE   PIC X(10).
              03  Z05-CDKBN-BU.
                04  Z05-VALUE   PIC X(2).
              03  Z05-CDKBN-KBNNUM.
                04  Z05-VALUE   PIC X(3).
              03  Z05-COPYCD-G.
                04  Z05-COPYCD-STATE   PIC S9(9)   BINARY.
                04  Z05-COPYCD         PIC X(9).
              03  Z05-B06.
                04  Z05-B06-STATE   PIC S9(9)   BINARY.
                04  Z05-B06-LABEL   PIC X(10).
              03  Z05-B09.
                04  Z05-B09-STATE   PIC S9(9)   BINARY.
                04  Z05-B09-LABEL   PIC X(10).
              03  Z05-B03.
                04  Z05-B03-STATE   PIC S9(9)   BINARY.
              03  Z05-B12.
                04  Z05-B12-STATE   PIC S9(9)   BINARY.
              04  Z05-MIDASHI.
                05  Z05-MIDASHI-STYLE  PIC X(20).
                05  Z05-MIDASHI-DATA   PIC X(80).
              03  Z05-SELNUM-G.
                04  Z05-SELNUM         PIC S9(3).
              03  Z05-SCROLLEDWINDOW1.
                04  Z05-PRICELIST.
                  05  Z05-PRICELIST-COUNT   PIC S9(9)   BINARY.
                  05  Z05-ROW PIC S9(9)   BINARY.
                  05  Z05-PRICELIST-SELECT
                          PIC X       OCCURS  50 TIMES.
                  05  Z05-FGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  Z05-BGCOLOR
                          PIC X(20)   OCCURS  50 TIMES.
                  05  Z05-ITEM        OCCURS  50 TIMES.
                    06  Z05-TRENNUM       PIC X(3).
                    06  Z05-TPRCYUKOSTYMD PIC X(10).
                    06  Z05-TPRCYUKOEDYMD PIC X(10).
                    06  Z05-TPRICE        PIC X(12).
              03  Z05-PRCYUKOSTYMD.
                04  Z05-TEXTDATA    PIC X(10).
              03  Z05-PRCYUKOEDYMD.
                04  Z05-TEXTDATA    PIC X(10).
              03  Z05-PRICE.
                04  Z05-TEXTDATA    PIC X(12).
              03  Z05-BS03.
                04  Z05-BS03-STATE   PIC S9(9)   BINARY.
              03  Z05-BS12.
                04  Z05-BS12-STATE   PIC S9(9)   BINARY.
              04  Z05-PRICEMSG.
                05  Z05-PRICEMSG-STYLE  PIC X(20).
                05  Z05-PRICEMSG-DATA   PIC X(80).
              04  Z05-USER-TANICD-G.
                05  Z05-USER-TANICD-STYLE   PIC X(20).
                05  Z05-USER-TANICD    PIC X(28).
