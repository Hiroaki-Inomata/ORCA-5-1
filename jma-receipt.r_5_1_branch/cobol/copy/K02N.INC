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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  K02N.
      ****  02  K02N-FIXED2.
            03  K02N-TITLE          PIC X(120).
            03  K02N-POPUP-STATUS.
              04  K02N-POPUP-SUMMARY   PIC X(256).
              04  K02N-POPUP-BODY      PIC X(1024).
              04  K02N-POPUP-ICON      PIC X(256).
              04  K02N-POPUP-TIMEOUT   PIC S9(9)  BINARY.
              03  K02N-PTNUM.
                04  K02N-VALUE  PIC X(20).
              03  K02N-KANANAME-G.
                04  K02N-KANANAME-STYLE   PIC X(20).
                04  K02N-KANANAME   PIC X(50).
              03  K02N-NAME-G.
                04  K02N-NAME-STYLE   PIC X(20).
                04  K02N-NAME   PIC X(50).
              03  K02N-SEX.
                04  K02N-VALUE  PIC X(2).
              03  K02N-HKNCOMBICOMB.
                04  K02N-HKNCOMBI.
                  05  K02N-VALUE  PIC X(65).
                04  K02N-HKNCOMBI-COUNT  PIC S9(9)   BINARY.
                04  K02N-HKNCOMBI-ITEM   PIC X(65)   OCCURS  30 TIMES.
              03  K02N-RATE.
                04  K02N-VALUE  PIC X(6).
              03  K02N-BIRTHDAY.
                04  K02N-VALUE  PIC X(9).
              03  K02N-NENREI.
                04  K02N-VALUE  PIC X(6).
              03  K02N-SRYYMD.
                04  K02N-VALUE  PIC X(14).
              03  K02N-DOSELNUM.
                04  K02N-VALUE  PIC X(4).
              03  K02N-NYUINKA.
                04  K02N-VALUE  PIC X(10).
              03  K02N-NYUINKIKAN-G.
                04  K02N-NYUINKIKAN-STYLE  PIC X(20).
                04  K02N-NYUINKIKAN  PIC X(22).
              03  K02N-TEISEI.
                04  K02N-TEISEI-VALUE  PIC X.
                04  K02N-TEISEI-LABEL  PIC X(10).
              03  K02N-SRYKACOMB.
                04  K02N-SRYKA.
                  05  K02N-VALUE  PIC X(16).
                04  K02N-SRYKA-COUNT  PIC S9(9)   BINARY.
                04  K02N-SRYKA-ITEM   PIC X(16)   OCCURS  99 TIMES.
              03  K02N-TABOOCOB.
                04  K02N-TABOO.
                  05  K02N-VALUE  PIC X(62).
                04  K02N-TABOO-COUNT  PIC S9(9)   BINARY.
                04  K02N-TABOO-ITEM   PIC X(62)   OCCURS  40 TIMES.
              03  K02N-SCROLLEDWINDOW1.
                04  K02N-RRKLIST.
                  05  K02N-COUNT  PIC S9(9)   BINARY.
                  05  K02N-ROW     PIC S9(9)   BINARY.
                  05  K02N-ROWATTR PIC S9(9)   BINARY.
                  05  K02N-ITEM       OCCURS  150 TIMES.
                    06  K02N-NO      PIC X(4).
                    06  K02N-RRKYMD  PIC X(13).
                    06  K02N-RRKSRYKA  PIC X(2).
                    06  K02N-RRKHKNCOMBI  PIC X(04).
                  05  K02N-SELECT PIC X       OCCURS  150 TIMES.
              03  K02N-CONTKBN.
                04  K02N-CONTKBN-STYLE  PIC X(20).
                04  K02N-CONTKBN-VALUE  PIC X(6).
              03  K02N-SYORIMEI.
                04  K02N-SYORIMEI-STYLE  PIC X(20).
                04  K02N-SYORIMEI-VALUE  PIC X(12).
              03  K02N-MODE.
                04  K02N-VALUE  PIC X(10).
              03  K02N-JIKANKBNCOMB.
                04  K02N-JIKANKBN.
                  05  K02N-VALUE  PIC X(22).
                04  K02N-JIKANKBN-COUNT  PIC S9(9)   BINARY.
                04  K02N-JIKANKBN-ITEM   PIC X(22)   OCCURS  5 TIMES.
              03  K02N-GRPMEI.
                04  K02N-GRPMEI-STYLE   PIC X(20).
                04  K02N-GRPMEI-VALUE   PIC X(10).
              03  K02N-B04S.
                04  K02N-B04S-STATE   PIC S9(9)   BINARY.
              03  K02N-B05S.
                04  K02N-B05S-STATE   PIC S9(9)   BINARY.
              03  K02N-B07S.
                04  K02N-B07S-STATE   PIC S9(9)   BINARY.
              03  K02N-B08S.
                04  K02N-B08S-STATE   PIC S9(9)   BINARY.
              03  K02N-B09S.
                04  K02N-B09S-STATE   PIC S9(9)   BINARY.
              03  K02N-KENSA.
                04  K02N-KENSA-VALUE  PIC X.
                04  K02N-KENSA-LABEL  PIC X(10).
              03  K02N-B12.
                04  K02N-B12-STATE   PIC S9(9)   BINARY.
              03  K02N-B06S.
                04  K02N-B06S-STATE   PIC S9(9)   BINARY.
              03  K02N-B11S.
                04  K02N-B11S-STATE   PIC S9(9)   BINARY.
              03  K02N-B12S.
                04  K02N-B12S-STATE   PIC S9(9)   BINARY.
              03  K02N-SCROLLEDWINDOW2.
                04  K02N-HPOS   PIC S9(9)   BINARY.
                04  K02N-VPOS   PIC S9(9)   BINARY.
                04  K02N-PANDATABLE1.
                  05  K02N-PANDATABLE1-STATE  PIC S9(9)   BINARY.
                  05  K02N-PANDATABLE1-STYLE  PIC X(20).
                  05  K02N-PANDATABLE1-TROW   PIC S9(9)   BINARY.
                  05  K02N-PANDATABLE1-TROWATTR   PIC S9(9)   BINARY.
                  05  K02N-PANDATABLE1-TCOLUMN    PIC S9(9)   BINARY.
                  05  K02N-PANDATABLE1-TVALUE PIC X(256).
                  05  K02N-PANDATABLE1-ROWDATA     OCCURS  400 TIMES.
                    06  K02N-SRYKBN-G.
                      07  K02N-SRYKBN     PIC X(2).
                      07  K02N-SRYKBN-FGCOLOR    PIC X(20).
                      07  K02N-SRYKBN-BGCOLOR    PIC X(20).
                    06  K02N-INPUTCD-G.
                      07  K02N-INPUTCD   PIC X(54).
                      07  K02N-INPUTCD-FGCOLOR    PIC X(20).
                      07  K02N-INPUTCD-BGCOLOR    PIC X(20).
                    06  K02N-MEISYO-G.
      ***             07  K02N-MEISYO   PIC X(80).
                      07  K02N-MEISYO   PIC X(140).
                      07  K02N-MEISYO-FGCOLOR    PIC X(20).
                      07  K02N-MEISYO-BGCOLOR    PIC X(20).
                    06  K02N-SURYO-G.
                      07  K02N-SURYO   PIC X(60).
                      07  K02N-SURYO-FGCOLOR    PIC X(20).
                      07  K02N-SURYO-BGCOLOR    PIC X(20).
              03  K02N-KOHMSG.
                04  K02N-KOHMSG-STYLE  PIC X(20).
                04  K02N-KOHMSG-DATA   PIC X(50).
              03  K02N-MODE3.
                04  K02N-MODE3-STYLE   PIC X(20).
                04  K02N-MODE3-VALUE   PIC X(10).
              03  K02N-BSH2.
                04  K02N-BSH2-STYLE  PIC X(20).
                04  K02N-BSH2-LABEL  PIC X(8).



