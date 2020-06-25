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
        02  K02.
      ****  02  K02-FIXED2.
            03  K02-TITLE          PIC X(120).
            03  K02-POPUP-STATUS.
              04  K02-POPUP-SUMMARY   PIC X(256).
              04  K02-POPUP-BODY      PIC X(1024).
              04  K02-POPUP-ICON      PIC X(256).
              04  K02-POPUP-TIMEOUT   PIC S9(9)  BINARY.
              03  K02-PTNUM.
                04  K02-VALUE   PIC X(20).
              03  K02-KANANAME-G.
                04  K02-KANANAME-STYLE   PIC X(20).
                04  K02-KANANAME   PIC X(50).
              03  K02-NAME-G.
                04  K02-NAME-STYLE   PIC X(20).
                04  K02-NAME   PIC X(50).
              03  K02-BIRTHDAY.
                04  K02-VALUE   PIC X(9).
              03  K02-SEX.
                04  K02-VALUE   PIC X(2).
              03  K02-RATE.
                04  K02-VALUE   PIC X(6).
              03  K02-LASTYMD.
                04  K02-VALUE   PIC X(24).
              03  K02-SYOSINYMD.
                04  K02-VALUE   PIC X(24).
              03  K02-MISYUMONEY.
                04  K02-VALUE   PIC X(15).
              03  K02-SRYYMD.
                04  K02-VALUE   PIC X(14).
              03  K02-GOKEITEN.
                04  K02-VALUE   PIC X(15).
              03  K02-NENREI.
                04  K02-VALUE   PIC X(6).
              03  K02-ZAITENTOTAL.
                04  K02-VALUE   PIC X(15).
              03  K02-INGAI.
                04  K02-INGAI-VALUE   PIC X.
                04  K02-INGAI-LABEL   PIC X(10).
              03  K02-DOSELNUM.
                04  K02-VALUE   PIC X(4).
              03  K02-TEISEI.
                04  K02-TEISEI-VALUE   PIC X.
                04  K02-TEISEI-LABEL   PIC X(10).
              03  K02-HKNCOMBICOMB.
                04  K02-HKNCOMBI.
                  05  K02-VALUE   PIC X(65).
                04  K02-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  K02-HKNCOMBI-ITEM    PIC X(65)   OCCURS  30 TIMES.
              03  K02-CONTKBN.
                04  K02-CONTKBN-STYLE   PIC X(20).
                04  K02-CONTKBN-VALUE   PIC X(6).
              03  K02-MODE.
                04  K02-VALUE   PIC X(10).
              03  K02-SRYKACOMB.
                04  K02-SRYKA.
                  05  K02-VALUE   PIC X(16).
                04  K02-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  K02-SRYKA-ITEM    PIC X(16)   OCCURS  99 TIMES.
              03  K02-TABOOCOB.
                04  K02-TABOO.
                  05  K02-VALUE   PIC X(62).
                04  K02-TABOO-COUNT   PIC S9(9)   BINARY.
                04  K02-TABOO-ITEM    PIC X(62)   OCCURS  40 TIMES.
              03  K02-SYORIMEI.
                04  K02-SYORIMEI-STYLE   PIC X(20).
                04  K02-SYORIMEI-VALUE   PIC X(12).
              03  K02-SCROLLEDWINDOW1.
                04  K02-RRKLIST.
                  05  K02-COUNT   PIC S9(9)   BINARY.
                  05  K02-ROW     PIC S9(9)   BINARY.
                  05  K02-ROWATTR PIC S9(9)   BINARY.
                  05  K02-ITEM        OCCURS  150 TIMES.
                    06  K02-NO      PIC X(4).
                    06  K02-RRKYMD  PIC X(13).
                    06  K02-RRKSRYKA  PIC X(2).
                    06  K02-RRKHKNCOMBI  PIC X(04).
                  05  K02-SELECT  PIC X       OCCURS  150 TIMES.
              03  K02-JIKANMSG.
                04  K02-JIKANMSG-STYLE   PIC X(20).
                04  K02-JIKANMSG-VALUE   PIC X(20).
              03  K02-B04S.
                04  K02-B04S-STATE   PIC S9(9)   BINARY.
              03  K02-B05S.
                04  K02-B05S-STATE   PIC S9(9)   BINARY.
              03  K02-B07S.
                04  K02-B07S-STATE   PIC S9(9)   BINARY.
              03  K02-B08S.
                04  K02-B08S-STATE   PIC S9(9)   BINARY.
              03  K02-B09S.
                04  K02-B09S-STATE   PIC S9(9)   BINARY.
              03  K02-B06S.
                04  K02-B06S-LABEL   PIC X(10).
              03  K02-SCROLLEDWINDOW2.
                04  K02-BYOMEILST.
                  05  K02-BYOMEI-COUNT   PIC S9(9)   BINARY.
                  05  K02-BYOMEI-ROW PIC S9(9)   BINARY.
                  05  K02-BYOMEI-ROWATTR PIC S9(9)   BINARY.
                  05  K02-ITEM        OCCURS  100 TIMES.
                    06  K02-BYOMEIKA     PIC X(02).
                    06  K02-BYOMEI       PIC X(22).
                  05  K02-BYOMEI-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  K02-LASTMID.
                04  K02-VALUE   PIC X(20).
              03  K02-KOHMSG.
                04  K02-KOHMSG-STYLE   PIC X(20).
                04  K02-KOHMSG-VALUE   PIC X(50).
              03  K02-GRPMEI.
                04  K02-GRPMEI-STYLE   PIC X(20).
                04  K02-GRPMEI-VALUE   PIC X(10).
              03  K02-B12.
                04  K02-B12-STATE   PIC S9(9)   BINARY.
              03  K02-B12S.
                04  K02-B12S-STATE   PIC S9(9)   BINARY.
              03  K02-BC10.
                04  K02-BC10-STYLE  PIC X(20).
                04  K02-BC10-LABEL  PIC X(8).
              03  K02-SCROLLEDWINDOW3.
                04  K02-PANDATABLE1.
                  05  K02-PANDATABLE1-STATE   PIC S9(9)   BINARY.
                  05  K02-PANDATABLE1-STYLE   PIC X(20).
                  05  K02-PANDATABLE1-TROW    PIC S9(9)   BINARY.
                  05  K02-PANDATABLE1-TROWATTR    PIC S9(9)   BINARY.
                  05  K02-PANDATABLE1-TCOLUMN PIC S9(9)   BINARY.
                  05  K02-PANDATABLE1-TVALUE  PIC X(256).
                  05  K02-PANDATABLE1-ROWDATA     OCCURS  400 TIMES.
                    06  K02-SRYKBN-G.
                      07  K02-SRYKBN  PIC X(2).
                      07  K02-SRYKBN-FGCOLOR PIC X(20).
                      07  K02-SRYKBN-BGCOLOR PIC X(20).
                    06  K02-INPUTCD-G.
                      07  K02-INPUTCD    PIC X(54).
                      07  K02-INPUTCD-FGCOLOR PIC X(20).
                      07  K02-INPUTCD-BGCOLOR PIC X(20).
                    06  K02-MEISYO-G.
      *?????          07  K02-MEISYO    PIC X(80).
                      07  K02-MEISYO    PIC X(140).
                      07  K02-MEISYO-FGCOLOR PIC X(20).
                      07  K02-MEISYO-BGCOLOR PIC X(20).
                    06  K02-SURYO-G.
                      07  K02-SURYO    PIC X(60).
                      07  K02-SURYO-FGCOLOR PIC X(20).
                      07  K02-SURYO-BGCOLOR PIC X(20).
              03  K02-DRCDCOMB.
                04  K02-DRCD.
                  05  K02-VALUE   PIC X(29).
                04  K02-DRCD-COUNT   PIC S9(9)   BINARY.
                04  K02-DRCD-ITEM
                        PIC X(29)   OCCURS  99 TIMES.
              03  K02-MODE2.
                04  K02-MODE2-STYLE   PIC X(20).
                04  K02-MODE2-VALUE   PIC X(10).
              03  K02-MODE3.
                04  K02-MODE3-STYLE   PIC X(20).
                04  K02-MODE3-VALUE   PIC X(10).

