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
        02  S02.
              03  S02-B04.
                04  S02-B04-STATE    PIC S9(9)   BINARY.
              03  S02-B04S.
                04  S02-B04S-STATE   PIC S9(9)   BINARY.
              03  S02-B03S.
                04  S02-B03S-STATE   PIC S9(9)   BINARY.
                04  S02-B03S-LABEL   PIC X(10).
              03  S02-B06.
                04  S02-B06-STATE      PIC S9(9)   BINARY.
                04  S02-B06-LABEL      PIC X(10).
              03  S02-B07.
                04  S02-B07-STATE      PIC S9(9)   BINARY.
                04  S02-B07-LABEL      PIC X(10).
              03  S02-B08.
                04  S02-B08-LABEL      PIC X(10).
              03  S02-B11S.
                04  S02-B11S-LABEL      PIC X(10).
              03  S02-B12.
                04  S02-B12-STATE    PIC S9(9)   BINARY.
              03  S02-B12S.
                04  S02-B12S-LABEL      PIC X(10).
              03  S02-B12C.
                04  S02-B12C-STATE     PIC S9(9)   BINARY.
                04  S02-B12C-STYLE     PIC X(20).
                04  S02-B12C-LABEL     PIC X(20).
              03  S02-LBLMSG-G.
                04  S02-LBLMSG-STYLE   PIC X(20).
                04  S02-LBLMSG         PIC X(60).
              03  S02-LBLMSG2-G.
                04  S02-LBLMSG2-STYLE  PIC X(20).
                04  S02-LBLMSG2        PIC X(60).
              03  S02-LBLMSG3-G.
                04  S02-LBLMSG3-STYLE  PIC X(20).
                04  S02-LBLMSG3        PIC X(40).
              03  S02-LBLMSG4-G.
                04  S02-LBLMSG4-STYLE  PIC X(20).
                04  S02-LBLMSG4        PIC X(100).
              03  S02-LBLMSG5-G.
                04  S02-LBLMSG5-STYLE  PIC X(20).
                04  S02-LBLMSG5        PIC X(18).
              03  S02-LBLMSG6-G.
                04  S02-LBLMSG6-STYLE  PIC X(20).
                04  S02-LBLMSG6        PIC X(28).
              03  S02-LBLMSG7-G.
                04  S02-LBLMSG7-STYLE  PIC X(20).
                04  S02-LBLMSG7        PIC X(20).
              03  S02-LBLSRYYMG        PIC X(50).
              03  S02-LBLSRYYMN        PIC X(50).
              03  S02-PTNUM-STATE
                                PIC S9(9)   BINARY.
              03  S02-PTNUM     PIC X(20).
              03  S02-KANANAME-G.
                04  S02-KANANAME-STYLE   PIC X(20).
                04  S02-KANANAME   PIC X(50).
              03  S02-NAME-G.
                04  S02-NAME-STYLE   PIC X(20).
                04  S02-NAME   PIC X(50).
              03  S02-BIRTHDAY.
                04  S02-VALUE   PIC X(9).
              03  S02-SEX.
                04  S02-VALUE   PIC X(2).
              03  S02-SELNUM    PIC S9(4).
              03  S02-GAI-MISYU PIC S9(9).
              03  S02-NYU-MISYU PIC S9(9).
              03  S02-GAI-TUKISKY PIC S9(9).
              03  S02-NYU-TUKISKY PIC S9(9).
              03  S02-SRYYM-G.
                04  S02-SRYYM-STATE        PIC S9(9)   BINARY.
                04  S02-SRYYM              PIC X(10).
              03  S02-SCROLLEDWINDOW1.
                04  S02-SYULIST.
                  05  S02-SYULIST-COUNT    PIC S9(9)   BINARY.
                  05  S02-SYULIST-ROW      PIC S9(9)   BINARY.
                  05  S02-SYULIST-ROWATTR  PIC S9(9)   BINARY.
                  05  S02-SYULIST-ITEM     OCCURS  500 TIMES.
                    06  S02-TNUM        PIC X(5).
                    06  S02-TDENPNUM    PIC X(10).
                    06  S02-TSRYKA      PIC X(6).
                    06  S02-TNYUGAI     PIC X(4).
                    06  S02-THKN        PIC X(50).
                    06  S02-TFTNRATE    PIC X(5).
                    06  S02-TSKYSTYMD   PIC X(11).
                    06  S02-TSRYYMD     PIC X(15).
                    06  S02-TSKYMONEY   PIC X(11).
                    06  S02-TNYUKINMONEY   PIC X(11).
                    06  S02-TMISYUMONEY PIC X(11).
                    06  S02-TDENPJTI    PIC X(16).
                  05  S02-SYULIST-SELECT   PIC X
                                           OCCURS  500 TIMES.
              03  S02-CMB-SRYKA.
                04  S02-SRYKA         PIC X(21).
                04  S02-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  S02-SRYKALST      PIC X(21)   OCCURS  99 TIMES.
              03  S02-CMB-NYUGAIKBN.
                04  S02-NYUGAIKBN.
                  05  S02-VALUE   PIC X(8).
                04  S02-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  S02-NYUGAIKBN-ITEM    PIC X(8)    OCCURS  3 TIMES.
              03  S02-CMB-MODE.
                04  S02-MODE              PIC X(22).
                04  S02-MODE-COUNT        PIC S9(9)   BINARY.
                04  S02-MODE-ITEM         PIC X(22)   OCCURS  5 TIMES.
              03  S02-CMB-SORTKBN.
                04  S02-SORTKBN           PIC X(22).
                04  S02-SORTKBN-COUNT     PIC S9(9)   BINARY.
                04  S02-SORTKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
              03  S02-CHOSEI1IN PIC X(10).
              03  S02-CHOSEI1IN-STATE
                                PIC S9(9)   BINARY.
              03  S02-CHOSEI2IN PIC X(10).
              03  S02-CHOSEI2IN-STATE
                                PIC S9(9)   BINARY.
              03  S02-SKYMONEYIN-STYLE
                                PIC X(20).
              03  S02-SKYMONEYIN
                                PIC X(10).
              03  S02-SKYMONEYIN-STATE
                                PIC S9(9)   BINARY.
              03  S02-NYUKINGK2 PIC X(10).
              03  S02-NYUKINGK2-STATE
                                PIC S9(9)   BINARY.
              03  S02-NYUKINYMD.
                04  S02-VALUE   PIC X(10).
              03  S02-NYUKINYMD-STATE
                                PIC S9(9)   BINARY.
              03  S02-CMB-NYUKINHOUHOU.
                04  S02-NYUKINHOUHOU PIC X(33).
                04  S02-NYUKINHOUHOU-STATE
                                PIC S9(9)   BINARY.
                04  S02-NYUKINHOUHOU-COUNT   PIC S9(9)   BINARY.
                04  S02-NYUKINHOUHOULST      PIC X(33) OCCURS 50 TIMES.
                04  S02-NYUKINHOUHOULST-STATE
                                PIC S9(9)   BINARY.
              03  S02-CMB-SKYPRT.
                04  S02-SKYPRT PIC X(24).
                04  S02-SKYPRT-STATE PIC S9(9)   BINARY.
                04  S02-SKYPRT-COUNT   PIC S9(9)   BINARY.
                04  S02-SKYPRTLST      PIC X(24) OCCURS 5 TIMES.
                04  S02-SKYPRTLST-STATE PIC S9(9)   BINARY.
              03  S02-CMB-SRYPRT.
                04  S02-SRYPRT PIC X(24).
                04  S02-SRYPRT-STATE PIC S9(9)   BINARY.
                04  S02-SRYPRT-COUNT   PIC S9(9)   BINARY.
                04  S02-SRYPRTLST      PIC X(24) OCCURS 5 TIMES.
                04  S02-SRYPRTLST-STATE PIC S9(9)   BINARY.
              03  S02-RIREKIIN PIC S9(2).
              03  S02-SCROLLEDWINDOW2.
                04  S02-MEILIST.
                  05  S02-MEILIST-COUNT       PIC S9(9)   BINARY.
                  05  S02-MEILIST-ROW         PIC S9(9)   BINARY.
                  05  S02-MEILIST-ROWATTR     PIC S9(9)   BINARY.
                  05  S02-MEILIST-ITEM        OCCURS  99 TIMES.
                      06  S02-TMEDANUM       PIC X(4).
                      06  S02-TMSKYMONY      PIC X(12).
                      06  S02-TMNYUKINMONEY  PIC X(12).
                      06  S02-TMSYORIYMD     PIC X(11).
                      06  S02-TMJTI          PIC X(12).
                      06  S02-TMNYKN-NAME    PIC X(10).
                  05  S02-MEILIST-SELECT  PIC X       OCCURS  99 TIMES.
