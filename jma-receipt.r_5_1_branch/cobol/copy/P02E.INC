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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  P02E.
      ******02  P02E-FIXED10.
              03  P02E-PTNUM.
                04  P02E-VALUE  PIC X(20).
              03  P02E-ADRS1.
                04  P02E-VALUE  PIC X(100).
              03  P02E-NAME.
                04  P02E-VALUE  PIC X(50).
              03  P02E-ADRS2.
                04  P02E-VALUE  PIC X(100).
              03  P02E-BIRTHDAY.
                04  P02E-VALUE  PIC X(09).
              03  P02E-NENREI.
                04  P02E-VALUE  PIC X(8).
              03  P02E-SEX.
                04  P02E-VALUE  PIC X(4).
              03  P02E-GRPHOSPNUM-COMB.
                04  P02E-GRPHOSPNUM.
                  05  P02E-VALUE  PIC X(23).
                04  P02E-GRPHOSPNUM-COUNT  PIC S9(9)   BINARY.
                04  P02E-GRPHOSPNUM-ITEM
                        PIC X(23)   OCCURS  9 TIMES.
              03  P02E-MIDASI.
                04  P02E-MIDASI-STYLE  PIC X(20).
                04  P02E-MIDASI-VALUE  PIC X(20).
              03  P02E-SCROLLEDWINDOW2.
                04  P02E-HPOS   PIC S9(9)   BINARY.
                04  P02E-VPOS   PIC S9(9)   BINARY.
                04  P02E-KOHLIST.
                  05  P02E-KOHLIST-COUNT  PIC S9(9)   BINARY.
                  05  P02E-ROW    PIC S9(9)   BINARY.
                  05  P02E-ITEM       OCCURS  10 TIMES.
                    06  P02E-TKOHMEI    PIC X(10).
                    06  P02E-TFTNJANUM  PIC X(10).
                    06  P02E-TJKYSNUM   PIC X(20).
                  05  P02E-KOHLIST-SELECT
                          PIC X       OCCURS  10 TIMES.
              03  P02E-SCROLLEDWINDOW1.
                04  P02E-HPOS   PIC S9(9)   BINARY.
                04  P02E-VPOS   PIC S9(9)   BINARY.
                04  P02E-HKNLIST.
                  05  P02E-HKNLIST-COUNT  PIC S9(9)   BINARY.
                  05  P02E-ROW    PIC S9(9)   BINARY.
                  05  P02E-ITEM       OCCURS  5 TIMES.
                    06  P02E-THKNMEI    PIC X(10).
                    06  P02E-THKNJANUM  PIC X(8).
                    06  P02E-TKIGO      PIC X(20).
                    06  P02E-TNUM       PIC X(20).
                  05  P02E-HKNLIST-SELECT
                          PIC X       OCCURS  5 TIMES.
              03  P02E-B03.
                04  P02E-B03-STATE  PIC S9(9)   BINARY.
                04  P02E-B03-LABEL  PIC X(20).
              03  P02E-B10.
                04  P02E-B10-STATE  PIC S9(9)   BINARY.
                04  P02E-B10-LABEL  PIC X(20).
