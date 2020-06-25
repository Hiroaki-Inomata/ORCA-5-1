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
        02  P02F.
      ******02  P02F-FIXED10.
              03  P02F-SELNUM.
                04  P02F-VALUE  PIC S9(3).
              03  P02F-KNJKEY.
                04  P02F-VALUE  PIC X(20).
              03  P02F-SCROLLEDWINDOW1.
                04  P02F-KNJLIST.
                  05  P02F-COUNT  PIC S9(9)   BINARY.
                  05  P02F-ROW    PIC S9(9)   BINARY.
                  05  P02F-ROWATTR  PIC S9(9)   BINARY.
                  05  P02F-ITEM       OCCURS  100 TIMES.
                    06  P02F-SEQNUM   PIC X(3).
                    06  P02F-PTNUM    PIC X(22).
                    06  P02F-NAME     PIC X(50).
                    06  P02F-BIRTHDAY PIC X(10).
                  05  P02F-SELECT PIC X       OCCURS  100 TIMES.
              03  P02F-MIDASI.
                04  P02F-MIDASI-STYLE  PIC X(20).
                04  P02F-MIDASI-VALUE  PIC X(24).
