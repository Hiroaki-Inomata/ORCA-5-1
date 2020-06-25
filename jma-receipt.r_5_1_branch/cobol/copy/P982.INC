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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  P982.
     ***   02  P982-FIXED32.
              03  P982-SELNUM.
                04  P982-VALUE  PIC X(3).
              03  P982-SCROLLEDWINDOW26.
                04  P982-LIST.
                  05  P982-COUNT  PIC S9(9)   BINARY.
                  05  P982-ROW    PIC S9(9)   BINARY.
                  05  P982-ROWATTR PIC S9(9)   BINARY.
                  05  P982-ITEM       OCCURS  100 TIMES.
                    06  P982-SEQNO    PIC X(3).
                    06  P982-POST     PIC X(7).
                    06  P982-LPUBCD   PIC X(5).
                    06  P982-TOWNNAME  PIC X(100).
                    06  P982-TOWNNAME2  PIC X(600).
                  05  P982-SELECT PIC X       OCCURS  100 TIMES.
              03  P982-NEXT.
                04  P982-NEXT-STYLE   PIC X(20).
                04  P982-NEXT-VALUE   PIC X(20).
              03  P982-B10.
                04  P982-B10-STATE   PIC S9(9)   BINARY.
              03  P982-B12.
                04  P982-B12-STATE   PIC S9(9)   BINARY.

