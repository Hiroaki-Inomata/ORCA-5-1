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
        02  O02.
      *     02  O02-FIXED6.
              03  O02-DENPPRTYMD.
                04  O02-VALUE   PIC X(10).
              03  O02-RENNUM.
                04  O02-VALUE   PIC 9(3).
              03  O02-SRYYM.
                04  O02-VALUE   PIC X(7).
              03  O02-PTNUM.
                04  O02-VALUE   PIC X(20).
              03  O02-SAKKBN-COMBO.
                04  O02-SAKKBN  PIC X(16).
                04  O02-SAKKBN-STATE   PIC S9(9)   BINARY.
                04  O02-SAKKBN-COUNT   PIC S9(9)   BINARY.
                04  O02-SAKKBNLST      PIC X(16)   OCCURS  10 TIMES.
                04  O02-SAKKBN-COMBO-STATE     PIC S9(9)   BINARY.
              03  O02-NAME.
                04  O02-VALUE   PIC X(50).
              03  O02-SCROLLEDWINDOW1.
                04  O02-TEIKILST.
                  05  O02-COUNT       PIC S9(9)   BINARY.
                  05  O02-ROW         PIC S9(9)   BINARY.
                  05  O02-ROWATTR     PIC S9(9)   BINARY.
                  05  O02-ITEM        OCCURS  100 TIMES.
                    06  O02-TRENNUM  PIC X(3).
                    06  O02-TPTNUM   PIC X(20).
                    06  O02-TNAME    PIC X(50).
                    06  O02-TSRYYM   PIC X(7).
                    06  O02-TSAKKBN  PIC X(14).
                    06  O02-TSKYKIKAN  PIC X(21).
                    06  O02-TNYUINYMD  PIC X(09).
                    06  O02-TTAIINYMD  PIC X(09).
                  05  O02-SELECT  PIC X       OCCURS  100 TIMES.
