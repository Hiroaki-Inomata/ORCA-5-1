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
        02  K021.
      **** 02  K021-FIXED10.
              03  K021-SELNUM.
                04  K021-VALUE  PIC X(3).
              03  K021-TOUMEI.
                04  K021-VALUE  PIC X(100).
              03  K021-KNKMEI.
                04  K021-VALUE  PIC X(100).
              03  K021-SCROLLEDWINDOW3.
                04  K021-SAYOKIJYO.
                  05  K021-VALUE  PIC X(2000).
              03  K021-SCROLLEDWINDOW2.
                04  K021-SYOJYOU.
                  05  K021-VALUE  PIC X(2000).
              03  K021-SCROLLEDWINDOW1.
                04  K021-MEDLIST.
                  05  K021-COUNT  PIC S9(9)   BINARY.
                  05  K021-ROW     PIC S9(9)   BINARY.
                  05  K021-ROWATTR PIC S9(9)   BINARY.
                  05  K021-ITEM       OCCURS  30 TIMES.
                    06  K021-TSEQNO  PIC X(3).
                    06  K021-TTOUMEI PIC X(40).
                    06  K021-TKNKMEI PIC X(40).
                    06  K021-TTOUYMD PIC X(9).
                    06  K021-TSYOJYOUCD PIC X(7).
                  05  K021-SELECT PIC X       OCCURS  30 TIMES.
              03  K021-B12.
                04  K021-B12-STATE  PIC S9(9)   BINARY.
