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
        02  Z01.
            03  Z01-FIXED1.
              04  Z01-SRYCD.
                05  Z01-VALUE   PIC X(9).
              04  Z01-BS1.
                05  Z01-BS1VALUE   PIC X.
                05  Z01-BS1LABEL   PIC X(10).
              04  Z01-SCROLLEDWINDOW1.
                05  Z01-TENSU-LIST.
                  06  Z01-COUNT   PIC S9(9)   BINARY.
                  06  Z01-ROW     PIC S9(9)   BINARY.
                  06  Z01-ROWATTR PIC S9(9)   BINARY.
                  06  Z01-ITEM        OCCURS  30 TIMES.
                    07  Z01-TRENNUM  PIC X(3).
                    07  Z01-TYUKOSTYMD  PIC X(9).
                    07  Z01-TYUKOEDYMD  PIC X(9).
                  06  Z01-SELECT  PIC X       OCCURS  30 TIMES.
              04  Z01-SELNUM.
                05  Z01-VALUE   PIC X(3).
              04  Z01-YUKOSTYMD.
                05  Z01-VALUE   PIC X(10).
              04  Z01-YUKOEDYMD.
                05  Z01-VALUE   PIC X(10).
              04  Z01-NAME.
                05  Z01-VALUE   PIC X(100).
              03  Z01-B08.
                04  Z01-B08-STATE   PIC S9(9)   BINARY.
                04  Z01-B08-LABEL   PIC X(10).
              03  Z01-B10.
                04  Z01-B10-STATE   PIC S9(9)   BINARY.
                04  Z01-B10-LABEL   PIC X(10).
              04  Z01-MIDASHI.
                05  Z01-MIDASHI-STYLE  PIC X(20).
                05  Z01-MIDASHI-DATA   PIC X(80).
