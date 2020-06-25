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
        02  K01.
      ****  02  K01-FIXED2.
              03  K01-PTNUM.
                04  K01-VALUE   PIC X(20).
              03  K01-NAME.
                04  K01-VALUE   PIC X(50).
              03  K01-CALENDAR1.
                04  K01-YEAR    PIC S9(9)   BINARY.
                04  K01-MONTH   PIC S9(9)   BINARY.
                04  K01-DAY PIC S9(9)   BINARY.
              03  K01-SRYKACOMBO.
                04  K01-SRYKA.
                  05  K01-VALUE   PIC X(21).
                04  K01-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  K01-SRYKA-ITEM    PIC X(21)   OCCURS  99 TIMES.
              03  K01-SRYYMD.
                04  K01-VALUE   PIC X(10).
              03  K01-ZENKAICOMB.
                04  K01-ZENKAI.
                  05  K01-VALUE   PIC X(20).
                04  K01-ZENKAI-COUNT   PIC S9(9)   BINARY.
                04  K01-ZENKAI-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  K01-DRNAMECOMBO.
                04  K01-DRNAME.
                  05  K01-VALUE   PIC X(29).
                04  K01-DRNAME-COUNT   PIC S9(9)   BINARY.
                04  K01-DRNAME-ITEM    PIC X(29)   OCCURS  99 TIMES.
              03  K01-BUNSANCOMB.
                04  K01-BUNSAN.
                  05  K01-VALUE   PIC X(20).
                04  K01-BUNSAN-COUNT   PIC S9(9)   BINARY.
                04  K01-BUNSAN-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  K01-INGAIKBNCOMB.
                04  K01-INGAISHOHOKBN.
                  05  K01-VALUE   PIC X(20).
                04  K01-INGAI-COUNT   PIC S9(9)   BINARY.
                04  K01-INGAI-ITEM    PIC X(20)   OCCURS  5 TIMES.
              03  K01-HKNKUMI.
                04  K01-VALUE   PIC X(4).
              03  K01-SCROLLEDWINDOW3.
                04  K01-HKNKUMILST.
                  05  K01-HKNKUM-COUNT   PIC S9(9)   BINARY.
                  05  K01-HKNKUM-ROW    PIC S9(9)   BINARY.
                  05  K01-HKNKUM-ROWATTR PIC S9(9)   BINARY.
                  05  K01-ITEM        OCCURS  100 TIMES.
                    06  K01-THKNCOMBINUM  PIC X(5).
                    06  K01-THKNCOMBIMEI  PIC X(100).
                    06  K01-TTEKSTYMD     PIC X(10).
                    06  K01-TTEKEDYMD     PIC X(10).
                    06  K01-TFTNRATE      PIC X(5).
                  05  K01-HKNKUM-SELECT  PIC X       OCCURS  100 TIMES.
              03  K01-SYORIMEI.
                04  K01-SYORIMEI-STYLE   PIC X(20).
                04  K01-SYORIMEI-VALUE   PIC X(12).
