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
        02  Z07.
      ***   02  Z07-FIXED1.
              03  Z07-OLDSRYCD.
                04  Z07-VALUE   PIC X(9).
              03  Z07-YUKOYMD.
                04  Z07-VALUE   PIC X(10).
              03  Z07-SRYCD.
                04  Z07-VALUE   PIC X(20).
              03  Z07-OLDNAME.
                04  Z07-VALUE   PIC X(100).
              03  Z07-NAME.
                04  Z07-VALUE   PIC X(100).
              03  Z07-INPUTCD01.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD02.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD03.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD04.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD05.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD06.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD07.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD08.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD09.
                04  Z07-VALUE   PIC X(20).
              03  Z07-INPUTCD10.
                04  Z07-VALUE   PIC X(20).
              03  Z07-CMB-CHKCHG.
                04  Z07-CHKCHG.
                  05  Z07-VALUE   PIC X(20).
                04  Z07-CHKCHG-COUNT   PIC S9(9)   BINARY.
                04  Z07-CHKCHG-ITEM
                        PIC X(20)   OCCURS  3 TIMES.
              03  Z07-CMB-YKJCHG.
                04  Z07-YKJCHG.
                  05  Z07-VALUE   PIC X(20).
                04  Z07-YKJCHG-COUNT   PIC S9(9)   BINARY.
                04  Z07-YKJCHG-ITEM
                        PIC X(20)   OCCURS  3 TIMES.
              03  Z07-CMB-PTKINKICHG.
                04  Z07-PTKINKICHG.
                  05  Z07-VALUE   PIC X(20).
                04  Z07-PTKINKICHG-COUNT   PIC S9(9)   BINARY.
                04  Z07-PTKINKICHG-ITEM
                        PIC X(20)   OCCURS  3 TIMES.
              03  Z07-SELNUM.
                04  Z07-VALUE   PIC S9(3).
              03  Z07-SCROLLEDWINDOW1.
                04  Z07-TENSU-LIST.
                  05  Z07-TENSU-COUNT   PIC S9(9)   BINARY.
                  05  Z07-TENSU-ROW     PIC S9(9)   BINARY.
                  05  Z07-TENSU-ROWATTR PIC S9(9)   BINARY.
                  05  Z07-ITEM        OCCURS  200 TIMES.
                    06  Z07-TNUM      PIC X(3).
                    06  Z07-TINPUTCD  PIC X(20).
                    06  Z07-TSRYCD  PIC X(9).
                    06  Z07-TNAME   PIC X(100).
                    06  Z07-TYUKOSTYMD  PIC X(10).
                    06  Z07-TYUKOEDYMD  PIC X(10).
                  05  Z07-TENSU-SELECT  PIC X       OCCURS  200 TIMES.
              03  Z07-NEXT-MSG.
                04  Z07-NEXT-MSG-STYLE   PIC X(20).
                04  Z07-NEXT-MSG-VALUE   PIC X(20).
