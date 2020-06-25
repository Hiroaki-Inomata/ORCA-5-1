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
         02  P97.
  *******  02  P97-FIXED1.
              03  P97-SELNO.
                04  P97-VALUE   PIC S9(4).
              03  P97-NAMEKEN.
                04  P97-VALUE   PIC X(30).
              03  P97-KBIRTHDAY.
                04  P97-VALUE   PIC X(10).
              03  P97-KSEXCOMB.
                04  P97-KSEX.
                  05  P97-VALUE   PIC X(6).
                04  P97-KSEX-COUNT   PIC S9(9)   BINARY.
                04  P97-KSEX-ITEM    PIC X(6)    OCCURS  3 TIMES.
              03  P97-NYUGAIKBNCOMB.
                04  P97-KNYUGAIKBN.
                  05  P97-VALUE   PIC X(10).
                04  P97-KNYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  P97-KNYUGAIKBN-ITEM    PIC X(10)   OCCURS  3 TIMES.
              03  P97-SCROLLEDWINDOW1.
                04  P97-LIST.
                  05  P97-COUNT   PIC S9(9)   BINARY.
                  05  P97-ROW     PIC S9(9)   BINARY.
                  05  P97-ROWATTR  PIC S9(9)   BINARY.
                  05  P97-ITEM        OCCURS  100 TIMES.
                    06  P97-NO      PIC X(4).
                    06  P97-PTNUM   PIC X(20).
                    06  P97-NAME    PIC X(50).
                    06  P97-SEX     PIC X(2).
                    06  P97-BIRTH   PIC X(10).
                    06  P97-NENREI  PIC X(10).
                    06  P97-RRKYMD  PIC X(10).
                    06  P97-SRYKA   PIC X(4).
                    06  P97-HKNCOMBI  PIC X(6).
                    06  P97-HONKZKKBN PIC X(2).
                    06  P97-BRMNUM  PIC X(10).
                  05  P97-SELECT  PIC X       OCCURS  100 TIMES.
              03  P97-NEXT.
                04  P97-NEXT-STYLE   PIC X(20).
                04  P97-NEXT-VALUE   PIC X(20).
