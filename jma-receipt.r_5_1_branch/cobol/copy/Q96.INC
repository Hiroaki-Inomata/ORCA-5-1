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
        02  Q96.
     ***  02  Q96-FIXED32.
              03  Q96-INPUT.
                04  Q96-VALUE   PIC X(22).
              03  Q96-SENTAKU.
                04  Q96-VALUE   PIC X(30).
              03  Q96-SELNUM     PIC X(03)   OCCURS  5.
              03  Q96-SCROLLEDWINDOW26.
                04  Q96-LIST.
                  05  Q96-COUNT   PIC S9(9)   BINARY.
                  05  Q96-ROW     PIC S9(9)   BINARY.
                  05  Q96-ROWATTR PIC S9(9)   BINARY.
                  05  Q96-ITEM        OCCURS  200 TIMES.
                    06  Q96-NO      PIC X(3).
                    06  Q96-NAME    PIC X(70).
                    06  Q96-TANI    PIC X(6).
                    06  Q96-KISOTEN  PIC X(10).
                  05  Q96-SELECT  PIC X       OCCURS  200 TIMES.
              03  Q96-JIINCOB.
                04  Q96-JIIN.
                  05  Q96-VALUE   PIC X(16).
                04  Q96-JIIN-COUNT   PIC S9(9)   BINARY.
                04  Q96-JIIN-ITEM    PIC X(16)   OCCURS  10 TIMES.
              03  Q96-USERCOB.
                04  Q96-USER.
                  05  Q96-VALUE   PIC X(10).
                04  Q96-USER-COUNT   PIC S9(9)   BINARY.
                04  Q96-USER-ITEM    PIC X(10)   OCCURS  10 TIMES.
              03  Q96-NEXT.
                04  Q96-NEXT-STYLE   PIC X(20).
                04  Q96-NEXT-VALUE   PIC X(20).
