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
        02  X98.
     ***  02  X98-FIXED32.
              03  X98-B09-STATE PIC S9(9)   BINARY.
              03  X98-INPUT.
                04  X98-VALUE   PIC X(22).
              03  X98-SENTAKU.
                04  X98-VALUE   PIC X(30).
              03  X98-SELNUM     PIC X(03)   OCCURS  5.
              03  X98-SCROLLEDWINDOW26.
                04  X98-LIST.
                  05  X98-COUNT   PIC S9(9)   BINARY.
                  05  X98-ROW     PIC S9(9)   BINARY.
                  05  X98-ROWATTR PIC S9(9)   BINARY.
                  05  X98-ITEM        OCCURS  200 TIMES.
                    06  X98-NO      PIC X(3).
                    06  X98-NAME    PIC X(70).
                    06  X98-TANI    PIC X(6).
                    06  X98-KISOTEN  PIC X(10).
                    06  X98-SRYKBN   PIC X(02).
                  05  X98-SELECT  PIC X       OCCURS  200 TIMES.
              03  X98-JIINCOB.
                04  X98-JIIN.
                  05  X98-VALUE   PIC X(16).
                04  X98-JIIN-COUNT   PIC S9(9)   BINARY.
                04  X98-JIIN-ITEM    PIC X(16)   OCCURS  10 TIMES.
              03  X98-USERCOB.
                04  X98-USER.
                  05  X98-VALUE   PIC X(10).
                04  X98-USER-COUNT   PIC S9(9)   BINARY.
                04  X98-USER-ITEM    PIC X(10)   OCCURS  10 TIMES.
              03  X98-NEXT.
                04  X98-NEXT-STYLE   PIC X(20).
                04  X98-NEXT-VALUE   PIC X(20).
