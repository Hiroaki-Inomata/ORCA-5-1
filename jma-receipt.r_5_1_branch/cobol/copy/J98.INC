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
        02  J98.
      ***  02  J98-FIXED32.
              03  J98-INPUT.
                04  J98-VALUE   PIC X(22).
              03  J98-SENTAKU.
                04  J98-VALUE   PIC X(30).
              03  J98-SELNUM    OCCURS  10.
                04  J98-VALUE   PIC X(3).
              03  J98-SCROLLEDWINDOW26.
                04  J98-LIST.
                  05  J98-COUNT   PIC S9(9)   BINARY.
                  05  J98-ROW     PIC S9(9)   BINARY.
                  05  J98-ROWATTR PIC S9(9)   BINARY.
                  05  J98-ITEM        OCCURS  200 TIMES.
                    06  J98-NO      PIC X(3).
                    06  J98-NAME    PIC X(70).
                    06  J98-TANI    PIC X(6).
                    06  J98-KISOTEN  PIC X(11).
                    06  J98-SETCD    PIC X(10).
                    06  J98-SRYKBN   PIC X(02).
                  05  J98-SELECT  PIC X       OCCURS  200 TIMES.
              03  J98-JIINCOB.
                04  J98-JIIN.
                  05  J98-VALUE   PIC X(16).
                04  J98-JIIN-COUNT   PIC S9(9)   BINARY.
                04  J98-JIIN-ITEM    PIC X(16)   OCCURS  10 TIMES.
              03  J98-USERCOB.
                04  J98-USER.
                  05  J98-VALUE   PIC X(10).
                04  J98-USER-COUNT   PIC S9(9)   BINARY.
                04  J98-USER-ITEM    PIC X(10)   OCCURS  10 TIMES.
              03  J98-NEXT.
                04  J98-NEXT-STYLE   PIC X(20).
                04  J98-NEXT-VALUE   PIC X(20).
