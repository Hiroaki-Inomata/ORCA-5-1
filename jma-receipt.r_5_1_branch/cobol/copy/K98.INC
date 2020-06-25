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
        02  K98.
      ***  02  K98-FIXED32.
              03  K98-INPUT.
                04  K98-VALUE   PIC X(22).
              03  K98-SENTAKU.
                04  K98-VALUE   PIC X(30).
              03  K98-SELNUM    OCCURS  10.
                04  K98-VALUE   PIC X(3).
              03  K98-SCROLLEDWINDOW26.
                04  K98-LIST.
                  05  K98-COUNT   PIC S9(9)   BINARY.
                  05  K98-ROW     PIC S9(9)   BINARY.
                  05  K98-ROWATTR PIC S9(9)   BINARY.
                  05  K98-ITEM        OCCURS  200 TIMES.
                    06  K98-NO      PIC X(3).
                    06  K98-NAME    PIC X(70).
                    06  K98-TANI    PIC X(6).
                    06  K98-KISOTEN  PIC X(11).
                    06  K98-SETCD    PIC X(10).
                    06  K98-SRYKBN   PIC X(02).
                  05  K98-SELECT  PIC X       OCCURS  200 TIMES.
              03  K98-JIINCOB.
                04  K98-JIIN.
                  05  K98-VALUE   PIC X(16).
                04  K98-JIIN-COUNT   PIC S9(9)   BINARY.
                04  K98-JIIN-ITEM    PIC X(16)   OCCURS  10 TIMES.
              03  K98-USERCOB.
                04  K98-USER.
                  05  K98-VALUE   PIC X(10).
                04  K98-USER-COUNT   PIC S9(9)   BINARY.
                04  K98-USER-ITEM    PIC X(10)   OCCURS  10 TIMES.
              03  K98-NEXT.
                04  K98-NEXT-STYLE   PIC X(20).
                04  K98-NEXT-VALUE   PIC X(20).
