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
        02  W96.
      *     02  W96-FIXED1.
              03  W96-LBLJMASCRIPTS-STYLE
                                       PIC X(20).
              03  W96-CRON-G.
                04  W96-CRON-OCC       OCCURS  10.
                  05  W96-NAME-STATE   PIC S9(9)   BINARY.
                  05  W96-NAME         PIC X(30).
                  05  W96-SCRIPT-STATE PIC S9(9)   BINARY.
                  05  W96-SCRIPT       PIC X(100).
                  05  W96-CMB-MON.
                    06  W96-MON        PIC X(2).
                    06  W96-MON-COUNT  PIC S9(9)   BINARY.
                    06  W96-MON-ITEM   PIC X(2)    OCCURS  13 TIMES.
                  05  W96-CMB-DOM.
                    06  W96-DOM        PIC X(2).
                    06  W96-DOM-COUNT  PIC S9(9)   BINARY.
                    06  W96-DOM-ITEM   PIC X(2)    OCCURS  32 TIMES.
                  05  W96-CMD-DOW.
                    06  W96-DOW        PIC X(10).
                    06  W96-DOW-COUNT  PIC S9(9)   BINARY.
                    06  W96-DOW-ITEM   PIC X(10)   OCCURS  8 TIMES.
                  05  W96-CMB-HOUR.
                    06  W96-HOUR       PIC X(2).
                    06  W96-HOUR-COUNT PIC S9(9)   BINARY.
                    06  W96-HOUR-ITEM  PIC X(2)    OCCURS  25 TIMES.
                  05  W96-EXEC-LBL     PIC X(10).
                  05  W96-EXEC         PIC X(01).
