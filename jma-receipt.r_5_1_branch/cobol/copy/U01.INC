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
         02  U01.
            03  U01-FIXED1.
              04  U01-SZ-SELNUM PIC X(4).
              04  U01-SI-SELNUM PIC X(4).
              04  U01-SRYYMD  PIC X(30).
              04  U01-SCROLLEDWINDOW1.
                05  U01-LIST1.
                  06  U01-SI-COUNT   PIC S9(9)   BINARY.
                  06  U01-SI-ROW     PIC S9(9)   BINARY.
                  06  U01-SI-ROWATTR PIC S9(9)   BINARY.
                  06  U01-SI-ITEM        OCCURS  400 TIMES.
                    07  U01-SI-NO     PIC X(4).
                    07  U01-SI-PTNUM  PIC X(20).
                    07  U01-SI-NAME   PIC X(50).
                    07  U01-SI-NENREI PIC X(06).
                    07  U01-SI-NICKNAME  PIC X(50).
                    07  U01-SI-CONDITION  PIC X(6).
                    07  U01-SI-SRYKAMEI  PIC X(6).
                    07  U01-SI-SRYNAIYOMEI  PIC X(8).
                    07  U01-SI-DRCD  PIC X(24).
                    07  U01-SI-HKNCOMBI  PIC X(50).
                    07  U01-SI-UKETIME  PIC X(8).
                    07  U01-SI-YYKTIME  PIC X(8).
                    07  U01-SI-SRFLG    PIC X(4).
                  06  U01-SI-SELECT  PIC X       OCCURS  400 TIMES.
              04  U01-SCROLLEDWINDOW2.
                05  U01-LIST2.
                  06  U01-SZ-COUNT   PIC S9(9)   BINARY.
                  06  U01-SZ-ROW     PIC S9(9)   BINARY.
                  06  U01-SZ-ROWATTR PIC S9(9)   BINARY.
                  06  U01-SZ-ITEM        OCCURS  400 TIMES.
                    07  U01-SZ-NO     PIC X(4).
                    07  U01-SZ-PTNUM  PIC X(20).
                    07  U01-SZ-NAME   PIC X(50).
                    07  U01-SZ-NENREI PIC X(06).
                    07  U01-SZ-NICKNAME  PIC X(50).
                    07  U01-SZ-CONDITION  PIC X(6).
                    07  U01-SZ-SRYKAMEI  PIC X(6).
                    07  U01-SZ-SRYNAIYOMEI  PIC X(8).
                    07  U01-SZ-DRCD  PIC X(24).
                    07  U01-SZ-HKNCOMBI  PIC X(50).
                    07  U01-SZ-UKETIME  PIC X(8).
                    07  U01-SZ-YYKTIME  PIC X(8).
                    07  U01-SZ-SRFLG    PIC X(4).
                  06  U01-SZ-SELECT  PIC X       OCCURS  400 TIMES.
