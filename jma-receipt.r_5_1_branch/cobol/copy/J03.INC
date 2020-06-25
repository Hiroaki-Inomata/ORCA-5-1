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
        02  J03.
      ***  02  J03-FIXED19.
              03  J03-PTNUM.
                04  J03-VALUE   PIC X(20).
              03  J03-KANANAME.
                04  J03-VALUE   PIC X(30).
              03  J03-NAME.
                04  J03-VALUE   PIC X(30).
              03  J03-BIRTHDAY.
                04  J03-VALUE   PIC X(9).
              03  J03-SEX.
                04  J03-VALUE   PIC X(2).
              03  J03-SCROLLEDWINDOW2.
                04  J03-SRYACTCNT.
                  05  J03-SRYACTCNT-COUNT   PIC S9(9)   BINARY.
                  05  J03-SRYACTCNT-ROW       PIC S9(9)   BINARY.
                  05  J03-SRYACTCNT-ROWATTR   PIC S9(9)   BINARY.
                  05  J03-SRYACTCNT-ITEM        OCCURS  200 TIMES.
                    06  J03-SRYACTCNT-YM    PIC X(7).
                    06  J03-SRYACTCNT-NAME  PIC X(40).
                    06  J03-SRYACTCNT-CNT    PIC X(5).
                  05  J03-SRYACTCNT-SELECT  PIC X    OCCURS  200 TIMES.
              03  J03-SCROLLEDWINDOW1.
                04  J03-SRYACTYMD.
                  05  J03-SRYACTYMD-COUNT   PIC S9(9)   BINARY.
                  05  J03-SRYACTYMD-ROW       PIC S9(9)   BINARY.
                  05  J03-SRYACTYMD-ROWATTR   PIC S9(9)   BINARY.
                  05  J03-SRYACTYMD-ITEM        OCCURS  200 TIMES.
                    06  J03-SRYACTYMD-YMD  PIC X(10).
                    06  J03-SRYACTYMD-NAME  PIC X(40).
                  05  J03-SRYACTYMD-SELECT  PIC X   OCCURS  200 TIMES.
              03  J03-NYUGAIKBN-COMB.
                04  J03-NYUGAIKBN.
                  05  J03-VALUE   PIC X(10).
                04  J03-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  J03-NYUGAIKBN-ITEM    PIC X(10)   OCCURS  2 TIMES.
              03  J03-SRYKA-COMB.
                04  J03-SRYKA.
                  05  J03-VALUE   PIC X(20).
                04  J03-SRYKA-COUNT   PIC S9(9)   BINARY.
                04  J03-SRYKA-ITEM    PIC X(20)   OCCURS  100 TIMES.
              03  J03-HHKNCOMBI-COMB.
                04  J03-HHKNCOMBI.
                  05  J03-VALUE   PIC X(65).
                04  J03-HHKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  J03-HHKNCOMBI-ITEM    PIC X(65)   OCCURS  40 TIMES.
              03  J03-MIDASI.
                04  J03-MIDASI-STYLE   PIC X(20).
                04  J03-MIDASI-VALUE   PIC X(20).
              03  J03-TENSU.
                04  J03-VALUE   PIC X(10).
              03  J03-RSITENSU.
                04  J03-VALUE   PIC X(10).
              03  J03-JIHIKIN.
                04  J03-VALUE   PIC X(10).
              03  J03-SRYYM.
                04  J03-VALUE   PIC X(6).
              03  J03-NYUTENSU.
                04  J03-VALUE   PIC X(10).
              03  J03-MIDASIN.
                04  J03-VALUE   PIC X(10).
