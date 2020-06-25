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
          02  H99.
              03  H99-PTNUM.
                04  H99-VALUE   PIC X(20).
              03  H99-SRYYM.
                04  H99-VALUE   PIC X(8).
              03  H99-SRYKA.
                04  H99-SRYKA-STATE   PIC S9(9)   BINARY.
                04  H99-SRYKA-VALUE   PIC X(2).
              03  H99-SCROLLEDWINDOW1.
                04  H99-RECELST.
                  05  H99-COUNT   PIC S9(9)   BINARY.
                  05  H99-ROW             PIC S9(9)   BINARY.
                  05  H99-ROWATTR         PIC S9(9)   BINARY.
                  05  H99-ITEM        OCCURS  50 TIMES.
                    06  H99-TRENNUM       PIC X(2).
                    06  H99-TNYUGAIKBN    PIC X(6).
                    06  H99-TSRYKA        PIC X(10).
                    06  H99-TTEISYUTUSAKI PIC X(10).
                    06  H99-TRECESYUBETU  PIC X(44).
                    06  H99-THKNJANUM     PIC X(14).
                    06  H99-THOJOKBN      PIC X(8).
                    06  H99-TKIGO         PIC X(20).
                    06  H99-TRECEIPT-KBN  PIC X(10).
                    06  H99-TDISASTER     PIC X(2).
                    06  H99-TCREYMD       PIC X(9).
                  05  H99-SELECT  PIC X       OCCURS  50 TIMES.
              03  H99-SELNUM.
                04  H99-VALUE   PIC S9(2).
              03  H99-NAME.
                04  H99-NAME-STYLE   PIC X(20).
                04  H99-NAME-VALUE   PIC X(24).
              03  H99-SRYKANAME.
                04  H99-SRYKANAME-STYLE   PIC X(20).
                04  H99-SRYKANAME-VALUE   PIC X(18).
