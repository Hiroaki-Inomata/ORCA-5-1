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
        02  K09.
      ***  02  K09-FIXED1.
              03  K09-DOSEL.
                04  K09-VALUE   PIC X(40).
              03  K09-SELNUM.
                04  K09-VALUE   PIC S9(4).
              03  K09-SCROLLEDWINDOW2.
                04  K09-RRKLIST.
                  05  K09-RRKLIST-COUNT   PIC S9(9)   BINARY.
                  05  K09-RRKLIST-ROW     PIC S9(9)   BINARY.
                  05  K09-RRKLIST-ROWATTR PIC S9(9)   BINARY.
                  05  K09-ITEM        OCCURS  150 TIMES.
                    06  K09-NO      PIC X(4).
                    06  K09-RRKYMD  PIC X(16).
                    06  K09-RRKSRYKA  PIC X(2).
                    06  K09-RRKHKNCOMBI   PIC X(4).
                  05  K09-RRKLIST-SELECT  PIC X       OCCURS  150 TIMES.
              03  K09-SCROLLEDWINDOW3.
                04  K09-MEILIST.
                  05  K09-MEILIST-COUNT   PIC S9(9)   BINARY.
                  05  K09-MEILIST-ROW     PIC S9(9)   BINARY.
                  05  K09-MEILIST-ROWATTR PIC S9(9)   BINARY.
                  05  K09-ITEM        OCCURS  800 TIMES.
                    06  K09-MNO     PIC X(3).
                    06  K09-MEISAI  PIC X(90).
                  05  K09-MEILIST-SELECT  PIC X       OCCURS  800 TIMES.
              03  K09-NYUGAIKBN-COMB.
                04  K09-NYUGAIKBN.
                  05  K09-VALUE   PIC X(10).
                04  K09-NYUGAIKBN-COUNT   PIC S9(9)   BINARY.
                04  K09-NYUGAIKBN-ITEM
                        PIC X(10)   OCCURS  2 TIMES.
              03  K09-SRYKBN-COMB.
                04  K09-SRYKBN.
                  05  K09-VALUE   PIC X(23).
                04  K09-SRYKBN-COUNT   PIC S9(9)   BINARY.
                04  K09-SRYKBN-ITEM
                        PIC X(23)   OCCURS  20 TIMES.
              03  K09-GRPHOSPNUM-COMB.
                04  K09-GRPHOSPNUM.
                  05  K09-VALUE   PIC X(23).
                04  K09-GRPHOSPNUM-COUNT   PIC S9(9)   BINARY.
                04  K09-GRPHOSPNUM-ITEM
                        PIC X(23)   OCCURS  9 TIMES.
              03  K09-B03.
                04  K09-B03-STATE   PIC S9(9)   BINARY.
                04  K09-B03-LABEL   PIC X(20).
              03  K09-B04.
                04  K09-B04-STATE   PIC S9(9)   BINARY.
                04  K09-B04-LABEL   PIC X(20).
