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
        02  D05.
      *     02  D05-FIXED6.
              03  D05-B06S-STATE           PIC S9(9)   BINARY.
              03  D05-B07S-STATE           PIC S9(9)   BINARY.
              03  D05-SELNUM               PIC S9(5).
              03  D05-SRYYM                PIC X(7).
              03  D05-CMB-NYUGAIKBN.
                04  D05-NYUGAIKBN          PIC X(16).
                04  D05-NYUGAIKBN-COUNT    PIC S9(9)   BINARY.
                04  D05-NYUGAIKBN-ITEM     PIC X(16)   OCCURS  5 TIMES.
              03  D05-LBLMSG01             PIC X(220).
              03  D05-SW1.
                04  D05-SW1-HPOS           PIC S9(9)   BINARY.
                04  D05-SW1-VPOS           PIC S9(9)   BINARY.
                04  D05-REIGAILST.
                  05  D05-REIGAILST-COUNT  PIC S9(9)   BINARY.
                  05  D05-REIGAILST-ROW    PIC S9(9)   BINARY.
                  05  D05-REIGAILST-ROWATTR    PIC S9(9)   BINARY.
                  05  D05-REIGAILST-ITEM   OCCURS  100 TIMES.
                    06  D05-TNUM           PIC X(5).
                    06  D05-TPTNUM         PIC X(20).
                    06  D05-TNAME          PIC X(100).
                    06  D05-TCHKNUMNAME    PIC X(30).
                    06  D05-TCHKNAIYO      PIC X(222).
                    06  D05-TKIKAN         PIC X(10).
                  05  D05-REIGAILST-SELECT PIC X
                                           OCCURS  100 TIMES.
