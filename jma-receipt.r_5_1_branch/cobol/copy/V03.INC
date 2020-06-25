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
        02  V03.
      *     02  V03-FIXED6.
              03  V03-B06-STATE            PIC S9(9)   BINARY.
              03  V03-SELNUM               PIC S9(3).
              03  V03-CMB-PREFNUM.
                04  V03-PREFNUM            PIC X(23).
                04  V03-PREFNUM-COUNT      PIC S9(9)   BINARY.
                04  V03-PREFNUMLST         PIC X(23)   OCCURS  50 TIMES.
              03  V03-CITYNUM.
                04  V03-VALUE              PIC X(3).
              03  V03-KOHNUM.
                04  V03-VALUE              PIC X(3).
              03  V03-SEIDONAME.
                04  V03-VALUE              PIC X(100).
              03  V03-CMB-PTKBN.
                04  V03-PTKBN              PIC X(20).
                04  V03-PTKBN-COUNT        PIC S9(9)   BINARY.
                04  V03-PTKBNLST           PIC X(20)   OCCURS  5 TIMES.
              03  V03-CMB-FTNKBN.
                04  V03-FTNKBN             PIC X(40).
                04  V03-FTNKBN-COUNT       PIC S9(9)   BINARY.
                04  V03-FTNKBNLST          PIC X(40)   OCCURS  5 TIMES.
              03  V03-CMB-RECESKYKBN.
                04  V03-RECESKYKBN         PIC X(40).
                04  V03-RECESKYKBN-COUNT   PIC S9(9)   BINARY.
                04  V03-RECESKYKBNLST      PIC X(40)   OCCURS  10 TIMES.
              03  V03-CMB-NYUSKJRYYKBN.
                04  V03-NYUSKJRYYKBN       PIC X(40).
                04  V03-NYUSKJRYYKBN-COUNT PIC S9(9)   BINARY.
                04  V03-NYUSKJRYYKBNLST    PIC X(40)   OCCURS  10 TIMES.
              03  V03-YUKOSTYMD.
                04  V03-VALUE              PIC X(10).
              03  V03-YUKOEDYMD.
                04  V03-VALUE              PIC X(10).
              03  V03-SCROLLEDWINDOW28.
                04  V03-KOHPLUSLST.
                  05  V03-KOHPLUSLST-COUNT PIC S9(9)   BINARY.
                  05  V03-KOHPLUSLST-ROW     PIC S9(9)   BINARY.
                  05  V03-KOHPLUSLST-ROWATTR PIC S9(9)   BINARY.
                  05  V03-KOHPLUSLST-ITEM  OCCURS  500 TIMES.
                    06  V03-TYUKO          PIC X(2).
                    06  V03-TSELNUM        PIC X(3).
                    06  V03-TPREFNUM       PIC X(10).
                    06  V03-TCITYNUM       PIC X(3).
                    06  V03-TKOHNUM        PIC X(3).
                    06  V03-TSEIDONAME     PIC X(100).
                    06  V03-TPTKBN         PIC X(6).
                    06  V03-TFTNKBN        PIC X(4).
                    06  V03-TYUKOSTYMD     PIC X(9).
                    06  V03-TYUKOEDYMD     PIC X(9).
                  05  V03-KOHPLUSLST-SELECT
                                           PIC X   OCCURS  500 TIMES.
