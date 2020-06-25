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
        02  W28.
      *     02  W28-FIXED6.
              03  W28-SCROLLEDWINDOW1.
                04  W28-BTULST.
                  05  W28-BTULST-CNT       PIC S9(9)   BINARY.
                  05  W28-BTULST-ROW       PIC S9(9)   BINARY.
                  05  W28-BTULST-ROWATTR   PIC S9(9)   BINARY.
                  05  W28-BTULST-ITM       OCCURS  400 TIMES.
                    06  W28-TYUKO          PIC X(4).
                    06  W28-TSELNUM        PIC X(4).
                    06  W28-TBTUNUM        PIC X(6).
                    06  W28-TBTUNAME       PIC X(40).
                    06  W28-TKHNRYO        PIC X(60).
                    06  W28-TFTNNAME       PIC X(20).
                    06  W28-TTANKA         PIC X(16).
                    06  W28-TTAXNAME       PIC X(10).
                    06  W28-TSTYUKYMD      PIC X(9).
                    06  W28-TEDYUKYMD      PIC X(9).
                  05  W28-BTULST-SELECT    PIC X
                                           OCCURS  400 TIMES.
              03  W28-STSELNUM             PIC S9(3).
              03  W28-EDSELNUM             PIC S9(3).
              03  W28-STBTUNUM             PIC S9(2).
              03  W28-EDBTUNUM             PIC S9(2).
              03  W28-KJNYMD               PIC X(10).
              03  W28-CMB-SENTEI-FTNKBN.
                04  W28-SENTEI-FTNKBN      PIC X(22).
                04  W28-SENTEI-FTNKBN-CNT  PIC S9(9)   BINARY.
                04  W28-SENTEI-FTNKBN-ITM  PIC X(22)
                                           OCCURS  10 TIMES.
              03  W28-SENTEI-TANKA         PIC S9(4).
              03  W28-SENTEI-TANKAIP       PIC S9(4).
              03  W28-SENTEI-TANKAIP-STATE PIC S9(9)   BINARY.
              03  W28-SENTEI-TANKARJ       PIC S9(4).
              03  W28-SENTEI-TANKARJ-STATE PIC S9(9)   BINARY.
              03  W28-CMB-SENTEI-TAX.
                04  W28-SENTEI-TAX         PIC X(20).
                04  W28-SENTEI-TAX-CNT     PIC S9(9)
                                           BINARY.
                04  W28-SENTEI-TAX-ITM     PIC X(20)
                                           OCCURS  5 TIMES.
              03  W28-LBL-SENTEI-TANKA     PIC X(10).
              03  W28-LBL-SENTEI-EN        PIC X(2).
              03  W28-LBL-SENTEI-TANKAIP   PIC X(8).
              03  W28-LBL-SENTEI-ENIP      PIC X(2).
              03  W28-LBL-SENTEI-TANKARJ   PIC X(8).
              03  W28-LBL-SENTEI-ENRJ      PIC X(2).
