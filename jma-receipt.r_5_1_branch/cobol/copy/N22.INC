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
            02  N22.
              03  N22-FIXED17.
                05  N22-DURATION       PIC S9(09) BINARY.
                05  N22-KENCD          PIC X(9).
                05  N22-KENNAME        PIC X(100).
                05  N22-SCROLLEDWINDOW1.
                  07  N22-SCR1-HPOS    PIC S9(9)   BINARY.
                  07  N22-SCR1-VPOS    PIC S9(9)   BINARY.
                  07  N22-KEKKALST.
                    09  N22-COUNT      PIC S9(9)   BINARY.
                    09  N22-ROW        PIC S9(9)   BINARY.
                    09  N22-ROWATTR    PIC S9(9)   BINARY.
                    09  N22-ITEM                   OCCURS  40 TIMES.
                      11  N22-TNO      PIC X(5).
                      11  N22-TYJCD    PIC X(12).
                      11  N22-TGAZO    PIC X(5).
                      11  N22-TMAKER   PIC X(50).
                      11  N22-TSELLER  PIC X(50).
                    09  N22-SELECT     PIC X       OCCURS  40 TIMES.
                05  N22-SELNUM         PIC S9(3).
                05  N22-MESSAGE-STYLE  PIC X(20).
                05  N22-MESSAGE        PIC X(100).
                05  N22-GAZOKBNCOMBO.
                  07  N22-GAZOKBN      PIC X(14).
                  07  N22-GAZOKBN-COUNT
                                       PIC S9(9)   BINARY.
                  07  N22-GAZOKBNLST   PIC X(14)   OCCURS  4 TIMES.
                05  N22-COPYKBNCOMBO.
                  07  N22-COPYKBN      PIC X(14).
                  07  N22-COPYKBN-COUNT
                                       PIC S9(9)   BINARY.
                  07  N22-COPYKBNLST   PIC X(14)   OCCURS  3 TIMES.
                05  N22-GAZO           PIC X(36)   OCCURS  3 TIMES.
                05  N22-GAZOLABEL      PIC X(14)   OCCURS  3 TIMES.
                05  N22-SCROLLEDWINDOW2.
                  07  N22-SCR2-HPOS    PIC S9(9)   BINARY.
                  07  N22-SCR2-VPOS    PIC S9(9)   BINARY.
                  07  N22-EFFECT       PIC X(2000).
                05  N22-SCROLLEDWINDOW3.
                  07  N22-SCR3-HPOS    PIC S9(9)   BINARY.
                  07  N22-SCR3-VPOS    PIC S9(9)   BINARY.
                  07  N22-COLOR        PIC X(360).
                05  N22-SCROLLEDWINDOW4.
                  07  N22-SCR4-HPOS    PIC S9(9)   BINARY.
                  07  N22-SCR4-VPOS    PIC S9(9)   BINARY.
                  07  N22-SHAPE        PIC X(360).
                05  N22-SCROLLEDWINDOW5.
                  07  N22-SCR5-HPOS    PIC S9(9)   BINARY.
                  07  N22-SCR5-VPOS    PIC S9(9)   BINARY.
                  07  N22-MARK         PIC X(360).
                05  N22-SCROLLEDWINDOW6.
                  07  N22-SCR6-HPOS    PIC S9(9)   BINARY.
                  07  N22-SCR6-VPOS    PIC S9(9)   BINARY.
                  07  N22-CAUTION      PIC X(4000).
