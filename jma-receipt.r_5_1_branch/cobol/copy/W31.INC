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
        02  W31.
            03  W31-FIXED6.
              04  W31-B02-LABEL            PIC X(10).
              04  W31-B03-LABEL            PIC X(10).
              04  W31-B03-STATE            PIC S9(9)   BINARY.
              04  W31-STYUKYMD             PIC X(10).
              04  W31-EDYUKYMD             PIC X(10).
              04  W31-N1.
                05  W31-PAGENO  PIC S9(9)   BINARY.
                05  W31-FT01.
                  06  W31-CHK-MEDKBN-OCC       OCCURS  40.
                    07  W31-CHK-MEDKBN         PIC X(01).
                    07  W31-CHK-LBLMEDKBN      PIC X(60).
                    07  W31-CHK-MEDKBN-STATE   PIC S9(9)   BINARY.
                05  W31-NB01-STATE             PIC S9(9)   BINARY.
                05  W31-FT02.
                  06  W31-LBLADLINPUTINFO      PIC X(80).
                  06  W31-CMB-ADLINPUTKBN.
                    07  W31-ADLINPUTKBN        PIC X(22).
                    07  W31-ADLINPUTKBN-STATE  PIC S9(9)   BINARY.
                    07  W31-ADLINPUTKBN-CNT    PIC S9(9)   BINARY.
                    07  W31-ADLINPUTKBN-ITM    PIC X(22)   OCCURS  2.
                    07  W31-ADLINPUTKBN-ITM-STATE
                                               PIC S9(9)   BINARY.
                  06  W31-CMB-SETMEDKBN.
                    07  W31-SETMEDKBN          PIC X(32).
                    07  W31-SETMEDKBN-CNT      PIC S9(9)   BINARY.
                    07  W31-SETMEDKBN-ITM      PIC X(32)   OCCURS  2.
                05  W31-NB02-STATE             PIC S9(9)   BINARY.
