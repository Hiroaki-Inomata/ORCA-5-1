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
        01  SC03.
            03  SC03-SRYYM              PIC X(16).
            03  SC03-ADRS               PIC X(100).
            03  SC03-HOSPNAME           PIC X(100).
            03  SC03-KAISETU            PIC X(60).
            03  SC03-NYUGAI-TBL         OCCURS  2 TIMES.
                05  SC03-SEIKYU-TBL         OCCURS  8 TIMES.
                    07  SC03-KENSU      PIC X(05).
                    07  SC03-NISSU      PIC X(05).
                    07  SC03-TENSU      PIC X(09).
                    07  SC03-SEIKYUGK   PIC X(11).
            03  SC03-GKENSU             PIC X(05).
            03  SC03-GNISSU             PIC X(05).
            03  SC03-GTENSU             PIC X(09).
            03  SC03-GSEIKYUGK          PIC X(11).
