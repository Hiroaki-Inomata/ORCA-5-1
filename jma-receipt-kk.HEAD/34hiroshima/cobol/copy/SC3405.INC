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
        01  SC05.
            03  SC05-SRYYM                PIC X(16).
            03  SC05-HOSPCDN              PIC X(20).
            03  SC05-HOSPADRS             PIC X(100).
            03  SC05-HOSPNAME             PIC X(100).
            03  SC05-KAISETU              PIC X(40).
            03  SC05-POST3                PIC X(03).
            03  SC05-POST4                PIC X(04).
            03  SC05-SEIKYU-TBL           OCCURS  25.
              05  SC05-CITYNUM            PIC X(03).
              05  SC05-KENSU              PIC X(06).
              05  SC05-TENSU              PIC X(10).
            03  SC05-GOKEI-TBL.
              05  SC05-GKENSU             PIC X(06).
              05  SC05-GTENSU             PIC X(10).
