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
        01  SC07.
            03  SC07-HOSPCD             PIC X(07).
            03  SC07-ADRS               PIC X(100).
            03  SC07-HOSPNAME           PIC X(100).
            03  SC07-KAISETU            PIC X(40).
            03  SC07-TEL                PIC X(15).
            03  SC07-PRTYM              PIC X(16).
            03  SC07-SEIYMD             PIC X(22).
            03  SC07-CITYNAME           PIC X(36).
            03  SC07-SRYKANAME          PIC X(20).
            03  SC07-GOKEI-TBL          OCCURS  5 TIMES.
                05  SC07-GKENSU         PIC X(05).
                05  SC07-GTENSU         PIC X(09).
                05  SC07-GITBFTN        PIC X(07).
                05  SC07-GTESURYO       PIC X(06).
            03  SC07-SEIKYU-TBL         OCCURS  10 TIMES.
                05  SC07-SRYYM          PIC X(08).
                05  SC07-JKYSNUM        PIC X(14).
                05  SC07-NAME           PIC X(24).
                05  SC07-NYUGAIKBN      PIC X(02)    OCCURS  2 TIMES.
                05  SC07-TENSU7         PIC X(07).
                05  SC07-TENSU8         PIC X(07).
                05  SC07-TENSU9         PIC X(07).
                05  SC07-ZNK            PIC X(07).
                05  SC07-RJN            PIC X(07).
                05  SC07-KOHKBN         PIC X(02)    OCCURS  6 TIMES.
                05  SC07-ITBFTN         PIC X(06).
                05  SC07-FTNMARU        PIC X(02)    OCCURS  2 TIMES.
                05  SC07-MARU           PIC X(02).
                05  SC07-BIKO1          PIC X(03).
                05  SC07-BIKO2          PIC X(02).
            03  SC07-SYOKEI-TBL.
                05  SC07-SKENSU         PIC X(05)    OCCURS  5 TIMES.
                05  SC07-STENSU7        PIC X(07).
                05  SC07-STENSU8        PIC X(07).
                05  SC07-STENSU9        PIC X(07).
                05  SC07-SZNK           PIC X(07).
                05  SC07-SRJN           PIC X(07).
                05  SC07-SITBFTN        PIC X(07).
