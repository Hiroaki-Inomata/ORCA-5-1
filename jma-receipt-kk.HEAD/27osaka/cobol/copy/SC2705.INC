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
            03  SC05-TEISYUTU1        PIC X(10).
            03  SC05-TEISYUTU2        PIC X(02).
            03  SC05-PRTYM            PIC X(16).
            03  SC05-TOWNNAME         PIC X(12).
            03  SC05-GKOHFTN          PIC X(09).
            03  SC05-SEIYMD           PIC X(22).
            03  SC05-HOSPCDN          PIC X(20).
            03  SC05-ADRS             PIC X(100).
            03  SC05-HOSPNAME         PIC X(100).
            03  SC05-KAISETU          PIC X(40).
            03  SC05-TEL              PIC X(15).
            03  SC05-SRY-TBL            OCCURS  20 TIMES.
                05  SC05-FTNJANUM1    PIC X(04).
                05  SC05-FTNJANUM2    PIC X(08).
                05  SC05-JKYSNUM      PIC X(14).
                05  SC05-NAME         PIC X(24).
                05  SC05-SRYYY        PIC X(02).
                05  SC05-SRYMM        PIC X(02).
                05  SC05-NYUGAI-TBL         OCCURS  2 TIMES.
                    07  SC05-NYUGAI   PIC X(02).
                05  SC05-NISSU        PIC X(04).
                05  SC05-ITBFTN       PIC X(12).
                05  SC05-JKYFTN       PIC X(10).
                05  SC05-BIKO1        PIC X(02).
                05  SC05-BIKO2        PIC X(02).
            03  SC05-GOKEI-TBL.
                05  SC05-GITBFTN      PIC X(12).
                05  SC05-GJKYFTN       PIC X(10).
