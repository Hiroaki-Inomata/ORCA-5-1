      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will beuseful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditionsdescribed
      * in the JMA Open Source License. You should have received acopy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku,Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        01  SC27.
            03  SC27-SEIYMD         PIC X(22).
            03  SC27-HOSPCD         PIC X(14).
            03  SC27-ADRS           PIC X(100).
            03  SC27-HOSPNAME       PIC X(100).
            03  SC27-KAISETU        PIC X(40).
            03  SC27-HKNKBN         PIC X(02)    OCCURS  2 TIMES.
            03  SC27-SEIKYUGK       PIC X(09).
            03  SC27-SEIKYU-TBL     OCCURS  10 TIMES.
                05  SC27-JKYSNUM    PIC X(14).
                05  SC27-SRYYY      PIC X(02).
                05  SC27-SRYMM      PIC X(02).
                05  SC27-NYUGAIKBN  PIC X(02)  OCCURS  2 TIMES.
                05  SC27-NISSU      PIC X(02).
                05  SC27-TENSU      PIC X(10).
                05  SC27-TAHOTENSU  PIC X(10).
                05  SC27-ITBFTN     PIC X(07).
                05  SC27-HONFTN     PIC X(06).
                05  SC27-S-NISSU    PIC X(03).
                05  SC27-S-FTNGK    PIC X(07).
                05  SC27-NAME       PIC X(20).
            03  SC27-SYOKEI-TBL.
                05  SC27-SKENSU     PIC X(02).
                05  SC27-SNISSU     PIC X(03).
                05  SC27-SITBFTN    PIC X(09).
                05  SC27-SHONFTN    PIC X(07).
                05  SC27-SS-NISSU   PIC X(05).
                05  SC27-SS-FTNGK   PIC X(09).
            03  SC27-GOKEI-TBL.
                05  SC27-GKENSU     PIC X(03).
                05  SC27-GNISSU     PIC X(03).
                05  SC27-GITBFTN    PIC X(09).
                05  SC27-GHONFTN    PIC X(07).
                05  SC27-GS-NISSU   PIC X(05).
                05  SC27-GS-FTNGK   PIC X(09).
