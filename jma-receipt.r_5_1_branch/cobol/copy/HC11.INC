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
       01  HC11.
              03  HC11-HOSPCD          PIC  X(10).
              03  HC11-ADRS            PIC  X(30).
              03  HC11-HOSPNAME        PIC  X(30).
              03  HC11-KAISETUNAME     PIC  X(30).
              03  HC11-PRTYM           PIC  X(16).
              03  HC11-SEIYMD          PIC  X(22).
              03  HC11-CITYNUM         PIC  X(6).
              03  HC11-FTNJANUM        PIC  X(8).
              03  HC11-CITYNAME        PIC  X(20).
              03  HC11-PAGE            PIC  X(3).
              03  HC11-KBN-TBL         OCCURS  3.
                  05  HC11-KBN         PIC  X(2).
              03  HC11-JKYSNUM-TBL     OCCURS  22.
                  05  HC11-JKYSNUM     PIC  X(36).
              03  HC11-NAME-TBL        OCCURS  22.
                  05  HC11-NAME        PIC  X(26).
              03  HC11-HKNJANUM-TBL    OCCURS  22.
                  05  HC11-HKNJANUM    PIC  X(8).
              03  HC11-KIGOBANGO-TBL   OCCURS  22.
                  05  HC11-KIGOBANGO   PIC  X(36).
              03  HC11-BIRTHDAY-TBL    OCCURS  22.
                  05  HC11-BIRTHDAY    PIC  X(8).
              03  HC11-SRYYM-TBL       OCCURS  22.
                  05  HC11-SRYYM       PIC  X(5).
              03  HC11-NYUGAIKBN-TBL   OCCURS  22.
                  05  HC11-NYUGAIKBN   PIC  X(1).
              03  HC11-KYURATE-TBL     OCCURS  22.
                  05  HC11-KYURATE     PIC  X(1).
              03  HC11-NISSU-TBL       OCCURS  22.
                  05  HC11-NISSU       PIC  X(2).
              03  HC11-TENSU-TBL       OCCURS  22.
                  05  HC11-TENSU       PIC  X(7).
              03  HC11-KOHFTN-TBL      OCCURS  22.
                  05  HC11-KOHFTN      PIC  X(8).
              03  HC11-GOKEI           PIC  X(6).
              03  HC11-GKENSU-TBL      OCCURS  4.
                  05  HC11-GKENSU      PIC  X(6).
              03  HC11-GNISSU-TBL      OCCURS  4.
                  05  HC11-GNISSU      PIC  X(3).
              03  HC11-GTENSU-TBL      OCCURS  4.
                  05  HC11-GTENSU      PIC  X(9).
              03  HC11-GKOHFTN-TBL     OCCURS  4.
                  05  HC11-GKOHFTN     PIC  X(7).
