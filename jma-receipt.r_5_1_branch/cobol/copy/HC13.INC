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
       01  HC13.
              03  HC13-HOSPCD          PIC  X(10).
              03  HC13-ADRS            PIC  X(30).
              03  HC13-HOSPNAME        PIC  X(30).
              03  HC13-KAISETUNAME     PIC  X(30).
              03  HC13-PRTYM           PIC  X(16).
              03  HC13-SEIYMD          PIC  X(22).
              03  HC13-CITYNUM         PIC  X(6).
              03  HC13-FTNJANUM        PIC  X(8).
              03  HC13-CITYNAME        PIC  X(20).
              03  HC13-PAGE            PIC  X(3).
              03  HC13-KBN-TBL         OCCURS  3.
                  05  HC13-KBN         PIC  X(2).
              03  HC13-ROFTNJANUM-TBL  OCCURS  23.
                  05  HC13-ROFTNJANUM  PIC  X(8).
              03  HC13-ROJKYSNUM-TBL   OCCURS  23.
                  05  HC13-ROJKYSNUM   PIC  X(28).
              03  HC13-JKYSNUM-TBL     OCCURS  23.
                  05  HC13-JKYSNUM     PIC  X(28).
              03  HC13-NAME-TBL        OCCURS  23.
                  05  HC13-NAME        PIC  X(26).
              03  HC13-HKNJANUM-TBL    OCCURS  23.
                  05  HC13-HKNJANUM    PIC  X(8).
              03  HC13-KIGOBANGO-TBL   OCCURS  23.
                  05  HC13-KIGOBANGO   PIC  X(36).
              03  HC13-SRYYM-TBL       OCCURS  23.
                  05  HC13-SRYYM       PIC  X(5).
              03  HC13-NYUGAIKBN-TBL   OCCURS  23.
                  05  HC13-NYUGAIKBN   PIC  X(1).
              03  HC13-NISSU-TBL       OCCURS  23.
                  05  HC13-NISSU       PIC  X(2).
              03  HC13-YKZFTN-TBL      OCCURS  23.
                  05  HC13-YKZFTN      PIC  X(6).
              03  HC13-ITBFTN-TBL      OCCURS  23.
                  05  HC13-ITBFTN      PIC  X(6).
              03  HC13-GOKEI           PIC  X(6).
              03  HC13-GKENSU-TBL      OCCURS  2.
                  05  HC13-GKENSU      PIC  X(6).
              03  HC13-GNISSU-TBL      OCCURS  2.
                  05  HC13-GNISSU      PIC  X(3).
              03  HC13-GYKZFTN-TBL     OCCURS  2.
                  05  HC13-GYKZFTN     PIC  X(7).
              03  HC13-GITBFTN-TBL     OCCURS  2.
                  05  HC13-GITBFTN     PIC  X(7).
