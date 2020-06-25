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
       01  HC12.
              03  HC12-JKYSNUM-TBL     OCCURS  21.
                  05  HC12-JKYSNUM     PIC  X(36).
              03  HC12-NAME-TBL        OCCURS  21.
                  05  HC12-NAME        PIC  X(26).
              03  HC12-HKNJANUM-TBL    OCCURS  21.
                  05  HC12-HKNJANUM    PIC  X(8).
              03  HC12-KIGOBANGO-TBL   OCCURS  21.
                  05  HC12-KIGOBANGO   PIC  X(36).
              03  HC12-SRYYM-TBL       OCCURS  21.
                  05  HC12-SRYYM       PIC  X(5).
              03  HC12-NYUGAIKBN-TBL   OCCURS  21.
                  05  HC12-NYUGAIKBN   PIC  X(1).
              03  HC12-KYURATE-TBL     OCCURS  21.
                  05  HC12-KYURATE     PIC  X(1).
              03  HC12-CHOKBN-TBL      OCCURS  21.
                  05  HC12-CHOKBN      PIC  X(2).
              03  HC12-NISSU-TBL       OCCURS  21.
                  05  HC12-NISSU       PIC  X(2).
              03  HC12-TENSU-TBL       OCCURS  21.
                  05  HC12-TENSU       PIC  X(7).
              03  HC12-YKZFTN-TBL      OCCURS  21.
                  05  HC12-YKZFTN      PIC  X(6).
              03  HC12-SKYMONEY-TBL    OCCURS  21.
                  05  HC12-SKYMONEY    PIC  X(6).
              03  HC12-GOKEI           PIC  X(6).
              03  HC12-GKENSU-TBL      OCCURS  6.
                  05  HC12-GKENSU      PIC  X(6).
              03  HC12-GNISSU-TBL      OCCURS  6.
                  05  HC12-GNISSU      PIC  X(3).
              03  HC12-GTENSU-TBL      OCCURS  6.
                  05  HC12-GTENSU      PIC  X(9).
              03  HC12-GYKZFTN-TBL     OCCURS  6.
                  05  HC12-GYKZFTN     PIC  X(7).
              03  HC12-GSKYMONEY-TBL   OCCURS  6.
                  05  HC12-GSKYMONEY   PIC  X(7).
              03  HC12-HOSPCD          PIC  X(10).
              03  HC12-ADRS            PIC  X(30).
              03  HC12-HOSPNAME        PIC  X(30).
              03  HC12-KAISETUNAME     PIC  X(30).
              03  HC12-PRTYM           PIC  X(16).
              03  HC12-SEIYMD          PIC  X(22).
              03  HC12-CITYNUM         PIC  X(6).
              03  HC12-FTNJANUM        PIC  X(8).
              03  HC12-CITYNAME        PIC  X(20).
              03  HC12-PAGE            PIC  X(3).
              03  HC12-KBN-TBL         OCCURS  3.
                  05  HC12-KBN         PIC  X(2).
              03  HC12-SRYKA           PIC  X(16).
