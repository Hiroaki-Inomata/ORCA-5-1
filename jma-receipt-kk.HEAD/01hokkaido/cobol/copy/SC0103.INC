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
            03  SC03-HOSPCD                 PIC X(07).
            03  SC03-SRYKANAME              PIC X(20).
            03  SC03-CITYNUM                PIC X(08).
            03  SC03-SEIKYU-TBL             OCCURS  10 TIMES.
                05  SC03-SRYYM              PIC X(08).
                05  SC03-JKYSNUM            PIC X(08).
                05  SC03-NAME               PIC X(22).
                05  SC03-HKNJANUM           PIC X(08).
                05  SC03-KIGO               PIC X(20).
                05  SC03-NUM                PIC X(20).
                05  SC03-FTNRATE-TBL        OCCURS  4 TIMES.
                    07  SC03-FTNRATE        PIC X(02).
                05  SC03-KOGAKU             PIC X(02).
                05  SC03-KAISU              PIC X(02).
                05  SC03-ITBFTN             PIC X(06).
                05  SC03-NISSU              PIC X(02).
                05  SC03-TENSU              PIC X(07).
                05  SC03-KOGAKUTEN          PIC X(07).
            03  SC03-GOKEI-TBL.
                05  SC03-GKAISU             PIC X(03).
                05  SC03-GITBFTN            PIC X(09).
                05  SC03-GTENSU             PIC X(09).
                05  SC03-GKOGAKUTEN         PIC X(09).
