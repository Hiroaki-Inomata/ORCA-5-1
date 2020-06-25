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
       01  TOWN-DAT.
           03  TOWN-TBL  OCCURS  100.
               05  TOWN-KEY.
                   07  TOWN-NUM       PIC 9(02).
               05  TOWN-INF.
                   07  TOWN-SPACE1    PIC 9(01).
                   07  TOWN-CITYNUM   PIC X(06).
                   07  TOWN-SPACE2    PIC 9(01).
                   07  TOWN-NAME      PIC X(20).
                   07  TOWN-KAIGYO    PIC X(01).
