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
       01  DSUB01-LNK.
           03  DSUB01-KBN              PIC X(01).
           03  DSUB01-TERMID           PIC X(64).
           03  DSUB01-SCRSPA-AREA.
               05  DSUB01-SCRSPA-OCC   OCCURS  10.
                   07  DSUB01-SCRSPA   PIC X(50000).
           03  DSUB01-COMMON-AREA.
               05  DSUB01-COMMON       PIC X(10000).
