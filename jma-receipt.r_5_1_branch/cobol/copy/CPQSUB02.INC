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
       01  QSUB02-LNK.
           03  QSUB02-IN-AREA.
               05  QSUB02-YMD          PIC X(08).
               05  QSUB02-HMS          PIC X(06).
               05  QSUB02-LIMITCONTROL PIC 9(01).
               05  QSUB02-LIMIT        PIC 9(08).
               05  QSUB02-OFFSET       PIC 9(08).
           03  QSUB02-OT-AREA.
               05  QSUB02-SQL          PIC X(20000).
               05  QSUB02-SQL-ORDER    PIC X(500).
