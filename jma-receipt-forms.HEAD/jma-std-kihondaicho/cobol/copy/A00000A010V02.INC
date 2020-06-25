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
       01  A010.
           03  A010-JOBMSG        PIC  X(60).
           03  A010-SYSYMD        PIC  X(22).
           03  A010-PAGE          PIC  X(05).
           03  A010-SRYYM         PIC  X(18).
           03  A010-SRYYMD        PIC  X(22).
           03  A010-SRYSTYMD      PIC  X(24).
           03  A010-SRYEDYMD      PIC  X(22).
           03  A010-OUTTYPE       PIC  X(40).
           03  A010-NYUGAIKBN     PIC  X(20).
           03  A010-SRYKA         PIC  X(30).
           03  A010-DRNAME        PIC  X(30).
           03  A010-NUM-TBL       OCCURS 38.
               05  A010-NUM       PIC  X(05).
           03  A010-SRYCD-TBL     OCCURS 38.
               05  A010-SRYCD     PIC  X(09).
           03  A010-INPCD-TBL     OCCURS 38.
               05  A010-INPCD     PIC  X(20).
           03  A010-INPMK-TBL     OCCURS 38.
               05  A010-INPMK     PIC  X(02).
           03  A010-SRYNM-TBL     OCCURS 38.
               05  A010-SRYNM     PIC  X(60).
           03  A010-TENTANI-TBL   OCCURS 38.
               05  A010-TENTANI   PIC  X(02).
           03  A010-TEN-TBL       OCCURS 38.
               05  A010-TEN       PIC  X(11).
           03  A010-KAISU-TBL     OCCURS 38.
               05  A010-KAISU     PIC  X(09).
           03  A010-TTEN-TBL      OCCURS 38.
               05  A010-TTEN      PIC  X(12).
           03  A010-SKTTEN        PIC  X(13).
           03  A010-SKTKIN        PIC  X(13).
           03  A010-SKTENTANI     PIC  X(02).
           03  A010-SKYENTANI     PIC  X(02).
           03  A010-GKTTEN        PIC  X(13).
           03  A010-GKTKIN        PIC  X(13).
           03  A010-GKTENTANI     PIC  X(02).
           03  A010-GKYENTANI     PIC  X(02).
