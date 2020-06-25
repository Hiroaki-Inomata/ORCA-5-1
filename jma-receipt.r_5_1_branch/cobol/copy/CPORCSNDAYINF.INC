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
       01  SNDAYINF-AREA.
           03  SNDAYINF-STSRYYMD       PIC X(08).
           03  SNDAYINF-EDSRYYMD       PIC X(08).
           03  SNDAYINF-DAYS-MAX       PIC 9(05).
           03  SNDAYINF-DAYS-OCC       OCCURS   366.
               05  SNDAYINF-SRYYMD     PIC X(08).
               05  SNDAYINF-ZAIINKBN   PIC X(01).
               05  SNDAYINF-BTUNUM     PIC X(02).
               05  SNDAYINF-BTUNUM-SAKI
                                       PIC X(02).
               05  SNDAYINF-BRMNUM     PIC X(06).
               05  SNDAYINF-BRMNUM-SAKI
                                       PIC X(06).
               05  SNDAYINF-SRYKA      PIC X(02).
               05  SNDAYINF-NYUINKBN   PIC X(01).
               05  SNDAYINF-TAIINKBN   PIC X(01).
