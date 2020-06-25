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
      *
        02  L06.
      *     02  L06-FIXED.
              03  L06-B03-STATE        PIC S9(9)   BINARY.
              03  L06-LBLSTDATE-STYLE  PIC X(20).
              03  L06-LBLSTDATE        PIC X(50).
              03  L06-EDSRYYMD         PIC X(10).
              03  L06-EDHR             PIC S9(02).
              03  L06-EDMIN            PIC S9(02).
              03  L06-PREDATE          PIC X(60).
              03  L06-CMB-HOURSKBN.
                04  L06-HOURSKBN.
                  05  L06-VALUE        PIC X(20).
                04  L06-HOURSKBN-COUNT PIC S9(9)   BINARY.
                04  L06-HOURSKBN-ITEM  PIC X(20)
                                       OCCURS  10 TIMES.
