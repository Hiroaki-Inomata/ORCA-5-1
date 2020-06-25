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
           02  L01.
              03  L01-B04.
                04  L01-B04-STATE     PIC S9(9)   BINARY.
              03  L01-B06.
                04  L01-B06-STATE     PIC S9(9)   BINARY.
              03  L01-B07.
                04  L01-B07-STATE     PIC S9(9)   BINARY.
              03  L01-B10.
                04  L01-B10-STATE     PIC S9(9)   BINARY.
              03  L01-MARKHISSU-G.
                04  L01-MARKHISSU-STYLE
                                      PIC X(20).
                04  L01-MARKHISSU     PIC X(01).
              03  L01-MARKNINI-G.
                04  L01-MARKNINI-STYLE
                                      PIC X(20).
                04  L01-MARKNINI      PIC X(01).
              03  L01-CHK-G           OCCURS  10.
                04  L01-CHK-STATE     PIC S9(9)   BINARY.
                04  L01-CHK-VALUE     PIC X.
                04  L01-CHK-LABEL     PIC X(80).
              03  L01-PARA-G          OCCURS  10.
                04  L01-PARA-TBL      OCCURS  8.
                  05  L01-PARA-STATE  PIC S9(9)   BINARY.
                  05  L01-PARA        PIC X(20).
                04  L01-PARAHYOJI-G   OCCURS  8.
                  05  L01-PARAHYOJI   PIC X(20).
                04  L01-PARAMARK-G   OCCURS  8.
                  05  L01-PARAMARK-STYLE
                                      PIC X(20).
                  05  L01-PARAMARK    PIC X(01).
              03  L01-NUM-G           OCCURS  10.
                04  L01-NUM-STATE     PIC S9(9)   BINARY.
                04  L01-NUM           PIC X(03).
