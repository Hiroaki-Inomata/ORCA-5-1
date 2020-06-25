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
           02  L03.
              03  L03-B04-LABEL       PIC X(08).
              03  L03-MARKHISSU-G.
                04  L03-MARKHISSU-STYLE
                                      PIC X(20).
                04  L03-MARKHISSU     PIC X(01).
              03  L03-MARKNINI-G.
                04  L03-MARKNINI-STYLE
                                      PIC X(20).
                04  L03-MARKNINI      PIC X(01).
              03  L03-CHK-G.
                04  L03-CHK-STATE     PIC S9(9)   BINARY.
                04  L03-CHK-VALUE     PIC X.
                04  L03-CHK-LABEL     PIC X(80).
              03  L03-PARA-G.
                04  L03-PARA-TBL      OCCURS  8.
                  05  L03-PARA-STATE  PIC S9(9)   BINARY.
                  05  L03-PARA        PIC X(20).
                04  L03-PARAHYOJI-G   OCCURS  8.
                  05  L03-PARAHYOJI   PIC X(20).
                04  L03-PARAMARK-G   OCCURS  8.
                  05  L03-PARAMARK-STYLE
                                      PIC X(20).
                  05  L03-PARAMARK    PIC X(01).
              03  L03-NUM-G.
                04  L03-NUM           PIC X(03).
              03  L03-PRGNAME         PIC X(20).
              03  L03-REPOS-NAME      PIC X(256).
              03  L03-SCROLLEDWINDOW1.
                06  L03-MEMO          PIC X(6000).
              03  L03-SCROLLEDWINDOW2.
                06  L03-USERMEMO      PIC X(5000).

