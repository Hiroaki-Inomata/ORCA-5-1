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
        02  O01.
      *     02  O01-FIXED17.
              03  O01-B04-LABEL       PIC X(10).
              03  O01-B04-STATE       PIC S9(9)   BINARY.
              03  O01-B13-STATE       PIC S9(9)   BINARY.
              03  O01-LBLSRYYM        PIC X(10).
              03  O01-LBLDENPPRTYMD   PIC X(10).
              03  O01-LBLINFO01       PIC X(80).
              03  O01-CMB-PRTKBN.
                04  O01-PRTKBN.
                  05  O01-VALUE       PIC X(20).
                04  O01-PRTKBN-COUNT  PIC S9(9)   BINARY.
                04  O01-PRTKBNLST     PIC X(20)   OCCURS  5 TIMES.
              03  O01-SRYYM           PIC X(7).
              03  O01-SRYYM-STATE     PIC S9(9)   BINARY.
              03  O01-DENPPRTYMD          PIC X(10).
              03  O01-DENPPRTYMD-STATE    PIC S9(9)   BINARY.
              03  O01-IKKATU.
                04  O01-VALUE   PIC X.
              03  O01-KOBETU.
                04  O01-VALUE   PIC X.
              03  O01-CHK-KIKAN PIC X  OCCURS 3.
              03  O01-KIKAN01.
                04  O01-VALUE   PIC X(20).
              03  O01-KIKAN02.
                04  O01-VALUE   PIC X(20).
              03  O01-KIKAN03.
                04  O01-VALUE   PIC X(20).
