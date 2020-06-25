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
        02  D01.
          03  D01-B05S-STATE           PIC S9(9)   BINARY.
          03  D01-B06S-STATE           PIC S9(9)   BINARY.
          03  D01-B07S-STATE           PIC S9(9)   BINARY.
          03  D01-B08-LABEL            PIC X(10).
          03  D01-LBLIKTKBT-STYLE      PIC X(20).
          03  D01-LBLIKTKBT            PIC X(20).
          03  D01-SRYYM                PIC X(07).
          03  D01-CMB-NYUGAIKBN.
            04  D01-NYUGAIKBN          PIC X(20).
            04  D01-NYUGAIKBN-COUNT    PIC S9(9)   BINARY.
            04  D01-NYUGAIKBN-ITEM     PIC X(20)   OCCURS  5 TIMES.
          03  D01-CHK-SYAHO            PIC X(01).
          03  D01-CHK-KOKUHO           PIC X(01).
          03  D01-CHK-KOUKI-STATE      PIC S9(9)   BINARY.
          03  D01-CHK-KOUKI            PIC X(01).
          03  D01-CHK-RSIJBI           PIC X(01).
          03  D01-CHK-KOUGAI           PIC X(01).
          03  D01-CHK-JIHI             PIC X(01).
          03  D01-CMB-PRTSRTKBN.
            04  D01-PRTSRTKBN          PIC X(22).
            04  D01-PRTSRTKBN-COUNT    PIC S9(9)   BINARY.
            04  D01-PRTSRTKBN-ITEM     PIC X(22)   OCCURS  5.
          03  D01-CHK-SRYYMD-STATE     PIC S9(9)   BINARY.
          03  D01-CHK-SRYYMD           PIC X(01).
          03  D01-STSRYYMD-STATE       PIC S9(9)   BINARY.
          03  D01-STSRYYMD             PIC S9(2).
          03  D01-EDSRYYMD-STATE       PIC S9(9)   BINARY.
          03  D01-EDSRYYMD             PIC S9(2).
          03  D01-CMB-USRCHKKBN.
            04  D01-USRCHKKBN          PIC X(33).
            04  D01-USRCHKKBN-COUNT    PIC S9(9)   BINARY.
            04  D01-USRCHKKBN-ITEM     PIC X(33)   OCCURS 10 TIMES.
          03  D01-CHKMSG-G.
            04  D01-CHKMSG             PIC X(40)   OCCURS 21 TIMES.
          03  D01-INGAIPROCMSG         PIC X(40).
          03  D01-CMB-CHKKBN-G.
            04  D01-CMB-CHKKBN-OCC                 OCCURS 21 TIMES.
              05  D01-CHKKBN           PIC X(20).
              05  D01-CHKKBN-COUNT     PIC S9(9)   BINARY.
              05  D01-CHKKBN-ITEM      PIC X(20)   OCCURS  5 TIMES.
          03  D01-CMB-INGAIPROCKBN.
            04  D01-INGAIPROCKBN       PIC X(20).
            04  D01-INGAIPROCKBN-COUNT PIC S9(9)   BINARY.
            04  D01-INGAIPROCKBN-ITEM  PIC X(20)   OCCURS  5 TIMES.
