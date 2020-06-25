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
       02  L04.
         03  L04-B04-LABEL             PIC X(10).
         03  L04-B12-LABEL             PIC X(10).
         03  L04-LBLMES01              PIC X(40).
         03  L04-CMB-PROCKBN.
           04  L04-PROCKBN             PIC X(50).
                04  L04-PROCKBN-COUNT  PIC S9(9)   BINARY.
                04  L04-PROCKBN-ITEM   PIC X(50)   OCCURS  10 TIMES.
         03  L04-TERMLST-G.
           04  L04-TERMLST-OCC         OCCURS  10.
             05  L04-LBLNO            PIC X(2)
                                       OCCURS  2.
             05  L04-LBLMID           PIC X(2).
             05  L04-LBLMARK-STYLE    PIC X(20).
             05  L04-LBLMARK          PIC X(30).
             05  L04-TERM-OCC         OCCURS  2.
               06  L04-CHK-SRYYMD-STYLE   PIC X(20).
               06  L04-SRYYMD             PIC X(10).
               06  L04-CHK-TIME-STYLE     PIC X(20).
               06  L04-TIME               PIC X(10).
         03  L04-STSRYYMD-STATE           PIC S9(9)   BINARY.
         03  L04-STSRYYMD-STYLE           PIC X(20).
         03  L04-STSRYYMD                 PIC X(10).
         03  L04-STNO-STATE               PIC S9(9)   BINARY.
         03  L04-STNO                     PIC S9(2).
         03  L04-STHR-STATE               PIC S9(9)   BINARY.
         03  L04-STHR-STYLE               PIC X(20).
         03  L04-STHR                     PIC X(2).
         03  L04-STMIN-STATE              PIC S9(9)   BINARY.
         03  L04-STMIN-STYLE              PIC X(20).
         03  L04-STMIN                    PIC X(2).
         03  L04-EDSRYYMD-STATE           PIC S9(9)   BINARY.
         03  L04-EDSRYYMD                 PIC X(10).
         03  L04-EDNO-STATE               PIC S9(9)   BINARY.
         03  L04-EDNO                     PIC X(2).
         03  L04-EDHR-STATE               PIC S9(9)   BINARY.
         03  L04-EDHR                     PIC S9(2).
         03  L04-EDMIN-STATE              PIC S9(9)   BINARY.
         03  L04-EDMIN                    PIC S9(2).
         03  L04-LBLKIKAN1                PIC X(20).
         03  L04-LBLKIKAN2                PIC X(2).
         03  L04-CMB-HOURSKBN.
           04  L04-HOURSKBN               PIC X(10).
           04  L04-HOURSKBN-STATE         PIC S9(9)   BINARY.
           04  L04-HOURSKBN-COUNT         PIC S9(9)   BINARY.
           04  L04-HOURSKBN-ITEM
                        PIC X(10)   OCCURS  10 TIMES.
           04  L04-HOURSKBN-ITEM-STATE    PIC S9(9)   BINARY.
