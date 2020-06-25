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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  X05.
            03  X05-FIXED2.
              04  X05-B03.
                05  X05-B03-STATE   PIC S9(9)   BINARY.
              04  X05-B11.
                05  X05-B11-LABEL   PIC X(10).
              04  X05-NAME.
                05  X05-VALUE   PIC X(100).
              04  X05-NO        PIC S9(3)  OCCURS  20.
              04  X05-TEKCD-G   OCCURS  20.
                05  X05-TEKCD.   
                  06  X05-VALUE PIC X(20).
              04  X05-DSPNAME-G OCCURS  20.
                05  X05-DSPNAME. 
                  06  X05-VALUE PIC X(100).
              04  X05-DMKBN-OCC OCCURS  20.
                05  X05-DMKBN   PIC S9(1).
                05  X05-DMKBN-STATE   PIC S9(9)   BINARY.
              04  X05-SRYCD.
                05  X05-VALUE   PIC X(200).
              04  X05-STYUKYMD.
                05  X05-VALUE   PIC X(9).
              04  X05-EDYUKYMD.
                05  X05-VALUE   PIC X(9).
              04  X05-CHKKBN.
                05  X05-VALUE   PIC X(50).
              04  X05-LBLPAGE.
                05  X05-VALUE   PIC X(10).
              03  X05-CMB-NYUGAIKBN.
                04  X05-NYUGAIKBN      PIC X(22).
                04  X05-NYUGAIKBN-STATE   PIC S9(9)   BINARY.
                04  X05-NYUGAIKBN-COUNT
                                PIC S9(9)   BINARY.
                04  X05-NYUGAIKBN-ITEM
                                PIC X(22)   OCCURS  5.
                04  X05-NYUGAIKBN-ITEM-STATE   PIC S9(9)   BINARY.
              03  X05-CMB-NYUGAIBNDKBN.
                04  X05-NYUGAIBNDKBN        PIC X(22).
                04  X05-NYUGAIBNDKBN-STATE  PIC S9(9)   BINARY.
                04  X05-NYUGAIBNDKBN-COUNT  PIC S9(9)   BINARY.
                04  X05-NYUGAIBNDKBN-ITEM   PIC X(22)   OCCURS  5.
                04  X05-NYUGAIBNDKBN-ITEM-STATE  PIC S9(9)   BINARY.
              03  X05-CMB-JYKNKBN.
                04  X05-JYKNKBN             PIC X(12).
                04  X05-JYKNKBN-STATE       PIC S9(9)   BINARY.
                04  X05-JYKNKBN-COUNT       PIC S9(9)   BINARY.
                04  X05-JYKNKBN-ITEM        PIC X(12)   OCCURS  5.
                04  X05-JYKNKBN-ITEM-STATE  PIC S9(9)   BINARY.
              03  X05-LBLSRYCD              PIC X(20).
              03  X05-LBLSRYCD-STYLE        PIC X(20).
              03  X05-LBLNYUGAIBND          PIC X(20).
              03  X05-LBLJYKN               PIC X(20).
              03  X05-LBLDM                 PIC X(20).
