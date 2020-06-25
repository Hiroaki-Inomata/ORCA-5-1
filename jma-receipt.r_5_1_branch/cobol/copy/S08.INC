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
        02  S08.
      *      02  S08-FIXED6.
              03  S08-B04-LABEL            PIC X(10).
              03  S08-B05-STATE            PIC S9(9)   BINARY.
              03  S08-B07-LABEL            PIC X(10).
              03  S08-B12-STATE            PIC S9(9)   BINARY.
              03  S08-PTNUM                PIC X(20).
              03  S08-NAME                 PIC X(60).
              03  S08-NAME-STYLE           PIC X(20).
              03  S08-KANANAME             PIC X(60).
              03  S08-BIRTHDAY-MOM         PIC X(9).
              03  S08-AGE                  PIC X(8).
              03  S08-NYUINYMD             PIC X(10).
              03  S08-TAIINYMD             PIC X(10).
              03  S08-SYAKOK               PIC X(4).
              03  S08-HONKZK               PIC X(4).
              03  S08-HKNJANUM             PIC X(8).
              03  S08-KIGO                 PIC X(80).
              03  S08-NUM                  PIC X(80).
              03  S08-CMB-RIYOUKBN.
                04  S08-RIYOUKBN           PIC X(22).
                04  S08-RIYOUKBN-COUNT     PIC S9(9)   BINARY.
                04  S08-RIYOUKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
              03  S08-SKYYM                PIC X(10).
              03  S08-SKYYM-STATE          PIC S9(9)   BINARY.
              03  S08-CMB-TEISYUTUBIKBN.
                04  S08-TEISYUTUBIKBN       PIC X(22).
                04  S08-TEISYUTUBIKBN-STATE PIC S9(9)   BINARY.
                04  S08-TEISYUTUBIKBN-COUNT PIC S9(9)   BINARY.
                04  S08-TEISYUTUBIKBN-ITEM  PIC X(22)   OCCURS  5 TIMES.
                04  S08-TEISYUTUBIKBN-ITEM-STATE  PIC S9(9)   BINARY.
              03  S08-CMB-SKYKBN.
                04  S08-SKYKBN             PIC X(22).
                04  S08-SKYKBN-STATE       PIC S9(9)   BINARY.
                04  S08-SKYKBN-COUNT       PIC S9(9)   BINARY.
                04  S08-SKYKBN-ITEM        PIC X(22)   OCCURS  5 TIMES.
                04  S08-SKYKBN-ITEM-STATE  PIC S9(9)   BINARY.
              03  S08-CMB-BUNBENKBN.
                04  S08-BUNBENKBN          PIC X(22).
                04  S08-BUNBENKBN-COUNT    PIC S9(9)   BINARY.
                04  S08-BUNBENKBN-ITEM     PIC X(22)   OCCURS  5 TIMES.
              03  S08-CMB-TEISYUTUSAKI.
                04  S08-TEISYUTUSAKI       PIC X(22).
                04  S08-TEISYUTUSAKI-STATE PIC S9(9)   BINARY.
                04  S08-TEISYUTUSAKI-COUNT PIC S9(9)   BINARY.
                04  S08-TEISYUTUSAKI-ITEM  PIC X(22)   OCCURS  5 TIMES.
                04  S08-TEISYUTUSAKI-ITEM-STATE  PIC S9(9)   BINARY.
              03  S08-ZAIWEEK              PIC S9(2).
              03  S08-BIRTHDAY-BABY        PIC X(10).
              03  S08-CMB-SIZANKBN.
                04  S08-SIZANKBN           PIC X(22).
                04  S08-SIZANKBN-COUNT     PIC S9(9)   BINARY.
                04  S08-SIZANKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
              03  S08-BIRTHCNT             PIC S9(1).
              03  S08-NYUIN-DAY            PIC S9(3).
              03  S08-NYUIN-DAY-STATE      PIC S9(9)   BINARY.
              03  S08-CMB-SANKAKBN.
                04  S08-SANKAKBN           PIC X(22).
                04  S08-SANKAKBN-STATE     PIC S9(9)   BINARY.
                04  S08-SANKAKBN-COUNT     PIC S9(9)   BINARY.
                04  S08-SANKAKBN-ITEM      PIC X(22)   OCCURS  5 TIMES.
                04  S08-SANKAKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  S08-LBLNYU-MONEY         PIC X(20).
              03  S08-NYU-MONEY            PIC S9(7).
              03  S08-NYU-MONEY-VISIBLE    PIC X(1).
              03  S08-NYU-MONEY-STATE      PIC S9(9)   BINARY.
      *
              03  S08-LBLRMSGK-MONEY       PIC X(20).
              03  S08-RMSGK-MONEY          PIC S9(7).
              03  S08-RMSGK-MONEY-VISIBLE  PIC X(1).
              03  S08-RMSGK-MONEY-STATE    PIC S9(9)   BINARY.
      *
              03  S08-LBLKAIJO-MONEY       PIC X(20).
              03  S08-KAIJO-MONEY          PIC S9(7).
              03  S08-KAIJO-MONEY-VISIBLE  PIC X(1).
              03  S08-KAIJO-MONEY-STATE    PIC S9(9)   BINARY.
      *
              03  S08-LBLBUNBEN-MONEY      PIC X(20).
              03  S08-BUNBEN-MONEY         PIC S9(7).
              03  S08-BUNBEN-MONEY-VISIBLE PIC X(1).
              03  S08-BUNBEN-MONEY-STATE   PIC S9(9)   BINARY.
      *
              03  S08-LBLSHIN-MONEY        PIC X(20).
              03  S08-SHIN-MONEY           PIC S9(7).
              03  S08-SHIN-MONEY-VISIBLE   PIC X(1).
              03  S08-SHIN-MONEY-STATE     PIC S9(9)   BINARY.
      *
              03  S08-LBLKNS-MONEY         PIC X(20).
              03  S08-KNS-MONEY            PIC S9(7).
              03  S08-KNS-MONEY-VISIBLE    PIC X(1).
              03  S08-KNS-MONEY-STATE      PIC S9(9)   BINARY.
      *
              03  S08-LBLSYC-MONEY         PIC X(20).
              03  S08-SYC-MONEY            PIC S9(7).
              03  S08-SYC-MONEY-VISIBLE    PIC X(1).
              03  S08-SYC-MONEY-STATE      PIC S9(9)   BINARY.
      *
              03  S08-LBLSNK-MONEY         PIC X(20).
              03  S08-SNK-MONEY            PIC S9(7).
              03  S08-SNK-MONEY-VISIBLE    PIC X(1).
              03  S08-SNK-MONEY-STATE      PIC S9(9)   BINARY.
      *
              03  S08-LBLETC-MONEY         PIC X(20).
              03  S08-ETC-MONEY            PIC S9(7).
              03  S08-ETC-MONEY-VISIBLE    PIC X(1).
              03  S08-ETC-MONEY-STATE      PIC S9(9)   BINARY.
      *
              03  S08-LBLICBFTN-MONEY       PIC X(20).
              03  S08-ICBFTN-MONEY         PIC S9(7).
              03  S08-ICBFTN-MONEY-VISIBLE PIC X(1).
              03  S08-ICBFTN-MONEY-STATE   PIC S9(9)   BINARY.
      *
              03  S08-LBLNINPUFTN-MONEY    PIC X(20).
              03  S08-NINPUFTN-MONEY       PIC S9(7).
              03  S08-NINPUFTN-MONEY-VISIBLE PIC X(1).
              03  S08-NINPUFTN-MONEY-STATE PIC S9(9)   BINARY.
      *
              03  S08-LBLDAIRI-MONEY       PIC X(20).
              03  S08-DAIRI-MONEY          PIC S9(7).
              03  S08-DAIRI-MONEY-VISIBLE  PIC X(1).
              03  S08-DAIRI-MONEY-STATE    PIC S9(9)   BINARY.
      *
              03  S08-BIKOU                PIC X(60).
      *
              03  S08-LBLMEIPRTKBN         PIC X(20).
              03  S08-CMB-MEIPRTKBN.
                04  S08-MEIPRTKBN          PIC X(22).
                04  S08-MEIPRTKBN-VISIBLE  PIC X(1).
                04  S08-MEIPRTKBN-STATE    PIC S9(9)   BINARY.
                04  S08-MEIPRTKBN-COUNT    PIC S9(9)   BINARY.
                04  S08-MEIPRTKBN-ITEM     PIC X(22)   OCCURS  5 TIMES.
                04  S08-MEIPRTKBN-ITEM-VISIBLE  PIC X(1).
                04  S08-MEIPRTKBN-ITEM-STATE    PIC S9(9)   BINARY.
              03  S08-LBLINFO01-OCC        OCCURS  4 TIMES.
                04  S08-LBLINFO            PIC X(100).
                04  S08-LBLINFO-STYLE      PIC X(20).
              03  S08-LBLHEADER            PIC X(50).
              03  S08-LBLHEADER-STYLE      PIC X(20).
              03  S08-LBLNYUINYMD          PIC X(10).
              03  S08-LBLTAIINYMD          PIC X(10).
              03  S08-LBLNYUINDAY          PIC X(10).
