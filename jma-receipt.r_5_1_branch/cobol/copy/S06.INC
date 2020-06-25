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
        02  S06.
      *      02  S06-FIXED12.
              03  S06-PTNUM                PIC X(20).
              03  S06-NAME                 PIC X(50).
              03  S06-CMB-NYUGAIKBN.
                04  S06-NYUGAIKBN          PIC X(10).
                04  S06-NYUGAIKBN-COUNT    PIC S9(9)   BINARY.
                04  S06-NYUGAIKBN-ITEM     PIC X(10)   OCCURS  3 TIMES.
              03  S06-LBLMSG-G.
                04  S06-LBLMSG-STYLE
                                           PIC X(20).
                04  S06-LBLMSG             PIC X(50).
              03  S06-GAI-MISYU            PIC S9(9).
              03  S06-NYU-MISYU            PIC S9(9).
              03  S06-SELNUM               PIC S9(3).
              03  S06-SELNUM-STATE         PIC S9(9)   BINARY.
              03  S06-RD-NYUKIN-IKT        PIC X.
              03  S06-RD-NYUKIN-KBT        PIC X.
              03  S06-CMB-NYKNKNRKBN.
                04  S06-NYKNKNRKBN         PIC X(42).
                04  S06-NYKNKNRKBN-STATE   PIC S9(9)   BINARY.
                04  S06-NYKNKNRKBN-COUNT   PIC S9(9)   BINARY.
                04  S06-NYKNKNRKBN-ITEM    PIC X(42)   OCCURS  5 TIMES.
                04  S06-NYKNKNRKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  S06-CMB-IKTSRYYMKBN.
                04  S06-IKTSRYYMKBN         PIC X(32).
                04  S06-IKTSRYYMKBN-STATE   PIC S9(9)   BINARY.
                04  S06-IKTSRYYMKBN-COUNT   PIC S9(9)   BINARY.
                04  S06-IKTSRYYMKBN-ITEM    PIC X(32)   OCCURS  5 TIMES.
                04  S06-IKTSRYYMKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  S06-IKTSRYYM             PIC X(10).
              03  S06-IKTSRYYM-STATE       PIC S9(9)   BINARY.
              03  S06-NYUKINYMD            PIC X(10).
              03  S06-CMB-NYKNHOHO.
                04  S06-NYKNHOHO           PIC X(33).
                04  S06-NYKNHOHO-COUNT     PIC S9(9)   BINARY.
                04  S06-NYKNHOHO-ITEM      PIC X(33)   OCCURS  50 TIMES.
              03  S06-GAI-NYUKIN           PIC S9(9).
              03  S06-GAI-NYUKIN-STATE     PIC S9(9)   BINARY.
              03  S06-GAI-LNYUKIN-MAX      PIC X(10).
              03  S06-GAI-NYUKIN-MAX       PIC X(12).
              03  S06-GAI-HENKIN           PIC S9(9).
              03  S06-GAI-HENKIN-STATE     PIC S9(9)   BINARY.
              03  S06-GAI-LHENKIN-MAX      PIC X(10).
              03  S06-GAI-HENKIN-MAX       PIC X(12).
              03  S06-NYU-NYUKIN           PIC S9(9).
              03  S06-NYU-NYUKIN-STATE     PIC S9(9)   BINARY.
              03  S06-NYU-LNYUKIN-MAX      PIC X(10).
              03  S06-NYU-NYUKIN-MAX       PIC X(12).
              03  S06-NYU-HENKIN           PIC S9(9).
              03  S06-NYU-HENKIN-STATE     PIC S9(9)   BINARY.
              03  S06-NYU-LHENKIN-MAX      PIC X(10).
              03  S06-NYU-HENKIN-MAX       PIC X(12).
              03  S06-LBLNYUKIN            PIC X(20).
              03  S06-NYUKIN               PIC S9(9).
              03  S06-NYUKIN-STATE         PIC S9(9)   BINARY.
              03  S06-LNYUKIN-MAX          PIC X(10).
              03  S06-NYUKIN-MAX           PIC X(12).
              03  S06-CMB-SKYPRTKBN.
                04  S06-SKYPRTKBN          PIC X(32).
                04  S06-SKYPRTKBN-COUNT    PIC S9(9)   BINARY.
                04  S06-SKYPRTKBN-ITEM     PIC X(32)   OCCURS  5 TIMES.
              03  S06-CMB-SKYSUMKBN.
                04  S06-SKYSUMKBN          PIC X(42).
                04  S06-SKYSUMKBN-STATE    PIC S9(9)   BINARY.
                04  S06-SKYSUMKBN-COUNT    PIC S9(9)   BINARY.
                04  S06-SKYSUMKBN-ITEM     PIC X(42)   OCCURS  5 TIMES.
                04  S06-SKYSUMKBN-ITEM-STATE
                                           PIC S9(9)   BINARY.
              03  S06-SCROLLEDWINDOW1.
                04  S06-SYULIST.
                  05  S06-SYULIST-COUNT    PIC S9(9)   BINARY.
                  05  S06-SYULIST-ROW      PIC S9(9)   BINARY.
                  05  S06-SYULIST-ROWATTR  PIC S9(9)   BINARY.
                  05  S06-SYULIST-ITEM     OCCURS  400 TIMES.
                    06  S06-TNUM        PIC X(5).
                    06  S06-TDENPNUM    PIC X(10).
                    06  S06-TSRYKA      PIC X(6).
                    06  S06-TNYUGAI     PIC X(4).
                    06  S06-THKN        PIC X(50).
                    06  S06-TFTNRATE    PIC X(5).
                    06  S06-TSKYSTYMD   PIC X(11).
                    06  S06-TSRYYMD     PIC X(15).
                    06  S06-TSKYMONEY   PIC X(11).
                    06  S06-TMISYUMONEY PIC X(11).
                    06  S06-TNYKNMONEY  PIC X(11).
                  05  S06-SYULIST-SELECT   PIC X   OCCURS  400 TIMES.
