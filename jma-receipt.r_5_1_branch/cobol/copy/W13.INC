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
        02  W13.
              03  W13-B03.
                04  W13-B03-STATE      PIC S9(9)   BINARY.
              03  W13-BTNPREV.
                04  W13-BTNPREV-STATE  PIC S9(9)   BINARY.
              03  W13-BTNNEXT.
                04  W13-BTNNEXT-STATE  PIC S9(9)   BINARY.
              03  W13-BTNDEL-G.
                04  W13-BTNDEL-STATE   PIC S9(9)   BINARY.
              03  W13-YUKOSTYMD.
                04  W13-VALUE   PIC X(9).
              03  W13-YUKOEDYMD.
                04  W13-VALUE   PIC X(9).
              03  W13-RSIIRYOCDN.
                04  W13-VALUE   PIC X(40).
              03  W13-SONSHOUKBN1.
                04  W13-VALUE   PIC X(2).
              03  W13-SONSHOUKBN2.
                04  W13-VALUE   PIC X(1).
              03  W13-SONSHOUNAME.
                04  W13-VALUE   PIC X(60).
              03  W13-SCROLLEDWINDOW6.
                04  W13-SONSHOUKBNLST.
                  05  W13-SONSHOUKBN-COUNT   PIC S9(9)   BINARY.
                  05  W13-SONSHOUKBN-ROW     PIC S9(9)   BINARY.
                  05  W13-SONSHOUKBN-ROWATTR  PIC S9(9)   BINARY.
                  05  W13-SONSHOUKBN-ITEM    OCCURS  40 TIMES.
                    06  W13-TSONSHOUKBN      PIC X(4).
                    06  W13-TSONSHOUNAME     PIC X(60).
                  05  W13-SONSHOUKBN-SELECT  PIC X   OCCURS  40 TIMES.
              03  W13-TENKBN-COMBO.
                04  W13-TENKBN         PIC X(20).
                04  W13-TENKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-TENKBNLST      PIC X(20)   OCCURS  2 TIMES.
              03  W13-ROUKAISU-COMBO.
                04  W13-ROUKAISU         PIC X(10).
                04  W13-ROUKAISU-COUNT   PIC S9(9)   BINARY.
                04  W13-ROUKAISULST      PIC X(10)   OCCURS  2 TIMES.
              03  W13-KSNRATE            PIC S9(3).
              03  W13-RSIIRYOKBN-COMBO.
                04  W13-RSIIRYOKBN       PIC X(20).
                04  W13-RSIIRYOKBN-COUNT PIC S9(9)   BINARY.
                04  W13-RSIIRYOKBNLST    PIC X(20)   OCCURS  2 TIMES.
              03  W13-JIBAIKBN-COMBO.
                04  W13-JIBAIKBN           PIC X(10).
                04  W13-JIBAIKBN-COUNT     PIC S9(9)   BINARY.
                04  W13-JIBAIKBNLST        PIC X(10)   OCCURS  2 TIMES.
              03  W13-TAXKBN-COMBO.
                04  W13-TAXKBN           PIC X(10).
                04  W13-TAXKBN-COUNT     PIC S9(9)   BINARY.
                04  W13-TAXKBNLST        PIC X(10)   OCCURS  2 TIMES.
              03  W13-SKJKBN-COMBO.
                04  W13-SKJKBN           PIC X(20).
                04  W13-SKJKBN-COUNT     PIC S9(9)   BINARY.
                04  W13-SKJKBNLST        PIC X(20)   OCCURS  2 TIMES.
              03  W13-KIZAITAXKBN-COMBO.
                04  W13-KIZAITAXKBN        PIC X(10).
                04  W13-KIZAITAXKBN-COUNT  PIC S9(9)   BINARY.
                04  W13-KIZAITAXKBNLST     PIC X(10)   OCCURS  2 TIMES.
              03  W13-KOTEIKBN-COMBO.
                04  W13-KOTEIKBN         PIC X(10).
                04  W13-KOTEIKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-KOTEIKBNLST      PIC X(10)   OCCURS  2 TIMES.
              03  W13-LBLKIZAILST          PIC X(60).
              03  W13-SCROLLEDWINDOW7.
                04  W13-KIZAILST.
                  05  W13-KIZAILST-COUNT   PIC S9(9)   BINARY.
                  05  W13-KIZAILST-ROW     PIC S9(9)   BINARY.
                  05  W13-KIZAILST-ROWATTR PIC S9(9)   BINARY.
                  05  W13-KIZAILST-ITEM    OCCURS  50 TIMES.
                    06  W13-TNUM            PIC X(3).
                    06  W13-TSRYCD-MARK     PIC X(1).
                    06  W13-TSRYCD          PIC X(9).
                    06  W13-TSRYCDNAME      PIC X(60).
                  05  W13-KIZAILIST-SELECT
                          PIC X       OCCURS  50 TIMES.
              03  W13-RSIPRT-COMBO.
                04  W13-RSIPRT           PIC X(10).
                04  W13-RSIPRT-COUNT   PIC S9(9)   BINARY.
                04  W13-RSIPRTLST      PIC X(10)   OCCURS  2 TIMES.
              03  W13-SYUTENTANKA-G      PIC S9(2).
              03  W13-SYUTENTANKA-N      PIC S9(2).
              03  W13-YKZTENTANKA-G      PIC S9(2).
              03  W13-YKZTENTANKA-N      PIC S9(2).
              03  W13-KOMUKBN-COMBO.
                04  W13-KOMUKBN           PIC X(10).
                04  W13-KOMUKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-KOMUKBNLST      PIC X(10)   OCCURS  2 TIMES.
              03  W13-KOMURSITANKAKBN-COMBO.
                04  W13-KOMURSITANKAKBN PIC X(30).
                04  W13-KOMURSITANKAKBN-COUNT
                                        PIC S9(9)   BINARY.
                04  W13-KOMURSITANKAKBNLST
                                        PIC X(30)   OCCURS  5 TIMES.
              03  W13-KOMURSITANKA      PIC S9(2)V9.
              03  W13-KOMURSITANKA-STATE PIC S9(9)   BINARY.
              03  W13-KOMUTENTANKA      PIC S9(2).
              03  W13-JBISKYKBN-COMBO.
                04  W13-JBISKYKBN           PIC X(10).
                04  W13-JBISKYKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-JBISKYKBNLST      PIC X(10)   OCCURS  2 TIMES.
              03  W13-DAISANGENBTKBN-COMBO.
                04  W13-DAISANGENBTKBN         PIC X(20).
                04  W13-DAISANGENBTKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-DAISANGENBTKBNLST PIC X(20)   OCCURS  5 TIMES.
              03  W13-KOMUSKYKBN-COMBO.
                04  W13-KOMUSKYKBN           PIC X(10).
                04  W13-KOMUSKYKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-KOMUSKYKBNLST      PIC X(10)   OCCURS  2 TIMES.
              03  W13-KYUGYOPRTKBN-COMBO.
                04  W13-KYUGYOPRTKBN           PIC X(50).
                04  W13-KYUGYOPRTKBN-COUNT   PIC S9(9)   BINARY.
                04  W13-KYUGYOPRTKBNLST      PIC X(50) OCCURS 2 TIMES.

