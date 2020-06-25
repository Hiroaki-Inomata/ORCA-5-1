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
        02  P02D.
      *****  02  P02D-FIXED10.
              03  P02D-KOHNUM2.
                04  P02D-VALUE  PIC X(9).
              03  P02D-JIGYOUNAME.
                04  P02D-VALUE  PIC X(80).
              03  P02D-HIHKNJANAME.
                04  P02D-VALUE  PIC X(50).
              03  P02D-SHOBYOYMD.
                04  P02D-VALUE  PIC X(10).
              03  P02D-KOHNUM1.
                04  P02D-VALUE  PIC X(14).
              03  P02D-SINSATUYMD.
                04  P02D-VALUE  PIC X(10).
              03  P02D-KOHNUM3.
                04  P02D-VALUE  PIC X(13).
              03  P02D-RYOEDYMD.
                04  P02D-VALUE  PIC X(10).
              03  P02D-RYOSTYMD.
                04  P02D-VALUE  PIC X(10).
              03  P02D-CITYNAME.
                04  P02D-VALUE  PIC X(20).
              03  P02D-HKNKBNCOMBO.
                04  P02D-HKNKBN.
                  05  P02D-VALUE  PIC X(16).
                04  P02D-HKNKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-HKNKBN-ITEM   PIC X(16)   OCCURS  9 TIMES.
              03  P02D-COMMENT.
                04  P02D-VALUE  PIC X(100).
              03  P02D-SONSHOUKBNCOMBO.
                04  P02D-SONSHOUKBN.
                  05  P02D-VALUE  PIC X(40).
                04  P02D-SONSHOUKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-SONSHOUKBN-ITEM   PIC X(40)   OCCURS  50 TIMES.
              03  P02D-PREFNAMECOMBO.
                04  P02D-PREFNAME.
                  05  P02D-VALUE  PIC X(20).
                04  P02D-PREFNAME-COUNT  PIC S9(9)   BINARY.
                04  P02D-PREFNAME-ITEM   PIC X(20)   OCCURS  47 TIMES.
              03  P02D-PREFKBNCOMBO.
                04  P02D-PREFKBN.
                  05  P02D-VALUE  PIC X(4).
                04  P02D-PREFKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-PREFKBN-ITEM   PIC X(4)    OCCURS  4 TIMES.
              03  P02D-CITYKBNCOMBO.
                04  P02D-CITYKBN.
                  05  P02D-VALUE  PIC X(4).
                04  P02D-CITYKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-CITYKBN-ITEM   PIC X(4)    OCCURS  3 TIMES.
              03  P02D-SISIKBNCOMBO.
                04  P02D-SISIKBN.
                  05  P02D-VALUE  PIC X(8).
                04  P02D-SISIKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-SISIKBN-ITEM   PIC X(8)    OCCURS  3 TIMES.
              03  P02D-SINKEICOMBO.
                04  P02D-SINKEI.
                  05  P02D-VALUE  PIC X(10).
                04  P02D-SINKEI-COUNT  PIC S9(9)   BINARY.
                04  P02D-SINKEI-ITEM   PIC X(10)   OCCURS  4 TIMES.
              03  P02D-TENKICOMBO.
                04  P02D-TENKI.
                  05  P02D-VALUE  PIC X(8).
                04  P02D-TENKI-COUNT  PIC S9(9)   BINARY.
                04  P02D-TENKI-ITEM   PIC X(8)    OCCURS  5 TIMES.
              03  P02D-SAIGAICOB.
                04  P02D-SAIGAI.
                  05  P02D-VALUE  PIC X(20).
                04  P02D-SAIGAI-COUNT  PIC S9(9)   BINARY.
                04  P02D-SAIGAI-ITEM   PIC X(20)   OCCURS  3 TIMES.
              03  P02D-SINSATUEND.
                04  P02D-VALUE  PIC X(10).
              03  P02D-JIBAISEIKBNCOB.
                04  P02D-JIBAISEIKBN.
                  05  P02D-VALUE  PIC X(30).
                04  P02D-JIBAISEIKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-JIBAISEIKBN-ITEM   PIC X(30)   OCCURS  2 TIMES.
              03  P02D-SRYKA-COMB.
                04  P02D-SRYKA.
                  05  P02D-VALUE  PIC X(15).
                04  P02D-SRYKA-COUNT  PIC S9(9)   BINARY.
                04  P02D-SRYKA-ITEM
                        PIC X(15)   OCCURS  99 TIMES.
              03  P02D-DRCD-COMB.
                04  P02D-DRCD.
                  05  P02D-VALUE  PIC X(29).
                04  P02D-DRCD-COUNT  PIC S9(9)   BINARY.
                04  P02D-DRCD-ITEM
                        PIC X(29)   OCCURS  99 TIMES.
              03  P02D-HOKENNAME.
                04  P02D-VALUE  PIC X(80).
              03  P02D-KIJYUNYM.
                04  P02D-VALUE  PIC X(6).
              03  P02D-KAISU.
                04  P02D-VALUE  PIC S9(3).
              03  P02D-TANTOIMEI.
                04  P02D-VALUE  PIC X(20).
              03  P02D-SEIKBNMEI.
                04  P02D-VALUE  PIC X(20).
              03  P02D-SYOCD.
                04  P02D-VALUE  PIC X(5).
              03  P02D-SYOCDNAME.
                04  P02D-VALUE  PIC X(26).
              03  P02D-SCROLLEDWINDOW1.
                04  P02D-HPOS   PIC S9(9)   BINARY.
                04  P02D-VPOS   PIC S9(9)   BINARY.
                04  P02D-RSILIST.
                  05  P02D-RSILIST-COUNT  PIC S9(9)   BINARY.
                  05  P02D-RSILIST-ROW    PIC S9(9)   BINARY.
                  05  P02D-ITEM       OCCURS  50 TIMES.
                   06  P02D-RSINUM    PIC X(3).
                    06  P02D-RSINAME  PIC X(16).
                    06  P02D-RSIKIKAN PIC X(20).
                  05  P02D-RSILIST-SELECT
                          PIC X       OCCURS  50 TIMES.
              03  P02D-SELNUM.
                04  P02D-VALUE  PIC S9(3).
              03  P02D-DAI3-GENBTKBN-COMB.
                04  P02D-DAI3-GENBTKBN.
                  05  P02D-TEXTDATA   PIC X(20).
                04  P02D-DAI3-GENBTKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-DAI3-GENBTKBN-ITEM
                        PIC X(20)   OCCURS  5 TIMES.
              03  P02D-DAI3-TOKKIKBN-COMB.
                04  P02D-DAI3-TOKKIKBN.
                  05  P02D-TEXTDATA   PIC X(30).
                04  P02D-DAI3-TOKKIKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-DAI3-TOKKIKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
              03  P02D-JIBAIKBN-COMB.
                04  P02D-JIBAIKBN.
                  05  P02D-TEXTDATA   PIC X(40).
                04  P02D-JIBAIKBN-COUNT  PIC S9(9)   BINARY.
                04  P02D-JIBAIKBN-ITEM
                        PIC X(40)   OCCURS  5 TIMES.



