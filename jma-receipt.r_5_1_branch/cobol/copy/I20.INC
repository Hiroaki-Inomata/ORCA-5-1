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
      *****************************************************************
            02  I20.
              03  I20-B06-STATE PIC S9(9)   BINARY.
              03  I20-B07-STATE PIC S9(9)   BINARY.
              03  I20-B10-STATE PIC S9(9)   BINARY.
              03  I20-B11-STATE PIC S9(9)   BINARY.
              03  I20-PTNUM.
                04  I20-VALUE   PIC X(20).
              03  I20-STNYUINYMD.
                04  I20-VALUE   PIC X(10).
              03  I20-NAME.
                04  I20-VALUE   PIC X(30).
              03  I20-EDNYUINYMD.
                04  I20-VALUE   PIC X(10).
              03  I20-STTAIINYMD.
                04  I20-VALUE   PIC X(10).
              03  I20-EDTAIINYMD.
                04  I20-VALUE   PIC X(10).
              03  I20-ADRS.
                04  I20-VALUE   PIC X(100).
              03  I20-SELNUM.
                04  I20-VALUE   PIC S9(3).
              03  I20-EDAGECOMBO.
                04  I20-EDAGE       PIC X(13).
                04  I20-EDAGE-COUNT PIC S9(9)   BINARY.
                04  I20-EDAGELST    PIC X(13)   OCCURS  14 TIMES.
              03  I20-STAGECOMBO.
                04  I20-STAGE       PIC X(13).
                04  I20-STAGE-COUNT PIC S9(9)   BINARY.
                04  I20-STAGELST    PIC X(13)   OCCURS  14 TIMES.
              03  I20-SRYKA-COMBO.
                04  I20-SRYKA       PIC X(23).
                04  I20-SRYKA-COUNT PIC S9(9)   BINARY.
                04  I20-SRYKALST    PIC X(23)   OCCURS  99 TIMES.
              03  I20-BTUNUMCOMBO.
                04  I20-BTUNUM          PIC X(23).
                04  I20-BTUNUM-COUNT    PIC S9(9)   BINARY.
                04  I20-BTUNUMLST       PIC X(23)   OCCURS  50 TIMES.
              03  I20-JOTAICOMBO.
                04  I20-JOTAI           PIC X(14).
                04  I20-JOTAI-COUNT     PIC S9(9)   BINARY.
                04  I20-JOTAILST        PIC X(14)   OCCURS  5 TIMES.
              03  I20-CHKTESTPT         PIC X(01).
              03  I20-TAIINCDCOMBO.
                04  I20-TAIINCD         PIC X(23).
                04  I20-TAIINCD-COUNT   PIC S9(9)   BINARY.
                04  I20-TAIINCDLST      PIC X(23)   OCCURS  50 TIMES.
              03  I20-DEATHKBNCOMBO.
                04  I20-DEATHKBN         PIC X(10).
                04  I20-DEATHKBN-COUNT   PIC S9(9)   BINARY.
                04  I20-DEATHKBNLST      PIC X(10)   OCCURS  5 TIMES.
              03  I20-SRT1KBNCOMBO.
                04  I20-SRT1KBN         PIC X(22).
                04  I20-SRT1KBN-COUNT   PIC S9(9)   BINARY.
                04  I20-SRT1KBNLST      PIC X(22)   OCCURS  10 TIMES.
              03  I20-SRT2KBNCOMBO.
                04  I20-SRT2KBN         PIC X(22).
                04  I20-SRT2KBN-COUNT   PIC S9(9)   BINARY.
                04  I20-SRT2KBNLST      PIC X(22)   OCCURS  10 TIMES.
              03  I20-SRT3KBNCOMBO.
                04  I20-SRT3KBN         PIC X(22).
                04  I20-SRT3KBN-COUNT   PIC S9(9)   BINARY.
                04  I20-SRT3KBNLST      PIC X(22)   OCCURS  10 TIMES.
              03  I20-KJNYMDMES         PIC X(10).
              03  I20-TOTAL             PIC X(22).
              03  I20-SCROLLEDWINDOW28.
                04  I20-KANJYALST.
                  05  I20-COUNT        PIC S9(9)   BINARY.
                  05  I20-ROW          PIC S9(9)   BINARY.
                  05  I20-ROWATTR      PIC S9(9)   BINARY.
                  05  I20-ITEM        OCCURS  200 TIMES.
                    06  I20-TRENNUM    PIC X(3).
                    06  I20-TPTNUM     PIC X(20).
                    06  I20-TNAME      PIC X(30).
                    06  I20-TSEX       PIC X(2).
                    06  I20-TAGE       PIC X(8).
                    06  I20-THKN       PIC X(50).
                    06  I20-TBTUNAME   PIC X(20).
                    06  I20-TBRMNUM    PIC X(8).
                    06  I20-TNYUINYMD  PIC X(9).
                    06  I20-TTAIINYMD  PIC X(9).
                    06  I20-TTAIINCD   PIC X(10).
                    06  I20-TSRYKA     PIC X(20).
                    06  I20-TPOST      PIC X(08).
                    06  I20-TADRS      PIC X(50).
                  05  I20-SELECT  PIC X       OCCURS  200 TIMES.
