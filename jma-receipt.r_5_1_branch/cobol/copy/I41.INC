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
        02  I41.
          03  I41-B05.
            04  I41-B05-LABEL   PIC X(10).
          03  I41-B05S-LABEL    PIC X(10).
          03  I41-B05S-STATE    PIC S9(9)   BINARY.
          03  I41-PTNUM.
            04  I41-VALUE       PIC X(20).
          03  I41-SRYYM.
            04  I41-VALUE       PIC X(6).
          03  I41-KANANAME.
            04  I41-VALUE       PIC X(50).
          03  I41-NAME.
            04  I41-VALUE       PIC X(50).
          03  I41-BIRTHDAY.
            04  I41-VALUE       PIC X(9).
          03  I41-SEX.
            04  I41-VALUE       PIC X(2).
          03  I41-IJYOU91-G.
            04  I41-IJYOU91-STYLE     PIC X(20).
            04  I41-IJYOU91     PIC X(9).
          03  I41-IJYOU180.
            04  I41-VALUE       PIC X(9).
          03  I41-NYUINBI.
            04  I41-VALUE       PIC X(9).
          03  I41-TAINBI.
            04  I41-VALUE       PIC X(9).
          03  I41-NYUINNISSU    PIC S9(6).
          03  I41-NUM.
            04  I41-VALUE       PIC X(3).
          03  I41-MEISYO.
            04  I41-VALUE       PIC X(50).
          03  I41-KEIKOKU-G.
            04  I41-KEIKOKU-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-KEIKOKU.
              05  I41-VALUE       PIC X(60).
          03  I41-KOMENTO.
            04  I41-VALUE       PIC X(50).
          03  I41-LDAY-OCC      OCCURS 31.
            04  I41-LDAY-STYLE  PIC X(20).
            04  I41-LDAY        PIC X(02).
          03  I41-DAY-OCC       OCCURS 31.
            04  I41-DAY         PIC X(03).
          03  I41-SCROLLEDWINDOW1.
            04  I41-DATELIST.
              05  I41-DATELIST-COUNT   PIC S9(9)   BINARY.
              05  I41-DATELIST-ROW     PIC S9(9)   BINARY.
              05  I41-DATELIST-ROWATTR PIC S9(9)   BINARY.
              05  I41-DATELIST-LBLDAY-OCC  OCCURS  31.
                06  I41-TLBLDAY-STYLE  PIC X(20).
              05  I41-DATELIST-ITEM    OCCURS  400 TIMES.
                06  I41-TNO            PIC X(3).
                06  I41-TMEISYO        PIC X(60).
                06  I41-TTENSU         PIC X(7).
                06  I41-TDAY-G.
                  08  I41-TDAY         PIC X(2)  OCCURS  31.
              05  I41-DATELIST-SELECT  PIC X     OCCURS  400 TIMES.
          03  I41-SCROLLEDWINDOW2.
            04  I41-KUMILIST.
              05  I41-KUMILIST-COUNT   PIC S9(9)   BINARY.
              05  I41-KUMILIST-ROW     PIC S9(9)   BINARY.
              05  I41-KUMILIST-ROWATTR PIC S9(9)   BINARY.
              05  I41-KUMI-ITEM        OCCURS  30 TIMES.
                06  I41-KUMINO         PIC X(4).
                06  I41-KUMI           PIC X(50).
              05  I41-HEYALIST-SELECT  PIC X      OCCURS  30 TIMES.
          03  I41-SCROLLEDWINDOW3.
            04  I41-SAGAKULIST.
              05  I41-SAGAKU-TITLE     PIC X(22).
              05  I41-SAGAKULIST-COUNT PIC S9(9)   BINARY.
              05  I41-SAGAKULIST-ROW     PIC S9(9)   BINARY.
              05  I41-SAGAKULIST-ROWATTR PIC S9(9)   BINARY.
              05  I41-SAGAKU-ITEM      OCCURS  99 TIMES.
                06  I41-SAGAKUNO       PIC X(4).
                06  I41-SAGAKU         PIC X(22).
              05  I41-SAGAKULIST-SELECT  PIC X      OCCURS  99 TIMES.
          03  I41-TSUSANNISSU     PIC S9(6).
          03  I41-SYUBETSU1-G.
            04  I41-SYUBETSU1-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-SYUBETSU1.
              05  I41-VALUE       PIC X(20).
          03  I41-SYUBETSU2-G.
            04  I41-SYUBETSU2-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-SYUBETSU2.
              05  I41-VALUE       PIC X(20).
          03  I41-SYUBETSU3-G.
            04  I41-SYUBETSU3-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-SYUBETSU3.
              05  I41-VALUE       PIC X(20).
          03  I41-SYUBETSU4-G.
            04  I41-SYUBETSU4-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-SYUBETSU4.
              05  I41-VALUE       PIC X(20).
          03  I41-SYUBETSU5-G.
            04  I41-SYUBETSU5-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-SYUBETSU5.
              05  I41-VALUE       PIC X(20).
          03  I41-SYUBETSU6-G.
            04  I41-SYUBETSU6-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-SYUBETSU6.
              05  I41-VALUE       PIC X(20).
          03  I41-LASTNYUIN-G.
            04  I41-LASTNYUIN-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-LASTNYUIN.
              05  I41-VALUE       PIC X(40).
          03  I41-AGE.
            04  I41-VALUE         PIC X(9).
          03  I41-NYUINKA-G.
            04  I41-NYUINKA-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-NYUINKA.
              05  I41-VALUE       PIC X(40).
          03  I41-UPYMD-G.
            04  I41-UPYMD-STYLE.
              05  I41-VALUE       PIC X(20).
            04  I41-UPYMD.
              05  I41-VALUE       PIC X(20).
