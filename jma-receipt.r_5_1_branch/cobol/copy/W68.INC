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
       02  W68.
      *     02  W68-FIXED8.
              03  W68-B06-STATE                  PIC S9(9) BINARY.
              03  W68-B07-STATE                  PIC S9(9) BINARY.
              03  W68-B08-STATE                  PIC S9(9) BINARY.
              03  W68-KJNYMD                     PIC X(10).
              03  W68-SELNUM1                    PIC X(50).
              03  W68-HKNJANUM                   PIC X(08).
              03  W68-HKNJANUM-STATE             PIC S9(9) BINARY.
              03  W68-HKNJANAME                  PIC X(100).
              03  W68-HKNJANAME-STATE            PIC S9(9) BINARY.
              03  W68-CMB-HBTNUM10KBN.
                04  W68-HBTNUM10KBN              PIC X(80).
                04  W68-HBTNUM10KBN-COUNT        PIC S9(9) BINARY.
                04  W68-HBTNUM10KBN-ITEM         PIC X(80)   OCCURS  10.
              03  W68-CMB-HBTNUM10TAIKBN.
                04  W68-HBTNUM10TAIKBN           PIC X(40).
                04  W68-HBTNUM10TAIKBN-STATE     PIC S9(9) BINARY.
                04  W68-HBTNUM10TAIKBN-COUNT     PIC S9(9) BINARY.
                04  W68-HBTNUM10TAIKBN-ITEM      PIC X(40)   OCCURS  10.
                04  W68-HBTNUM10TAIKBN-ITEM-STATE PIC S9(9) BINARY.
              03  W68-CMB-HBTNUM11KBN.
                04  W68-HBTNUM11KBN              PIC X(80).
                04  W68-HBTNUM11KBN-COUNT        PIC S9(9) BINARY.
                04  W68-HBTNUM11KBN-ITEM         PIC X(80)   OCCURS  10.
              03  W68-CMB-HBTNUM11TAIKBN.
                04  W68-HBTNUM11TAIKBN           PIC X(40).
                04  W68-HBTNUM11TAIKBN-STATE     PIC S9(9) BINARY.
                04  W68-HBTNUM11TAIKBN-COUNT     PIC S9(9) BINARY.
                04  W68-HBTNUM11TAIKBN-ITEM      PIC X(40)   OCCURS  10.
                04  W68-HBTNUM11TAIKBN-ITEM-STATE PIC S9(9) BINARY.
              03  W68-CMB-HBTNUM20KBN.
                04  W68-HBTNUM20KBN              PIC X(80).
                04  W68-HBTNUM20KBN-COUNT        PIC S9(9) BINARY.
                04  W68-HBTNUM20KBN-ITEM         PIC X(80)   OCCURS  10.
              03  W68-CMB-HBTNUM20TAIKBN.
                04  W68-HBTNUM20TAIKBN           PIC X(40).
                04  W68-HBTNUM20TAIKBN-STATE     PIC S9(9) BINARY.
                04  W68-HBTNUM20TAIKBN-COUNT     PIC S9(9) BINARY.
                04  W68-HBTNUM20TAIKBN-ITEM      PIC X(40)   OCCURS  10.
                04  W68-HBTNUM20TAIKBN-ITEM-STATE PIC S9(9) BINARY.
              03  W68-CMB-HBTNUM21KBN.
                04  W68-HBTNUM21KBN              PIC X(80).
                04  W68-HBTNUM21KBN-COUNT        PIC S9(9) BINARY.
                04  W68-HBTNUM21KBN-ITEM         PIC X(80)   OCCURS  10.
              03  W68-CMB-HBTNUM21TAIKBN.
                04  W68-HBTNUM21TAIKBN           PIC X(40).
                04  W68-HBTNUM21TAIKBN-STATE     PIC S9(9) BINARY.
                04  W68-HBTNUM21TAIKBN-COUNT     PIC S9(9) BINARY.
                04  W68-HBTNUM21TAIKBN-ITEM      PIC X(40)   OCCURS  10.
                04  W68-HBTNUM21TAIKBN-ITEM-STATE PIC S9(9) BINARY.
              03  W68-YUKOSTYMD                  PIC X(10).
              03  W68-YUKOSTYMD-STATE            PIC S9(9) BINARY.
              03  W68-YUKOEDYMD                  PIC X(10).
              03  W68-YUKOEDYMD-STATE            PIC S9(9) BINARY.
              03  W68-SELNUM2 PIC S9(2).
              03  W68-SELNUM2-STATE              PIC S9(9) BINARY.
              03  W68-LBLMODE-STYLE              PIC X(20).
              03  W68-LBLMODE                    PIC X(40).
              03  W68-SCRW1.
                04  W68-HKNJALST.
                  05  W68-HKNJALST-COUNT         PIC S9(9) BINARY.
                  05  W68-HKNJALST-ROW           PIC S9(9) BINARY.
                  05  W68-HKNJALST-ROWATTR       PIC S9(9) BINARY.
                  05  W68-HKNJALST-ITEM          OCCURS  200.
                    06  W68-TNO                  PIC X(4).
                    06  W68-THKNJANUM            PIC X(8).
                    06  W68-THKNJANAME           PIC X(100).
                    06  W68-TYUKSTYMD            PIC X(10).
                    06  W68-TYUKEDYMD            PIC X(10).
                  05  W68-HKNJALST-SELECT        PIC X  OCCURS  200.
              03  W68-SCRW29.
                04  W68-RIREKILST.
                  05  W68-RIREKILST-COUNT   PIC S9(9) BINARY.
                  05  W68-RIREKILST-ROW     PIC S9(9) BINARY.
                  05  W68-RIREKILST-ROWATTR PIC S9(9) BINARY.
                  05  W68-RIREKILST-ITEM    OCCURS  20.
                    06  W68-TRNO            PIC X(3).
                    06  W68-TRKIKAN         PIC X(30).
                  05  W68-RIREKILST-SELECT  PIC X   OCCURS  20.
