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
        02  W17.
      *      02  W17-FIXED1.
            03  W17-B06-STATE               PIC S9(9)   BINARY.
            03  W17-CLISTSCROLL.
                04  W17-CHECKLIST.
                    05  W17-CLIST-COUNT     PIC S9(9)   BINARY.
                    05  W17-CLIST-ROW       PIC S9(9)   BINARY.
                    05  W17-CLIST-ROWATTR   PIC S9(9)   BINARY.
                    05  W17-CLIST-ITEM              OCCURS  100 TIMES.
                        06  W17-CLIST-NUML      PIC X(3).
                        06  W17-CLIST-CDL       PIC X(4).
                        06  W17-CLIST-KOUHIL    PIC X(21).
                        06  W17-CLIST-FTNNUML   PIC X(15).
                        06  W17-CLIST-JKSNUML   PIC X(15).
                        06  W17-CLIST-STYMDL    PIC X(10).
                        06  W17-CLIST-EDYMDL    PIC X(10).
                    05  W17-CLIST-SELECT    PIC X   OCCURS  100 TIMES.
            03  W17-SELNUM              PIC S9(2).
            03  W17-KOUHICOMBO.
                04  W17-KOUHI           PIC X(24).
                04  W17-KOUHI-COUNT     PIC S9(9)   BINARY.
                04  W17-KOUHI-ITEM      PIC X(24)   OCCURS  99 TIMES.
            03  W17-FTSNUMCOMBO.
                04  W17-FTSNUM          PIC X(16).
                04  W17-FTSNUM-COUNT    PIC S9(9)   BINARY.
                04  W17-FTSNUM-ITEM     PIC X(16)   OCCURS  2 TIMES.
            03  W17-JKSNUMCOMBO.
                04  W17-JKSNUM          PIC X(16).
                04  W17-JKSNUM-COUNT    PIC S9(9)   BINARY.
                04  W17-JKSNUM-ITEM     PIC X(16)   OCCURS  2 TIMES.
            03  W17-FTSHANKAKUCOMBO.
                04  W17-FTSHANKAKU       PIC X(20).
                04  W17-FTSHANKAKU-COUNT PIC S9(9)   BINARY.
                04  W17-FTSHANKAKU-ITEM  PIC X(20)   OCCURS  5 TIMES.
            03  W17-FTSZENKAKUCOMBO.
                04  W17-FTSZENKAKU       PIC X(20).
                04  W17-FTSZENKAKU-COUNT PIC S9(9)   BINARY.
                04  W17-FTSZENKAKU-ITEM  PIC X(20)   OCCURS  5 TIMES.
            03  W17-FTSNUMERICCOMBO.
                04  W17-FTSNUMERIC       PIC X(20).
                04  W17-FTSNUMERIC-COUNT PIC S9(9)   BINARY.
                04  W17-FTSNUMERIC-ITEM  PIC X(20)   OCCURS  5 TIMES.
            03  W17-FTSKETACOMBO.
                04  W17-FTSKETA          PIC X(20).
                04  W17-FTSKETA-COUNT    PIC S9(9)   BINARY.
                04  W17-FTSKETA-ITEM     PIC X(20)   OCCURS  5 TIMES.
            03  W17-FTSKETAMINCOMBO.
                04  W17-FTSKETAMIN       PIC X(19).
                04  W17-FTSKETAMIN-STATE PIC S9(9)   BINARY.
                04  W17-FTSKETAMIN-COUNT PIC S9(9)   BINARY.
                04  W17-FTSKETAMIN-ITEM  PIC X(19)   OCCURS  10 TIMES.
                04  W17-FTSKETAMIN-ITEM-STATE
                                         PIC S9(9)   BINARY.
            03  W17-JKSHANKAKUCOMBO.
                04  W17-JKSHANKAKU       PIC X(20).
                04  W17-JKSHANKAKU-COUNT PIC S9(9)   BINARY.
                04  W17-JKSHANKAKU-ITEM  PIC X(20)   OCCURS  5 TIMES.
            03  W17-JKSZENKAKUCOMBO.
                04  W17-JKSZENKAKU       PIC X(20).
                04  W17-JKSZENKAKU-COUNT PIC S9(9)   BINARY.
                04  W17-JKSZENKAKU-ITEM  PIC X(20)   OCCURS  5 TIMES.
            03  W17-JKSNUMERICCOMBO.
                04  W17-JKSNUMERIC       PIC X(20).
                04  W17-JKSNUMERIC-COUNT PIC S9(9)   BINARY.
                04  W17-JKSNUMERIC-ITEM  PIC X(20)   OCCURS  5 TIMES.
            03  W17-JKSKETACOMBO.
                04  W17-JKSKETA          PIC X(20).
                04  W17-JKSKETA-COUNT    PIC S9(9)   BINARY.
                04  W17-JKSKETA-ITEM     PIC X(20)   OCCURS  5 TIMES.
            03  W17-JKSKETAMINCOMBO.
                04  W17-JKSKETAMIN       PIC X(19).
                04  W17-JKSKETAMIN-STATE PIC S9(9)   BINARY.
                04  W17-JKSKETAMIN-COUNT PIC S9(9)   BINARY.
                04  W17-JKSKETAMIN-ITEM  PIC X(19)   OCCURS  20 TIMES.
                04  W17-JKSKETAMIN-ITEM-STATE
                                         PIC S9(9)   BINARY.
            03  W17-YUKOSTYMD       PIC X(10).
            03  W17-YUKOEDYMD       PIC X(10).
