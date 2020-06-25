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
            02  C03.
              03  C03-BANGO.
                04  C03-VALUE   PIC S9(5).
              03  C03-BYOMEICD      OCCURS  6.
                04  C03-VALUE   PIC X(8).
              03  C03-BYOMEI.
                04  C03-VALUE   PIC X(80).
              03  C03-HOSOKU-COMMENT.
                04  C03-VALUE   PIC X(40).
              03  C03-CHARTBYOMEI.
                04  C03-VALUE   PIC X(80).
              03  C03-BYOMEIHENFLG.
                04  C03-VALUE   PIC X(10).
              03  C03-SCR-ACTLST.
                04  C03-BUNRUILST.
                  05  C03-BUNRUI-COUNT   PIC S9(9)   BINARY.
                  05  C03-BUNRUI-ROW     PIC S9(9)   BINARY.
                  05  C03-BUNRUI-ROWATTR PIC S9(9)   BINARY.
                  05  C03-ITEM           OCCURS  100 TIMES.
                    06  C03-BBUNRUICD    PIC X(4).
                    06  C03-BBUNRUIMEI   PIC X(30).
                  05  C03-BUNRUI-SELECT  PIC X       OCCURS  100 TIMES.
              03  C03-BUNRUICD.
                04  C03-VALUE   PIC S9(3).
              03  C03-BUNRUIMEI.
                04  C03-VALUE   PIC X(30).
              03  C03-DSPSEQ.
                04  C03-VALUE   PIC S9(2).
              03  C03-SCROLLEDWINDOW28.
                04  C03-BYOMEILST.
                  05  C03-BYOMEILST-COUNT   PIC S9(9)   BINARY.
                  05  C03-BYOMEILST-ROW     PIC S9(9)   BINARY.
                  05  C03-BYOMEILST-ROWATTR PIC S9(9)   BINARY.
                  05  C03-ITEM        OCCURS  100 TIMES.
                    06  C03-TBANGO          PIC X(5).
                    06  C03-TBUNRUIMEI      PIC X(16).
                    06  C03-TBYOMEIINPUTCD  PIC X(10).
                    06  C03-TBYOMEISBT      PIC X(6).
                    06  C03-TBYOMEI         PIC X(130).
                    06  C03-TCHARTBYOMEI    PIC X(80).
                    06  C03-TUTAGAIFLG      PIC X(14).
                    06  C03-TMANSEIFLG      PIC X(42).
                    06  C03-THKNBYOMEIFLG   PIC X(8).
                    06  C03-TDSPSEQ         PIC X(4).
                  05  C03-BYOMEILST-SELECT  PIC X   OCCURS  100 TIMES.
                  05  C03-BYOMEILST-FGCOLOR PIC X(16) OCCURS  100 TIMES.
                  05  C03-BYOMEILST-BGCOLOR PIC X(16) OCCURS  100 TIMES.
              03  C03-UTAGAIFLGCOMBO.
                04  C03-UTAGAIFLG.
                  05  C03-VALUE   PIC X(14).
                04  C03-UTAGAIFLG-COUNT     PIC S9(9)   BINARY.
                04  C03-UTAGAIFLG-ITEM      PIC X(14)   OCCURS  3 TIMES.
              03  C03-BYOMEIINPUTCD OCCURS  3.
                04  C03-VALUE   PIC X(10).
              03  C03-HKNBYOMEIFLGCOMBO.
                04  C03-HKNBYOMEIFLG.
                  05  C03-VALUE   PIC X(10).
                04  C03-HKNBYOMEIFLG-COUNT  PIC S9(9) BINARY.
                04  C03-HKNBYOMEIFLG-ITEM   PIC X(10) OCCURS  2 TIMES.
              03  C03-BYOMEISBTCOMBO.
                04  C03-BYOMEISBT.
                  05  C03-VALUE   PIC X(8).
                04  C03-BYOMEISBT-COUNT     PIC S9(9) BINARY.
                04  C03-BYOMEISBT-ITEM      PIC X(8) OCCURS  4 TIMES.
              03  C03-MANSEIFLGCOMBO.
                04  C03-MANSEIFLG.
                  05  C03-VALUE   PIC X(46).
                04  C03-MANSEIFLG-COUNT     PIC S9(9)   BINARY.
                04  C03-MANSEIFLG-ITEM      PIC X(46)   OCCURS  6 TIMES.
              03  C03-HYOUJI-STYLE   PIC X(20).
              03  C03-HYOUJI         PIC X(22).
