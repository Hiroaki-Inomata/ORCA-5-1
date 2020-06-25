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
        02  D04.
      *     02  D04-FIXED6.
              03  D04-B06-STATE        PIC S9(9)   BINARY.
              03  D04-B06C-STATE       PIC S9(9)   BINARY.
              03  D04-B07-STATE        PIC S9(9)   BINARY.
              03  D04-B07C-STATE       PIC S9(9)   BINARY.
              03  D04-B08-STATE        PIC S9(9)   BINARY.
              03  D04-B09-STATE        PIC S9(9)   BINARY.
              03  D04-B12-STATE        PIC S9(9)   BINARY.
              03  D04-LBLSRYYM-STYLE   PIC X(20).
              03  D04-LBLSRYYM         PIC X(30).
              03  D04-LBLNYUGAI-STYLE  PIC X(20).
              03  D04-LBLNYUGAI        PIC X(20).
              03  D04-CMB-CHKNUM.
                04  D04-CHKNUM             PIC X(43).
                04  D04-CHKNUM-COUNT       PIC S9(9)   BINARY.
                04  D04-CHKNUM-ITEM        PIC X(43)   OCCURS  50.
              03  D04-SELNUM               PIC S9(5).
              03  D04-PTNUM                PIC X(20).
              03  D04-NAME                 PIC X(100).
              03  D04-SEX                  PIC X(2).
              03  D04-BIRTHDAY             PIC X(10).
              03  D04-SRYNAME              PIC X(222).
              03  D04-CMB-BEHAVE.
                04  D04-BEHAVE             PIC X(42).
                04  D04-BEHAVE-STATE       PIC S9(9)   BINARY.
                04  D04-BEHAVE-COUNT       PIC S9(9)   BINARY.
                04  D04-BEHAVE-ITEM        PIC X(42)   OCCURS  5.
                04  D04-BEHAVE-ITEM-STATE  PIC S9(9)   BINARY.
              03  D04-BYOSELNUM            PIC S9(4).
              03  D04-BYOMEI               PIC X(100).
              03  D04-HOSOKU               PIC X(40).
              03  D04-CHKSELNUM            PIC S9(4).
              03  D04-SW1.
                04  D04-ERRLST.
                  05  D04-ERRLST-COUNT     PIC S9(9)   BINARY.
                  05  D04-ERRLST-ROW       PIC S9(9)   BINARY.
                  05  D04-ERRLST-ROWATTR   PIC S9(9)   BINARY.
                  05  D04-ERRLST-ITEM      OCCURS  200.
                    06  D04-TERRLSTNO      PIC X(5).
                    06  D04-TPTNUM         PIC X(20).
                    06  D04-TNAME          PIC X(100).
                    06  D04-THKNSBT        PIC X(20).
                    06  D04-TERRMSG        PIC X(220).
                  05  D04-ERRLST-SELECT    PIC X       OCCURS  200.
              03  D04-SW2.
                04  D04-BYOLST.
                  05  D04-BYOLST-TITLE     PIC X(50).
                  05  D04-BYOLST-COUNT     PIC S9(9)   BINARY.
                  05  D04-BYOLST-ROW       PIC S9(9)   BINARY.
                  05  D04-BYOLST-ROWATTR   PIC S9(9)   BINARY.
                  05  D04-BYOLST-ITEM      OCCURS  300.
                    06  D04-TBYOLSTNO      PIC X(05).
                    06  D04-TBYOMEI        PIC X(101).
                    06  D04-TSRYKA         PIC X(10).
                    06  D04-TSRYYMD        PIC X(10).
                    06  D04-TTENKIYMD      PIC X(10).
                    06  D04-TTENKIKBN      PIC X(10).
                    06  D04-TSYUBYO        PIC X(2).
                    06  D04-TUTAGAI        PIC X(14).
                    06  D04-TNYUGAIKBN     PIC X(2).
                    06  D04-THKNCOMBI      PIC X(40).
                  05  D04-BYOLST-SELECT    PIC X       OCCURS  300.
              03  D04-SW3.
                04  D04-CHKLST.
                  05  D04-CHKLST-LBLCHKMST PIC X(40).
                  05  D04-CHKLST-LBLCOMPKBN PIC X(10).
                  05  D04-CHKLST-COUNT     PIC S9(9)   BINARY.
                  05  D04-CHKLST-ROW       PIC S9(9)   BINARY.
                  05  D04-CHKLST-ROWATTR   PIC S9(9)   BINARY.
                  05  D04-CHKLST-ITEM      OCCURS  501.
                    06  D04-TCHKLSTNO-G.
                      07  D04-TCHKLSTNO    PIC X(4).
                      07  D04-TCHKGRPID    PIC X(1).
                    06  D04-TCHKBYOMEI     PIC X(101).
                    06  D04-TCOMPKBN       PIC X(12).
                  05  D04-CHKLST-SELECT    PIC X       OCCURS  501.

