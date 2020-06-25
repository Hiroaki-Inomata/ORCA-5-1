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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        02  X03.
      *      02  X03-FIXED6.
              03  X03-B09-STATE PIC S9(9)   BINARY.
              03  X03-B09-2-STATE PIC S9(9)   BINARY.
              03  X03-B10-STATE PIC S9(9)   BINARY.
              03  X03-B10-2-STATE PIC S9(9)   BINARY.
              03  X03-BTN-INS-STATE PIC S9(9)   BINARY.
              03  X03-LCHKKBNMEI.
                04  X03-VALUE   PIC X(70).
              03  X03-LYUKOSTYMD.
                04  X03-VALUE   PIC X(9).
              03  X03-LYUKOEDYMD.
                04  X03-VALUE   PIC X(9).
              03  X03-LNAME.
                04  X03-VALUE   PIC X(70).
              03  X03-SCROLLEDWINDOW1.
                04  X03-LCHKLST.
                  05  X03-LBLLJYKN        PIC X(06).
                  05  X03-LCHKLST-COUNT   PIC S9(9)   BINARY.
                  05  X03-LCHKLST-ROW     PIC S9(9)   BINARY.
                  05  X03-LCHKLST-ROWATTR PIC S9(9)   BINARY.
                  05  X03-LCHKLST-ITEM    OCCURS  600 TIMES.
                    06  X03-TLCHK-NUM         PIC X(6).
                    06  X03-TLCHK-NAME        PIC X(150).
                    06  X03-TLCHK-JYKN        PIC X(12).
                  05  X03-LCHKLST-SELECT
                          PIC X       OCCURS  600 TIMES.
              03  X03-RCHKKBNMEI.
                04  X03-VALUE   PIC X(70).
              03  X03-RYUKOSTYMD.
                04  X03-VALUE   PIC X(9).
              03  X03-RYUKOEDYMD.
                04  X03-VALUE   PIC X(9).
              03  X03-RNAME.
                04  X03-VALUE   PIC X(70).
              03  X03-SCROLLEDWINDOW3.
                04  X03-RCHKLST.
                  05  X03-LBLRJYKN        PIC X(06).
                  05  X03-RCHKLST-COUNT   PIC S9(9)   BINARY.
                  05  X03-RCHKLST-ROW     PIC S9(9)   BINARY.
                  05  X03-RCHKLST-ROWATTR PIC S9(9)   BINARY.
                  05  X03-RCHKLST-ITEM        OCCURS  400 TIMES.
                    06  X03-TRCHK-NUM     PIC X(6).
                    06  X03-TRCHK-NAME    PIC X(150).
                    06  X03-TRCHK-JYKN    PIC X(12).
                  05  X03-RCHKLST-SELECT
                          PIC X       OCCURS  400 TIMES.
              03  X03-CMB-CHKKBN.
                04  X03-CHKKBN.
                  05  X03-VALUE   PIC X(72).
                04  X03-CHKKBN-COUNT   PIC S9(9)   BINARY.
                04  X03-CHKKBN-ITEM
                        PIC X(72)   OCCURS  5 TIMES.
              03  X03-SELNAME.
                04  X03-VALUE             PIC X(70).
              03  X03-SCROLLEDWINDOW2.
                04  X03-SELCHKLST.
                  05  X03-SELCHKLST-COUNT   PIC S9(9)   BINARY.
                  05  X03-SELCHKLST-ROW     PIC S9(9)   BINARY.
                  05  X03-SELCHKLST-ROWATTR PIC S9(9)   BINARY.
                  05  X03-SELCHKLST-ITEM        OCCURS  100 TIMES.
                    06  X03-TSEL-CHKNUM  PIC X(4).
                    06  X03-TSEL-NAME PIC X(70).
                    06  X03-TSEL-YUKOSTYMD   PIC X(9).
                    06  X03-TSEL-YUKOEDYMD   PIC X(9).
                  05  X03-SELCHKLST-SELECT
                          PIC X       OCCURS  100 TIMES.
              03  X03-KJNYMD.
                04  X03-VALUE   PIC X(10).
