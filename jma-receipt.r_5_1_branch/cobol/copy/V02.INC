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
        02  V02.
      ******02  V02-FIXED12.
              03  V02-B03.
                04  V02-B03-STATE   PIC S9(9)   BINARY.
              03  V02-STYUKYMD.
                04  V02-VALUE   PIC X(9).
              03  V02-EDYUKYMD.
                04  V02-VALUE   PIC X(9).
              03  V02-NOTEBOOK3.
                04  V02-PAGENO  PIC S9(9)   BINARY.
                04  V02-FIXED23-G   OCCURS  4.
                  05  V02-GAIYKZFTNKBN-CMB.
                    06  V02-GAIYKZFTNKBN.
                      07  V02-VALUE   PIC X(26).
                    06  V02-GAIYKZFTNKBN-COUNT   PIC S9(9)   BINARY.
                    06  V02-GAIYKZFTNKBNLST    PIC X(26)   OCCURS  4.
                  05  V02-GAIFTNKBN-CMB.
                    06  V02-GAIFTNKBN.
                      07  V02-VALUE   PIC X(22).
                    06  V02-GAIFTNKBN-COUNT   PIC S9(9)   BINARY.
                    06  V02-GAIFTNKBNLST    PIC X(22)   OCCURS  4.
                  05  V02-GAIKFTNRATE.
                    06  V02-VALUE   PIC S9(3).
                  05  V02-GAIKKOTGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-GAIKJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-GAIDJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-GAIMNAIJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-GAIMGAIJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-GAIDJGNCNT.
                    06  V02-VALUE   PIC S9(2).
                  05  V02-GAIMJGNCNT.
                    06  V02-VALUE   PIC S9(2).
                  05  V02-NYUFTNKBN-CMB.
                    06  V02-NYUFTNKBN.
                      07  V02-VALUE   PIC X(22).
                    06  V02-NYUFTNKBN-COUNT   PIC S9(9)   BINARY.
                    06  V02-NYUFTNKBNLST      PIC X(22)   OCCURS  4.
                  05  V02-NYUKFTNRATE.
                    06  V02-VALUE   PIC S9(3).
                  05  V02-NYUKKOTGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-NYUKJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-NYUDJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-NYUDJGNCNT.
                    06  V02-VALUE   PIC S9(2).
                  05  V02-NYUMJGNGAK.
                    06  V02-VALUE   PIC S9(6).
                  05  V02-NYUMJGNCNT.
                    06  V02-VALUE   PIC S9(2).
                  05  V02-NYUSKJRYYKBN-CMB.
                    06  V02-NYUSKJRYYKBN.
                      07  V02-VALUE   PIC X(28).
                    06  V02-NYUSKJRYYKBN-COUNT   PIC S9(9)   BINARY.
                    06  V02-NYUSKJRYYKBNLST      PIC X(28)   OCCURS 10.
                  05  V02-NYUSKJJYOGAK.
                    06  V02-VALUE   PIC S9(6).
              03  V02-HKNNUM.
                04  V02-VALUE   PIC X(3).
              03  V02-PAYKBN.
                04  V02-VALUE   PIC X(2).
              03  V02-HBTNUM.
                04  V02-VALUE   PIC X(2).
              03  V02-SEIDONAME.
                04  V02-VALUE   PIC X(100).
              03  V02-TANSEIDONAME.
                04  V02-VALUE   PIC X(10).
              03  V02-GENHKNNUM-G   OCCURS  3.
                04  V02-GENHKNNUM.
                  05  V02-VALUE   PIC X(3).
              03  V02-JOKENSTYEAR.
                04  V02-VALUE   PIC S9(3).
              03  V02-JOKENEDYEAR.
                04  V02-VALUE   PIC S9(3).
              03  V02-TENTNK.
                04  V02-VALUE   PIC S9(3).
              03  V02-HKNKOHKBN-CMB.
                04  V02-HKNKOHKBN.
                  05  V02-VALUE   PIC X(10).
                04  V02-HKNKOHKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-HKNKOHKBNLST    PIC X(10)   OCCURS  3 TIMES.
              03  V02-HKNKOHSBTKBN-CMB.
                04  V02-HKNKOHSBTKBN.
                  05  V02-VALUE   PIC X(20).
                04  V02-HKNKOHSBTKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-HKNKOHSBTKBNLST   PIC X(20)   OCCURS  9 TIMES.
              03  V02-RECEFTNDSPKBN-CMB.
                04  V02-RECEFTNDSPKBN.
                  05  V02-VALUE   PIC X(30).
                04  V02-RECEFTNDSPKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-RECEFTNDSPKBNLST   PIC X(30)   OCCURS  2 TIMES.
              03  V02-JKYSKENSNUMCHKKBN-CMB.
                04  V02-JKYSKENSNUMCHKKBN.
                  05  V02-VALUE   PIC X(16).
                04  V02-JKYSKENSNUMCHKKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-JKYSKENSNUMCHKKBNLST      PIC X(16)   OCCURS  3.
              03  V02-RECESKYKBN-CMB.
                04  V02-RECESKYKBN.
                  05  V02-VALUE   PIC X(36).
                04  V02-RECESKYKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-RECESKYKBNLST    PIC X(36)   OCCURS  10 TIMES.
              03  V02-HBTNUMCHKKBN-CMB.
                04  V02-HBTNUMCHKKBN.
                  05  V02-VALUE   PIC X(16).
                04  V02-HBTNUMCHKKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-HBTNUMCHKKBNLST   PIC X(16)   OCCURS  2 TIMES.
              03  V02-KENSNUMCHKKBN-CMB.
                04  V02-KENSNUMCHKKBN.
                  05  V02-VALUE   PIC X(16).
                04  V02-KENSNUMCHKKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-KENSNUMCHKKBNLST   PIC X(16)   OCCURS  2 TIMES.
              03  V02-RECEFTNJGNDSPKBN-CMB.
                04  V02-RECEFTNJGNDSPKBN.
                  05  V02-VALUE   PIC X(24).
                04  V02-RECEFTNJGNDSPKBN-COUNT   PIC S9(9)   BINARY.
                04  V02-RECEFTNJGNDSPKBNLST PIC X(24)   OCCURS  2 TIMES.
