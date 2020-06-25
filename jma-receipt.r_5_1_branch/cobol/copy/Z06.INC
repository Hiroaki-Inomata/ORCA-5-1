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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
        02  Z06.
     *******02  Z06-FIXED1.
              03  Z06-SRYCDH.
                04  Z06-VALUE   PIC X(9).
              03  Z06-YUKOSTYMDH.
                04  Z06-VALUE   PIC X(9).
              03  Z06-YUKOEDYMDH.
                04  Z06-VALUE   PIC X(9).
              03  Z06-NAMEH.
                04  Z06-VALUE   PIC X(100).
              03  Z06-NAME.
                04  Z06-VALUE   PIC X(100).
              03  Z06-KANANAME.
                04  Z06-VALUE   PIC X(100).
              03  Z06-YUKOSTYMD.
                04  Z06-VALUE   PIC X(10).
              03  Z06-YUKOEDYMD.
                04  Z06-VALUE   PIC X(10).
              03  Z06-FORMALNAME.
                04  Z06-VALUE   PIC X(200).
              03  Z06-JITEN1COMB.
                04  Z06-JITEN1.
                  05  Z06-VALUE   PIC X(12).
                04  Z06-JITEN1-COUNT   PIC S9(9)   BINARY.
                04  Z06-JITEN1-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06-JITEN2COMB.
                04  Z06-JITEN2.
                  05  Z06-VALUE   PIC X(12).
                04  Z06-JITEN2-COUNT   PIC S9(9)   BINARY.
                04  Z06-JITEN2-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06-JITEN3COMB.
                04  Z06-JITEN3.
                  05  Z06-VALUE   PIC X(12).
                04  Z06-JITEN3-COUNT   PIC S9(9)   BINARY.
                04  Z06-JITEN3-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06-JITEN4COMB.
                04  Z06-JITEN4.
                  05  Z06-VALUE   PIC X(12).
                04  Z06-JITEN4-COUNT   PIC S9(9)   BINARY.
                04  Z06-JITEN4-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06-JITEN5COMB.
                04  Z06-JITEN5.
                  05  Z06-VALUE   PIC X(12).
                04  Z06-JITEN5-COUNT   PIC S9(9)   BINARY.
                04  Z06-JITEN5-ITEM    PIC X(12)   OCCURS  2 TIMES.
              03  Z06-DIGIT1.
                04  Z06-VALUE   PIC S9(3).
              03  Z06-DIGIT2.
                04  Z06-VALUE   PIC S9(3).
              03  Z06-DIGIT3.
                04  Z06-VALUE   PIC S9(3).
              03  Z06-DIGIT4.
                04  Z06-VALUE   PIC S9(3).
              03  Z06-DIGIT5.
                04  Z06-VALUE   PIC S9(3).
              03  Z06-COMMENTKBN-COMB.
                04  Z06-COMMENTKBN.
                  05  Z06-VALUE   PIC X(30).
                04  Z06-COMMENTKBN-COUNT   PIC S9(9)   BINARY.
                04  Z06-COMMENTKBN-ITEM
                        PIC X(30)   OCCURS  5 TIMES.
              03  Z06-COPYCD-G.
                04  Z06-COPYCD-STATE   PIC S9(9)   BINARY.
                04  Z06-COPYCD         PIC X(9).
              03  Z06-B06.
                04  Z06-B06-STATE   PIC S9(9)   BINARY.
                04  Z06-B06-LABEL   PIC X(10).
              03  Z06-B09.
                04  Z06-B09-STATE   PIC S9(9)   BINARY.
                04  Z06-B09-LABEL   PIC X(10).
              03  Z06-B03.
                04  Z06-B03-STATE   PIC S9(9)   BINARY.
              04  Z06-MIDASHI.
                05  Z06-MIDASHI-STYLE  PIC X(20).
                05  Z06-MIDASHI-DATA   PIC X(80).

