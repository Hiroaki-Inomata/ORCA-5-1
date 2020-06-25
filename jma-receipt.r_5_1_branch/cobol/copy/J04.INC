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
        02  J04.
      ***  02  J04-FIXED2.
              03  J04-SRYKBN    OCCURS  20.
                04  J04-VALUE   PIC X(2).
              03  J04-INPUTCD   OCCURS  20.
                04  J04-VALUE   PIC X(54).
              03  J04-MEISYO-G    OCCURS  20.
                04  J04-MEISYO-STYLE   PIC X(20).
                04  J04-MEISYO    PIC X(140).
              03  J04-SURYO-G   OCCURS  20.
                04  J04-SURYO-STYLE   PIC X(20).
                04  J04-SURYO   PIC X(40).
              03  J04-PTNUM.
                04  J04-VALUE   PIC X(20).
              03  J04-SRYYM.
                04  J04-VALUE   PIC X(8).
              03  J04-KANANAME.
                04  J04-VALUE   PIC X(50).
              03  J04-NAME.
                04  J04-VALUE   PIC X(50).
              03  J04-BIRTHDAY.
                04  J04-VALUE   PIC X(9).
              03  J04-SEX.
                04  J04-VALUE   PIC X(2).
              03  J04-RATE.
                04  J04-VALUE   PIC X(6).
              03  J04-RRKYMD.
                04  J04-VALUE   PIC X(13).
              03  J04-DAY       OCCURS  31.
                04  J04-VALUE   PIC X(3).
              03  J04-HKNCOMBI-COMB.
                04  J04-HKNCOMBI.
                  05  J04-VALUE   PIC X(55).
                04  J04-HKNCOMBI-COUNT   PIC S9(9)   BINARY.
                04  J04-HKNCOMBI-ITEM
                        PIC X(55)   OCCURS  40 TIMES.
              03  J04-SRYKA.
                04  J04-VALUE   PIC X(20).
              03  J04-HKTKBN-COMB.
                04  J04-HKTKBN.
                  05  J04-VALUE   PIC X(30).
                04  J04-HKTKBN-COUNT   PIC S9(9)   BINARY.
                04  J04-HKTKBN-ITEM
                        PIC X(30)   OCCURS  3 TIMES.

