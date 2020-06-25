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
            01  HC303.
              03  HC303-SYORIBI       PIC  X(22).
              03  HC303-PAGE          PIC  X(3).
              03  HC303-SYURUI        PIC  X(12).
              03  HC303-ID            PIC  X(20).
              03  HC303-KANANAME      PIC  X(44).
              03  HC303-NAME          PIC  X(30).
              03  HC303-JYUSYO        PIC  X(56).
              03  HC303-HSPNAME       PIC  X(56).
              03  HC303-TEL           PIC  X(15).
              03  HC303-DRNAME        PIC  X(26).
              03  HC303-COMMENT       PIC  X(108)  OCCURS  7.
              03  HC303-TBL                        OCCURS  3.
                05  HC303-NUM         PIC  X(2).
                05  HC303-YKZNAME     PIC  X(26)   OCCURS  3.
                05  HC303-KONOKOKA    PIC  X(30)   OCCURS  6.
                05  HC303-GAZO        PIC  X(100).
                05  HC303-KEIJYO      PIC  X(16)   OCCURS  10.
                05  HC303-KBN         PIC  X(2)    OCCURS  4.
                05  HC303-NAIYO       PIC  X(16)   OCCURS  8.
                05  HC303-CYUJKO      PIC  X(40)   OCCURS  10.
