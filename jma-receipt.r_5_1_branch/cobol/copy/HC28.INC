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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
          01  HC28.
              03  HC28-HYODAI     PIC  X(100).
              03  HC28-JYOKEN     PIC  X(100)    OCCURS  2.
              03  HC28-SAKUSEIBI  PIC  X(22).
              03  HC28-PAGE       PIC  X(6).
              03  HC28-CD         PIC  X(5)      OCCURS  30.
              03  HC28-PTNUM      PIC  X(10)     OCCURS  30.
              03  HC28-PTNUMUP    PIC  X(10)     OCCURS  30.
              03  HC28-PTNUMDN    PIC  X(10)     OCCURS  30.
              03  HC28-NAME       PIC  X(30)     OCCURS  30.
              03  HC28-SEIBETU    PIC  X(2)      OCCURS  30.
              03  HC28-TANJYOBI   PIC  X(18)     OCCURS  30.
              03  HC28-AGENUM     PIC  X(04)     OCCURS  30.
              03  HC28-AGETANI    PIC  X(04)     OCCURS  30.
              03  HC28-JYUSYO     PIC  X(70)     OCCURS  30.
