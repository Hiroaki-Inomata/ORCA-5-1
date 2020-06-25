      *****************************************************************
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
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *****************************************************************
       01  L102.
              03    L102-MIDASI         PIC  X(10).
              03    L102-ERRMSG         PIC  X(50).
              03    L102-PAGE           PIC  X(03).
              03    L102-SYSYMD         PIC  X(22).
              03    L102-YMD1           PIC  X(22).
              03    L102-YMD2           PIC  X(22).
              03    L102-SRYKA          PIC  X(30).
              03    L102-DRNAME         PIC  X(30).
              03    L102-NUM-G.
               05   L102-NUM-TBL        OCCURS  15.
                 07 L102-NUM            PIC  X(05).
              03    L102-PTNUM-G.
               05   L102-PTNUM-TBL      OCCURS  15.
                 07 L102-PTNUM          PIC  X(10).
              03    L102-KANANAME-G.
               05   L102-KANANAME-TBL   OCCURS  15.
                 07 L102-KANANAME       PIC  X(40).
              03    L102-NAME-G.
               05   L102-NAME-TBL       OCCURS  15.
                 07 L102-NAME           PIC  X(26).
              03    L102-SEX-G.
               05   L102-SEX-TBL        OCCURS  15.
                 07 L102-SEX            PIC  X(02).
              03    L102-BIRTHDAY-G.
               05   L102-BIRTHDAY-TBL   OCCURS  15.
                 07 L102-BIRTHDAY       PIC  X(10).
              03    L102-AGE-G.
               05   L102-AGE-TBL        OCCURS  15.
                 07 L102-AGE            PIC  X(03).
              03    L102-HKNJANUM-G.
               05   L102-HKNJANUM-TBL   OCCURS  15.
                 07 L102-HKNJANUM       PIC  X(08).
              03    L102-HONKAZKKBN-G.
               05   L102-HONKAZKKBN-TBL OCCURS  15.
                 07 L102-HONKAZKKBN     PIC  X(02).
              03    L102-KIGONUM-G.
               05   L102-KIGONUM-TBL    OCCURS  15.
                 07 L102-KIGONUM        PIC  X(52).
                 07 L102-KIGO           PIC  X(52).
                 07 L102-BANGO          PIC  X(52).
              03    L102-ATAMAGAKI-G.
               05   L102-ATAMAGAKI-TBL  OCCURS  15.
                 07 L102-ATAMAGAKI      PIC  X(10).
              03    L102-KOH1FTNNUM-G.
               05   L102-KOH1FTNNUM-TBL OCCURS  15.
                 07 L102-KOH1FTNNUM     PIC  X(08).
              03    L102-KOH2FTNNUM-G.
               05   L102-KOH2FTNNUM-TBL OCCURS  15.
                 07 L102-KOH2FTNNUM     PIC  X(08).
              03    L102-KOH3FTNNUM-G.
               05   L102-KOH3FTNNUM-TBL OCCURS  15.
                 07 L102-KOH3FTNNUM     PIC  X(08).
              03    L102-CNTTIT1        PIC  X(20).
              03    L102-CNTTIT2        PIC  X(20).
              03    L102-CNTTIT3        PIC  X(20).
              03    L102-CNT1           PIC  X(10).
              03    L102-CNT2           PIC  X(10).
              03    L102-CNT3           PIC  X(10).
