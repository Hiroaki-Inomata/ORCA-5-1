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
       01  L106.
              03    L106-MIDASI         PIC  X(10).
              03    L106-ERRMSG         PIC  X(50).
              03    L106-PAGE           PIC  X(03).
              03    L106-SYSYMD         PIC  X(22).
              03    L106-YMD1           PIC  X(22).
              03    L106-YMD2           PIC  X(22).
              03    L106-SRYKA          PIC  X(30).
              03    L106-DRNAME         PIC  X(30).
              03    L106-NUM-G.
               05   L106-NUM-TBL        OCCURS  15.
                 07 L106-NUM            PIC  X(05).
              03    L106-PTNUM-G.
               05   L106-PTNUM-TBL      OCCURS  15.
                 07 L106-PTNUM          PIC  X(10).
              03    L106-KANANAME-G.
               05   L106-KANANAME-TBL   OCCURS  15.
                 07 L106-KANANAME       PIC  X(40).
              03    L106-NAME-G.
               05   L106-NAME-TBL       OCCURS  15.
                 07 L106-NAME           PIC  X(26).
              03    L106-SEX-G.
               05   L106-SEX-TBL        OCCURS  15.
                 07 L106-SEX            PIC  X(02).
              03    L106-BIRTHDAY-G.
               05   L106-BIRTHDAY-TBL   OCCURS  15.
                 07 L106-BIRTHDAY       PIC  X(10).
              03    L106-AGE-G.
               05   L106-AGE-TBL        OCCURS  15.
                 07 L106-AGE            PIC  X(03).
              03    L106-HKNJANUM-G.
               05   L106-HKNJANUM-TBL   OCCURS  15.
                 07 L106-HKNJANUM       PIC  X(08).
              03    L106-HONKAZKKBN-G.
               05   L106-HONKAZKKBN-TBL OCCURS  15.
                 07 L106-HONKAZKKBN     PIC  X(02).
              03    L106-KIGONUM-G.
               05   L106-KIGONUM-TBL    OCCURS  15.
                 07 L106-KIGONUM        PIC  X(52).
                 07 L106-KIGO           PIC  X(52).
                 07 L106-BANGO          PIC  X(52).
              03    L106-KOH1FTNNUM-G.
               05   L106-KOH1FTNNUM-TBL OCCURS  15.
                 07 L106-KOH1FTNNUM     PIC  X(08).
              03    L106-KOH2FTNNUM-G.
               05   L106-KOH2FTNNUM-TBL OCCURS  15.
                 07 L106-KOH2FTNNUM     PIC  X(08).
              03    L106-KOH3FTNNUM-G.
               05   L106-KOH3FTNNUM-TBL OCCURS  15.
                 07 L106-KOH3FTNNUM     PIC  X(08).
              03    L106-ATAMAGAKI-G.
               05   L106-ATAMAGAKI-TBL  OCCURS  15.
                 07 L106-ATAMAGAKI      PIC  X(10).
              03    L106-MISYU-G.
               05   L106-MISYU-TBL      OCCURS  15.
                 07 L106-MISYU          PIC  X(10).
              03    L106-LASTYMD-G.
               05   L106-LASTYMD-TBL    OCCURS  15.
                 07 L106-LASTYMD        PIC  X(10).
              03    L106-CNTTIT1        PIC  X(20).
              03    L106-CNTTIT2        PIC  X(20).
              03    L106-CNTTIT3        PIC  X(20).
              03    L106-CNT1           PIC  X(10).
              03    L106-CNT2           PIC  X(10).
              03    L106-CNT3           PIC  X(10).
