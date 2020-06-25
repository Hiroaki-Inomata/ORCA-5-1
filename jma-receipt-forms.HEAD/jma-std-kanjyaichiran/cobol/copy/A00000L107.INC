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
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *****************************************************************
       01  L107.
              03    L107-MIDASI         PIC  X(10).
              03    L107-ERRMSG         PIC  X(50).
              03    L107-PAGE           PIC  X(03).
              03    L107-SYSYMD         PIC  X(22).
              03    L107-YMD1           PIC  X(22).
              03    L107-YMD2           PIC  X(22).
              03    L107-SRYKA          PIC  X(30).
              03    L107-DRNAME         PIC  X(30).
              03    L107-NUM-G.
               05   L107-NUM-TBL        OCCURS  15.
                 07 L107-NUM            PIC  X(05).
              03    L107-PTNUM-G.
               05   L107-PTNUM-TBL      OCCURS  15.
                 07 L107-PTNUM          PIC  X(10).
              03    L107-KANANAME-G.
               05   L107-KANANAME-TBL   OCCURS  15.
                 07 L107-KANANAME       PIC  X(40).
              03    L107-NAME-G.
               05   L107-NAME-TBL       OCCURS  15.
                 07 L107-NAME           PIC  X(26).
              03    L107-SEX-G.
               05   L107-SEX-TBL        OCCURS  15.
                 07 L107-SEX            PIC  X(02).
              03    L107-BIRTHDAY-G.
               05   L107-BIRTHDAY-TBL   OCCURS  15.
                 07 L107-BIRTHDAY       PIC  X(10).
              03    L107-AGE-G.
               05   L107-AGE-TBL        OCCURS  15.
                 07 L107-AGE            PIC  X(03).
              03    L107-HKNJANUM-G.
               05   L107-HKNJANUM-TBL   OCCURS  15.
                 07 L107-HKNJANUM       PIC  X(08).
              03    L107-HONKAZKKBN-G.
               05   L107-HONKAZKKBN-TBL OCCURS  15.
                07 L107-HONKAZKKBN     PIC  X(02).
              03    L107-KIGONUM-G.
               05   L107-KIGONUM-TBL    OCCURS  15.
                 07 L107-KIGONUM        PIC  X(52).
                 07 L107-KIGO           PIC  X(52).
                 07 L107-BANGO          PIC  X(52).
              03    L107-KOH1FTNNUM-G.
               05   L107-KOH1FTNNUM-TBL OCCURS  15.
                 07 L107-KOH1FTNNUM     PIC  X(08).
              03    L107-KOH2FTNNUM-G.
               05   L107-KOH2FTNNUM-TBL OCCURS  15.
                 07 L107-KOH2FTNNUM     PIC  X(08).
              03    L107-KOH3FTNNUM-G.
               05   L107-KOH3FTNNUM-TBL OCCURS  15.
                 07 L107-KOH3FTNNUM     PIC  X(08).
              03    L107-ATAMAGAKI-G.
               05   L107-ATAMAGAKI-TBL  OCCURS  15.
                 07 L107-ATAMAGAKI      PIC  X(10).
              03    L107-AZUKARI-G.
               05   L107-AZUKARI-TBL    OCCURS  15.
                 07 L107-AZUKARI        PIC  X(10).
              03    L107-LASTYMD-G.
               05   L107-LASTYMD-TBL    OCCURS  15.
                 07 L107-LASTYMD        PIC  X(10).
              03    L107-CNTTIT1        PIC  X(20).
              03    L107-CNTTIT2        PIC  X(20).
              03    L107-CNTTIT3        PIC  X(20).
              03    L107-CNT1           PIC  X(10).
              03    L107-CNT2           PIC  X(10).
              03    L107-CNT3           PIC  X(10).
