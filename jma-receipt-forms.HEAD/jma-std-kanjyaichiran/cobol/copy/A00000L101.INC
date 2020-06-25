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
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *****************************************************************
       01  L101.
              03    L101-MIDASI         PIC  X(10).
              03    L101-ERRMSG         PIC  X(50).
              03    L101-PAGE           PIC  X(03).
              03    L101-SYSYMD         PIC  X(22).
              03    L101-YMD1           PIC  X(22).
              03    L101-YMD2           PIC  X(22).
              03    L101-SRYKA          PIC  X(30).
              03    L101-DRNAME         PIC  X(30).
              03    L101-NUM-G.
               05   L101-NUM-TBL        OCCURS  15.
                 07 L101-NUM            PIC  X(05).
              03    L101-PTNUM-G.
               05   L101-PTNUM-TBL      OCCURS  15.
                 07 L101-PTNUM          PIC  X(10).
              03    L101-KANANAME-G.
               05   L101-KANANAME-TBL   OCCURS  15.
                 07 L101-KANANAME       PIC  X(40).
              03    L101-NAME-G.
               05   L101-NAME-TBL       OCCURS  15.
                 07 L101-NAME           PIC  X(26).
              03    L101-SEX-G.
               05   L101-SEX-TBL        OCCURS  15.
                 07 L101-SEX            PIC  X(02).
              03    L101-BIRTHDAY-G.
               05   L101-BIRTHDAY-TBL   OCCURS  15.
                 07 L101-BIRTHDAY       PIC  X(10).
              03    L101-AGE-G.
               05   L101-AGE-TBL        OCCURS  15.
                 07 L101-AGE            PIC  X(03).
              03    L101-HKNJANUM-G.
               05   L101-HKNJANUM-TBL   OCCURS  15.
                 07 L101-HKNJANUM       PIC  X(08).
              03    L101-HONKAZKKBN-G.
               05   L101-HONKAZKKBN-TBL OCCURS  15.
                 07 L101-HONKAZKKBN     PIC  X(02).
              03    L101-KIGONUM-G.
               05   L101-KIGONUM-TBL    OCCURS  15.
                 07 L101-KIGONUM        PIC  X(52).
                 07 L101-KIGO           PIC  X(52).
                 07 L101-BANGO          PIC  X(52).
              03    L101-ATAMAGAKI-G.
               05   L101-ATAMAGAKI-TBL  OCCURS  15.
                 07 L101-ATAMAGAKI      PIC  X(10).
              03    L101-KOH1FTNNUM-G.
               05   L101-KOH1FTNNUM-TBL OCCURS  15.
                 07 L101-KOH1FTNNUM     PIC  X(08).
              03    L101-KOH2FTNNUM-G.
               05   L101-KOH2FTNNUM-TBL OCCURS  15.
                 07 L101-KOH2FTNNUM     PIC  X(08).
              03    L101-KOH3FTNNUM-G.
               05   L101-KOH3FTNNUM-TBL OCCURS  15.
                 07 L101-KOH3FTNNUM     PIC  X(08).
              03    L101-JRYRRK-G.
               05   L101-JRYRRK-TBL     OCCURS  15.
                 07 L101-JRYRRK         PIC  X(02).
              03    L101-BYOMEI-G.
               05   L101-BYOMEI-TBL     OCCURS  15.
                 07 L101-BYOMEI         PIC  X(44).
              03    L101-CNTTIT1        PIC  X(20).
              03    L101-CNTTIT2        PIC  X(20).
              03    L101-CNTTIT3        PIC  X(20).
              03    L101-CNT1           PIC  X(10).
              03    L101-CNT2           PIC  X(10).
              03    L101-CNT3           PIC  X(10).
