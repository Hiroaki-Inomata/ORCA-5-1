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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     @@@@-門間    10/12/07  コメント履歴削除
      *****************************************************************
       01  L103.
              03    L103-MIDASI         PIC  X(10).
              03    L103-ERRMSG         PIC  X(50).
              03    L103-PAGE           PIC  X(03).
              03    L103-SYSYMD         PIC  X(22).
              03    L103-YMD1           PIC  X(22).
              03    L103-YMD2           PIC  X(22).
              03    L103-SRYKA          PIC  X(30).
              03    L103-DRNAME         PIC  X(30).
              03    L103-NUM-G.
               05   L103-NUM-TBL        OCCURS  15.
                 07 L103-NUM            PIC  X(05).
              03    L103-PTNUM-G.
               05   L103-PTNUM-TBL      OCCURS  15.
                 07 L103-PTNUM          PIC  X(10).
              03    L103-KANANAME-G.
               05   L103-KANANAME-TBL   OCCURS  15.
                 07 L103-KANANAME       PIC  X(40).
              03    L103-NAME-G.
               05   L103-NAME-TBL       OCCURS  15.
                 07 L103-NAME           PIC  X(26).
              03    L103-SEX-G.
               05   L103-SEX-TBL        OCCURS  15.
                 07 L103-SEX            PIC  X(02).
              03    L103-BIRTHDAY-G.
               05   L103-BIRTHDAY-TBL   OCCURS  15.
                 07 L103-BIRTHDAY       PIC  X(10).
              03    L103-AGE-G.
               05   L103-AGE-TBL        OCCURS  15.
                 07 L103-AGE            PIC  X(03).
              03    L103-HKNJANUM-G.
               05   L103-HKNJANUM-TBL   OCCURS  15.
                 07 L103-HKNJANUM       PIC  X(08).
              03    L103-HONKAZKKBN-G.
               05   L103-HONKAZKKBN-TBL OCCURS  15.
                 07 L103-HONKAZKKBN     PIC  X(02).
              03    L103-KIGONUM-G.
               05   L103-KIGONUM-TBL    OCCURS  15.
                 07 L103-KIGONUM        PIC  X(52).
                 07 L103-KIGO           PIC  X(52).
                 07 L103-BANGO          PIC  X(52).
              03    L103-ATAMAGAKI-G.
               05   L103-ATAMAGAKI-TBL  OCCURS  15.
                 07 L103-ATAMAGAKI      PIC  X(10).
              03    L103-KOH1FTNNUM-G.
               05   L103-KOH1FTNNUM-TBL OCCURS  15.
                 07 L103-KOH1FTNNUM     PIC  X(08).
              03    L103-KOH2FTNNUM-G.
               05   L103-KOH2FTNNUM-TBL OCCURS  15.
                 07 L103-KOH2FTNNUM     PIC  X(08).
              03    L103-KOH3FTNNUM-G.
               05   L103-KOH3FTNNUM-TBL OCCURS  15.
                 07 L103-KOH3FTNNUM     PIC  X(08).
              03    L103-NYUINYMD-G.
               05   L103-NYUINYMD-TBL   OCCURS  15.
                 07 L103-NYUINYMD       PIC  X(10).
              03    L103-TAIINYMD-G.
               05   L103-TAIINYMD-TBL   OCCURS  15.
                 07 L103-TAIINYMD       PIC  X(10).
              03    L103-CNTTIT1        PIC  X(20).
              03    L103-CNTTIT2        PIC  X(20).
              03    L103-CNTTIT3        PIC  X(20).
              03    L103-CNT1           PIC  X(10).
              03    L103-CNT2           PIC  X(10).
              03    L103-CNT3           PIC  X(10).
