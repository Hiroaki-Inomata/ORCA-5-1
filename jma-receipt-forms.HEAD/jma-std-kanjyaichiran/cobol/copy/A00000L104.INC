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
       01  L104.
              03    L104-MIDASI         PIC  X(10).
              03    L104-ERRMSG         PIC  X(50).
              03    L104-PAGE           PIC  X(03).
              03    L104-SYSYMD         PIC  X(22).
              03    L104-YMD1           PIC  X(22).
              03    L104-YMD2           PIC  X(22).
              03    L104-SRYKA          PIC  X(30).
              03    L104-DRNAME         PIC  X(30).
              03    L104-NUM-G.
               05   L104-NUM-TBL        OCCURS  15.
                 07 L104-NUM            PIC  X(05).
              03    L104-PTNUM-G.
               05   L104-PTNUM-TBL      OCCURS  15.
                 07 L104-PTNUM          PIC  X(10).
              03    L104-KANANAME-G.
               05   L104-KANANAME-TBL   OCCURS  15.
                 07 L104-KANANAME       PIC  X(40).
              03    L104-NAME-G.
               05   L104-NAME-TBL       OCCURS  15.
                 07 L104-NAME           PIC  X(26).
              03    L104-SEX-G.
               05   L104-SEX-TBL        OCCURS  15.
                 07 L104-SEX            PIC  X(02).
              03    L104-BIRTHDAY-G.
               05   L104-BIRTHDAY-TBL   OCCURS  15.
                 07 L104-BIRTHDAY       PIC  X(10).
              03    L104-AGE-G.
               05   L104-AGE-TBL        OCCURS  15.
                 07 L104-AGE            PIC  X(03).
              03    L104-HKNJANUM-G.
               05   L104-HKNJANUM-TBL   OCCURS  15.
                 07 L104-HKNJANUM       PIC  X(08).
              03    L104-HONKAZKKBN-G.
               05   L104-HONKAZKKBN-TBL OCCURS  15.
                 07 L104-HONKAZKKBN     PIC  X(02).
              03    L104-KIGONUM-G.
               05   L104-KIGONUM-TBL    OCCURS  15.
                 07 L104-KIGONUM        PIC  X(52).
                 07 L104-KIGO           PIC  X(52).
                 07 L104-BANGO          PIC  X(52).
              03    L104-ATAMAGAKI-G.
               05   L104-ATAMAGAKI-TBL  OCCURS  15.
                 07 L104-ATAMAGAKI      PIC  X(10).
              03    L104-KOH1FTNNUM-G.
               05   L104-KOH1FTNNUM-TBL OCCURS  15.
                 07 L104-KOH1FTNNUM     PIC  X(08).
              03    L104-KOH2FTNNUM-G.
               05   L104-KOH2FTNNUM-TBL OCCURS  15.
                 07 L104-KOH2FTNNUM     PIC  X(08).
              03    L104-KOH3FTNNUM-G.
               05   L104-KOH3FTNNUM-TBL OCCURS  15.
                 07 L104-KOH3FTNNUM     PIC  X(08).
              03    L104-NYUINYMD-G.
               05   L104-NYUINYMD-TBL   OCCURS  15.
                 07 L104-NYUINYMD       PIC  X(10).
              03    L104-TAIINYMD-G.
               05   L104-TAIINYMD-TBL   OCCURS  15.
                 07 L104-TAIINYMD       PIC  X(10).
              03    L104-CNTTIT1        PIC  X(20).
              03    L104-CNTTIT2        PIC  X(20).
              03    L104-CNTTIT3        PIC  X(20).
              03    L104-CNT1           PIC  X(10).
              03    L104-CNT2           PIC  X(10).
              03    L104-CNT3           PIC  X(10).
