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
      * 01.00.01     @@@@-門間    11/01/07  コメント履歴削除
      ********************************************************************
       01  L300.
              03    L300-JOBMSG         PIC  X(60).
              03    L300-PAGE           PIC  X(03).
              03    L300-PRTYMD         PIC  X(22).
              03    L300-SRYYM          PIC  X(16).
              03    L300-NYUGAIKBN      PIC  X(10).
              03    L300-SRYKA          PIC  X(30).
              03    L300-NO-G.
               05   L300-NO-TBL         OCCURS  20.
                 07 L300-NO             PIC  X(05).
              03    L300-PTNUM-G.
               05   L300-PTNUM-TBL      OCCURS  20.
                 07 L300-PTNUM          PIC  X(10).
              03    L300-KANANAME-G.
               05   L300-KANANAME-TBL   OCCURS  20.
                 07 L300-KANANAME       PIC  X(18).
              03    L300-NAME-G.
               05   L300-NAME-TBL       OCCURS  20.
                 07 L300-NAME           PIC  X(30).
              03    L300-HKNJANUM-G.
               05   L300-HKNJANUM-TBL   OCCURS  20.
                 07 L300-HKNJANUM       PIC  X(08).
              03    L300-HKNJANAME-G.
               05   L300-HKNJANAME-TBL  OCCURS  20.
                 07 L300-HKNJANAME      PIC  X(60).
              03    L300-BIRTHDAY-G.
               05   L300-BIRTHDAY-TBL   OCCURS  20.
                 07 L300-BIRTHDAY       PIC  X(10).
              03    L300-AGE-G.
               05   L300-AGE-TBL        OCCURS  20.
                 07 L300-AGE            PIC  X(03).
              03    L300-ZOKU-G.
               05   L300-ZOKU-TBL       OCCURS  20.
                 07 L300-ZOKU           PIC  X(08).
              03    L300-KOHNUM-G.
               05   L300-KOHNUM-TBL     OCCURS  20.
                 07 L300-KOHNUM         PIC  X(03).
              03    L300-ATAMAYMD-G.
               05   L300-ATAMAYMD-TBL   OCCURS  20.
                 07 L300-ATAMAYMD       PIC  X(10).
              03    L300-MISYU-G.
               05   L300-MISYU-TBL      OCCURS  20.
                 07 L300-MISYU          PIC  X(11).
              03    L300-AZUKARI-G.
               05   L300-AZUKARI-TBL    OCCURS  20.
                 07 L300-AZUKARI        PIC  X(11).
              03    L300-LASTYMD-G.
               05   L300-LASTYMD-TBL    OCCURS  20.
                 07 L300-LASTYMD        PIC  X(10).
