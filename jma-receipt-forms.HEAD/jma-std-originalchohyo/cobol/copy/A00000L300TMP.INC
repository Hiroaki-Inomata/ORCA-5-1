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
       01  L300T.
              03    L300T-KEY.
               05   L300T-NYUGAIKBN      PIC  X(04).
               05   L300T-SRYKA          PIC  X(30).
               05   L300T-PTNUM          PIC  X(10).
              03    L300T-KANANAME       PIC  X(18).
              03    L300T-NAME           PIC  X(30).
              03    L300T-HKNJANUM       PIC  X(08).
              03    L300T-HKNJANAME      PIC  X(60).
              03    L300T-BIRTHDAY       PIC  X(10).
              03    L300T-AGE            PIC  X(03).
              03    L300T-ZOKU           PIC  X(08).
              03    L300T-KOHNUM         PIC  X(03).
              03    L300T-ATAMAYMD       PIC  X(10).
              03    L300T-SKYMONEY       PIC S9(09).
              03    L300T-NYUKIN-TOTAL   PIC S9(09).
              03    L300T-AZUKARI        PIC S9(09).
              03    L300T-LASTYMD        PIC  X(10).
