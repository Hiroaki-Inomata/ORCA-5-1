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
      *----------------------------------------------------------*
      *    自費内訳明細書診療行為サブ     COPY (CPORCHCM50S01.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  HCM50S01-AREA.
           03  HCM50S01-IN.
               05  HCM50S01-HOSPNUM        PIC  9(02).
               05  HCM50S01-NYUGAIKBN      PIC  X(01).
               05  HCM50S01-PTID           PIC  9(10).
               05  HCM50S01-SUMKBN         PIC  X(01).
               05  HCM50S01-SRYYMD         PIC  X(08).
               05  HCM50S01-STYMD          PIC  X(08).
               05  HCM50S01-EDYMD          PIC  X(08).
               05  HCM50S01-HCM50F01       PIC  X(1024).
