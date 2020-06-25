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
      *    支払金額編集サブ            COPY (CPORCHCM31S01.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  HCM31S01-AREA.
           03  HCM31S01-IN.
               05  HCM31S01-PROCKBN        PIC  X(01).
               05  HCM31S01-HOSPNUM        PIC  9(02).
               05  HCM31S01-NYUGAIKBN      PIC  X(01).
               05  HCM31S01-PTID           PIC  9(10).
               05  HCM31S01-DENPNUM        PIC  9(07).
               05  HCM31S01-SRYYMD         PIC  X(08).
               05  HCM31S01-STYMD          PIC  X(08).
               05  HCM31S01-EDYMD          PIC  X(08).
               05  HCM31S01-SUMKBN         PIC  X(01).
           03  HCM31S01-OT.
               05  HCM31S01-OT-MAX         PIC  9(05).
               05  HCM31S01-OT-OCC         OCCURS  100.
                   07  HCM31S01-NYKNYMD    PIC  X(08).
                   07  HCM31S01-KINGAKU    PIC  S9(10).
                   07  HCM31S01-BFKINGAKU  PIC  S9(10).
      *
