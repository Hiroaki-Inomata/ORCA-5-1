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
      ************************************************************
      *    定期請求差額取得サブ　COPY (CPORCSTEIKISGK.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  STEIKISGK-AREA.
           03  STEIKISGK-PTID                  PIC 9(10).
           03  STEIKISGK-SRYYMD                PIC X(08).
           03  STEIKISGK-RRKNUM                PIC 9(03).
           03  STEIKISGK-TAIIN-SKYSTYMD        PIC X(08).
           03  STEIKISGK-TAIIN-SKYEDYMD        PIC X(08).
           03  STEIKISGK-KBT-G.
               05  STEIKISGK-KBT-MAX           PIC 9(03).
               05  STEIKISGK-KBT-OCC           OCCURS  3.
                   07  STEIKISGK-SKYSTYMD      PIC X(08).
                   07  STEIKISGK-SKYEDYMD      PIC X(08).
                   07  STEIKISGK-SYU-SKYMONEY  PIC S9(9).
                   07  STEIKISGK-FTN-SKYMONEY  PIC S9(9).
                   07  STEIKISGK-SAGAKU        PIC S9(9).
           03  STEIKISGK-TAIIN-SKYMONEY        PIC S9(9).
           03  STEIKISGK-RC                    PIC S9(9).
           03  STEIKISGK-S02-RC                PIC S9(9).
