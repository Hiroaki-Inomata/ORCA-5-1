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
      *         医薬品分類マスタ    COPY (CPGENERICCLASS.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      * 01  GECLASS-REC.
           03  GECLASS-KEY.
               05  GECLASS-YAKKAKJNCD  PIC X(12).
               05  GECLASS-YUKOSTYMD   PIC X(08).
               05  GECLASS-YUKOEDYMD   PIC X(08).
           03  GECLASS-KOUHATU         PIC 9(01).
           03  GECLASS-UPYMD           PIC X(08).
      *更新用
           03  GECLASS-UP-KEY.
               05  GECLASS-UPYAKKAKJNCD    PIC X(12).
               05  GECLASS-UPYUKOSTYMD     PIC X(08).
               05  GECLASS-UPYUKOEDYMD     PIC X(08).
