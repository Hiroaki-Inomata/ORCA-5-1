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
      *         一般名マスタ  COPY (CPGENERICNAME.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  GENERICNAME-REC.
           03  GENERIC-KEY.
      *    薬価基準コード
               05  GENERIC-YAKKAKJNCD      PIC  X(12).
      *    一般名有効桁
           03  GENERIC-YUKOKETA        PIC  9(02).
      *    一般名
           03  GENERIC-GENERICNAME     PIC  X(200).
      *    単位コード
           03  GENERIC-TANICD          PIC  X(03).
      *    単位名称
           03  GENERIC-TANINAME        PIC  X(24).
      *    数量換算値
           03  GENERIC-KANZANCHI       PIC  9(05)V9(05).
      *
           03  GENERIC-TERMID          PIC  X(16).
           03  GENERIC-OPID            PIC  X(16).
           03  GENERIC-CREYMD          PIC  X(08).
           03  GENERIC-UPYMD           PIC  X(08).
           03  GENERIC-UPHMS           PIC  X(06).
