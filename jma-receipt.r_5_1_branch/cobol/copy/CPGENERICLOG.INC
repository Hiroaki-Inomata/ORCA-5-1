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
      *         一般名記載ログ       COPY (CPGENERICLOG.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  GENERICLOG-REC.
      *医療機関ＩＤ
           03  GENERICLOG-HOSPNUM          PIC  9(02).
      *患者ＩＤ
           03  GENERICLOG-PTID             PIC  9(10).
      *診療日
           03  GENERICLOG-SRYYMD           PIC  X(08).
      *発行日
           03  GENERICLOG-PRINTYMD         PIC  X(08).
      *発行時間
           03  GENERICLOG-PRINTHMS         PIC  X(06).
      *ＲＰ番号
           03  GENERICLOG-RECIPENUM        PIC  9(02).
      *処理区分
           03  GENERICLOG-SYORIKBN         PIC  9(01).
      *伝票番号
           03  GENERICLOG-DENPNUM          PIC  9(07).
      *剤番号
           03  GENERICLOG-ZAINUM           PIC  9(08).
      *診療コード
           03  GENERICLOG-SRYCD            PIC  X(09).
      *薬価基準コード（一般名コード）
           03  GENERICLOG-YAKKAKJNCD       PIC  X(12).
      *一般名記載内容
           03  GENERICLOG-GENERICNAME      PIC  X(200).
      *
