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
      *    患者禁忌薬剤マスタ            COPY (CPPTKINKI.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTKINKI-REC.
           03  PTKINKI-KEY.
      *医療機関コード
               05  PTKINKI-HOSPNUM           PIC  9(02).
      *患者ＩＤ
               05  PTKINKI-PTID              PIC  9(10).
      *薬剤コード
               05  PTKINKI-SRYCD             PIC  X(09).
      *禁忌開始日
           03  PTKINKI-KINKSTYMD             PIC  X(08).
      *投与日
           03  PTKINKI-TOUYOYMD              PIC  X(08).
      *端末ＩＤ
           03  PTKINKI-TERMID                PIC  X(32).
      *オペーレータＩＤ
           03  PTKINKI-OPID                  PIC  X(16).
      *作成年月日
           03  PTKINKI-CREYMD                PIC  X(08).
      *更新年月日
           03  PTKINKI-UPYMD                 PIC  X(08).
      *更新時間
           03  PTKINKI-UPHMS                 PIC  X(06).
      *
