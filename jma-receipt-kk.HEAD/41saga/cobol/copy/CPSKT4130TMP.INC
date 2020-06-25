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
      *    診療報酬請求書     COPY (CPSKT4130TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  MF100-REC.
           03  MF100-KEY.
      *公費−負担者番号
               05  MF100-CITYNUM               PIC  X(06).
      *請求テーブル
           03  MF100-SEIKYU-TBL.
               05  MF100-NYUGAI-TBL  OCCURS 2.
      *件数
                   07  MF100-KENSU             PIC  9(06).
      *日数
                   07  MF100-NISSU             PIC  9(06).
      *点数
                   07  MF100-TENSU             PIC  9(09).
      *患者負担額
                   07  MF100-ITBFTN            PIC  9(09).
      *食事件数
               05  MF100-SKENSU                PIC  9(06).
      *食事日数
               05  MF100-SNISSU                PIC  9(06).
      *食事療養費
               05  MF100-SKINGK                PIC  9(09).
      *食事標準負担額
               05  MF100-SFTNGK                PIC  9(09).
