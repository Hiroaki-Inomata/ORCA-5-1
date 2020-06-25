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
      *    新診療会計マスタ（従）     COPY (CPSRYACCTS.INC)      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SRYACCTS-REC.
           03  ACCTS-KEY.
      *医療機関ＩＤ
               05  ACCTS-HOSPNUM           PIC  9(02).
      *患者ＩＤ
               05  ACCTS-PTID              PIC  9(10).
      *剤番号
               05  ACCTS-ZAINUM            PIC  9(08).
      *連番
               05  ACCTS-RENNUM            PIC  9(03).
      *日付
               05  ACCTS-DATE-HI           PIC  9(02).
      *回数
           03  ACCTS-DAY-CNT               PIC  9(03).
      *----    以下検索用項目
      *入外区分
           03  ACCTS-VNYUGAIKBN            PIC  X(01).
      *診療年月
           03  ACCTS-VSRYYM                PIC  X(06).
      *範囲指定開始
           03  ACCTS-VFROM                 PIC  9(02).
      *範囲指定終了
           03  ACCTS-VTO                   PIC  9(02).
