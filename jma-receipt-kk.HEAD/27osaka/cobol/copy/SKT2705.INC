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
      ********************************************************************
      *    後期高齢総括表・府内分（大阪府）
      *
      *  作成日付    作業者        記述
      *  08/04/24    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT2705.
      *    診療年月
            03  SKT2705-SRYYM             PIC X(16).
      *    請求年月日
            03  SKT2705-SEIYMD            PIC X(22).
      *    住所
            03  SKT2705-ADRS              PIC X(100).
      *    医療機関名
            03  SKT2705-HOSPNAME          PIC X(100).
      *    開設者名
            03  SKT2705-KAISETUNAME       PIC X(40).
      *    電話番号
            03  SKT2705-TEL               PIC X(15).
      *    医療機関コード
            03  SKT2705-HOSPCD            PIC X(20).
      *    在総診・在医総
            03  SKT2705-ZAI1              PIC X(2).
            03  SKT2705-ZAI2              PIC X(2).
      *    ページ番号
            03  SKT2705-NO                PIC X(4).
      *    免除証明書数
            03  SKT2705-MENJO             PIC X(5).
      *    他府県公費所持者件数
            03  SKT2705-TAKOH             PIC X(5).
      *    請求テーブル
            03  SKT2705-SEIKYU-TBL  OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT2705-NYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT2705-KENSU         PIC X(5).
      *    診療実日数
                07  SKT2705-NISSU         PIC X(5).
      *    点数
                07  SKT2705-TENSU         PIC X(10).
      *    一部負担金
                07  SKT2705-ITBFTN        PIC X(10).
      *    食事療養：件数
              05  SKT2705-S-KENSU         PIC X(5).
      *    食事療養：日数
              05  SKT2705-S-NISSU         PIC X(5).
      *    食事療養：金額
              05  SKT2705-S-KINGK         PIC X(10).
      *    食事療養：標準負担額
              05  SKT2705-S-FTNGK         PIC X(10).
      *    公費請求テーブル
            03  SKT2705-KOUHI-TBL  OCCURS  2 TIMES.
      *    公費名
              05  SKT2705-HBTNUM          PIC X(2).
              05  SKT2705-KOHNAME         PIC X(10).
      *    入外テーブル
              05  SKT2705-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT2705-KKENSU        PIC X(5).
      *    診療実日数
                07  SKT2705-KNISSU        PIC X(5).
      *    点数
                07  SKT2705-KTENSU        PIC X(10).
      *    一部負担金
                07  SKT2705-KITBFTN       PIC X(10).
      *    食事療養：件数
              05  SKT2705-S-KKENSU        PIC X(5).
      *    食事療養：日数
              05  SKT2705-S-KNISSU        PIC X(5).
      *    食事療養：金額
              05  SKT2705-S-KKINGK        PIC X(10).
      *    食事療養：標準負担額
              05  SKT2705-S-KFTNGK        PIC X(10).
