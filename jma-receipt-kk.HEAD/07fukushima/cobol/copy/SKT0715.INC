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
      *    後期高齢請求書（福島県）
      *
      *  作成日付    作業者        記述
      *  08/04/26    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT0715.
      *    診療年月
            03  SKT0715-SRYYM            PIC X(16).
      *    請求年月日
            03  SKT0715-SEIYMD           PIC X(22).
      *    保険者名
            03  SKT0715-HKNJANAME        PIC X(20).
      *    住所
            03  SKT0715-ADRS             PIC X(100).
      *    医療機関名
            03  SKT0715-HOSPNAME         PIC X(100).
      *    開設者名
            03  SKT0715-KAISETUNAME      PIC X(40).
      *    電話番号
            03  SKT0715-TEL              PIC X(15).
      *    医療機関コード
            03  SKT0715-HOSPCD           PIC X(14).
      *    保険者番号
            03  SKT0715-HKNJANUM         PIC X(12).
      *    在総診・在医総
            03  SKT0715-ZAI1             PIC X(2).
            03  SKT0715-ZAI2             PIC X(2).
      *    ページ番号
            03  SKT0715-NO               PIC X(4).
      *    老人請求テーブル
            03  SKT0715-SEIKYU-TBL  OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT0715-NYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT0715-KENSU        PIC X(5).
      *    診療実日数
                07  SKT0715-NISSU        PIC X(5).
      *    点数
                07  SKT0715-TENSU        PIC X(9).
      *    一部負担金
                07  SKT0715-ITBFTN       PIC X(9).
      *    食事療養：件数
              05  SKT0715-S-KENSU        PIC X(5).
      *    食事療養：日数
              05  SKT0715-S-NISSU        PIC X(5).
      *    食事療養：金額
              05  SKT0715-S-KINGK        PIC X(9).
      *    食事療養：標準負担額
              05  SKT0715-S-FTNGK        PIC X(9).
      *    公費請求テーブル
            03  SKT0715-KOUHI-TBL  OCCURS  10 TIMES.
      *    公費名
              05  SKT0715-HBTNUM         PIC X(2).
              05  SKT0715-KOHNAME        PIC X(10).
      *    入外テーブル
              05  SKT0715-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT0715-KKENSU       PIC X(5).
      *    診療実日数
                07  SKT0715-KNISSU       PIC X(5).
      *    点数
                07  SKT0715-KTENSU       PIC X(9).
      *    一部負担金
                07  SKT0715-KITBFTN      PIC X(9).
      *    食事療養：件数
              05  SKT0715-S-KKENSU       PIC X(5).
      *    食事療養：日数
              05  SKT0715-S-KNISSU       PIC X(5).
      *    食事療養：金額
              05  SKT0715-S-KKINGK       PIC X(9).
      *    食事療養：標準負担額
              05  SKT0715-S-KFTNGK       PIC X(9).
