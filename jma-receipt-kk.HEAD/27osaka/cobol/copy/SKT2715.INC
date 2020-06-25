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
      *    後期高齢総括表・他府県分（大阪府）
      *
      *  作成日付    作業者        記述
      *  08/04/24    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付         内容
      * 01.00.01     門間     2018/12/03   修正履歴コメント削除
      ********************************************************************
        01  SKT2715.
      *    診療年月
            03  SKT2715-SRYYM             PIC X(16).
      *    請求年月日
            03  SKT2715-SEIYMD            PIC X(22).
      *    保険者名
            03  SKT2715-HKNJANAME         PIC X(20).
      *    住所
            03  SKT2715-ADRS              PIC X(100).
      *    医療機関名
            03  SKT2715-HOSPNAME          PIC X(100).
      *    開設者名
            03  SKT2715-KAISETUNAME       PIC X(40).
      *    電話番号
            03  SKT2715-TEL               PIC X(15).
      *    医療機関コード
            03  SKT2715-HOSPCD            PIC X(20).
      *    保険者番号
            03  SKT2715-HKNJANUM          PIC X(16).
      *    在総診・在医総
            03  SKT2715-ZAI1              PIC X(2).
            03  SKT2715-ZAI2              PIC X(2).
      *    ページ番号
            03  SKT2715-NO                PIC X(4).
      *    免除証明書数
            03  SKT2715-MENJO             PIC X(5).
      *    請求テーブル
            03  SKT2715-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
              05  SKT2715-NYUGAI-TBL  OCCURS  2.
      *    件数
                07  SKT2715-KENSU         PIC X(5).
      *    診療実日数
                07  SKT2715-NISSU         PIC X(5).
      *    点数
                07  SKT2715-TENSU         PIC X(10).
      *    一部負担金
                07  SKT2715-ITBFTN        PIC X(10).
      *    食事療養：件数
              05  SKT2715-S-KENSU         PIC X(5).
      *    食事療養：日数
              05  SKT2715-S-NISSU         PIC X(5).
      *    食事療養：金額
              05  SKT2715-S-KINGK         PIC X(10).
      *    食事療養：標準負担額
              05  SKT2715-S-FTNGK         PIC X(10).
      *    公費請求テーブル
            03  SKT2715-KOUHI-TBL  OCCURS  2.
      *    公費名
              05  SKT2715-HBTNUM          PIC X(2).
              05  SKT2715-KOHNAME         PIC X(10).
      *    入外テーブル
              05  SKT2715-KNYUGAI-TBL  OCCURS  2.
      *    件数
                07  SKT2715-KKENSU        PIC X(5).
      *    診療実日数
                07  SKT2715-KNISSU        PIC X(5).
      *    点数
                07  SKT2715-KTENSU        PIC X(10).
      *    一部負担金
                07  SKT2715-KITBFTN       PIC X(10).
      *    食事療養：件数
              05  SKT2715-S-KKENSU        PIC X(5).
      *    食事療養：日数
              05  SKT2715-S-KNISSU        PIC X(5).
      *    食事療養：金額
              05  SKT2715-S-KKINGK        PIC X(10).
      *    食事療養：標準負担額
              05  SKT2715-S-KFTNGK        PIC X(10).
