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
      *    後期高齢総括表（山形県）
      *
      *  作成日付    作業者        記述
      *  08/04/25    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT0915.
      *    診療年月
            03  SKT0915-SRYYM             PIC X(16).
      *    請求年月日
            03  SKT0915-SEIYMD            PIC X(22).
      *    住所
            03  SKT0915-ADRS              PIC X(100).
      *    医療機関名
            03  SKT0915-HOSPNAME          PIC X(100).
      *    開設者名
            03  SKT0915-KAISETUNAME       PIC X(40).
      *    医療機関コード
            03  SKT0915-HOSPCD            PIC X(14).
      *    保険者番号
            03  SKT0915-HKNJANUM          PIC X(16).
      *    在総診・在医総
            03  SKT0915-ZAI1              PIC X(2).
            03  SKT0915-ZAI2              PIC X(2).
      *    ページ番号
            03  SKT0915-NO                PIC X(4).
      *    請求テーブル
            03  SKT0915-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
              05  SKT0915-NYUGAI-TBL  OCCURS  2.
      *    件数
                07  SKT0915-KENSU         PIC X(5).
      *    診療実日数
                07  SKT0915-NISSU         PIC X(5).
      *    点数
                07  SKT0915-TENSU         PIC X(9).
      *    一部負担金
                07  SKT0915-ITBFTN        PIC X(9).
      *    食事療養：件数
              05  SKT0915-S-KENSU         PIC X(5).
      *    食事療養：日数
              05  SKT0915-S-NISSU         PIC X(5).
      *    食事療養：金額
              05  SKT0915-S-KINGK         PIC X(9).
      *    食事療養：標準負担額
              05  SKT0915-S-FTNGK         PIC X(9).
      *    公費請求テーブル
            03  SKT0915-KOUHI-TBL  OCCURS  4.
      *    公費名
              05  SKT0915-HBTNUM          PIC X(2).
              05  SKT0915-KOHNAME         PIC X(10).
      *    入外テーブル
             05  SKT0915-KNYUGAI-TBL  OCCURS  2.
      *    件数
                07  SKT0915-KKENSU        PIC X(5).
      *    診療実日数
                07  SKT0915-KNISSU        PIC X(5).
      *    点数
                07  SKT0915-KTENSU        PIC X(9).
      *    一部負担金
                07  SKT0915-KITBFTN       PIC X(9).
      *    食事療養：件数
              05  SKT0915-S-KKENSU        PIC X(5).
      *    食事療養：日数
              05  SKT0915-S-KNISSU        PIC X(5).
      *    食事療養：金額
              05  SKT0915-S-KKINGK        PIC X(9).
      *    食事療養：標準負担額
              05  SKT0915-S-KFTNGK        PIC X(9).
