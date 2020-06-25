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
      *    後期高齢請求書（秋田県）
      *
      *  作成日付    作業者        記述
      *  08/04/27    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT0525.
      *    診療年月
            03  SKT0525-SRYYM             PIC X(16).
      *    請求年月日
            03  SKT0525-SEIYMD            PIC X(22).
      *    点数表区分
            03  SKT0525-TENHYOKBN-TBL     OCCURS  2.
                05  SKT0525-TENHYOKBN     PIC X(02).
      *    保険者名
            03  SKT0525-HKNJANAME         PIC X(30).
      *    保険者番号
            03  SKT0525-HKNJANUM          PIC X(04).
      *    住所
            03  SKT0525-ADRS              PIC X(100).
      *    医療機関名
            03  SKT0525-HOSPNAME          PIC X(100).
      *    開設者名
            03  SKT0525-KAISETUNAME       PIC X(40).
      *    医療機関コード
            03  SKT0525-HOSPCD            PIC X(20).
      *    在総診・在医総
            03  SKT0525-ZAI1           PIC X(02).
            03  SKT0525-ZAI2           PIC X(02).
      *    ページ番号
            03  SKT0525-NO                PIC X(04).
      *    老人請求テーブル
            03  SKT0525-SEIKYU-TBL  OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT0525-NYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT0525-KENSU         PIC X(05).
      *    診療実日数
                07  SKT0525-NISSU         PIC X(05).
      *    点数
                07  SKT0525-TENSU         PIC X(10).
      *    一部負担金
                07  SKT0525-ITBFTN        PIC X(10).
      *    食事療養：件数
              05  SKT0525-S-KENSU         PIC X(05).
      *    食事療養：日数
              05  SKT0525-S-NISSU         PIC X(05).
      *    食事療養：金額
              05  SKT0525-S-KINGK         PIC X(10).
      *    食事療養：標準負担額
              05  SKT0525-S-FTNGK         PIC X(10).
      *    公費請求テーブル
            03  SKT0525-KOUHI-TBL.
      *    公費名
              05  SKT0525-HBTNUM          PIC X(02).
              05  SKT0525-KOHNAME         PIC X(10).
      *    入外テーブル
              05  SKT0525-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT0525-KKENSU        PIC X(05).
      *    診療実日数
                07  SKT0525-KNISSU        PIC X(05).
      *    点数
                07  SKT0525-KTENSU        PIC X(10).
      *    一部負担金
                07  SKT0525-KITBFTN       PIC X(10).
      *    食事療養：件数
              05  SKT0525-S-KKENSU        PIC X(05).
      *    食事療養：日数
              05  SKT0525-S-KNISSU        PIC X(05).
      *    食事療養：金額
              05  SKT0525-S-KKINGK        PIC X(10).
      *    食事療養：標準負担額
              05  SKT0525-S-KFTNGK        PIC X(10).
