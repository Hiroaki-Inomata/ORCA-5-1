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
      *    後期高齢総括表（鹿児島県）
      *
      *  作成日付    作業者  記述
      *  08/04/27    大竹    新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    11/06/17    食事日数の桁を増やす
      *  01.00.02    門間    19/02/04    修正履歴コメント削除
      ********************************************************************
        01  SKT4615.
      *    診療年月
            03  SKT4615-SRYYM             PIC X(16).
      *    請求年月日
            03  SKT4615-SEIYMD            PIC X(22).
      *    住所
            03  SKT4615-ADRS              PIC X(100).
      *    医療機関名
            03  SKT4615-HOSPNAME          PIC X(100).
      *    開設者名
            03  SKT4615-KAISETUNAME       PIC X(40).
      *    医療機関コード
            03  SKT4615-HOSPCD            PIC X(20).
      *    保険者名
            03  SKT4615-HKNJANAME         PIC X(20).
      *    保険者番号
            03  SKT4615-HKNJANUM          PIC X(12).
      *    在総診・在医総
            03  SKT4615-ZAI1           PIC X(02).
            03  SKT4615-ZAI2           PIC X(02).
      *    ページ番号
            03  SKT4615-NO                PIC X(04).
      *    老人請求テーブル
            03  SKT4615-SEIKYU-TBL  OCCURS  2 TIMES.
      *    入外テーブル
              05  SKT4615-NYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT4615-KENSU         PIC X(05).
      *    診療実日数
                07  SKT4615-NISSU         PIC X(05).
      *    点数
                07  SKT4615-TENSU         PIC X(10).
      *    一部負担金
                07  SKT4615-ITBFTN        PIC X(10).
      *    食事療養：件数
              05  SKT4615-S-KENSU         PIC X(05).
      *    食事療養：日数
              05  SKT4615-S-NISSU         PIC X(06).
      *    食事療養：金額
              05  SKT4615-S-KINGK         PIC X(10).
      *    食事療養：標準負担額
              05  SKT4615-S-FTNGK         PIC X(10).
      *    公費請求テーブル
            03  SKT4615-KOUHI-TBL  OCCURS  4 TIMES.
      *    公費名
              05  SKT4615-HBTNUM          PIC X(02).
              05  SKT4615-KOHNAME         PIC X(10).
      *    入外テーブル
              05  SKT4615-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    件数
                07  SKT4615-KKENSU        PIC X(05).
      *    診療実日数
                07  SKT4615-KNISSU        PIC X(05).
      *    点数
                07  SKT4615-KTENSU        PIC X(10).
      *    一部負担金
                07  SKT4615-KITBFTN       PIC X(10).
      *    食事療養：件数
              05  SKT4615-S-KKENSU        PIC X(05).
      *    食事療養：日数
              05  SKT4615-S-KNISSU        PIC X(06).
      *    食事療養：金額
              05  SKT4615-S-KKINGK        PIC X(10).
      *    食事療養：標準負担額
              05  SKT4615-S-KFTNGK        PIC X(10).
