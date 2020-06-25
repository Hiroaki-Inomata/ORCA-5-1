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
      *    後期高齢者請求書（福岡県）     COPY (SKT4025.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/16    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         10/06/10  原爆用法別番号追加
      *  01.00.02    門間         19/01/10  修正履歴コメント削除
      ********************************************************************
          01  SKT4025.
      *    診療年月
           03  SKT4025-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4025-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4025-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4025-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4025-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4025-TEL                 PIC  X(15).
      *    医療機関コード
           03  SKT4025-HOSPCD              PIC  X(20).
      *    保険者名
           03  SKT4025-HKNJANAME           PIC  X(100).
      *    保険者番号
           03  SKT4025-HKNJANUM            PIC  X(12).
      *    在総診・在医総
           03  SKT4025-ZAI1                PIC  X(02).
           03  SKT4025-ZAI2                PIC  X(02).
      *    ページ番号
           03  SKT4025-NO                  PIC  X(04).
      *    老人請求テーブル
           03  SKT4025-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT4025-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4025-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4025-NISSU       PIC  X(05).
      *    点数
                   07  SKT4025-TENSU       PIC  X(10).
      *    一部負担金
                   07  SKT4025-ITBFTN      PIC  X(10).
      *    食事療養：件数
               05  SKT4025-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4025-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4025-S-KINGK         PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4025-S-FTNGK         PIC  X(10).
      *    原爆請求テーブル
           03  SKT4025-GENBK-TBL.
      *    法別番号
               05  SKT4025-GHBTNUM         PIC  X(02).
      *    入外テーブル
               05  SKT4025-GNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4025-GKENSU      PIC  X(05).
      *    診療実日数
                   07  SKT4025-GNISSU      PIC  X(05).
      *    点数
                   07  SKT4025-GTENSU      PIC  X(10).
      *    一部負担金
                   07  SKT4025-GITBFTN     PIC  X(10).
      *    患者負担額
                   07  SKT4025-GPTFTN      PIC  X(10).
      *    食事療養：件数
               05  SKT4025-S-GKENSU        PIC  X(05).
      *    食事療養：日数
               05  SKT4025-S-GNISSU        PIC  X(05).
      *    食事療養：金額
               05  SKT4025-S-GKINGK        PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4025-S-GFTNGK        PIC  X(10).
      *
      *    公費請求テーブル
           03  SKT4025-KOUHI-TBL  OCCURS  4.
      *    公費名
               05  SKT4025-HBTNUM          PIC  X(02).
      *    入外テーブル
               05  SKT4025-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4025-KKENSU      PIC  X(05).
      *    診療実日数
                   07  SKT4025-KNISSU      PIC  X(05).
      *    点数
                   07  SKT4025-KTENSU      PIC  X(10).
      *    一部負担金
                   07  SKT4025-KITBFTN     PIC  X(10).
      *    患者負担額
                   07  SKT4025-KPTFTN      PIC  X(10).
      *    食事療養：件数
               05  SKT4025-S-KKENSU        PIC  X(05).
      *    食事療養：日数
               05  SKT4025-S-KNISSU        PIC  X(05).
      *    食事療養：金額
               05  SKT4025-S-KKINGK        PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4025-S-KFTNGK        PIC  X(10).
