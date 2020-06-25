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
      *  後期高齢者請求書（宮城県）     COPY (SKT0415.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/16    田中          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    井上      2019/12/09   修正履歴コメント削除
      ********************************************************************
       01  SKT0415.
      *    診療年月
           03  SKT0415-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT0415-SEIYMD              PIC  X(22).
      *    住所
           03  SKT0415-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT0415-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT0415-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT0415-TEL                 PIC  X(15).
      *    保険者名
           03  SKT0415-HKNJANAME           PIC  X(20).
      *    保険者番号
           03  SKT0415-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT0415-HOSPCD              PIC  X(14).
      *    在宅
           03  SKT0415-ZAI1                PIC  X(02).
           03  SKT0415-ZAI2                PIC  X(02).
      *    ページ番号
           03  SKT0415-NO                  PIC  X(04).
      *    請求テーブル
           03  SKT0415-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT0415-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT0415-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT0415-NISSU       PIC  X(05).
      *    点数
                   07  SKT0415-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT0415-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT0415-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT0415-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT0415-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0415-S-FTNGK         PIC  X(09).
      *    公費請求テーブル
           03  SKT0415-KOHI-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT0415-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT0415-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT0415-KNISSU       PIC  X(05).
      *    点数
                   07  SKT0415-KTENSU       PIC  X(09).
      *    一部負担金
                   07  SKT0415-KITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT0415-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT0415-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT0415-S-KKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0415-S-KFTNGK         PIC  X(09).
