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
      *    後期高齢請求書（高知県）     COPY (SKT3925.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/09    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
       01  SKT3925.
      *    診療年月
           03  SKT3925-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT3925-SEIYMD              PIC  X(22).
      *    住所
           03  SKT3925-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT3925-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT3925-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT3925-TEL                 PIC  X(15).
      *    保険者名
           03  SKT3925-HKNJANAME           PIC  X(100).
      *    保険者番号
           03  SKT3925-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT3925-HOSPCD              PIC  X(14).
      *    在総診・在医総
           03  SKT3925-ZAI1                PIC  X(02).
           03  SKT3925-ZAI2                PIC  X(02).
      *    ページ番号
           03  SKT3925-NO                  PIC  X(04).
      *    後期高齢者請求テーブル
           03  SKT3925-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT3925-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3925-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3925-NISSU       PIC  X(05).
      *    点数
                   07  SKT3925-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT3925-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT3925-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3925-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3925-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3925-S-FTNGK         PIC  X(09).
      *
      *    公費請求テーブル
           03  SKT3925-KOUHI-TBL  OCCURS  2.
      *    法別番号
               05  SKT3925-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT3925-KOHNAME         PIC  X(12).
      *    入外テーブル
               05  SKT3925-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3925-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3925-KNISSU       PIC  X(05).
      *    点数
                   07  SKT3925-KTENSU       PIC  X(09).
      *    一部負担金
                   07  SKT3925-KITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT3925-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3925-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3925-S-KKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3925-S-KFTNGK         PIC  X(09).
