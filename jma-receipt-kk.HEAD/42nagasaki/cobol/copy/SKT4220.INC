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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2011/01/12  特別療養費国保対応
      *  01.00.02    萩野    2019/02/07  修正履歴コメント削除
      ********************************************************************
       01  SKT4220.
      *    診療年月
           03  SKT4220-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4220-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4220-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4220-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4220-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4220-TEL                 PIC  X(15).
      *    医療機関コード
           03  SKT4220-HOSPCD              PIC  X(07).
      *    保険者番号
           03  SKT4220-HKNJANUM            PIC  X(06).
      *    保険者名
           03  SKT4220-HKNJANAME           PIC  X(20).
      *    在総診・在医総
           03  SKT4220-ZAI1                PIC  X(02).
           03  SKT4220-ZAI2                PIC  X(02).
      *    長期
           03  SKT4220-TYOKI1              PIC  X(02).
           03  SKT4220-TYOKI2              PIC  X(02).
      *    ページ番号
           03  SKT4220-NO                  PIC  X(04).
      *    特別療養費
           03  SKT4220-TOKUBETU            PIC  X(10).
      *    請求テーブル
           03  SKT4220-SEIKYU-TBL  OCCURS  4.
      *    入外テーブル
               05  SKT4220-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4220-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4220-NISSU       PIC  X(05).
      *    点数
                   07  SKT4220-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT4220-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT4220-SKENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT4220-SNISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT4220-SKINGK          PIC  X(09).
      *    食事療養：標準負担額
               05  SKT4220-SFTNGK          PIC  X(09).
      *    公費テーブル
           03  SKT4220-KOH-TBL  OCCURS 4.
      *    法別番号
               05  SKT4220-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT4220-KOHNAME         PIC  X(08).
      *    入外テーブル
               05  SKT4220-K-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4220-K-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4220-K-NISSU       PIC  X(05).
      *    点数
                   07  SKT4220-K-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT4220-K-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT4220-K-SKENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT4220-K-SNISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT4220-K-SKINGK          PIC  X(09).
      *    食事療養：標準負担額
               05  SKT4220-K-SFTNGK          PIC  X(09).
