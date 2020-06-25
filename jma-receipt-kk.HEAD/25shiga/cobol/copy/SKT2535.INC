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
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川      2010/11/01   特別療養費国保対応
      *  01.00.02    井上      2018/12/12   修正履歴コメント削除
      ********************************************************************
       01  SKT2535.
      *    NO
           03  SKT2535-NO                  PIC  X(04).
      *    保険者番号
           03  SKT2535-HKNJANUM            PIC  X(06).
      *    医療機関コード
           03  SKT2535-HOSPCD              PIC  X(07).
      *    特別療養費
           03  SKT2535-TOKUBETU            PIC  X(10).
      *    給付割合
      *    老人請求テーブル
           03  SKT2535-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT2535-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2535-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT2535-NISSU       PIC  X(05).
      *    点数
                   07  SKT2535-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT2535-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT2535-SKENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT2535-SNISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT2535-SKINGK          PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2535-SFTNGK          PIC  X(09).
      *
      *    福祉請求テーブル
           03  SKT2535-FUKUSHI-TBL.
      *    入外テーブル
               05  SKT2535-FNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2535-F-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT2535-F-NISSU       PIC  X(05).
      *    点数
                   07  SKT2535-F-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT2535-F-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT2535-F-SKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT2535-F-SNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT2535-F-SKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2535-F-SFTNGK         PIC  X(09).
      *
      *    公費請求テーブル
           03  SKT2535-KOUHI-TBL  OCCURS  4.
      *    公費名
               05  SKT2535-KOHNAME         PIC  X(12).
      *    入外テーブル
               05  SKT2535-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2535-K-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT2535-K-NISSU       PIC  X(05).
      *    点数
                   07  SKT2535-K-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT2535-K-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT2535-K-SKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT2535-K-SNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT2535-K-SKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2535-K-SFTNGK         PIC  X(09).
      *    長期高額疾病件数
           03  SKT2535-TYOKI-KENSU          PIC  X(05).
      *    老人・原爆併用件数
           03  SKT2535-GENBK-KENSU          PIC  X(05).
