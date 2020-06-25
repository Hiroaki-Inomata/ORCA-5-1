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
      *  01.00.01    吉川    2011/06/15  特別療養費国保対応
      *  01.00.02    吉川    2011/11/09  保険者名のバイト数増加
      *  01.00.03    井上    2019/12/09  修正履歴コメント削除
      *  01.00.04    井上    2020/04/10  通常欄の点数において印字桁数を修正
      ********************************************************************
       01  SKT0410.
      *    診療年月
           03  SKT0410-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT0410-SEIYMD              PIC  X(22).
      *    住所
           03  SKT0410-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT0410-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT0410-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT0410-TEL                 PIC  X(15).
      *    保険者名
           03  SKT0410-HKNJANAME           PIC  X(40).
      *    保険者番号
           03  SKT0410-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT0410-HOSPCD              PIC  X(14).
      *    在宅
           03  SKT0410-ZAI1                PIC  X(02).
           03  SKT0410-ZAI2                PIC  X(02).
      *    給付割合
           03  SKT0410-WARI                PIC  X(02).
      *    ページ番号
           03  SKT0410-NO                  PIC  X(04).
      *    特別療養費
           03  SKT0410-TOKUBETU            PIC  X(10).
      *    請求テーブル
           03  SKT0410-SEIKYU-TBL  OCCURS  13.
      *    入外テーブル
               05  SKT0410-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT0410-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT0410-NISSU       PIC  X(05).
      *    点数
                   07  SKT0410-TENSU       PIC  X(10).
      *    一部負担金
                   07  SKT0410-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT0410-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT0410-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT0410-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0410-S-FTNGK         PIC  X(09).
      *    公費請求テーブル
           03  SKT0410-KOHI-TBL  OCCURS  9.
      *    入外テーブル
               05  SKT0410-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT0410-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT0410-KNISSU       PIC  X(05).
      *    点数
                   07  SKT0410-KTENSU       PIC  X(10).
      *    一部負担金
                   07  SKT0410-KITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT0410-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT0410-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT0410-S-KKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0410-S-KFTNGK         PIC  X(09).
