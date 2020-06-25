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
      * Maj.Min.Rev  修正者     日付        内容
      *  01.00.03    高嶋       05/10/30    ７割追加対応
      *                                     薬剤一部負担金削除対応
      *  01.00.04    吉川       11/02/09    特別療養費国保対応
      *  01.00.05    門間       19/01/10    修正履歴コメント削除
      ********************************************************************
       01  SKT4030.
      *    診療年月
           03  SKT4030-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4030-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4030-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4030-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4030-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4030-TEL                 PIC  X(15).
      *    医療機関コード
           03  SKT4030-HOSPCD              PIC  X(20).
      *    保険者番号
           03  SKT4030-HKNJANUM            PIC  X(12).
      *    在総診・在医総
           03  SKT4030-ZAISOU1             PIC  X(02).
           03  SKT4030-ZAISOU2             PIC  X(02).
      *    ページ番号
           03  SKT4030-NO                  PIC  X(04).
      *    特別療養費
           03  SKT4030-TOKUBETU            PIC X(10).
      *    請求テーブル
           03  SKT4030-SEIKYU-TBL  OCCURS  7.
      *    入外テーブル
               05  SKT4030-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4030-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4030-NISSU       PIC  X(05).
      *    点数
                   07  SKT4030-TENSU       PIC  X(10).
      *    薬剤一部負担金
      *             07  SKT4030-YKZFTN      PIC  X(07).
      *    一部負担金
                   07  SKT4030-ITBFTN      PIC  X(10).
      *    食事療養：件数
               05  SKT4030-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4030-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4030-S-KINGK         PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4030-S-FTNGK         PIC  X(10).
      *    給付割合
           03  SKT4030-KYURATE-TBL.
               05  SKT4030-KYURATE         PIC  X(02) OCCURS 2.
