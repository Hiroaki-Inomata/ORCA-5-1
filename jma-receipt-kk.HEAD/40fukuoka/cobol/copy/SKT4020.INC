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
      *  01.00.01    吉川    2011/02/09  特別療養費国保対応
      *  01.00.02    門間    2019/01/10  修正履歴コメント削除
      ********************************************************************
          01  SKT4020.
      *    診療年月
           03  SKT4020-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4020-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4020-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4020-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4020-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4020-TEL                 PIC  X(15).
      *    医療機関コード
           03  SKT4020-HOSPCD              PIC  X(20).
      *    保険者番号
           03  SKT4020-HKNJANUM            PIC  X(12).
      *    在総診・在医総
           03  SKT4020-ZAISOU1             PIC  X(02).
           03  SKT4020-ZAISOU2             PIC  X(02).
      *    ページ番号
           03  SKT4020-NO                  PIC  X(04).
      *    特別療養費
           03  SKT4020-TOKUBETU            PIC X(10).
      *    老人請求テーブル
           03  SKT4020-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT4020-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4020-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4020-NISSU       PIC  X(05).
      *    点数
                   07  SKT4020-TENSU       PIC  X(10).
      *    一部負担金
                   07  SKT4020-ITBFTN      PIC  X(10).
      *    食事療養：件数
               05  SKT4020-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4020-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4020-S-KINGK         PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4020-S-FTNGK         PIC  X(10).
      *    老人原爆請求テーブル
           03  SKT4020-GENBK-TBL.
      *    入外テーブル
               05  SKT4020-GNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4020-GKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4020-GNISSU       PIC  X(05).
      *    点数
                   07  SKT4020-GTENSU       PIC  X(10).
      *    一部負担金
                   07  SKT4020-GITBFTN      PIC  X(10).
      *    患者負担額
                   07  SKT4020-GPTFTN       PIC  X(10).
      *    食事療養：件数
               05  SKT4020-S-GKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4020-S-GNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4020-S-GKINGK         PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4020-S-GFTNGK         PIC  X(10).
      *
      *    公費請求テーブル
           03  SKT4020-KOUHI-TBL  OCCURS  4.
      *    公費名
               05  SKT4020-HBTNUM          PIC  X(02).
               05  SKT4020-KOHNAME         PIC  X(10).
      *    入外テーブル
               05  SKT4020-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4020-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4020-KNISSU       PIC  X(05).
      *    点数
                   07  SKT4020-KTENSU       PIC  X(10).
      *    一部負担金
                   07  SKT4020-KITBFTN      PIC  X(10).
      *    患者負担額
                   07  SKT4020-KPTFTN       PIC  X(10).
      *    食事療養：件数
               05  SKT4020-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4020-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4020-S-KKINGK         PIC  X(10).
      *    食事療養：標準負担額
               05  SKT4020-S-KFTNGK         PIC  X(10).
