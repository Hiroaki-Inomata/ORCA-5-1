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
      *  01.00.02    門間    2019/01/07  修正履歴コメント削除
      ********************************************************************
       01  SKT3930.
      *    診療年月
           03  SKT3930-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT3930-SEIYMD              PIC  X(22).
      *    住所
           03  SKT3930-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT3930-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT3930-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT3930-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT3930-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT3930-HOSPCD              PIC  X(14).
      *    在総診・在医総
           03  SKT3930-ZAI1             PIC  X(02).
           03  SKT3930-ZAI2             PIC  X(02).
      *    ページ番号
           03  SKT3930-NO               PIC  X(04).
      *    特別療養費
           03  SKT3930-TOKUBETU            PIC X(10).
      *    老人請求テーブル
           03  SKT3930-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT3930-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3930-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3930-NISSU       PIC  X(05).
      *    点数
                   07  SKT3930-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT3930-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT3930-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3930-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3930-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3930-S-FTNGK         PIC  X(09).
      *    原爆件数
               05  SKT3930-GENBK           PIC  X(05).
      *
      *    公費請求テーブル
           03  SKT3930-KOUHI-TBL  OCCURS  2.
      *    法別番号
               05  SKT3930-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT3930-KOHNAME         PIC  X(12).
      *    入外テーブル
               05  SKT3930-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3930-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3930-KNISSU       PIC  X(05).
      *    点数
                   07  SKT3930-KTENSU       PIC  X(09).
      *    薬剤一部負担金
                   07  SKT3930-KYKZFTN      PIC  X(07).
      *    一部負担金
                   07  SKT3930-KITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT3930-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3930-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3930-S-KKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3930-S-KFTNGK         PIC  X(09).
