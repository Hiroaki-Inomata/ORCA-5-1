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
      *  プログラム修正履歴
      * Maj/Min/Rev 	修正者	日付		内容
      *  01.00.01    長屋    08/04/29    帳票様式に伴う修正
      *  01.00.02    吉川    11/02/09    特別療養費国保対応
      *  01.00.03    萩野    18/10/10    修正履歴コメント削除
      ********************************************************************
       01  SKT0710.
      *    診療年月
           03  SKT0710-SRYYM                PIC  X(16).
      *    医療機関住所
           03  SKT0710-ADRS                 PIC  X(100).
      *    医療機関名
           03  SKT0710-HOSPNAME             PIC  X(100).
      *    電話番号
           03  SKT0710-TEL                  PIC  X(15).
      *    開設者名
           03  SKT0710-KAISETUNAME          PIC  X(40).
      *    保険者名
           03  SKT0710-HKNJANAME            PIC  X(20).
      *    請求年月日
           03  SKT0710-SEIYMD               PIC  X(22).
      *    保険者番号
           03  SKT0710-HKNJANUM             PIC  X(12).
      *    医療機関コード
           03  SKT0710-HOSPCD               PIC  X(14).
      *    在宅
           03  SKT0710-ZAI1                 PIC  X(02).
           03  SKT0710-ZAI2                 PIC  X(02).
      *    旧老人
           03  SKT0710-ROUJIN               PIC  X(10).
      *    ページ番号
           03  SKT0710-NO                   PIC  X(04).
      *    旧７０歳以上９割対応
           03  SKT0710-KYUHANTEI            PIC  X(02).
      *    特別療養費
           03  SKT0710-TOKUBETU             PIC  X(10).
      *    請求テーブル
           03  SKT0710-SEIKYU-TBL     OCCURS  14.
      *    入外テーブル
               05  SKT0710-NYUGAI-TBL     OCCURS  2.
      *    件数
                   07  SKT0710-KENSU        PIC  X(05).
      *    実日数
                   07  SKT0710-NISSU        PIC  X(05).
      *    点数
                   07  SKT0710-TENSU        PIC  X(10).
      *    一部負担金
                   07  SKT0710-ITBFTN       PIC  X(09).
      *    食事療養：件数
               05  SKT0710-S-KENSU          PIC  X(05).
      *    食事療養：日数
               05  SKT0710-S-NISSU          PIC  X(05).
      *    食事療養：金額
               05  SKT0710-S-KINGAKU        PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0710-S-FTNGAKU        PIC  X(09).
      *    公費テーブル
           03  SKT0710-KOH-TBL     OCCURS  5.
      *    公費名
               05  SKT0710-KOHNAME          PIC  X(08).
      *    入外テーブル
               05  SKT0710-K-NYUGAI-TBL     OCCURS  2.
      *    件数
                   07  SKT0710-KKENSU       PIC  X(05).
      *    実日数
                   07  SKT0710-KNISSU       PIC  X(05).
      *    点数
                   07  SKT0710-KTENSU       PIC  X(10).
      *    一部負担金
                   07  SKT0710-KITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT0710-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT0710-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT0710-S-KKINGAKU       PIC  X(09).
      *    食事療養：標準負担額
               05  SKT0710-S-KFTNGAKU       PIC  X(09).
