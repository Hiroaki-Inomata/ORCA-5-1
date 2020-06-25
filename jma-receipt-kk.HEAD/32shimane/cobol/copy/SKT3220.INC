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
      *  01.00.01    吉川      2010/12/09   特別療養費国保対応
      *  01.00.02    井上      2019/01/10   修正履歴コメント削除
      ********************************************************************
       01  SKT3220.
      *    診療年月
           03  SKT3220-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT3220-SEIYMD              PIC  X(22).
      *    住所
           03  SKT3220-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT3220-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT3220-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT3220-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT3220-HKNJANUM            PIC  X(12).
      *    保険者名
           03  SKT3220-HKNJANAME           PIC  X(40).
      *    医療機関コード
           03  SKT3220-HOSPCD              PIC  X(14).
      *    在総◯枠
           03  SKT3220-ZAIMARU             PIC  X(02).
      *    在総文字
           03  SKT3220-ZAIMOJI             PIC  X(02).
      *    特別療養費
           03  SKT3220-TOKUBETU            PIC  X(10).
      *    NO
           03  SKT3220-NO                  PIC  X(04).
      *    請求テーブル
           03  SKT3220-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT3220-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3220-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3220-NISSU       PIC  X(05).
      *    点数
                   07  SKT3220-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT3220-ITBFTN      PIC  X(09).
      *    長期療養件数
                   07  SKT3220-TYOUKEN     PIC  X(03).
      *    原爆療養件数
                   07  SKT3220-GENKEN      PIC  X(03).
      *    食事療養：件数
               05  SKT3220-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3220-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3220-S-KINGAKU       PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3220-S-FTNGAKU       PIC  X(09).
      *    公費テーブル
           03  SKT3220-KOH-TBL  OCCURS  17.
      *    公費コード
               05  SKT3220-KOHCD           PIC  X(02).
      *    公費名
               05  SKT3220-KOHNAME         PIC  X(12).
      *    件数
               05  SKT3220-KKENSU          PIC  X(05).
      *    点数
               05  SKT3220-KTENSU          PIC  X(09).
      *    患者負担額
               05  SKT3220-KKNJFTN         PIC  X(07).
      *    食事療養：件数
               05  SKT3220-S-KKENSU        PIC  X(05).
      *    食事療養：金額
               05  SKT3220-S-KKINGAKU      PIC  X(07).
      *    食事療養：標準負担額
               05  SKT3220-S-KFTNGAKU      PIC  X(07).
