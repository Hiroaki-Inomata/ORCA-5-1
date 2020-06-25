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
      *  01.00.01    吉川      2010/10/13   特別療養費国保対応
      *  01.00.02    萩野      2018/10/11   修正履歴コメント削除
      ********************************************************************
       01  SKT2120.
      *    NO
           03  SKT2120-NO                  PIC  X(04).
      *    診療年月
           03  SKT2120-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT2120-SEIYMD              PIC  X(22).
      *    住所
           03  SKT2120-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT2120-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT2120-KAISETUNAME         PIC  X(60).
      *    電話番号
           03  SKT2120-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT2120-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT2120-HOSPCD              PIC  X(14).
      *    在宅
           03  SKT2120-ZAI1                PIC  X(02).
           03  SKT2120-ZAI2                PIC  X(02).
      *    点数表区分
           03  SKT2120-TENHYOKBN           PIC  X(04).
      *    給付割合
           03  SKT2120-KYURATE-TBL  OCCURS  4.
               05  SKT2120-KYURATE         PIC  X(02).
      *    診療科
           03  SKT2120-SRYKA               PIC  X(04).
      *    特別療養費
           03  SKT2120-TOKUBETU            PIC  X(10).
      *    請求テーブル
           03  SKT2120-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT2120-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2120-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT2120-NISSU       PIC  X(05).
      *    点数
                   07  SKT2120-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT2120-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT2120-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT2120-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT2120-S-KINGAKU       PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2120-S-FTNGAKU       PIC  X(09).
      *    公費テーブル
           03  SKT2120-KOH-TBL  OCCURS  3.
      *    公費名
               05  SKT2120-KOHNAME         PIC  X(08).
      *    入外テーブル
               05  SKT2120-K-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT2120-KKENSU      PIC  X(05).
      *    診療実日数
                   07  SKT2120-KNISSU      PIC  X(05).
      *    点数
                   07  SKT2120-KTENSU      PIC  X(09).
      *    一部負担金
                   07  SKT2120-KITBFTN     PIC  X(09).
      *    食事療養：件数
               05  SKT2120-S-KKENSU        PIC  X(05).
      *    食事療養：日数
               05  SKT2120-S-KNISSU        PIC  X(05).
      *    食事療養：金額
               05  SKT2120-S-KKINGAKU      PIC  X(09).
      *    食事療養：標準負担額
               05  SKT2120-S-KFTNGAKU      PIC  X(09).
      *    長期：件数
           03  SKT2120-TYOKI-TBL  OCCURS  2.
               05  SKT2120-TKENSU          PIC  X(02).
      *    長処：件数
           03  SKT2120-TYOKIT-TBL  OCCURS  2.
               05  SKT2120-TTKENSU         PIC  X(02).
