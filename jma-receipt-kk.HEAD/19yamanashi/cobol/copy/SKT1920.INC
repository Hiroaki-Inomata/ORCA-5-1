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
      * Maj/Min/Rev 修正者  日付        内容
      *  01.00.01   吉川    2008/05/26  帳票様式の変更に伴う修正
      *  01.00.02   吉川    2008/06/05  医療機関コードバイト数増加
      *  01.00.03   吉川    2010/10/01  特別療養費国保対応
      *  01.00.04   井上    2018/11/12  修正履歴コメント削除
      ********************************************************************
       01  SKT1920.
      *    NO
           03  SKT1920-NO                  PIC  X(04).
      *    診療年月
           03  SKT1920-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT1920-SEIYMD              PIC  X(22).
      *    郵便番号
      *     03  SKT1920-POST                PIC  X(18).
      *    住所
           03  SKT1920-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT1920-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT1920-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT1920-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT1920-HKNJANAME           PIC  X(16).
      *    保険者番号
           03  SKT1920-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SKT1920-HOSPCD              PIC  X(20).
      *    給付割合
           03  SKT1920-KYURATE-TBL  OCCURS  3.
               05  SKT1920-KYURATE         PIC  X(02).
      *    在宅
           03  SKT1920-ZAI1                PIC  X(02).
           03  SKT1920-ZAI2                PIC  X(02).
      *    特別療養費
           03  SKT1920-TOKUBETU            PIC  X(10).
      *    請求テーブル
           03  SKT1920-SEIKYU-TBL  OCCURS  9.
      *    入外テーブル
               05  SKT1920-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT1920-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT1920-NISSU       PIC  X(05).
      *    点数
                   07  SKT1920-TENSU       PIC  X(09).
      *    薬剤一部負担金
                   07  SKT1920-YKZFTN      PIC  X(07).
      *    一部負担金
                   07  SKT1920-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT1920-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT1920-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT1920-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT1920-S-FTNGK         PIC  X(09).
      *    地方公費テーブル
           03  SKT1920-TKOUHI-TBL  OCCURS  3.
      *    入外テーブル
               05  SKT1920-TNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT1920-TKENSU      PIC  X(05).
      *    診療実日数
                   07  SKT1920-TNISSU      PIC  X(05).
      *    点数
                   07  SKT1920-TTENSU      PIC  X(09).
      *    一部負担金
                   07  SKT1920-TITBFTN     PIC  X(09).
      *    公費テーブル
           03  SKT1920-KOUHI-TBL  OCCURS  3.
      *    法別番号
               05  SKT1920-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT1920-KOHNAME         PIC  X(20).
      *    入外テーブル
               05  SKT1920-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT1920-KKENSU      PIC  X(05).
      *    診療実日数
                   07  SKT1920-KNISSU      PIC  X(05).
      *    点数
                   07  SKT1920-KTENSU      PIC  X(09).
      *    一部負担金
                   07  SKT1920-KITBFTN     PIC  X(09).
      *    食事療養：件数
               05  SKT1920-S-KKENSU        PIC  X(05).
      *    食事療養：日数
               05  SKT1920-S-KNISSU        PIC  X(05).
      *    食事療養：金額
               05  SKT1920-S-KKINGK        PIC  X(09).
      *    食事療養：標準負担額
               05  SKT1920-S-KFTNGK        PIC  X(09).
