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
      * Maj/Min/Rev  修正者   日付         内容
      *  01.00.01    吉川     2009/06/10   保険者名バイト数増加
      *  01.00.02    吉川     2010/11/01   特別療養費国保対応
      *  01.00.03    井上     2018/12/12   修正履歴コメント削除
      ********************************************************************
       01  SKT2520.
      *    NO
           03  SKT2520-NO                  PIC  X(04).
      *    診療年月
           03  SKT2520-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT2520-SEIYMD              PIC  X(22).
      *    住所
           03  SKT2520-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT2520-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT2520-KAISETUNAME         PIC  X(80).
      *    電話番号
           03  SKT2520-TEL                 PIC  X(15).
      *    保険者名
           03  SKT2520-HKNJANAME           PIC  X(40).
      *    保険者番号
           03  SKT2520-HKNJANUM            PIC  X(06).
      *    医療機関コード
           03  SKT2520-HOSPCD              PIC  X(07).
      *    在総診・在医総
           03  SKT2520-ZAISOU1             PIC  X(02).
      *    特別療養費
           03  SKT2520-TOKUBETU            PIC  X(10).
      *    給付割合
           03  SKT2520-KYURATE-TBL  OCCURS  4.
               05  SKT2520-KYURATE         PIC  X(02).
      *    請求テーブル
           03  SKT2520-SEIKYU-TBL  OCCURS  11.
      *    件数
               05  SKT2520-KENSU       PIC  X(05).
      *    診療実日数
               05  SKT2520-NISSU       PIC  X(05).
      *    点数
               05  SKT2520-TENSU       PIC  X(09).
      *    一部負担金
               05  SKT2520-ITBFTN      PIC  X(09).
      *    福祉医療請求テーブル
           03  SKT2520-FSEIKYU-TBL.
      *    件数
               05  SKT2520-FKENSU       PIC  X(05).
      *    診療実日数
               05  SKT2520-FNISSU       PIC  X(05).
      *    点数
               05  SKT2520-FTENSU       PIC  X(09).
      *    一部負担金
               05  SKT2520-FITBFTN      PIC  X(09).
      *    公費医療請求テーブル
           03  SKT2520-KSEIKYU-TBL OCCURS 2.
      *    公費名
               05  SKT2520-KNAME        PIC  X(12).
      *    件数
               05  SKT2520-KKENSU       PIC  X(05).
      *    診療実日数
               05  SKT2520-KNISSU       PIC  X(05).
      *    点数
               05  SKT2520-KTENSU       PIC  X(09).
      *    一部負担金
               05  SKT2520-KITBFTN      PIC  X(09).
      *    長期高額疾病件数
           03  SKT2520-TYOKI-KENSU      PIC  X(05).
      *
      *    原爆老人併用件数
           03  SKT2520-GENBK-KENSU      PIC  X(05).
