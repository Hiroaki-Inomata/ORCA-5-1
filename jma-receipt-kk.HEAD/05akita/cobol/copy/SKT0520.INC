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
      *  01.00.02    井上    2019/12/09  修正履歴コメント削除
      ********************************************************************
       01  SKT0520.
      *    NO
           03  SKT0520-NO                PIC  X(04).
      *    診療年月
           03  SKT0520-SRYYM             PIC  X(16).
      *    請求年月日
           03  SKT0520-SEIYMD            PIC  X(22).
      *    点数表区分
           03  SKT0520-TENHYOKBN-TBL     OCCURS  2.
               05  SKT0520-TENHYOKBN     PIC  X(02).
      *    診療科名
           03  SKT0520-SRYKANAME         PIC  X(20).
      *    医療機関コード
           03  SKT0520-HOSPCDN           PIC  X(20).
      *    医療機関住所
           03  SKT0520-ADRS              PIC  X(100).
      *    医療機関名
           03  SKT0520-HOSPNAME          PIC  X(100).
      *    開設者名
           03  SKT0520-KAISETU           PIC  X(80).
      *    保険者番号
           03  SKT0520-HKNJANUM          PIC  X(12).
      *    在総診
           03  SKT0520-ZAI1              PIC  X(02).
           03  SKT0520-ZAI2              PIC  X(02).
      *    特別療養費
           03  SKT0520-TOKUBETU          PIC  X(10).
      *    請求テーブル
           03  SKT0520-SEIKYU-TBL      OCCURS  11.
               05  SKT0520-NYUGAI-TBL  OCCURS 2.
      *    件数
                   07  SKT0520-KENSU     PIC  X(05).
      *    実日数
                   07  SKT0520-NISSU     PIC  X(05).
      *    点数
                   07  SKT0520-TENSU     PIC  X(09).
      *    一部負担金
                   07  SKT0520-ITBFTN    PIC  X(09).
      *    食事件数
               05  SKT0520-SKENSU        PIC  X(05).
      *    食事実日数
               05  SKT0520-SNISSU        PIC  X(05).
      *    金額
               05  SKT0520-SKINGK        PIC  X(09).
      *    標準負担額
               05  SKT0520-SFTNGK        PIC  X(09).
      *    公費・福祉テーブル
           03  SKT0520-KOHI-TBL.
               05  SKT0520-KNYUGAI-TBL  OCCURS 2.
      *    件数
                   07  SKT0520-K-KENSU     PIC  X(05).
      *    実日数
                   07  SKT0520-K-NISSU     PIC  X(05).
      *    点数
                   07  SKT0520-K-TENSU     PIC  X(09).
      *    一部負担金
                   07  SKT0520-K-ITBFTN    PIC  X(09).
      *    食事件数
               05  SKT0520-K-SKENSU        PIC  X(05).
      *    食事実日数
               05  SKT0520-K-SNISSU        PIC  X(05).
      *    金額
               05  SKT0520-K-SKINGK        PIC  X(09).
      *    標準負担額
               05  SKT0520-K-SFTNGK        PIC  X(09).
