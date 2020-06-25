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
      *  01.00.01    吉川    2010/08/04  特別療養費国保対応
      *  01.00.02    吉川    2015/01/30  平成２７年１月改正対応
      ********************************************************************
       01  SKT1410.
      *    NO
           03  SKT1410-NO                PIC  X(04).
      *    診療年月
           03  SKT1410-SRYYM             PIC  X(16).
      *    保険者番号
           03  SKT1410-HKNJANUM          PIC  X(12).
      *    請求年月日
           03  SKT1410-SEIYMD            PIC  X(22).
      *    在宅
           03  SKT1410-ZAI               PIC  X(02).
           03  SKT1410-ZAIMARU           PIC  X(02).
      *    診療科
           03  SKT1410-SRYKA             PIC  X(04).
      *    医療機関コード
           03  SKT1410-HOSPCDN           PIC  X(20).
      *    医療機関住所
           03  SKT1410-ADRS              PIC  X(100).
      *    医療機関名
           03  SKT1410-HOSPNAME          PIC  X(100).
      *    開設者名
           03  SKT1410-KAISETUNAME       PIC  X(40).
      *    電話番号
           03  SKT1410-TEL               PIC  X(15).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) start
      *    特別療養費
           03  SKT1410-TOKUBETU          PIC  X(10).
      *=== 特別療養費国保対応 by Yoshikawa(2010/08/04) end
      *    請求テーブル
           03  SKT1410-SEIKYU-TBL     OCCURS  15.
      *    入外テーブル
               05  SKT1410-NYUGAI-TBL      OCCURS   2.
      *    件数
                   07  SKT1410-KENSU     PIC  X(05).
      *    実日数
                   07  SKT1410-NISSU     PIC  X(05).
      *    点数
                   07  SKT1410-TENSU     PIC  X(09).
      *    薬剤一部負担金
      *             07  SKT1410-YKZFTN    PIC  X(07).
      *    一部負担金
                   07  SKT1410-ITBFTN    PIC  X(09).
      *    食事療養：件数
               05  SKT1410-S-KENSU       PIC  X(05).
      *    食事療養：日数
               05  SKT1410-S-NISSU       PIC  X(05).
      *    食事療養：金額
               05  SKT1410-S-KINGAKU     PIC  X(09).
      *    食事療養：標準負担額
               05  SKT1410-S-FTNGAKU     PIC  X(09).
      *    公費テーブル
           03  SKT1410-KOH-TBL        OCCURS   34.
      *    件数
               05  SKT1410-K-KENSU       PIC  X(03).
      *    点数
      *         05  SKT1410-K-TENSU       PIC  X(07).
      *    薬剤一部負担金
      *         05  SKT1410-K-YKZFTN      PIC  X(07).
      *    標準負担額
      *         05  SKT1410-K-ITBFTN      PIC  X(07).
      *    合計テーブル
           03  SKT1410-SUM-TBL  OCCURS  2.
      *    合計：件数
               05  SKT1410-GKENSU        PIC  X(06).
      *    合計：金額
               05  SKT1410-GKINGAKU      PIC  X(07).
      *    その他テーブル
           03  SKT1410-ETC-TBL  OCCURS  1.
      *    法別名
               05  SKT1410-ETC-HBT       PIC  X(06).
      *    コード
               05  SKT1410-ETC-CODE      PIC  X(02).
      *    件数
               05  SKT1410-ETC-KENSU     PIC  X(03).
