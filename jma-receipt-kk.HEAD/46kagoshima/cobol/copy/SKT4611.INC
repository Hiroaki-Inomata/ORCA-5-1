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
      *  01.00.01    吉川    11/03/04    特別療養費国保対応
      *  01.00.02    門間    19/02/04    修正履歴コメント削除
      ********************************************************************
       01  SKT4611.
      *ページ番号
           03  SKT4611-NO                  PIC  X(04).
      *保険者番号
           03  SKT4611-HKNJANUM            PIC  X(12).
      *医療機関コード
           03  SKT4611-HOSPCD              PIC  X(14).
      *在宅
           03  SKT4611-ZAI1                PIC  X(02).
           03  SKT4611-ZAI2                PIC  X(02).
      *特別療養費
           03  SKT4611-TOKUBETU            PIC  X(10).
      *療養の給付テーブル
           03  SKT4611-ROJIN   OCCURS  2.
               05  SKT4611-R-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4611-KENSU      PIC  X(05).
      *日数
                   07 SKT4611-NISSU      PIC  X(05).
      *点数
                   07 SKT4611-TENSU      PIC  X(09).
      *一部負担金
                   07 SKT4611-ITBFTN     PIC  X(09).
      *食事件数
               05  SKT4611-SKENSU        PIC  X(05).
      *食事日数
               05  SKT4611-SNISSU        PIC  X(05).
      *食事療養費
               05  SKT4611-SKINGK        PIC  X(09).
      *標準負担額
               05  SKT4611-SFTNGK        PIC  X(09).
      *療養の給付テーブル
           03  SKT4611-KOHI    OCCURS  7.
      *公費名
               05  SKT4611-HBTNUM          PIC  X(02).
               05  SKT4611-KOHNAME         PIC  X(10).
      *
               05  SKT4611-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4611-K-KENSU        PIC  X(05).
      *日数
                   07 SKT4611-K-NISSU        PIC  X(05).
      *点数
                   07 SKT4611-K-TENSU        PIC  X(09).
      *一部負担金
                   07 SKT4611-K-ITBFTN       PIC  X(09).
      *食事件数
               05  SKT4611-K-SKENSU          PIC  X(05).
      *食事日数
               05  SKT4611-K-SNISSU          PIC  X(05).
      *食事療養費
               05  SKT4611-K-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT4611-K-SFTNGK          PIC  X(09).
      *老人併用マル原
           03  SKT4611-MARUGEN.
      *マル原文字
               05  SKT4611-MARUGEN1        PIC  X(06).
      *マル原件数
               05  SKT4611-MARUGEN2-1      PIC  X(04).
               05  SKT4611-MARUGEN2-2      PIC  X(02).
