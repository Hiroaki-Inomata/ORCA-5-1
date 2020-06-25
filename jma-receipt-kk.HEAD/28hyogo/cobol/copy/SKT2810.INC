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
      *  01.00.01    吉川      2010/11/02   特別療養費国保対応
      *  01.00.02    萩野      2019/03/08   修正履歴コメント削除
      ********************************************************************
       01  SKT2810.
      *印刷年月
         03      SKT2810-SRYYM        PIC X(16).
      *医療機関住所
         03      SKT2810-ADRS         PIC X(100).
      *医療機関名称
         03      SKT2810-HOSPNAME     PIC X(100).
      *開設者名
         03      SKT2810-KAISETUNAME  PIC X(40).
      *保険者名
         03      SKT2810-HKNJANAME    PIC X(26).
      *保険者番号
         03      SKT2810-HKNJANUM     PIC X(12).
      *医療機関コード
         03      SKT2810-HOSPCD       PIC X(14).
      *請求年月日
         03      SKT2810-SEIYMD       PIC X(22).
      *在宅
         03      SKT2810-ZAI1         PIC X(02).
         03      SKT2810-ZAI2         PIC X(02).
      *ページ番号
         03      SKT2810-NO           PIC X(04).
      *特別療養費
         03      SKT2810-TOKUBETU     PIC  X(10).
      *公費請求テーブル
         03      SKT2810-KOH-TBL      OCCURS 2.
      *公費件数
           05    SKT2810-KOHKENSU     PIC X(05).
      *一部負担金
           05    SKT2810-KOHITBFTN    PIC X(09).
      *請求テーブル
         03      SKT2810-SEIKYU-TBL   OCCURS 12.
      *入外テーブル
           05    SKT2810-NYUGAI-TBL   OCCURS 2.
      *件数
             07  SKT2810-KENSU        PIC X(05).
      *日数
             07  SKT2810-NISSU        PIC X(05).
      *点数
             07  SKT2810-TENSU        PIC X(09).
      *一部負担金
             07  SKT2810-ITBFTN       PIC X(09).
      *食事件数
           05    SKT2810-SKENSU       PIC X(05).
      *食事日数
           05    SKT2810-SNISSU       PIC X(05).
      *金額
           05    SKT2810-SKINGK       PIC X(09).
      *標準負担金
           05    SKT2810-SFTNGK       PIC X(09).
      *マル原
         03      SKT2810-TOKUBETU1    PIC X(02) OCCURS 2.
      *マル特
         03      SKT2810-TOKUBETU2    PIC X(02) OCCURS 2.
