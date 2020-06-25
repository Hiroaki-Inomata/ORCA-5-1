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
       01  SKT4610.
      *ページ番号
           03  SKT4610-NO                  PIC  X(04).
      *点表区分
           03  SKT4610-TENHYOKBN           PIC  X(02)  OCCURS 2.
      *診療年月
           03  SKT4610-SRYYM               PIC  X(16).
      *保険者名
           03  SKT4610-HKNJANAME           PIC  X(30).
      *請求年月日
           03  SKT4610-SEIYMD              PIC  X(22).
      *保険者番号
           03  SKT4610-HKNJANUM            PIC  X(12).
      *医療機関コード
           03  SKT4610-HOSPCD              PIC  X(14).
      *医療機関住所
           03  SKT4610-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT4610-HOSPNAME            PIC  X(100).
      *電話番号
           03  SKT4610-TEL                 PIC  X(15).
      *開設者名
           03  SKT4610-KAISETU             PIC  X(40).
      *在宅
           03  SKT4610-ZAI1                PIC  X(02).
           03  SKT4610-ZAI2                PIC  X(02).
      *特別療養費
           03  SKT4610-TOKUBETU            PIC  X(10).
      *療養の給付テーブル
           03  SKT4610-SEIKYU  OCCURS  9.
               05  SKT4610-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4610-KENSU        PIC  X(05).
      *日数
                   07 SKT4610-NISSU        PIC  X(05).
      *点数
                   07 SKT4610-TENSU        PIC  X(09).
      *一部負担金
                   07 SKT4610-ITBFTN       PIC  X(09).
      *食事件数
               05  SKT4610-SKENSU          PIC  X(05).
      *食事日数
               05  SKT4610-SNISSU          PIC  X(05).
      *食事療養費
               05  SKT4610-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT4610-SFTNGK          PIC  X(09).
