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
      *  01.00.01    吉川      2010/12/08   特別療養費国保対応
      *  01.00.02    萩野      2019/04/25   修正履歴コメント削除
      ********************************************************************
       01  SKT2911.
      *診療年月
           03  SKT2911-SRYYM               PIC  X(16).
      *保険者名
           03  SKT2911-HKNJANAME           PIC  X(40).
      *請求年月日
           03  SKT2911-SEIYMD              PIC  X(22).
      *保険者番号
           03  SKT2911-HKNJANUM            PIC  X(12).
      *医療機関コード
           03  SKT2911-HOSPCDN             PIC  X(20).
      *医療機関住所
           03  SKT2911-HOSPADRS            PIC  X(100).
      *医療機関名称
           03  SKT2911-HOSPNAME            PIC  X(100).
      *開設者名
           03  SKT2911-KAISETU             PIC  X(40).
      *点表区分
           03  SKT2911-TENHYOKBN           PIC  X(02).
      *在宅
           03  SKT2911-ZAITAKU1            PIC  X(02).
           03  SKT2911-ZAITAKU2            PIC  X(02).
      *特別療養費
           03  SKT2911-TOKUBETU            PIC  X(10).
      *ページ番号
           03  SKT2911-NO                  PIC  X(04).
      *療養の給付テーブル
           03  SKT2911-SEIKYU  OCCURS  2.
               05  SKT2911-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT2911-KENSU        PIC  X(05).
      *日数
                   07 SKT2911-NISSU        PIC  X(05).
      *点数
                   07 SKT2911-TENSU        PIC  X(09).
      *一部負担金
                   07 SKT2911-ITBFTN       PIC  X(09).
      *食事件数
               05  SKT2911-SKENSU          PIC  X(05).
      *食事日数
               05  SKT2911-SNISSU          PIC  X(05).
      *食事療養費
               05  SKT2911-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT2911-SFTNGK          PIC  X(09).
      *公費の給付テーブル
           03  SKT2911-KOHI    OCCURS  2.
      *公費名
               05  SKT2911-KOHNAME         PIC  X(04).
               05  SKT2911-KNYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT2911-K-KENSU       PIC  X(05).
      *日数
                   07 SKT2911-K-NISSU       PIC  X(05).
      *点数
                   07 SKT2911-K-TENSU       PIC  X(09).
      *一部負担金
                   07 SKT2911-K-ITBFTN      PIC  X(09).
