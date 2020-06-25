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
      *  01.00.01    吉川    2011/02/09  特別療養費国保対応
      *  01.00.02    萩野    2018/10/10  修正履歴コメント削除
      ********************************************************************
       01  SKT0700.
      *
           03  SKT0700-TBL-A.
      *請求年月
               05  SKT0700-SEIYM                   PIC  X(16).
      *医療機関住所
               05  SKT0700-ADRS                    PIC  X(100).
      *医療機関名称
               05  SKT0700-HOSPNAME                PIC  X(100).
      *開設者名
               05  SKT0700-KAISETUNAME             PIC  X(40).
      *医療機関コード
               05  SKT0700-HOSPCD                  PIC  X(28).
      *標榜診療科名
               05  SKT0700-SRYKANAME1              PIC  X(30).
               05  SKT0700-SRYKANAME2              PIC  X(30).
               05  SKT0700-SRYKANAME3              PIC  X(30).
      *銀行
               05  SKT0700-GINKO                   PIC  X(20).
      *店名
               05  SKT0700-TENMEI                  PIC  X(20).
      *県内保険者数
               05  SKT0700-KENNAI-HKNJSU           PIC  X(11).
      *県外保険者数
               05  SKT0700-KENGAI-HKNJSU           PIC  X(11).
      *請求テーブル
               05  SKT0700-SEIKYU-TBL              OCCURS  4.
      *入外テーブル
                   07  SKT0700-NYUGAI-TBL          OCCURS  2.
      *件数
                       09  SKT0700-KENSU           PIC  X(11).
      *日数
                       09  SKT0700-NISSU           PIC  X(11).
      *点数
                       09  SKT0700-TENSU           PIC  X(14).
      *食事ー件数
                   07  SKT0700-SKENSU              PIC  X(06).
      *食事ー日数
                   07  SKT0700-SNISSU              PIC  X(06).
      *食事ー金額
                   07  SKT0700-SKINGK              PIC  X(11).
      *合計テーブル
                   07  SKT0700-GOKEI-TBL.
      *合計ー件数
                       09  SKT0700-G-KENSU         PIC  X(11).
      *合計ー日数
                       09  SKT0700-G-NISSU         PIC  X(11).
      *合計ー点数
                       09  SKT0700-G-TENSU         PIC  X(14).
      *合計ー食事件数
                       09  SKT0700-G-SKENSU        PIC  X(06).
      *合計ー食事日数
                       09  SKT0700-G-SNISSU        PIC  X(06).
      *合計ー食事金額
                       09  SKT0700-G-SKINGK        PIC  X(11).
      *特別療養費
               05  SKT0700-TOKUBETU                PIC  X(10).
           03  SKT0700-TBL-B.
      *請求年月
               05  SKT0700-B-SEIYM                 PIC  X(16).
      *医療機関住所
               05  SKT0700-B-ADRS                  PIC  X(100).
      *医療機関名称
               05  SKT0700-B-HOSPNAME              PIC  X(100).
      *開設者名
               05  SKT0700-B-KAISETUNAME           PIC  X(40).
      *医療機関コード
               05  SKT0700-B-HOSPCD                PIC  X(28).
      *標榜診療科名
               05  SKT0700-B-SRYKANAME1            PIC  X(30).
               05  SKT0700-B-SRYKANAME2            PIC  X(30).
               05  SKT0700-B-SRYKANAME3            PIC  X(30).
      *銀行
               05  SKT0700-B-GINKO                 PIC  X(20).
      *店名
               05  SKT0700-B-TENMEI                PIC  X(20).
      *県内保険者数
               05  SKT0700-B-KENNAI-HKNJSU         PIC  X(11).
      *県外保険者数
               05  SKT0700-B-KENGAI-HKNJSU         PIC  X(11).
      *請求テーブル
               05  SKT0700-B-SEIKYU-TBL            OCCURS  4.
      *入外テーブル
                   07  SKT0700-B-NYUGAI-TBL        OCCURS  2.
      *件数
                       09  SKT0700-B-KENSU         PIC  X(11).
      *日数
                       09  SKT0700-B-NISSU         PIC  X(11).
      *点数
                       09  SKT0700-B-TENSU         PIC  X(14).
      *食事ー件数
                   07  SKT0700-B-SKENSU            PIC  X(06).
      *食事ー日数
                   07  SKT0700-B-SNISSU            PIC  X(06).
      *食事ー金額
                   07  SKT0700-B-SKINGK            PIC  X(11).
      *合計テーブル
                   07  SKT0700-B-GOKEI-TBL.
      *合計ー件数
                       09  SKT0700-B-G-KENSU       PIC  X(11).
      *合計ー日数
                       09  SKT0700-B-G-NISSU       PIC  X(11).
      *合計ー点数
                       09  SKT0700-B-G-TENSU       PIC  X(14).
      *合計ー食事件数
                       09  SKT0700-B-G-SKENSU      PIC  X(06).
      *合計ー食事日数
                       09  SKT0700-B-G-SNISSU      PIC  X(06).
      *合計ー食事金額
                       09  SKT0700-B-G-SKINGK      PIC  X(11).
      *特別療養費
               05  SKT0700-B-TOKUBETU              PIC  X(10).
