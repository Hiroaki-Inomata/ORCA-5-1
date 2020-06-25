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
      ********************************************************************
      *    後期高齢総括表（福島県）
      *
      *  作成日付    作業者        記述
      *  08/04/26    大竹          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ********************************************************************
        01  SKT0705.
            03  SKT0705-TBL     OCCURS  2 TIMES.
      *請求年月
              05  SKT0705-SRYYM   PIC X(16).
      *医療機関住所
              05  SKT0705-ADRS    PIC X(100).
      *医療機関名称
              05  SKT0705-HOSPNAME    PIC X(100).
      *開設者名
              05  SKT0705-KAISETUNAME PIC X(40).
      *医療機関コード
              05  SKT0705-HOSPCD  PIC X(28).
      *標榜診療科名
              05  SKT0705-SRYKANAME1  PIC X(30).
              05  SKT0705-SRYKANAME2  PIC X(30).
              05  SKT0705-SRYKANAME3  PIC X(30).
      *銀行
              05  SKT0705-GINKO   PIC X(20).
      *店名
              05  SKT0705-TENMEI  PIC X(20).
      *県内保険者数
              05  SKT0705-KENNAI-HKNJSU   PIC X(11).
      *県外保険者数
              05  SKT0705-KENGAI-HKNJSU   PIC X(11).
      *請求テーブル
              05  SKT0705-SEIKYU-TBL      OCCURS  2 TIMES.
      *入外テーブル
                07  SKT0705-NYUGAI-TBL      OCCURS  2 TIMES.
      *件数
                  09  SKT0705-KENSU   PIC X(11).
      *日数
                  09  SKT0705-NISSU   PIC X(11).
      *点数
                  09  SKT0705-TENSU   PIC X(14).
      *食事ー件数
                07  SKT0705-S-KENSU   PIC X(5).
      *食事ー日数
                07  SKT0705-S-NISSU   PIC X(5).
      *食事ー金額
                07  SKT0705-S-KINGK   PIC X(11).
      *合計テーブル
                07  SKT0705-GOKEI-TBL.
      *合計ー件数
                  09  SKT0705-G-KENSU PIC X(11).
      *合計ー日数
                  09  SKT0705-G-NISSU PIC X(11).
      *合計ー点数
                  09  SKT0705-G-TENSU PIC X(14).
      *合計ー食事件数
                  09  SKT0705-G-SKENSU    PIC X(6).
      *合計ー食事日数
                  09  SKT0705-G-SNISSU    PIC X(6).
      *合計ー食事金額
                  09  SKT0705-G-SKINGK    PIC X(11).
