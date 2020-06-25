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
      ************************************************************
      *    診療報酬請求書     COPY (CPSKT0700TMP.INC)              *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    吉川         11/04/04  東北地方太平洋沖地震対応
      *  01.00.02    萩野         18/10/10  修正履歴コメント削除
      ************************************************************
       01  SKT0700TMP-REC.
           03  SKT0700TMP-KEY.
      *医療機関コード
               05  SKT0700TMP-HOSPCD               PIC  X(20).
      *減免区分
               05  SKT0700TMP-GENMEN               PIC  X(01).
      *保険者数ー県内
           03  SKT0700TMP-KENNAI-HKNJSU            PIC  9(06).
      *保険者数ー県外
           03  SKT0700TMP-KENGAI-HKNJSU            PIC  9(06).
      *請求テーブル
           03  SKT0700TMP-SEIKYU-TBL               OCCURS  4.
      *入外テーブル
               05  SKT0700TMP-NYUGAI-TBL           OCCURS  2.
      *件数
                   07  SKT0700TMP-KENSU            PIC  9(11).
      *日数
                   07  SKT0700TMP-NISSU            PIC  9(11).
      *点数
                   07  SKT0700TMP-TENSU            PIC  9(14).
      *食事ー件数
               05  SKT0700TMP-SKENSU               PIC  9(06).
      *食事ー日数
               05  SKT0700TMP-SNISSU               PIC  9(06).
      *食事ー金額
               05  SKT0700TMP-SKINGK               PIC  9(11).
      *合計テーブル
               05  SKT0700TMP-GOKEI-TBL.
      *合計ー件数
                   07  SKT0700TMP-G-KENSU          PIC  9(11).
      *合計ー日数
                   07  SKT0700TMP-G-NISSU          PIC  9(11).
      *合計ー点数
                   07  SKT0700TMP-G-TENSU          PIC  9(14).
      *合計ー食事件数
                   07  SKT0700TMP-G-SKENSU         PIC  9(06).
      *合計ー食事日数
                   07  SKT0700TMP-G-SNISSU         PIC  9(06).
      *合計ー食事金額
                   07  SKT0700TMP-G-SKINGK         PIC  9(11).
      *県内保険者数
           03  SKT0700TMP-KENNAI.
               05  SKT0700TMP-KENNAI-MAX    PIC 9(04).
               05  SKT0700TMP-KENNAI-HKNJA  PIC X(06) OCCURS 100.
      *県外保険者数
           03  SKT0700TMP-KENGAI.
               05  SKT0700TMP-KENGAI-MAX    PIC 9(04).
               05  SKT0700TMP-KENGAI-HKNJA  PIC X(06) OCCURS 100.
