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
      *    診療報酬請求書     COPY (CPSKT2910TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01     山口      05/04/13  全国公費と地方公費を合算して集計する
      *                                   ため公費テーブルのOCCURSを３に変更
      *  01.00.02     吉川      10/05/21  長期を別帳票にするためKEY追加
      *  01.00.03     萩野      19/04/25  修正履歴コメント削除
      ************************************************************
       01  SKT2910TMP-REC.
           03      SKT2910TMP-KEY.
             05    SKT2910TMP-SYUTURYOKU          PIC  X(01).
             05    SKT2910TMP-HKNJANUM            PIC  X(06).
             05    SKT2910TMP-ZAITAKU             PIC  X(01).
             05    SKT2910TMP-TYOKI               PIC  X(01).
             05    SKT2910TMP-KYURATE             PIC  9(02).
           03      SKT2910TMP-PAGE-FLG1           PIC  9(01).
           03      SKT2910TMP-PAGE-FLG2           PIC  9(01).
           03      SKT2910TMP-SEIKYU-TBL          OCCURS  11.
             05    SKT2910TMP-NYUGAI-TBL          OCCURS  2.
               07  SKT2910TMP-KENSU               PIC  9(06).
               07  SKT2910TMP-NISSU               PIC  9(06).
               07  SKT2910TMP-TENSU               PIC  9(10).
               07  SKT2910TMP-ITBFTN              PIC  9(10).
             05    SKT2910TMP-SKENSU              PIC  9(06).
             05    SKT2910TMP-SNISSU              PIC  9(06).
             05    SKT2910TMP-SKINGK              PIC  9(10).
             05    SKT2910TMP-SFTNGK              PIC  9(10).
           03      SKT2910TMP-KOH-TBL             OCCURS 3.
             05    SKT2910TMP-KOH-FLG             PIC  9(01).
             05    SKT2910TMP-K-NYUGAI-TBL        OCCURS 2.
               07  SKT2910TMP-K-KENSU             PIC  9(06).
               07  SKT2910TMP-K-NISSU             PIC  9(06).
               07  SKT2910TMP-K-TENSU             PIC  9(10).
               07  SKT2910TMP-K-ITBFTN            PIC  9(10).
