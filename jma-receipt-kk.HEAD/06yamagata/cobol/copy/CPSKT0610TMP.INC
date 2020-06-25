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
      *    診療報酬請求書     COPY (CPSKT0610TMP.INC)            *
      *
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者   日付       内容
      *  01.00.01    吉川     11/03/24   東北地方太平洋沖地震対応（対象者別集計）
      *  01.00.02    吉川     12/05/10   帳票様式の変更に伴う修正
      *  01.00.03    井上     18/10/12   修正履歴コメント削除
      ************************************************************
       01  SKT0610TMP-REC.
           03      SKT0610TMP-KEY.
             05    SKT0610TMP-SYUTURYOKU            PIC  9(01).
             05    SKT0610TMP-HKNJANUM              PIC  X(06).
             05    SKT0610TMP-NYUGAIKBN             PIC  X(01).
             05    SKT0610TMP-GENMEN                PIC  X(05).
           03      SKT0610TMP-DATA.
             05    SKT0610TMP-SEIKYU-TBL            OCCURS    13.
               07  SKT0610TMP-KENSU                 PIC  9(06).
               07  SKT0610TMP-NISSU                 PIC  9(06).
               07  SKT0610TMP-TENSU                 PIC  9(10).
               07  SKT0610TMP-ITBFTN                PIC  9(10).
               07  SKT0610TMP-SKENSU                PIC  9(06).
               07  SKT0610TMP-SNISSU                PIC  9(06).
               07  SKT0610TMP-SKINGK                PIC  9(10).
               07  SKT0610TMP-SFTNGK                PIC  9(10).
             05    SKT0610TMP-KOH-TBL  OCCURS 20.
               07  SKT0610TMP-KOHNUM                PIC  X(03).
               07  SKT0610TMP-F-KENSU               PIC  9(06).
               07  SKT0610TMP-F-NISSU               PIC  9(06).
               07  SKT0610TMP-F-TENSU               PIC  9(10).
               07  SKT0610TMP-F-ITBFTN              PIC  9(10).
               07  SKT0610TMP-F-SKENSU              PIC  9(06).
               07  SKT0610TMP-F-SNISSU              PIC  9(06).
               07  SKT0610TMP-F-SKINGK              PIC  9(10).
               07  SKT0610TMP-F-SFTNGK              PIC  9(10).
