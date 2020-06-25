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
      *  01.00.01    吉川         10/09/24  主科対応
      *  01.00.02    吉川         11/01/11  特別療養費国保対応
      *  01.00.03    萩野         19/01/11  修正履歴コメント削除
      ************************************************************
       01  SKT3720.
      *診療年月
           03  SKT3720-SRYY                PIC  X(04).
           03  SKT3720-SRYM                PIC  X(04).
      *医療機関所在地
           03  SKT3720-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT3720-HOSPNAME            PIC  X(100).
      *電話番号
           03  SKT3720-TEL                 PIC  X(15).
      *開設者氏名
           03  SKT3720-KAISETU             PIC  X(40).
      *医療機関コード
           03  SKT3720-HOSPCDN             PIC  X(20).
      *診療科
           03  SKT3720-RECEKA              PIC  X(20).
      *特別療養費
           03  SKT3720-TOKUBETU            PIC  X(10).
      *請求一覧
           03  SKT3720-SEIKYU  OCCURS 11.
      *食事療養費：件数
               05  SKT3720-SKENSU       PIC  X(05).
      *食事療養費：金額
               05  SKT3720-SKINGK       PIC  X(09).
      *食事療養費：標準負担額
               05  SKT3720-SFTNGK       PIC  X(09).
      *結核エリア
           03  SKT3720-K35-SEIKYU.
      *食事療養費：件数
               05  SKT3720-K35-SKENSU       PIC  X(05).
      *食事療養費：金額
               05  SKT3720-K35-SKINGK       PIC  X(09).
      *食事療養費：標準負担額
               05  SKT3720-K35-SFTNGK       PIC  X(09).
      *公費エリア
           03  SKT3720-KOHI-SEIKYU OCCURS 6.
      *法別番号
               05  SKT3720-K-HBTNUM       PIC  X(04).
      *公費名
               05  SKT3720-K-KOHNAME      PIC  X(10).
      *食事療養費：件数
               05  SKT3720-K-SKENSU       PIC  X(05).
      *食事療養費：金額
               05  SKT3720-K-SKINGK       PIC  X(09).
      *食事療養費：標準負担額
               05  SKT3720-K-SFTNGK       PIC  X(09).
