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
       01  SC07.
      *医療機関コード
           03  SC07-HOSPCD            PIC  X(14).
      *印刷日付
           03  SC07-PRTYY             PIC  X(02).
           03  SC07-PRTMM             PIC  X(02).
      *医療機関住所
           03  SC07-ADRS              PIC  X(100).
      *医療機関名称
           03  SC07-HOSPNAME          PIC  X(100).
      *医療機関電話番号
           03  SC07-TEL               PIC  X(15).
      *医療機関開設者氏名
           03  SC07-KAISETU           PIC  X(40).
      *点数票区分
           03  SC07-TENSUHYOG         OCCURS 2.
               05  SC07-TENSUHYO      PIC  X(02).
      *入外テーブル
           03  SC07-SEIKYU-TBL.
               05  SC07-NYUGAI-TBL OCCURS 2.
      *総件数
                   07  SC07-KENSU     PIC  X(07).
      *総点数
                   07  SC07-TENSU     PIC  X(11).
      *負担金
                   07  SC07-FTNMONEY  PIC  X(10).
      *食事療養費-件数
               05  SC07-S-KENSU        PIC  X(07).
      *食事療養費-金額
               05  SC07-S-KINGK        PIC  X(11).
      *食事療養費-標準負担
               05  SC07-S-FTNGK        PIC  X(10).
      *合計テーブル
           03  SC07-GOKEI-TBL.
      *合計総件数
               05  SC07-GKENSU         PIC  X(07).
      *合計総点数
               05  SC07-GTENSU         PIC  X(11).
      *合計負担金
               05  SC07-GFTNMONEY      PIC  X(10).
      *食事療養合計件数
               05  SC07-S-GKENSU       PIC  X(07).
      *食事療養合計金額
               05  SC07-S-GKINGK       PIC  X(11).
      *食事療養合計標準負担金
               05  SC07-S-GFTNGK       PIC  X(10).
      *区分
           03  SC07-KBN-TBL OCCURS 4.
               05  SC07-KBN            PIC  X(02).

