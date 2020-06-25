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
      ********************************************************************
       01  SC23.
      *診療年月
           03  SC23-SRYYM                 PIC X(16).
      *合計ページ
           03  SC23-GPAGE                 PIC X(02).
      *ページ
           03  SC23-PAGE                  PIC X(02).
      *医療機関住所
           03  SC23-ADRS                  PIC X(100).
      *医療機関名称
           03  SC23-HOSPNAME              PIC X(100).
      *開設者名
           03  SC23-KAISETU               PIC X(40).
      *医療機関コード
           03  SC23-HOSPCDN               PIC X(20).
      *請求テーブル
           03  SC23-SEIKYU-TBL            OCCURS 12 TIMES.
      *法別番号
               05  SC23-HBTNUM            PIC X(03).
      *入外テーブル
               05  SC23-NYUGAI-TBL        OCCURS 2 TIMES.
      *件数
                   07  SC23-KENSU         PIC X(05).
      *点数
                   07  SC23-TENSU         PIC X(09).
      *金額
                   07  SC23-SEIKYUGK      PIC X(09).
