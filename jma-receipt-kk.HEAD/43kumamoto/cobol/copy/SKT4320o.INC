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
       01  SKT4320o.
      *ページ番号
           03  SKT4320o-NO                  PIC  X(04).
      *診療年月
           03  SKT4320o-SRYYM               PIC  X(16).
      *請求年月日
           03  SKT4320o-SEIYMD              PIC  X(22).
      *医療機関所在地
           03  SKT4320o-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT4320o-HOSPNAME            PIC  X(100).
      *電話番号
           03  SKT4320o-TEL                 PIC  X(15).
      *開設者氏名
           03  SKT4320o-KAISETU             PIC  X(40).
      *医療機関コード
           03  SKT4320o-HOSPCDN             PIC  X(20).
      *保険者番号
           03  SKT4320o-HKNJANUM            PIC  X(12).
      *保険者名
           03  SKT4320o-HKNJANAME           PIC  X(24).
      *一般・在宅メッセージ
           03  SKT4320o-ZAI                 PIC  X(32).
      *療養の給付テーブル
           03  SKT4320o-SEIKYU  OCCURS  9.
               05  SKT4320o-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4320o-KENSU        PIC  X(05).
      *日数
                   07 SKT4320o-NISSU        PIC  X(05).
      *点数
                   07 SKT4320o-TENSU        PIC  X(09).
      *一部負担金
                   07 SKT4320o-ITBFTN       PIC  X(09).
      *食事件数
               05  SKT4320o-SKENSU          PIC  X(05).
      *食事日数
               05  SKT4320o-SNISSU          PIC  X(05).
      *食事療養費
               05  SKT4320o-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT4320o-SFTNGK          PIC  X(09).
