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
      *  01.00.01    吉川         11/02/10  特別療養費国保対応
      *  01.00.02    門間         18/09/05  不要コメントを削除
      ********************************************************************
       01  SKT4330.
      *ページ番号
           03  SKT4330-NO                  PIC  X(04).
      *診療年月
           03  SKT4330-SRYYM               PIC  X(16).
      *請求年月日
           03  SKT4330-SEIYMD              PIC  X(22).
      *医療機関所在地
           03  SKT4330-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT4330-HOSPNAME            PIC  X(100).
      *電話番号
           03  SKT4330-TEL                 PIC  X(15).
      *開設者氏名
           03  SKT4330-KAISETU             PIC  X(40).
      *医療機関コード
           03  SKT4330-HOSPCDN             PIC  X(20).
      *保険者番号
           03  SKT4330-HKNJANUM            PIC  X(12).
      *保険者名
           03  SKT4330-HKNJANAME           PIC  X(24).
      *一般・在宅メッセージ
           03  SKT4330-ZAI                 PIC  X(32).
      *特別療養費
           03  SKT4330-TOKUBETU            PIC  X(10).
      *療養の給付テーブル
           03  SKT4330-SEIKYU  OCCURS  2.
               05  SKT4330-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4330-KENSU        PIC  X(05).
      *日数
                   07 SKT4330-NISSU        PIC  X(05).
      *点数
                   07 SKT4330-TENSU        PIC  X(09).
      *一部負担金
                   07 SKT4330-ITBFTN       PIC  X(09).
      *食事件数
               05  SKT4330-SKENSU          PIC  X(05).
      *食事日数
               05  SKT4330-SNISSU          PIC  X(05).
      *食事療養費
               05  SKT4330-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT4330-SFTNGK          PIC  X(09).
      *公費テーブル
           03  SKT4330-KOHSEIKYU  OCCURS  4.
      *法別番号
               05  SKT4330-HBTNUM          PIC  X(02).
      *公費名
      *         05  SKT4330-KOHNAME         PIC  X(10).
      *
               05  SKT4330-K-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT4330-K-KENSU      PIC  X(05).
      *日数
                   07 SKT4330-K-NISSU      PIC  X(05).
      *点数
                   07 SKT4330-K-TENSU      PIC  X(09).
      *一部負担金
                   07 SKT4330-K-ITBFTN     PIC  X(09).
      *食事件数
               05  SKT4330-K-SKENSU        PIC  X(05).
      *食事日数
               05  SKT4330-K-SNISSU        PIC  X(05).
      *食事療養費
               05  SKT4330-K-SKINGK        PIC  X(09).
      *標準負担額
               05  SKT4330-K-SFTNGK        PIC  X(09).
