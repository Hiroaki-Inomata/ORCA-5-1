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
       01  SC13.
      *診療年月
           03  SC13-SRYYM                   PIC  X(16).
      *診療区分
           03  SC13-SRYKBN1                 PIC  X(02).
           03  SC13-SRYKBN2                 PIC  X(02).
      *総ページ数
           03  SC13-GPAGE                   PIC  X(02).
      *ページ数
           03  SC13-PAGE                    PIC  X(02).
      *請求年月日
           03  SC13-SEIYMD                  PIC  X(22).
      *医療機関コード
           03  SC13-HOSPCD                  PIC  X(20).
      *医療機関所在地
           03  SC13-ADRS                    PIC  X(100).
      *医療機関名称
           03  SC13-HOSPNAME                PIC  X(100).
      *開設者名
           03  SC13-KAISETUNAME             PIC  X(40).
      *電話番号
           03  SC13-TEL                     PIC  X(15).
      *請求テーブル
           03  SC13-TBL   OCCURS  12.
      *制度区分
               05  SC13-SEIDOKBN-TBL  OCCURS  5.
                   07  SC13-SEIDOKBN        PIC  X(02).
      *保険者番号
               05  SC13-HKNJANUM            PIC  X(12).
      *本人家族区分
               05  SC13-HONKZKKBN           PIC  X(02).
      *入院・外来
               05  SC13-NYUGAIKBN-TBL  OCCURS  2.
      *件数
                   07  SC13-KENSU           PIC  X(05).
      *点数
                   07  SC13-TENSU           PIC  X(09).
      *公費分点数
                   07  SC13-KOHTENSU        PIC  X(09).
      *薬剤一部負担金
                   07  SC13-YKZFTN          PIC  X(07).
      *老人一部負担金又は健康診査請求額
                   07  SC13-ITBFTN          PIC  X(09).
      *食事療養費：件数
               05  SC13-SKENSU           PIC  X(05).
      *食事療養費：金額
               05  SC13-SKINGK           PIC  X(09).
      *食事療養費：標準負担額
               05  SC13-SFTNGK           PIC  X(09).
      *入院合計件数
           03  SC13-N-GKENSU                PIC  X(05).
      *外来合計件数
           03  SC13-G-GKENSU                PIC  X(05).
      *合計
           03  SC13-GOKEI-TBL    OCCURS  2.
      *入院・外来テーブル
               05  SC13-G-NYUGAI-TBL  OCCURS  2.
      *件数
                   07  SC13-GKENSU          PIC  X(05).
      *金額
                   07  SC13-GTENSU          PIC  X(09).
