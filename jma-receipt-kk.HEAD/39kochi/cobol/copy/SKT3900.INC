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
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2011/01/12  特別療養費国保対応
      *  01.00.02    門間    2019/01/07  修正履歴コメント削除
      ********************************************************************
       01  SKT3900.
      *診療年月
           03  SKT3900-SRYYM                   PIC  X(16).
      *診療区分
           03  SKT3900-SRYKBN1                 PIC  X(02).
           03  SKT3900-SRYKBN2                 PIC  X(02).
      *総ページ数
           03  SKT3900-GPAGE                   PIC  X(02).
      *ページ数
           03  SKT3900-PAGE                    PIC  X(02).
      *請求年月日
           03  SKT3900-SEIYMD                  PIC  X(22).
      *医療機関コード
           03  SKT3900-HOSPCD                  PIC  X(20).
      *医療機関所在地
           03  SKT3900-ADRS                    PIC  X(100).
      *医療機関名称
           03  SKT3900-HOSPNAME                PIC  X(100).
      *開設者名
           03  SKT3900-KAISETUNAME             PIC  X(40).
      *電話番号
           03  SKT3900-TEL                     PIC  X(15).
      *特別療養費
           03  SKT3900-TOKUBETU                PIC X(10).
      *請求テーブル
           03  SKT3900-TBL   OCCURS  12.
               05  SKT3900-SEIDOKBN-TBL  OCCURS  4.
                   07  SKT3900-SEIDOKBN        PIC  X(02).
      *保険者番号
               05  SKT3900-HKNJANUM            PIC  X(12).
      *本人家族区分
      *         05  SKT3900-HONKZKKBN           PIC  X(02).
      *入院・外来
               05  SKT3900-NYUGAIKBN-TBL  OCCURS  2.
      *件数
                   07  SKT3900-KENSU           PIC  X(05).
      *点数
                   07  SKT3900-TENSU           PIC  X(09).
      *老人一部負担金又は健康診査請求額
                   07  SKT3900-RJNFTN          PIC  X(09).
      *原爆テーブル
                   07  SKT3900-GENBK-TBL.
                       09  SKT3900-GENMARU     PIC  X(02).
                       09  SKT3900-GENBK1      PIC  X(12).
                       09  SKT3900-GENBK2      PIC  X(03).
      *食事療養費：件数
               05  SKT3900-SKJ-KENSU           PIC  X(05).
      *食事療養費：金額
               05  SKT3900-SKJ-KINGAKU         PIC  X(09).
      *食事療養費：標準負担額
               05  SKT3900-SKJ-FTNGAKU         PIC  X(09).
      *入院合計件数
      *     03  SKT3900-N-GKENSU                PIC  X(05).
      *外来合計件数
      *     03  SKT3900-G-GKENSU                PIC  X(05).
      *合計
           03  SKT3900-GOKEI-TBL    OCCURS  2.
      *入院・外来テーブル
               05  SKT3900-G-NYUGAI-TBL  OCCURS  2.
      *件数
                   07  SKT3900-GKENSU          PIC  X(05).
      *金額
                   07  SKT3900-GTENSU          PIC  X(09).
