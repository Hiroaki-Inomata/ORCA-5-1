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
      *  01.00.01    吉川    11/03/04    特別療養費国保対応
      *  01.00.02    吉川    12/01/16    点数欄のバイト数増加
      *  01.00.02    門間    19/02/04    修正履歴コメント削除
      ********************************************************************
       01  SKT4601.
      *
      *医療機関コード
           03  SKT4601-HOSPCD              PIC  X(07).
      *特別療養費
           03  SKT4601-TOKUBETU            PIC  X(10).
      *公費の給付テーブル
           03  SKT4601-KOHI        OCCURS  10.
      *法別番号
               05  SKT4601-HBTNUM          PIC  X(02).
      *公費名称
               05  SKT4601-KOHNAME         PIC  X(10).
      *入院・外来テーブル
               05  SKT4601-NYUGAI-TBL      OCCURS    2.
      *件数
                   07 SKT4601-KENSU        PIC  X(05).
      *薬剤一部負担金
      *             07 SKT4601-YKZFTN       PIC  X(09).
      *一部負担金
                   07 SKT4601-ITBFTN       PIC  X(09).
           03  SKT4601-SHOKUJI     OCCURS  10.
      *食事件数
               05  SKT4601-SKENSU          PIC  X(05).
      *食事療養費
               05  SKT4601-SKINGK          PIC  X(09).
      *標準負担額
               05  SKT4601-SFTNGK          PIC  X(09).
      *
      *在宅の給付テーブル
           03  SKT4601-ZAITAKU             OCCURS  7.
      *件数
               05  SKT4601-Z-KENSU         PIC  X(05).
      *点数
               05  SKT4601-Z-TENSU         PIC  X(10).
      *一部負担金
               05  SKT4601-Z-ITBFTN        PIC  X(09).
      *公費の給付テーブル
           03  SKT4601-ZAITAKU-KOHI        OCCURS  3.
      *法別番号
               05  SKT4601-Z-HBTNUM        PIC  X(02).
      *公費名称
               05  SKT4601-Z-KOHNAME       PIC  X(10).
      *件数
               05  SKT4601-ZK-KENSU        PIC  X(05).
      *一部負担金
               05  SKT4601-ZK-ITBFTN       PIC  X(09).
