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
       01  SKT3700.
      *診療年月
           03  SKT3700-SRYY                PIC  X(04).
           03  SKT3700-SRYM                PIC  X(04).
      *郵便番号
      *医療機関所在地
           03  SKT3700-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT3700-HOSPNAME            PIC  X(100).
      *医療機関電話番号
           03  SKT3700-TEL                 PIC  X(15).
      *開設者氏名
           03  SKT3700-KAISETU             PIC  X(40).
      *医療機関コード
           03  SKT3700-HOSPCDN             PIC  X(20).
      *診療科
           03  SKT3700-RECEKA              PIC  X(20).
      *特別療養費
           03  SKT3700-TOKUBETU            PIC  X(10).
      *請求一覧
           03  SKT3700-SEIKYU  OCCURS 11.
      *入院・外来テーブル
               05  SKT3700-NYUGAIKBN   OCCURS  2.
      *療養の給付：件数
                   07  SKT3700-KENSU       PIC  X(05).
      *療養の給付：点数
                   07  SKT3700-TENSU       PIC  X(09).
      *療養の給付：一部負担金
                   07  SKT3700-ITBFTN      PIC  X(09).
