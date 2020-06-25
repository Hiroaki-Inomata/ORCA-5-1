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
       01  SKT3710.
      *診療年月
           03  SKT3710-SRYY                PIC  X(04).
           03  SKT3710-SRYM                PIC  X(04).
      *医療機関コード
           03  SKT3710-HOSPCDN             PIC  X(20).
      *診療科
           03  SKT3710-RECEKA              PIC  X(20).
      *特別療養費
           03  SKT3710-TOKUBETU            PIC  X(10).
      *既存公費請求テーブル
           03  SKT3710-KZN-TBL OCCURS 2.
      *入院・外来テーブル
               05  SKT3710-NYUGAIKBN1   OCCURS  2.
      *療養の給付：件数
                   07  SKT3710-KZN-KENSU       PIC  X(05).
      *療養の給付：点数
                   07  SKT3710-KZN-TENSU       PIC  X(09).
      *療養の給付：一部負担金
                   07  SKT3710-KZN-ITBFTN      PIC  X(09).
      *請求一覧
           03  SKT3710-SEIKYU  OCCURS 9.
      *公費法別番号
               05  SKT3710-HBTNUM              PIC  X(04).
      *公費公費名
               05  SKT3710-KOHNAME             PIC  X(10).
      *入院・外来テーブル
               05  SKT3710-NYUGAIKBN2   OCCURS  2.
      *療養の給付：件数
                   07  SKT3710-KENSU       PIC  X(05).
      *療養の給付：点数
                   07  SKT3710-TENSU       PIC  X(09).
      *療養の給付：一部負担金
                   07  SKT3710-ITBFTN      PIC  X(09).
