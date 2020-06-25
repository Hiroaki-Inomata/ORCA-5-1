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
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者    日付        内容
      *  01.00.01    長屋      08/04/29    帳票様式に伴う修正
      *  01.00.02    吉川      09/03/06    特別療養費国保対応
      *  01.00.03    門間      18/09/05    不要コメントを削除
      *****************************************************************
       01  SKT3800.
      *診療年月
           03  SKT3800-SRYYM               PIC  X(16).
      *医療機関コード
           03  SKT3800-HOSPCD              PIC  X(14).
      *特別療養費
           03  SKT3800-TOKUBETU            PIC  X(10).
      *療養の給付テーブル
           03  SKT3800-SEIKYU  OCCURS  7.
               05  SKT3800-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT3800-KENSU        PIC  X(06).
      *点数
                   07 SKT3800-TENSU        PIC  X(10).
      *一部負担金
                   07 SKT3800-ITBFTN       PIC  X(10).
      *食事療養テーブル
           03  SKT3800-SHOKUJI-TBL OCCURS 7.
      *食事件数
               05  SKT3800-S-KENSU         PIC  X(06).
      *食事療養費
               05  SKT3800-S-MONEY         PIC  X(10).
      *標準負担額
               05  SKT3800-S-FTNGAKU       PIC  X(10).
