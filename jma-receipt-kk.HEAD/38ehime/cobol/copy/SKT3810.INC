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
      *  01.00.03    吉川      12/04/11    帳票様式に伴う修正
      *  01.00.02    門間      18/09/05    不要コメントを削除
      *****************************************************************
       01  SKT3810.
      *診療年月
           03  SKT3810-SRYYM               PIC  X(16).
      *医療機関コード
           03  SKT3810-HOSPCD              PIC  X(14).
      *医療機関住所
           03  SKT3810-HOSPADRS            PIC  X(100).
      *医療機関名称
           03  SKT3810-HOSPNAME            PIC  X(100).
      *特別療養費
           03  SKT3810-TOKUBETU            PIC  X(10).
      *老人療養の給付テーブル
           03  SKT3810-SEIKYU  OCCURS  2.
               05  SKT3810-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT3810-KENSU        PIC  X(06).
      *点数
                   07 SKT3810-TENSU        PIC  X(11).
      *一部負担金
                   07 SKT3810-ITBFTN       PIC  X(10).
      *公費療養の給付テーブル
           03  SKT3810-K-SEIKYU  OCCURS  2.
               05  SKT3810-K-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT3810-KKENSU        PIC  X(06).
      *点数
                   07 SKT3810-KTENSU        PIC  X(11).
      *一部負担金
                   07 SKT3810-KITBFTN       PIC  X(10).
      *老人食事療養テーブル
           03  SKT3810-SHOKUJI-TBL OCCURS 2.
      *食事件数
               05  SKT3810-S-KENSU         PIC  X(06).
      *食事療養費
               05  SKT3810-S-MONEY         PIC  X(11).
      *標準負担額
               05  SKT3810-S-FTNGAKU       PIC  X(10).
      *公費食事療養テーブル
           03  SKT3810-K-SHOKUJI-TBL OCCURS 2.
      *食事件数
               05  SKT3810-S-KKENSU         PIC  X(06).
      *食事療養費
               05  SKT3810-S-KMONEY         PIC  X(11).
      *標準負担額
               05  SKT3810-S-KFTNGAKU       PIC  X(10).
      *県内保険者
           03  SKT3810-KENNAI               PIC  X(02) OCCURS 22.
      *旧県内保険者
           03  SKT3810-KENNAI-OLD           PIC  X(120).
      *県外保険者名
           03  SKT3810-KENGAI               PIC  X(350).
      *県内保険者数
           03  SKT3810-KENNAISU             PIC  X(02).
      *県外保険者数
           03  SKT3810-KENGAISU             PIC  X(02).
      *平均点数
           03  SKT3810-AVG-HKN OCCURS 2.
               05  SKT3810-AVG-NYUGAI OCCURS 2.
                   07  SKT3810-AVG          PIC  X(07).
