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
      *  01.00.02    吉川      11/01/12    特別療養費国保対応
      *  01.00.03    吉川      11/08/19    開設者名のバイト数増加
      *  01.00.04    門間      18/09/05    不要コメントを削除
      *****************************************************************
       01  SKT3820.
      *ページ番号
           03  SKT3820-NO                  PIC  X(04).
      *診療年月
           03  SKT3820-SRYYM               PIC  X(16).
      *請求年月日
           03  SKT3820-SEIYMD              PIC  X(22).
      *医療機関コード
           03  SKT3820-HOSPCD              PIC  X(14).
      *医療機関住所
           03  SKT3820-HOSPADRS            PIC  X(100).
      *医療機関名称
           03  SKT3820-HOSPNAME            PIC  X(100).
      *開設者名
           03  SKT3820-KAISETU             PIC  X(60).
      *保険者名称
           03  SKT3820-HKNJANAME           PIC  X(24).
      *保険者番号
           03  SKT3820-HKNJANUM            PIC  X(12).
      *在総診・在医総
           03  SKT3820-ZAI                 PIC  X(02) OCCURS 2.
      *特別療養費
           03  SKT3820-TOKUBETU            PIC  X(10).
      *療養の給付テーブル
           03  SKT3820-SEIKYU  OCCURS  7.
               05  SKT3820-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT3820-KENSU        PIC  X(06).
      *日数
                   07 SKT3820-NISSU        PIC  X(06).
      *点数
                   07 SKT3820-TENSU        PIC  X(10).
      *一部負担金
                   07 SKT3820-ITBFTN       PIC  X(09).
      *食事療養テーブル
           03  SKT3820-SHOKUJI-TBL OCCURS 7.
      *食事件数
               05  SKT3820-S-KENSU         PIC  X(06).
      *食事日数
               05  SKT3820-S-NISSU         PIC  X(06).
      *食事療養費
               05  SKT3820-S-MONEY         PIC  X(09).
      *標準負担額
               05  SKT3820-S-FTNGAKU       PIC  X(09).
