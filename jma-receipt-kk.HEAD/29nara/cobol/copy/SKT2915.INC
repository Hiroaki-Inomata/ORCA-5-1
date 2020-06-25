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
      ********************************************************************
      *    後期高齢者請求書（奈良県）     COPY (SKT2915.INC)
      *
      *  作成日付    作業者        記述
      *  08/04/14    吉川          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/07/11  公費番号追加
      *  01.00.02    吉川    2012/06/04  電話番号追加
      *  01.00.03    萩野    2019/04/25  修正履歴コメント削除
      ********************************************************************
       01  SKT2915.
      *診療年月
           03  SKT2915-SRYYM               PIC  X(16).
      *保険者名
           03  SKT2915-HKNJANAME           PIC  X(20).
      *請求年月日
           03  SKT2915-SEIYMD              PIC  X(22).
      *保険者番号
           03  SKT2915-HKNJANUM            PIC  X(12).
      *医療機関コード
           03  SKT2915-HOSPCDN             PIC  X(20).
      *医療機関住所
           03  SKT2915-ADRS                PIC  X(100).
      *医療機関名称
           03  SKT2915-HOSPNAME            PIC  X(100).
      *医療機関電話番号
           03  SKT2915-HOSPTEL             PIC  X(15).
      *開設者名
           03  SKT2915-KAISETUNAME         PIC  X(40).
      *点表区分
           03  SKT2915-TENHYOKBN           PIC  X(02).
      *在宅
           03  SKT2915-ZAI1                PIC  X(02).
           03  SKT2915-ZAI2                PIC  X(02).
      *ページ番号
           03  SKT2915-NO                  PIC  X(04).
      *療養の給付テーブル
           03  SKT2915-SEIKYU  OCCURS  2.
               05  SKT2915-NYUGAI-TBL  OCCURS  2.
      *件数
                   07 SKT2915-KENSU        PIC  X(05).
      *日数
                   07 SKT2915-NISSU        PIC  X(05).
      *点数
                   07 SKT2915-TENSU        PIC  X(09).
      *一部負担金
                   07 SKT2915-ITBFTN       PIC  X(09).
      *食事件数
               05  SKT2915-S-KENSU         PIC  X(05).
      *食事日数
               05  SKT2915-S-NISSU         PIC  X(05).
      *食事療養費
               05  SKT2915-S-KINGK         PIC  X(09).
      *標準負担額
               05  SKT2915-S-FTNGK         PIC  X(09).
      *公費の給付テーブル
           03  SKT2915-KOHI    OCCURS  5.
      *公費名
               05  SKT2915-KOHNAME         PIC  X(10).
      *公費番号
               05  SKT2915-HBTNUM          PIC  X(02).
               05  SKT2915-KNYUGAI-TBL    OCCURS  2.
      *件数
                   07 SKT2915-KKENSU       PIC  X(05).
      *日数
                   07 SKT2915-KNISSU       PIC  X(05).
      *点数
                   07 SKT2915-KTENSU       PIC  X(09).
      *一部負担金
                   07 SKT2915-KITBFTN      PIC  X(09).
      *長期高額療養費件数
           03  SKT2915-TYOKI               PIC  X(05).
