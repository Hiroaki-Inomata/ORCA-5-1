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
      *  01.00.01    吉川      2010/11/01   特別療養費国保対応
      *  01.00.02    萩野      2020/02/12   修正履歴コメント削除
      ********************************************************************
       01  SKT2320.
      *    診療年月
           03  SKT2320-PRTYM       PIC  X(16).
      *    請求年月日
           03  SKT2320-SEIYMD      PIC  X(22).
      *    保険者番号
           03  SKT2320-HKNJANUM    PIC  X(12).
      *    医療機関コード
           03  SKT2320-HOSPCD      PIC  X(14).
      *    医療機関住所
           03  SKT2320-HOSPADRS    PIC  X(100).
      *    医療機関名称
           03  SKT2320-HOSPNAME    PIC  X(100).
      *    医療機関電話番号
           03  SKT2320-HOSPTEL     PIC  X(15).
      *    医療機関開設者名
           03  SKT2320-KAISETU     PIC  X(40).
      *    在宅
           03  SKT2320-ZAI1        PIC  X(02).
           03  SKT2320-ZAI2        PIC  X(02).
      *    ページ番号
           03  SKT2320-NO          PIC  X(04).
      *    特別療養費
           03  SKT2320-TOKUBETU    PIC  X(10).
      *    療養の給付テーブル
           03  SKT2320-RYOUYOU-TBL OCCURS 2.
      *    入外テーブル
               05  SKT2320-NYUGAI-TBL OCCURS 2.
      *    件数
                   07  SKT2320-KENSU      PIC X(05).
      *    日数
                   07  SKT2320-NISSU      PIC X(05).
      *    点数
                   07  SKT2320-TENSU      PIC X(09).
      *    一部負担金
                   07  SKT2320-ITBFTN     PIC X(09).
      *    食事療養費テーブル
           03  SKT2320-SHOKUJI-TBL OCCURS 2.
      *    食事−件数
               05  SKT2320-S-KENSU         PIC X(05).
      *    食事−日数
               05  SKT2320-S-NISSU         PIC X(05).
      *    食事−金額
               05  SKT2320-S-MONEY         PIC X(09).
      *    食事−標準負担額
               05  SKT2320-S-HJNFTN        PIC X(09).
      *    原爆等公費件数
           03  SKT2320-GENBAKU            PIC X(09).
