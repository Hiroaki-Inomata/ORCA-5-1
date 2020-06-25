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
      *  後期高齢請求書（愛知県）     COPY (SKT2315.INC)               
      *
      *  作成日付    作業者        記述
      *  08/04/16    田中          新規作成(後期高齢者対応)
      ********************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者  日付        内容
      *  01.00.01    吉川    2008/05/20  原爆件数が印字されない不具合修正
      *  01.00.02    吉川    2008/06/24  項目名変更
      *  01.00.03    吉川    2013/11/05  項目追加
      *  01.00.04    萩野    2020/02/12  修正履歴コメント削除
      ********************************************************************
       01  SKT2315.
      *    診療年月
           03  SKT2315-SRYYM       PIC  X(16).
      *    請求年月日
           03  SKT2315-SEIYMD      PIC  X(22).
      *    保険者名
           03  SKT2315-HKNJANAME   PIC  X(100).
      *    保険者番号
           03  SKT2315-HKNJANUM    PIC  X(12).
      *    医療機関コード
           03  SKT2315-HOSPCD      PIC  X(14).
      *    医療機関住所
           03  SKT2315-ADRS        PIC  X(100).
      *    医療機関名称
           03  SKT2315-HOSPNAME    PIC  X(100).
      *    医療機関電話番号
           03  SKT2315-TEL         PIC  X(15).
      *    医療機関開設者名
           03  SKT2315-KAISETUNAME PIC  X(40).
      *    在宅
           03  SKT2315-ZAI1        PIC  X(02).
           03  SKT2315-ZAI2        PIC  X(02).
      *    ページ番号
           03  SKT2315-NO          PIC  X(04).
           03  SKT2315-KOHKENSU    PIC  X(09).
      *    療養の給付テーブル
           03  SKT2315-SEIKYU-TBL OCCURS 2.
      *    入外テーブル
               05  SKT2315-NYUGAI-TBL OCCURS 2.
      *    件数
                   07  SKT2315-KENSU      PIC X(05).
      *    日数
                   07  SKT2315-NISSU      PIC X(05).
      *    点数
                   07  SKT2315-TENSU      PIC X(09).
      *    一部負担金
                   07  SKT2315-ITBFTN     PIC X(09).
      *    食事−件数
               05  SKT2315-S-KENSU         PIC X(05).
      *    食事−日数
               05  SKT2315-S-NISSU         PIC X(05).
      *    食事−金額
               05  SKT2315-S-KINGK         PIC X(09).
      *    食事−標準負担額
               05  SKT2315-S-FTNGK         PIC X(09).
      *    月遅れ件数
           03  SKT2315-TUKIOKURE          PIC X(05).
      *    長期件数
           03  SKT2315-TYOKI              PIC X(05).
      *    高額療養費件数
           03  SKT2315-KOGAKU             PIC X(05).
      *    特別療養費
           03  SKT2315-TOKUBETU           PIC X(10).
