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
      *  01.00.02    吉川      2013/11/05   月遅れ件数、長期件数追加
      *  01.00.03    萩野      2020/02/12   修正履歴コメント削除
      ********************************************************************
       01  SKT2310.
      *    診療年月
           03  SKT2310-PRTYM       PIC  X(16).
      *    請求年月日
           03  SKT2310-SEIYMD      PIC  X(22).
      *    保険者番号
           03  SKT2310-HKNJANUM    PIC  X(12).
      *    医療機関コード
           03  SKT2310-HOSPCD      PIC  X(14).
      *    医療機関住所
           03  SKT2310-HOSPADRS    PIC  X(100).
      *    医療機関名称
           03  SKT2310-HOSPNAME    PIC  X(100).
      *    医療機関電話番号
           03  SKT2310-HOSPTEL     PIC  X(15).
      *    医療機関開設者名
           03  SKT2310-KAISETU     PIC  X(40).
      *    在宅
           03  SKT2310-ZAI1        PIC  X(02).
           03  SKT2310-ZAI2        PIC  X(02).
      *    ページ番号
           03  SKT2310-NO          PIC  X(04).
      *    特別療養費
           03  SKT2310-TOKUBETU    PIC  X(10).
      *    療養の給付テーブル
           03  SKT2310-RYOUYOU-TBL OCCURS 8.
      *    入外テーブル
               05  SKT2310-NYUGAI-TBL OCCURS 2.
      *    件数
                   07  SKT2310-KENSU      PIC X(05).
      *    日数
                   07  SKT2310-NISSU      PIC X(05).
      *    点数
                   07  SKT2310-TENSU      PIC X(09).
      *    一部負担金
                   07  SKT2310-ITBFTN     PIC X(09).
      *    食事療養費テーブル
           03  SKT2310-SHOKUJI-TBL OCCURS 8.
      *    食事−件数
               05  SKT2310-S-KENSU        PIC X(05).
      *    食事−日数
               05  SKT2310-S-NISSU        PIC X(05).
      *    食事−金額
               05  SKT2310-S-MONEY        PIC X(09).
      *    食事−標準負担額
               05  SKT2310-S-HJNFTN       PIC X(09).
      *    公費件数
           03  SKT2310-KOHKENSU           PIC X(09).
      *    乳・障・母・傷件数
           03  SKT2310-T-KOHKENSU         PIC X(09).
      *    高額療養費件数
           03  SKT2310-KOGAKU             PIC X(07).
      *    月遅れ件数
           03  SKT2310-TUKIOKURE          PIC X(07).
      *    長期件数
           03  SKT2310-TYOKI              PIC X(07).
