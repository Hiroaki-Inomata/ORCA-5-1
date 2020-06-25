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
      *  01.00.01    吉川    2010/08/04  特別療養費国保対応
      *  01.00.02    萩野    2018/11/07  修正履歴コメント削除
      ********************************************************************
       01  SKT1520.
      *    ページ番号
           03  SKT1520-NO                PIC  X(04).
      *    保険者番号
           03  SKT1520-HKNJANUM          PIC  X(12).
      *    医療機関コード
           03  SKT1520-HOSPCD            PIC  X(20).
      *    在総診
           03  SKT1520-ZAI1              PIC  X(02).
           03  SKT1520-ZAI2              PIC  X(02).
      *    特別療養費
           03  SKT1520-TOKUBETU          PIC  X(10).
      *    福祉請求テーブル
           03  SKT1520-F-SEIKYU-TBL     OCCURS  6.
      *    件数
               05  SKT1520-F-KENSU             PIC  X(05).
      *    点数  
               05  SKT1520-F-TENSU             PIC  X(09).
      *    食事件数
               05  SKT1520-F-SKENSU            PIC  X(05).
      *    食事金額
               05  SKT1520-F-SKINGK            PIC  X(09).
      *    食事標準負担額
               05  SKT1520-F-SFTNGK            PIC  X(09).
      *    公費請求テーブル
           03  SKT1520-K-SEIKYU-TBL     OCCURS 2.
      *    法別番号
               05  SKT1520-K-HBTNUM            PIC  X(02).
      *    公費名
               05  SKT1520-K-KOHNAME           PIC  X(08).
      *    入外テーブル
               05  SKT1520-K-NYUGAI-TBL OCCURS 2.
      *    件数
                   07  SKT1520-K-KENSU         PIC  X(05).
      *    日数
                   07  SKT1520-K-NISSU         PIC  X(05).
      *    点数
                   07  SKT1520-K-TENSU         PIC  X(09).
      *    患者負担額
                   07  SKT1520-K-ITBFTN        PIC  X(09).
      *    食事件数
               05  SKT1520-K-SKENSU            PIC  X(05).
      *    食事日数
               05  SKT1520-K-SNISSU            PIC  X(05).
      *    食事金額
               05  SKT1520-K-SKINGK            PIC  X(09).
      *    食事標準負担額
               05  SKT1520-K-SFTNGK            PIC  X(09).
