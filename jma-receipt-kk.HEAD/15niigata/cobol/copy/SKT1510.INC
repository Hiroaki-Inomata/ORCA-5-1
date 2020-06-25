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
       01  SKT1510.
      *    ページ番号
           03  SKT1510-NO                PIC  X(04).
      *    診療年月
           03  SKT1510-SRYYM             PIC  X(16).
      *    医療機関住所
           03  SKT1510-ADRS              PIC  X(100).
      *    医療機関名
           03  SKT1510-HOSPNAME          PIC  X(100).
      *    開設者名
           03  SKT1510-KAISETUNAME       PIC  X(40).
      *    電話番号
           03  SKT1510-TEL               PIC  X(15).
      *    請求年月日
           03  SKT1510-SEIYMD            PIC  X(22).
      *    保険者番号
           03  SKT1510-HKNJANUM          PIC  X(12).
      *    保険者名称
           03  SKT1510-HKNJANAME         PIC  X(26).
      *    医療機関コード
           03  SKT1510-HOSPCD            PIC  X(20).
      *    在総診
           03  SKT1510-ZAI1              PIC  X(02).
           03  SKT1510-ZAI2              PIC  X(02).
      *    特別療養費
           03  SKT1510-TOKUBETU          PIC  X(10).
      *    請求テーブル
           03  SKT1510-SEIKYU-TBL     OCCURS  11.
      *    入外テーブル
               05  SKT1510-NYUGAI-TBL  OCCURS 2.
      *    件数
                   07  SKT1510-KENSU         PIC  X(05).
      *    実日数
                   07  SKT1510-NISSU         PIC  X(05).
      *    点数
                   07  SKT1510-TENSU         PIC  X(09).
      *    一部負担金
                   07  SKT1510-ITBFTN        PIC  X(09).
      *    食事件数
               05  SKT1510-SKENSU            PIC  X(05).
      *    食事日数
               05  SKT1510-SNISSU            PIC  X(05).
      *    食事金額
               05  SKT1510-SKINGK            PIC  X(09).
      *    食事標準負担額
               05  SKT1510-SFTNGK            PIC  X(09).
