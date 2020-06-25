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
      *  01.00.01    吉川      2010/10/01   特別療養費国保対応
      *  01.00.02    萩野      2018/12/12   修正履歴コメント削除
      ********************************************************************
       01  SKT2050.
      *    診療年月
           03  SKT2050-SRYYM                PIC X(16).
      *    保険者名
           03  SKT2050-HKNJANAME            PIC X(20).
      *    請求年月日
           03  SKT2050-SEIYMD               PIC X(22).
      *    医療機関住所
           03  SKT2050-ADRS                 PIC X(100).
      *    医療機関名称
           03  SKT2050-HOSPNAME             PIC X(100).
      *    開設者名
           03  SKT2050-KAISETUNAME          PIC X(80).
      *    電話番号
           03  SKT2050-TEL                  PIC X(15).
      *    保険者番号
           03  SKT2050-HKNJANUM             PIC X(12).
      *    医療機関コード
           03  SKT2050-HOSPCD               PIC X(14).
      *    在宅
           03  SKT2050-ZAI1                 PIC X(02).
           03  SKT2050-ZAI2                 PIC X(02).
      *    ページ番号
           03  SKT2050-NO                   PIC X(04).
      *    特別療養費
           03  SKT2050-TOKUBETU             PIC X(10).
      *    請求テーブル
           03  SKT2050-SEIKYU-TBL  OCCURS  7.
      *    件数
               05  SKT2050-KENSU            PIC X(05).
      *    実日数
               05  SKT2050-NISSU            PIC X(05).
      *    点数
               05  SKT2050-TENSU            PIC X(11).
      *    一部負担金
               05  SKT2050-ITBFTN           PIC X(11).
      *    公費テーブル
           03  SKT2050-KOH-TBL  OCCURS  5 .
      *    公費番号
               05  SKT2050-KOHNUM           PIC X(02).
      *    公費名
               05  SKT2050-KOHNAME          PIC X(10).
      *    件数
               05  SKT2050-K-KENSU          PIC X(05).
      *    実日数
               05  SKT2050-K-NISSU          PIC X(05).
      *    点数
               05  SKT2050-K-TENSU          PIC X(11).
      *    一部負担金
               05  SKT2050-K-ITBFTN         PIC X(11).
