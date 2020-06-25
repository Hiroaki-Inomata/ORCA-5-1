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
       01  SKT1020.
      *    診療年月
           03  SKT1020-SRYYM             PIC  X(16).
      *    保険者名
           03  SKT1020-HKNJANAME         PIC  X(30).
      *    請求年月日
           03  SKT1020-SEIYMD            PIC  X(22).
      *    在医総
           03  SKT1020-ZAI               PIC  X(02).
      *    郵便番号
      *     03  SKT1020-POST              PIC  X(18).
      *    住所
           03  SKT1020-ADRS              PIC  X(100).
      *    医療機関名
           03  SKT1020-HOSPNAME          PIC  X(100).
      *    開設者名
           03  SKT1020-KAISETUNAME       PIC  X(40).
      *    電話番号
           03  SKT1020-TEL               PIC  X(15).
      *    保険者番号
           03  SKT1020-HKNJANUM          PIC  X(12).
      *    医療機関コード
           03  SKT1020-HOSPCD            PIC  X(14).
      *    ページ番号
           03  SKT1020-NO                PIC  X(04).
      *    特別療養費
           03  SKT1020-TOKUBETU          PIC  X(10).
      *    請求テーブル
           03  SKT1020-SEIKYU-TBL   OCCURS  5.
      *    入外テーブル
               05  SKT1020-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT1020-KENSU     PIC  X(03).
      *    公費件数
                   07  SKT1020-KKENSU    PIC  X(03).
      *    福祉件数
                   07  SKT1020-FKENSU    PIC  X(03).
      *    日数
                   07  SKT1020-NISSU     PIC  X(05).
      *    点数
                   07  SKT1020-TENSU     PIC  X(09).
      *    一部負担金
                   07  SKT1020-ITBFTN    PIC  X(09).
      *    食事療養：件数
               05  SKT1020-S-KENSU       PIC  X(03).
      *    食事療養：日数
               05  SKT1020-S-NISSU       PIC  X(05).
      *    食事療養：金額
               05  SKT1020-S-KINGAKU     PIC  X(09).
      *    食事療養：標準負担額
               05  SKT1020-S-FTNGAKU     PIC  X(09).
