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
       01  SKT3410.
      *    NO
           03  SKT3410-NO                  PIC  X(04).
      *    診療年月
           03  SKT3410-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT3410-SEIYMD              PIC  X(22).
      *    住所
           03  SKT3410-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT3410-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT3410-KAISETUNAME         PIC  X(40).
      *    保険者番号
           03  SKT3410-HKNJANAME           PIC  X(20).
           03  SKT3410-HKNJANUM            PIC  X(06).
      *    医療機関コード
           03  SKT3410-HOSPCD              PIC  X(20).
      *    特別療養費
           03  SKT3410-TOKUBETU            PIC  X(10).
      *    請求テーブル
           03  SKT3410-SEIKYU-TBL  OCCURS  9.
      *    入外テーブル
               05  SKT3410-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3410-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3410-NISSU       PIC  X(05).
      *    点数
                   07  SKT3410-TENSU       PIC  X(09).
      *    一部負担金
                   07  SKT3410-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT3410-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3410-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3410-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3410-S-FTNGK         PIC  X(09).
