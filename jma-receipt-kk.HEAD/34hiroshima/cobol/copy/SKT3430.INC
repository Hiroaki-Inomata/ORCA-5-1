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
       01  SKT3430.
      *    NO
           03  SKT3430-NO                  PIC  X(04).
      *    医療機関コード
           03  SKT3430-HOSPCD              PIC  X(20).
      *    保険者番号
           03  SKT3430-HKNJANUM            PIC  X(06).
      *    特別療養費
           03  SKT3430-TOKUBETU            PIC  X(10).
      *    結核３４条請求テーブル
           03  SKT3430-34-SEIKYU-TBL .
      *    入外テーブル
               05  SKT3430-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT3430-34KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3430-34NISSU       PIC  X(05).
      *    点数
                   07  SKT3430-34TENSU       PIC  X(09).
      *    食事療養：件数
               05  SKT3430-S-34KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3430-S-34NISSU         PIC  X(05).
      *    結核３５条請求テーブル
           03  SKT3430-35-SEIKYU-TBL.
      *    件数
               05  SKT3430-35KENSU       PIC  X(05).
      *    診療実日数
               05  SKT3430-35NISSU       PIC  X(05).
      *    点数
               05  SKT3430-35TENSU       PIC  X(09).
      *    患者負担額
               05  SKT3430-35ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT3430-S-35KENSU     PIC  X(05).
      *    食事療養：日数
               05  SKT3430-S-35NISSU     PIC  X(05).
      *    食事療養：金額
               05  SKT3430-S-35KINGK     PIC  X(09).
      *
      *    公費請求テーブル
           03  SKT3430-KSEIKYU-TBL OCCURS 6.
      *    入外テーブル
               05  SKT3430-KNYUGAI-TBL OCCURS 2.
      *    件数
                   07  SKT3430-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT3430-KNISSU       PIC  X(05).
      *    点数
                   07  SKT3430-KTENSU       PIC  X(09).
      *    一部負担金
                   07  SKT3430-KITBFTN      PIC  X(09).
      *    公費法別番号
               05  SKT3430-HBTNUM           PIC  X(02).
      *    公費法別番号(老人用)
               05  SKT3430-RHBTNUM1         PIC  X(02).
               05  SKT3430-RHBTNUM2         PIC  X(02).
               05  SKT3430-RSEN             PIC  X(02).
      *    公費名称
               05  SKT3430-KNAME            PIC  X(10).
      *    食事療養：件数
               05  SKT3430-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT3430-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT3430-S-KKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT3430-S-KFTNGK         PIC  X(09).
