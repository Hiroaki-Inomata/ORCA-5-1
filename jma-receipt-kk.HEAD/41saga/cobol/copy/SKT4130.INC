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
       01  SKT4130.
      *    診療年月
           03  SKT4130-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4130-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4130-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4130-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4130-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4130-TEL                 PIC  X(15).
      *    保険者番号
           03  SKT4130-HKNJANUM            PIC  X(06).
      *    医療機関コード
           03  SKT4130-HOSPCD              PIC  X(07).
      *    保険者名
           03  SKT4130-HOKENJANAME         PIC  X(20).
      *    在総診・在医総
           03  SKT4130-ZAI1                PIC  X(02).
           03  SKT4130-ZAI2                PIC  X(02).
      *    給付割合
           03  SKT4130-KYURATE-TBL  OCCURS  4.
               05  SKT4130-KYURATE         PIC  X(02).
      *    老人請求テーブル
           03  SKT4130-SEIKYU-TBL  OCCURS  2.
      *    入外テーブル
               05  SKT4130-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4130-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4130-NISSU       PIC  X(05).
      *    点数
                   07  SKT4130-TENSU       PIC  X(09).
      *    薬剤一部負担金
                   07  SKT4130-YKZFTN      PIC  X(07).
      *    一部負担金
                   07  SKT4130-ITBFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT4130-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4130-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4130-S-KINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT4130-S-FTNGK         PIC  X(09).
      *
      *    公費請求テーブル
           03  SKT4130-KOUHI-TBL  OCCURS  5.
      *    法別番号
               05  SKT4130-HBTNUM          PIC  X(02).
      *    公費名
               05  SKT4130-KOHNAME         PIC  X(10).
      *    入外テーブル
               05  SKT4130-KNYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4130-KKENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4130-KNISSU       PIC  X(05).
      *    点数
                   07  SKT4130-KTENSU       PIC  X(09).
      *    薬剤一部負担金
                   07  SKT4130-KYKZFTN      PIC  X(07).
      *    一部負担金
                   07  SKT4130-KITBFTN      PIC  X(09).
      *    患者負担金
                   07  SKT4130-KKANFTN      PIC  X(09).
      *    食事療養：件数
               05  SKT4130-S-KKENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4130-S-KNISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4130-S-KKINGK         PIC  X(09).
      *    食事療養：標準負担額
               05  SKT4130-S-KFTNGK         PIC  X(09).
