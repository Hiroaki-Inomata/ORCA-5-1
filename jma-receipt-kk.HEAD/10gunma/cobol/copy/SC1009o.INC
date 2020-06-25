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
       01  SC09o.
      *    印刷日付
           03  SC09o-PRTYM               PIC  X(16).
      *    市町村名
           03  SC09o-CITYNAME            PIC  X(20).
      *    請求日付
           03  SC09o-SEIYMD              PIC  X(22).
      *    郵便番号
      *     03  SC09o-POST                PIC  X(18).
      *    所在地
           03  SC09o-ADRS                PIC  X(100).
      *    医療機関名称
           03  SC09o-HOSPNAME            PIC  X(100).
      *    電話番号
           03  SC09o-TEL                 PIC  X(15).
      *    開設者氏名
           03  SC09o-KAISETUNAME         PIC  X(40).
      *    保険者番号
           03  SC09o-HKNJANUM            PIC  X(12).
      *    医療機関コード
           03  SC09o-HOSPCD              PIC  X(14).
      *    点数表区分
           03  SC09o-TENHYOKBN-TBL        OCCURS  4.
               05  SC09o-TENHYOKBN       PIC  X(02).
      *    その他公費番号
           03  SC09o-ETCKOHNUM           PIC  X(02).
      *    請求テーブル
           03  SC09o-SEIKYU-TBL      OCCURS  22.
      *    件数
               05  SC09o-KENSU           PIC  X(05).
      *    点数
               05  SC09o-TENSU           PIC  X(09).
      *    福祉医療費請求額
               05  SC09o-FKSGAKU         PIC  X(10).
      *    食事療養：件数
               05  SC09o-S-KENSU         PIC  X(05).
      *    食事療養：標準負担額
               05  SC09o-S-SKJFTN        PIC  X(09).
