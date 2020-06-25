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
      ********************************************************************
          01  SKT4060.
      *    診療年月
           03  SKT4060-SRYYM               PIC  X(16).
      *    請求年月日
           03  SKT4060-SEIYMD              PIC  X(22).
      *    住所
           03  SKT4060-ADRS                PIC  X(100).
      *    医療機関名
           03  SKT4060-HOSPNAME            PIC  X(100).
      *    開設者名
           03  SKT4060-KAISETUNAME         PIC  X(40).
      *    電話番号
           03  SKT4060-TEL                 PIC  X(15).
      *    医療機関コード
           03  SKT4060-HOSPCD              PIC  X(20).
      *    保険者番号
           03  SKT4060-HKNJANUM            PIC  X(12).
      *    在総診・在医総
           03  SKT4060-ZAISOU1             PIC  X(02).
           03  SKT4060-ZAISOU2             PIC  X(02).
      *    ページ番号
           03  SKT4060-NO                  PIC  X(04).
      *    請求テーブル
           03  SKT4060-SEIKYU-TBL  OCCURS  5.
      *    入外テーブル
               05  SKT4060-NYUGAI-TBL  OCCURS  2.
      *    件数
                   07  SKT4060-KENSU       PIC  X(05).
      *    診療実日数
                   07  SKT4060-NISSU       PIC  X(05).
      *    点数
                   07  SKT4060-TENSU       PIC  X(10).
      *    食事療養：件数
               05  SKT4060-S-KENSU         PIC  X(05).
      *    食事療養：日数
               05  SKT4060-S-NISSU         PIC  X(05).
      *    食事療養：金額
               05  SKT4060-S-KINGK         PIC  X(10).
