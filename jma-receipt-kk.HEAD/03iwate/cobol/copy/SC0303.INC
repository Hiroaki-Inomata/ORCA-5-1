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
       01  SC03.
      *診療年月
           03  SC03-SRYYM              PIC  X(16).
      *請求年月日
           03  SC03-SEIYMD             PIC  X(22).
      *医療機関コード
           03  SC03-HOSPCDN            PIC  X(20).
      *単独部分
           03  SC03-TANDOKUKBN         PIC  X(02) OCCURS 2.
      *医療機関所在地
           03  SC03-ADRS               PIC  X(100).
      *医療機関名称
           03  SC03-HOSPNAME           PIC  X(100).
      *開設者名
           03  SC03-KAISETU            PIC  X(40).
      *電話番号
           03  SC03-TEL                PIC  X(15).
      *点数表区分
           03  SC03-TENSUHYO           PIC  X(02) OCCURS 4.
      *市町村名称
           03  SC03-CITYNAME           PIC  X(20).
      *市町村番号
           03  SC03-CITYNUM            PIC  X(08).
      *請求テーブル
           03  SC03-NYUGAI-TBL  OCCURS 2.
               05  SC03-SEIKYU-TBL     OCCURS 4.
      *件数
                   07  SC03-KENSU      PIC X(06).
      *点数
                   07  SC03-TENSU      PIC X(10).
      *一部負担金
                   07  SC03-ITBFTN     PIC X(09).
      *長期件数
               05  SC03-TYOKI1         PIC X(06).
      *長期上位件数
               05  SC03-TYOKI2         PIC X(06).
