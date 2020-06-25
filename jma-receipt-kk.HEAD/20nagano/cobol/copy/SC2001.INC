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
       01  SC01.
      *診療年月
           03  SC01-SRYYM               PIC  X(16).
      *医療機関コード
           03  SC01-HOSPCD              PIC  X(20).
      *医療機関所在地
           03  SC01-ADRS                PIC  X(100).
      *医療機関名称
           03  SC01-HOSPNAME            PIC  X(100).
      *電話番号
           03  SC01-TEL                 PIC  X(15).
      *旧総合病院診療科
           03  SC01-SRYKA               PIC  X(04).
      *請求一覧
           03  SKT2500-SEIKYU  OCCURS 40.
      *市町村コード
               05  SC01-CITYNUM         PIC  X(06).
      *市町村名
               05  SC01-CITYNAME        PIC  X(20).
      *件数
               05  SC01-KENSU           PIC  X(05).
      *合計件数
           03  SC01-GKENSU              PIC  X(06).
