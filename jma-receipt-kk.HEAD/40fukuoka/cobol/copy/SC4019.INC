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
       01  SC19.
      *印刷日付
           03  SC19-SRYYM             PIC  X(16).
      *請求日付
           03  SC19-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC19-HOSPCDN           PIC  X(20).
      *医療機関所在地
           03  SC19-ADRS              PIC  X(100).
      *医療機関名称
           03  SC19-HOSPNAME          PIC  X(100).
      *医療機関電話番号
           03  SC19-TEL               PIC  X(15).
      *管理者名
           03  SC19-KAISETU           PIC  X(30).
      *点数表区分
           03  SC19-TENSUHYO          PIC  X(02) OCCURS 4.
      *市町村名
           03  SC19-CITYNAME          PIC  X(26).
      *請求エリア
           03  SC19-SEIKYU-AREA.
      *件数
               05  SC19-KENSU      PIC  X(06).
      *一部負担金額
               05  SC19-SEIKYUGK   PIC  X(11).
