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
       01  SC11.
      *印刷日付
           03  SC11-PRTYM             PIC  X(16).
      *請求日付
           03  SC11-SEIYMD            PIC  X(22).
      *医療機関コード
           03  SC11-HOSPCDN           PIC  X(20).
      *医療機関所在地
           03  SC11-ADRS              PIC  X(100).
      *医療機関名称
           03  SC11-HOSPNAME          PIC  X(100).
      *開設者名
           03  SC11-KAISETU           PIC  X(40).
      *表別
           03  SC11-TENSUHYO          PIC  X(02) OCCURS 3.
      *市町村名
           03  SC11-CITYNAME          PIC  X(26).
      *件数
           03  SC11-KENSU             PIC  X(05).
      *点数
           03  SC11-TENSU             PIC  X(09).
      *薬剤一部負担金
           03  SC11-YKZFTN            PIC  X(09).
      *食事件数
           03  SC11-SKENSU            PIC  X(05).
      *食事標準負担額
           03  SC11-FTNGK             PIC  X(09).
