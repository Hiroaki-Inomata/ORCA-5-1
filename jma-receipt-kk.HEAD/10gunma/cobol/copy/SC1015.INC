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
       01  SC15.
      *    住所
           03  SC15-ADRS              PIC  X(100).
      *    開設者名
           03  SC15-KAISETUNAME       PIC  X(40).
      *    医療機関名
           03  SC15-HOSPNAME          PIC  X(100).
      *    医療機関コード
           03  SC15-HOSPCD            PIC  X(14).
      *    点数表区分
           03  SC15-TENHYOKBN         PIC  X(02)  OCCURS  4.
      *    診療年月
           03  SC15-SRYYM             PIC  X(16).
      *    請求年月日
           03  SC15-SEIYMD            PIC  X(22).
      *    媒体種類
           03  SC15-BAITAI            PIC  X(02)  OCCURS  3.
