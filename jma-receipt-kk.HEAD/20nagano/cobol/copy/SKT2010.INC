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
       01  SKT2010.
      *    診療年月
              03  SKT2010-SRYYM            PIC  X(16).
      *    請求テーブル
              03  SKT2010-SEIKYU-TBL       OCCURS  42.
      *    保険者名
                  05  SKT2010-HKNJANAME    PIC  X(20).
      *    件数
                  05  SKT2010-KENSU        PIC  X(05).
      *    小計：件数
              03  SKT2010-SKENSU           PIC  X(06).
      *    合計：件数
              03  SKT2010-GKENSU           PIC  X(06).
      *    特別療養費
              03  SKT2010-TOKUBETU         PIC  X(05).
      *    保険者数
              03  SKT2010-HKNJASU          PIC  X(03).
      *    長期
              03  SKT2010-TYOKI            PIC  X(07) OCCURS 42.
              03  SKT2010-STYOKI           PIC  X(08).
              03  SKT2010-GTYOKI           PIC  X(08).

