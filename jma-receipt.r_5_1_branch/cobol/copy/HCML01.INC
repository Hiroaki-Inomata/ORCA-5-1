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
            01  HCML01.
              03  HCML01-SRYCD            PIC  X(9)    OCCURS  15.
              03  HCML01-TEN              PIC  X(14)   OCCURS  15.
              03  HCML01-TANINAME         PIC  X(24)   OCCURS  15.
              03  HCML01-CSYYOURYO        PIC  X(5)    OCCURS  15.
              03  HCML01-MADOKUKBN        PIC  X(8)    OCCURS  15.
              03  HCML01-SINKEIHAKAIKBN   PIC  X(8)    OCCURS  15.
              03  HCML01-SEIBUTUGAKUKBN   PIC  X(8)    OCCURS  15.
              03  HCML01-ZOEIZAIKBN       PIC  X(8)    OCCURS  15.
              03  HCML01-YKZKBN           PIC  X(8)    OCCURS  15.
              03  HCML01-ZAIGATAKBN       PIC  X(8)    OCCURS  15.
              03  HCML01-KEIKASOCHIYMD    PIC  X(8)    OCCURS  15.
              03  HCML01-YAKKAKJNCD       PIC  X(12)   OCCURS  15.
              03  HCML01-YUKOSTYMD        PIC  X(8)    OCCURS  15.
              03  HCML01-YUKOEDYMD        PIC  X(8)    OCCURS  15.
              03  HCML01-NAME             PIC  X(100)  OCCURS  15.
              03  HCML01-NYORO            PIC  X(2)    OCCURS  15.
              03  HCML01-PAGE             PIC  X(6).
              03  HCML01-HOSPNAME         PIC  X(60).
              03  HCML01-MSTYMD           PIC  X(22).
