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
          01  HCML05.
              03  HCML05-SRYCD                 PIC  X(9)    OCCURS  15.
              03  HCML05-NAME                  PIC  X(100)  OCCURS  15.
              03  HCML05-TEN                   PIC  X(14)   OCCURS  15.
              03  HCML05-TANINAME              PIC  X(24)   OCCURS  15.
              03  HCML05-DATAKBN               PIC  X(1)    OCCURS  15.
              03  HCML05-JGNTEN                PIC  X(7)    OCCURS  15.
              03  HCML05-KGNAGE                PIC  X(2)    OCCURS  15.
              03  HCML05-NYOROAGE              PIC  X(2)    OCCURS  15.
              03  HCML05-JGNAGE                PIC  X(2)    OCCURS  15.
              03  HCML05-TOKUKIZAIAGEKSNKBN    PIC  X(8)    OCCURS  15.
              03  HCML05-MEIUSESKB             PIC  X(8)    OCCURS  15.
              03  HCML05-SANSOKBN              PIC  X(8)    OCCURS  15.
              03  HCML05-TOKUKIZAISBTONE       PIC  X(8)    OCCURS  15.
              03  HCML05-TOKUKIZAISBTTSU       PIC  X(8)    OCCURS  15.
              03  HCML05-KOUHYOJYUNNUM         PIC  X(9)    OCCURS  15.
              03  HCML05-YUKOSTYMD             PIC  X(8)    OCCURS  15.
              03  HCML05-NYORO                 PIC  X(2)    OCCURS  15.
              03  HCML05-YUKOEDYMD             PIC  X(8)    OCCURS  15.
              03  HCML05-PAGE                  PIC  X(6).
              03  HCML05-HOSPNAME              PIC  X(60).
              03  HCML05-MSTYMD                PIC  X(22).
