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
            01  HCML02.
              03  HCML02-SRYCD                 PIC  X(9)    OCCURS  15.
              03  HCML02-NAME                  PIC  X(100)  OCCURS  15.
              03  HCML02-TEN                   PIC  X(14)   OCCURS  15.
              03  HCML02-TANINAME              PIC  X(24)   OCCURS  15.
              03  HCML02-DATAKBN               PIC  X(1)    OCCURS  15.
              03  HCML02-JGNTEN                PIC  X(7)    OCCURS  15.
              03  HCML02-KGNAGE                PIC  X(2)    OCCURS  15.
              03  HCML02-NYOROAGE              PIC  X(2)    OCCURS  15.
              03  HCML02-JGNAGE                PIC  X(2)    OCCURS  15.
              03  HCML02-TOKUKIZAIAGEKSNKBN    PIC  X(8)    OCCURS  15.
              03  HCML02-MEIUSESKB             PIC  X(8)    OCCURS  15.
              03  HCML02-SANSOKBN              PIC  X(8)    OCCURS  15.
              03  HCML02-TOKUKIZAISBTONE       PIC  X(8)    OCCURS  15.
              03  HCML02-TOKUKIZAISBTTSU       PIC  X(8)    OCCURS  15.
              03  HCML02-KOUHYOJYUNNUM         PIC  X(9)    OCCURS  15.
              03  HCML02-YUKOSTYMD             PIC  X(8)    OCCURS  15.
              03  HCML02-NYORO                 PIC  X(2)    OCCURS  15.
              03  HCML02-YUKOEDYMD             PIC  X(8)    OCCURS  15.
              03  HCML02-PAGE                  PIC  X(6).
              03  HCML02-HOSPNAME              PIC  X(60).
              03  HCML02-MSTYMD                PIC  X(22).
              03  HCML02-TITLE                 PIC  X(14).
