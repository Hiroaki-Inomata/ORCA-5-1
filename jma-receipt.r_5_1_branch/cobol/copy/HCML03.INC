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
            01  HCML03.
              03  HCML03-SRYCD          PIC  X(9)      OCCURS  15.
              03  HCML03-NAME           PIC  X(100)    OCCURS  15.
              03  HCML03-TEN            PIC  X(14)     OCCURS  15.
              03  HCML03-TANINAME       PIC  X(24)     OCCURS  15.
              03  HCML03-TENSIKIBETU    PIC  X(1)      OCCURS  15.
              03  HCML03-SRYKBN         PIC  X(2)      OCCURS  15.
              03  HCML03-SRYSYUKBN      PIC  X(3)      OCCURS  15.
              03  HCML03-DATAKBN        PIC  X(1)      OCCURS  15.
              03  HCML03-HKNTEKKBN      PIC  X(1)      OCCURS  15.
              03  HCML03-NYUGAITEKKBN   PIC  X(1)      OCCURS  15.
              03  HCML03-ROUTEKKBN      PIC  X(1)      OCCURS  15.
              03  HCML03-HOSPSRYKBN     PIC  X(1)      OCCURS  15.
              03  HCML03-HOUKNSKBN      PIC  X(2)      OCCURS  15.
              03  HCML03-JGNCNT         PIC  X(3)      OCCURS  15.
              03  HCML03-TSUSOKUAGE     PIC  X(1)      OCCURS  15.
              03  HCML03-KGNAGE         PIC  X(2)      OCCURS  15.
              03  HCML03-NYOROAGE       PIC  X(2)      OCCURS  15.
              03  HCML03-JGNAGE         PIC  X(2)      OCCURS  15.
              03  HCML03-KNSJISGRPKBN   PIC  X(2)      OCCURS  15.
              03  HCML03-GAIKANRIKBN    PIC  X(1)      OCCURS  15.
              03  HCML03-BUIKBN         PIC  X(1)      OCCURS  15.
              03  HCML03-SANTEIRRKKBN   PIC  X(1)      OCCURS  15.
              03  HCML03-YUKOSTYMD      PIC  X(8)      OCCURS  15.
              03  HCML03-NYORO          PIC  X(2)      OCCURS  15.
              03  HCML03-YUKOEDYMD      PIC  X(8)      OCCURS  15.
              03  HCML03-CDKBN          PIC  X(10)     OCCURS  15.
              03  HCML03-PAGE           PIC  X(6).
              03  HCML03-HOSPNAME       PIC  X(60).
              03  HCML03-MSTYMD         PIC  X(22).
