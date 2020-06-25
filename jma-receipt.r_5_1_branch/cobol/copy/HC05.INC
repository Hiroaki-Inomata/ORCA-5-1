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
          01  HC05.
              03  HC05-SYSYMD        PIC  X(22).
              03  HC05-PAGE          PIC  X(3).
              03  HC05-TEISYUTUNAME  PIC  X(6).
              03  HC05-RENNUM        PIC  X(5)     OCCURS  22.
              03  HC05-PTNUM         PIC  X(20)    OCCURS  22.
              03  HC05-NAME          PIC  X(26)    OCCURS  22.
              03  HC05-SRYYM         PIC  X(16)    OCCURS  22.
              03  HC05-HENYM         PIC  X(16)    OCCURS  22.
              03  HC05-SEIYM         PIC  X(16)    OCCURS  22.
              03  HC05-HKNTEN        PIC  X(11)    OCCURS  22.
              03  HC05-MONEY1        PIC  X(11)    OCCURS  22.
              03  HC05-MONEY2        PIC  X(11)    OCCURS  22.
              03  HC05-GKENSU        PIC  X(5).
              03  HC05-GHKNTEN       PIC  X(11).
              03  HC05-GMONEY1       PIC  X(11).
              03  HC05-GMONEY2       PIC  X(11).
              03  HC05-SKYKBN        PIC  X(10)    OCCURS  22.
              03  HC05-MSG           PIC  X(04)    OCCURS  22.
              03  HC05-HSRYYM        PIC  X(16).
              03  HC05-TAISYO        PIC  X(02)    OCCURS  22.
              03  HC05-HKNJANUM      PIC  X(16)    OCCURS  22.
