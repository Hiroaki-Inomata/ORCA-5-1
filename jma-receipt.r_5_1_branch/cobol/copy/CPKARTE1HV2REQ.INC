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
      ******************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************
       01  KARTE1HREQ-AREA.
           03  KARTE1HREQ-CONTEXT          PIC S9(9)   BINARY.
           03  KARTE1HREQ-OBJECT           PIC X(36).
           03  KARTE1HREQ-MODE             PIC S9(9)   BINARY.
           03  KARTE1HREQ-RECORDNAME       PIC X(128).
           03  KARTE1HREQ-DATA-AREA.
              05  KARTE1HREQ-REQNUM        PIC X(2).
              05  KARTE1HREQ-PRINT-MODE    PIC X(10).
              05  KARTE1HREQ-HAKKOKBN      PIC X(1).
              05  KARTE1HREQ-PTNUM         PIC X(20).
              05  KARTE1HREQ-NYUINYMD      PIC X(10).
              05  KARTE1HREQ-TAIINYMD      PIC X(10).
              05  KARTE1HREQ-SRYKA         PIC X(02).
              05  KARTE1HREQ-DRCD          PIC X(05)   OCCURS 3.
              05  KARTE1HREQ-HKNCOMBINUM   PIC X(04).
