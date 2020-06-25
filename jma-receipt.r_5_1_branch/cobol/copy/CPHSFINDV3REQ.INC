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
       01  XML-HSFINDREQ.
         02  HSFINDREQ-CONTEXT                 PIC S9(9)   BINARY.
         02  HSFINDREQ-OBJECT                  PIC X(36).
         02  HSFINDREQ-MODE                    PIC S9(9)   BINARY.
         02  HSFINDREQ-RECORDNAME              PIC X(128).
         02  HSFINDREQ-AREA.
           03  HSFINDREQ-KIHON-AREA.
             04  HSFINDREQ-REQNUM              PIC X(2).
             04  HSFINDREQ-BASEYMD             PIC X(10).
             04  HSFINDREQ-RANGE-FROM          PIC X(7).
             04  HSFINDREQ-RANGE-TO            PIC X(7).
             04  HSFINDREQ-PTNUM               PIC X(20).
             04  HSFINDREQ-NAME                PIC X(100).
             04  HSFINDREQ-STBIRTHDAY          PIC X(10).
             04  HSFINDREQ-EDBIRTHDAY          PIC X(10).
             04  HSFINDREQ-ADRS                PIC X(100).
             04  HSFINDREQ-STNYUINYMD          PIC X(10).
             04  HSFINDREQ-EDNYUINYMD          PIC X(10).
             04  HSFINDREQ-STTAIINYMD          PIC X(10).
             04  HSFINDREQ-EDTAIINYMD          PIC X(10).
             04  HSFINDREQ-SRYKA               PIC X(2).
             04  HSFINDREQ-BTUNUM              PIC X(2).
             04  HSFINDREQ-JOTAI               PIC X(1).
             04  HSFINDREQ-SEIKYUJOTAI         PIC X(1).
             04  HSFINDREQ-TEIKIYM             PIC X(7).
             04  HSFINDREQ-KBT                 PIC X(1).
             04  HSFINDREQ-TAIINCD             PIC X(2).
             04  HSFINDREQ-DEATHKBN            PIC X(1).
             04  HSFINDREQ-TSTPT               PIC X(1).
             04  HSFINDREQ-SORT-KEY-OCC        OCCURS 3.
               05  HSFINDREQ-SORT-KEY          PIC X(1).
               05  HSFINDREQ-SORT-ORDER        PIC X(1).
