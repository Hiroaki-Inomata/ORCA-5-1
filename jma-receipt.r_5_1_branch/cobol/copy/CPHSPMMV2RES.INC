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
       01  XML-HSPMMRES.
         02  HSPMMRES-CONTEXT                  PIC S9(9)   BINARY.
         02  HSPMMRES-OBJECT                   PIC X(36).
         02  HSPMMRES-MODE                     PIC S9(9)   BINARY.
         02  HSPMMRES-RECORDNAME               PIC X(128).
         02  HSPMMRES-AREA.
           03  HSPMMRES-API-INFORMATION-DATE   PIC X(10).
           03  HSPMMRES-API-INFORMATION-TIME   PIC X(8).
           03  HSPMMRES-API-RESULT             PIC X(4).
           03  HSPMMRES-API-RESULT-MSG         PIC X(100).
           03  HSPMMRES-SRYYM-MAX-G.
             04  HSPMMRES-SRYYM-MAX-OCC        OCCURS  1000.
               05  HSPMMRES-PTNUM              PIC X(20).
               05  HSPMMRES-SRYYM              PIC X(7).
