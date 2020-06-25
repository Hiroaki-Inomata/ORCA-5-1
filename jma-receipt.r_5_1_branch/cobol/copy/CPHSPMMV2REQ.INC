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
       01  XML-HSPMMREQ.
         02  HSPMMREQ-CONTEXT                  PIC S9(9)   BINARY.
         02  HSPMMREQ-OBJECT                   PIC X(36).
         02  HSPMMREQ-MODE                     PIC S9(9)   BINARY.
         02  HSPMMREQ-RECORDNAME               PIC X(128).
         02  HSPMMREQ-AREA.
           03  HSPMMREQ-KIHON-AREA.
             04  HSPMMREQ-PTID                 PIC X(20).
             04  HSPMMREQ-SRYYM                PIC X(07).
