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
      *
       01  SDTCHK05AREA.
           03  SDTCHK05-IN.
               05  SDTCHK05-HOSPNUM        PIC 9(02).
               05  SDTCHK05-PTID           PIC X(100).
               05  SDTCHK05-HKNKBN         PIC X(01).
           03  SDTCHK05-OT.
               05  SDTCHK05-OT-MAX         PIC 9(05).
               05  SDTCHK05-OT-OCC         OCCURS 50.
      *        主保険:0/公費:1
                   07  SDTCHK05-HKNKOHKBN  PIC X(01).
                   07  SDTCHK05-HKNNUM     PIC X(03).
                   07  SDTCHK05-HKNNUMMEI  PIC X(20).
                   07  SDTCHK05-PAYKBN     PIC X(02).
                   07  SDTCHK05-HKNJANUM   PIC X(08).
                   07  SDTCHK05-HONKZKKBN  PIC X(01).
                   07  SDTCHK05-RSIHKNKBN  PIC X(01).

