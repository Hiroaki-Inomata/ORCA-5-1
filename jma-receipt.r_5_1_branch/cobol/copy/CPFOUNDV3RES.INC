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
       01  XML-FOUNDV3RES.
         02  FOUNDRES-CONTEXT                 PIC S9(9)   BINARY.
         02  FOUNDRES-OBJECT                  PIC X(36).
         02  FOUNDRES-MODE                    PIC S9(9)   BINARY.
         02  FOUNDRES-RECORDNAME              PIC X(128).
         02  FOUNDRES-FOUNDV2RES.
             03  FOUNDRES-INFORMATION-DATE    PIC X(10).
             03  FOUNDRES-INFORMATION-TIME    PIC X(08).
             03  FOUNDRES-API-RESULT          PIC X(05).
             03  FOUNDRES-API-RESULT-MSG      PIC X(100).
             03  FOUNDRES-REQNUM              PIC X(02).
             03  FOUNDRES-RESNUM              PIC X(02).
             03  FOUNDRES-KARTE-UID           PIC X(36).
             03  FOUNDRES-ORCA-UID            PIC X(36).
             03  FOUNDRES-DATA-ID             PIC X(37).
             03  FOUNDRES-PRINT-ID            PIC X(37).
             03  FOUNDRES-DATA-COUNT          PIC X(05).
             03  FOUNDRES-PRINT-COUNT         PIC X(05).
