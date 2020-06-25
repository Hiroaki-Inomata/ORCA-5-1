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
       01  XML-MLURES.
         02  MLURES-CONTEXT                        PIC S9(9)   BINARY.
         02  MLURES-OBJECT                         PIC X(36).
         02  MLURES-MODE                           PIC S9(9)   BINARY.
         02  MLURES-RECORDNAME                     PIC X(128).
         02  MLURES-AREA.
           03  MLURES-KIHON-AREA.
             04  MLURES-API-INFORMATION-DATE       PIC X(10).
             04  MLURES-API-INFORMATION-TIME       PIC X(8).
             04  MLURES-API-RESULT                 PIC X(4).
             04  MLURES-API-RESULT-MSG             PIC X(100).
             04  MLURES-MASTER-LAST-UPDATE-OCC     OCCURS 20.
               05  MLURES-MSTID                    PIC X(64).
               05  MLURES-LASTYMD                  PIC X(10).
             04  MLURES-SOLO-MSTID                 PIC X(64).
             04  MLURES-SOLO-LASTYMD               PIC X(10).
             04  MLURES-HISTORY-OCC                OCCURS 5.
               05  MLURES-HLASTYMD                 PIC X(10).
               05  MLURES-HCOUNT                   PIC X(8).
