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
      ******************************************************************
      *    ＡＰＩレスポンスシステム情報
      *    COPY (CPSYSINFV2RES.INC)
      ******************************************************************
       01  XML-SYSINFRES.
           03  SYSINFRES-CONTEXT                PIC S9(9)   BINARY.
           03  SYSINFRES-OBJECT                 PIC X(36).
           03  SYSINFRES-MODE                   PIC S9(9)   BINARY.
           03  SYSINFRES-RECORDNAME             PIC X(128).
      *
           03  SYSINFRES-PRIVATE-OBJECTS.
               05  SYSINFRES-INFORMATION-DATE   PIC X(10).
               05  SYSINFRES-INFORMATION-TIME   PIC X(08).
               05  SYSINFRES-API-RESULT         PIC X(04).
               05  SYSINFRES-API-RESULT-MSG     PIC X(100).
               05  SYSINFRES-JMA-VERSION        PIC X(64).
               05  SYSINFRES-DB-G.
                   07  SYSINFRES-DB-LOCAL       PIC X(64).
                   07  SYSINFRES-DB-NEW         PIC X(64).
               05  SYSINFRES-MST-G.
                   07  SYSINFRES-LAST-UPDATE    PIC X(10).
                   07  SYSINFRES-MST-OCC        OCCURS 99.
                       09  SYSINFRES-MST-NAME   PIC X(30).
                       09  SYSINFRES-MST-LOCAL  PIC X(64).
                       09  SYSINFRES-MST-NEW    PIC X(64).
               05  SYSINFRES-PG-G.
                   07  SYSINFRES-PG-OCC         OCCURS 10.
                       09  SYSINFRES-PG-DATE    PIC X(10).
                       09  SYSINFRES-PG-STATE   PIC X(08).
                       09  SYSINFRES-PG-COMMENT PIC X(100).
