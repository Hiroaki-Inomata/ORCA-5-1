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
      *    ＡＰＩレスポンス医療区分・ＡＤＬ点数情報 
      *    COPY (CPHSPTEVALMREQ.INC)
      ******************************************************************
       01  XML-HSPTEVALMREQ.
           03  HSPTEVALMREQ-CONTEXT              PIC S9(9)   BINARY.
           03  HSPTEVALMREQ-OBJECT               PIC X(36).
           03  HSPTEVALMREQ-MODE                 PIC S9(9)   BINARY.
           03  HSPTEVALMREQ-RECORDNAME           PIC X(128).
           03  HSPTEVALMREQ-PRIVATE-OBJECTS.
               05  HSPTEVALMREQ-SAVEREQ          PIC X(1).
               05  HSPTEVALMREQ-PTNUM            PIC X(20).
               05  HSPTEVALMREQ-NYUINYMD         PIC X(10).
               05  HSPTEVALMREQ-SRYYMD           PIC X(10).
               05  HSPTEVALMREQ-MED-AREA.
                   07  HSPTEVALMREQ-MED-OCC      OCCURS    50.
                       09  HSPTEVALMREQ-MED-ID   PIC X(02).
                       09  HSPTEVALMREQ-MED-DATA PIC X(01).
                       09  HSPTEVALMREQ-MED-ALL  PIC X(128).
               05  HSPTEVALMREQ-ADL-AREA.
                   07  HSPTEVALMREQ-ADL-OCC      OCCURS    4.
                       09  HSPTEVALMREQ-ADL-ID   PIC X(1).
                       09  HSPTEVALMREQ-ADL-TEN  PIC X(1).
                       09  HSPTEVALMREQ-ADL-ALL  PIC X(128).
               05  HSPTEVALMREQ-DESIGNR-AREA.
                   07  HSPTEVALMREQ-DESIGNR-ALL  PIC X(128).
