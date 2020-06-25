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
      *    HAORIリクエスト収納情報
      *    COPY (CPINCOMEV3REQ.INC)
      ******************************************************************
       01  INCOMEREQ-AREA.
           03  INCOMEREQ-CONTEXT          PIC S9(9)   BINARY.
           03  INCOMEREQ-OBJECT           PIC X(36).
           03  INCOMEREQ-MODE             PIC S9(9)   BINARY.
           03  INCOMEREQ-RECORDNAME       PIC X(128).
      *
           03  INCOMEREQ-DATA-AREA.
               05  INCOMEREQ-REQNUM       PIC X(02).
               05  INCOMEREQ-REQMODE      PIC X(02).
               05  INCOMEREQ-KARTE-UID    PIC X(36).
               05  INCOMEREQ-ORCA-UID     PIC X(36).
