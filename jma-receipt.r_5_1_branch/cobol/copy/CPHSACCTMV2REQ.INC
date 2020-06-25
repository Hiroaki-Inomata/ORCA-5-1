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
      *    ＡＰＩリクエスト食事情報 
      *    COPY (CPHSACCTMV2REQ.INC)
      ******************************************************************
       01  XML-HSACCTMREQ.
           03  HSACCTMREQ-CONTEXT            PIC S9(9)   BINARY.
           03  HSACCTMREQ-OBJECT             PIC X(36).
           03  HSACCTMREQ-MODE               PIC S9(9)   BINARY.
           03  HSACCTMREQ-RECORDNAME         PIC X(128).
           03  HSACCTMREQ-PRIVATE-OBJECTS.
               05  HSACCTMREQ-SAVEREQ        PIC X(1).
               05  HSACCTMREQ-REQNUM         PIC X(1).
               05  HSACCTMREQ-PTNUM          PIC X(20).
               05  HSACCTMREQ-NYUINYMD       PIC X(10).
               05  HSACCTMREQ-ENDYM          PIC X(7).
               05  HSACCTMREQ-SRYYMD         PIC X(10).
               05  HSACCTMREQ-ENDYMD         PIC X(10).
               05  HSACCTMREQ-SKJ-AREA.
                   07  HSACCTMREQ-SKJ-OCC    OCCURS  3.
                       09  HSACCTMREQ-SKJTIME    PIC X(2).
                       09  HSACCTMREQ-SKJTYPE    PIC X(1).
                       09  HSACCTMREQ-NISSU      PIC X(100).
                       09  HSACCTMREQ-SAME-AFTER PIC X(1).
               05  HSACCTMREQ-GAIHAKUKBN        PIC X(2).
               05  HSACCTMREQ-SAGAKU            PIC X(7).
