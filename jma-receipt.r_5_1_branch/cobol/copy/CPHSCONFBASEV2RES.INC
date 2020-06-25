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
      *    ＡＰＩレスポンス入院基本情報    COPY (CPHSCONFBASEV2RES.INC)
      ******************************************************************
       01  XML-HSCONFBASERES.
           03  HSCBRES-CONTEXT          PIC S9(9)   BINARY.
           03  HSCBRES-OBJECT           PIC X(36).
           03  HSCBRES-MODE             PIC S9(9)   BINARY.
           03  HSCBRES-RECORDNAME       PIC X(128).
      *
           03  HSCBRES-PRIVATE-OBJECTS.
               05  HSCBRES-INFORMATION-DATE       PIC X(10).
               05  HSCBRES-INFORMATION-TIME       PIC X(8).
               05  HSCBRES-API-RESULT             PIC X(2).
               05  HSCBRES-API-RESULT-MSG         PIC X(100).
               05  HSCBRES-SKJRYOYO-G.
                   07  HSCBRES-SKJRYOYO-LBL        PIC X(30).
                   07  HSCBRES-SKJRYOYO-DATA       PIC X(9).
                   07  HSCBRES-SKJRYOYO-NAME       PIC X(30).
               05  HSCBRES-EIYOKANRI-G.
                   07  HSCBRES-EIYOKANRI-LBL       PIC X(30).
                   07  HSCBRES-EIYOKANRI-DATA      PIC X(1).
                   07  HSCBRES-EIYOKANRI-NAME      PIC X(30).
               05  HSCBRES-SAGAKUTAX-G.
                   07  HSCBRES-SAGAKUTAX-LBL       PIC X(30).
                   07  HSCBRES-SAGAKUTAX-DATA      PIC X(1).
                   07  HSCBRES-SAGAKUTAX-NAME      PIC X(30).
               05  HSCBRES-HYOKETSU-G.
                   07  HSCBRES-HYOKETSU-LBL        PIC X(30).
                   07  HSCBRES-HYOKETSU-DATA       PIC X(1).
                   07  HSCBRES-HYOKETSU-NAME       PIC X(30).
               05  HSCBRES-TEISUCHOKA-G.
                   07  HSCBRES-TEISUCHOKA-LBL      PIC X(30).
                   07  HSCBRES-TEISUCHOKA-DATA     PIC X(1).
                   07  HSCBRES-TEISUCHOKA-NAME     PIC X(30).
               05  HSCBRES-NYUINKSN-G.
                   07  HSCBRES-NYUINKSN-OCC        OCCURS  21.
                       09  HSCBRES-NYUINKSN-LBL    PIC X(30).
                       09  HSCBRES-NYUINKSN-DATA   PIC X(9).
                       09  HSCBRES-NYUINKSN-NAME   PIC X(50).
               05  HSCBRES-SENTEI-DEF-G.
                   07  HSCBRES-SENTEI-DEF-LBL  PIC X(30).
                   07  HSCBRES-SENTEI-DEF-DATA PIC X(1).
                   07  HSCBRES-SENTEI-DEF-NAME PIC X(30).
