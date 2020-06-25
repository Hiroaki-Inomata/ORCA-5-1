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
        01  FMKARTE3N-REC.
            03  FMKARTE3N-CONTEXT               PIC S9(9)   BINARY.
            03  FMKARTE3N-OBJECT                PIC X(36).
            03  FMKARTE3N-MODE                  PIC S9(9)   BINARY.
            03  FMKARTE3N-RECORDNAME            PIC X(128).
            03  FMKARTE3N-DATA.
              05  FMKARTE3N-FORM-ID             PIC X(30).
              05  FMKARTE3N-CUSTOM-ID           PIC X(30).
              05  FMKARTE3N-PRINTER             PIC X(20).
              05  FMKARTE3N-PRINTER-ZOKUSHI     PIC X(20).
      *       1:頭書印刷/1以外:通常印刷
              05  FMKARTE3N-HAKKOKBN            PIC X(1).
      *       最終印刷頁
              05  FMKARTE3N-ENDPAGE             PIC X(3).
      *       最終印刷行
              05  FMKARTE3N-ENDLINE             PIC X(2).
      *       診療年月
              05  FMKARTE3N-SRYYM               PIC X(7).
      *       診療科
              05  FMKARTE3N-SRYKA               PIC X(2).
      *       診療科名
              05  FMKARTE3N-SRYKANAME           PIC X(10).
      *       保険の名称
              05  FMKARTE3N-HKNSYUBETU          PIC X(120).
              05  FMKARTE3N-PTINF-AREA.
                07  FMKARTE3N-PTNUM             PIC X(20).
                07  FMKARTE3N-NAME              PIC X(50).
                07  FMKARTE3N-KANANAME          PIC X(50).
                07  FMKARTE3N-BIRTHDAY          PIC X(10).
                07  FMKARTE3N-SEX               PIC X(1).
              05  FMKARTE3N-BODY-AREA.
                07  FMKARTE3N-BODY-OCC         OCCURS 400.
                  09  FMKARTE3N-SRYYMD         PIC X(10).
                  09  FMKARTE3N-NAIYO          PIC X(8)  OCCURS 11.
                  09  FMKARTE3N-BIKOU          PIC X(8).
