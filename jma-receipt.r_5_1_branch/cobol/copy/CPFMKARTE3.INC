      *******************************************************************
      * Project code name "ORCA"
      * úãWZvg\tgiJMA standard receipt softwarej
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
        01  FMKARTE3-REC.
            03  FMKARTE3-CONTEXT               PIC S9(9)   BINARY.
            03  FMKARTE3-OBJECT                PIC X(36).
            03  FMKARTE3-MODE                  PIC S9(9)   BINARY.
            03  FMKARTE3-RECORDNAME            PIC X(128).
            03  FMKARTE3-DATA.
              05  FMKARTE3-FORM-ID             PIC X(30).
              05  FMKARTE3-CUSTOM-ID           PIC X(30).
              05  FMKARTE3-PRINTER             PIC X(20).
              05  FMKARTE3-PRINTER-ZOKUSHI     PIC X(20).
      *       1:ªóü/1ÈO:Êíóü
              05  FMKARTE3-HAKKOKBN            PIC X(1).
      *       ÅIóüÅ
              05  FMKARTE3-ENDPAGE             PIC X(3).
      *       ÅIóüs
              05  FMKARTE3-ENDLINE             PIC X(2).
      *       fÃN
              05  FMKARTE3-SRYYM               PIC X(7).
      *       fÃÈ
              05  FMKARTE3-SRYKA               PIC X(2).
      *       fÃÈ¼
              05  FMKARTE3-SRYKANAME           PIC X(10).
      *       Û¯Ì¼Ì
              05  FMKARTE3-HKNSYUBETU          PIC X(120).
              05  FMKARTE3-PTINF-AREA.
                07  FMKARTE3-PTNUM             PIC X(20).
                07  FMKARTE3-NAME              PIC X(50).
                07  FMKARTE3-KANANAME          PIC X(50).
                07  FMKARTE3-BIRTHDAY          PIC X(10).
                07  FMKARTE3-SEX               PIC X(1).
              05  FMKARTE3-BODY-AREA.
                07  FMKARTE3-BODY-OCC         OCCURS 400.
                  09  FMKARTE3-SRYYMD         PIC X(10).
                  09  FMKARTE3-NAIYO          PIC X(8)  OCCURS 10.
                  09  FMKARTE3-BIKOU          PIC X(8).
