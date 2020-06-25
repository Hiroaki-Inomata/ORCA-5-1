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
        01  FMYYKHYO-REC.
            03  FMYH-CONTEXT                PIC S9(9)   BINARY.
            03  FMYH-OBJECT                 PIC X(36).
            03  FMYH-MODE                   PIC S9(9)   BINARY.
            03  FMYH-RECORDNAME             PIC X(128).
            03  FMYH-DATA.
      *     yoyaku_hyo
              05  FMYH-FORM-ID            PIC X(30).
              05  FMYH-CUSTOM-ID            PIC X(30).
              05  FMYH-PRINTER              PIC X(20).
              05  FMYH-KOFYMD               PIC X(10).
              05  FMYH-PTINF.
                07  FMYH-PTNUM              PIC X(20).
                07  FMYH-NAME               PIC X(50).
                07  FMYH-KANANAME           PIC X(50).
                07  FMYH-BIRTHDAY           PIC X(10).
                07  FMYH-SEX                PIC X(1).
              05  FMYH-HOSP-AREA.
                07  FMYH-HOSP-NAME-G.
                  09  FMYH-HOSP-NAME        PIC X(120)
                                            OCCURS   3.
                07  FMYH-HOSP-POST          PIC X(7).
                07  FMYH-HOSP-ADRS-G.
                  09  FMYH-HOSP-ADRS        PIC X(200)
                                            OCCURS   3.
                07  FMYH-HOSP-TEL           PIC X(15).
              05  FMYH-MEMO                 PIC X(100).
              05  FMYH-TBL                  OCCURS 50.
                07  FMYH-TYYKYMD            PIC X(10).
      *         ex."12:59"
                07  FMYH-TYYKTIME           PIC X(05).
                07  FMYH-TSRYKA             PIC X(02).
                07  FMYH-TSRYKANAME         PIC X(40).
                07  FMYH-TDOCTOR-AREA.
                  09  FMYH-TDR-CODE         PIC X(5).
                  09  FMYH-TDR-NAME         PIC X(50).
                  09  FMYH-TDR-KANANAME     PIC X(50).
                07  FMYH-TCOMMENT           PIC X(80) OCCURS 6.
