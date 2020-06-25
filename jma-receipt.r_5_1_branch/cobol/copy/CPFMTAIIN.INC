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
        01  FMTAIIN-REC.
            03  FMTAIIN-PCONTEXT               PIC S9(9)   BINARY.
            03  FMTAIIN-OBJECT                 PIC X(36).
            03  FMTAIIN-MODE                   PIC S9(9)   BINARY.
            03  FMTAIIN-RECORDNAME             PIC X(128).
            03  FMTAIIN-DATA.
              05  FMTAIIN-FORM-ID              PIC X(30).
              05  FMTAIIN-CUSTOM-ID            PIC X(30).
              05  FMTAIIN-PRINTER              PIC X(20).
              05  FMTAIIN-PTINF-AREA.
                07  FMTAIIN-PTNUM              PIC X(20).
                07  FMTAIIN-NAME               PIC X(50).
                07  FMTAIIN-KANANAME           PIC X(50).
                07  FMTAIIN-BIRTHDAY           PIC X(10).
                07  FMTAIIN-AGE                PIC X(3).
                07  FMTAIIN-SEX                PIC X(1).
                07  FMTAIIN-HOME-ADRS          PIC X(100)  OCCURS  2.
                07  FMTAIIN-HOME-POST          PIC X(7).
                07  FMTAIIN-HOME-TEL           PIC X(15).
              05  FMTAIIN-HOSP-AREA.
                07  FMTAIIN-HOSP-NAME-G.
                  09  FMTAIIN-HOSP-NAME        PIC X(120)
                                               OCCURS   3.
                07  FMTAIIN-HOSP-POST          PIC X(7).
                07  FMTAIIN-HOSP-ADRS-G.
                  09  FMTAIIN-HOSP-ADRS        PIC X(200)
                                               OCCURS   3.
                07  FMTAIIN-HOSP-TEL           PIC X(15).
              05  FMTAIIN-DOCTOR-AREA.
                07  FMTAIIN-DR-CODE            PIC X(5).
                07  FMTAIIN-DR-NAME            PIC X(50).
                07  FMTAIIN-DR-KANANAME        PIC X(50).
              05  FMTAIIN-PRTYMD               PIC X(10).
              05  FMTAIIN-NYUINYMD             PIC X(10).
              05  FMTAIIN-TAIINYMD             PIC X(10).
              05  FMTAIIN-NYUINRYO-INF-AREA.
                07  FMTAIIN-NYUINRYO-AREA.
                  09  FMTAIIN-NYUINRYO-OCC       OCCURS   3.
                    11  FMTAIIN-NYUINRYO-NAME    PIC X(50).
                    11  FMTAIIN-NYUINRYO-NISSU   PIC X(5).
                07  FMTAIIN-OTHER-NISSU          PIC X(5).
                07  FMTAIIN-SANTEI-KIKAN-AREA.
                  09  FMTAIIN-TOTAL-NISSU        PIC X(5).
                  09  FMTAIIN-SNTSTYMD           PIC X(10).
                  09  FMTAIIN-SNTEDYMD           PIC X(10).
              05  FMTAIIN-IPPAN-AREA.
                07  FMTAIIN-IPPAN-NISSU        PIC X(5).
                07  FMTAIIN-BASEYMD            PIC X(10).
              05  FMTAIIN-BYOMEI               PIC X(160).
