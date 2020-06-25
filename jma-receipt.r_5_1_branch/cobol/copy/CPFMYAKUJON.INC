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
        01  FMYAKUJON-REC.
            03  FMYAKUJON-CONTEXT                PIC S9(9)   BINARY.
            03  FMYAKUJON-OBJECT                 PIC X(36).
            03  FMYAKUJON-MODE                   PIC S9(9)   BINARY.
            03  FMYAKUJON-RECORDNAME             PIC X(128).
            03  FMYAKUJON-DATA.
              05  FMYAKUJON-FORM-ID            PIC X(30).
              05  FMYAKUJON-CUSTOM-ID            PIC X(30).
              05  FMYAKUJON-PRINTER              PIC X(20).
              05  FMYAKUJON-STYLE                PIC X(1).
              05  FMYAKUJON-KOFYMD               PIC X(10).
              05  FMYAKUJON-BTUNUM               PIC X(2).
              05  FMYAKUJON-BTUNAME              PIC X(20).
              05  FMYAKUJON-BRMNUM               PIC X(6).
              05  FMYAKUJON-PTINF.
                07  FMYAKUJON-PTNUM              PIC X(20).
                07  FMYAKUJON-NAME               PIC X(50).
                07  FMYAKUJON-KANANAME           PIC X(50).
                07  FMYAKUJON-BIRTHDAY           PIC X(10).
                07  FMYAKUJON-SEX                PIC X(1).
              05  FMYAKUJON-HOSP-AREA.
                07  FMYAKUJON-HOSP-NAME-G.
                  09  FMYAKUJON-HOSP-NAME         PIC X(120)
                                                 OCCURS   3.
                07  FMYAKUJON-HOSP-POST           PIC X(7).
      *         住所
                07  FMYAKUJON-HOSP-ADRS-G.
                  09  FMYAKUJON-HOSP-ADRS         PIC X(200)
                                                 OCCURS   3.
                07  FMYAKUJON-HOSP-TEL            PIC X(15).
              05  FMYAKUJON-DOCTOR-AREA.
                07  FMYAKUJON-DR-CODE             PIC X(5).
                07  FMYAKUJON-DR-NAME             PIC X(50).
                07  FMYAKUJON-DR-KANANAME         PIC X(50).
              05  FMYAKUJON-SHURUISU              PIC X(3).
              05  FMYAKUJON-YAKUJO-AREA           OCCURS   99.
                07  FMYAKUJON-YKZNAME             PIC X(100).
                07  FMYAKUJON-SRYCD               PIC X(9).
                07  FMYAKUJON-IMAGE-ID            PIC X(36).
                07  FMYAKUJON-EFFECT              PIC X(1000).
                07  FMYAKUJON-SHAPE               PIC X(180).
                07  FMYAKUJON-COLOR               PIC X(180).
                07  FMYAKUJON-MARK                PIC X(180).
                07  FMYAKUJON-CAUTION             PIC X(2000).
                07  FMYAKUJON-YOHO                PIC X(100) OCCURS 30.
                07  FMYAKUJON-FUKUYOJITEN.
                    09  FMYAKUJON-KISHOJI         PIC X(3).
                    09  FMYAKUJON-KISHOJI-TANI    PIC X(2).
                    09  FMYAKUJON-ASA             PIC X(3).
                    09  FMYAKUJON-ASA-TANI        PIC X(2).
                    09  FMYAKUJON-HIRU            PIC X(3).
                    09  FMYAKUJON-HIRU-TANI       PIC X(2).
                    09  FMYAKUJON-YU              PIC X(3).
                    09  FMYAKUJON-YU-TANI         PIC X(2).
                    09  FMYAKUJON-SHUSHINJI       PIC X(3).
                    09  FMYAKUJON-SHUSHINJI-TANI  PIC X(2).
