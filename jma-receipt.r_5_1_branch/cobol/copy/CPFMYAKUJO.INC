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
        01  FMYAKUJO-REC.
            03  FMYAKUJO-CONTEXT                PIC S9(9)   BINARY.
            03  FMYAKUJO-OBJECT                 PIC X(36).
            03  FMYAKUJO-MODE                   PIC S9(9)   BINARY.
            03  FMYAKUJO-RECORDNAME             PIC X(128).
            03  FMYAKUJO-DATA.
              05  FMYAKUJO-FORM-ID            PIC X(30).
              05  FMYAKUJO-CUSTOM-ID            PIC X(30).
              05  FMYAKUJO-PRINTER              PIC X(20).
              05  FMYAKUJO-STYLE                PIC X(1).
      *       診療日
              05  FMYAKUJO-SRYYMD               PIC X(10).
      *       交付日
              05  FMYAKUJO-KOFYMD               PIC X(10).
              05  FMYAKUJO-PTINF.
                07  FMYAKUJO-PTNUM              PIC X(20).
                07  FMYAKUJO-NAME               PIC X(50).
                07  FMYAKUJO-KANANAME           PIC X(50).
                07  FMYAKUJO-BIRTHDAY           PIC X(10).
                07  FMYAKUJO-SEX                PIC X(1).
              05  FMYAKUJO-HOSP-AREA.
                07  FMYAKUJO-HOSP-NAME-G.
                  09  FMYAKUJO-HOSP-NAME         PIC X(120)
                                                 OCCURS   3.
                07  FMYAKUJO-HOSP-POST           PIC X(7).
      *         住所
                07  FMYAKUJO-HOSP-ADRS-G.
                  09  FMYAKUJO-HOSP-ADRS         PIC X(200)
                                                 OCCURS   3.
                07  FMYAKUJO-HOSP-TEL            PIC X(15).
              05  FMYAKUJO-DOCTOR-AREA.
                07  FMYAKUJO-DR-CODE             PIC X(5).
                07  FMYAKUJO-DR-NAME             PIC X(50).
                07  FMYAKUJO-DR-KANANAME         PIC X(50).
              05  FMYAKUJO-SHURUISU              PIC X(3).
              05  FMYAKUJO-YAKUJO-AREA           OCCURS   99.
                07  FMYAKUJO-YKZNAME             PIC X(100).
                07  FMYAKUJO-SRYCD               PIC X(9).
                07  FMYAKUJO-IMAGE-ID            PIC X(36).
                07  FMYAKUJO-EFFECT              PIC X(1000).
                07  FMYAKUJO-SHAPE               PIC X(180).
                07  FMYAKUJO-COLOR               PIC X(180).
                07  FMYAKUJO-MARK                PIC X(180).
                07  FMYAKUJO-CAUTION             PIC X(2000).
                07  FMYAKUJO-YOHO                PIC X(100) OCCURS 30.
                07  FMYAKUJO-FUKUYOJITEN.
                    09  FMYAKUJO-KISHOJI         PIC X(3).
                    09  FMYAKUJO-KISHOJI-TANI    PIC X(2).
                    09  FMYAKUJO-ASA             PIC X(3).
                    09  FMYAKUJO-ASA-TANI        PIC X(2).
                    09  FMYAKUJO-HIRU            PIC X(3).
                    09  FMYAKUJO-HIRU-TANI       PIC X(2).
                    09  FMYAKUJO-YU              PIC X(3).
                    09  FMYAKUJO-YU-TANI         PIC X(2).
                    09  FMYAKUJO-SHUSHINJI       PIC X(3).
                    09  FMYAKUJO-SHUSHINJI-TANI  PIC X(2).
