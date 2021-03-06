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
       01  FMMNOTE-REC.
           03  FMMNOTE-CONTEXT                     PIC S9(9)   BINARY.
           03  FMMNOTE-OBJECT                      PIC X(36).
           03  FMMNOTE-MODE                        PIC S9(9)   BINARY.
           03  FMMNOTE-RECORDNAME                  PIC X(128).
           03  FMMNOTE-DATA.
      *             [hc
               05  FMMNOTE-FORM-ID                PIC X(30).
      *            JX^hc
               05  FMMNOTE-CUSTOM-ID               PIC X(30).
      *            v^¼
               05  FMMNOTE-PRINTER                 PIC X(20).
      *            fÃú YYYY-MM-DD
               05  FMMNOTE-SRYYMD                  PIC X(10).
      *            `[Ô
               05  FMMNOTE-DENPNUM                 PIC X(7).
      *            ³Òîñ
               05  FMMNOTE-PTINF-AREA.
      *                ³ÒÔ
                   07  FMMNOTE-PTNUM               PIC X(20).
      *                ¼
                   07  FMMNOTE-NAME                PIC X(50).
      *                Ji¼
                   07  FMMNOTE-KANANAME            PIC X(50).
      *                a¶ú
                   07  FMMNOTE-BIRTHDAY            PIC X(10).
      *                «Êæª
                   07  FMMNOTE-SEX                 PIC X(1).
      *            ãÃ@Öîñ
               05  FMMNOTE-HOSP-AREA.
      *                §Ô
                   07  FMMNOTE-PREFNUM             PIC X(2).
      *                ¼Ì
                   07  FMMNOTE-HOSP-NAME-G.
                       09  FMMNOTE-HOSP-NAME       PIC X(120)
                                                   OCCURS   3.
      *                Z
                   07  FMMNOTE-HOSP-ADRS-G.
                       09  FMMNOTE-HOSP-ADRS       PIC X(200)
                                                   OCCURS   3.
      *                dbÔ
                   07  FMMNOTE-HOSP-TEL            PIC X(15).
               05  FMMNOTE-DR-AREA.
      *                ãtR[h
                   07  FMMNOTE-DR-CODE             PIC X(5).
      *                ãt¼
                   07  FMMNOTE-DR-NAME             PIC X(50).
               05  FMMNOTE-ZAI-AREA                OCCURS  50 TIMES.
      *                fÃæª
                   07  FMMNOTE-SRYKBN              PIC X(2).
      *                Üñ
                   07  FMMNOTE-ZAIKAISU            PIC X(3).
      *                à_Üæª
                   07  FMMNOTE-NAIHENKBN           PIC X(01).
      *                ÜPÊ¼iñ^ñªj
                   07  FMMNOTE-ZAITANINAME         PIC X(12).
                   07  FMMNOTE-MEDICATION          OCCURS  50 TIMES.
      *                òÜ¼
                       09  FMMNOTE-YKZNAME         PIC X(80).
      *                Ê(ZZZZ9.99999)
                       09  FMMNOTE-SURYO           PIC X(12).
      *                PÊ¼
                       09  FMMNOTE-TANINAME        PIC X(12).
      *                fÃR[h
                       09  FMMNOTE-SRYCD           PIC X(9).
      *        õl
               05  FMMNOTE-MEMO    PIC X(80)       OCCURS  10 TIMES.
      *        pqæ¾phc
               05  FMMNOTE-QR-ID   PIC X(80).

