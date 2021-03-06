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
       01  FMMNOTEN-REC.
           03  FMMNOTEN-CONTEXT                     PIC S9(9)   BINARY.
           03  FMMNOTEN-OBJECT                      PIC X(36).
           03  FMMNOTEN-MODE                        PIC S9(9)   BINARY.
           03  FMMNOTEN-RECORDNAME                  PIC X(128).
           03  FMMNOTEN-DATA.
      *             [hc
               05  FMMNOTEN-FORM-ID                PIC X(30).
      *            JX^hc
               05  FMMNOTEN-CUSTOM-ID               PIC X(30).
      *            v^¼
               05  FMMNOTEN-PRINTER                 PIC X(20).
      *            ­sú YYYY-MM-DD
               05  FMMNOTEN-KOFYMD                  PIC X(10).
      *            aÔ
               05  FMMNOTEN-BTUNUM                  PIC X(2).
      *            a¼Ì
               05  FMMNOTEN-BTUNAME                 PIC X(20).
      *            aºÔ
               05  FMMNOTEN-BRMNUM                  PIC X(6).
      *            ³Òîñ
               05  FMMNOTEN-PTINF-AREA.
      *                ³ÒÔ
                   07  FMMNOTEN-PTNUM               PIC X(20).
      *                ¼
                   07  FMMNOTEN-NAME                PIC X(50).
      *                Ji¼
                   07  FMMNOTEN-KANANAME            PIC X(50).
      *                a¶ú
                   07  FMMNOTEN-BIRTHDAY            PIC X(10).
      *                «Êæª
                   07  FMMNOTEN-SEX                 PIC X(1).
      *            ãÃ@Öîñ
               05  FMMNOTEN-HOSP-AREA.
      *                §Ô
                   07  FMMNOTEN-PREFNUM             PIC X(2).
      *                ¼Ì
                   07  FMMNOTEN-HOSP-NAME-G.
                       09  FMMNOTEN-HOSP-NAME       PIC X(120)
                                                   OCCURS   3.
      *                Z
                   07  FMMNOTEN-HOSP-ADRS-G.
                       09  FMMNOTEN-HOSP-ADRS       PIC X(200)
                                                   OCCURS   3.
      *                dbÔ
                   07  FMMNOTEN-HOSP-TEL            PIC X(15).
               05  FMMNOTEN-DR-AREA.
      *                ãtR[h
                   07  FMMNOTEN-DR-CODE             PIC X(5).
      *                ãt¼
                   07  FMMNOTEN-DR-NAME             PIC X(50).
               05  FMMNOTEN-ZAI-AREA                OCCURS  50 TIMES.
      *                fÃæª
                   07  FMMNOTEN-SRYKBN              PIC X(2).
      *                Üñ
                   07  FMMNOTEN-ZAIKAISU            PIC X(3).
      *                à_Üæª
                   07  FMMNOTEN-NAIHENKBN           PIC X(01).
      *                ÜPÊ¼iñ^ñªj
                   07  FMMNOTEN-ZAITANINAME         PIC X(12).
                   07  FMMNOTEN-MEDICATION          OCCURS  50 TIMES.
      *                òÜ¼
                       09  FMMNOTEN-YKZNAME         PIC X(80).
      *                Ê(ZZZZ9.99999)
                       09  FMMNOTEN-SURYO           PIC X(12).
      *                PÊ¼
                       09  FMMNOTEN-TANINAME        PIC X(12).
      *                fÃR[h
                       09  FMMNOTEN-SRYCD           PIC X(9).
      *        õl
               05  FMMNOTEN-MEMO    PIC X(80)       OCCURS  10 TIMES.
      *        pqæ¾phc
               05  FMMNOTEN-QR-ID   PIC X(80).
