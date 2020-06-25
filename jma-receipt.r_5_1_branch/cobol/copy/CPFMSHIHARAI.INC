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
        01  FMSHIHARAI-REC.
            03  FMSHIHARAI-PCONTEXT             PIC S9(9)   BINARY.
            03  FMSHIHARAI-OBJECT               PIC X(36).
            03  FMSHIHARAI-MODE                 PIC S9(9)   BINARY.
            03  FMSHIHARAI-RECORDNAME           PIC X(128).
            03  FMSHIHARAI-DATA.
              05  FMSHIHARAI-FORM-ID            PIC X(30).
              05  FMSHIHARAI-CUSTOM-ID          PIC X(30).
              05  FMSHIHARAI-PRINTER            PIC X(20).
              05  FMSHIHARAI-FORMKBN            PIC X(1).
              05  FMSHIHARAI-PRTKBN             PIC X(1).
              05  FMSHIHARAI-SUMKBN             PIC X(1).
              05  FMSHIHARAI-PTINF-AREA.
                07  FMSHIHARAI-PTNUM            PIC X(20).
                07  FMSHIHARAI-NAME             PIC X(50).
                07  FMSHIHARAI-KANANAME         PIC X(50).
                07  FMSHIHARAI-BIRTHDAY         PIC X(10).
                07  FMSHIHARAI-SEX              PIC X(1).
              05  FMSHIHARAI-HOSP-AREA.
                07  FMSHIHARAI-HOSP-NAME-G.
                  09  FMSHIHARAI-HOSP-NAME      PIC X(120)
                                                OCCURS   3.
                07  FMSHIHARAI-HOSP-POST        PIC X(7).
                07  FMSHIHARAI-HOSP-ADRS-G.
                  09  FMSHIHARAI-HOSP-ADRS      PIC X(200)
                                                OCCURS   3.
                07  FMSHIHARAI-HOSP-TEL         PIC X(15).
              05  FMSHIHARAI-JNISSU           PIC X(3).
              05  FMSHIHARAI-STRSRYYMD        PIC X(10).
              05  FMSHIHARAI-ENDSRYYMD        PIC X(10).
              05  FMSHIHARAI-PRTYMD           PIC X(10).
              05  FMSHIHARAI-NYUGAIKBN        PIC X(1).
              05  FMSHIHARAI-LIST-AREA.
                07  FMSHIHARAI-LIST           OCCURS 2000.
                  09  FMSHIHARAI-SRYYM        PIC X(7).
                  09  FMSHIHARAI-SRYSTYMD     PIC X(10).
                  09  FMSHIHARAI-SRYEDYMD     PIC X(10).
                  09  FMSHIHARAI-TENSU        PIC X(8).
                  09  FMSHIHARAI-HOKEN        PIC X(8).
                  09  FMSHIHARAI-SHOKUJI      PIC X(8).
                  09  FMSHIHARAI-JIHI         PIC X(8).
                  09  FMSHIHARAI-NYUKIN       PIC X(8).
                  09  FMSHIHARAI-ZENKAI-NYUKIN PIC X(8).
              05  FMSHIHARAI-TTL-TENSU        PIC X(9).
              05  FMSHIHARAI-TTL-HOKEN        PIC X(9).
              05  FMSHIHARAI-TTL-SHOKUJI      PIC X(9).
              05  FMSHIHARAI-TTL-JIHI         PIC X(9).
              05  FMSHIHARAI-TTL-NYUKIN       PIC X(9).
