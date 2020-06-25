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
        01  FMYYKLIST-REC.
            03  FMYL-CONTEXT                   PIC S9(9)   BINARY.
            03  FMYL-OBJECT                    PIC X(36).
            03  FMYL-MODE                      PIC S9(9)   BINARY.
            03  FMYL-RECORDNAME                PIC X(128).
            03  FMYL-DATA.
      *       yoyaku_list
              05  FMYL-FORM-ID               PIC X(30).
              05  FMYL-CUSTOM-ID               PIC X(30).
              05  FMYL-PRINTER                 PIC X(20).
              05  FMYL-SRYYMD                  PIC X(10).
              05  FMYL-PRTYMD                  PIC X(10).
      *       処理区分（１：予約一覧、２：未院一覧）
              05  FMYL-HAKKOKBN                PIC X(01).
              05  FMYL-SRYNAIYO-AREA.
                07  FMYL-SRYNAIYO              PIC X(02).
                07  FMYL-SRYNAIYONAME          PIC X(20).
              05  FMYL-DOCTOR-AREA.
                07  FMYL-DR-CODE               PIC X(5).
                07  FMYL-DR-NAME               PIC X(50).
                07  FMYL-DR-KANANAME           PIC X(50).
              05  FMYL-TBL                     OCCURS 1000.
                07  FMYL-TYYKTIME              PIC X(5).
                07  FMYL-TSRYKA                PIC X(2).
                07  FMYL-TSRYKANAME            PIC X(40).
                07  FMYL-TSRYNAIYO-AREA.
                  09  FMYL-TSRYNAIYO           PIC X(02).
                  09  FMYL-TSRYNAIYONAME       PIC X(20).
                07  FMYL-TDOCTOR-AREA.
                  09  FMYL-TDR-CODE            PIC X(5).
                  09  FMYL-TDR-NAME            PIC X(50).
                  09  FMYL-TDR-KANANAME        PIC X(50).
                07  FMYL-TPTINF.
                  09  FMYL-TPTNUM              PIC X(20).
      *           予約患者氏名
                  09  FMYL-TYYKNAME            PIC X(50).
      *           患者情報
                  09  FMYL-TNAME               PIC X(50).
                  09  FMYL-TKANANAME           PIC X(50).
                  09  FMYL-TBIRTHDAY           PIC X(10).
                  09  FMYL-TSEX                PIC X(1).
                07  FMYL-TYYKKBN               PIC X(02).
                07  FMYL-TYYKKBNNAME           PIC X(30).
                07  FMYL-TKAKREN               PIC X(02).
                07  FMYL-TKAKRENNAME           PIC X(20).
                07  FMYL-TRAIINZUMI            PIC X(1).
