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
        01  FMYYKPTLIST-REC.
            03  FMYPL-CONTEXT              PIC S9(9)   BINARY.
            03  FMYPL-OBJECT               PIC X(36).
            03  FMYPL-MODE                 PIC S9(9)   BINARY.
            03  FMYPL-RECORDNAME           PIC X(128).
            03  FMYPL-DATA.
      *       yoyaku_kanja_list
              05  FMYPL-FORM-ID          PIC X(30).
              05  FMYPL-CUSTOM-ID          PIC X(30).
              05  FMYPL-PRINTER            PIC X(20).
              05  FMYPL-PRTYMD             PIC X(10).
              05  FMYPL-SRYNAIYO-AREA.
                07  FMYPL-SRYNAIYO         PIC X(02).
                07  FMYPL-SRYNAIYONAME     PIC X(20).
              05  FMYPL-DOCTOR-AREA.
                07  FMYPL-DR-CODE          PIC X(5).
                07  FMYPL-DR-NAME          PIC X(50).
                07  FMYPL-DR-KANANAME      PIC X(50).
              05  FMYPL-TBL                OCCURS 6.
                07  FMYPL-TSRYYMD          PIC X(10).
                07  FMYPL-TBL2             OCCURS 1000.
                  09  FMYPL-TYYKTIME         PIC X(5).
                  09  FMYPL-TSRYKA           PIC X(2).
                  09  FMYPL-TSRYKANAME       PIC X(40).
                  09  FMYPL-TSRYNAIYO-AREA.
                    11  FMYPL-TSRYNAIYO      PIC X(02).
                    11  FMYPL-TSRYNAIYONAME  PIC X(20).
                  09  FMYPL-TDOCTOR-AREA.
                    11  FMYPL-TDR-CODE       PIC X(5).
                    11  FMYPL-TDR-NAME       PIC X(50).
                    11  FMYPL-TDR-KANANAME   PIC X(50).
                  09  FMYPL-TPTINF.
                    11  FMYPL-TPTNUM         PIC X(20).
                    11  FMYPL-TYYKNAME       PIC X(50).
                    11  FMYPL-TNAME          PIC X(50).
                    11  FMYPL-TKANANAME      PIC X(50).
                    11  FMYPL-TBIRTHDAY      PIC X(10).
                    11  FMYPL-TSEX           PIC X(1).
                  09  FMYPL-TYYKKBN          PIC X(02).
                  09  FMYPL-TYYKKBNNAME      PIC X(30).
                  09  FMYPL-TKAKREN          PIC X(02).
                  09  FMYPL-TKAKRENNAME      PIC X(20).
                  09  FMYPL-TRAIINZUMI       PIC X(1).
