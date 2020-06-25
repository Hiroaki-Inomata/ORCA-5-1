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
      ******************************************************************
      *    ＡＰＩリクエスト入院登録情報 
      *    COPY (CPHSPTINFMV2REQ.INC)
      ******************************************************************
       01  XML-HSPTINFMREQ.
            03  HSPTINFMREQ-CONTEXT             PIC S9(9)   BINARY.
            03  HSPTINFMREQ-OBJECT              PIC X(36).
            03  HSPTINFMREQ-MODE                PIC S9(9)   BINARY.
            03  HSPTINFMREQ-RECORDNAME          PIC X(128).
            03  HSPTINFMREQ-PRIVATE-OBJECTS.
              05  HSPTINFMREQ-SAVEREQ           PIC X(1).
              05  HSPTINFMREQ-REQNUM            PIC X(2).
              05  HSPTINFMREQ-FORCE-UPDATE      PIC X(5).
              05  HSPTINFMREQ-PTNUM             PIC X(20).
              05  HSPTINFMREQ-NYUINYMD          PIC X(10).
              05  HSPTINFMREQ-IDOYMD            PIC X(10).
              05  HSPTINFMREQ-TAIINYMD          PIC X(10).
              05  HSPTINFMREQ-TAIINCD           PIC X(2).
              05  HSPTINFMREQ-BTUNUM            PIC X(2).
              05  HSPTINFMREQ-BRMNUM            PIC X(6).
              05  HSPTINFMREQ-SAGAKU            PIC X(7).
              05  HSPTINFMREQ-SENTEIKBN         PIC X(1).
              05  HSPTINFMREQ-SRYKA             PIC X(2).
              05  HSPTINFMREQ-SHOKAIYMD         PIC X(10).
              05  HSPTINFMREQ-KAIGOKBN          PIC X(1).
              05  HSPTINFMREQ-DR                PIC X(5)    OCCURS  3.
              05  HSPTINFMREQ-HKNINF-AREA.
                07  HSPTINFMREQ-HKNCOMBI-TEMPSET
                                                PIC X(1).
                07  HSPTINFMREQ-HKNCOMBI        PIC X(4).
                07  HSPTINFMREQ-HKNINF-REQUIRED.
                    09  HSPTINFMREQ-HKNNUM      PIC X(3).
                    09  HSPTINFMREQ-HKNJANUM    PIC X(8).
                    09  HSPTINFMREQ-HKNJANAME   PIC X(20).
                07  HSPTINFMREQ-HKIGO           PIC X(80).
                07  HSPTINFMREQ-HNUM            PIC X(80).
                07  HSPTINFMREQ-KEIZOKUKBN      PIC X(1).
                07  HSPTINFMREQ-HOJOKBN         PIC X(1).
                07  HSPTINFMREQ-HONKZKKBN       PIC X(1).
                07  HSPTINFMREQ-HIHKNJANAME     PIC X(100).
                07  HSPTINFMREQ-HTEKSTYMD       PIC X(10).
                07  HSPTINFMREQ-HTEKEDYMD       PIC X(10).
                07  HSPTINFMREQ-KOHINF-AREA.
                    09  HSPTINFMREQ-KOHINF-OCC  OCCURS  4.
                      11  HSPTINFMREQ-KOHINF-REQUIRED.
                          13  HSPTINFMREQ-KOHNUM   PIC X(3).
                          13  HSPTINFMREQ-KOHNAME  PIC X(20).
                          13  HSPTINFMREQ-FTNJANUM PIC X(8).
                          13  HSPTINFMREQ-JKYSNUM  PIC X(20).
                      11  HSPTINFMREQ-KTEKSTYMD PIC X(10).
                      11  HSPTINFMREQ-KTEKEDYMD PIC X(10).
              05  HSPTINFMREQ-AUTOSET           PIC X(1).
              05  HSPTINFMREQ-KHNCD             PIC X(9).
              05  HSPTINFMREQ-TOKHIGAITOKBN     PIC X(1).
              05  HSPTINFMREQ-NACCTKBN          PIC X(1).
              05  HSPTINFMREQ-BUNBENKBN         PIC X(1).
              05  HSPTINFMREQ-RIYOUKBN          PIC X(1).
              05  HSPTINFMREQ-KSNCD             PIC X(9) OCCURS 3.
              05  HSPTINFMREQ-TEIKIKBN          PIC X(1).
              05  HSPTINFMREQ-DISPKBN           PIC X(1).
