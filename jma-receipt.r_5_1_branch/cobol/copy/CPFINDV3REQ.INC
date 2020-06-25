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
       01  XML-FINDV3REQ.
         02  FINDREQ-CONTEXT                 PIC S9(9)   BINARY.
         02  FINDREQ-OBJECT                  PIC X(36).
         02  FINDREQ-MODE                    PIC S9(9)   BINARY.
         02  FINDREQ-RECORDNAME              PIC X(128).
         02  FINDREQ-FINDV2REQ.
           03  FINDREQ-KIHON-AREA.
             04  FINDREQ-REQNUM              PIC X(2).
             04  FINDREQ-KARTE-UID           PIC X(36).
             04  FINDREQ-ORCA-UID            PIC X(36).
             04  FINDREQ-RANGE-FROM          PIC X(7).
             04  FINDREQ-RANGE-TO            PIC X(7).
             04  FINDREQ-OUTKBN              PIC X(1).
             04  FINDREQ-STSRYYMD            PIC X(10).
             04  FINDREQ-EDSRYYMD            PIC X(10).
             04  FINDREQ-STBIRTHDAY          PIC X(10).
             04  FINDREQ-EDBIRTHDAY          PIC X(10).
             04  FINDREQ-BIRTHMON            PIC X(2).
             04  FINDREQ-SEX                 PIC X(1).
             04  FINDREQ-STLASTYMD           PIC X(10).
             04  FINDREQ-EDLASTYMD           PIC X(10).
             04  FINDREQ-DRCD                PIC X(5).
             04  FINDREQ-NYUGAIKBN           PIC X(1).
             04  FINDREQ-SRYKA               PIC X(2).
             04  FINDREQ-DEATH               PIC X(1).
             04  FINDREQ-STPTNUM             PIC X(20).
             04  FINDREQ-EDPTNUM             PIC X(20).
             04  FINDREQ-AGREEMENT           PIC X(1).
             04  FINDREQ-TSTPT               PIC X(1).
             04  FINDREQ-NAME                PIC X(100)  OCCURS  5.
             04  FINDREQ-JYUSHO              PIC X(100)  OCCURS  5.
             04  FINDREQ-TEL-TO              PIC X(1).
             04  FINDREQ-TEL                 PIC X(15)   OCCURS  3.
             04  FINDREQ-CONDITION1          PIC X(2)    OCCURS  3.
             04  FINDREQ-CONDITION2          PIC X(2)    OCCURS  3.
             04  FINDREQ-CONDITION3          PIC X(2)    OCCURS  3.
             04  FINDREQ-SELCOMMENT          PIC X(1).
             04  FINDREQ-COMMENT             PIC X(40)   OCCURS  3.
             04  FINDREQ-GENMEN              PIC X(2)    OCCURS  3.
             04  FINDREQ-TSY                 PIC X(1).
             04  FINDREQ-STTSYSKBT           PIC X(1).
             04  FINDREQ-TSYYM               PIC X(7).
             04  FINDREQ-STTSYYMD            PIC X(10).
             04  FINDREQ-EDTSYYMD            PIC X(10).
             04  FINDREQ-TOKKICD             PIC X(2).
             04  FINDREQ-TOKKINAME           PIC X(12).
             04  FINDREQ-STTOTALTEN          PIC X(7).
             04  FINDREQ-EDTOTALTEN          PIC X(7).
             04  FINDREQ-SAIGAIKBN           PIC X(1).
             04  FINDREQ-TENKENRECE          PIC X(1).
           03  FINDREQ-HOKEN-KOHI-AREA.
             04  FINDREQ-HKNEDYM             PIC X(07).
             04  FINDREQ-HONKZKKBN           PIC X(1).
             04  FINDREQ-FTNRATE             PIC X(1).
             04  FINDREQ-JKNHKNJANUM         PIC X(1).
             04  FINDREQ-HKNJANUM            PIC X(8)    OCCURS  5.
             04  FINDREQ-JKNHKNNUM           PIC X(1).
             04  FINDREQ-HKNNUM              PIC X(3)    OCCURS  5.
             04  FINDREQ-KIGO                PIC X(40)   OCCURS  5.
             04  FINDREQ-NUM                 PIC X(40)   OCCURS  5.
             04  FINDREQ-KOHEDYM             PIC X(07).
             04  FINDREQ-JKNKOHNUM           PIC X(1).
             04  FINDREQ-KOHNUM              PIC X(3)    OCCURS  5.
             04  FINDREQ-FTNJANUM            PIC X(8)    OCCURS  5.
             04  FINDREQ-JKNHKNKOH           PIC X(1).
           03  FINDREQ-BYOMEI-AREA.
             04  FINDREQ-STBYOYMD            PIC X(10).
             04  FINDREQ-EDBYOYMD            PIC X(10).
             04  FINDREQ-TENKIKBN            PIC X(1).
             04  FINDREQ-STTENKIYMD          PIC X(10).
             04  FINDREQ-EDTENKIYMD          PIC X(10).
             04  FINDREQ-UTAGAIKBN           PIC X(1).
             04  FINDREQ-KYUSEIKBN           PIC X(1).
             04  FINDREQ-SYUBYOKBN           PIC X(1).
             04  FINDREQ-JKNBYOMEI           PIC X(1).
             04  FINDREQ-BYOMEI-OCC          OCCURS  5.
               05  FINDREQ-BYOMEI            PIC X(80).
           03  FINDREQ-SRYACT-AREA.
             04  FINDREQ-SRYKBN              PIC X(2).
             04  FINDREQ-SRYSYUKBN           PIC X(2).
             04  FINDREQ-HOKATSUKBN          PIC X(1).
             04  FINDREQ-SRYVALITEM          PIC X(1).
             04  FINDREQ-JKNSRYACT           PIC X(1).
             04  FINDREQ-SRYACT-OCC          OCCURS  5.
               05   FINDREQ-SRYCD            PIC X(20).
               05   FINDREQ-SRYCD-NOTKBN     PIC X(1).
               05   FINDREQ-SRYVAL           PIC X(11).
               05   FINDREQ-SRYVALCOMP       PIC X(1).
             04  FINDREQ-RECECOM             OCCURS  3
                                           PIC X(50).
           03  FINDREQ-KAIGO-AREA.
             04  FINDREQ-KHSKBT              PIC X(01).
             04  FINDREQ-KHYM                PIC X(10).
             04  FINDREQ-STKHYMD             PIC X(10).
             04  FINDREQ-EDKHYMD             PIC X(10).
             04  FINDREQ-KHHKNJANUM          PIC X(06).
             04  FINDREQ-KNSKBT              PIC X(01).
             04  FINDREQ-KNYM                PIC X(10).
             04  FINDREQ-STKNYMD             PIC X(10).
             04  FINDREQ-EDKNYMD             PIC X(10).
             04  FINDREQ-KNJOTAI             PIC X(2).
           03  FINDREQ-SORT-AREA.
             04  FINDREQ-SORT-KEY            OCCURS  3.
               05  FINDREQ-SORT-KEYID        PIC X(1).
               05  FINDREQ-SORT-ORDER        PIC X(1).
