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
            02  XG03.
              03  XG03-B04-LABEL               PIC X(10).
              03  XG03-B06S-STATE              PIC S9(9)   BINARY.
              03  XG03-B07S-STATE              PIC S9(9)   BINARY.
              03  XG03-B09-STATE               PIC S9(9)   BINARY.
              03  XG03-B10-LABEL               PIC X(10).
              03  XG03-B11-STATE               PIC S9(9)   BINARY.
              03  XG03-B12-STATE               PIC S9(9)   BINARY.
              03  XG03-LBLMSG01-G.
                04  XG03-LBLMSG01-STYLE      PIC X(20).
                04  XG03-LBLMSG01            PIC X(100).
              03  XG03-LBLMSG02-G.
                04  XG03-LBLMSG02-STYLE      PIC X(20).
                04  XG03-LBLMSG02            PIC X(100).
              03  XG03-SELNUM                  PIC S9(3).
              03  XG03-PTNUM-STATE             PIC S9(9)   BINARY.
              03  XG03-PTNUM                   PIC X(20).
              03  XG03-PTNAME-G.
                04  XG03-PTNAME-STYLE          PIC X(20).
                04  XG03-PTNAME-VALUE          PIC X(50).
              03  XG03-CREYMD-STATE            PIC S9(9)   BINARY.
              03  XG03-CREYMD                  PIC X(10).
              03  XG03-SCROLLEDWINDOW26.
                04  XG03-LIST.
                  05  XG03-COUNT               PIC S9(9)   BINARY.
                  05  XG03-ROW                 PIC S9(9)   BINARY.
                  05  XG03-ROWATTR             PIC S9(9)   BINARY.
                  05  XG03-ITEM                OCCURS  200 TIMES.
                    06  XG03-TNO               PIC X(5).
                    06  XG03-TPTNUM            PIC X(20).
                    06  XG03-TPTNAME           PIC X(50).
                    06  XG03-TTITLE            PIC X(50).
                    06  XG03-TDATA-VERSION     PIC X(10).
                    06  XG03-TSRYYMD           PIC X(9).
                    06  XG03-TDRNAME           PIC X(50).
                    06  XG03-TLDNAME           PIC X(20).
                    06  XG03-TCREYMD           PIC X(9).
                    06  XG03-TCREHMS           PIC X(8).
                    06  XG03-TSTATUS           PIC X(6).
                  05  XG03-SELECT              PIC X     OCCURS  200.
              03  XG03-IPTNUM              PIC X(20).
              03  XG03-IPTNAME             PIC X(50).
              03  XG03-ITITLE              PIC X(50).
              03  XG03-IDATA-VERSION       PIC X(10).
              03  XG03-IDRNAME             PIC X(50).
              03  XG03-ISRYYMD             PIC X(9).
              03  XG03-ILDNAME             PIC X(20).
              03  XG03-ICREYMD             PIC X(9).
              03  XG03-ICREHMS             PIC X(8).
              03  XG03-IOPID               PIC X(16).
              03  XG03-ISTATUS             PIC X(6).
              03  XG03-CONTEXT             PIC X(50000).
              03  XG03-PANDATIMER1             PIC S9(9)   BINARY.
              03  XG03-PANDADOWNLOAD1.
                  05  XG03-OBJECTDATA          PIC X(36).
                  05  XG03-FILENAME            PIC X(1024).
