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
       01  XML-FINDV3RES.
         02  FINDRES-CONTEXT                 PIC S9(9)   BINARY.
         02  FINDRES-OBJECT                  PIC X(36).
         02  FINDRES-MODE                    PIC S9(9)   BINARY.
         02  FINDRES-RECORDNAME              PIC X(128).
         02  FINDRES-FINDV2RES.
             03  FINDRES-INFORMATION-DATE    PIC X(10).
             03  FINDRES-INFORMATION-TIME    PIC X(08).
             03  FINDRES-API-RESULT          PIC X(05).
             03  FINDRES-API-RESULT-MSG      PIC X(100).
             03  FINDRES-REQNUM              PIC X(02).
             03  FINDRES-RESNUM              PIC X(02).
             03  FINDRES-KARTE-UID           PIC X(36).
             03  FINDRES-ORCA-UID            PIC X(36).
             03  FINDRES-COUNT               PIC X(7).
             03  FINDRES-RANGE-FROM          PIC X(7).
             03  FINDRES-RANGE-TO            PIC X(7).
             03  FINDRES-RESULTS-INFO        OCCURS 200.
                 05  FINDRES-SEQNUM          PIC X(7).
                 05  FINDRES-PTNUM           PIC X(20).
                 05  FINDRES-KANANAME        PIC X(100).
                 05  FINDRES-NAME            PIC X(100).
                 05  FINDRES-SEX             PIC X(1).
                 05  FINDRES-BIRTHDAY        PIC X(10).
                 05  FINDRES-BTUNUM          PIC X(2).
                 05  FINDRES-BTUNAME         PIC X(20).
                 05  FINDRES-BRMNUM          PIC X(06).
                 05  FINDRES-LASTYMD         PIC X(10).
                 05  FINDRES-TEL             PIC X(15).
                 05  FINDRES-ZIPCODE         PIC X(07).
                 05  FINDRES-ADD1            PIC X(100).
                 05  FINDRES-ADD2            PIC X(100).
                 05  FINDRES-NYUGAIKBN       PIC X(01).
                 05  FINDRES-PATIENT-ID1     PIC X(20).
                 05  FINDRES-HKNCOMBI-G.
                     07  FINDRES-HKNNUM                PIC X(03).
                     07  FINDRES-HKNJANUM              PIC X(08).
                     07  FINDRES-HKNNAME               PIC X(20).
                     07  FINDRES-CONTKBN               PIC X(01).
                     07  FINDRES-HOJOKBN               PIC X(01).
                     07  FINDRES-HONKZKKBN             PIC X(01).
                     07  FINDRES-HIHKNJANAME           PIC X(100).
                     07  FINDRES-TEKSTYMD              PIC X(10).
                     07  FINDRES-TEKEDYMD              PIC X(10).
                     07  FINDRES-KOH-G.
                         09  FINDRES-KOH-OCC           OCCURS 4.
                             11  FINDRES-KOHNUM        PIC X(03).
                             11  FINDRES-KOHNAME       PIC X(20).
                             11  FINDRES-FTNJANUM      PIC X(08).
                             11  FINDRES-JKYSNUM       PIC X(20).
                             11  FINDRES-KOH-TEKSTYMD  PIC X(10).
                             11  FINDRES-KOH-TEKEDYMD  PIC X(10).
                     07  FINDRES-TOKKI-OCC             OCCURS 5.
                         09  FINDRES-TOKKI             PIC X(02).
                         09  FINDRES-TOKKINM           PIC X(12).
                 05  FINDRES-TOTALTEN                  PIC X(7).
                 05  FINDRES-SRYACT-AREA.
                     07  FINDRES-SRYCD                 PIC X(9).
                     07  FINDRES-SRYNAME               PIC X(100).
                     07  FINDRES-KAISUTOTAL            PIC X(07).
                     07  FINDRES-SURYOTOTAL            PIC X(13).
                     07  FINDRES-TANICD                PIC X(3).
                     07  FINDRES-TANINAME              PIC X(12).
                     07  FINDRES-SRYACT-OCC            OCCURS 500.
                         09  FINDRES-SRYYMD            PIC X(10).
                         09  FINDRES-SRYSURYO          PIC X(11).
                         09  FINDRES-ZAIKAISU          PIC X(3).
