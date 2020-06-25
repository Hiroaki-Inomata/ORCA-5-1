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
       01  XML-HSFINDRES.
         02  HSFINDRES-CONTEXT                 PIC S9(9)   BINARY.
         02  HSFINDRES-OBJECT                  PIC X(36).
         02  HSFINDRES-MODE                    PIC S9(9)   BINARY.
         02  HSFINDRES-RECORDNAME              PIC X(128).
         02  HSFINDRES-HSFINDV2RES.
             03  HSFINDRES-INFORMATION-DATE    PIC X(10).
             03  HSFINDRES-INFORMATION-TIME    PIC X(08).
             03  HSFINDRES-API-RESULT          PIC X(05).
             03  HSFINDRES-API-RESULT-MSG      PIC X(100).
             03  HSFINDRES-COUNT               PIC X(7).
             03  HSFINDRES-RANGE-FROM          PIC X(7).
             03  HSFINDRES-RANGE-TO            PIC X(7).
             03  HSFINDRES-LAST-TEIKIYM        PIC X(7).
             03  HSFINDRES-LAST-KBT            PIC X(1).
             03  HSFINDRES-RESULTS-INFO        OCCURS 200.
                 05  HSFINDRES-SEQNUM          PIC X(7).
                 05  HSFINDRES-PTNUM           PIC X(20).
                 05  HSFINDRES-KANANAME        PIC X(100).
                 05  HSFINDRES-NAME            PIC X(100).
                 05  HSFINDRES-SEX             PIC X(1).
                 05  HSFINDRES-BIRTHDAY        PIC X(10).
                 05  HSFINDRES-BTUNUM          PIC X(2).
                 05  HSFINDRES-BTUNAME         PIC X(20).
                 05  HSFINDRES-BRMNUM          PIC X(06).
                 05  HSFINDRES-TEL             PIC X(15).
                 05  HSFINDRES-ZIPCODE         PIC X(07).
                 05  HSFINDRES-ADD1            PIC X(100).
                 05  HSFINDRES-ADD2            PIC X(100).
                 05  HSFINDRES-HKNCOMBI-G.
                     07  HSFINDRES-HKNCOMBI              PIC X(04).
                     07  HSFINDRES-HKNNUM                PIC X(03).
                     07  HSFINDRES-HKNJANUM              PIC X(08).
                     07  HSFINDRES-HKNNAME               PIC X(20).
                     07  HSFINDRES-CONTKBN               PIC X(01).
                     07  HSFINDRES-HOJOKBN               PIC X(01).
                     07  HSFINDRES-HONKZKKBN             PIC X(01).
                     07  HSFINDRES-HIHKNJANAME           PIC X(100).
                     07  HSFINDRES-TEKSTYMD              PIC X(10).
                     07  HSFINDRES-TEKEDYMD              PIC X(10).
                     07  HSFINDRES-KOH-G.
                         09  HSFINDRES-KOH-OCC           OCCURS 4.
                             11  HSFINDRES-KOHNUM        PIC X(03).
                             11  HSFINDRES-KOHNAME       PIC X(20).
                             11  HSFINDRES-FTNJANUM      PIC X(08).
                             11  HSFINDRES-JKYSNUM       PIC X(20).
                             11  HSFINDRES-KOH-TEKSTYMD  PIC X(10).
                             11  HSFINDRES-KOH-TEKEDYMD  PIC X(10).
                 05  HSFINDRES-NYUINYMD                  PIC X(10).
                 05  HSFINDRES-TAIINYMD                  PIC X(10).
                 05  HSFINDRES-TAIINCD                   PIC X(02).
                 05  HSFINDRES-TAIINNAME                 PIC X(20).
                 05  HSFINDRES-SRYKA                     PIC X(02).
                 05  HSFINDRES-SRYKANAME                 PIC X(30).
