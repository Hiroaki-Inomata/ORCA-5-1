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
      *    ＡＰＩレスポンス食事情報     COPY (CPHSMEALV2RES.INC)
      ******************************************************************
       01  XML-HSMEALRES.
           03  HSMEALRES-CONTEXT                     PIC S9(9)   BINARY.
           03  HSMEALRES-OBJECT                      PIC X(36).
           03  HSMEALRES-MODE                        PIC S9(9)   BINARY.
           03  HSMEALRES-RECORDNAME                  PIC X(128).
      *
           03  HSMEALRES-PRIVATE-OBJECTS.
               05  HSMEALRES-INFORMATION-DATE        PIC X(10).
               05  HSMEALRES-INFORMATION-TIME        PIC X(8).
               05  HSMEALRES-API-RESULT              PIC X(2).
               05  HSMEALRES-API-RESULT-MSG          PIC X(100).
               05  HSMEALRES-PTINF-AREA.
                   07  HSMEALRES-PTNUM               PIC X(20).
                   07  HSMEALRES-NAME                PIC X(100).
                   07  HSMEALRES-KANANAME            PIC X(100).
                   07  HSMEALRES-BIRTHDAY            PIC X(10).
                   07  HSMEALRES-SEX                 PIC X(01).
               05  HSMEALRES-NYUIN-TAIIN-YMD-AREA.
                   07  HSMEALRES-NYUIN-TAIIN-YMD-OCC OCCURS  5.
                       09  HSMEALRES-NYUINYMD        PIC X(10).
                       09  HSMEALRES-TAIINYMD        PIC X(10).
               05  HSMEALRES-SRYYM                   PIC X(07).
               05  HSMEALRES-CALENDAR-AREA.
                   07  HSMEALRES-CALENDAR-OCC        OCCURS  31.
                       09  HSMEALRES-SRYYMD              PIC X(10).
                       09  HSMEALRES-SRYKA-G.
                           11  HSMEALRES-SRYKA-LBL       PIC X(20).
                           11  HSMEALRES-SRYKA-DATA      PIC X(02).
                           11  HSMEALRES-SRYKA-NAME      PIC X(30).
                       09  HSMEALRES-BTUNUM-G.
                           11  HSMEALRES-BTUNUM-LBL      PIC X(20).
                           11  HSMEALRES-BTUNUM-DATA     PIC X(2).
                       09  HSMEALRES-BTUNAME-G.
                           11  HSMEALRES-BTUNAME-LBL     PIC X(20).
                           11  HSMEALRES-BTUNAME-DATA    PIC X(20).
                       09  HSMEALRES-BRMNUM-G.
                           11  HSMEALRES-BRMNUM-LBL      PIC X(10).
                           11  HSMEALRES-BRMNUM-DATA     PIC X(06).
                       09  HSMEALRES-HKNCOMBI            PIC X(04).
                       09  HSMEALRES-PTSTATUS-G.
                           11  HSMEALRES-PTSTATUS-LBL    PIC X(40).
                           11  HSMEALRES-PTSTATUS-DATA   PIC X(02).
                           11  HSMEALRES-PTSTATUS-NAME   PIC X(50).
                       09  HSMEALRES-SKJ-ASA-G.
                           11  HSMEALRES-SKJ-ASA-LBL     PIC X(20).
                           11  HSMEALRES-SKJ-ASA-DATA    PIC X(02).
                           11  HSMEALRES-SKJ-ASA-NAME    PIC X(30).
                       09  HSMEALRES-SKJ-HIRU-G.
                           11  HSMEALRES-SKJ-HIRU-LBL    PIC X(20).
                           11  HSMEALRES-SKJ-HIRU-DATA   PIC X(02).
                           11  HSMEALRES-SKJ-HIRU-NAME   PIC X(30).
                       09  HSMEALRES-SKJ-YU-G.
                           11  HSMEALRES-SKJ-YU-LBL      PIC X(20).
                           11  HSMEALRES-SKJ-YU-DATA     PIC X(02).
                           11  HSMEALRES-SKJ-YU-NAME     PIC X(30).
                       09  HSMEALRES-SAGAKU-G.
                           11  HSMEALRES-SAGAKU-LBL      PIC X(20).
                           11  HSMEALRES-SAGAKU-DATA     PIC X(07).
                           11  HSMEALRES-SAGAKU-NAME     PIC X(10).
                       09  HSMEALRES-NYUKHN-G.
                           11  HSMEALRES-NYUKHN-LBL      PIC X(20).
                           11  HSMEALRES-NYUKHN-OCC      OCCURS 20.
                               13  HSMEALRES-NYUKHN-DATA PIC X(09).
                               13  HSMEALRES-NYUKHN-NAME PIC X(50).
                       09  HSMEALRES-NYUKSN-G.
                           11  HSMEALRES-NYUKSN-OCC      OCCURS 20.
                               13  HSMEALRES-NYUKSN-LBL  PIC X(20).
                               13  HSMEALRES-NYUKSN-DATA PIC X(09).
                               13  HSMEALRES-NYUKSN-NAME PIC X(50).
               05  HSMEALRES-HKNCOMBI-INF-AREA.
                   07  HSMEALRES-HKNCOMBI-INF-OCC         OCCURS 10.
                       09  HSMEALRES-HKNCOMBI-KEY             PIC X(04).
                       09  HSMEALRES-HKNNUM                   PIC X(03).
                       09  HSMEALRES-HKNJANUM                 PIC X(08).
                       09  HSMEALRES-HKNJANAME                PIC X(20).
                       09  HSMEALRES-HKIGO                    PIC X(80).
                       09  HSMEALRES-HNUM                     PIC X(80).
                       09  HSMEALRES-KOHINF-AREA.
                           11  HSMEALRES-KOHINF-OCC           OCCURS 4.
                               13  HSMEALRES-KOHNUM           PIC X(03).
                               13  HSMEALRES-KOHNAME          PIC X(20).
                               13  HSMEALRES-FTNJANUM         PIC X(08).
                               13  HSMEALRES-JKYSNUM          PIC X(20).
