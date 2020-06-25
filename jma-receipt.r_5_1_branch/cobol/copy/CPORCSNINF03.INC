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
      *    食事・外泊等情報取得
      *    COPY (CPORCSNINF03.INC)
      ******************************************************************
       01  SNINF03-AREA.
           03  SNINF03-IN-SRYYM                    PIC X(06).
      *
           03  SNINF03-RESPONSE-AREA.
               05  SNINF03-PTINF-AREA.
                   07  SNINF03-PTNUM               PIC X(20).
                   07  SNINF03-NAME                PIC X(100).
                   07  SNINF03-KANANAME            PIC X(100).
                   07  SNINF03-BIRTHDAY            PIC X(10).
                   07  SNINF03-SEX                 PIC X(01).
               05  SNINF03-NYUIN-TAIIN-YMD-AREA.
                   07  SNINF03-NYUIN-TAIIN-YMD-OCC OCCURS  5.
                       09  SNINF03-NYUINYMD        PIC X(10).
                       09  SNINF03-TAIINYMD        PIC X(10).
               05  SNINF03-SRYYM                   PIC X(07).
               05  SNINF03-CALENDAR-AREA.
                   07  SNINF03-CALENDAR-OCC        OCCURS  31.
                       09  SNINF03-SRYYMD              PIC X(10).
                       09  SNINF03-SRYKA-G.
                           11  SNINF03-SRYKA-LBL       PIC X(20).
                           11  SNINF03-SRYKA-DATA      PIC X(02).
                           11  SNINF03-SRYKA-NAME      PIC X(30).
                       09  SNINF03-BTUNUM-G.
                           11  SNINF03-BTUNUM-LBL      PIC X(20).
                           11  SNINF03-BTUNUM-DATA     PIC X(2).
                       09  SNINF03-BTUNAME-G.
                           11  SNINF03-BTUNAME-LBL     PIC X(20).
                           11  SNINF03-BTUNAME-DATA    PIC X(20).
                       09  SNINF03-BRMNUM-G.
                           11  SNINF03-BRMNUM-LBL      PIC X(10).
                           11  SNINF03-BRMNUM-DATA     PIC X(06).
                       09  SNINF03-HKNCOMBI            PIC X(04).
                       09  SNINF03-PTSTATUS-G.
                           11  SNINF03-PTSTATUS-LBL    PIC X(40).
                           11  SNINF03-PTSTATUS-DATA   PIC X(02).
                           11  SNINF03-PTSTATUS-NAME   PIC X(50).
                       09  SNINF03-SKJ-ASA-G.
                           11  SNINF03-SKJ-ASA-LBL     PIC X(20).
                           11  SNINF03-SKJ-ASA-DATA    PIC X(02).
                           11  SNINF03-SKJ-ASA-NAME    PIC X(30).
                       09  SNINF03-SKJ-HIRU-G.
                           11  SNINF03-SKJ-HIRU-LBL    PIC X(20).
                           11  SNINF03-SKJ-HIRU-DATA   PIC X(02).
                           11  SNINF03-SKJ-HIRU-NAME   PIC X(30).
                       09  SNINF03-SKJ-YU-G.
                           11  SNINF03-SKJ-YU-LBL      PIC X(20).
                           11  SNINF03-SKJ-YU-DATA     PIC X(02).
                           11  SNINF03-SKJ-YU-NAME     PIC X(30).
                       09  SNINF03-SAGAKU-G.
                           11  SNINF03-SAGAKU-LBL      PIC X(20).
                           11  SNINF03-SAGAKU-DATA     PIC X(07).
                           11  SNINF03-SAGAKU-NAME     PIC X(10).
                       09  SNINF03-NYUKHN-G.
                           11  SNINF03-NYUKHN-LBL      PIC X(20).
                           11  SNINF03-NYUKHN-OCC      OCCURS 20.
                               13  SNINF03-NYUKHN-DATA PIC X(09).
                               13  SNINF03-NYUKHN-NAME PIC X(50).
                       09  SNINF03-NYUKSN-G.
                           11  SNINF03-NYUKSN-OCC      OCCURS 20.
                               13  SNINF03-NYUKSN-LBL  PIC X(20).
                               13  SNINF03-NYUKSN-DATA PIC X(09).
                               13  SNINF03-NYUKSN-NAME PIC X(50).
               05  SNINF03-HKNCOMBI-INF-AREA.
                   07  SNINF03-HKNCOMBI-INF-OCC         OCCURS 10.
                       09  SNINF03-HKNCOMBI-KEY             PIC X(04).
                       09  SNINF03-HKNNUM                   PIC X(03).
                       09  SNINF03-HKNJANUM                 PIC X(08).
                       09  SNINF03-HKNJANAME                PIC X(20).
                       09  SNINF03-HKIGO                    PIC X(80).
                       09  SNINF03-HNUM                     PIC X(80).
                       09  SNINF03-KOHINF-AREA.
                           11  SNINF03-KOHINF-OCC           OCCURS 4.
                               13  SNINF03-KOHNUM           PIC X(03).
                               13  SNINF03-KOHNAME          PIC X(20).
                               13  SNINF03-FTNJANUM         PIC X(08).
                               13  SNINF03-JKYSNUM          PIC X(20).
