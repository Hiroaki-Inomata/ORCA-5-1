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
      *    ＡＰＩレスポンス外泊等情報
      *    COPY (CPHSACCTMV2RES.INC)
      ******************************************************************
       01  XML-HSACCTMRES.
           03  HSACCTMRES-CONTEXT              PIC S9(9)   BINARY.
           03  HSACCTMRES-OBJECT               PIC X(36).
           03  HSACCTMRES-MODE                 PIC S9(9)   BINARY.
           03  HSACCTMRES-RECORDNAME           PIC X(128).
           03  HSACCTMRES-PRIVATE-OBJECTS.
               05  HSACCTMRES-INFORMATION-DATE     PIC X(10).
               05  HSACCTMRES-INFORMATION-TIME     PIC X(8).
               05  HSACCTMRES-API-RESULT-OCC       OCCURS  10.
                   07  HSACCTMRES-API-RESULT       PIC X(4).
                   07  HSACCTMRES-API-RESULT-MSG   PIC X(100).
               05  HSACCTMRES-REQNUM-G.
                   07  HSACCTMRES-REQNUM-LBL       PIC X(20).
                   07  HSACCTMRES-REQNUM-DATA      PIC X(02).
                   07  HSACCTMRES-REQNUM-NAME      PIC X(30).
               05  HSACCTMRES-PTINF-AREA.
                   07  HSACCTMRES-PTNUM               PIC X(20).
                   07  HSACCTMRES-NAME                PIC X(100).
                   07  HSACCTMRES-KANANAME            PIC X(100).
                   07  HSACCTMRES-BIRTHDAY            PIC X(10).
                   07  HSACCTMRES-SEX                 PIC X(01).
               05  HSACCTMRES-NYUIN-TAIIN-YMD-AREA.
                   07  HSACCTMRES-NYUIN-TAIIN-YMD-OCC OCCURS  5.
                       09  HSACCTMRES-NYUINYMD        PIC X(10).
                       09  HSACCTMRES-TAIINYMD        PIC X(10).
               05  HSACCTMRES-SRYYM                   PIC X(07).
               05  HSACCTMRES-CALENDAR-AREA.
                   07  HSACCTMRES-CALENDAR-OCC        OCCURS  31.
                       09  HSACCTMRES-SRYYMD          PIC X(10).
                       09  HSACCTMRES-SRYKA-G.
                           11  HSACCTMRES-SRYKA-LBL       PIC X(20).
                           11  HSACCTMRES-SRYKA-DATA      PIC X(02).
                           11  HSACCTMRES-SRYKA-NAME      PIC X(30).
                       09  HSACCTMRES-BTUNUM-G.
                           11  HSACCTMRES-BTUNUM-LBL      PIC X(20).
                           11  HSACCTMRES-BTUNUM-DATA     PIC X(2).
                       09  HSACCTMRES-BTUNAME-G.
                           11  HSACCTMRES-BTUNAME-LBL     PIC X(20).
                           11  HSACCTMRES-BTUNAME-DATA    PIC X(20).
                       09  HSACCTMRES-BRMNUM-G.
                           11  HSACCTMRES-BRMNUM-LBL      PIC X(10).
                           11  HSACCTMRES-BRMNUM-DATA     PIC X(06).
                       09  HSACCTMRES-HKNCOMBI            PIC X(04).
                       09  HSACCTMRES-PTSTATUS-G.
                           11  HSACCTMRES-PTSTATUS-LBL    PIC X(40).
                           11  HSACCTMRES-PTSTATUS-DATA   PIC X(02).
                           11  HSACCTMRES-PTSTATUS-NAME   PIC X(50).
                       09  HSACCTMRES-SKJ-ASA-G.
                           11  HSACCTMRES-SKJ-ASA-LBL     PIC X(20).
                           11  HSACCTMRES-SKJ-ASA-DATA    PIC X(02).
                           11  HSACCTMRES-SKJ-ASA-NAME    PIC X(30).
                       09  HSACCTMRES-SKJ-HIRU-G.
                           11  HSACCTMRES-SKJ-HIRU-LBL    PIC X(20).
                           11  HSACCTMRES-SKJ-HIRU-DATA   PIC X(02).
                           11  HSACCTMRES-SKJ-HIRU-NAME   PIC X(30).
                       09  HSACCTMRES-SKJ-YU-G.
                           11  HSACCTMRES-SKJ-YU-LBL      PIC X(20).
                           11  HSACCTMRES-SKJ-YU-DATA     PIC X(02).
                           11  HSACCTMRES-SKJ-YU-NAME     PIC X(30).
                       09  HSACCTMRES-SAGAKU-G.
                           11  HSACCTMRES-SAGAKU-LBL      PIC X(20).
                           11  HSACCTMRES-SAGAKU-DATA     PIC X(07).
                           11  HSACCTMRES-SAGAKU-NAME     PIC X(10).
               05  HSACCTMRES-HKNCOMBI-INF-AREA.
                   07  HSACCTMRES-HKNCOMBI-INF-OCC        OCCURS 10.
                       09  HSACCTMRES-HKNCOMBI-KEY        PIC X(04).
                       09  HSACCTMRES-HKNNUM              PIC X(03).
                       09  HSACCTMRES-HKNJANUM            PIC X(08).
                       09  HSACCTMRES-HKNJANAME           PIC X(20).
                       09  HSACCTMRES-HKIGO               PIC X(80).
                       09  HSACCTMRES-HNUM                PIC X(80).
                       09  HSACCTMRES-KOHINF-AREA.
                           11  HSACCTMRES-KOHINF-OCC      OCCURS 4.
                               13  HSACCTMRES-KOHNUM      PIC X(03).
                               13  HSACCTMRES-KOHNAME     PIC X(20).
                               13  HSACCTMRES-FTNJANUM    PIC X(08).
                               13  HSACCTMRES-JKYSNUM     PIC X(20).
