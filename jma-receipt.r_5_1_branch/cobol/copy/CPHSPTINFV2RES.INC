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
      *    ＡＰＩレスポンス入院患者基本情報 
      *    COPY (CPHSPTINFV2RES.INC)
      ******************************************************************
       01  XML-HSPTINFRES.
           03  HSPTINFRES-CONTEXT                  PIC S9(9)   BINARY.
           03  HSPTINFRES-OBJECT                   PIC X(36).
           03  HSPTINFRES-MODE                     PIC S9(9)   BINARY.
           03  HSPTINFRES-RECORDNAME               PIC X(128).
      *
           03  HSPTINFRES-PRIVATE-OBJECTS.
               05  HSPTINFRES-INFORMATION-DATE         PIC X(10).
               05  HSPTINFRES-INFORMATION-TIME         PIC X(8).
               05  HSPTINFRES-API-RESULT               PIC X(2).
               05  HSPTINFRES-API-RESULT-MSG           PIC X(100).
               05  HSPTINFRES-PTINF-AREA. 
                   07  HSPTINFRES-PTNUM                PIC X(20).
                   07  HSPTINFRES-NAME                 PIC X(100).
                   07  HSPTINFRES-KANANAME             PIC X(100).
                   07  HSPTINFRES-BIRTHDAY             PIC X(10).
                   07  HSPTINFRES-SEX                  PIC X(01).
               05  HSPTINFRES-NYUINRRK-INF-AREA.
                   07 HSPTINFRES-NYUINRRK-INF-OCC      OCCURS  100.
                       09 HSPTINFRES-RRKNUM            PIC X(03).
                       09  HSPTINFRES-CREATEKBN-G.
                           11  HSPTINFRES-CREATEKBN-LBL
                                                       PIC X(30).
                           11  HSPTINFRES-CREATEKBN-DATA
                                                       PIC X(01).
                           11  HSPTINFRES-CREATEKBN-NAME
                                                       PIC X(40).
                       09  HSPTINFRES-NYUINYMD         PIC X(10).
                       09  HSPTINFRES-TIMEKBN-IN-G.
                           11  HSPTINFRES-TIMEKBN-IN-LBL
                                                       PIC X(20).
                           11  HSPTINFRES-TIMEKBN-IN-DATA
                                                       PIC X(01).
                           11  HSPTINFRES-TIMEKBN-IN-NAME
                                                       PIC X(20).
                       09  HSPTINFRES-TAIINYMD         PIC X(10).
                       09  HSPTINFRES-TAIINCD-G.
                           11  HSPTINFRES-TAIINCD-LBL  PIC X(10).
                           11  HSPTINFRES-TAIINCD-DATA PIC X(02).
                           11  HSPTINFRES-TAIINCD-NAME PIC X(20).
                       09  HSPTINFRES-BTUNUM-G.
                           11  HSPTINFRES-BTUNUM-LBL      PIC X(20).
                           11  HSPTINFRES-BTUNUM-DATA     PIC X(2).
                       09  HSPTINFRES-BTUNAME-G.
                           11  HSPTINFRES-BTUNAME-LBL     PIC X(20).
                           11  HSPTINFRES-BTUNAME-DATA    PIC X(20).
                       09  HSPTINFRES-BRMNUM-G.
                           11  HSPTINFRES-BRMNUM-LBL      PIC X(10).
                           11  HSPTINFRES-BRMNUM-DATA     PIC X(06).
                       09  HSPTINFRES-SRYKA-G.
                           11  HSPTINFRES-SRYKA-LBL       PIC X(20).
                           11  HSPTINFRES-SRYKA-DATA      PIC X(2).
                           11  HSPTINFRES-SRYKA-NAME      PIC X(30).
                       09  HSPTINFRES-DR-G.
                           11  HSPTINFRES-DR-OCC          OCCURS 3.
                               13  HSPTINFRES-DR-LBL      PIC X(10).
                               13  HSPTINFRES-DR-DATA     PIC X(05).
                               13  HSPTINFRES-DR-NAME     PIC X(100).
                       09  HSPTINFRES-HKNINF-AREA.
                           11  HSPTINFRES-HKNCOMBI            PIC X(04).
                           11  HSPTINFRES-HKNNUM              PIC X(03).
                           11  HSPTINFRES-HKNJANUM            PIC X(08).
                           11  HSPTINFRES-HKNJANAME           PIC X(20).
                           11  HSPTINFRES-HKIGO               PIC X(80).
                           11  HSPTINFRES-HNUM                PIC X(80).
                           11  HSPTINFRES-KOHINF-AREA.
                               13  HSPTINFRES-KOHINF-OCC      OCCURS 4.
                                   15  HSPTINFRES-KOHNUM      PIC X(03).
                                   15  HSPTINFRES-KOHNAME     PIC X(20).
                                   15  HSPTINFRES-FTNJANUM    PIC X(08).
                                   15  HSPTINFRES-JKYSNUM     PIC X(20).
                      09  HSPTINFRES-SHOKAIYMD             PIC X(10).
                      09  HSPTINFRES-KAIGOKBN-G.
                          11   HSPTINFRES-KAIGOKBN-LBL     PIC X(20).
                          11   HSPTINFRES-KAIGOKBN-DATA    PIC X(01).
                          11   HSPTINFRES-KAIGOKBN-NAME    PIC X(20).
                      09  HSPTINFRES-SAGAKU-G.
                          11   HSPTINFRES-SAGAKU-LBL       PIC X(20).
                          11   HSPTINFRES-SAGAKU-DATA      PIC X(07).
                          11   HSPTINFRES-SAGAKU-NAME      PIC X(10).
                      09  HSPTINFRES-SENTEIKBN-G.
                          11   HSPTINFRES-SENTEIKBN-LBL    PIC X(20).
                          11   HSPTINFRES-SENTEIKBN-DATA   PIC X(01).
                          11   HSPTINFRES-SENTEIKBN-NAME   PIC X(10).
                      09  HSPTINFRES-NYUINRYO-G.
                          11   HSPTINFRES-NYUINRYO-LBL     PIC X(20).
                          11   HSPTINFRES-NYUINRYO-DATA    PIC X(09).
                          11   HSPTINFRES-NYUINRYO-NAME    PIC X(100).
                      09  HSPTINFRES-TOKSNTKBN-G.
                          11   HSPTINFRES-TOKSNTKBN-LBL     PIC X(30).
                          11   HSPTINFRES-TOKSNTKBN-DATA    PIC X(1).
                          11   HSPTINFRES-TOKSNTKBN-NAME    PIC X(30).
                      09  HSPTINFRES-KSNSRYCD-G.
                          11   HSPTINFRES-KSNSRYCD-OCC     OCCURS 3.
                               13   HSPTINFRES-KSNSRYCD-LBL
                                                           PIC X(20).
                               13   HSPTINFRES-KSNSRYCD-DATA
                                                           PIC X(09).
                               13   HSPTINFRES-KSNSRYCD-NAME
                                                           PIC X(100).
                      09  HSPTINFRES-NACCTKBN-G.
                          11   HSPTINFRES-NACCTKBN-LBL     PIC X(20).
                          11   HSPTINFRES-NACCTKBN-DATA    PIC X(01).
                          11   HSPTINFRES-NACCTKBN-NAME    PIC X(30).
                      09  HSPTINFRES-BUNBENKBN-G.
                          11   HSPTINFRES-BUNBENKBN-LBL    PIC X(20).
                          11   HSPTINFRES-BUNBENKBN-DATA   PIC X(01).
                          11   HSPTINFRES-BUNBENKBN-NAME   PIC X(30).
                      09  HSPTINFRES-RIYOUKBN-G.
                          11   HSPTINFRES-RIYOUKBN-LBL     PIC X(20).
                          11   HSPTINFRES-RIYOUKBN-DATA    PIC X(01).
                          11   HSPTINFRES-RIYOUKBN-NAME    PIC X(20).
                      09  HSPTINFRES-TEIKIKBN-G.
                          11   HSPTINFRES-TEIKIKBN-LBL     PIC X(20).
                          11   HSPTINFRES-TEIKIKBN-DATA    PIC X(01).
                          11   HSPTINFRES-TEIKIKBN-NAME    PIC X(20).
                      09  HSPTINFRES-DISPKBN-G.
                          11   HSPTINFRES-DISPKBN-LBL      PIC X(20).
                          11   HSPTINFRES-DISPKBN-DATA     PIC X(01).
                          11   HSPTINFRES-DISPKBN-NAME     PIC X(20).
                      09  HSPTINFRES-SKJKBN-G.
                          11   HSPTINFRES-SKJKBN-LBL       PIC X(20).
                          11   HSPTINFRES-SKJKBN-DATA      PIC X(01).
                          11   HSPTINFRES-SKJKBN-NAME      PIC X(30).
                      09  HSPTINFRES-TSKJKBN-G.
                          11   HSPTINFRES-TSKJKBN-OCC OCCURS   3.
                               13   HSPTINFRES-TSKJKBN-LBL  PIC X(20).
                               13   HSPTINFRES-TSKJKBN-DATA PIC X(01).
                               13   HSPTINFRES-TSKJKBN-NAME PIC X(30).
