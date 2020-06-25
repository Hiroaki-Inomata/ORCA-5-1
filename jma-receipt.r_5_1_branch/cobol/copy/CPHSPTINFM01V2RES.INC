      *******************************************************************
      * Project code name "ORCA"
      * ì˙à„ïWèÄÉåÉZÉvÉgÉ\ÉtÉgÅiJMA standard receipt softwareÅj
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
      *    Ç`ÇoÇhÉåÉXÉ|ÉìÉXì¸â@ìoò^èÓïÒ Åiì¸â@óöóèÓïÒÅj
      *    COPY (CPHSPTINFM01V2RES.INC)
      ******************************************************************
       01  XML-HSPTINFM01RES.
           03  HSPTINFM01RES-CONTEXT              PIC S9(9)   BINARY.
           03  HSPTINFM01RES-OBJECT               PIC X(36).
           03  HSPTINFM01RES-MODE                 PIC S9(9)   BINARY.
           03  HSPTINFM01RES-RECORDNAME           PIC X(128).
           03  HSPTINFM01RES-PRIVATE-OBJECTS.
               05  HSPTINFM01RES-INFORMATION-DATE     PIC X(10).
               05  HSPTINFM01RES-INFORMATION-TIME     PIC X(8).
               05  HSPTINFM01RES-API-RESULT-OCC       OCCURS  10.
                   07  HSPTINFM01RES-API-RESULT       PIC X(4).
                   07  HSPTINFM01RES-API-RESULT-MSG   PIC X(100).
               05  HSPTINFM01RES-REQNUM-G.
                   07  HSPTINFM01RES-REQNUM-LBL       PIC X(20).
                   07  HSPTINFM01RES-REQNUM-DATA      PIC X(02).
                   07  HSPTINFM01RES-REQNUM-NAME      PIC X(30).
               05  HSPTINFM01RES-PTINF-AREA. 
                   07  HSPTINFM01RES-PTNUM                PIC X(20).
                   07  HSPTINFM01RES-NAME                 PIC X(100).
                   07  HSPTINFM01RES-KANANAME             PIC X(100).
                   07  HSPTINFM01RES-BIRTHDAY             PIC X(10).
                   07  HSPTINFM01RES-SEX                  PIC X(01).
               05  HSPTINFM01RES-NYUINRRK-INF-AREA.
                   07 HSPTINFM01RES-NYUINRRK-INF-G.
                       09  HSPTINFM01RES-RRKNUM           PIC X(03).
                       09  HSPTINFM01RES-CREATEKBN-G.
                           11  HSPTINFM01RES-CREATEKBN-LBL
                                                       PIC X(30).
                           11  HSPTINFM01RES-CREATEKBN-DATA
                                                       PIC X(01).
                           11  HSPTINFM01RES-CREATEKBN-NAME
                                                       PIC X(40).
                       09  HSPTINFM01RES-NYUINYMD      PIC X(10).
                       09  HSPTINFM01RES-IDOYMD        PIC X(10).
                       09  HSPTINFM01RES-BTUNUM-G.
                           11  HSPTINFM01RES-BTUNUM-LBL      PIC X(20).
                           11  HSPTINFM01RES-BTUNUM-DATA     PIC X(2).
                       09  HSPTINFM01RES-BTUNAME-G.
                           11  HSPTINFM01RES-BTUNAME-LBL     PIC X(20).
                           11  HSPTINFM01RES-BTUNAME-DATA    PIC X(20).
                       09  HSPTINFM01RES-BRMNUM-G.
                           11  HSPTINFM01RES-BRMNUM-LBL      PIC X(10).
                           11  HSPTINFM01RES-BRMNUM-DATA     PIC X(06).
                       09  HSPTINFM01RES-SRYKA-G.
                           11  HSPTINFM01RES-SRYKA-LBL       PIC X(20).
                           11  HSPTINFM01RES-SRYKA-DATA      PIC X(2).
                           11  HSPTINFM01RES-SRYKA-NAME      PIC X(30).
                       09  HSPTINFM01RES-DR-G.
                           11  HSPTINFM01RES-DR-OCC          OCCURS 3.
                               13  HSPTINFM01RES-DR-LBL      PIC X(10).
                               13  HSPTINFM01RES-DR-DATA     PIC X(05).
                               13  HSPTINFM01RES-DR-NAME     PIC X(100).
                       09  HSPTINFM01RES-HKNINF-AREA.
                           11  HSPTINFM01RES-HKNCOMBI        PIC X(04).
                           11  HSPTINFM01RES-HKNNUM          PIC X(03).
                           11  HSPTINFM01RES-HKNJANUM        PIC X(08).
                           11  HSPTINFM01RES-HKNJANAME       PIC X(20).
                           11  HSPTINFM01RES-HKIGO           PIC X(80).
                           11  HSPTINFM01RES-HNUM            PIC X(80).
                           11  HSPTINFM01RES-KOHINF-AREA.
                               13  HSPTINFM01RES-KOHINF-OCC  OCCURS 4.
                                   15  HSPTINFM01RES-KOHNUM  PIC X(03).
                                   15  HSPTINFM01RES-KOHNAME PIC X(20).
                                   15  HSPTINFM01RES-FTNJANUM PIC X(08).
                                   15  HSPTINFM01RES-JKYSNUM  PIC X(20).
                      09  HSPTINFM01RES-SHOKAIYMD             PIC X(10).
                      09  HSPTINFM01RES-KAIGOKBN-G.
                          11   HSPTINFM01RES-KAIGOKBN-LBL     PIC X(20).
                          11   HSPTINFM01RES-KAIGOKBN-DATA    PIC X(01).
                          11   HSPTINFM01RES-KAIGOKBN-NAME    PIC X(20).
                      09  HSPTINFM01RES-SAGAKU-G.
                          11   HSPTINFM01RES-SAGAKU-LBL       PIC X(20).
                          11   HSPTINFM01RES-SAGAKU-DATA      PIC X(07).
                          11   HSPTINFM01RES-SAGAKU-NAME      PIC X(10).
                      09  HSPTINFM01RES-SENTEIKBN-G.
                          11   HSPTINFM01RES-SENTEIKBN-LBL    PIC X(20).
                          11   HSPTINFM01RES-SENTEIKBN-DATA   PIC X(01).
                          11   HSPTINFM01RES-SENTEIKBN-NAME   PIC X(10).
                      09  HSPTINFM01RES-NYUINRYO-G.
                          11   HSPTINFM01RES-NYUINRYO-LBL     PIC X(40).
                          11   HSPTINFM01RES-NYUINRYO-DATA    PIC X(09).
                          11   HSPTINFM01RES-NYUINRYO-NAME   PIC X(100).
                      09  HSPTINFM01RES-IDONYUINRYO-G.
                          11  HSPTINFM01RES-IDONYUINRYO-LBL  PIC X(40).
                          11  HSPTINFM01RES-IDONYUINRYO-DATA PIC X(09).
                          11  HSPTINFM01RES-IDONYUINRYO-NAME PIC X(100).
                      09  HSPTINFM01RES-NACCTKBN-G.
                          11   HSPTINFM01RES-NACCTKBN-LBL     PIC X(20).
                          11   HSPTINFM01RES-NACCTKBN-DATA    PIC X(01).
                          11   HSPTINFM01RES-NACCTKBN-NAME    PIC X(30).
                      09  HSPTINFM01RES-BUNBENKBN-G.
                          11   HSPTINFM01RES-BUNBENKBN-LBL   PIC X(20).
                          11   HSPTINFM01RES-BUNBENKBN-DATA  PIC X(01).
                          11   HSPTINFM01RES-BUNBENKBN-NAME  PIC X(20).
                      09  HSPTINFM01RES-RIYOUKBN-G.
                          11   HSPTINFM01RES-RIYOUKBN-LBL     PIC X(20).
                          11   HSPTINFM01RES-RIYOUKBN-DATA    PIC X(01).
                          11   HSPTINFM01RES-RIYOUKBN-NAME    PIC X(20).
                      09  HSPTINFM01RES-TEIKIKBN-G.
                          11   HSPTINFM01RES-TEIKIKBN-LBL     PIC X(20).
                          11   HSPTINFM01RES-TEIKIKBN-DATA    PIC X(01).
                          11   HSPTINFM01RES-TEIKIKBN-NAME    PIC X(20).
                      09  HSPTINFM01RES-DISPKBN-G.
                          11   HSPTINFM01RES-DISPKBN-LBL     PIC X(30).
                          11   HSPTINFM01RES-DISPKBN-DATA    PIC X(01).
                          11   HSPTINFM01RES-DISPKBN-NAME    PIC X(20).
