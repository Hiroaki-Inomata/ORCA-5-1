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
      *    入院情報取得
      *    COPY (CPORCSNINF01.INC)
      ******************************************************************
       01  SNINF01-AREA.
           03  SNINF01-IN-RRKNUM                PIC 9(03).
           03  SNINF01-IN-SHONUM                PIC 9(03).
           03  SNINF01-IN-KJNYMD                PIC X(08).
      *
           03  SNINF01-PTINF-AREA.
               05  SNINF01-PTNUM                PIC X(20).
               05  SNINF01-NAME                 PIC X(100).
               05  SNINF01-KANANAME             PIC X(100).
               05  SNINF01-BIRTHDAY             PIC X(10).
               05  SNINF01-SEX                  PIC X(01).
           03  SNINF01-NYUINRRK-INF-AREA.
               05 SNINF01-NYUINRRK-INF-MAX      PIC 9(05).
               05 SNINF01-NYUINRRK-INF-OCC      OCCURS  100.
                   07  SNINF01-RRKNUM           PIC 9(03).
                   07  SNINF01-CREATEKBN-G.
                       09  SNINF01-CREATEKBN-LBL
                                                   PIC X(30).
                       09  SNINF01-CREATEKBN-DATA
                                                   PIC X(01).
                       09  SNINF01-CREATEKBN-NAME
                                                   PIC X(40).
                   07  SNINF01-NYUINYMD            PIC X(10).
                   07  SNINF01-TIMEKBN-IN-G.
                       09  SNINF01-TIMEKBN-IN-LBL  PIC X(20).
                       09  SNINF01-TIMEKBN-IN-DATA PIC X(02).
                       09  SNINF01-TIMEKBN-IN-NAME PIC X(20).
                   07  SNINF01-IDOYMD              PIC X(10).
                   07  SNINF01-TAIINYMD            PIC X(10).
                   07  SNINF01-TAIINCD-G.
                       09  SNINF01-TAIINCD-LBL  PIC X(10).
                       09  SNINF01-TAIINCD-DATA PIC X(02).
                       09  SNINF01-TAIINCD-NAME PIC X(20).
                   07  SNINF01-BTUNUM-G.
                       09  SNINF01-BTUNUM-LBL      PIC X(20).
                       09  SNINF01-BTUNUM-DATA     PIC X(2).
                   07  SNINF01-BTUNAME-G.
                       09  SNINF01-BTUNAME-LBL     PIC X(20).
                       09  SNINF01-BTUNAME-DATA    PIC X(20).
                   07  SNINF01-BRMNUM-G.
                       09  SNINF01-BRMNUM-LBL      PIC X(10).
                       09  SNINF01-BRMNUM-DATA     PIC X(06).
                   07  SNINF01-SRYKA-G.
                       09  SNINF01-SRYKA-LBL       PIC X(20).
                       09  SNINF01-SRYKA-DATA      PIC X(2).
                       09  SNINF01-SRYKA-NAME      PIC X(30).
                   07  SNINF01-DR-G.
                       09  SNINF01-DR-OCC          OCCURS 3.
                           11  SNINF01-DR-LBL      PIC X(10).
                           11  SNINF01-DR-DATA     PIC X(05).
                           11  SNINF01-DR-NAME     PIC X(100).
                   07  SNINF01-HKNINF-AREA.
                       09  SNINF01-HKNCOMBI            PIC X(04).
                       09  SNINF01-HKNNUM              PIC X(03).
                       09  SNINF01-HKNJANUM            PIC X(08).
                       09  SNINF01-HKNJANAME           PIC X(20).
                       09  SNINF01-HKIGO               PIC X(80).
                       09  SNINF01-HNUM                PIC X(80).
                       09  SNINF01-KOHINF-AREA.
                           11  SNINF01-KOHINF-OCC      OCCURS 4.
                               13  SNINF01-KOHNUM      PIC X(03).
                               13  SNINF01-KOHNAME     PIC X(20).
                               13  SNINF01-FTNJANUM    PIC X(08).
                               13  SNINF01-JKYSNUM     PIC X(20).
                  07  SNINF01-SHOKAIYMD             PIC X(10).
                  07  SNINF01-KAIGOKBN-G.
                      09   SNINF01-KAIGOKBN-LBL     PIC X(20).
                      09   SNINF01-KAIGOKBN-DATA    PIC X(01).
                      09   SNINF01-KAIGOKBN-NAME    PIC X(20).
                  07  SNINF01-SAGAKU-G.
                      09   SNINF01-SAGAKU-LBL       PIC X(20).
                      09   SNINF01-SAGAKU-DATA      PIC X(07).
                      09   SNINF01-SAGAKU-NAME      PIC X(10).
                  07  SNINF01-SENTEIKBN-G.
                      09   SNINF01-SENTEIKBN-LBL    PIC X(20).
                      09   SNINF01-SENTEIKBN-DATA   PIC X(01).
                      09   SNINF01-SENTEIKBN-NAME   PIC X(10).
                  07  SNINF01-NYUINRYO-G.
                      09   SNINF01-NYUINRYO-LBL     PIC X(20).
                      09   SNINF01-NYUINRYO-DATA    PIC X(09).
                      09   SNINF01-NYUINRYO-NAME    PIC X(100).
                  07  SNINF01-TOKSNTKBN-G.
                      09   SNINF01-TOKSNTKBN-LBL     PIC X(30).
                      09   SNINF01-TOKSNTKBN-DATA    PIC X(1).
                      09   SNINF01-TOKSNTKBN-NAME    PIC X(30).
                  07  SNINF01-TOKSNTCD-G.
                      09   SNINF01-TOKSNTCD-LBL      PIC X(30).
                      09   SNINF01-TOKSNTCD-DATA     PIC X(9).
                      09   SNINF01-TOKSNTCD-NAME     PIC X(100).
                  07  SNINF01-KSNSRYCD-G.
                      09   SNINF01-KSNSRYCD-MAX     PIC 9(03).
                      09   SNINF01-KSNSRYCD-OCC     OCCURS 3.
                           11   SNINF01-KSNSRYCD-LBL
                                                    PIC X(20).
                           11   SNINF01-KSNSRYCD-DATA
                                                    PIC X(09).
                           11   SNINF01-KSNSRYCD-NAME
                                                    PIC X(100).
                  07  SNINF01-IDONYUINRYO-G.
                      09   SNINF01-IDONYUINRYO-LBL    PIC X(20).
                      09   SNINF01-IDONYUINRYO-DATA   PIC X(09).
                      09   SNINF01-IDONYUINRYO-NAME   PIC X(100).
                  07  SNINF01-IDOKSNSRYCD-G.
                      09   SNINF01-IDOKSNSRYCD-MAX  PIC 9(03).
                      09   SNINF01-IDOKSNSRYCD-OCC  OCCURS 3.
                           11   SNINF01-IDOKSNSRYCD-LBL
                                                    PIC X(20).
                           11   SNINF01-IDOKSNSRYCD-DATA
                                                    PIC X(09).
                           11   SNINF01-IDOKSNSRYCD-NAME
                                                    PIC X(100).
                  07  SNINF01-NACCTKBN-G.
                      09   SNINF01-NACCTKBN-LBL     PIC X(20).
                      09   SNINF01-NACCTKBN-DATA    PIC X(01).
                      09   SNINF01-NACCTKBN-NAME    PIC X(30).
                  07  SNINF01-BUNBENKBN-G.
                      09   SNINF01-BUNBENKBN-LBL    PIC X(20).
                      09   SNINF01-BUNBENKBN-DATA   PIC X(01).
                      09   SNINF01-BUNBENKBN-NAME   PIC X(30).
                  07  SNINF01-RIYOUKBN-G.
                      09   SNINF01-RIYOUKBN-LBL     PIC X(20).
                      09   SNINF01-RIYOUKBN-DATA    PIC X(01).
                      09   SNINF01-RIYOUKBN-NAME    PIC X(20).
                  07  SNINF01-TEIKIKBN-G.
                      09   SNINF01-TEIKIKBN-LBL     PIC X(20).
                      09   SNINF01-TEIKIKBN-DATA    PIC X(01).
                      09   SNINF01-TEIKIKBN-NAME    PIC X(20).
                  07  SNINF01-DISPKBN-G.
                      09   SNINF01-DISPKBN-LBL      PIC X(20).
                      09   SNINF01-DISPKBN-DATA     PIC X(01).
                      09   SNINF01-DISPKBN-NAME     PIC X(20).
                  07  SNINF01-SKJKBN-G.
                      09   SNINF01-SKJKBN-LBL       PIC X(20).
                      09   SNINF01-SKJKBN-DATA      PIC X(01).
                      09   SNINF01-SKJKBN-NAME      PIC X(30).
                  07  SNINF01-TSKJKBN-G.
                      09   SNINF01-TSKJKBN-OCC         OCCURS 3.
                           11   SNINF01-TSKJKBN-LBL    PIC X(20).
                           11   SNINF01-TSKJKBN-DATA   PIC X(01).
                           11   SNINF01-TSKJKBN-NAME   PIC X(30).
