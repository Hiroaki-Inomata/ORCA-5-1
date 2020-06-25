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
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      ************************************************************
       01  SS001-AREA.
           03  SS001-HOSPNUM                       PIC 9(2).
           03  SS001-PTID                          PIC 9(10).
           03  SS001-SRYYM                         PIC X(06).
           03  SS001-SRYKAKBN                      PIC X(01).
           03  SS001-KIKANKBN                      PIC X(01).
           03  SS001-STDAY                         PIC 9(02).
           03  SS001-EDDAY                         PIC 9(02).
           03  SS001-HKNGRP-G.
      *
               05  SS001-NIHO-FLG                  PIC 9(01).
               05  SS001-NRSI-FLG                  PIC 9(01).
               05  SS001-NJBI-FLG                  PIC 9(01).
               05  SS001-NJBIR-FLG                 PIC 9(01).
               05  SS001-NJBIK-FLG                 PIC 9(01).
               05  SS001-NKGI-FLG                  PIC 9(01).
               05  SS001-NSYA-FLG                  PIC 9(01).
               05  SS001-NKOK-FLG                  PIC 9(01).
               05  SS001-NKOUKI-FLG                PIC 9(01).
               05  SS001-NJHI-FLG                  PIC 9(01).
               05  SS001-NHKT-FLG                  PIC 9(01).
               05  SS001-NDI3-FLG                  PIC 9(01).
               05  SS001-NCHIKEN-FLG               PIC 9(01).
      *
               05  SS001-GIHO-FLG                  PIC 9(01).
               05  SS001-GRSI-FLG                  PIC 9(01).
               05  SS001-GJBI-FLG                  PIC 9(01).
               05  SS001-GJBIR-FLG                 PIC 9(01).
               05  SS001-GJBIK-FLG                 PIC 9(01).
               05  SS001-GKGI-FLG                  PIC 9(01).
               05  SS001-GSYA-FLG                  PIC 9(01).
               05  SS001-GKOK-FLG                  PIC 9(01).
               05  SS001-GKOUKI-FLG                  PIC 9(01).
               05  SS001-GJHI-FLG                  PIC 9(01).
               05  SS001-GHKT-FLG                  PIC 9(01).
               05  SS001-GDI3-FLG                  PIC 9(01).
               05  SS001-GCHIKEN-FLG               PIC 9(01).
           03  SS001-HKNGRP-R  REDEFINES SS001-HKNGRP-G.
               05  SS001-HKNGRP-OCC                OCCURS 2.
                   07  SS001-IHO-FLG               PIC 9(01).
                   07  SS001-RSI-FLG               PIC 9(01).
                   07  SS001-JBI-FLG               PIC 9(01).
                   07  SS001-JBIR-FLG              PIC 9(01).
                   07  SS001-JBIK-FLG              PIC 9(01).
                   07  SS001-KGI-FLG               PIC 9(01).
                   07  SS001-SYA-FLG               PIC 9(01).
                   07  SS001-KOK-FLG               PIC 9(01).
                   07  SS001-KOUKI-FLG             PIC 9(01).
                   07  SS001-JHI-FLG               PIC 9(01).
                   07  SS001-HKT-FLG               PIC 9(01).
                   07  SS001-DI3-FLG               PIC 9(01).
                   07  SS001-CHIKEN-FLG            PIC 9(01).
           03  SS001-SRYKA-G.
               05  SS001-SRYKA-MAX                 PIC 9(03).
               05  SS001-SRYKA-OCC                 OCCURS  50.
                   07  SS001-SRYKA                 PIC X(02).
                   07  SS001-SRYKA-HKNGRP-OCC      OCCURS  2.
                       09  SS001-SRYKA-IHO-FLG     PIC 9(01).
                       09  SS001-SRYKA-RSI-FLG     PIC 9(01).
                       09  SS001-SRYKA-JBI-FLG     PIC 9(01).
                       09  SS001-SRYKA-JBIR-FLG    PIC 9(01).
                       09  SS001-SRYKA-JBIK-FLG    PIC 9(01).
                       09  SS001-SRYKA-KGI-FLG     PIC 9(01).
                       09  SS001-SRYKA-SYA-FLG     PIC 9(01).
                       09  SS001-SRYKA-KOK-FLG     PIC 9(01).
                       09  SS001-SRYKA-KOUKI-FLG   PIC 9(01).
                       09  SS001-SRYKA-JHI-FLG     PIC 9(01).
                       09  SS001-SRYKA-HKT-FLG     PIC 9(01).
                       09  SS001-SRYKA-DI3-FLG     PIC 9(01).
                       09  SS001-SRYKA-CHIKEN-FLG  PIC 9(01).
                   07  SS001-SRYKA-DAY-G.
                       09  SS001-SRYKA-NYU-DAY     PIC 9(01)
                                                   OCCURS  31.
                       09  SS001-SRYKA-GAI-DAY     PIC 9(01)
                                                   OCCURS  31.
                   07  SS001-SRYKA-DAY-R   REDEFINES
                                               SS001-SRYKA-DAY-G.
                       09  SS001-SRYKA-DAY-OCC     OCCURS  2.
                           11  SS001-SRYKA-DAY     PIC 9(01)
                                                   OCCURS  31.
                   07  SS001-SRYKA-NYURRK-DAY-G.
                       09  SS001-SRYKA-NYURRK-DAY  PIC 9(01)
                                                   OCCURS  31.
           03  SS001-HKNCOMBI-G.
               05  SS001-HKNCOMBI-MAX              PIC 9(03).
               05  SS001-HKNCOMBINUM               PIC 9(04)
                                                   OCCURS  100.
