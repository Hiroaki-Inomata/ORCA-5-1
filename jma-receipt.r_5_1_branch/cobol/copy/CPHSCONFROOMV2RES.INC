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
      *    ＡＰＩレスポンス入院病棟情報    COPY (CPHSCONFBASEV2RES.INC)
      ******************************************************************
       01  XML-HSCONFROOMRES.
           03  HSCRRES-CONTEXT                     PIC S9(9)   BINARY.
           03  HSCRRES-OBJECT                      PIC X(36).
           03  HSCRRES-MODE                        PIC S9(9)   BINARY.
           03  HSCRRES-RECORDNAME                  PIC X(128).
           03  HSCRRES-PRIVATE-OBJECTS.
               05  HSCRRES-INFORMATION-DATE        PIC X(10).
               05  HSCRRES-INFORMATION-TIME        PIC X(8).
               05  HSCRRES-API-RESULT              PIC X(2).
               05  HSCRRES-API-RESULT-MSG          PIC X(100).
               05  HSCRRES-OCC                     OCCURS 200.
                   07  HSCRRES-ST-DATE             PIC X(10).
                   07  HSCRRES-ED-DATE             PIC X(10).
                   07  HSCRRES-BTUNUM-G.
                       09  HSCRRES-BTUNUM-LBL      PIC X(20).
                       09  HSCRRES-BTUNUM-DATA     PIC X(2).
                   07  HSCRRES-BTUNAME-G.
                       09  HSCRRES-BTUNAME-LBL         PIC X(20).
                       09  HSCRRES-BTUNAME-DATA        PIC X(20).
                   07  HSCRRES-BRMNUM-G.
                       09  HSCRRES-BRMNUM-LBL          PIC X(20).
                       09  HSCRRES-BRMNUM-DATA         PIC X(6).
                   07  HSCRRES-BRMSBT-G.
                       09  HSCRRES-BTUSBT-LBL          PIC X(20).
                       09  HSCRRES-BTUSBT-DATA         PIC X(2).
                       09  HSCRRES-BTUSBT-NAME         PIC X(30).
                   07  HSCRRES-NINZU-G.
                       09  HSCRRES-NINZU-LBL           PIC X(20).
                       09  HSCRRES-NINZU-DATA          PIC X(2).
                       09  HSCRRES-NINZU-NAME          PIC X(10).
                   07  HSCRRES-SAGAKU-G.
                       09  HSCRRES-SAGAKU-LBL          PIC X(20).
                       09  HSCRRES-SAGAKU-DATA         PIC X(7).
                       09  HSCRRES-SAGAKU-NAME         PIC X(10).
                   07  HSCRRES-SEX-G.
                       09  HSCRRES-SEX-LBL             PIC X(20).
                       09  HSCRRES-SEX-DATA            PIC X(1).
                       09  HSCRRES-SEX-NAME            PIC X(20).
                   07  HSCRRES-NAISEN-G.
                       09  HSCRRES-NAISEN-LBL          PIC X(20).
                       09  HSCRRES-NAISEN-DATA         PIC X(5).
                       09  HSCRRES-NAISEN-NAME         PIC X(10).
                   07  HSCRRES-SRYKA-G.
                       09  HSCRRES-SRYKA-LBL           PIC X(20).
                       09  HSCRRES-SRYKA-DATA          PIC X(02).
                       09  HSCRRES-SRYKA-NAME          PIC X(30).
                   07  HSCRRES-TOKNYUIN-G.
                       09  HSCRRES-TOKNYUIN-LBL        PIC X(20).
                       09  HSCRRES-TOKNYUIN-DATA       PIC X(9).
                       09  HSCRRES-TOKNYUIN-NAME       PIC X(100).
                   07  HSCRRES-NYUINKHN-G.
                       09  HSCRRES-NYUINKHN-LBL        PIC X(20).
                       09  HSCRRES-NYUINKHN-DATA       PIC X(9).
                       09  HSCRRES-NYUINKHN-NAME       PIC X(100).
                   07  HSCRRES-NYUINKSN-G.
                       09 HSCRRES-NYUINKSN-OCC         OCCURS  12.
                          11 HSCRRES-NYUINKSN-LBL      PIC X(20).
                          11  HSCRRES-NYUINKSN-DATA    PIC X(9).
                          11  HSCRRES-NYUINKSN-NAME    PIC X(50).
