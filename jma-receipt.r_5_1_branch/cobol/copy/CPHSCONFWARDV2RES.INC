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
       01  XML-HSCONFWARDRES.
           03  HSCWRES-CONTEXT                     PIC S9(9)   BINARY.
           03  HSCWRES-OBJECT                      PIC X(36).
           03  HSCWRES-MODE                        PIC S9(9)   BINARY.
           03  HSCWRES-RECORDNAME                  PIC X(128).
           03  HSCWRES-PRIVATE-OBJECTS.
               05  HSCWRES-INFORMATION-DATE        PIC X(10).
               05  HSCWRES-INFORMATION-TIME        PIC X(8).
               05  HSCWRES-API-RESULT              PIC X(2).
               05  HSCWRES-API-RESULT-MSG          PIC X(100).
               05  HSCWRES-OCC                     OCCURS  50.
                   07  HSCWRES-ST-DATE             PIC X(10).
                   07  HSCWRES-ED-DATE             PIC X(10).
                   07  HSCWRES-BTUNUM-G.
                       09  HSCWRES-BTUNUM-LBL          PIC X(20).
                       09  HSCWRES-BTUNUM-DATA         PIC X(2).
                   07  HSCWRES-BTUNAME-G.
                       09  HSCWRES-BTUNAME-LBL         PIC X(20).
                       09  HSCWRES-BTUNAME-DATA        PIC X(20).
                   07  HSCWRES-BTUNAME2-G.
                       09  HSCWRES-BTUNAME2-LBL        PIC X(20).
                       09  HSCWRES-BTUNAME2-DATA       PIC X(10).
                   07  HSCWRES-BTUSBT-G.
                       09  HSCWRES-BTUSBT-LBL          PIC X(20).
                       09  HSCWRES-BTUSBT-DATA         PIC X(2).
                       09  HSCWRES-BTUSBT-NAME         PIC X(30).
                   07  HSCWRES-SRYKA-G.
                       09  HSCWRES-SRYKA-LBL           PIC X(20).
                       09  HSCWRES-SRYKA-DATA          PIC X(2).
                       09  HSCWRES-SRYKA-NAME          PIC X(30).
                   07  HSCWRES-TOKNYUIN-G.
                       09  HSCWRES-TOKNYUIN-LBL        PIC X(20).
                       09  HSCWRES-TOKNYUIN-DATA       PIC X(9).
                       09  HSCWRES-TOKNYUIN-NAME       PIC X(100).
                   07  HSCWRES-NYUINKHN-G.
                       09  HSCWRES-NYUINKHN-LBL        PIC X(20).
                       09  HSCWRES-NYUINKHN-DATA       PIC X(9).
                       09  HSCWRES-NYUINKHN-NAME       PIC X(100).
                   07  HSCWRES-YKNCHOKA-G.
                       09  HSCWRES-YKNCHOKA-LBL        PIC X(20).
                       09  HSCWRES-YKNCHOKA-DATA       PIC X(1).
                       09  HSCWRES-YKNCHOKA-NAME       PIC X(20).
                   07  HSCWRES-NYUINKSN-G.
                       09 HSCWRES-NYUINKSN-OCC         OCCURS  20.
                          11 HSCWRES-NYUINKSN-LBL      PIC X(20).
                          11  HSCWRES-NYUINKSN-DATA    PIC X(9).
                          11  HSCWRES-NYUINKSN-NAME    PIC X(50).
                   07  HSCWRES-USERKSN-G.
                       09 HSCWRES-USERKSN-OCC          OCCURS  10.
                          11 HSCWRES-USERKSN-LBL       PIC X(30).
                          11  HSCWRES-USERKSN-DATA     PIC X(9).
                          11  HSCWRES-USERKSN-NAME     PIC X(50).
                   07  HSCWRES-SENTEI-G.
                       09 HSCWRES-SENTEI-LBL           PIC X(30).
                       09 HSCWRES-SENTEI-FTN-G.
                          11  HSCWRES-SENTEI-FTN-LBL   PIC X(30).
                          11  HSCWRES-SENTEI-FTN-DATA  PIC X(1).
                          11  HSCWRES-SENTEI-FTN-NAME  PIC X(30).
                       09 HSCWRES-SENTEI-TAX-G.
                          11  HSCWRES-SENTEI-TAX-LBL   PIC X(30).
                          11  HSCWRES-SENTEI-TAX-DATA  PIC X(1).
                          11  HSCWRES-SENTEI-TAX-NAME  PIC X(30).
                       09 HSCWRES-SENTEI-IPPAN-G.
                          11  HSCWRES-SENTEI-IPPAN-LBL  PIC X(30).
                          11  HSCWRES-SENTEI-IPPAN-DATA PIC X(04).
                          11  HSCWRES-SENTEI-IPPAN-NAME PIC X(30).
                       09 HSCWRES-SENTEI-KOUKI-G.
                          11  HSCWRES-SENTEI-KOUKI-LBL  PIC X(30).
                          11  HSCWRES-SENTEI-KOUKI-DATA PIC X(04).
                          11  HSCWRES-SENTEI-KOUKI-NAME PIC X(30).
                   07  HSCWRES-BRMNUM-G.
                       09  HSCWRES-BRMNUM-OCC           OCCURS 200.
                          11  HSCWRES-BRMNUM-LBL        PIC X(10).
                          11  HSCWRES-BRMNUM-DATA       PIC X(06).
