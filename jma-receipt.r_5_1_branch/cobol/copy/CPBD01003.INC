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
      *****************************************************************
      *    住所                COPY (CPBD01003.INC)        *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  BD01003-REC.
           03  BD01003-KEY.
               05  BD01003-SRT-NYUGAIKBN           PIC X(01).
               05  BD01003-SRT-SRYKA               PIC X(02).
           03  BD01003-SEIKYU-AREA.
               05  BD01003-SEIKYU-TOTAL            PIC S9(10)
                                                   OCCURS  11.
               05  BD01003-SEIKYU-VAL.
                   07  BD01003-SEIKYU-OCC          OCCURS  11.
                       09  BD01003-SEIKYU          PIC S9(10)
                                                   OCCURS  15.
           03  BD01003-JIHI-AREA.
               05  BD01003-JIHI-TOTAL              PIC S9(10).
               05  BD01003-JIHI                    PIC S9(10)
                                                   OCCURS  10.
           03  BD01003-NYUKIN-AREA.
               05  BD01003-NYUKIN-TOTAL            PIC S9(10).
               05  BD01003-NYUKIN                  PIC S9(10)
                                                   OCCURS  13.
           03  BD01003-TENSU-AREA.
               05  BD01003-TENSU-TOTAL             PIC S9(10).
               05  BD01003-TENSU                   PIC S9(10)
                                                   OCCURS  15.
