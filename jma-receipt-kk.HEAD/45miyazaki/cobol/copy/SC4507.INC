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
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    井上         19/05/22  改元対応
      ********************************************************************
        01  SC07.
            03  SC07-SRYYY      PIC X(04).
            03  SC07-SRYMM      PIC X(04).
            03  SC07-HOSPCD     PIC X(14).
            03  SC07-POST3      PIC X(03).
            03  SC07-POST4      PIC X(04).
            03  SC07-ADRS       PIC X(100).
            03  SC07-HOSPNAME   PIC X(100).
            03  SC07-KAISETU    PIC X(80).
            03  SC07-TEL1       PIC X(05).
            03  SC07-TEL2       PIC X(04).
            03  SC07-TEL3       PIC X(06).
            03  SC07-SEIKYU-TBL         OCCURS 6 TIMES.
                05  SC07-KENSU  PIC X(06).
                05  SC07-TENSU  PIC X(10).
                05  SC07-ITBFTN PIC X(09).
            03  SC07-SRYGG      PIC X(04).
