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
      *    ADL区分情報          COPY (CPADLKBN.INC)                  *
      *****************************************************************
       01  ADL-AREA.
           03  ADL-VAL.
               05  ADL-01FLS     PIC X(08)    VALUE  "00000000".
               05  ADL-01FLE     PIC X(08)    VALUE  "99999999".
               05  ADL-01FL01    PIC X(09)    VALUE  "840000126".
               05  ADL-01FL02    PIC 9(04)    VALUE  1001.
               05  ADL-01FL03    PIC X(01)    VALUE  "a".
               05  ADL-01FL04    PIC X(100)   VALUE
                   "　ａ　ベッドの可動性".
      *
               05  ADL-02FLS     PIC X(08)    VALUE  "00000000".
               05  ADL-02FLE     PIC X(08)    VALUE  "99999999".
               05  ADL-02FL01    PIC X(09)    VALUE  "840000127".
               05  ADL-02FL02    PIC 9(04)    VALUE  1002.
               05  ADL-02FL03    PIC X(01)    VALUE  "b".
               05  ADL-02FL04    PIC X(100)   VALUE
                   "　ｂ　移乗".
      *
               05  ADL-03FLS     PIC X(08)    VALUE  "00000000".
               05  ADL-03FLE     PIC X(08)    VALUE  "99999999".
               05  ADL-03FL01    PIC X(09)    VALUE  "840000128".
               05  ADL-03FL02    PIC 9(04)    VALUE  1003.
               05  ADL-03FL03    PIC X(01)    VALUE  "c".
               05  ADL-03FL04    PIC X(100)   VALUE
                   "　ｃ　食事".
      *
               05  ADL-04FLS     PIC X(08)    VALUE  "00000000".
               05  ADL-04FLE     PIC X(08)    VALUE  "99999999".
               05  ADL-04FL01    PIC X(09)    VALUE  "840000129".
               05  ADL-04FL02    PIC 9(04)    VALUE  1004.
               05  ADL-04FL03    PIC X(01)    VALUE  "d".
               05  ADL-04FL04    PIC X(100)   VALUE
                   "　ｄ　トイレの使用".
      *
           03  ADL-R    REDEFINES      ADL-VAL.
               05  ADL-OCC             OCCURS  4.
                   07  ADL-STYMD.
                       09  ADL-STYM    PIC X(06).
                       09  ADL-STDD    PIC 9(02).
                   07  ADL-EDYMD.
                       09  ADL-EDYM    PIC X(06).
                       09  ADL-EDDD    PIC 9(02).
                   07  ADL-CDL         PIC X(09).
                   07  ADL-ZAINUM      PIC 9(04).
                   07  ADL-ID          PIC X(01).
                   07  MEDH20-MEIL     PIC X(100).
       01  ADL-MAX                     PIC 9(03)     VALUE  4.
