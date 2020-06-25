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
       01  SKT3400.
           03         SKT3400-SEIYM            PIC  X(16).
           03         SKT3400-POST1            PIC  X(03).
           03         SKT3400-POST2            PIC  X(04).
           03         SKT3400-ADRS             PIC  X(100).
           03         SKT3400-HOSPNAME         PIC  X(100).
           03         SKT3400-HOSPCD           PIC  X(20).
           03         SKT3400-SRYKANAME        PIC  X(30) OCCURS 3.
           03         SKT3400-TOKUBETU         PIC  X(10).
           03         SKT3400-DATA-TBL         OCCURS  46.
             05       SKT3400-HKNJANUM         PIC  X(08).
             05       SKT3400-NYUIN.
               07     SKT3400-N-KENSU          PIC  X(06).
               07     SKT3400-N-TENSU          PIC  X(11).
             05       SKT3400-GAIRAI.
               07     SKT3400-G-KENSU          PIC  X(06).
               07     SKT3400-G-TENSU          PIC  X(11).
           03         SKT3400-TOTAL            OCCURS   2.
             05       SKT3400-TL-NYUIN.
               07     SKT3400-TL-N-KENSU       PIC  X(06).
               07     SKT3400-TL-N-TENSU       PIC  X(11).
             05       SKT3400-TL-GAIRAI.
               07     SKT3400-TL-G-KENSU       PIC  X(06).
               07     SKT3400-TL-G-TENSU       PIC  X(11).
