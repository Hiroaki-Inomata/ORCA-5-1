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
      *    医療区分・ＡＤＬ点数情報 
      *    COPY (CPORCSNINF04.INC)
      ******************************************************************
       01  SNINF04-AREA.
           03  SNINF04-RESPONSE-AREA.
               05  SNINF04-PTINF-AREA.
                   07  SNINF04-PTNUM               PIC X(20).
                   07  SNINF04-NAME                PIC X(100).
                   07  SNINF04-KANANAME            PIC X(100).
                   07  SNINF04-BIRTHDAY            PIC X(10).
                   07  SNINF04-SEX                 PIC X(01).
               05  SNINF04-NYUTAIINYMD-AREA.
                   07  SNINF04-NYUTAIINYMD-OCC     OCCURS  5.
                       09  SNINF04-NYUINYMD        PIC X(10).
                       09  SNINF04-TAIINYMD        PIC X(10).
               05  SNINF04-SRYYM                   PIC X(07).
               05  SNINF04-MEDKBN-AREA.
                   07  SNINF04-MEDKBN-MAX          PIC 9(03).
                   07  SNINF04-MEDKBN-OCC          OCCURS  100.
                       09  SNINF04-MEDKBN-LV       PIC X(1).
                       09  SNINF04-MEDKBN-ID       PIC X(2).
                       09  SNINF04-MEDKBN-NAME     PIC X(100).
                       09  SNINF04-MEDKBN-MON      PIC X(1).
                       09  SNINF04-MEDKBN-DAY      PIC X(128).
               05  SNINF04-ADLKBN-AREA.
                   07  SNINF04-ADLKBN-OCC          OCCURS  4.
                       09  SNINF04-ADLKBN-ID       PIC X(1).
                       09  SNINF04-ADLKBN-NAME     PIC X(100).
                       09  SNINF04-ADLKBN-DAY      PIC X(128).
               05  SNINF04-MEDKBN-LV-DAY           PIC X(128).
               05  SNINF04-ADLKBN-TTL-DAY          PIC X(128).
               05  SNINF04-PTEVAL-AREA.
                   07  SNINF04-PTEVAL-DAY          PIC X(128).
                   07  SNINF04-PTEVAL-DIFF         PIC X(128).
               05  SNINF04-DESIGNR-AREA.
                   07  SNINF04-DESIGNR-DAY         PIC X(128).
