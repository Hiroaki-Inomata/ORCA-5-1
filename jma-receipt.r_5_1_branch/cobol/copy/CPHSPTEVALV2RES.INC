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
      *    ＡＰＩレスポンス入院医療区分・ＡＤＬ点数情報 
      *    COPY (CPHSPTEVALV2RES.INC)
      ******************************************************************
       01  XML-HSPTEVALRES.
           03  HSPTEVALRES-CONTEXT                 PIC S9(9)   BINARY.
           03  HSPTEVALRES-OBJECT                  PIC X(36).
           03  HSPTEVALRES-MODE                    PIC S9(9)   BINARY.
           03  HSPTEVALRES-RECORDNAME              PIC X(128).
      *
           03  HSPTEVALRES-PRIVATE-OBJECTS.
               05  HSPTEVALRES-INFORMATION-DATE        PIC X(10).
               05  HSPTEVALRES-INFORMATION-TIME        PIC X(8).
               05  HSPTEVALRES-API-RESULT              PIC X(2).
               05  HSPTEVALRES-API-RESULT-MSG          PIC X(100).
               05  HSPTEVALRES-PTINF-AREA.
                   07  HSPTEVALRES-PTNUM               PIC X(20).
                   07  HSPTEVALRES-NAME                PIC X(100).
                   07  HSPTEVALRES-KANANAME            PIC X(100).
                   07  HSPTEVALRES-BIRTHDAY            PIC X(10).
                   07  HSPTEVALRES-SEX                 PIC X(01).
               05  HSPTEVALRES-NYUTAIINYMD-AREA.
                   07  HSPTEVALRES-NYUTAIINYMD-OCC     OCCURS  5.
                       09  HSPTEVALRES-NYUINYMD        PIC X(10).
                       09  HSPTEVALRES-TAIINYMD        PIC X(10).
               05  HSPTEVALRES-SRYYM                   PIC X(07).
               05  HSPTEVALRES-MEDKBN-AREA.
                   07  HSPTEVALRES-MEDKBN-OCC          OCCURS  50.
                       09  HSPTEVALRES-MEDKBN-LV       PIC X(1).
                       09  HSPTEVALRES-MEDKBN-ID       PIC X(2).
                       09  HSPTEVALRES-MEDKBN-NAME     PIC X(100).
                       09  HSPTEVALRES-MEDKBN-MON      PIC X(1).
                       09  HSPTEVALRES-MEDKBN-DAY      PIC X(128).
               05  HSPTEVALRES-ADLKBN-AREA.
                   07  HSPTEVALRES-ADLKBN-OCC          OCCURS  4.
                       09  HSPTEVALRES-ADLKBN-ID       PIC X(1).
                       09  HSPTEVALRES-ADLKBN-NAME     PIC X(100).
                       09  HSPTEVALRES-ADLKBN-DAY      PIC X(128).
               05  HSPTEVALRES-MEDKBN-LV-DAY           PIC X(128).
               05  HSPTEVALRES-ADLKBN-TTL-DAY          PIC X(128).
               05  HSPTEVALRES-PTEVAL-AREA.
                   07  HSPTEVALRES-PTEVAL-DAY          PIC X(128).
                   07  HSPTEVALRES-PTEVAL-DIFF         PIC X(128).
               05  HSPTEVALRES-DESIGNR-AREA.
                   07  HSPTEVALRES-DESIGNR-DAY         PIC X(128).
