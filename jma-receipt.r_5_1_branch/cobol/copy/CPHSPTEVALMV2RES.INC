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
      *    ＡＰＩレスポンス医療区分・ＡＤＬ点数情報 
      *    COPY (CPHSPTEVALMRES.INC)
      ******************************************************************
       01  XML-HSPTEVALMRES.
           03  HSPTEVALMRES-CONTEXT              PIC S9(9)   BINARY.
           03  HSPTEVALMRES-OBJECT               PIC X(36).
           03  HSPTEVALMRES-MODE                 PIC S9(9)   BINARY.
           03  HSPTEVALMRES-RECORDNAME           PIC X(128).
           03  HSPTEVALMRES-PRIVATE-OBJECTS.
               05  HSPTEVALMRES-INFORMATION-DATE     PIC X(10).
               05  HSPTEVALMRES-INFORMATION-TIME     PIC X(8).
               05  HSPTEVALMRES-API-RESULT-OCC       OCCURS  10.
                   07  HSPTEVALMRES-API-RESULT       PIC X(4).
                   07  HSPTEVALMRES-API-RESULT-MSG   PIC X(100).
               05  HSPTEVALMRES-PTINF-AREA.
                   07  HSPTEVALMRES-PTNUM               PIC X(20).
                   07  HSPTEVALMRES-NAME                PIC X(100).
                   07  HSPTEVALMRES-KANANAME            PIC X(100).
                   07  HSPTEVALMRES-BIRTHDAY            PIC X(10).
                   07  HSPTEVALMRES-SEX                 PIC X(01).
               05  HSPTEVALMRES-NYUTAIINYMD-AREA.
                   07  HSPTEVALMRES-NYUTAIINYMD-OCC     OCCURS  5.
                       09  HSPTEVALMRES-NYUINYMD        PIC X(10).
                       09  HSPTEVALMRES-TAIINYMD        PIC X(10).
               05  HSPTEVALMRES-SRYYM                   PIC X(07).
               05  HSPTEVALMRES-MEDKBN-AREA.
                   07  HSPTEVALMRES-MEDKBN-OCC          OCCURS  50.
                       09  HSPTEVALMRES-MEDKBN-LV       PIC X(1).
                       09  HSPTEVALMRES-MEDKBN-ID       PIC X(2).
                       09  HSPTEVALMRES-MEDKBN-NAME     PIC X(100).
                       09  HSPTEVALMRES-MEDKBN-MON      PIC X(1).
                       09  HSPTEVALMRES-MEDKBN-DAY      PIC X(128).
               05  HSPTEVALMRES-ADLKBN-AREA.
                   07  HSPTEVALMRES-ADLKBN-OCC          OCCURS  4.
                       09  HSPTEVALMRES-ADLKBN-ID       PIC X(1).
                       09  HSPTEVALMRES-ADLKBN-NAME     PIC X(100).
                       09  HSPTEVALMRES-ADLKBN-DAY      PIC X(128).
               05  HSPTEVALMRES-MEDKBN-LV-DAY           PIC X(128).
               05  HSPTEVALMRES-ADLKBN-TTL-DAY          PIC X(128).
               05  HSPTEVALMRES-PTEVAL-AREA.
                   07  HSPTEVALMRES-PTEVAL-DAY          PIC X(128).
      *            07  HSPTEVALMRES-PTEVAL-DIFF         PIC X(128).
               05  HSPTEVALMRES-DESIGNR-AREA.
                   07  HSPTEVALMRES-DESIGNR-DAY         PIC X(128).
