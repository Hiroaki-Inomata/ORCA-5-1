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
      ********************************************************************
      *    ＡＰＩレスポンス帳票データ取得情報 COPY (CPFORMDATAGETV2RES.INC)
      ********************************************************************
       01  XML-FDGRES.
           03  FDGRES-CONTEXT                     PIC S9(9)   BINARY.
           03  FDGRES-OBJECT                      PIC X(36).
           03  FDGRES-MODE                        PIC S9(9)   BINARY.
           03  FDGRES-RECORDNAME                  PIC X(128).
      *
           03  FDGRES-HEADER.
               05  FDGRES-INFORMATION-DATE        PIC X(10).
               05  FDGRES-INFORMATION-TIME        PIC X(8).
               05  FDGRES-API-RESULT              PIC X(4).
               05  FDGRES-API-RESULT-MSG          PIC X(100).
               05  FDGRES-FORMID                  PIC X(20).
               05  FDGRES-FORMNAME                PIC X(100).
               05  FDGRES-PRTYMD                  PIC X(10).
               05  FDGRES-PRTHMS                  PIC X(08).
               05  FDGRES-PTINF-AREA.
                   07  FDGRES-PTNUM               PIC X(20).
                   07  FDGRES-NAME                PIC X(50).
                   07  FDGRES-KANANAME            PIC X(50).
                   07  FDGRES-BIRTHDAY            PIC X(10).
                   07  FDGRES-SEX                 PIC X(01).
               05  FDGRES-SRYYMD                  PIC X(10).
               05  FDGRES-DATAID                  PIC X(80).
               05  FDGRES-RPDATA                  PIC X(36).
               05  FDGRES-OFFSET                  PIC X(10).
