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
      *
      *     エラーファイル名称領域      CPERRFL.INC
      *            
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  ERRFLPARA.
           03  ERRFLPARA-OLD.
               05  FILLER                  PIC X(05)   VALUE  "/tmp/".
               05  ERRFLPARA-BASENAME.
                   07  ERRFLPARA-HOSPNUM   PIC 9(02).
                   07  ERRFLPARA-SYOKBN1   PIC 9(01)   VALUE   0.
                   07  ERRFLPARA-SYOKBN2   PIC 9(01)   VALUE   0.
                   07  ERRFLPARA-FILE-ID   PIC X(08).
                   07  ERRFLPARA-TERMID    PIC X(16).
                   07  ERRFLPARA-FILL1     PIC X(04)   VALUE   ".dat".
               05  FILLER                  PIC X(987)  VALUE    SPACE.
           03  ERRFLPARA-FULLNAME-R REDEFINES ERRFLPARA-OLD.
               05  ERRFLPARA-FULLNAME      PIC X(1024).
