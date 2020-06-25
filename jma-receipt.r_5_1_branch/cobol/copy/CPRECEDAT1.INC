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
      *     レセプト作成用中間ファイル名称領域      CPRECEDAT1.INC
      *            
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  RECEDAT1PARA-AREA.
           03  RECEDAT1PARA.
               05  RECEDAT1PARA-OLD.
                   07  FILLER                      PIC X(05)
                                                       VALUE   "/tmp/".
                   07  RECEDAT1PARA-BASENAME.
                       09  RECEDAT1PARA-HOSPNUM    PIC 9(02).
                       09  RECEDAT1PARA-SYOKBN     PIC 9(01)
                                                       VALUE   0.
                       09  RECEDAT1PARA-RECEKBN    PIC 9(01)
                                                       VALUE   0.
                       09  RECEDAT1PARA-FILE-ID    PIC X(07).
                       09  RECEDAT1PARA-TERMID     PIC X(16).
                   07  FILLER                      PIC X(992)
                                                       VALUE   SPACE.
               05  RECEDAT1PARA-FULLNAME-R REDEFINES RECEDAT1PARA-OLD.
                   07  RECEDAT1PARA-FULLNAME      PIC X(1024).
