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
      *    ＡＰＩレスポンスお薬手帳情報
      *    COPY (CPOKUSURIPUTV2RES.INC)
      ******************************************************************
       01  XML-OKUSURIPUTRES.
           03  OKUSURIPUTRES-CONTEXT     PIC S9(9)   BINARY.
           03  OKUSURIPUTRES-OBJECT      PIC X(36).
           03  OKUSURIPUTRES-MODE        PIC S9(9)   BINARY.
           03  OKUSURIPUTRES-RECORDNAME  PIC X(128).
      *
           03  OKUSURIPUTRES-OKUSURIPUTV2RES.
               05  OKUSURIPUTRES-INFORMATION-DATE    PIC X(10).
               05  OKUSURIPUTRES-INFORMATION-TIME    PIC X(08).
               05  OKUSURIPUTRES-API-RESULT          PIC X(04).
               05  OKUSURIPUTRES-API-RESULT-MSG      PIC X(100).
