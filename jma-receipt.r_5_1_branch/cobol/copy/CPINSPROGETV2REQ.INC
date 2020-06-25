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
      *    ＡＰＩレスポンス保険者情報         COPY (CPINSPROV2REQ.INC)
      ******************************************************************
        01  XML-INSPROGETREQ.
            02  INSPROREQ-CONTEXT            PIC S9(9)   BINARY.
            02  INSPROREQ-OBJECT             PIC X(36).
            02  INSPROREQ-MODE               PIC S9(9)   BINARY.
            02  INSPROREQ-RECORDNAME         PIC X(128).
            02  INSPROREQ-INSURANCE.
      *       保険者番号
              03  INSPROREQ-INSPRO-NUMBER    PIC X(08).
      *       保険番号
              03  INSPROREQ-INS-NUMBER       PIC X(03).
      *
