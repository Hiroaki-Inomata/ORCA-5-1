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
      *    ＡＰＩリクエストお薬手帳情報(xml2)
      *                                  COPY (CPOKUSURIGETV2REQ.INC)
      ******************************************************************
       01  XML-OKUSURIGETREQ.
           03  OKUSURIGETREQ-CONTEXT         PIC S9(9)   BINARY.
           03  OKUSURIGETREQ-OBJECT          PIC X(36).
           03  OKUSURIGETREQ-MODE            PIC S9(9)   BINARY.
           03  OKUSURIGETREQ-RECORDNAME      PIC X(128).
           03  OKUSURIGETREQ-OKUSURIGETV2REQ.
      *        患者番号
               05  OKUSURIGETREQ-PATIENTID   PIC X(20).
      *        連番
               05  OKUSURIGETREQ-RENNUM      PIC X(04).
      *        グループ
               05  OKUSURIGETREQ-TBL-GROUP   PIC X(14).
      *        UUID
               05  OKUSURIGETREQ-TBL-UUID    PIC X(36).
      *        処理内連番
               05  OKUSURIGETREQ-SHORI-RENNUM    PIC X(04).
