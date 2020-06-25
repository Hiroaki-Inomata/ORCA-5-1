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
      *    ＡＰＩリクエスト患者予約状況(xml2)
      *                                  COPY (CPAPPTLST2V2REQ.INC)
      ******************************************************************
        01  XML-APPOINTLST2REQ.
            03  APLST2REQ-CONTEXT           PIC S9(9)   BINARY.
            03  APLST2REQ-OBJECT            PIC X(36).
            03  APLST2REQ-MODE              PIC S9(9)   BINARY.
            03  APLST2REQ-RECORDNAME        PIC X(128).
            03  APLST2REQ-APPOINT.
      *       患者番号
              05  APLST2REQ-PATIENTID         PIC X(20).
      *       基準日
              05  APLST2REQ-BASE-DATE         PIC X(10).
