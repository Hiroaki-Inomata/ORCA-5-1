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
      *    ＡＰＩリクエスト患者一覧取得 (xml2)
      *                              COPY (CPPATIENTLST1V2SEQ.INC)
      ******************************************************************
        01  XML-PATIENTLST1REQ.
            03  PATLSTREQ-CONTEXT          PIC S9(9)   BINARY.
            03  PATLSTREQ-OBJECT           PIC X(36).
            03  PATLSTREQ-MODE             PIC S9(9)   BINARY.
            03  PATLSTREQ-RECORDNAME       PIC X(128).
            03  PATLSTREQ-PATIENTINFOREQ.
      *       対象開始日付
              05  PATLSTREQ-START-DATE     PIC X(10).
      *       対象終了日付
              05  PATLSTREQ-END-DATE       PIC X(10).
      *       テスト患者区分(1:含まない）
              05  PATLSTREQ-TEST-KBN       PIC X(01).
      *
