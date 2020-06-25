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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩ症状詳記情報取得　 (xml2)
      *                                   COPY (CPSUBJECTLSTV2REQ.INC)
      ******************************************************************
        01  XML-SUBJECTLSTREQ.
            03  SUBJETREQ-CONTEXT           PIC S9(9)   BINARY.
            03  SUBJETREQ-OBJECT            PIC X(36).
            03  SUBJETREQ-MODE              PIC S9(9)   BINARY.
            03  SUBJETREQ-RECORDNAME        PIC X(128).
            03  SUBJETREQ-MEDICALREQ.
      *       リクエスト番号
              05  SUBJETREQ-REQUEST-NUMBER     PIC X(2).
      *       患者番号
              05  SUBJETREQ-PATIENTID          PIC X(20).
      *       診療年月
              05  SUBJETREQ-PERFORM-DATE       PIC X(07).
      *
      *       入外区分
              05  SUBJETREQ-OUTPATIENT-CLASS   PIC X(1).
      *       診療科
              05  SUBJETREQ-DEP-CODE           PIC X(02).
      *       保険組合せ番号
              05  SUBJETREQ-COMBINATION-NUMBER PIC X(04).
      *       診療日
              05  SUBJETREQ-PERFORM-DAY        PIC X(02).
      *       症状詳記区分コード
              05  SUBJETREQ-SUBJECTIVES-CODE   PIC X(02).
      *       連番
              05  SUBJETREQ-RENNUM             PIC X(02).
      *
