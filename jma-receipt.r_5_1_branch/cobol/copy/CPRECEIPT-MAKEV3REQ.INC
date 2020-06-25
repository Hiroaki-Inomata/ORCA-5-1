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
      *    ＡＰＩレセプト情報（レセプト作成）(xml2)
      *                                   COPY (CPRECEIPT-MAKEV3REQ.INC)
      ******************************************************************
        01  XML-RECEIPT-MAKEREQ.
            02  RECEMAKEQ-CONTEXT          PIC S9(9)   BINARY.
            02  RECEMAKEQ-OBJECT           PIC X(36).
            02  RECEMAKEQ-MODE             PIC S9(9)   BINARY.
            02  RECEMAKEQ-RECORDNAME       PIC X(128).
            02  RECEMAKEQ-PATIENTREQ.
      *         リクエスト番号
                03  RECEMAKEQ-REQUEST-NUMBER PIC X(2).
      *         カルテUID
                03  RECEMAKEQ-KARTE-UID    PIC X(36).
      *         オルカUID
                03  RECEMAKEQ-ORCA-UID     PIC X(36).
      *         実施日
                03  RECEMAKEQ-PERFORM-DATE PIC X(10).
      *         診療年月
                03  RECEMAKEQ-PERFORM-MONTH PIC X(7).
      *         入外区分
                03  RECEMAKEQ-INOUT        PIC X(1).
      *         処理区分
                03  RECEMAKEQ-RECEIPT-MODE PIC X(10).
      *         印刷モード
                03  RECEMAKEQ-PRINT-MODE   PIC X(6).
      *         提出先
                03  RECEMAKEQ-SUBMISSION-MODE  PIC X(2).
      *         個別対象患者一覧
                03  RECEMAKEQ-PATIENT-INFORMATION  OCCURS  100.
      *             個別対象−患者番号
                    05  RECEMAKEQ-PATIENT-ID   PIC X(20).
      *             個別対象−診療年月
                    05  RECEMAKEQ-PATIENT-PERFORM-DATE  PIC X(7).
      *
