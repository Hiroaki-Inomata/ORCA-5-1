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
      *    ＡＰＩリクエスト患者保険組合せ取得　(xml2)
      *                                 COPY (CPPATIENTLST6V2REQ.INC)
      ******************************************************************
        01  XML-PATIENTLST6REQ.
            03  PTREQ-CONTEXT          PIC S9(9)   BINARY.
            03  PTREQ-OBJECT           PIC X(36).
            03  PTREQ-MODE             PIC S9(9)   BINARY.
            03  PTREQ-RECORDNAME       PIC X(128).
      *     患者情報
            03  PTREQ-PATIENTLST6.
      *         処理区分
                05  PTREQ-REQUEST-NUMBER   PIC X(02).
      *         患者番号
                05  PTREQ-PATIENTID        PIC X(20).
      *         基準日付
                05  PTREQ-BASE-DATE        PIC X(10).
      *         取得開始年月
                05  PTREQ-START-DATE       PIC X(07).
      *         取得終了年月
                05  PTREQ-END-DATE         PIC X(07).
      *
