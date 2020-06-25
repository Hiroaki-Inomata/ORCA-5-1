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
      *    ＡＰＩレセプト情報（レセプト印刷）(xml2)
      *                                   COPY (CPRECEIPT-PRINTV3REQ.INC)
      ******************************************************************
        01  XML-RECEIPT-PRINTREQ.
            02  RECEPRINTQ-CONTEXT         PIC S9(9)   BINARY.
            02  RECEPRINTQ-OBJECT          PIC X(36).
            02  RECEPRINTQ-MODE            PIC S9(9)   BINARY.
            02  RECEPRINTQ-RECORDNAME      PIC X(128).
            02  RECEPRINTQ-PATIENTREQ.
      *         リクエスト番号
                03  RECEPRINTQ-REQUEST-NUMBER PIC X(2).
      *         カルテUID
                03  RECEPRINTQ-KARTE-UID   PIC X(36).
      *         オルカUID
                03  RECEPRINTQ-ORCA-UID    PIC X(36).
      *         実施日
                03  RECEPRINTQ-PERFORM-DATE PIC X(10).
      *         診療年月
                03  RECEPRINTQ-PERFORM-MONTH PIC X(7).
      *         入外区分
                03  RECEPRINTQ-INOUT       PIC X(1).
      *         処理区分
                03  RECEPRINTQ-RECEIPT-MODE PIC X(10).
      *         印刷モード
                03  RECEPRINTQ-PRINT-MODE  PIC X(5).
      *         提出先
                03  RECEPRINTQ-SUBMISSION-MODE  PIC X(2).
      *         明細一覧
                03  RECEPRINTQ-RECEIPT-INFORMATION.
                   05  RECEPRINTQ-SUBMISSION OCCURS  19.
      *                連番
                       07  RECEPRINTQ-SUBMISSION-POSITION
                                               PIC X(5).
                       07  RECEPRINTQ-DETAIL-INFORMATION OCCURS  36.
      *                    連番
                           09  RECEPRINTQ-DETAIL-POSITION
                                                   PIC X(5).
      *                    印刷指示(Yes :印刷 それ以外 :印刷しない)
                           09  RECEPRINTQ-PRINT-INSTRUCT
                                                   PIC X(5).
      *                    提出先
                           09  RECEPRINTQ-SUBMISSION-IDENT
                                               PIC X(2).
      *                    種別等コード
                           09  RECEPRINTQ-SUBMISSION-CODE
                                                   PIC X(10).
      *                    種別
                           09  RECEPRINTQ-SUBMISSION-TYPE
                                                   PIC X(100).
      *                    件数
                           09  RECEPRINTQ-COUNT    PIC X(5).
      *                    枚数
                           09  RECEPRINTQ-NUMBER-OF-SHEETS
                                                   PIC X(5).
