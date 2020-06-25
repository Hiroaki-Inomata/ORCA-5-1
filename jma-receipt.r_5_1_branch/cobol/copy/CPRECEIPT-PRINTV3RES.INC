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
      *                                   COPY (CPRECEIPT-PRINTV3RES.INC)
      ******************************************************************
       01  XML-RECEIPT-PRINTRES.
           02  RECEPRINTS-CONTEXT           PIC S9(9)   BINARY.
           02  RECEPRINTS-OBJECT            PIC X(36).
           02  RECEPRINTS-MODE              PIC S9(9)   BINARY.
           02  RECEPRINTS-RECORDNAME        PIC X(128).
           02  RECEPRINTS-RECEIPT-PRINT-RES.
      *        リクエスト番号
               03  RECEPRINTS-REQUEST-NUMBER    PIC X(2).
      *
               03  RECEPRINTS-RESPONS-NUMBER    PIC X(2).
      *        カルテUID
               03  RECEPRINTS-KARTE-UID         PIC X(36).
      *        オルカUID
               03  RECEPRINTS-ORCA-UID          PIC X(36).
      *        実施日
               03  RECEPRINTS-INFORMATION-DATE  PIC X(10).
      *        実施時間
               03  RECEPRINTS-INFORMATION-TIME  PIC X(8).
      *        返却コード
               03  RECEPRINTS-API-RESULT        PIC X(3).
      *        返却メッセージ
               03  RECEPRINTS-API-RESULT-MSG    PIC X(100).
      *        ワーニングコード
               03  RECEPRINTS-API-WARNING       PIC X(3).
      *        ワーニングメッセージ
               03  RECEPRINTS-API-WARNING-MSG   PIC X(100).
      *
               03  RECEPRINTS-RESKEY            PIC X(100).
      *
               03  RECEPRINTS-PERFORM-MONTH     PIC X(7).
      *
               03  RECEPRINTS-DATA-ID-INFORMATION  OCCURS  10.
                   05  RECEPRINTS-DATA-ID          PIC X(37).
                   05  RECEPRINTS-PRINT-ID         PIC X(37).
      *
               03  RECEPRINTS-ALL-NUMBER-OF-SHEETS
                                                PIC X(5).
      *        エラー明細一覧
               03  RECEPRINTS-ERR-INFORMATION.
                   05  RECEPRINTS-SUBMISSION OCCURS  19.
      *                連番
                       07  RECEPRINTS-SUBMISSION-POSITION
                                               PIC X(5).
      *                エラー
                       07  RECEPRINTS-ERROR-CODE
                                               PIC X(3).
      *
                       07  RECEPRINTS-DETAIL-INFORMATION OCCURS  36.
      *                    連番
                           09  RECEPRINTS-DETAIL-POSITION
                                                   PIC X(5).
      *                    印刷指示(Yes :印刷 それ以外 :印刷しない)
                           09  RECEPRINTS-PRINT-INSTRUCT
                                                   PIC X(5).
      *                    提出先
                           09  RECEPRINTS-SUBMISSION-IDENT
                                                   PIC X(2).
      *                    種別等コード
                           09  RECEPRINTS-SUBMISSION-CODE
                                                   PIC X(10).
      *                    種別
                           09  RECEPRINTS-SUBMISSION-TYPE
                                                   PIC X(100).
      *                    件数
                           09  RECEPRINTS-COUNT    PIC X(5).
      *                    枚数
                           09  RECEPRINTS-NUMBER-OF-SHEETS
                                                   PIC X(5).
      *                    エラー
                           09  RECEPRINTS-DETAIL-ERROR-CODE
                                                   PIC X(3).
