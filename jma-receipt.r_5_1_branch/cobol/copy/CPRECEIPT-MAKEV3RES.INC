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
      *                                   COPY (CPRECEIPT-MAKEV3RES.INC)
      ******************************************************************
       01  XML-RECEIPT-MAKERES.
           02  RECEMAKES-CONTEXT           PIC S9(9)   BINARY.
           02  RECEMAKES-OBJECT            PIC X(36).
           02  RECEMAKES-MODE              PIC S9(9)   BINARY.
           02  RECEMAKES-RECORDNAME        PIC X(128).
           02  RECEMAKES-RECEIPT-MAKE-RES.
      *        リクエスト番号
               03  RECEMAKES-REQUEST-NUMBER    PIC X(2).
      *
               03  RECEMAKES-RESPONS-NUMBER    PIC X(2).
      *        カルテUID
               03  RECEMAKES-KARTE-UID         PIC X(36).
      *        オルカUID
               03  RECEMAKES-ORCA-UID          PIC X(36).
      *        実施日
               03  RECEMAKES-INFORMATION-DATE  PIC X(10).
      *        実施時間
               03  RECEMAKES-INFORMATION-TIME  PIC X(8).
      *        返却コード
               03  RECEMAKES-API-RESULT        PIC X(3).
      *        返却メッセージ
               03  RECEMAKES-API-RESULT-MSG    PIC X(100).
      *        ワーニングコード
               03  RECEMAKES-API-WARNING       PIC X(3).
      *        ワーニングメッセージ
               03  RECEMAKES-API-WARNING-MSG   PIC X(100).
      *
               03  RECEMAKES-RESKEY            PIC X(100).
      *
               03  RECEMAKES-PERFORM-MONTH     PIC X(7).
      *
               03  RECEMAKES-PERFORM-TITLE     PIC X(100).
      *        エラー患者一覧
               03  RECEMAKES-ERR-INFORMATION       OCCURS  100.
                   05  RECEMAKES-PATIENT-ID   PIC X(20).
                   05  RECEMAKES-PERFORM-DATE PIC X(7).
                   05  RECEMAKES-ERROR-CODE   PIC X(3).
                   05  RECEMAKES-ERROR-MSG    PIC X(100).
      *        印刷順
               03  RECEMAKES-RECEIPT-PRINT-ORDER
                                               PIC X(30)
                                                       OCCURS  3.
      *        総件数
               03  RECEMAKES-ALL-COUNT             PIC X(5).
      *        総枚数
               03  RECEMAKES-ALL-NUMBER-OF-SHEETS  PIC X(5).
      *
               03  RECEMAKES-RECEIPT-INFORMATION.
                   05  RECEMAKES-SUBMISSION-INFO   OCCURS  19.
      *                連番
                       07  RECEMAKES-SUBMISSION-POSITION
                                                   PIC X(5).
      *
                       07  RECEMAKES-SUBMISSION-TITLE
                                                   PIC X(30).
                       07  RECEMAKES-DETAIL-INFORMATION OCCURS  36.
      *                    連番
                           09  RECEMAKES-DETAIL-POSITION
                                                       PIC X(5).
      *                    提出先
                           09  RECEMAKES-SUBMISSION-IDENT
                                                       PIC X(2).
      *                    種別等コード
                           09  RECEMAKES-SUBMISSION-CODE
                                                       PIC X(10).
      *                    種別
                           09  RECEMAKES-SUBMISSION-TYPE
                                                       PIC X(100).
      *                    件数
                           09  RECEMAKES-COUNT         PIC X(5).
      *                    枚数
                           09  RECEMAKES-NUMBER-OF-SHEETS PIC X(5).
