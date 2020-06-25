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
      ******************************************************************
      *    ＡＰＩリクエスト在庫管理取得 (xml2)
      *                                        COPY (CPSTOCK1V02RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
        01  XML-STOCKRES.
            03  STOCKRES-CONTEXT             PIC S9(9)   BINARY.
            03  STOCKRES-OBJECT              PIC X(36).
            03  STOCKRES-MODE                PIC S9(9)   BINARY.
            03  STOCKRES-RECORDNAME          PIC X(128).
            03  STOCKRES-MEDICALRES.
      *       リクエスト番号
              05  STOCKRES-REQUEST-NUMBER    PIC X(2).
      *       レスポンス番号
              05  STOCKRES-RESPONSE-NUMBER   PIC X(2).
      *       UID（電子カルテ）
              05  STOCKRES-KARTE-UID         PIC X(36).
      *       オルカUID
              05  STOCKRES-ORCA-UID          PIC X(36).
      *       実施日
              05  STOCKRES-INFORMATION-DATE  PIC X(10).
      *       実施時間
              05  STOCKRES-INFORMATION-TIME  PIC X(8).
      *       返却コード
              05  STOCKRES-API-RESULT        PIC X(03).
      *       返却メッセージ
              05  STOCKRES-API-RESULT-MSG    PIC X(100).
      *
              05  STOCKRES-RESKEY            PIC X(100).
      *       基準日付
              05  STOCKRES-BASE-DATE         PIC X(10).
      *       入外区分（外来のみ）
              05  STOCKRES-INOUT             PIC X(01).
      *       テスト患者区分(True:含む）
              05  STOCKRES-CONTAIN-TESTPATIENT
                                             PIC X(04).
      *       件数
              05  STOCKRES-OUTPUT-COUNT      PIC X(10).
      *
              05  STOCKRES-DATA-ID-INFORMATION   OCCURS  10.
                  07  STOCKRES-DATA-ID       PIC X(37).
                  07  STOCKRES-PRINT-ID      PIC X(37).
