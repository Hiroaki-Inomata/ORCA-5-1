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
      *    ＡＰＩ中途データ一括取り込み（日時統計） (xml2)
      *                             COPY (CPTEMPMEDICALADDV3RES.INC)
      ******************************************************************
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
       01  XML-LSTRES.
           03  LSTRES-CONTEXT              PIC S9(9)   BINARY.
           03  LSTRES-OBJECT               PIC X(36).
           03  LSTRES-MODE                 PIC S9(9)   BINARY.
           03  LSTRES-RECORDNAME           PIC X(128).
           03  LSTRES-MEDICALRES.
      *        リクエスト番号
               05  LSTRES-REQUEST-NUMBER   PIC X(2).
      *        レスポンス番号
               05  LSTRES-RESPONSE-NUMBER  PIC X(2).
      *        UID（電子カルテ）
               05  LSTRES-KARTE-UID        PIC X(36).
      *        オルカUID
               05  LSTRES-ORCA-UID         PIC X(36).
      *        実施日
               05  LSTRES-INFORMATION-DATE PIC X(10).
      *        実施時間
               05  LSTRES-INFORMATION-TIME PIC X(8).
      *        返却コード
               05  LSTRES-API-RESULT       PIC X(03).
      *        返却メッセージ
               05  LSTRES-API-RESULT-MSG   PIC X(100).
      *
               05  LSTRES-RESKEY           PIC X(100).
      *
               05  LSTRES-PROCESSING-ERR-G.
                   07  LSTRES-PROCESSING-ERR-INFO  OCCURS  10.
                       09  LSTRES-ERR-PROGRAM-NAME PIC X(24).
                       09  LSTRES-ERR-PROGRAM-LABEL
                                                   PIC X(80).
                       09  LSTRES-ERR-PARAMETER-G.
                           11  LSTRES-ERR-PARAMETER-INFO   OCCURS  8.
                               13  LSTRES-ERR-PARM-NO  PIC X(1).
                               13  LSTRES-ERR-PARM-CLASS
                                                       PIC X(10).
                               13  LSTRES-ERR-PARM-VALUE
                                                       PIC X(20).
                       09  LSTRES-ERROR-CODE       PIC X(3).
                       09  LSTRES-ERROR-MSG        PIC X(100).
      *
               05  LSTRES-PROCESSING-G.
                   07  LSTRES-PROCESSING-INFO      OCCURS  10.
                       09  LSTRES-PROGRAM-NAME     PIC X(24).
                       09  LSTRES-PROGRAM-LABEL    PIC X(80).
      *        件数
               05  LSTRES-OUTPUT-COUNT     PIC X(10).
      *
              05  LSTRES-DATA-ID-INFORMATION   OCCURS  10.
                  07  LSTRES-DATA-ID       PIC X(37).
                  07  LSTRES-PRINT-ID      PIC X(37).
