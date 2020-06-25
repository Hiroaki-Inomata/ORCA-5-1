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
      *    ＡＰＩ中途データ一括取り込み（日時統計） (xml2)
      *                             COPY (CPTEMPMEDICALADDV3REQ.INC)
      ******************************************************************
       01  XML-LSTREQ.
           03  LSTREQ-CONTEXT              PIC S9(9)   BINARY.
           03  LSTREQ-OBJECT               PIC X(36).
           03  LSTREQ-MODE                 PIC S9(9)   BINARY.
           03  LSTREQ-RECORDNAME           PIC X(128).
           03  LSTREQ-MEDICALREQ.
      *        リクエスト番号
               05  LSTREQ-REQUEST-NUMBER   PIC X(2).
      *        UID（電子カルテ）
               05  LSTREQ-KARTE-UID        PIC X(36).
      *        オルカUID
               05  LSTREQ-ORCA-UID         PIC X(36).
      *
               05  LSTREQ-PROCESSING-G.
                   07  LSTREQ-PROCESSING-INFO      OCCURS  10.
                       09  LSTREQ-PROGRAM-NAME PIC X(24).
                       09  LSTREQ-PARAMETER-G.
                           11  LSTREQ-PARAMETER-INFO       OCCURS  8.
                               13  LSTREQ-PARM-NO      PIC X(1).
                               13  LSTREQ-PARM-CLASS   PIC X(10).
                               13  LSTREQ-PARM-VALUE   PIC X(20).
