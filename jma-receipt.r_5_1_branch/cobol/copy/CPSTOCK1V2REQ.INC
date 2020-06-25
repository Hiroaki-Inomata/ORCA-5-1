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
      *    ＡＰＩリクエスト在庫管理取得 (xml2)
      *                                        COPY (CPSTOK1V02REQ.INC)
      ******************************************************************
        01  XML-STOCKREQ.
            02  STOCKREQ-CONTEXT           PIC S9(9)   BINARY.
            02  STOCKREQ-OBJECT            PIC X(36).
            02  STOCKREQ-MODE              PIC S9(9)   BINARY.
            02  STOCKREQ-RECORDNAME        PIC X(128).
            02  STOCKREQ-MEDICALREQ.
      *       リクエスト番号
              03  STOCKREQ-REQUEST-NUMBER    PIC X(2).
      *       UID（電子カルテ）
              03  STOCKREQ-KARTE-UID         PIC X(36).
      *       オルカUID
              03  STOCKREQ-ORCA-UID          PIC X(36).
      *       基準日
              03  STOCKREQ-BASE-DATE         PIC X(10).
      *       入外区分（外来のみ）
              03  STOCKREQ-INOUT             PIC X(01).
      *       テスト患者区分(True:含む）
              03  STOCKREQ-CONTAIN-TESTPATIENT
                                             PIC X(04).
      *
