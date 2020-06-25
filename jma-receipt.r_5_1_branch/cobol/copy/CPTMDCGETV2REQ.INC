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
      *    ＡＰＩリクエスト中途データ一覧情報取得 (xml2)
      *                                      COPY (CPTMDCGETV2REQ.INC)
      ******************************************************************
        01  XML-TMEDICALGETREQ.
            02  TMDCREQ-CONTEXT           PIC S9(9)   BINARY.
            02  TMDCREQ-OBJECT            PIC X(36).
            02  TMDCREQ-MODE              PIC S9(9)   BINARY.
            02  TMDCREQ-RECORDNAME        PIC X(128).
            02  TMDCREQ-TMEDICALREQ.
      *       診療日付
              03  TMDCREQ-PERFORM-DATE      PIC X(10).
      *       入外区分（１：入院）
              03  TMDCREQ-OUTPATIENT-CLASS  PIC X(1).
      *       診療科
              03  TMDCREQ-DEP-CODE          PIC X(02).
      *       患者番号
              03  TMDCREQ-PATIENTID         PIC X(20).
      *
