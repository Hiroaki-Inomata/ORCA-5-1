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
      *    ＡＰＩ患者登録情報（保険・公費の種類、補助区分）(xml2)
      *                                   COPY (CPINSURANCEINF1V2REQ.INC)
      ******************************************************************
        01  XML-INSURANCEINFREQ.
            02  INSUINFQ-CONTEXT           PIC S9(9)   BINARY.
            02  INSUINFQ-OBJECT            PIC X(36).
            02  INSUINFQ-MODE              PIC S9(9)   BINARY.
            02  INSUINFQ-RECORDNAME        PIC X(128).
            02  INSUINFQ-PATIENTREQ.
      *
      *       リクエスト番号
              03  INSUINFQ-REQUEST-NUMBER      PIC X(2).
      *       基準日付
              03  INSUINFQ-BASE-DATE           PIC X(10).
      *
      *
