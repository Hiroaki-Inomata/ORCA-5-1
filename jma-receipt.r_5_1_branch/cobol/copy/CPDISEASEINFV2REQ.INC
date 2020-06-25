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
      *    ＡＰＩリクエスト患者病名情報(xml2)
      *                                  COPY (CPDISEASEINFV2REQ.INC)
      ******************************************************************
        01  XML-DISEASE-INFREQ.
            02  DISEREQ-CONTEXT            PIC S9(9)   BINARY.
            02  DISEREQ-OBJECT             PIC X(36).
            02  DISEREQ-MODE               PIC S9(9)   BINARY.
            02  DISEREQ-RECORDNAME         PIC X(128).
            02  DISEREQ-DISEASE-INFREQ.
      *       患者番号
              03  DISEREQ-PATIENTID    PIC X(20).
      *       基準年月
              03  DISEREQ-BASE-DATE    PIC X(10).
      *       出力設定
              03  DISEREQ-SELECT-MODE  PIC X(3).
