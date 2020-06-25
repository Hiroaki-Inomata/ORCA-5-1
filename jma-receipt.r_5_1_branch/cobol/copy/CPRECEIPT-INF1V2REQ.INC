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
      *    ＡＰＩレセプト情報（レセプトの枚数、点数）(xml2)
      *                                   COPY (CPRECEIPT-INF1V2REQ.INC)
      ******************************************************************
        01  XML-RECEIPT-INFREQ.
            02  RECEIPT-INFQ-CONTEXT       PIC S9(9)   BINARY.
            02  RECEIPT-INFQ-OBJECT        PIC X(36).
            02  RECEIPT-INFQ-MODE          PIC S9(9)   BINARY.
            02  RECEIPT-INFQ-RECORDNAME    PIC X(128).
            02  RECEIPT-INFQ-PATIENTREQ.
      *         診療年月
                03  RECEIPT-INFQ-PERFORM-MONTH PIC X(7).
      *
