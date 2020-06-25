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
      *                                   COPY (CPRECEIPT-INF1V2RES.INC)
      ******************************************************************
       01  XML-RECEIPT-INFRES.
           02  RECEIPT-INFS-CONTEXT            PIC S9(9)   BINARY.
           02  RECEIPT-INFS-OBJECT             PIC X(36).
           02  RECEIPT-INFS-MODE               PIC S9(9)   BINARY.
           02  RECEIPT-INFS-RECORDNAME         PIC X(128).
           02  RECEIPT-INFS-RECEIPT-INF-RES.
      *        実施日
               03  RECEIPT-INFS-INFORMATION-DATE   PIC X(10).
      *        実施時間
               03  RECEIPT-INFS-INFORMATION-TIME   PIC X(8).
      *        返却コード
               03  RECEIPT-INFS-API-RESULT         PIC X(2).
      *        返却メッセージ
               03  RECEIPT-INFS-API-RESULT-MSG     PIC X(100).
               03  RECEIPT-INFS-RESKEY             PIC X(100).
      *
      *        レセプト情報
               03  RECEIPT-INFS-INFO-G.
      *            診療年月
                   05  RECEIPT-INFS-PERFORM-MONTH PIC X(7).
      *            レセプト総枚数
                   05  RECEIPT-INFS-RECEIPT-COUNT PIC X(6).
      *            レセプト総点数
                   05  RECEIPT-INFS-RECEIPT-POINT PIC X(10).
