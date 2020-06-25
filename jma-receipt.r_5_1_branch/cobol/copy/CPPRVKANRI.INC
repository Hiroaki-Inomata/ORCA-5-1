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
      ************************************************************
      *    プレビュー印刷用データ     COPY (CPPREVIEW.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
        01  PRVKANRI-REC.
            02  PRVKANRI-HOSPNUM   PIC 9(2).
            02  PRVKANRI-TBL-KEY    PIC X(8).
            02  PRVKANRI-RENNUM PIC S9(4).
            02  PRVKANRI-TBL-GROUP  PIC X(14).
            02  PRVKANRI-TBL-UUID   PIC X(36).
            02  PRVKANRI-SHORI-RENNUM   PIC S9(4).
            02  PRVKANRI-GYOUMU-CD  PIC 9(03).
            02  PRVKANRI-SHELLID    PIC X(20).
            02  PRVKANRI-PRTID  PIC X(20).
            02  PRVKANRI-SRYYM  PIC X(6).
            02  PRVKANRI-SKYYMD PIC X(8).
            02  PRVKANRI-PTID   PIC S9(10).
            02  PRVKANRI-PAGE   PIC S9(5).
            02  PRVKANRI-MOD-FLG    PIC X(1).
            02  PRVKANRI-SHORI-MOD-FLG    PIC X(1).
            02  PRVKANRI-FILENAME   PIC X(200).
            02  PRVKANRI-TITLE  PIC X(100).
            02  PRVKANRI-PRTNM  PIC X(16).
	    02  PRVKANRI-PTNUM  PIC X(20).
	    02  PRVKANRI-MEMO   PIC X(100).
	    02  PRVKANRI-RECE-FLG   PIC 9(4).
	    02  PRVKANRI-SHORI-KBN  PIC X(1).
            02  PRVKANRI-T-G.
              03  PRVKANRI-TBL-GROUP-RRK  PIC X(14) OCCURS 5.
            02  PRVKANRI-TERMID PIC X(32).
            02  PRVKANRI-OPID   PIC X(16).
            02  PRVKANRI-CREYMD PIC X(8).
            02  PRVKANRI-UPYMD  PIC X(8).
            02  PRVKANRI-UPHMS  PIC X(6).
      *検索用項目
            02  PRVKANRIXX-STPAGE       PIC 9(05).
            02  PRVKANRIXX-EDPAGE       PIC 9(05).    
