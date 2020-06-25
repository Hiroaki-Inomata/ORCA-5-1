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
      *         チェックマスタ５    COPY   (CPCHK005.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      * 01  CHK005-REC.
            03  CHK005-KEY.
                05  CHK005-HOSPNUM  PIC 9(2).
                05  CHK005-CHKKBN   PIC X(1).
                05  CHK005-ID   PIC S9(9)   BINARY.
                05  CHK005-YUKOSTYMD    PIC X(8).
                05  CHK005-YUKOEDYMD    PIC X(8).
                05  CHK005-CDKBN    PIC X(1).
                05  CHK005-RENNUM   PIC S9(9)   BINARY.
            03  CHK005-SRYCD    PIC X(9).
            03  CHK005-CD   PIC X(170).
            03  CHK005-BYOMEI   PIC X(160).
            03  CHK005-COMPKBN  PIC X(1).
            03  CHK005-BYOMEIHENFLG PIC X(1).
            03  CHK005-NYUGAIKBN    PIC X(1).
            03  CHK005-NYUGAIBNDKBN       PIC  X(1).
            03  CHK005-DAYMONTHKBN        PIC  X(1).
            03  CHK005-TERMID   PIC X(16).
            03  CHK005-OPID PIC X(16).
            03  CHK005-CREYMD   PIC X(8).
            03  CHK005-UPYMD    PIC X(8).
            03  CHK005-UPHMS    PIC X(6).
            03  CHK005-UPHOSPNUM  PIC 9(2).
            03  CHK005-UPCHKKBN PIC X(1).
            03  CHK005-UPID  PIC S9(9)   BINARY.
            03  CHK005-UPRENNUM PIC S9(9)   BINARY.
            03  CHK005-SELNAME  PIC X(200).
