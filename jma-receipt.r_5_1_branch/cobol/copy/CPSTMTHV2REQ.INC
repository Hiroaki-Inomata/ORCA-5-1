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
      ******************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************
       01  STMTHREQ-AREA.
           03  STMTHREQ-CONTEXT             PIC S9(9)   BINARY.
           03  STMTHREQ-OBJECT              PIC X(36).
           03  STMTHREQ-MODE                PIC S9(9)   BINARY.
           03  STMTHREQ-RECORDNAME          PIC X(128).
           03  STMTHREQ-DATA-AREA.
      *       リクエスト番号
              05  STMTHREQ-REQNUM          PIC X(2).
      *       患者番号
              05  STMTHREQ-PTNUM           PIC X(20).
      *       印刷モード
              05  STMTHREQ-PRINT-MODE      PIC X(10).
      *       伝票番号
              05  STMTHREQ-DENPNUM         PIC X(7).
