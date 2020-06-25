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
      *----------------------------------------------------------*
      *    バッチジョブ制御パラメタ　   COPY (CPORSMONBATCH.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SMONBATCH-AREA.
           03  SMONBATCH-MODE                  PIC X(10).
           03  SMONBATCH-KEY.
               05  SMONBATCH-OPID              PIC X(16).
               05  SMONBATCH-TERMID            PIC X(64).
               05  SMONBATCH-SHELLID           PIC X(08).
           03  SMONBATCH-FOUND                 PIC 9(01).
           03  SMONBATCH-RC                    PIC S9(9).
