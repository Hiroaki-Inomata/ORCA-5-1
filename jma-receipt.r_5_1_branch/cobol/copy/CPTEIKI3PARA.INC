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
      *    患者定期請求-請求削除パラメータ
      *　  COPY  (CPTEIKI3PARA.INC)                              *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  TEIKI3PARA-AREA.
           03  TEIKI3P-ERRFILE             PIC X(200).
           03  TEIKI3P-HOSPNUM             PIC 9(02).
           03  TEIKI3P-TERMID              PIC X(64).
           03  TEIKI3P-OPID                PIC X(16).
           03  TEIKI3P-SYSYMD              PIC X(08).
           03  TEIKI3P-JOBID               PIC 9(07).
           03  TEIKI3P-SHELLID             PIC X(08).
           03  TEIKI3P-SRYYM               PIC X(06).
           03  TEIKI3P-KBT                 PIC X(01).
           03  TEIKI3P-SKYSTYMD            PIC X(08).
           03  TEIKI3P-SKYEDYMD            PIC X(08).
