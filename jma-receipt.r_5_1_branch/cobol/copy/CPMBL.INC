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
      ************************************************************
      *    バッチログ(MONBATCH_LOG)      COPY (CPMBL.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      * 01  MBL-REC.
            03  MBL-ID                 PIC X(37).
            03  MBL-PGID               PIC S9(9)   BINARY.
            03  MBL-RC                 PIC S9(9)   BINARY.
            03  MBL-MESSAGE            PIC X(512).
            03  MBL-STARTTIME          PIC X(64).
            03  MBL-ENDTIME            PIC X(64).
            03  MBL-TENANT             PIC S9(9)   BINARY.
            03  MBL-NAME               PIC X(256).
            03  MBL-GROUPNAME          PIC X(64).
            03  MBL-COMMENT            PIC X(512).
            03  MBL-EXTRA              PIC X(512).
            03  MBL-EXWINDOW           PIC X(256).
            03  MBL-EXWIDGET           PIC X(256).
            03  MBL-EXEVENT            PIC X(256).
            03  MBL-EXTERM             PIC X(256).
            03  MBL-EXUSER             PIC X(256).
            03  MBL-EXHOST             PIC X(256).
            03  MBL-EXEC-RECORD        PIC X(8192).
            03  MBL-MOVECOUNT          PIC S9(9)   BINARY.
