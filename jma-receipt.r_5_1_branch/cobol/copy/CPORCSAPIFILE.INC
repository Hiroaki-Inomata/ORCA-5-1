      ******************************************************************
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
      ******************************************************************
      *    大容量ＡＰＩ出力用サブ              CPORCSAPIFILE.INC
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
       01  ORCSAPIFILEAREA.
           03  SAPIFILE-IN-DIRNAME     PIC X(1024).
           03  SAPIFILE-IN-FILENAME    PIC X(1024).
           03  SAPIFILE-IN-KBN         PIC X(01).
           03  SAPIFILE-IN-COUNT       PIC 9(06).
           03  SAPIFILE-IN-RENNUM      PIC 9(06).
           03  SAPIFILE-IN-FILE-TYPE   PIC X(04).
           03  SAPIFILE-OT-DIRNAME     PIC X(1024).
           03  SAPIFILE-OT-FILENAME    PIC X(1024).
           03  SAPIFILE-RC             PIC S9(9).
