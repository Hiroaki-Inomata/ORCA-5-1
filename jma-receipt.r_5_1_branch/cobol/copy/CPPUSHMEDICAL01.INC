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
       01  PUSHMD01-REC.
           03  PUSHMD01-EVENT                  PIC X(50).
           03  PUSHMD01-BODY.
      *      更新モード
             05  PUSHMD01-PMODE                PIC X(10).
      *      患者番号
             05  PUSHMD01-PTNUM                PIC X(20).
      *      システム日付
             05  PUSHMD01-SYSYMD               PIC X(10).
      *      システム時間
             05  PUSHMD01-SYSTIME              PIC X(08).
      *      診療日付
             05  PUSHMD01-SRYYMD               PIC X(10).
      *      診療情報
             05  PUSHMD01-MEDICAL-G.
                 07  PUSHMD01-MEDICAL-TBL      OCCURS   15.
      *      保険組合せ
                     09  PUSHMD01-HKNCOMBI     PIC X(04).
      *      診療科
                     09  PUSHMD01-SRYKA        PIC X(02).
      *      ドクターコード
                     09  PUSHMD01-DRCD         PIC X(05).
      *      伝票番号
                     09  PUSHMD01-DENPNUM      PIC X(07).
      *

