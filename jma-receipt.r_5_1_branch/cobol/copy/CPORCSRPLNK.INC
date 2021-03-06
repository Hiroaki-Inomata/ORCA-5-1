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
      *    CPORCSRPLNK.INC （帳票API 識別名連絡用一覧）
      ********************************************************************
       01  SRPLNK-AREA.
           03  SRPLNK-MODE             PIC X(16).
           03  SRPLNK-VAL              PIC X(20).
           03  SRPLNK-KARTE-UID        PIC X(36).
           03  SRPLNK-ORCA-UID         PIC X(36).
           03  SRPLNK-TITLE            PIC X(100).
           03  SRPLNK-RC               PIC S9(09).
