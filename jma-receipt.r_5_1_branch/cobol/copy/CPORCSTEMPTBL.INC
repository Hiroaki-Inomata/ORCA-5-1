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
      *    一時テーブル作成サブ                 COPY (CPORCSTEMPTBL.INC)
      ******************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ******************************************************************
       01  STEMPTBL-AREA.
           03  STEMPTBL-FUNC               PIC X(16).
           03  STEMPTBL-TABLE              PIC X(64).
           03  STEMPTBL-PTID               PIC 9(10).
           03  STEMPTBL-SRYYM              PIC X(06).
           03  STEMPTBL-FROM               PIC 9(02).
           03  STEMPTBL-TO                 PIC 9(02).
      *
