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
      *-------------------------------------------------*
      *    診療科履歴更新パラメタ                       *
      *                         COPY  (CPORCSKARRK.INC) *
      *-------------------------------------------------*
       01  ORCSKARRKAREA.
      *
      *対象年月
           03  SKARRK-SRYYM              PIC  X(06).
      *最終診療日
           03  SKARRK-LASTYMD            PIC  X(08).
      *診療科
           03  SKARRK-SRYKA              PIC  X(02).
      *復帰コード
           03  SKARRK-RC                 PIC S9(9)   BINARY.
      *エラー名称
           03  SKARRK-ERRMSG             PIC  X(80).
      *
