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
      *    患者番号発行パラメタ   COPY (CPORCSP01.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSP01AREA.
      *
      *    処理（１：発行、２：チェック、３：再発行）
           03  ORCSP01-SYORI          PIC X(01).
      *    患者番号
           03  ORCSP01-PTNUM          PIC X(20).
      *
      *    患者ＩＤ
           03  ORCSP01-PTID           PIC 9(10).
      *
      *    患者番号（個別）
           03  ORCSP01-PTNUM1         PIC X(20).
           03  ORCSP01-PTNUM2         PIC X(20).
           03  ORCSP01-PTNUM3         PIC X(20).
      *
      *
