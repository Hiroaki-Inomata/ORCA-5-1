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
      *    収納一覧表用収納マスタ         COPY (CPBD001.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  BD001-REC.
      *医療機関番号
           03  BD001-HOSPNUM             PIC  9(02).
      *患者ＩＤ
           03  BD001-PTID                PIC  9(10).
      *患者番号
           03  BD001-PTNUM               PIC  X(20).
      *漢字氏名
           03  BD001-NAME                PIC  X(100).
      *性別
           03  BD001-SEX                 PIC  X(01).
      *生年月日
           03  BD001-BIRTHDAY            PIC  X(08).
      *診療科
           03  BD001-SRYKA               PIC  X(02).
      *伝票発行日
           03  BD001-DENPPRTYMD          PIC  X(08).
      *請求金額
           03  BD001-SKYMONEY            PIC  9(07).
      *入金合計額
           03  BD001-NYUKIN-TOTAL        PIC  9(07).
