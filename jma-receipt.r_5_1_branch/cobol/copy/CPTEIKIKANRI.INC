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
      *    定期請求管理                       (CPTEIKIKANRI.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TEIKIKANRI-REC.
           03  TEIKIKANRI-HOSPNUM          PIC 9(2).
      *病棟番号
           03  TEIKIKANRI-BTUNUM           PIC  X(02).
      *処理区分（１：一括、２：個別）
           03  TEIKIKANRI-SYOKBN           PIC  X(01).
      *作成日
           03  TEIKIKANRI-CREYMD           PIC  X(08).
      *作成時間
           03  TEIKIKANRI-CREHMS           PIC  X(06).
      *診療年月（一括：診療年月、個別：空白）
           03  TEIKIKANRI-SRYYM            PIC  X(06).
      *ページ
           03  TEIKIKANRI-PAGE             PIC  9(05).
      *ページ
           03  TEIKIKANRI-KENSU            PIC  9(05).
      *端末ＩＤ
           03  TEIKIKANRI-TERMID           PIC  X(16).
