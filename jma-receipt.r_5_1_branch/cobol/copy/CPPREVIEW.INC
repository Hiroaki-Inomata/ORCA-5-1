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
      *    プレビュー印刷用データ     COPY (CPPREVIEW.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
        01  PREVIEW-REC.
            03  PREVIEW-KEY.
                05  PREVIEW-HOSPNUM   PIC 9(2).
                05  PREVIEW-TBL-KEY    PIC X(8).
                05  PREVIEW-RENNUM     PIC 9(4).
                05  PREVIEW-TBL-GROUP  PIC X(14).
                05  PREVIEW-TBL-UUID   PIC X(36).
                05  PREVIEW-SHORI-RENNUM
                                       PIC 9(4).
            03  PREVIEW-SHELLID        PIC X(20).
            03  PREVIEW-PRTID          PIC X(20).
            03  PREVIEW-SRYYM          PIC X(6).
            03  PREVIEW-SKYYMD         PIC X(8).
            03  PREVIEW-PTID           PIC 9(10).
            03  PREVIEW-PAGE           PIC 9(5).
            03  PREVIEW-PRTNM          PIC X(16).
            03  PREVIEW-PRTDATA        PIC X(100000).
            03  PREVIEW-TERMID         PIC X(64).
            03  PREVIEW-OPID           PIC X(16).
            03  PREVIEW-CREYMD         PIC X(8).
            03  PREVIEW-UPYMD          PIC X(8).
            03  PREVIEW-UPHMS          PIC X(6).
      *検索用項目
            03  PREVIEWXX-STPAGE       PIC 9(05).
            03  PREVIEWXX-EDPAGE       PIC 9(05).    
