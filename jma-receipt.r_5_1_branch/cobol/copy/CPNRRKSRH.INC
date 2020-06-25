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
      *    入院履歴照会テーブル       COPY (CPNRRKSRH.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付     内容
      ************************************************************
      *01  NRRKSRH-REC.
           03  NRRKSRH-HOSPNUM         PIC 9(02).
           03  NRRKSRH-PTID            PIC 9(10).
           03  NRRKSRH-SRYYMD          PIC X(08).
           03  NRRKSRH-ZAINUM          PIC 9(09).
           03  NRRKSRH-RRKNUM          PIC 9(03).
           03  NRRKSRH-TERMID          PIC X(16).
           03  NRRKSRH-OPID            PIC X(16).
           03  NRRKSRH-CREYMD          PIC X(08).
           03  NRRKSRH-UPYMD           PIC X(08).
           03  NRRKSRH-UPHMS           PIC X(06).
      *
           03  NRRKSRH-SELSHONUM       PIC 9(03).
           03  NRRKSRH-SELSRYCD        PIC X(09).
