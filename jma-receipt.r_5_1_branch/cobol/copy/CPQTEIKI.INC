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
      *    定期請求用クエリ        COPY (CPQTEIKI.INC)           *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  QTEIKI-REC.
           03  QTEIKI-HOSPNUM          PIC 9(02).
           03  QTEIKI-SRYYM            PIC X(6).
           03  QTEIKI-KBT              PIC X(1).
           03  QTEIKI-BTUNUM           PIC X(2).
           03  QTEIKI-BRMNUM           PIC X(8).
           03  QTEIKI-PTID             PIC 9(10).
           03  QTEIKI-PTNUM            PIC X(20).
           03  QTEIKI-DENPNUM          PIC 9(07).
           03  QTEIKI-KANANAME         PIC X(100).
           03  QTEIKI-KENSU            PIC 9(10).
           03  QTEIKI-TENNYUYMD        PIC X(08).
           03  QTEIKI-TAIINYMD         PIC X(08).
           03  QTEIKI-SELKBT           PIC X(01) OCCURS 3.
           03  QTEIKI-SELTAIINYMD      PIC X(08) OCCURS 2.
