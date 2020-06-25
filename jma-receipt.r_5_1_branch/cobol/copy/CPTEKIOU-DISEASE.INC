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
      **************************************************************
      *         適応病名傷病名マスタ  COPY (CPTEKIOU-DISEASE.INC)  *
      **************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TEKIOU-DISEASE-REC.
           03  TEKDIS-KEY.
      *効能コード
               05  TEKDIS-KONOCD       PIC X(05).
      *傷病名
               05  TEKDIS-BYOMEI       PIC X(200).
      *前置修飾語
           03  TEKDIS-BEFORE-BYOMEI    PIC X(100).
      *後置修飾語
           03  TEKDIS-AFTER-BYOMEI     PIC X(100).
      *傷病名コード
           03  TEKDIS-BYOMEICD         PIC X(07).
      *前置修飾語コード
           03  TEKDIS-BEFORE-BYOMEICD  PIC X(04).
      *後置修飾語コード
           03  TEKDIS-AFTER-BYOMEICD   PIC X(04).
      *ICD
           03  TEKDIS-ICD              PIC X(05).
      *改訂年月
           03  TEKDIS-KAITEIYM         PIC X(06).
      *補助区分
           03  TEKDIS-HOJYOKBN         PIC 9(02).
      *効能名称番号
           03  TEKDIS-KONOMEINO        PIC 9(06).
      *
           03  TEKDIS-CREYMD           PIC X(08).
           03  TEKDIS-UPYMD            PIC X(08).
           03  TEKDIS-UPHMS            PIC X(06).
      *検索用診療行為コード
           03  TEKDIS-SELSRYCD         PIC X(09).
      *検索用病名
           03  TEKDIS-SELBYOMEI        PIC X(200).
