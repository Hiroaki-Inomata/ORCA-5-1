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
      *         適応病名医薬品マスタ  COPY (CPTEKIOU-MEDICINE.INC) *
      **************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TEKIOU-MEDICINE-REC.
           03  TEKMED-KEY.
      *診療行為コード
               05  TEKMED-SRYCD        PIC X(09).
      *効能コード
           03  TEKMED-KONOCD           PIC X(05).
      *改訂年月
           03  TEKMED-KAITEIYM         PIC X(06).
      *
           03  TEKMED-CREYMD           PIC X(08).
           03  TEKMED-UPYMD            PIC X(08).
           03  TEKMED-UPHMS            PIC X(06).
