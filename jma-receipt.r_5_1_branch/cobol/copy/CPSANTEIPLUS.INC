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
      *    算定履歴付加情報          COPY (CPSANTEIPLUS.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SANTEIPLUS-REC.
           03  SANTEIPLUS-KEY.
      *医療機関ＩＤ
               05  SANTEIPLUS-HOSPNUM          PIC 9(2).
      *患者ＩＤ
               05  SANTEIPLUS-PTID             PIC  9(10).
      *診療年月
               05  SANTEIPLUS-SRYYM            PIC  X(06).
      *診療コード
               05  SANTEIPLUS-SRYCD            PIC  X(09).
      *入外区分
               05  SANTEIPLUS-NYUGAIKBN        PIC  X(01).
      *診療科
               05  SANTEIPLUS-SRYKA            PIC  X(02).
      *保険組合番号
               05  SANTEIPLUS-HKNCOMBINUM      PIC  9(04).
      *診療日
               05  SANTEIPLUS-DAYKEY           PIC  9(02).
      *連番
               05  SANTEIPLUS-RENNUM           PIC  9(02).
      *リハビリコメント
           03  SANTEIPLUS-COMMENT              PIC  X(80).
      *終了日付
           03  SANTEIPLUS-ENDYMD               PIC  X(08).
      *端末ＩＤ
           03  SANTEIPLUS-TERMID           PIC  X(16).
      *オペーレータＩＤ
           03  SANTEIPLUS-OPID             PIC  X(16).
      *作成年月日
           03  SANTEIPLUS-CREYMD           PIC  X(08).
      *更新年月日
           03  SANTEIPLUS-UPYMD            PIC  X(08).
      *更新時間
           03  SANTEIPLUS-UPHMS            PIC  X(06).
      *
