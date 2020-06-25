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
      *****************************************************************
      *  診療科毎のドクター設定              (CPLIST_DOCTOR.INC) 
      *****************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  LIST-DOCTOR-REC.
      *
      *キー
           03  LSDOCTOR-KEY.
      *医療機関ＩＤ
               05  LSDOCTOR-HOSPNUM     PIC 9(02).
      *診療科
               05  LSDOCTOR-SRYKA       PIC X(02).
      *表示順
               05  LSDOCTOR-RENNUM      PIC 9(03).
      *ドクター
           03  LSDOCTOR-DRCD            PIC X(05).
      *端末ＩＤ
           03  LSDOCTOR-TERMID          PIC  X(16).
      *オペーレータＩＤ
           03  LSDOCTOR-OPID            PIC  X(16).
      *作成年月日
           03  LSDOCTOR-CREYMD          PIC  X(08).
      *更新年月日
           03  LSDOCTOR-UPYMD           PIC  X(08).
      *更新時間
           03  LSDOCTOR-UPHMS           PIC  X(06).
