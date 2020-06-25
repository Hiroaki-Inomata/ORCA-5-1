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
      *    患者地域連携                  COPY  (CPPTNUM-PUBLIC.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTPUBLIC-REC.
      *キー
           03  PTPUBLIC-KEY.
      *医療機関ＩＤ
               05  PTPUBLIC-HOSPNUM        PIC  9(2).
      *患者ＩＤ
               05  PTPUBLIC-PTID           PIC  9(10).
      *地域連携ＩＤ
           03  PTPUBLIC-PATIENT-ID1        PIC  X(20).
      *情報提供同意有無
           03  PTPUBLIC-AGREEMENT          PIC  X(01).
      *端末ＩＤ
           03  PTPUBLIC-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  PTPUBLIC-OPID               PIC  X(16).
      *作成年月日
           03  PTPUBLIC-CREYMD             PIC  X(08).
      *更新年月日
           03  PTPUBLIC-UPYMD              PIC  X(08).
      *更新時間
           03  PTPUBLIC-UPHMS              PIC  X(06).
      *
