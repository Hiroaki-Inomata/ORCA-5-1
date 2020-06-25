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
      *    患者介護認定者情報         COPY  (CPPTCARE-NINTEI.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTCARE-NINTEI-REC.
      *キー
           03  PTCRNIN-KEY.
      *医療機関ＩＤ
               05  PTCRNIN-HOSPNUM         PIC  9(02).
      *患者ＩＤ
               05  PTCRNIN-PTID            PIC  9(10).
      *適用開始年月日
               05  PTCRNIN-TEKSTYMD        PIC  X(08).
      *適用終了年月日
           03  PTCRNIN-TEKEDYMD            PIC  X(08).
      *要介護状態区分
           03  PTCRNIN-CAREJOTAICD         PIC  X(02).
      *認定日付
           03  PTCRNIN-NINTEIYMD           PIC  X(08).
      *端末ＩＤ
           03  PTCRNIN-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  PTCRNIN-OPID                PIC  X(16).
      *作成年月日
           03  PTCRNIN-CREYMD              PIC  X(08).
      *更新年月日
           03  PTCRNIN-UPYMD               PIC  X(08).
      *更新時間
           03  PTCRNIN-UPHMS               PIC  X(06).
