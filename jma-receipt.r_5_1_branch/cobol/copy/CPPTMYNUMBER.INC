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
      *    患者個人番号            COPY  (CPPTMYNUMBER.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  PTMYNUMBER-REC.
      *キー
           03  PTMYNUM-KEY.
      *医療機関ＩＤ
               05  PTMYNUM-HOSPNUM         PIC  9(2).
      *患者ＩＤ
               05  PTMYNUM-PTID            PIC  9(10).
      *ＩＤ識別
               05  PTMYNUM-ID-KEY          PIC  X(10).
      *ＩＤ番号
           03  PTMYNUM-MY-NUMBER           PIC  X(20).
      *備考
           03  PTMYNUM-DESCRIPTION         PIC  X(100).
      *端末ＩＤ
           03  PTMYNUM-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  PTMYNUM-OPID                PIC  X(16).
      *作成年月日
           03  PTMYNUM-CREYMD              PIC  X(08).
      *更新年月日
           03  PTMYNUM-UPYMD               PIC  X(08).
      *更新時間
           03  PTMYNUM-UPHMS               PIC  X(06).
      *
