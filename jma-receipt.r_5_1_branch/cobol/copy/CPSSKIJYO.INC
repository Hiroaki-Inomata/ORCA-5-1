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
      *    症状措置機序マスタ        COPY (CPSSKIJYO.INC)       *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SSKIJYO-REC.
           03  SSKIJYO-KEY.
      *症状コード
               05  SSKIJYO-SYOJYOUCD        PIC  X(07).
      *症状
           03  SSKIJYO-SYOJYOU             PIC  X(2048).
      *作用機序
           03  SSKIJYO-SAYOKIJYO           PIC  X(2048).
      *端末ＩＤ
           03  SSKIJYO-TERMID              PIC  X(32).
      *オペーレータＩＤ
           03  SSKIJYO-OPID                PIC  X(16).
      *作成年月日
           03  SSKIJYO-CREYMD              PIC  X(08).
      *更新年月日
           03  SSKIJYO-UPYMD               PIC  X(08).
      *更新時間
           03  SSKIJYO-UPHMS               PIC  X(06).
      *
