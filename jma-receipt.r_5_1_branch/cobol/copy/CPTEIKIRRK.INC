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
      *    定期請求履歴                       (CPTEIKIRRK.INC)   *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  TEIKIRRK-REC.
           03  TEIKIRRK-HOSPNUM             PIC 9(2).
      *診療年月
           03  TEIKIRRK-SRYYM              PIC  X(06).
      *期別
      *    １〜３：各処理期
           03  TEIKIRRK-KBT                PIC  X(01).
      *端末ＩＤ
           03  TEIKIRRK-TERMID             PIC  X(16).
      *オペーレータＩＤ
           03  TEIKIRRK-OPID               PIC  X(16).
      *作成年月日
           03  TEIKIRRK-CREYMD             PIC  X(08).
      *更新年月日
           03  TEIKIRRK-UPYMD              PIC  X(08).
      *更新時間
           03  TEIKIRRK-UPHMS              PIC  X(06).
