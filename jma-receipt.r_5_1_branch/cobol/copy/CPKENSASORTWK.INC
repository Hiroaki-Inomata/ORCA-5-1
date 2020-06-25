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
      ************************************************************
      ************************************************************
      *    検査分類　        　 COPY (CPKENSASORTWK.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  KENSASORTWK-REC.
      *処理区分
           03  KENSASORTWK-SYORIKBN        PIC  X(01).
      *キー
           03  KENSASORTWK-KEY.
      *検査分類コード
               05  KENSASORTWK-KNSBUNRUI   PIC  9(02).
      *診療コード
               05  KENSASORTWK-SRYCD       PIC  X(09).
      *表示連番
           03  KENSASORTWK-DSPSEQ          PIC  9(04).
      *端末ＩＤ
           03  KENSASORTWK-TERMID          PIC  X(32).
      *オペレータＩＤ
           03  KENSASORTWK-OPID            PIC  X(16).
      *作成年月日
           03  KENSASORTWK-CREYMD          PIC  X(08).
      *更新年月日
           03  KENSASORTWK-UPYMD           PIC  X(08).
      *更新時間
           03  KENSASORTWK-UPHMS           PIC  X(06).
