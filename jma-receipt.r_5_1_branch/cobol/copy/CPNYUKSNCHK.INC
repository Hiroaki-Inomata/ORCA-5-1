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
      *    入院料加算チェックマスタ   COPY (CPNYUKSNCHK.INC)          *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  NYUKSNCHK-REC.
           03  NYUKSNCHK-KEY.
      *入院料区分
               05  NYUKSNCHK-NYUINKBN  PIC  X(03).
      *加算区分
               05  NYUKSNCHK-KSNKBN    PIC  X(03).
      *有効開始年月日
               05  NYUKSNCHK-YUKOSTYMD PIC  X(08).
      *有効終了年月日
               05  NYUKSNCHK-YUKOEDYMD PIC  X(08).
      *チェック区分
           03  NYUKSNCHK-CHKKBN        PIC  X(01).
      *端末ＩＤ
           03  NYUKSNCHK-TERMID        PIC  X(16).
      *オペーレータＩＤ
           03  NYUKSNCHK-OPID          PIC  X(16).
      *作成年月日
           03  NYUKSNCHK-CREYMD        PIC  X(08).
      *更新年月日
           03  NYUKSNCHK-UPYMD         PIC  X(08).
      *更新時間 
           03  NYUKSNCHK-UPHMS         PIC  X(06).
      *主キー更新用項目
           03  NYUKSNCHKUP-KEY.
               05  NYUKSNCHKUP-NYUINKBN      PIC  X(03).
               05  NYUKSNCHKUP-KSNKBN        PIC  X(03).
               05  NYUKSNCHKUP-YUKOSTYMD     PIC  X(08).
               05  NYUKSNCHKUP-YUKOEDYMD     PIC  X(08).
      *
