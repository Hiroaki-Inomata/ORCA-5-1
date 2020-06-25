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
      *----------------------------------------------------------*
      *    入院院外処方せん編集パラ   COPY (CPORCSNYUINSHO.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSNYUINSHOAREA.
      *    処理区分（１：診療科編集、２：処方せん印刷、３：診療科のみ)
           03  ORCSNYUINSHO-KBN        PIC X(01).
      *    入院日（必須）
           03  ORCSNYUINSHO-NYUINYMD   PIC X(08).
      *    退院日（必須）
           03  ORCSNYUINSHO-TAIINYMD   PIC X(08).
      *    入・出力（処理区分＝１ は出力、２ は 入力項目）
           03  ORCSNYUINSHO-SRYKA-AREA.
               05  ORCSNYUINSHO-SRYKA-G    OCCURS   5.
      *            診療科
                   07  ORCSNYUINSHO-SRYKA      PIC X(02).
      *            ドクター
                   07  ORCSNYUINSHO-DRCD       PIC X(05).
               05  ORCSNYUINSHO-SRYKA-MAX      PIC 9(02).
      *    主治医（処理区分＝２ でドクターがない時のみ必須）
           03  ORCSNYUINSHO-SYUDRCD    PIC X(05).
      *    同日再入院区分（１：同日再入院）
           03  ORCSNYUINSHO-DOUJITSUKBN    PIC X(01).
      *
