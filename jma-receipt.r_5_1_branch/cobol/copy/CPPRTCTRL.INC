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
      *    印刷制御(クライアント印刷用) COPY  (CPPRTCTRL.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  XX.XX.XX    NACL-XXXX    99/99/99 
      ************************************************************
            03  PRTCTRL-KEY.
      *  医療機関ＩＤ
              05  PRTCTRL-HOSPNUM      PIC S9(2).
      *  KEY
              05  PRTCTRL-TBL-KEY      PIC X(8).
      *  連番
              05  PRTCTRL-RENNUM       PIC S9(4).
      *  グループ
              05  PRTCTRL-TBL-GROUP    PIC X(14).
      *  UUID
              05  PRTCTRL-TBL-UUID     PIC X(36).
      *  処理内連番
              05  PRTCTRL-SHORI-RENNUM PIC S9(4).
              05  PRTCTRL-PAGE         PIC S9(5).
      *  データ区分
            03  PRTCTRL-DATA-KBN       PIC S9(2).
      *  連続フラグ
            03  PRTCTRL-CONT-FLG       PIC S9(2).
      *  業務コード
            03  PRTCTRL-GYOUMU-CD      PIC S9(3).
      *  印刷データ格納名称
            03  PRTCTRL-DNAME          PIC X(200).
      *  端末ID
            03  PRTCTRL-TERMID         PIC X(64).
            03  PRTCTRL-OPID           PIC X(16).
            03  PRTCTRL-CREYMD         PIC X(8).
            03  PRTCTRL-CREHMS         PIC X(6).
            03  PRTCTRL-SELLIMIT       PIC S9(8).
            03  PRTCTRL-SELOFFSET      PIC S9(8).
