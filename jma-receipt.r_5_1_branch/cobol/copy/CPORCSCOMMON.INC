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
      *    ＳＰＡ初期設定処理パラメタ
      *                                      COPY (CPORCSCCOMMON.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSCOMMONAREA.
      *    処理ＰＧ
           03  ORCSCOMMON-PG               PIC X(08).
      *    変換医療機関番号
           03  ORCSCOMMON-HOSPNUM          PIC 9(02).
      *    患者番号年号変換メッセージ
           03  ORCSCOMMON-LABLMSG          PIC X(80).
      *    医療機関名称
           03  ORCSCOMMON-HOSPNAME         PIC X(120).
      *
