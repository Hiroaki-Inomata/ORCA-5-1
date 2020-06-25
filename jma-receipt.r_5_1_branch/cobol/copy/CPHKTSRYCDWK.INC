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
      *    包括診療コードテーブル        COPY (CPHKTSRYCDWK.INC)
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-伊藤    16/03/13  連番２桁→３桁
      ************************************************************
      *01  HKTSRYCDWK-REC.
           03  HKTSRYCDWK-SYORIKBN            PIC  X(01).
           03  HKTSRYCDWK-KEY.
      *包括管理番号
               05  HKTSRYCDWK-KANRICD         PIC  9(03).
      *包括管理番号連番
               05  HKTSRYCDWK-RENNUM          PIC  9(03).
      *有効開始日
               05  HKTSRYCDWK-YUKOSTYMD       PIC  X(08).
      *有効終了日
               05  HKTSRYCDWK-YUKOEDYMD       PIC  X(08).
      *包括対象診療行為コード
           03  HKTSRYCDWK-SRYCD               PIC  X(09).
      *端末ＩＤ
           03  HKTSRYCDWK-TERMID              PIC  X(16).
      *オペレータＩＤ
           03  HKTSRYCDWK-OPID                PIC  X(16).
      *作成年月日
           03  HKTSRYCDWK-CREYMD              PIC  X(08).
      *更新年月日
           03  HKTSRYCDWK-UPYMD               PIC  X(08).
      *更新時間
           03  HKTSRYCDWK-UPHMS               PIC  X(06).
