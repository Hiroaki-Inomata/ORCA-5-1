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
      *    包括チェックマスタ              COPY (CPHKTSANTEI.INC)
      *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  HKTSANTEI-REC.
           03  HKTSANTEI-KEY.
      *包括管理番号
               05  HKTSANTEI-KANRICD         PIC  9(03).
      *有効開始日
               05  HKTSANTEI-YUKOSTYMD       PIC  X(08).
      *有効終了日
               05  HKTSANTEI-YUKOEDYMD       PIC  X(08).
      *包括対象診療行為コード
               05  HKTSANTEI-SRYCD           PIC  X(09).
      *包括算定区分
           03  HKTSANTEI-SANTEIKBN           PIC  X(01).
      *包括対象検査（尿）
           03  HKTSANTEI-KENTAIKBN           PIC  X(01).
      *端末ＩＤ
           03  HKTSANTEI-TERMID              PIC  X(16).
      *オペレータＩＤ
           03  HKTSANTEI-OPID                PIC  X(16).
      *作成年月日
           03  HKTSANTEI-CREYMD              PIC  X(08).
      *更新年月日
           03  HKTSANTEI-UPYMD               PIC  X(08).
      *更新時間
           03  HKTSANTEI-UPHMS               PIC  X(06).
