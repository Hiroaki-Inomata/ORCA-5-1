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
      *      電子点数表　背反関連テーブル
      *                                     COPY (CPETENSU3.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  ETENSU3-REC.
      *
           03  ETENSU3-KEY.
      *診療行為コード１
               05  ETENSU3-SRYCD1             PIC  X(09).
      *診療行為コード２
               05  ETENSU3-SRYCD2             PIC  X(09).
      *有効開始日
               05  ETENSU3-YUKOSTYMD          PIC  X(08).
      *有効終了日
               05  ETENSU3-YUKOEDYMD          PIC  X(08).
      *背反区分
           03  ETENSU3-HAIHAN              PIC  9(01).
      *特例条件
           03  ETENSU3-TOKUREI             PIC  9(01).
      *作成年月日
           03  ETENSU3-CHGYMD              PIC  X(08).
      *更新用
           03  ETENSU3-UP-KEY.
               05  ETENSU3-UPSRYCD1        PIC  X(09).
               05  ETENSU3-UPSRYCD2        PIC  X(09).
               05  ETENSU3-UPYUKOSTYMD     PIC  X(08).
               05  ETENSU3-UPYUKOEDYMD     PIC  X(08).
      *
