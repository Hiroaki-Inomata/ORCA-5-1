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
      *         最低薬価マスタ  COPY (CPGENERICPRICE.INC)     *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.00    NACL-多々納  16/03/17  薬価桁数、一般名コード対応
      ************************************************************
      *01  GENERICPRICE-REC.
           03  GENERICPRICE-KEY.
      *薬価基準コード
               05  GENERICPRICE-YAKKAKJNCD   PIC  X(12).
      *有効期間開始
               05  GENERICPRICE-YUKOSTYMD    PIC  X(08).
      *有効期間終了
               05  GENERICPRICE-YUKOEDYMD    PIC  X(08).
      *薬価
           03  GENERICPRICE-PRICE            PIC  9(09)V9(3).
      *一般名コード
           03  GENERICPRICE-GECODE           PIC  X(12).
      *作成年月日
           03  GENERICPRICE-CREYMD           PIC  X(08).
      *更新年月日
           03  GENERICPRICE-UPYMD            PIC  X(08).
      *更新用
           03  GENERICPRICE-UP-KEY.
               05  GENERICPRICE-UPYAKKAKJNCD PIC  X(12).
               05  GENERICPRICE-UPYUKOSTYMD  PIC  X(08).
               05  GENERICPRICE-UPYUKOEDYMD  PIC  X(08).
