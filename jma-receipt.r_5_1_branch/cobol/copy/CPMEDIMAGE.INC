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
      *    医薬品イメージ       COPY  (CPMEDIMAGE.INC)                *
      *****************************************************************
      *****************************************************************
      * コピー句修正履歴
      * Maj/Min/Rev    修正者      日付        内容
      *****************************************************************
       01  MEDIMAGE-REC.
           03  MEDIMG-KEY.
      *医療機関ＩＤ
               05  MEDIMG-HOSPNUM            PIC 9(2).
      *医薬品コード
               05  MEDIMG-SRYCD              PIC X(9).
      *イメージ区分
               05  MEDIMG-IMAGEKBN           PIC 9(1).
      *イメージデータ
           03  MEDIMG-IMAGEDATA              PIC X(500000).
      *オペレータＩＤ
           03  MEDIMG-OPID                   PIC X(16).
      *更新年月日
           03  MEDIMG-UPYMD                  PIC X(08).
