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
      ***************************************************************
      *      向精神薬成分コードテーブル   COPY (CPPSYCHOTROPIC.INC) *
      ***************************************************************
      ***************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ***************************************************************
      *01  PSYCHOTROPIC-REC.
           03  PSYCHOTROPIC-KEY.
      *        コード
               05  PSYCHOTROPIC-CODE             PIC  X(07).
      *        有効開始日
               05  PSYCHOTROPIC-YUKOSTYMD        PIC  X(08).
      *        有効終了日
               05  PSYCHOTROPIC-YUKOEDYMD        PIC  X(08).
      *    向精神薬区分
           03  PSYCHOTROPIC-CLASS                PIC  X(01).
      *    グループコード
           03  PSYCHOTROPIC-GROUPCODE            PIC  X(07).
      *    更新年月日
           03  PSYCHOTROPIC-UPYMD                PIC  X(08).
      *更新用
           03  PSYCHOTROPIC-UP-KEY.
               05  PSYCHOTROPIC-UPCODE              PIC  X(07).
               05  PSYCHOTROPIC-UPYUKOSTYMD         PIC  X(08).
               05  PSYCHOTROPIC-UPYUKOEDYMD         PIC  X(08).
