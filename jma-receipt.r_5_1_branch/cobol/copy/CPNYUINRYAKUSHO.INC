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
      *    入院レセプト記載略称         COPY (CPNYUINRYAKUSHO.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  NYURYAKUSHO-REC.
           03  NYURYAKUSHO-KEY.
      *基本点数診療コード
               05  NYURYAKUSHO-SRYCD   PIC  X(09).
      *有効開始年月日
               05  NYURYAKUSHO-YUKOSTYMD    PIC  X(08).
      *有効終了年月日
               05  NYURYAKUSHO-YUKOEDYMD    PIC  X(08).
      *略称
           03  NYURYAKUSHO-RYAKUSHO     PIC  X(20).
      *名称
           03  NYURYAKUSHO-NAME         PIC  X(200).
      *端末ＩＤ
           03  NYURYAKUSHO-TERMID       PIC  X(16).
      *オペーレータＩＤ
           03  NYURYAKUSHO-OPID         PIC  X(16).
      *作成年月日
           03  NYURYAKUSHO-CREYMD       PIC  X(08).
      *更新年月日
           03  NYURYAKUSHO-UPYMD        PIC  X(08).
      *更新時間
           03  NYURYAKUSHO-UPHMS        PIC  X(06).
      *更新用
           03  NYURYAKUSHO-UP-KEY.
               05  NYURYAKUSHO-UPSRYCD     PIC  X(09).
               05  NYURYAKUSHO-UPYUKOSTYMD PIC  X(08).
               05  NYURYAKUSHO-UPYUKOEDYMD PIC  X(08).
      *
