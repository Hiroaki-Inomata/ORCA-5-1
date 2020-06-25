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
      *    ＳＰＡ一時データ　  COPY  (CPSPA-TMP.INC)             *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  SPATMP-REC.
           03  SPATMP-KEY.
               05  SPATMP-HOSPNUM           PIC 9(02).
      *業務ＩＤ
               05  SPATMP-GYOUMUID          PIC X(04).
      *端末ＩＤ
               05  SPATMP-TERMID            PIC X(64).
      *ファイル名
               05  SPATMP-FILEMEI           PIC X(20).
      *枝番
               05  SPATMP-EDANUM            PIC 9(03).
      *データ領域
           03  SPATMP-DATA-REC              PIC X(600000).
      *
      *更新年月日
           03  SPATMP-UPYMD                 PIC X(08).
      *更新時間
           03  SPATMP-UPHMS                 PIC X(06).
