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
      *    パラメタ　  COPY  (CPPARA.INC)               *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    多々納       02/07/30  端末ＩＤを16 から 32 へ
      *  01.00.02    伊藤         03/03/12  端末ＩＤを32 から 64 へ
      ************************************************************
      *01  PARA-REC.
           03  PARA-KEY.
               05  PARA-HOSPNUM           PIC 9(2).
      *業務ＩＤ
               05  PARA-GYOUMUID          PIC X(04).
      *端末ＩＤ
               05  PARA-TERMID            PIC X(64).
      *ファイル名
               05  PARA-FILEMEI           PIC X(20).
      *枝番
               05  PARA-EDANUM            PIC 9(03).
      *データ領域
           03  PARA-DATA-REC.
               05  PARA-DATA-REC1         PIC X(1000).
               05  PARA-DATA-REC2         PIC X(1000).
               05  PARA-DATA-REC3         PIC X(1000).
               05  PARA-DATA-REC4         PIC X(1000).
               05  PARA-DATA-REC5         PIC X(1000).
      *更新年月日
           03  PARA-UPYMD                 PIC X(08).
      *更新時間
           03  PARA-UPHMS                 PIC X(06).
