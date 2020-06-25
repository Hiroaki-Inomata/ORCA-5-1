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
      *      電子点数表　算定回数テーブル COPY (CPETENSU5.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  ETENSU5-REC.
           03  ETENSU5-KEY.
      *        診療行為コード
               05  ETENSU5-SRYCD               PIC  X(09).
      *        有効開始日
               05  ETENSU5-YUKOSTYMD           PIC  X(08).
      *        有効終了日
               05  ETENSU5-YUKOEDYMD           PIC  X(08).
      *        算定単位コード
               05  ETENSU5-TANICD              PIC  9(03).
      *    算定単位名称
           03  ETENSU5-TANINAME                PIC  X(12).
      *    算定回数
           03  ETENSU5-KAISU                   PIC  9(03).
      *    特例条件
           03  ETENSU5-TOKUREI                 PIC  9(01).
      *    作成年月日
           03  ETENSU5-CHGYMD                  PIC  X(08).
      *更新用
           03  ETENSU5-UP-KEY.
               05  ETENSU5-UPSRYCD             PIC  X(09).
               05  ETENSU5-UPYUKOSTYMD         PIC  X(08).
               05  ETENSU5-UPYUKOEDYMD         PIC  X(08).
               05  ETENSU5-UPTANICD            PIC  9(03).
