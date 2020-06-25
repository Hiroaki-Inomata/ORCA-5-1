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
      *      電子点数表　入院基本料テーブル COPY (CPETENSU4.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  ETENSU4-REC.
           03  ETENSU4-KEY.
      *        グループ番号
               05  ETENSU4-N-GROUP             PIC  9(03).
      *        診療行為コード
               05  ETENSU4-SRYCD               PIC  X(09).
      *        有効開始日
               05  ETENSU4-YUKOSTYMD           PIC  X(08).
      *        有効終了日
               05  ETENSU4-YUKOEDYMD           PIC  X(08).
      *    加算識別
           03  ETENSU4-KASAN                   PIC  9(02).
      *    作成年月日
           03  ETENSU4-CHGYMD                  PIC  X(08).
      *更新用
           03  ETENSU4-UP-KEY.
               05  ETENSU4-UPN-GROUP           PIC  9(03).
               05  ETENSU4-UPSRYCD             PIC  X(09).
               05  ETENSU4-UPYUKOSTYMD         PIC  X(08).
               05  ETENSU4-UPYUKOEDYMD         PIC  X(08).
