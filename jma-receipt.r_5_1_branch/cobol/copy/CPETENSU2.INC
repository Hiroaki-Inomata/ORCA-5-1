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
      *      電子点数表　包括・被包括テーブル   COPY (CPETENSU2.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.08.00     NACL-伊藤    16/03/13  特例条件追加
      ************************************************************
      *01  ETENSU2-REC.
           03  ETENSU2-KEY.
      *        グループ番号
               05  ETENSU2-H-GROUP             PIC  X(07).
      *        診療行為コード
               05  ETENSU2-SRYCD               PIC  X(09).
      *        有効開始日
               05  ETENSU2-YUKOSTYMD           PIC  X(08).
      *        有効終了日
               05  ETENSU2-YUKOEDYMD           PIC  X(08).
      *    更新日付
           03  ETENSU2-CHGYMD                  PIC  X(08).
      *    特例条件
           03  ETENSU2-TOKUREI                 PIC  9(01).
      *点数マスタ用
           03  ETENSU2-TK-KEY.
               05  ETENSU2-TK-HOSPNUM          PIC  9(02).
               05  ETENSU2-TK-SRYKBN           PIC  X(02).
               05  ETENSU2-TK-SRYSYUKBN        PIC  X(03).
               05  ETENSU2-TK-SRYKBN-2         PIC  X(02).
               05  ETENSU2-TK-SRYSYUKBN-2      PIC  X(03).
      *更新用
           03  ETENSU2-UP-KEY.
               05  ETENSU2-UPH-GROUP           PIC  X(07).
               05  ETENSU2-UPSRYCD             PIC  X(09).
               05  ETENSU2-UPYUKOSTYMD         PIC  X(08).
               05  ETENSU2-UPYUKOEDYMD         PIC  X(08).
