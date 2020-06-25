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
      *      電子点数表　補助マスター   COPY (CPETENSU1.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *01  ETENSU1-REC.
           03  ETENSU1-KEY.
      *        診療行為コード
               05  ETENSU1-SRYCD               PIC  X(09).
      *        有効開始日
               05  ETENSU1-YUKOSTYMD           PIC  X(08).
      *        有効終了日
               05  ETENSU1-YUKOEDYMD           PIC  X(08).
      *    包括関連
           03  ETENSU1-HKTGRP-AREA.
               05  ETENSU1-HKTGRP-T            OCCURS   3.
      *            包括単位
                   07  ETENSU1-H-TANI          PIC 9(02).
      *            グループ番号
                   07  ETENSU1-H-GROUP         PIC X(07).
      *    背反関連識別
           03  ETENSU1-HAIHAN-KANREN.
      *        １日につき
               05  ETENSU1-R-DAY               PIC  9(01).
      *        同一月内
               05  ETENSU1-R-MONTH             PIC  9(01).
      *        同時
               05  ETENSU1-R-SAME              PIC  9(01).
      *        １週間につき
               05  ETENSU1-R-WEEK              PIC  9(01).
      *    入院基本料識別
           03  ETENSU1-N-GROUP                 PIC  9(03).
      *    算定回数
           03  ETENSU1-C-KAISU                 PIC  9(01).
      *    作成年月日
           03  ETENSU1-CHGYMD                  PIC  X(08).
      *更新用
           03  ETENSU1-UP-KEY.
               05  ETENSU1-UPSRYCD             PIC  X(09).
               05  ETENSU1-UPYUKOSTYMD         PIC  X(08).
               05  ETENSU1-UPYUKOEDYMD         PIC  X(08).
