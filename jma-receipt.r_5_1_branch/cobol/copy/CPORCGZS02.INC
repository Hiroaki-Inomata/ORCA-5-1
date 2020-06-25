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
      *----------------------------------------------------------*
      *    点数付加情報更新パラメタ        COPY (CPORCGZS02.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCGZS02AREA.
      *    期間
           03  ZS02-NAI-YUKOSTYMD      PIC X(08).
           03  ZS02-NAI-YUKOEDYMD      PIC X(08).
      *    改正時処理
           03  ZS02-YUKOEDYMD          PIC X(08).
      *
      **** 処方名称
      *    03  ZS02-SHOYUKOKETA        PIC  9(02).
      *    03  ZS02-SHONAME            PIC  X(200).
      *    採血料区分
      *    03  ZS02-SAIKETUKBN         PIC  9(01).
      *    入力チェック区分
      *    03  ZS02-INPCHKKBN          PIC  9(01).
      *    分類区分
      *    03  ZS02-TENPUYAKKBN        PIC  X(02).
      *
      *    全数量記載区分
      *    03  ZS02-ALLSURYOKBN        PIC  X(01).
      *    数量換算単位コード
      *    03  ZS02-KANZANTANICD       PIC  X(03).
      *    数量換算単位名称
      *    03  ZS02-KANZANTANINAME     PIC  X(24).
      *    数量換算値
      **** 03  ZS02-KANZANCHI          PIC  9(05)V9(05).
      *
