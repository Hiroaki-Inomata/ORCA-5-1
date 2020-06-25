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
      *    診療行為診察料自動発生編集パラメタ　  COPY (CPORCSC10S.INC)
      *                              (ORCSC10Sで使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSC10SAREA.
      *    処理区分（１：初診料発生、２：同日再診、３：算定回数集計
      *              ４：院内、院外振替え、５：電話再診振替え）
           03  LNK-SC10S-SYORI          PIC X(01).
      *    最終テーブル位置
           03  LNK-SC10S-IDX2           PIC 9(04).
      *!   診察料編集なし(1:診察料算定済み）
      *!   03  LNK-SC10S-SINSATU        PIC 9(01).
      *!   03  LNK-SC10S-ZAINUM-AREA.
      *!       05  LNK-SC10S-ZAINUM     PIC  9(08)   OCCURS  5.
      *    開始テーブル位置
           03  LNK-SC10S-STRIDX         PIC 9(04).
      *
