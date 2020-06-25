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
      *    診療行為算定編集用パラメタ　   COPY (CPORCSC90.INC)
      *                              (ORCSC90 で使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01.06.11  注射液の廃棄追加
      * 01.00.03     MCC-多々納   01.08.09  レイアウト変更（SPAを共通）
      ************************************************************
      *
       01  ORCSC90AREA.
      *    処理区分（１：明細、２：合計）
           03  LNK-SC90-KBN            PIC X(01).
      *    対象行（必須）
           03  LNK-SC90-IDX            PIC 9(04).
      *
           03  LNK-SC90-KAISU          PIC 9(03).
      *
           03  LNK-SC90-HAIKI          PIC 9(01).
      *
           03  LNK-SC90-SUM-AREA.
      *        点数計
               05  LNK-SC90-TENSUKEI       PIC 9(08)V99.
      *        点数計（通則加算対象外計）
               05  LNK-SC90-TENGAKEI       PIC 9(08)V99.
      *        内服剤計
               05  LNK-SC90-ZAIKEI         PIC 9(08)V9(5).
      *        外用薬計
               05  LNK-SC90-GAIKEI         PIC 9(08)V9(5).
      *        内服剤点数
               05  LNK-SC90-ZAITEN         PIC 9(08).
      *        外用剤点数
               05  LNK-SC90-GAITEN         PIC 9(08).
      *        器材点数
               05  LNK-SC90-KIZKEI         PIC 9(08).
               05  LNK-SC90-KIZTEN         PIC 9(08).
      *        器材点数（酸素点数）
               05  LNK-SC90-SANKEI         PIC 9(08).
               05  LNK-SC90-SANTEN         PIC 9(08).
      *        器材点数（窒素点数）
               05  LNK-SC90-CHIKEI         PIC 9(08).
               05  LNK-SC90-CHITEN         PIC 9(08).
      *        フィルム点数
               05  LNK-SC90-FIRTEN         PIC 9(08).
               05  LNK-SC90-MAISU          PIC 9(08).
      *        分画数
               05  LNK-SC90-BUNGSU         PIC 9(08).
      *
      *
