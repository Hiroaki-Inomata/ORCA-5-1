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
      *    診療行為算定編集用パラメタ　   COPY (CPORCSC90TEN2.INC)
      *                              (ORCSC90TEN2 で使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.00    NACL-多々納  19/03/XX  点数を9桁、ERRCD追加
      ************************************************************
      *
       01  ORCSC90AREA.
      *    処理区分（１：明細、２：合計）
           03  LNK-SC90-KBN            PIC X(01).
      *    対象行（必須）
           03  LNK-SC90-IDX-STR        PIC 9(04).
      *    終了行
           03  LNK-SC90-IDX-END        PIC 9(04).
      *    手技行
           03  LNK-SC90-IDX            PIC 9(04).
      *    診療日（未使用）
           03  LNK-SC90-SRYYMD         PIC X(08).
      *
      *****03  LNK-SC90-KAISU          PIC 9(03).
      *
      *    点数（手技）
           03  LNK-SC90-TENSU          PIC 9(09).
      *    合計点数
           03  LNK-SC90-SUM-AREA.
      *        点数計
               05  LNK-SC90-TENSUKEI       PIC 9(09).
      *        薬剤点数
               05  LNK-SC90-YKZTEN         PIC 9(09).
      *        器材点数
               05  LNK-SC90-KIZTEN         PIC 9(09).
      *        器材点数（酸素点数）
               05  LNK-SC90-SANTEN         PIC 9(09).
      *        器材点数（窒素点数）
               05  LNK-SC90-CHITEN         PIC 9(09).
      *        フィルム点数
               05  LNK-SC90-FIRTEN         PIC 9(09).
      *
      *H31.3
      *    エラーコード
           03  LNK-SC90-ERRCD          PIC X(04).
      *
