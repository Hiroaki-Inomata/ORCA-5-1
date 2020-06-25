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
      *    診療行為画面表示編集用パラメタ　   COPY (CPORCSC94.INC)
      *                              (ORCSC94 で使用）
      *
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSC94AREA.
      *    処理区分（１：入力値の再編集（画面の初期表示、自動算定時）
           03  LNK-SC94-KBN            PIC X(01).
      *    処理ＰＧ名
           03  LNK-SC94-PG             PIC X(03).
      *    対象行（必須入力）
           03  LNK-SC94-IDX            PIC 9(04).
      *    訂正フラグ（入院の回数のみ使用）
           03  LNK-SC94-SYORIFLG       PIC 9(01).
      *
