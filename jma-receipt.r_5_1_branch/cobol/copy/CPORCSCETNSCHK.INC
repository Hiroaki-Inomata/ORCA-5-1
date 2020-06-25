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
      *    診療行為包括診療　電子点数表チェックパラメタ
      *                                      COPY (CPORCSCETNSCHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSCETNSCHKAREA.
      *    処理区分（１：包括チェック、２：背反チェック）
           03  ORCSCETNSCHK-SYORI         PIC X(01).
      *    ＰＧ名
           03  ORCSCETNSCHK-PG            PIC X(06).
      *    対象診療コード
           03  ORCSCETNSCHK-SRYCD1        PIC X(09).
      *    入力診療コード
           03  ORCSCETNSCHK-SRYCD2        PIC X(09).
      *    診療日付
           03  ORCSCETNSCHK-SRYYMD        PIC X(08).
      *    返却領域
           03  ORCSCETNSCHK-OUTAREA.
      *    チェック内容（０：対象なし、１：対象あり）
               05  ORCSCETNSCHK-CHKKBN        PIC 9(01).
      *        検体コード
               05  ORCSCETNSCHK-KNT-SRYCD-G.
                   07  ORCSCETNSCHK-KNT-SRYCD     PIC X(09)
                                                  OCCURS   10.
      *
