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
      *    福岡県地方公費対応給付対象外編集サブパラメタ
      *                                              (CPORCSSYU40.INC)
      *                            使用プログラム 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSSYU40AREA.
      *
      *    処理区分（１：診療行為、２：収納）
           03  ORCS40-KBN                  PIC X(01).
      *
      *    診療行為の場合
           03  ORCS40-KBN1-AREA.
      *        対象診療コード
               05  ORCS40-ZAI-TBL          OCCURS  5.
                   07  ORCS40-SRYKBN           PIC X(02).
                   07  ORCS40-ZAITEN           PIC 9(08).
                   07  ORCS40-SRYCD            PIC X(09)
                                               OCCURS  5.
      *
      *    収納の場合
           03  ORCS40-KBN2-AREA.
      *        伝票番号
               05  ORCS40-DENPNUM              PIC 9(08).
      *
