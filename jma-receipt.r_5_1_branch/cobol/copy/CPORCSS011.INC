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
      *    療養病棟入院基本料取得サブ　COPY (CPORCSS011.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  SS011-AREA.
           03  SS011-PTID                  PIC 9(10).
      *    療養病棟入院料設定フラグ（SS011-LIFE,SS011-DAYの設定)
      *    0:サブで設定する / 1:呼出元で設定する
           03  SS011-DAYFLG                PIC 9(01).
      *    医療区分、ＡＤＬ点数設定フラグ（SS011-ADLTEN,MEDKBNの設定)
      *    0:サブで設定する / 1:呼出元で設定する
           03  SS011-MEDADLFLG             PIC 9(01).
      *    入院基本料更新フラグ
      *    0:更新しない／1:更新する
           03  SS011-NYUUPDFLG             PIC 9(01).
           03  SS011-SRYYM                 PIC X(06).
           03  SS011-DAY-G.
               05  SS011-DAY-OCC           OCCURS    31.
      *    入院基本料の種類(CPRYOYOCD.INC参照）
                   07  SS011-KHN           PIC 9(02).
      *    入院会計の入院料より判断した区分
      *    1:Ａ／2:Ｂ／3:Ｃ...9:Ｉ（特別入院基本料は1で返却）
                   07  SS011-DAY           PIC 9(02).
      *    医療区分、ＡＤＬ点数より判断した区分（特別入院基本料を除く）
                   07  SS011-DAY2          PIC 9(02).
      *    医療区分、ＡＤＬ点数より判断した区分（特別入院基本料を含む）
                   07  SS011-DAY3          PIC 9(02).
                   07  SS011-ADLTEN        PIC 9(03).
                   07  SS011-MEDKBN        PIC 9(01).
      *    医療区分、ＡＤＬ点数より判断した入院料コード
                   07  SS011-SRYCD         PIC X(09).
           03  SS011-RC                    PIC S9(9) BINARY.
