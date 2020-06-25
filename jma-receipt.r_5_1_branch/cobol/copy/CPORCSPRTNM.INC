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
      *    オンライン帳票名・出力先プリンタ名取得パラメタ        *
      *                         COPY  (CPORCSPRTNM.INC)         *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSPRTNMAREA.
      *入力値
      *    処理（１： オンライン帳票名取得
      *          ２：出力先プリンタ名取得）
           03  ORCSPRTNM-KBN           PIC X(01).
      *    帳票（処理区分：１ 標準帳票区分名）
           03  ORCSPRTNM-KBNCD         PIC X(08).
      *    基準日（空白時、システム日付）
           03  ORCSPRTNM-SRYYMD        PIC X(08).
      *    入外区分（空白時、ＳＰＡ）
           03  ORCSPRTNM-NYUGAIKBN     PIC X(01).
      *
      *出力値
      *    帳票名（処理区分：１の時のみ）
           03  ORCSPRTNM-PRTPG         PIC X(20).
      *    復帰コード
           03  ORCSPRTNM-RC            PIC 9(02).
      *
      *
