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
      *    診療行為入力入院加算チェック
      *                               COPY (CPORCSCNKSNCHK.INC)
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSCNKSNCHKAREA.
      *診療行為コード
           03  ORCSCNKSN-SRYCD             PIC  X(09).
      *診療年月
           03  ORCSCNKSN-SRYYM             PIC  X(06).
      *算定日　（算定日に1設定）
           03  ORCSCNKSN-DAY-G.
               05  ORCSCNKSN-DAY           PIC 9(01)
                                           OCCURS   31.
      *エラーコード
      *  １：パラメタ漏れ
      *  ２：患者情報検索エラー
      *  ３：入院会計保険組合せ無し
      *  ４：入院料検索エラー
      *  ５：入院料との併算定エラー（注加算）
      *  ６：入院料との併算定エラー（入院基本料等加算）
      *  ７：外泊日に加算入力
      *  ８：入院会計で加算算定済み
      *  ９：入院会計で加算の剤が存在する
      *
           03  ORCSCNKSN-RC                PIC S9(9) BINARY.
      *エラーメッセージ
           03  ORCSCNKSN-ERRMSG            PIC X(80).
      *
