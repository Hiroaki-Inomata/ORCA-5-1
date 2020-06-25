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
      *    まとめ入金編集パラメタ２（未収まとめ入金画面用）      *
      *                         COPY  (CPORCSNYUKINS.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSNYUKINAREAS.
      *入力値
      *    医療機関ＩＤ（必須）
           03  LNK-SCNYUKINS-HOSPNUM         PIC 9(2).
      *    患者ＩＤ（必須）
           03  LNK-SCNYUKINS-PTID            PIC 9(10).
      *    入金日
           03  LNK-SCNYUKINS-NYUKIN-YMD      PIC X(08).
      *    入金方法
           03  LNK-SCNYUKINS-NYUKIN-HOHO     PIC X(02).
      *    入金額設定情報
           03  LNK-SCNYUKINS-AREA.
               05  LNK-SCNYUKINS-MAX         PIC 9(05).
               05  LNK-SCNYUKINS-TBL    OCCURS 500.
      *            入外区分
                   07  LNK-SCNYUKINS-NYUGAIKBN     PIC X(01).
      *            伝票番号
                   07  LNK-SCNYUKINS-DENPNUM       PIC 9(07).
      *            入金日２
                   07  LNK-SCNYUKINS-NYUKIN-YMD2   PIC X(08).
      *            入金時刻
                   07  LNK-SCNYUKINS-NYUKIN-HMS2   PIC X(06).
      *            入金方法
                   07  LNK-SCNYUKINS-NYUKIN-HOHO2  PIC X(02).
      *            入金額
                   07  LNK-SCNYUKINS-NYUKIN-MONEY  PIC S9(08).
      *
      *            以下返却情報
                   07  LNK-SCNYUKINS-SKYMONEY      PIC S9(08).
                   07  LNK-SCNYUKINS-NYUKIN-TOTAL  PIC S9(08).
                   07  LNK-SCNYUKINS-DENPJTIKBN    PIC X(01).
                   07  LNK-SCNYUKINS-CREATEKBN     PIC X(01).
                   07  LNK-SCNYUKINS-DENPEDANUM    PIC 9(02).
      *
      *復帰コード
      *00:正常
      *10:パラメータ不正
      *11:収納更新エラー
      *12:収納明細インサートエラー
           03  LNK-SCNYUKINS-RC              PIC 9(02).
      *
      *
