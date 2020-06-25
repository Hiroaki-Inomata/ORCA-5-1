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
      *    クライアント印刷情報取得                              *
      *                         COPY  (CPORCSPRTCTRL.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
       01  ORCSPRTCTRLAREA.
      *モード
           03  ORCSPRTCTRL-MODE          PIC X(01).
      *バッチ処理判定
           03  ORCSPRTCTRL-BATCH-FLG     PIC 9(01).
      *入力値
           03  ORCSPRTCTRL-IN            OCCURS   10.
      *    帳票タイトル
             05  ORCSPRTCTRL-TITLE-I     PIC X(256).
      *ＫＥＹ
             05  ORCSPRTCTRL-TBL-KEY     PIC X(08).
      *連番
             05  ORCSPRTCTRL-RENNUM      PIC 9(04).
      *グループ
             05  ORCSPRTCTRL-TBL-GROUP   PIC X(14).
      *UUID
             05  ORCSPRTCTRL-TBL-UUID    PIC X(36).
      *処理内連番
             05  ORCSPRTCTRL-SHORI-RENNUM
                                         PIC 9(04).
      *開始頁
             05  ORCSPRTCTRL-STPAGE
                                         PIC 9(05).
      *終了頁
             05  ORCSPRTCTRL-EDPAGE
                                         PIC 9(05).
      *
      *出力値
           03  ORCSPRTCTRL-OUT.
             05  ORCSPRTCTRL-O           OCCURS   10.
      *    帳票パス
               07  ORCSPRTCTRL-PATH-O    PIC X(256).
      *    帳票タイトル
               07  ORCSPRTCTRL-TITLE-O   PIC X(256).
      *    表示有無フラグ
               07  ORCSPRTCTRL-SHOWDIALOG  PIC S9(9)   BINARY.
      *    リトライ回数
               07  ORCSPRTCTRL-NRETRY   PIC S9(9)   BINARY.
      *
      *
