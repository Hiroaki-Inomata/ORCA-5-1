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
      *    全角項目チェックパラメタ　  COPY (CPORCSKANACHK.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  02/08/09  入力・出力領域をX(1000)へ
      * 04.05.00     NACL-多々納  09/12/11  全角空白削除追加
      * 04.06.00     NACL-多々納  10/10/01  入力・出力領域をX(5000)へ
      * 05.01.00     NACL-多々納  17/09/26  復帰コード４(改行）追加
      ************************************************************
      *
       01  ORCSKANACHKAREA.
      *    処理フラグ（１：チェック、２：全角変換）
           03  KANACHK-SYORI               PIC X(01).
      *    処理フラグ２（１：後ろの全角空白削除）
           03  KANACHK-SYORIKBN            PIC X(01).
      *    入力領域
           03  KANACHK-MAE-INPUT           PIC X(5000).
      *    出力桁数（全角の最大桁数の指定（省略可））
           03  KANACHK-MAX-CNT             PIC 9(04).
      *
      *    出力領域（半角空白を全角へ置換えて編集）
           03  KANACHK-OUT-INPUT           PIC X(5000).
      *    文字タイプ（１：半角、２：全角）
           03  KANACHK-SYUBETU             PIC 9(01).
      *    最大桁数
           03  KANACHK-MAX                 PIC 9(04).
      *    復帰コード （０：正常、１：混在エラー、２：変換エラー）
           03  KANACHK-RC                  PIC 9(04).
      *    復帰コード２（０：英数字以外なし、９：英数字以外あり、
      *                  ８：空白あり）
           03  KANACHK-RC2                 PIC 9(04).
      *    復帰コード３（０：全角空白なし、１：全角空白あり
      *                  ２：全角空白のみ、３：最後が全角空白）
           03  KANACHK-RC3                 PIC 9(01).
      *
      *    復帰コード４（０：改行なし、１：改行コードあり）
           03  KANACHK-RC4                 PIC 9(01).
      *
      *
