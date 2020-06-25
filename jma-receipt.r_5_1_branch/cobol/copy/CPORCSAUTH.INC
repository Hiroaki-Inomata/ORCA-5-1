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
      *-------------------------------------------------*
      *    パスワード作成、認証パラメタ  COPY (CPORCSAUTH.INC)     *
      *-------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      ************************************************************
      *
       01  ORCSAUTHAREA.
      *コマンド（"passwd" or "auth")
           03  AUTH-COMMAND            PIC X(08).
      *暗号化手法（01:MD5）
           03  AUTH-ID                 PIC 9(02).
      *パスワード（プレーン）
           03  AUTH-PASSWORD1          PIC X(128).
      *暗号化パスワード
           03  AUTH-PASSWORD2          PIC X(128).
      *結果（0:成功 1:失敗）
           03  AUTH-RC                 PIC 9(01).
      *
