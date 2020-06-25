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
      *    パスワード        COPY   (CPPASSWD.INC) *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      ************************************************************
       01  PASSWD-REC.
      *キー
           03  PASSWD-KEY.
      *ユーザーＩＤ
               05  PASSWD-USERID     PIC X(16).
      *パスワード
           03  PASSWD-PASSWORD       PIC X(256).
      *以下 virtual
      *    ユーザＩＤ（更新後）
           03  PASSWD-UPD-USERID     PIC X(16).
