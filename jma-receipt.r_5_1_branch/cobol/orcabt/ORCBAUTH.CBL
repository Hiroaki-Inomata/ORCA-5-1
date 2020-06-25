      *****************************************************************
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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      *****************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBAUTH.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : セットアップ
      *  コンポーネント名  : パスワードチェック
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/08/07    NACL−伊藤　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           05  WRK-PASSWORD1           PIC X(128).
           05  WRK-PASSWORD2           PIC X(128).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPORCSAUTH.INC".
      *
      *****************************************************************
       PROCEDURE                   DIVISION.
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-MAIN                    SECTION.
      *
           INITIALIZE  WRK-AREA
      *
           ACCEPT  WRK-PASSWORD1   FROM  ENVIRONMENT "PASSWORD1"
           ACCEPT  WRK-PASSWORD2   FROM  ENVIRONMENT "PASSWORD2"
      *
           INITIALIZE  ORCSAUTHAREA
           MOVE    "auth"              TO  AUTH-COMMAND
           MOVE    1                   TO  AUTH-ID
           MOVE    WRK-PASSWORD1       TO  AUTH-PASSWORD1
           MOVE    WRK-PASSWORD2       TO  AUTH-PASSWORD2
           CALL    "orcsauth"      USING   ORCSAUTHAREA
           MOVE    AUTH-RC             TO  RETURN-CODE
           .
       000-MAIN-EXT.
           EXIT    PROGRAM.
      *
