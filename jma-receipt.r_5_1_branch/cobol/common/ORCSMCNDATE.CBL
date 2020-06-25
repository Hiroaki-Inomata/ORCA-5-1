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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSMCNDATE.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : マシン日付取得サブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/11/11    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  00.00.01    NACL−太田   03/11/11  新規作成
      *****************************************************************
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           03  WRK-YMD.
               05  WRK-YY20        PIC X(02).
               05  WRK-YYMMDD      PIC X(06).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    マシン日付取得サブパラメタ
           COPY    "CPORCSMCNDATE.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               ORCSMCNDATEAREA.
      *
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              WRK-AREA
      *
           INITIALIZE              ORCSMCNDATEAREA
      *
           MOVE    "20"            TO      WRK-YY20
           ACCEPT  WRK-YYMMDD      FROM    DATE
           MOVE    WRK-YMD         TO      SMCNDATE-YMD
      *
           ACCEPT  SMCNDATE-TIME   FROM    TIME
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
