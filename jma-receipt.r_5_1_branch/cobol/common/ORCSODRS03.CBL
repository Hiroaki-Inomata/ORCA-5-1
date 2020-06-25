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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODRS03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : オーダー編集サブ３（エラーメッセージ返却）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/17    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/05  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX-CMB                     PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-CMB-CD                  PIC X(20).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-IDX             PIC 9(05).
               05  WRK-CMB-ITM             PIC X(200).
           03  WRK-ERRCD.
               05  WRK-ERRCD9              PIC 9(04).
           03  WRK-ERRMSG                  PIC X(80).
      *
      *    エラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
       COPY    "CPORCS02MSG.INC".
       COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
       COPY    "CPORCSODRS03.INC".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                               SODRS03-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       WRK-AREA
                                       SPA-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SODRS03-ERRCD(1:1)  =   "7" )
               MOVE    "7001"          TO  WRK-CMB-CD
               PERFORM 2001-GET-ERRMSG-SEC
               INITIALIZE                  S02MSG-AREA
               MOVE    SODRS03-ERRCD   TO  WRK-ERRCD
               COMPUTE S02MSG-S02-RC = WRK-ERRCD9  -   7000
               CALL    "ORCS02MSG"     USING
                                       S02MSG-AREA
                                       SPA-AREA
               MOVE    "7001"          TO  SODRS03-ERRCD
               STRING  WRK-CMB-ITM     DELIMITED   BY  SPACE
                       S02MSG-MSG (1)  DELIMITED   BY  SPACE
               INTO    SODRS03-ERRMSG
               END-STRING
           ELSE
               MOVE    SODRS03-ERRCD   TO  WRK-CMB-CD
               PERFORM 2001-GET-ERRMSG-SEC
               IF    ( WRK-CMB-ITM (1:4)   IS  NUMERIC )
                AND  ( WRK-CMB-ITM (5: )   =   SPACE )
                   MOVE    WRK-CMB-ITM TO  WRK-CMB-CD
                   PERFORM 2001-GET-ERRMSG-SEC
                   MOVE    WRK-CMB-ITM TO  SODRS03-ERRMSG
                   IF    ( WRK-CMB-IDX NOT =   ZERO )
                       MOVE    NCLAIMERRCDL (WRK-CMB-IDX)
                                   TO  SODRS03-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-CMB-ITM TO  SODRS03-ERRMSG
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
           EXIT.
      *****************************************************************
      *    エラーメッセージ取得処理
      *****************************************************************
       2001-GET-ERRMSG-SEC                    SECTION.
      *
           INITIALIZE                      WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   NCLAIMERRCD-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( NCLAIMERRCDL (IDX-CMB)  =   WRK-CMB-CD )
                   MOVE    NCLAIMERRCDLMEI  (IDX-CMB)
                                       TO  WRK-CMB-ITM
                   MOVE    IDX-CMB     TO  WRK-CMB-IDX
               END-IF
      *
           END-PERFORM
      *
           .
       2001-GET-ERRMSG-EXT.
           EXIT.
      *
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
