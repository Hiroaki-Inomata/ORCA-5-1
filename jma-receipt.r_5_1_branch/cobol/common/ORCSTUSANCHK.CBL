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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSTUSANCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 通算日数チェックサブ
      *  返却エラーコード  : 00:正常　99:エラー
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/02/06    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
      *
       DATA                                DIVISION.
      *FILE                                SECTION.
      *
      *
       WORKING-STORAGE                     SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-ERR                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-YMD                     PIC X(10). 
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-NISSU3                  PIC 9(05).
           03  WRK-TUSAN                   PIC S9(08).
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    通算日数取得サブ
           COPY "CPORCSTUSAN.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    共通領域
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSTUSANCHK.INC".
grpsys*
           COPY    "COMMON-SPA".
      *
       PROCEDURE                           DIVISION    USING
           ORCSTUSANCHKAREA
           SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      STUSANCHK-OUT-AREA
           INITIALIZE                      STUSANCHK-RC
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    パラメータチェック
           PERFORM 100-PRM-CHK-SEC
           IF    ( STUSANCHK-RC        NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    主処理
           PERFORM 200-MAIN-SEC
           IF    ( STUSANCHK-RC        NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           IF    ( STUSANCHK-NYUINYMD  =   SPACE )
               MOVE    99              TO  STUSANCHK-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           IF    ( STUSANCHK-KJNYMD    =   SPACE )
               MOVE    99              TO  STUSANCHK-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           MOVE    STUSANCHK-NYUINYMD  TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( STUSANCHK-RC       NOT =   ZERO )
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           MOVE    STUSANCHK-KJNYMD    TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( STUSANCHK-RC       NOT =   ZERO )
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                        SECTION.
      *
      *    過去の入院分の通算日数を取得する
           PERFORM 2100-ORCSTUSANCHK-CALL-SEC
           IF    ( STUSANCHK-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    自院入院日数編集処理
           PERFORM 2100-NYUINNISSU-EDIT-SEC
           IF    ( STUSANCHK-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    ９１日編集処理
           PERFORM 2100-91DAYS-EDIT-SEC
           IF    ( STUSANCHK-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    １８０日編集処理
           PERFORM 2100-180DAYS-EDIT-SEC
           IF    ( STUSANCHK-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    通算日数取得サブ呼出処理
      *****************************************************************
       2100-ORCSTUSANCHK-CALL-SEC          SECTION.
      *
           INITIALIZE                  ORCSTUSANAREA
           MOVE    STUSANCHK-HOSPNUM   TO  STUSAN-HOSPNUM
           MOVE    STUSANCHK-PTID      TO  STUSAN-PTID
           MOVE    STUSANCHK-NYUINYMD  TO  STUSAN-KJNYMD
           MOVE    "1"                 TO  STUSAN-RRKNUMKBN
           MOVE    STUSANCHK-RRKNUM    TO  STUSAN-RRKNUM
           MOVE    "1"                 TO  STUSAN-NOT-SELECT-NACCT
           CALL    "ORCSTUSAN"     USING   ORCSTUSANAREA
                                           SPA-AREA
           IF    ( STUSAN-RC           NOT =   ZERO )
               MOVE    99              TO  STUSANCHK-RC
               GO  TO  2100-ORCSTUSANCHK-CALL-EXT
           END-IF
      *
           COMPUTE WRK-NISSU3
               =   STUSAN-NISSU03
      *
           COMPUTE STUSAN-NISSU03
               =   STUSAN-NISSU03
               +   STUSAN-TAINREL-NISSU
      *
           .
       2100-ORCSTUSANCHK-CALL-EXT.
           EXIT.
      *****************************************************************
      *    自院入院日数編集処理
      *****************************************************************
       2100-NYUINNISSU-EDIT-SEC            SECTION.
      *
      *    入院日から基準日までの通算日数を求める
           MOVE    STUSANCHK-NYUINYMD  TO  WRK-STYMD
           MOVE    STUSANCHK-KJNYMD    TO  WRK-EDYMD
           PERFORM 800-KIKAN-CALC-SEC
           IF    ( STUSANCHK-RC        NOT =   ZERO )
               MOVE    99              TO  STUSANCHK-RC
               GO  TO  2100-NYUINNISSU-EDIT-EXT
           END-IF
      *
           COMPUTE STUSANCHK-TSUSAN    = ( WRK-NISSU3  -   1 )
                                       + WRK-NISSU2
      *
           .
       2100-NYUINNISSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ９１日編集処理
      *****************************************************************
       2100-91DAYS-EDIT-SEC                SECTION.
      *
           IF    ( STUSAN-RRK-FLG-OVER NOT =   ZERO )
               MOVE    9           TO  STUSANCHK-DAY-91-STS
               GO  TO  2100-91DAYS-EDIT-EXT
           END-IF
      *
           MOVE    ZERO            TO  WRK-TUSAN
           PERFORM VARYING IDX1    FROM    STUSAN-RRK-CNT  BY  -1
                   UNTIL ( IDX1    <   1 )
      *
               PERFORM 21001-NISSU-EDIT-SEC
      *
      D        DISPLAY "NISSU" STUSAN-RRK-NISSU(IDX1)
      *
               IF    ( STUSAN-RRK-JTIKBN (IDX1)
                                           NOT = "5" )
      *
                   IF  ( WRK-TUSAN + STUSAN-RRK-NISSU (IDX1)
                                              >= 91 )
                       INITIALIZE                  STS-AREA-DAY
                       INITIALIZE                  LNK-DAY6-AREA
                       MOVE    "61"            TO  LNK-DAY6-IRAI
                       MOVE    STUSAN-RRK-NYUINYMD (IDX1)
                                               TO  LNK-DAY6-KIJUN
                       MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                       COMPUTE LNK-DAY6-ZOGEN
                           =   91
                           -   WRK-TUSAN
                       CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      D                DISPLAY "LNK-DAY6-KIJUN=" LNK-DAY6-KIJUN
      D                        " LNK-DAY6-ZOGEN=" LNK-DAY6-ZOGEN
      D                        " LNK-DAY6-KOYOMI=" LNK-DAY6-KOYOMI
                       MOVE    LNK-DAY6-KOYOMI TO  STUSANCHK-DAY-91
      *
                       IF    ( STUSAN-RRK-JTIKBN (IDX1)  =   "6" )
                           MOVE    1           TO  STUSANCHK-DAY-91-STS
                       ELSE
                           MOVE    ZERO        TO  STUSANCHK-DAY-91-STS
                       END-IF
      *
                       MOVE    1               TO  IDX1
                   ELSE
                       COMPUTE WRK-TUSAN
                           =   WRK-TUSAN + STUSAN-RRK-NISSU (IDX1)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2100-91DAYS-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    １８０日編集処理
      *****************************************************************
       2100-180DAYS-EDIT-SEC               SECTION.
      *
           IF    ( STUSAN-RRK-FLG-OVER NOT =   ZERO )
               MOVE    9           TO  STUSANCHK-DAY-180-STS
               GO  TO  2100-180DAYS-EDIT-EXT
           END-IF
      *
      *
           MOVE    ZERO            TO  WRK-TUSAN
           PERFORM VARYING IDX1    FROM    STUSAN-RRK-CNT  BY  -1
                   UNTIL ( IDX1    <   1 )
      *
               PERFORM 21001-NISSU-EDIT-SEC
      *
      D            DISPLAY STUSAN-RRK-NYUINYMD (IDX1) "/"
      D                   STUSAN-RRK-TAIINYMD (IDX1) "/"
      D                   STUSAN-RRK-NISSU(IDX1)
      *
      D        DISPLAY "NISSU" STUSAN-RRK-NISSU(IDX1)
      *
               IF  ( WRK-TUSAN + STUSAN-RRK-NISSU (IDX1)
                                          >= 180 )
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    STUSAN-RRK-NYUINYMD (IDX1)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   COMPUTE LNK-DAY6-ZOGEN
                       =   180
                       -   WRK-TUSAN
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      D            DISPLAY "LNK-DAY6-KIJUN=" LNK-DAY6-KIJUN
      D                     " LNK-DAY6-ZOGEN=" LNK-DAY6-ZOGEN
      D                     " LNK-DAY6-KOYOMI=" LNK-DAY6-KOYOMI
                   MOVE    LNK-DAY6-KOYOMI TO  STUSANCHK-DAY-180
      *
                   EVALUATE    STUSAN-RRK-JTIKBN (IDX1)
                   WHEN    "5"
                       MOVE    1           TO  STUSANCHK-DAY-180-STS
                   WHEN    "6"
                       MOVE    2           TO  STUSANCHK-DAY-180-STS
                   WHEN    OTHER
                       MOVE    ZERO        TO  STUSANCHK-DAY-180-STS
                   END-EVALUATE
      *
                   MOVE    1               TO  IDX1
               ELSE
                   COMPUTE WRK-TUSAN
                       =   WRK-TUSAN + STUSAN-RRK-NISSU (IDX1)
               END-IF
           END-PERFORM
      *
           .
       2100-180DAYS-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日数編集処理
      *****************************************************************
       21001-NISSU-EDIT-SEC                SECTION.
      *
           IF    ( STUSAN-RRK-JTIKBN (IDX1)  NOT = "5" )
            AND  ( STUSAN-RRK-JTIKBN (IDX1)  NOT = "6" )
               IF   ( STUSAN-RRK-TAIINYMD(IDX1)
                                   =   "99999999" )
                   MOVE    999             TO  STUSAN-RRK-NISSU(IDX1)
               ELSE
      *
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY5-AREA
                   MOVE    "51"            TO  LNK-DAY5-IRAI
                   MOVE    STUSAN-RRK-NYUINYMD (IDX1)
                                           TO  LNK-DAY5-START
                   MOVE    STUSAN-RRK-TAIINYMD (IDX1)
                                           TO  LNK-DAY5-END
                   CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                               LNK-DAY5-AREA
                   MOVE    LNK-DAY5-NISSU2 TO  STUSAN-RRK-NISSU(IDX1)
      D            DISPLAY "LNK-DAY5-NISSU1 =" LNK-DAY5-NISSU1
      D            DISPLAY "LNK-DAY5-NISSU2 =" LNK-DAY5-NISSU2
      *
                   IF    ( IDX1    >   1 )
                    AND  ( STUSAN-RRK-TAIINYMD (IDX1)
                                   =   STUSAN-RRK-NYUINYMD (IDX1 - 1 ))
                       COMPUTE STUSAN-RRK-NISSU(IDX1)
                           =   STUSAN-RRK-NISSU(IDX1)
                           -   1
                   END-IF
      *
               END-IF
           END-IF
      *
           .
       21001-NISSU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  WRK-HENYMDG
                                               WRK-SYMD
      *
           IF    ( WRK-YMD                 =   SPACE )
               GO  TO  800-HIZUKE-CHK-EXT
           END-IF
      *
           INITIALIZE                          ORCSGDAYAREA
           MOVE    WRK-YMD                 TO  SGDAY-INDAY
           CALL    "ORCSGDAY"                  USING
                                               ORCSGDAYAREA
           IF  ( SGDAY-RC                  =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    99                  TO  STUSANCHK-RC
           END-IF
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    期間算出処理
      *****************************************************************
       800-KIKAN-CALC-SEC                  SECTION.
      *
           INITIALIZE                      WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF    ( STS-DAY-RC1     NOT =   ZERO )
               MOVE    99              TO  STUSANCHK-RC
               GO  TO                      800-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       800-KIKAN-CALC-EXT.
           EXIT.
