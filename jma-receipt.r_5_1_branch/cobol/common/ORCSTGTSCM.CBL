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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSTGTSCM.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : データ出力対象スキーマ一覧編集処理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/02    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
      *01  FLG-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SCHEMA-G.
               05  WRK-SCHEMA-NO       PIC 9(03).
               05  WRK-SCHEMA-ID       PIC X(20).
               05  WRK-SCHEMA-NAME     PIC X(20).
               05  WRK-SCHEMA-CHR-R    REDEFINES WRK-SCHEMA-NAME.
                   07  WRK-SCHEMA-CHR  PIC X(01)   OCCURS  20.
      *
           03  WRK-STIDX               PIC 9(03).
      *
       01  CONST-AREA.
           03  CONST-SCHEMA-ID-LEN-MAX PIC 9(03)   VALUE   20.
           03  CONST-CHR-MAX           PIC 9(03)   VALUE   26.
           03  CONST-CHR-VAL.
               05  PIC X(26)       VALUE   "abcdefghijklmnopqrstuvwxyz".
               05  PIC X(26)       VALUE   "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           03  CONST-CHR-R    REDEFINES    CONST-CHR-VAL.
               05  CONST-CHR-LOWCASE   PIC X(01)   OCCURS  26.
               05  CONST-CHR-UPCASE    PIC X(01)   OCCURS  26.
      *
           COPY    "CPTGTSCM.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPSTGTSCM.INC".
      *
       PROCEDURE                    DIVISION    USING
           STGTSCM-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       WRK-AREA
                                       STGTSCM-AREA
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   TGTSCM-MAX )
      *
               MOVE    TGTSCM-NO (IDX0)    TO  STGTSCM-NO (IDX0)
               MOVE    TGTSCM-ID (IDX0)    TO  STGTSCM-ID (IDX0)
      *
           END-PERFORM
      *
           MOVE    TGTSCM-MAX      TO  STGTSCM-MAX
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   STGTSCM-MAX -   1 )
      *
               COMPUTE WRK-STIDX   =   IDX1    +   1
      *
               PERFORM VARYING IDX2    FROM    WRK-STIDX   BY  1
                       UNTIL ( IDX2    >   STGTSCM-MAX )
      *
                   IF    ( STGTSCM-ID (IDX1)   >   STGTSCM-ID (IDX2) )
                       MOVE    STGTSCM-OCC (IDX2) TO  WRK-SCHEMA-G
                       MOVE    STGTSCM-OCC (IDX1) TO  STGTSCM-OCC (IDX2)
                       MOVE    WRK-SCHEMA-G       TO  STGTSCM-OCC (IDX1)
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   STGTSCM-MAX )
      *
               MOVE    STGTSCM-ID (IDX0)   TO  WRK-SCHEMA-ID
                                               WRK-SCHEMA-NAME
      *
               EVALUATE    TRUE
               WHEN    ( WRK-SCHEMA-ID    (1:4)    =   "tbl_" )
               WHEN    ( WRK-SCHEMA-ID    (1:4)    =   "TBL_" )
                   MOVE    WRK-SCHEMA-ID  (5:)     TO  WRK-SCHEMA-NAME
               WHEN    ( WRK-SCHEMA-ID    (1:5)    =   "view_" )
               WHEN    ( WRK-SCHEMA-ID    (1:5)    =   "VIEW_" )
                   MOVE    WRK-SCHEMA-ID  (6:)     TO  WRK-SCHEMA-NAME
               END-EVALUATE
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-SCHEMA-ID-LEN-MAX )
                        OR   ( WRK-SCHEMA-CHR (IDX1)   =   SPACE )
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL ( IDX2    >   CONST-CHR-MAX )
                            OR   ( WRK-SCHEMA-CHR (IDX1)
                                           =   CONST-CHR-LOWCASE (IDX2))
                       CONTINUE
                   END-PERFORM
      *
                   IF    ( IDX2    >   CONST-CHR-MAX )
                       MOVE    WRK-SCHEMA-CHR (IDX1)
                                       TO  STGTSCM-NAME (IDX0)(IDX1:1)
                   ELSE
                       MOVE    CONST-CHR-UPCASE (IDX2)
                                       TO  STGTSCM-NAME (IDX0)(IDX1:1)
                   END-IF
      *
               END-PERFORM
      *
      *
      *        STRING  WRK-SCHEMA-ID   DELIMITED   BY  SPACE
      *                ".db"           DELIMITED   BY  SIZE
      *        INTO    STGTSCM-ID (IDX0)
      *        END-STRING
      *
           END-PERFORM
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
