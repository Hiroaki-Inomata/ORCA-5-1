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
      ******************************************************************
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCL0012.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＣＬＡＩＭ
      *  コンポーネント名  : ＣＬＡＩＭ送信済み設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/12/06    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 03.05.00     NACL-多々納  07/05/09  グループ診療対応
      * 04.08.00     NACL-多々納  16/05/XX  API対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
      *01  IDX-AREA.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
      *API
           03  WRK-ERRCD             PIC X(02).
           03  WRK-UPDCNT            PIC 9(02).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  LNK-PTID             PIC 9(10).
       01  LNK-SRYKA            PIC X(02).
       01  LNK-TIME             PIC X(06).
       01  LNK-HOSPNUM          PIC 9(02).
       01  LNK-DRCD             PIC X(05).
       01  LNK-HKNCOMBI         PIC X(04).
       01  LNK-RC               PIC X(02).
       01  LNK-YMD              PIC X(08).
      *API
       01  LNK-ENDRC            PIC X(02).
      *
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   LNK-PTID
                                               LNK-SRYKA
                                               LNK-TIME
                                               LNK-HOSPNUM
                                               LNK-DRCD
                                               LNK-HKNCOMBI
                                               LNK-RC
                                               LNK-YMD
                                               LNK-ENDRC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *    システム日付セット
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
      *
           INITIALIZE              SPA-AREA
           MOVE    LNK-HOSPNUM         TO  SPA-HOSPNUM
           MOVE    LNK-PTID            TO  SPA-PTID
           MOVE    LNK-YMD             TO  WRK-SYSYMD
      *
           MOVE    SPACE               TO  LNK-ENDRC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    受付テーブル読み込み
           INITIALIZE                  UKETUKE-REC
           MOVE    LNK-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    LNK-PTID            TO  UKE-PTID
           MOVE    WRK-SYSYMD          TO  UKE-UKEYMD
           MOVE    LNK-TIME            TO  UKE-UKETIME
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL     (FLG-UKETUKE      =  1  )  OR
                             (FLG-OK           =  1  )
      *
               IF     (UKE-UKEID           >   ZERO  )   AND
                      (UKE-KAIKEITIME      =   ZERO  )   AND
                      (UKE-SRYKA           =   LNK-SRYKA )
                   IF      (LNK-HKNCOMBI       =   "@@@@")  OR
                          ((LNK-HKNCOMBI   NOT =   "@@@@")  AND
                           (LNK-HKNCOMBI       =   UKE-HKNCOMBI))
      *                送信完了
                       MOVE    1                   TO  FLG-OK
                       MOVE    LNK-RC              TO  UKE-SRFLG
                       MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                       MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                       MOVE    UKETUKE-REC         TO  MCPDATA-REC
      *
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *****            MOVE    "key"               TO  MCP-PATHNAME
      *ver.4.7   SRFLGのみ更新
                       MOVE    "upd1"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                      IF      MCP-RC          NOT =   ZERO 
                           DISPLAY "ORCL0012 UKETUKE UPD ERR:"  MCP-RC
                                   ",KEY:" UKE-KEY
      *API対応
                           MOVE    "01"            TO  WRK-ERRCD
                       ELSE
                           ADD     1               TO  WRK-UPDCNT
      *API対応
                       END-IF
                   END-IF
               END-IF
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    終了　処理
      *****************************************************************
       300-END-SEC                SECTION.
      *
      *API
           IF      WRK-ERRCD           =   SPACE
               IF      WRK-UPDCNT          =   ZERO
      *            対象なし
                   MOVE    "01"            TO  LNK-ENDRC
               ELSE
      *            更新あり
                   MOVE    "00"            TO  LNK-ENDRC
               END-IF
           ELSE
      *        更新エラー
               MOVE    "99"            TO  LNK-ENDRC
           END-IF
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読込
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
           EXIT.
