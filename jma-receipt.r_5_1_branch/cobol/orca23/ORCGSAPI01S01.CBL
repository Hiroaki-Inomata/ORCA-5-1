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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGSAPI01S01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 収納
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *
       01  FLG-AREA.
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-MONTH               PIC 9(01).
           03  FLG-LOCK                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-APIPARA             PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TERM                PIC X(64).
           03  WRK-WINDOW              PIC X(64).
           03  WRK-MOTOPG              PIC X(08).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-TABLE               PIC X(64).
           03  WRK-PATHNAME            PIC X(64).
           03  WRK-KINGAKU             PIC S9(10).
           03  WRK-KINGAKUM9           PIC ---------9.
           03  WRK-KINGAKUMM           PIC ----------.
           03  WRK-RATEZ               PIC ZZ9.
           03  WRK-MISYU-G.
               05  WRK-MISYU-OCC       OCCURS 2.
                   07  WRK-MISYU       PIC S9(10).
                   07  WRK-KANYU       PIC S9(10).
           03  WRK-INSTR               PIC X(500).
           03  WRK-OUTSTR              PIC X(500).
           03  WRK-LEN                 PIC S9(10).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LIMIT             PIC 9(05)   VALUE 99999.
           03  CONST-RES-MAX           PIC 9(05)   VALUE 500.
      *
           COPY    "SAPI01SCR-SPA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    文字列操作サブ
           COPY    "CPORCSSTRING.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML用定義体
           COPY    "CPAPIV3REQ.INC".
           COPY    "CPINCOMEV3RES01.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ(拡張文字対応版)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    バッチジョブ制御
           COPY    "CPORCSMONBATCH.INC".
      *
      *    排他制御
           COPY    "CPORCSLOCK.INC".
      *
       01  UIDIO-AREA.
           03  UIDIO-RET               PIC X(2).
           03  UIDIO-UID               PIC X(36).
       01  UIDIO-ST                    PIC 9(2).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA023.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
           COPY    "CPINCOMEV3REQ01.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               INCOMEREQ01-AREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-API-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  INCOMERES01-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF    ( INCOMEREQ01-ORCA-UID  =   SPACE )
               PERFORM 800-ORCUIDSET-SEC
               MOVE    UIDIO-UID   TO  SPA-API-ORCA-UID
           ELSE
               MOVE    INCOMEREQ01-ORCA-UID
                                   TO  SPA-API-ORCA-UID
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           PERFORM 2004-STATE-UPD-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KARTE-UID-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-PTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-OUTKBN-CHK-SEC
           END-IF
      *
           MOVE    ZERO            TO  FLG-MONTH
      *
           IF    ( SPA-API-OUTKBN  =   "1" OR "2" )
               MOVE    1           TO  FLG-MONTH
               MOVE    INCOMEREQ01-STSRYYM
                                   TO  INCOMEREQ01-STSRYYMD
               MOVE    INCOMEREQ01-EDSRYYM
                                   TO  INCOMEREQ01-EDSRYYMD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STSRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDSRYYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SORT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-ORDER-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2001-RANGE-FROM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2001-RANGE-TO-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カルテＵＩＤチェック
      *****************************************************************
       2001-KARTE-UID-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-APIPARA
      *
           IF    ( INCOMEREQ01-KARTE-UID =   SPACE )
               MOVE    "0203"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               PERFORM 900-APIPARA-KEY6-SEL-SEC
      *
               PERFORM UNTIL ( FLG-APIPARA NOT =   ZERO )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                   MOVE    APIPARA-DATA-REC    TO  PARA023-REC
      *
                   IF    ( INCOMEREQ01-ORCA-UID    = SPACE )
                       IF  ( APIPARA-KARTE-UID = INCOMEREQ01-KARTE-UID )
                           MOVE    "1038"      TO  SPA-ERRCD
                       END-IF
                   ELSE
                       IF  ( APIPARA-KARTE-UID = INCOMEREQ01-KARTE-UID )
                        AND( APIPARA-TERMID    = INCOMEREQ01-ORCA-UID )
                           PERFORM 20011-TERMINATE-SEC
                       END-IF
                   END-IF
      *
                   PERFORM 900-APIPARA-KEY6-FET-SEC
      *
               END-PERFORM
      *
               MOVE    "tbl_api_para"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( INCOMEREQ01-ORCA-UID  NOT =   SPACE )
                   IF    ( CNT-APIPARA         =   ZERO )
                       MOVE    "0206"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPACE               TO  SPA-API-ORCA-UID
           END-IF
      *
           .
       2001-KARTE-UID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       2001-PTNUM-CHK-SEC              SECTION.
      *
           PERFORM 990-LOCK-OUT-SEC
      *
           IF    ( INCOMEREQ01-PTNUM =   SPACE )
               MOVE    "0071"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    INCOMEREQ01-PTNUM TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        =   ZERO )
                   MOVE    SPTID-PTNUM     TO  SPA-PTNUM
                   MOVE    SPTID-PTID      TO  SPA-PTID
                   PERFORM 990-LOCK-IN-SEC
               ELSE
                   MOVE    "0072"          TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納終了処理
      *****************************************************************
       20011-TERMINATE-SEC             SECTION.
      *
           COMPUTE CNT-APIPARA =       CNT-APIPARA +   1
      *
           CALL    "ORCGSSUB04"        USING
                                       APIPARA-REC
                                       SPA-AREA
      *
           .
       20011-TERMINATE-EXT.
           EXIT.
      *****************************************************************
      *    開始診療日チェック
      *****************************************************************
       2001-STSRYYMD-CHK-SEC           SECTION.
      *
           IF    ( INCOMEREQ01-STSRYYMD  =   SPACE )
               IF    ( FLG-MONTH       =   1 )
                   MOVE    "0114"  TO  SPA-ERRCD
               ELSE
                   MOVE    "0113"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD    =      SPACE )
      *
               IF    ( INCOMEREQ01-STSRYYMD (1:4)
                                       =  "0000" )
                   MOVE    "00000000"  TO SPA-API-STSRYYMD
               ELSE
                   MOVE    INCOMEREQ01-STSRYYMD
                                       TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   IF    ( FLG-MONTH       =   1 )
                       MOVE    "01"    TO  WRK-SYMD(7:)
                   END-IF
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1     =   SPACE )
                       IF    ( FLG-MONTH       =   1 )
                           MOVE    "0083"  TO  SPA-ERRCD
                       ELSE
                           MOVE    "0074"  TO  SPA-ERRCD
                       END-IF
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-API-STSRYYMD
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-STSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了診療日チェック
      *****************************************************************
       2001-EDSRYYMD-CHK-SEC           SECTION.
      *
           IF    ( INCOMEREQ01-EDSRYYMD  =   SPACE )
               MOVE    "9999-12-31"    TO INCOMEREQ01-EDSRYYMD
           END-IF
      *
           IF    ( INCOMEREQ01-EDSRYYMD (1:4)
                                       =  "9999" )
               MOVE    "99999999"      TO SPA-API-EDSRYYMD
           ELSE
               MOVE    INCOMEREQ01-EDSRYYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               IF    ( FLG-MONTH       =   1 )
                   PERFORM 800-GETUMATU-SEC
               END-IF
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   IF    ( FLG-MONTH       =   1 )
                       MOVE    "0084"  TO  SPA-ERRCD
                   ELSE
                       MOVE    "0075"  TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-API-EDSRYYMD
               END-IF
           END-IF
      *
           .
       2001-EDSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    返却内容指示チェック
      *****************************************************************
       2001-OUTKBN-CHK-SEC             SECTION.
      *
           IF    ( INCOMEREQ01-OUTKBN        =   SPACE )
               MOVE    "1"                 TO  SPA-API-OUTKBN
           ELSE
               IF    ( INCOMEREQ01-OUTKBN    =   "1" OR"2" OR "3" )
                   MOVE    INCOMEREQ01-OUTKBN    TO  SPA-API-OUTKBN
               ELSE
                   MOVE    "0076"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-OUTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並べ変え項目チェック
      *****************************************************************
       2001-SORT-CHK-SEC               SECTION.
      *
           IF    ( INCOMEREQ01-SORT      =   SPACE )
               MOVE    "1"                 TO  SPA-API-SORT
           ELSE
               IF    ( INCOMEREQ01-SORT  =   "1" OR"2" )
                   MOVE    INCOMEREQ01-SORT  TO  SPA-API-SORT
               ELSE
                   MOVE    "0077"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-SORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順チェック
      *****************************************************************
       2001-ORDER-CHK-SEC              SECTION.
      *
           IF    ( INCOMEREQ01-ORDER     =   SPACE )
               MOVE    "1"                 TO  SPA-API-ORDER
           ELSE
               IF    ( INCOMEREQ01-ORDER =   "1" OR"2" )
                   MOVE    INCOMEREQ01-ORDER TO  SPA-API-ORDER
               ELSE
                   MOVE    "0078"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-ORDER-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始件数チェック処理
      *****************************************************************
       2001-RANGE-FROM-CHK-SEC         SECTION.
      *
           IF    ( INCOMEREQ01-RANGE-FROM  =   SPACE )
               MOVE    0               TO  SPA-API-OFFSET
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-RANGE-FROM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0079"      TO  SPA-ERRCD
               ELSE
                   IF    ( SNUM-OUTNUM     <= 1 )
                       MOVE    0       TO  SPA-API-OFFSET
                   ELSE
                       COMPUTE SPA-API-OFFSET
                           =   SNUM-OUTNUM
                           -   1
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-RANGE-FROM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了件数チェック処理
      *****************************************************************
       2001-RANGE-TO-CHK-SEC         SECTION.
      *
           IF    ( INCOMEREQ01-RANGE-TO  =   SPACE )
               MOVE    CONST-LIMIT     TO  SPA-API-LIMIT
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ01-RANGE-TO
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0080"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-API-TO
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-API-TO  <   SPA-API-OFFSET + 1 )
                       MOVE    "0082"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   COMPUTE SPA-API-LIMIT
                       =   SPA-API-TO
                       -   SPA-API-OFFSET
                   IF    ( CONST-LIMIT <   SPA-API-LIMIT )
                       MOVE    CONST-LIMIT TO  SPA-API-LIMIT
                   END-IF
               END-IF
           END-IF
      *
           COMPUTE SPA-API-FROM = SPA-API-OFFSET + 1
      *
           .
       2001-RANGE-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-DB-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-RESPONSE-SEC
           END-IF
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    データベース更新処理
      *****************************************************************
       20031-DB-SEC                    SECTION.
      *
      *
           .
       20031-DB-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-SEC                  SECTION.
      *
           MOVE    SPA-PTNUM           TO  INCOMERES01-PTNUM
      *
           MOVE    ZERO                TO  IDXA
      *
           PERFORM 900-SYUNOU-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU      NOT =   ZERO )
                    OR   ( IDXA               >=   CONST-RES-MAX )
      *
               COMPUTE IDXA    =   IDXA    +   1
      *
               PERFORM 200311-SYUNOU-EDIT-SEC
      *
               MOVE    WRK-TABLE       TO  MCP-TABLE
               MOVE    WRK-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SYUNOU-FET-SEC
      *
           END-PERFORM
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( IDXA                 >  ZERO )
               IF    ( IDXA            >=  CONST-RES-MAX )
                AND  ( FLG-SYUNOU       =  ZERO )
                   MOVE    "True"      TO  INCOMERES01-SYUNOU-OVER
               ELSE
                   MOVE    "False"     TO  INCOMERES01-SYUNOU-OVER
               END-IF
           END-IF
      *
           MOVE    IDXA                TO  SPA-API-SYU-COUNT
      *
           PERFORM 200311-MISYU-EDIT-SEC
      *
           MOVE    WRK-MISYU (1)    TO WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM    TO INCOMERES01-TMISYU-TTL-N
           MOVE    WRK-KANYU (1)    TO WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM    TO INCOMERES01-TKANYU-TTL-N
           MOVE    WRK-MISYU (2)    TO WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM    TO INCOMERES01-TMISYU-TTL-G
           MOVE    WRK-KANYU (2)    TO WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM    TO INCOMERES01-TKANYU-TTL-G
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       200311-SYUNOU-EDIT-SEC          SECTION.
      *
           IF    ( SYU-NYUGAIKBN       =   "1" )
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES01-TSRYYMD (IDXA)
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES01-TSKYEDYMD (IDXA)
           ELSE
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES01-TSRYYMD (IDXA)
           END-IF
      *
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  INCOMERES01-TDENPPRTYMD(IDXA)
           IF    ( SYU-NYUGAIKBN       =   "1" )
               MOVE    "I"             TO  INCOMERES01-TNYUGAIKBN (IDXA)
           ELSE
               MOVE    "O"             TO  INCOMERES01-TNYUGAIKBN (IDXA)
           END-IF
           MOVE    SYU-DENPNUM         TO  INCOMERES01-TDENPNUM   (IDXA)
           IF    ( SYU-GRP-DENPNUM     NOT =   ZERO )
               MOVE    SYU-GRP-DENPNUM
                                     TO  INCOMERES01-TGRP-DENPNUM (IDXA)
           END-IF
           MOVE    SYU-HKNCOMBINUM     TO  INCOMERES01-THKNCOMBI  (IDXA)
           MOVE    SYU-PTFTNRATE       TO  WRK-RATEZ
           MOVE    WRK-RATEZ           TO  INCOMERES01-TFTNRATE   (IDXA)
           MOVE    SYU-SRYKA           TO  INCOMERES01-TSRYKA     (IDXA)
      *
           MOVE    SYU-SKYMONEY        TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES01-TSEIKYU (IDXA)
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES01-TNYUKIN   (IDXA)
      *
           MOVE    SYU-SKYMONEY        TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES01-TSEIKYU (IDXA)
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES01-TNYUKIN   (IDXA)
           COMPUTE WRK-KINGAKU = SYU-SKYMONEY - SYU-NYUKIN-TOTAL
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES01-TMISYU    (IDXA)
      *
           EVALUATE    SYU-DENPJTIKBN
           WHEN    SPACE
               MOVE    "0"         TO  INCOMERES01-TJOTAIKBN (IDXA)
               MOVE    "請求額なし" TO INCOMERES01-TJOTAINAME(IDXA)
           WHEN    "1"
               MOVE    SYU-DENPJTIKBN
                                   TO  INCOMERES01-TJOTAIKBN (IDXA)
               MOVE    "入金済"    TO INCOMERES01-TJOTAINAME(IDXA)
           WHEN    "2"
               MOVE    SYU-DENPJTIKBN
                                   TO  INCOMERES01-TJOTAIKBN (IDXA)
               MOVE    "未入金"    TO INCOMERES01-TJOTAINAME(IDXA)
           WHEN    "3"
               MOVE    SYU-DENPJTIKBN
                                   TO  INCOMERES01-TJOTAIKBN (IDXA)
               MOVE    "請求取消"  TO INCOMERES01-TJOTAINAME(IDXA)
           END-EVALUATE
      *
           IF    ( SYU-NYUGAIKBN   =  "1" )
               MOVE    SYU-CREATEKBN
                                   TO INCOMERES01-TTYPE(IDXA)
           END-IF
      *
           IF    ( SYU-DENPJTIKBN  NOT =   "3" )
           AND   ( SYU-NYUGAIKBN       =   "2" )
               EVALUATE    TRUE
               WHEN    ( SYU-SECMONEY  =   1 OR 2 OR 3 OR 4 OR
                                           5 OR 6 OR 7   )
                   MOVE    "2"      TO  INCOMERES01-TKEIKOKU  (IDXA)
                   MOVE    "要一括再計算"
                                    TO  INCOMERES01-TKEIKOKUNAME  (IDXA)
               WHEN  ( SYU-ACCT-UPDKBN     NOT =   SPACE )
                   MOVE    "1"      TO  INCOMERES01-TKEIKOKU  (IDXA)
                   MOVE    "要再計算"
                                    TO  INCOMERES01-TKEIKOKUNAME  (IDXA)
               END-EVALUATE
           END-IF
      *
           .
       200311-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    未収編集処理
      *****************************************************************
       200311-MISYU-EDIT-SEC           SECTION.
      *
           INITIALIZE                  WRK-MISYU-G
      *
           PERFORM 900-SYUNOU-KEY98-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               MOVE    2               TO  IDXA
               IF    ( SYU-NYUGAIKBN   =   "1" )
                   MOVE    1           TO  IDXA
               END-IF
      *
               COMPUTE WRK-MISYU (IDXA)
                   =   WRK-MISYU (IDXA)    +   SYU-CHOSEI1
               COMPUTE WRK-KANYU (IDXA)
                   =   WRK-KANYU (IDXA)    +   SYU-CHOSEI2
      *
               PERFORM 900-SYUNOU-KEY98-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key98"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200311-MISYU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       2004-STATE-UPD-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE              PARA023-REC
               MOVE    "02"        TO  PARA023-STATE
               PERFORM 20041-APIPARA-UPD-SEC
           ELSE
               IF    ( SPA-API-ORCA-UID    =   SPACE )
                   CONTINUE
               ELSE
                   INITIALIZE              PARA023-REC
                   MOVE    "01"        TO  PARA023-STATE
                   PERFORM 20041-APIPARA-UPD-SEC
               END-IF
           END-IF
      *
           .
       2004-STATE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ更新処理
      *****************************************************************
       20041-APIPARA-UPD-SEC           SECTION.
      *
           IF    ( FLG-LOCK        =   ZERO )
               MOVE    SPA-PTID    TO  PARA023-PTID
           END-IF
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api023"        TO  APIPARA-GYOUMUID
           MOVE    SPA-API-ORCA-UID TO APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    1               TO  APIPARA-EDANUM
           MOVE    INCOMEREQ01-KARTE-UID
                                   TO  APIPARA-KARTE-UID
           MOVE    PARA023-REC     TO  APIPARA-DATA-REC
           MOVE    SMCNDATE-YMD    TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS    TO  APIPARA-UPHMS
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    "4001"  TO  SPA-ERRCD
                   MOVE    SPACE   TO  SPA-API-ORCA-UID
               END-IF
           END-IF
      *
            MOVE    APIPARA-REC     TO  MCPDATA-REC
            MOVE    "tbl_api_para"  TO  MCP-TABLE
            MOVE    "key"           TO  MCP-PATHNAME
            PERFORM 910-DBINSERT-SEC
            IF    ( MCP-RC      NOT =   ZERO )
                IF    ( SPA-ERRCD   =   SPACE )
                    MOVE    "4002"  TO  SPA-ERRCD
                    MOVE    SPACE   TO  SPA-API-ORCA-UID
                END-IF
            END-IF
      *
           .
       20041-APIPARA-UPD-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       800-GETUMATU-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-SYMD (1 : 6)    TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
      *
           .
       800-GETUMATU-EXT.
            EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD    TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角半角チェック処理
      *****************************************************************
       800-KANACHK-SEC                 SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-INSTR       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       800-KANACHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角チェック処理
      *****************************************************************
       801-KANACHK-UTF8-SEC            SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"             TO  KANACHK2-SYORI
           MOVE    WRK-INSTR       TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
           IF      ( KANACHK2-RC   =   ZERO )
               MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK2-RC2 =   7 )
                   MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       801-KANACHK-UTF8-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       800-ZENKAKU-SEC             SECTION.
      *
           INITIALIZE                  WRK-OUTSTR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-INSTR
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK-RC2 =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-EXT.
           EXIT.
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       801-ZENKAKU-UTF8-SEC            SECTION.
      *
           INITIALIZE                  WRK-OUTSTR
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "2"             TO  KANACHK2-SYORI
           MOVE    WRK-INSTR
                                   TO  KANACHK2-MAE-INPUT
           CALL    "ORCSKANACHK2"  USING   ORCSKANACHK2AREA
           IF      ( KANACHK2-RC   =   ZERO )
               MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
           ELSE
               IF    ( KANACHK2-RC2 =   7 )
                   MOVE    KANACHK2-OUT-INPUT
                                   TO  WRK-OUTSTR
               ELSE
                   MOVE    WRK-INSTR
                                   TO  WRK-OUTSTR
               END-IF
           END-IF
      *
           .
       801-ZENKAKU-UTF8-EXT.
           EXIT.
      *****************************************************************
      *    半角文字チェック
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-INSTR NOT =   SPACE )
               MOVE    SPACE       TO  ORCSKANACHKAREA
               INITIALIZE              ORCSKANACHKAREA
               MOVE    "1"         TO  KANACHK-SYORI
               MOVE    WRK-INSTR   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      ( KANACHK-RC    =   ZERO )
                   IF      ( KANACHK-SYUBETU   NOT =   1 )
                       MOVE    1   TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       MOVE    1   TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    文字列長取得
      *****************************************************************
       800-LENGTH-SEC              SECTION.
      *
      *    WRK-INSTRが空白の場合、FUNCTIONで長さを求めると１が返却される。
      *    空白の場合は0を返却するように調整
           MOVE    ZERO            TO  WRK-LEN
           IF    ( WRK-INSTR       NOT =   SPACE )
               MOVE    FUNCTION BYTE-LENGTH(
                           FUNCTION TRIM(WRK-INSTR TRAILING)
                       )           TO  WRK-LEN
           END-IF
      *
           .
       800-LENGTH-EXT.
           EXIT.
      *****************************************************************
      *    UID取得処理
      *****************************************************************
       800-ORCUIDSET-SEC               SECTION.
      *
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           .
       800-ORCUIDSET-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API"           TO  SPA-MOTOPG
           CALL    "ORCGSERRMSG"   USING
                                   SPA-AREA
                                   ORCSLOCKAREA
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    INCOMEREQ01-REQNUM        TO  INCOMERES01-REQNUM
           MOVE    INCOMEREQ01-REQMODE       TO  INCOMERES01-REQMODE
           IF    ( SPA-ERRCD       =   ZERO  OR  SPACE )
      *
               IF    ( SPA-API-SYU-COUNT     >   ZERO )
                   MOVE    "02"              TO  INCOMERES01-RESNUM
               ELSE
                   MOVE    "1001"            TO  SPA-ERRCD
                   MOVE    "01"              TO  INCOMERES01-RESNUM
               END-IF
           ELSE
               MOVE    "01"                  TO  INCOMERES01-RESNUM
           END-IF
      *
           MOVE    INCOMEREQ01-KARTE-UID     TO  INCOMERES01-KARTE-UID
           MOVE    SPA-API-ORCA-UID          TO  INCOMERES01-ORCA-UID
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  INCOMERES01-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  INCOMERES01-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  INCOMERES01-API-RESULT
           ELSE
               MOVE    "E"         TO  INCOMERES01-API-RESULT
               MOVE    SPA-ERRCD   TO  INCOMERES01-API-RESULT(2:)
           END-IF
           MOVE    SPA-ERRMSG      TO  INCOMERES01-API-RESULT-MSG
      *
           MOVE    WRK-TIME        TO  INCOMERES01-INFORMATION-TIME
      *
           MOVE    "xml_incomev3res01" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    INCOMERES01-AREA    TO  APIREQ-AREA
           MOVE    1                   TO  APIREQ-MODE
           MOVE    "incomev3res"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF    ( MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT   TO  SAPI01-BODY
               MOVE    "application/xml"
                                       TO  SAPI01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SPACE               TO  WRK-TABLE
                                           WRK-PATHNAME
           EVALUATE    SPA-API-OUTKBN
           WHEN    "2"
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key93"         TO  WRK-PATHNAME
           WHEN    "3"
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key94"         TO  WRK-PATHNAME
           WHEN    OTHER
               MOVE    "tbl_syunou"    TO  WRK-TABLE
               MOVE    "key95"         TO  WRK-PATHNAME
           END-EVALUATE
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    "0"                 TO  SYU-NYUGAIKBN
           MOVE    "00"                TO  SYU-SRYKA
           MOVE    "%"                 TO  SYU-SRYYMD
           MOVE    SPA-API-STSRYYMD    TO  SYU-SKYSTYMD
           MOVE    SPA-API-EDSRYYMD    TO  SYU-SKYEDYMD
           MOVE    "ALL"               TO  SYU-UPSRYYMD
           MOVE    SPA-API-LIMIT       TO  SYU-CHOSEI1
           MOVE    SPA-API-OFFSET      TO  SYU-CHOSEI2
           IF    ( SPA-API-SORT        =   "1" )
               IF    ( SPA-API-ORDER   =   "1" )
                   MOVE    "0"         TO  SYU-SELNYUGAIKBN
               ELSE
                   MOVE    "1"         TO  SYU-SELNYUGAIKBN
               END-IF
           ELSE
               IF    ( SPA-API-ORDER   =   "1" )
                   MOVE    "2"         TO  SYU-SELNYUGAIKBN
               ELSE
                   MOVE    "3"         TO  SYU-SELNYUGAIKBN
               END-IF
           END-IF
           DISPLAY "SYU-SELNYUGAIKBN=" SYU-SELNYUGAIKBN
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    WRK-PATHNAME        TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY98-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    "00000000"          TO  SYU-SKYSTYMD
           MOVE    "99999999"          TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key98"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY98-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY98-FET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key98"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY98-FET-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "api013"            TO  APIPARA-GYOUMUID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタFETCH処理
      *****************************************************************
       900-APIPARA-KEY4-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY6-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "api023"            TO  APIPARA-GYOUMUID
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタFETCH処理
      *****************************************************************
       900-APIPARA-KEY6-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           .
       900-APIPARA-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
           MOVE    MCP-WINDOW          TO  WRK-WINDOW
           MOVE    MCP-TERM            TO  WRK-TERM
           MOVE    SPA-API-ORCA-UID    TO  MCP-TERM
      *
           MOVE    "S02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
               MOVE     1              TO  FLG-LOCK
               MOVE    SPACE           TO  SLOCK-MSG
           END-IF
      *
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           MOVE    WRK-WINDOW          TO  MCP-WINDOW
           MOVE    WRK-TERM            TO  MCP-TERM
      *
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
           MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
           MOVE    MCP-WINDOW          TO  WRK-WINDOW
           MOVE    MCP-TERM            TO  WRK-TERM
           MOVE    SPA-API-ORCA-UID    TO  MCP-TERM
      *
           MOVE    "S02"               TO  SPA-MOTOPG
                                           MCP-WINDOW
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
      *
           MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
           MOVE    WRK-WINDOW          TO  MCP-WINDOW
           MOVE    WRK-TERM            TO  MCP-TERM
      *
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *****************************************************************
      *    XMLWRITE処理
      *****************************************************************
       910-XMLWRITE-SEC                 SECTION.
      *
           MOVE    "XMLWRITE"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       APIREQ-AREA
      *
           .
       910-XMLWRITE-EXT.
           EXIT.
