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
       PROGRAM-ID.         ORCGSAPI01S03.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 収納（請求確認）
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
           03  FLG-SYUMEI              PIC 9(01).
           03  FLG-SYURRK              PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-MONTH               PIC 9(01).
           03  FLG-LOCK                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXL                    PIC 9(05).
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
           03  WRK-NOZ                 PIC Z9.
           03  WRK-RATEZ               PIC ZZ9.
           03  WRK-MISYU-G.
               05  WRK-MISYU-OCC       OCCURS 2.
                   07  WRK-MISYU       PIC S9(10).
                   07  WRK-KANYU       PIC S9(10).
           03  WRK-INSTR               PIC X(500).
           03  WRK-OUTSTR              PIC X(500).
           03  WRK-LEN                 PIC S9(10).
           03  WRK-SMEI-JOTAIKBN       PIC X(1).
           03  WRK-SMEI-PROC-FLG       PIC X(1).
      *
       01  CONST-AREA.
           03  CONST-H190401           PIC X(08)   VALUE "20070401".
           03  CONST-SMEI-PROC-FLG-G.
               05  CONST-SEIKYU        PIC 9(01)   VALUE 1.
               05  CONST-TEISEI        PIC 9(01)   VALUE 2.
               05  CONST-NYUKIN        PIC 9(01)   VALUE 3.
               05  CONST-MINUS-NYUKIN  PIC 9(01)   VALUE 4.
      *
           COPY    "SAPI01SCR-SPA.INC".
      *
       01  TSYU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING //SYU-//
                                   BY        //TSYU-//.
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
      *    請求確認情報取得
           COPY    "CPORCSS015.INC".
      *
      *    API XML用定義体
           COPY    "CPAPIV3REQ.INC".
           COPY    "CPINCOMEV3RES02.INC".
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
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納履歴
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *    COPY    "CPSK4001.INC".
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
      *    患者労災
      *01  PTRSIINF-REC.
      *    COPY    "CPPTRSIINF.INC".
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
           COPY    "CPINCOMEV3REQ02.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               INCOMEREQ02-AREA
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
           INITIALIZE                  INCOMERES02-AREA
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF    ( INCOMEREQ02-ORCA-UID  =   SPACE )
               PERFORM 800-ORCUIDSET-SEC
               MOVE    UIDIO-UID   TO  SPA-API-ORCA-UID
           ELSE
               MOVE    INCOMEREQ02-ORCA-UID
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
               PERFORM 2001-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-DENPNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SYUNOU-CHK-SEC
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
           IF    ( INCOMEREQ02-KARTE-UID =   SPACE )
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
                   IF    ( INCOMEREQ02-ORCA-UID    = SPACE )
                       IF  ( APIPARA-KARTE-UID = INCOMEREQ02-KARTE-UID )
                           MOVE    "1038"      TO  SPA-ERRCD
                       END-IF
                   ELSE
                       IF  ( APIPARA-KARTE-UID = INCOMEREQ02-KARTE-UID )
                        AND( APIPARA-TERMID    = INCOMEREQ02-ORCA-UID )
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
               IF    ( INCOMEREQ02-ORCA-UID  NOT =   SPACE )
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
           IF    ( INCOMEREQ02-PTNUM =   SPACE )
               MOVE    "0071"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    INCOMEREQ02-PTNUM TO  SPTID-PTNUM
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
      *    伝票番号チェック
      *****************************************************************
       2001-DENPNUM-CHK-SEC            SECTION.
      *
           IF    ( INCOMEREQ02-DENPNUM =   SPACE )
               MOVE    "0100"          TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    INCOMEREQ02-DENPNUM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-API-DENPNUM
               END-IF
           END-IF
      *
      *
           .
       2001-DENPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       2001-NYUGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( INCOMEREQ02-NYUGAIKBN   =   SPACE )
               MOVE    "0086"      TO  SPA-ERRCD
           ELSE
               EVALUATE    FUNCTION
                             UPPER-CASE(INCOMEREQ02-NYUGAIKBN)
               WHEN  "I"
                   MOVE    "1"     TO  SPA-API-NYUGAIKBN
               WHEN  "O"
                   MOVE    "2"     TO  SPA-API-NYUGAIKBN
               WHEN  OTHER
                   MOVE    "0034"  TO  SPA-ERRCD
               END-EVALUATE
           END-IF
      *
           .
       2001-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納チェック
      *****************************************************************
       2001-SYUNOU-CHK-SEC             SECTION.
      *
            INITIALIZE                     SYUNOU-REC
            MOVE    SPA-HOSPNUM        TO  SYU-HOSPNUM
            MOVE    SPA-PTID           TO  SYU-PTID
            MOVE    SPA-API-NYUGAIKBN  TO  SYU-NYUGAIKBN
            MOVE    SPA-API-DENPNUM    TO  SYU-DENPNUM
            PERFORM 900-SYUNOU-KEY-SEL-SEC
            MOVE    SYUNOU-REC         TO  TSYU-REC
            IF    ( FLG-SYUNOU     NOT =   ZERO )
             OR   ( SYU-CREATEKBN      =   "3" )
                MOVE    "0085"         TO  SPA-ERRCD
            END-IF
      *
           .
       2001-SYUNOU-CHK-EXT.
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
           MOVE    TSYU-REC        TO  SYUNOU-REC
      *
           INITIALIZE                  SS015-AREA
           MOVE    SYU-SRYYMD      TO  SS015-SRYYMD
           CALL    "ORCSS015"          USING
                                       SS015-AREA
                                       SPA-AREA
      *
           PERFORM 200311-SYUNOU-EDIT-SEC
      *
           PERFORM 200311-SYUMEI-EDIT-SEC
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       200311-SYUNOU-EDIT-SEC          SECTION.
      *
           IF    ( SYU-SRYYMD  <   CONST-H190401 )
               COMPUTE SYU-JIHI-TOTAL-TAX
                   =   SYU-JIHI-TOTAL-TAX
                   +   SYU-JIHI-TAX
           END-IF
      *
           MOVE    SPA-PTNUM           TO  INCOMERES02-PTNUM
      *
           IF    ( SYU-NYUGAIKBN       =   "1" )
               MOVE    SYU-SKYSTYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES02-SRYYMD
               MOVE    SYU-SKYEDYMD    TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES02-SKYEDYMD
           ELSE
               MOVE    SYU-SRYYMD      TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES02-SRYYMD
           END-IF
      *
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  INCOMERES02-DENPPRTYMD
      *
           EVALUATE    SYU-NYUGAIKBN
           WHEN    "1"
               MOVE    "I"             TO  INCOMERES02-NYUGAIKBN
           WHEN    "2"
               MOVE    "O"             TO  INCOMERES02-NYUGAIKBN
           END-EVALUATE
      *
           MOVE    SYU-DENPNUM         TO  INCOMERES02-DENPNUM
           MOVE    SYU-HKNCOMBINUM     TO  INCOMERES02-HKNCOMBINUM
           MOVE    SYU-PTFTNRATE       TO  WRK-RATEZ
           MOVE    WRK-RATEZ           TO  INCOMERES02-FTNRATE
      *
           MOVE    SYU-SRYKA           TO  INCOMERES02-SRYKA
      *
           MOVE    SYU-SKYMONEY        TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES02-SKYMONEY
      *
           MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-SKYMONEY-TAX-SAI
      *
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES02-NYUKIN
      *
           MOVE    SYU-HKNTEKMONEY     TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES02-HKNMONEY
      *
           COMPUTE WRK-KINGAKU         =   SYU-RMSAGAKU
                                       +   SYU-SKYMONEY-SKJ-JIHI-KEI
                                       +   SYU-SKYMONEY-LIFE-JIHI-KEI
                                       +   SYU-TOTAL-TGMONEY
                                       +   SYU-TOTAL-TGMONEY-TAX
                                       +   SYU-JIHI-TOTAL
                                       +   SYU-JIHI-TOTAL-TAX
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES02-JIHIMONEY
      *
           MOVE    SYU-YKZ-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-YKZMONEY
      *
           MOVE    SYU-RJN-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-RJNMONEY
      *
           MOVE    SYU-KOH-FTN         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-KOHMONEY
      *
           MOVE    SYU-SKYMONEY-SKJ-KEI
                                       TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-SKJMONEY
           MOVE    SYU-SKYMONEY-LIFE-KEI
                                       TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-LIFEMONEY
      *
           MOVE    SYU-RSI-TOTAL       TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-RSI-TOTAL
      *
           MOVE    SYU-DISCOUNT-MONEY  TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-GENMEN
      *
           MOVE    SYU-CHOSEI1         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-CHOSEI1
      *
           MOVE    SYU-CHOSEI2         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-CHOSEI2
      *
           MOVE    SYU-TOTAL-HKNTEN    TO  WRK-KINGAKUM9
           MOVE    WRK-KINGAKUM9       TO  INCOMERES02-TOTAL-TEN
      *
           COMPUTE WRK-KINGAKU
               =   SYU-TOTAL-TGMONEY
               +   SYU-TOTAL-TGMONEY-TAX
           MOVE    WRK-KINGAKU         TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-TOTAL-TGMONEY
      *
           MOVE    SYU-TGMONEY-TAX-SAI TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM       TO  INCOMERES02-TGMONEY-TAX-SAI
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   16 )
      *
               IF    ( IDX0                    NOT =   16 )
                 OR  ( SYU-HKNTEN (IDX0)       NOT =   ZERO )
                 OR  ( SYU-TGMONEY(IDX0)       NOT =   ZERO )
                 OR  ( SYU-TGMONEY-TAX (IDX0)  NOT =   ZERO )
                       MOVE    SS015-SRYITEML (IDX0)
                               TO  INCOMERES02-SRYITEM-KBN  (IDX0)
                       MOVE    SS015-SRYITEMMEIL (IDX0)
                               TO  INCOMERES02-SRYITEM-NAME (IDX0)
                       MOVE    SYU-HKNTEN (IDX0)
                               TO  WRK-KINGAKUM9
                       MOVE    WRK-KINGAKUM9
                               TO  INCOMERES02-HKNTEN (IDX0)
                       COMPUTE WRK-KINGAKU
                           =   SYU-TGMONEY (IDX0)
                           +   SYU-TGMONEY-TAX (IDX0)
                       MOVE    WRK-KINGAKU
                               TO  WRK-KINGAKUMM
                       MOVE    WRK-KINGAKUMM
                               TO  INCOMERES02-TGMONEY (IDX0)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SYU-JIHI-TOTAL  TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  INCOMERES02-JIHI-TOTAL
      *
           MOVE    SYU-JIHI-TOTAL-TAX
                                   TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  INCOMERES02-JIHI-TOTAL-TAX
      *
           MOVE    SYU-JIHI-TAX    TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM   TO  INCOMERES02-JIHI-TOTAL-TAX-SAI
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   10 )
      *
               MOVE    2               TO  IDXA
               IF    ( SYU-NYUGAIKBN   =   "1" )
                   MOVE    1           TO  IDXA
               END-IF
      *
               IF    ( SS015-JIHINAME (IDXA IDX0)  NOT =   SPACE )
                OR   ( SYU-JIHI-TAXNASI (IDX0)     NOT =   ZERO )
                OR   ( SYU-JIHI-TAXARI (IDX0)      NOT =   ZERO )
      *
                   MOVE    IDX0    TO  WRK-NOZ
                   MOVE    WRK-NOZ TO  INCOMERES02-JIHI-NO (IDX0)
      *
                   MOVE    SS015-JIHINAME (IDXA IDX0)
                                   TO  INCOMERES02-JIHI-NAME (IDX0)
                   MOVE    SYU-JIHI-TAXNASI (IDX0)
                                   TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM
                                   TO  INCOMERES02-JIHI-TAXNASI (IDX0)
                   MOVE    SYU-JIHI-TAXARI (IDX0)
                                   TO  WRK-KINGAKUMM
                   MOVE    WRK-KINGAKUMM
                                   TO  INCOMERES02-JIHI-TAXARI (IDX0)
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    SYU-RSISHOSHIN-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-RSISHOSHIN-MONEY
      *
           MOVE    SYU-RSISAISHIN-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-RSISAISHIN-MONEY
      *
           MOVE    SYU-RSISDO-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-RSISDO-MONEY
      *
           MOVE    SYU-RSIETC-MONEY
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-RSIETC-MONEY
      *
           MOVE    SYU-RYOYOHI-SKJ
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-SKJRYOYO
      *
           MOVE    SYU-RYOYOHI-LIFE
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-LIFERYOYO
      *
           MOVE    SYU-RYOYOHI-SKJ-JIHI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-SKJRYOYO-JIHI
      *
           MOVE    SYU-SKYMONEY-SKJ-JIHI-KEI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-SKJFTN-JIHI
      *
           MOVE    SYU-RYOYOHI-LIFE-JIHI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-LIFERYOYO-JIHI
      *
           MOVE    SYU-SKYMONEY-LIFE-JIHI-KEI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-LIFEFTN-JIHI
      *
           MOVE    SYU-RMSAGAKU
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-RMSAGAKU
      *
           MOVE    SYU-RMSAGAKU-TAX-SAI
                                 TO  WRK-KINGAKUMM
           MOVE    WRK-KINGAKUMM
                                 TO  INCOMERES02-RMSAGAKU-TAX-SAI
      *
           .
       200311-SYUNOU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    収納明細編集処理
      *****************************************************************
       200311-SYUMEI-EDIT-SEC          SECTION.
      *
           MOVE    ZERO            TO  IDXA
      *
           PERFORM 900-SYUMEI-KEY6-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( IDXA            >=  99 )
      *
               COMPUTE IDXA  = IDXA  + 1
      *
               MOVE    SMEI-DENPEDANUM TO  WRK-NOZ
               MOVE    WRK-NOZ         TO  INCOMERES02-TDENPEDANUM(IDXA)
      *
               MOVE    SMEI-NYUHEN-YMD TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE        TO  INCOMERES02-TNYUHENYMD (IDXA)
      *
               MOVE    SMEI-DAILYKEY(9:)
                                       TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-TIME (1:5)  TO  INCOMERES02-TNYUHENTIME(IDXA)
      *
               MOVE    SMEI-JOUTAIKBN     TO  WRK-SMEI-JOTAIKBN
               PERFORM 800-SMEI-JOTAIKBN-SEC
      *
               IF    ( WRK-SMEI-PROC-FLG  = CONST-SEIKYU
                                        OR  CONST-TEISEI )
                   MOVE    SMEI-SKYMONEY
                                       TO   WRK-KINGAKUM9
                   MOVE    WRK-KINGAKUM9
                                       TO   INCOMERES02-TSKYMONEY (IDXA)
      *
                   IF    ( SMEI-SYURRKNUM  NOT =   ZERO )
                       PERFORM 900-SYURRK-KEY-SEL-SEC
                       MOVE    SYURRK-CHOSEI1
                                       TO   WRK-KINGAKUM9
                       MOVE    WRK-KINGAKUMM
                                       TO  INCOMERES02-TCHOSEI1(IDXA)
                       MOVE    SYURRK-CHOSEI2
                                       TO   WRK-KINGAKUM9
                       MOVE    WRK-KINGAKUMM
                                       TO  INCOMERES02-TCHOSEI2(IDXA)
                   END-IF
               END-IF
      *
               MOVE    SMEI-NYUHEN-MONEY
                                       TO   WRK-KINGAKUM9
               MOVE    WRK-KINGAKUM9   TO   INCOMERES02-TNYUKIN (IDXA)
      *
               IF    ( SMEI-JOUTAIKBN  =   SPACE )
                   MOVE    "0"         TO  INCOMERES02-TJOTAI (IDXA)
               ELSE
                   MOVE    SMEI-JOUTAIKBN
                                       TO  INCOMERES02-TJOTAI (IDXA)
               END-IF
      *
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   SS015-SMEIJOTAILST-MAX )
                        OR   ( INCOMERES02-TJOTAI-NAME  (IDXA)
                                       NOT =   SPACE )
      *
                   IF    ( SS015-SMEIJOTAIL (IDXL)
                               =   SMEI-JOUTAIKBN )
                       MOVE    SS015-SMEIJOTAIMEIL (IDXL)
                               TO  INCOMERES02-TJOTAI-NAME (IDXA)
                   END-IF
      *
               END-PERFORM
      *
               IF    ( SMEI-NYUKIN-HOHO    NOT =   ZERO )
                AND  ( SMEI-NYUKIN-HOHO    NOT =   SPACE )
      *
                   MOVE    SMEI-NYUKIN-HOHO
                                   TO  INCOMERES02-TNYUKIN-HOHO (IDXA)
                   PERFORM VARYING IDXL    FROM    1   BY  1
                           UNTIL ( IDXL    >   SS015-NYKN-HOHOLST-MAX )
                            OR   ( INCOMERES02-TNYUKIN-HOHO-NAME (IDXA)
                                       NOT =   SPACE )
      *
                       IF    ( SS015-NYKN-HOHOL (IDXL)
                                           =   SMEI-NYUKIN-HOHO)
                           MOVE    SS015-NYKN-HOHOMEIL (IDXL)
                               TO  INCOMERES02-TNYUKIN-HOHO-NAME (IDXA)
                       END-IF
      *
                   END-PERFORM
      *
               END-IF
      *
               PERFORM 900-SYUMEI-KEY6-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200311-SYUMEI-EDIT-EXT.
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
           MOVE    INCOMEREQ02-KARTE-UID
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
      *    収納明細状態区分->処理フラグ変換処理
      *****************************************************************
       800-SMEI-JOTAIKBN-SEC             SECTION.
      *
           MOVE    SPACE             TO  WRK-SMEI-PROC-FLG
      *
           EVALUATE   SMEI-JOUTAIKBN
           WHEN    " "
           WHEN    "1"
           WHEN    "9"
               MOVE   CONST-SEIKYU   TO  WRK-SMEI-PROC-FLG
           WHEN    "3"
           WHEN    "7"
           WHEN    "8"
           WHEN    "5"
               MOVE   CONST-TEISEI    TO  WRK-SMEI-PROC-FLG
           WHEN    "2"
                MOVE  CONST-NYUKIN    TO  WRK-SMEI-PROC-FLG
           WHEN    "4"
           WHEN    "6"
                MOVE  CONST-MINUS-NYUKIN
                                      TO  WRK-SMEI-PROC-FLG
           END-EVALUATE
      *
           .
       800-SMEI-JOTAIKBN-EXT.
           EXIT.
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
           MOVE    INCOMEREQ02-REQNUM        TO  INCOMERES02-REQNUM
           MOVE    INCOMEREQ02-REQMODE       TO  INCOMERES02-REQMODE
           IF    ( SPA-ERRCD       =   ZERO  OR  SPACE )
      *
               MOVE    "02"                  TO  INCOMERES02-RESNUM
           ELSE
               MOVE    "01"                  TO  INCOMERES02-RESNUM
           END-IF
      *
           MOVE    INCOMEREQ02-KARTE-UID     TO  INCOMERES02-KARTE-UID
           MOVE    SPA-API-ORCA-UID          TO  INCOMERES02-ORCA-UID
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  INCOMERES02-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  INCOMERES02-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  INCOMERES02-API-RESULT
           ELSE
               MOVE    "E"         TO  INCOMERES02-API-RESULT
               MOVE    SPA-ERRCD   TO  INCOMERES02-API-RESULT(2:)
           END-IF
           MOVE    SPA-ERRMSG      TO  INCOMERES02-API-RESULT-MSG
      *
           MOVE    WRK-TIME        TO  INCOMERES02-INFORMATION-TIME
      *
           MOVE    "xml_incomev3res02" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    INCOMERES02-AREA    TO  APIREQ-AREA
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
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"       TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細検索処理
      *****************************************************************
       900-SYUMEI-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUMEI
      *
           INITIALIZE                  SYUMEI-REC
           MOVE    SYU-HOSPNUM     TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN   TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID        TO  SMEI-PTID
           MOVE    SYU-DENPNUM     TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC      TO  MCPDATA-REC
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUMEI-REC
           ELSE
               MOVE    1           TO  FLG-SYUMEI
               INITIALIZE              SYUMEI-REC
           END-IF
      *
           .
       900-SYUMEI-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細FETCH処理
      *****************************************************************
       900-SYUMEI-KEY6-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"    TO  MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUMEI-REC
           ELSE
               MOVE    1           TO  FLG-SYUMEI
               INITIALIZE              SYUMEI-REC
           END-IF
      *
           .
       900-SYUMEI-KEY6-FET-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SMEI-SYUEDANUM      TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ検索処理
      *****************************************************************
      *900-HKNCOMBI-KEY-SEL-SEC            SECTION.
      *
      *    MOVE    ZERO                TO  FLG-HKNCOMBI
      *
      *    INITIALIZE                      HKNCOMBI-REC
      *    MOVE    SYU-HOSPNUM         TO  COMB-HOSPNUM
      *    MOVE    SYU-PTID            TO  COMB-PTID
      *    MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
      *    MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF    ( MCP-RC              =   ZERO )
      *        MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
      *    ELSE
      *        MOVE    1               TO  FLG-HKNCOMBI
      *        INITIALIZE                  HKNCOMBI-REC
      *    END-IF
      *
      *    MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    .
      *900-HKNCOMBI-KEY-SEL-EXT.
      *    EXIT.
      *****************************************************************
      *    患者労災保険情報検索処理
      *****************************************************************
      *900-PTRSIINF-KEY-SEL-SEC            SECTION.
      *
      *    MOVE    ZERO                TO  FLG-PTRSIINF
      *
      *    INITIALIZE                      PTRSIINF-REC
      *    MOVE    SYU-HOSPNUM         TO  PTRSIINF-HOSPNUM
      *    MOVE    SYU-PTID            TO  PTRSIINF-PTID
      *    MOVE    COMB-HKNID          TO  PTRSIINF-HKNID
      *    MOVE    PTRSIINF-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF    ( MCP-RC              =   ZERO )
      *        MOVE    MCPDATA-REC     TO  PTRSIINF-REC
      *    ELSE
      *        MOVE    1               TO  FLG-PTRSIINF
      *    END-IF
      *
      *    MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    .
      *900-PTRSIINF-KEY-SEL-EXT.
      *    EXIT.
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
