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
       PROGRAM-ID.         ORCGQAPI01S01.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者照会
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
      *    パラメータファイル
           SELECT  BQ01-FILE   ASSIGN  PRFNAME
                               ORGANIZATION    IS  SEQUENTIAL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメータファイル
       FD  BQ01-FILE.
           COPY    "CPBQ01.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //PRFNAME//.
      *    パラメータファイル名
           COPY    "CPCOMMONDAT3.INC"
                                   REPLACING  //TMPFLPARA//
                                   BY         //PUSHNAME//.
      *
           COPY    "CPASGNPARA.INC".
      *
           COPY    "Q01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-WILD-CARD           PIC 9(01).
           03  FLG-KIKANOVER           PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXB                    PIC 9(05).
           03  IDXC                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-APIPARA             PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
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
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTNUM-WK            PIC X(20).
           03  WRK-PTNUM-LENGTH        PIC 9(05).
           03  WRK-SYS-LENGTH1         PIC 9(05).
           03  WRK-SYS-LENGTH2         PIC 9(05).
           03  WRK-ST                  PIC 9(05).
           03  WRK-INSTR               PIC X(500).
           03  WRK-OUTSTR              PIC X(500).
           03  WRK-LEN                 PIC S9(10).
           03  WRK-SRYSURYO            PIC 99999V99999.
           03  WRK-SRYSURYOZ           PIC ZZZZ9.99999.
           03  WRK-KAISUZ              PIC ZZZZ9.
           03  WRK-RECECOM-G.
               05  WRK-RECECOM-ALL     PIC X(50).
               05  WRK-RECECOM         PIC X(50)
                                       OCCURS     5.
           03  WRK-SORT-KEYID.
               05  WRK-SORT-KEYID9     PIC 9(01).
           03  WRK-ERRCD-OCC           OCCURS     3.
               05  WRK-ERRCD1          PIC X(04).
               05  WRK-ERRCD2          PIC X(04).
           03  WRK-EDSRYYMD            PIC X(08).
           03  WRK-NAME                            PIC X(200).
           03  WRK-NAME-G.
               05  WRK-NAME1           PIC X(100).
               05  WRK-NAME2           PIC X(100).
           03  WRK-ORCSSTR-NUM1        PIC 9(03).
           03  WRK-TEMPDIR-ROOT        PIC X(512).
           03  WRK-NUM02-G.
               05  WRK-NUM02           PIC 9(02).
           03  WRK-TERMID              PIC X(36).
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-XSRYCDKBN           PIC X(01).
      *
       01  REQ-AREA.
           03  REQ-BASEYMD             PIC X(10).
      *
       01  RES-AREA.
           03  RES-ORCA-UID            PIC X(36).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-UTF8                          PIC 9(01)   VALUE 2.
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
      *    照会サブ２（検索条件編集）
           COPY    "CPQSUB02.INC".
      *
      *    照会サブ３（コンボボックス値返却）
           COPY    "CPQSUB03.INC".
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
      *    API XML書き込み用定義体
           COPY    "CPFINDV3RES.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ(拡張文字対応版)
           COPY    "CPORCSKANACHK2.INC".
      *
      *    一時ディレクトリ作成サブ
           COPY "CPORCSTEMPDIR.INC".
      *
      *    バッチジョブ制御
           COPY    "CPORCSMONBATCH.INC".
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
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA013.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    シェルテーブル
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
           COPY    "CPFINDV3REQ.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               XML-FINDV3REQ
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
           INITIALIZE                  REQ-AREA
           INITIALIZE                  RES-AREA
      *
           MOVE    SPACE           TO  SPA-Q01FREE
           INITIALIZE                  SPA-Q01FREE
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
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-FINDV3RES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF    ( FINDREQ-ORCA-UID    =   SPACE )
               PERFORM 800-ORCUIDSET-SEC
               MOVE    UIDIO-UID      TO   RES-ORCA-UID
           ELSE
               MOVE    FINDREQ-ORCA-UID
                                      TO   RES-ORCA-UID
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
               PERFORM 2001-BASEYMD-CHK-SEC
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
      *        リスト項目取得
               PERFORM 800-ORCGQSUB03-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-HOKEN-KOHI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BYOMEI-SRYACT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KAIGO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SORT-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
           MOVE    ZERO                    TO  FLG-KIKANOVER
           MOVE    SPA-Q01-STSRYYMD        TO  WRK-KEISANYMD
           IF    ( WRK-KEISANYMD           =   SPACE )
               MOVE    "00000000"          TO  WRK-KEISANYMD
           ELSE
               MOVE    WRK-KEISANYMD       TO  WRK-YMD
               MOVE    11                  TO  WRK-ZOGEN
               MOVE    "2"                 TO  WRK-ZOGENPTN
               PERFORM 800-CALENDAR-SEC
           END-IF
           MOVE    SPA-Q01-EDSRYYMD        TO  WRK-EDSRYYMD
           IF    ( WRK-EDSRYYMD            =   SPACE )
               MOVE    "99999999"          TO  WRK-EDSRYYMD
           END-IF
           IF   (  WRK-KEISANYMD (1:6)    >=   WRK-EDSRYYMD (1:6))
               CONTINUE
           ELSE
               MOVE    1                   TO  FLG-KIKANOVER
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KIHON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-HOKEN-KOHI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-BYOMEI-SRYACT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KAIGO-CHK-SEC
           END-IF
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本項目チェック
      *****************************************************************
       2001-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-OUTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STBIRTHDAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDBIRTHDAY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-BIRTHMON-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SEX-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STLASTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDLASTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-DRCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-NYUGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYKA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STPTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDPTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-AGREEMENT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-DEATH-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TSTPT-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-NAME-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-JYUSHO-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TEL-TO-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-TEL-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-CONDITION1-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-CONDITION2-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-CONDITION3-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SELCOMMENT-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-COMMENT-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-GENMEN-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TSY-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STTSYSKBT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    FINDREQ-STTSYSKBT
               WHEN    "2"
               WHEN    "3"
                   PERFORM 2001-TSYYM-CHK-SEC
               WHEN    "1"
               WHEN    "4"
                   PERFORM 2001-STTSYYMD-CHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 2001-EDTSYYMD-CHK-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TOKKICD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TOKKINAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TOKKINAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STTOTALTEN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDTOTALTEN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SAIGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TENKENRECE-CHK-SEC
           END-IF
      *
           .
       2001-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険・公費チェック
      *****************************************************************
       2001-HOKEN-KOHI-CHK-SEC         SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-HKNEDYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-HONKZKKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-FTNRATE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JKNHKNJANUM-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-HKNJANUM-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JKNHKNNUM-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-HKNNUM-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-KIGO-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-NUM-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KOHEDYM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JKNKOHNUM-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-KOHNUM-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-FTNJANUM-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JKNHKNKOH-CHK-SEC
           END-IF
      *
           .
       2001-HOKEN-KOHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名診療行為チェック
      *****************************************************************
       2001-BYOMEI-SRYACT-CHK-SEC      SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STBYOYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDBYOYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-TENKIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STTENKIYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-EDTENKIYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-UTAGAIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KYUSEIKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SYUBYOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JKNBYOMEI-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-BYOMEI-CHK-SEC
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYSYUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-HOKATSUKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-SRYVALITEM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-JKNSRYACT-CHK-SEC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-SRYCD-CHK-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-RECECOM-CHK-SEC
           END-PERFORM
      *
           .
       2001-BYOMEI-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護チェック
      *****************************************************************
       2001-KAIGO-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KHSKBT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    FINDREQ-KHSKBT
               WHEN    "2"
               WHEN    "3"
                   PERFORM 2001-KHYM-CHK-SEC
               WHEN    "1"
               WHEN    "4"
                   PERFORM 2001-STKHYMD-CHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 2001-EDKHYMD-CHK-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KHHKNJANUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KNSKBT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               EVALUATE    FINDREQ-KNSKBT
               WHEN    "2"
               WHEN    "3"
                   PERFORM 2001-KNYM-CHK-SEC
               WHEN    "1"
               WHEN    "4"
                   PERFORM 2001-STKNYMD-CHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 2001-EDKNYMD-CHK-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-KNJOTAI-CHK-SEC
           END-IF
      *
           .
       2001-KAIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並び順チェック
      *****************************************************************
       2001-SORT-CHK-SEC               SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   3 )
                        OR   ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 2001-SORT-KEYID-CHK-SEC
               END-PERFORM
           END-IF
      *
           .
       2001-SORT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カルテＵＩＤチェック
      *****************************************************************
       2001-KARTE-UID-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-APIPARA
      *
           IF    ( FINDREQ-KARTE-UID   =   SPACE )
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
                   MOVE    APIPARA-DATA-REC    TO  PARA013-REC
      *
                   IF    ( FINDREQ-ORCA-UID    =   SPACE )
                       IF    ( APIPARA-KARTE-UID   = FINDREQ-KARTE-UID )
                           MOVE    "1038"      TO  SPA-ERRCD
                       END-IF
                   ELSE
                       IF    ( APIPARA-KARTE-UID   = FINDREQ-KARTE-UID )
                        AND  ( APIPARA-TERMID      = FINDREQ-ORCA-UID )
                           IF   ( PARA013-STATE    =   "01" )
                               MOVE    "1040"  TO  SPA-ERRCD
                           ELSE
                               PERFORM 20011-TERMINATE-SEC
                           END-IF
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
               IF    ( FINDREQ-ORCA-UID    NOT =   SPACE )
                   IF    ( CNT-APIPARA         =   ZERO )
                       MOVE    "0206"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPACE               TO  RES-ORCA-UID
           END-IF
      *
           .
       2001-KARTE-UID-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日チェック
      *****************************************************************
       2001-BASEYMD-CHK-SEC            SECTION.
      *
      *    IF    ( FINDREQ-BASEYMD       =   SPACE )
               MOVE    SPA-SYSYMD      TO  REQ-BASEYMD
      *    ELSE
      *        MOVE    FINDREQ-BASEYMD TO WRK-DATE
      *        PERFORM 800-SYMD-SEC
      *        PERFORM 800-HIZUKE-SEC
      *        IF    ( WRK-EDTYMD1     =   SPACE )
      *            MOVE    "0210"      TO  SPA-ERRCD
      *        ELSE
      *            MOVE    WRK-SYMD    TO  REQ-BASEYMD
      *        END-IF
      *    END-IF
      *
           .
       2001-BASEYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    照会終了処理
      *****************************************************************
       20011-TERMINATE-SEC             SECTION.
      *
           COMPUTE CNT-APIPARA =   CNT-APIPARA +   1
      *
           CALL    "ORCGQSUB04"    USING
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
           IF    ( FINDREQ-STSRYYMD      =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-STSRYYMD  TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-STSRYYMD
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
           IF    ( FINDREQ-EDSRYYMD      =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-EDSRYYMD  TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0101"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-EDSRYYMD
               END-IF
           END-IF
      *
           .
       2001-EDSRYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索結果区分チェック
      *****************************************************************
       2001-OUTKBN-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-OUTKBN        =   "2" OR "3" )
               MOVE    FINDREQ-OUTKBN    TO  SPA-Q01-OUTKBN
           ELSE
               MOVE    "1"               TO  FINDREQ-OUTKBN
                                             SPA-Q01-OUTKBN
           END-IF
      *
           .
       2001-OUTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始誕生日チェック
      *****************************************************************
       2001-STBIRTHDAY-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-STBIRTHDAY    =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-STBIRTHDAY
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0102"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-STBIRTHDAY
               END-IF
           END-IF
      *
           .
       2001-STBIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了誕生日チェック
      *****************************************************************
       2001-EDBIRTHDAY-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-EDBIRTHDAY    =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-EDBIRTHDAY
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0102"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-EDBIRTHDAY
               END-IF
           END-IF
      *
           .
       2001-EDBIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    誕生月チェック
      *****************************************************************
       2001-BIRTHMON-CHK-SEC           SECTION.
      *
           IF  (   FINDREQ-BIRTHMON      =   SPACE )
               MOVE    "00"           TO   SPA-Q01-BIRTHMON
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    FINDREQ-BIRTHMON  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0133"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-NUM02
                   MOVE    WRK-NUM02-G TO  SPA-Q01-BIRTHMON
               END-IF
           END-IF
      *
           .
       2001-BIRTHMON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    性別チェック
      *****************************************************************
       2001-SEX-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-SEX       =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SEXLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SEX   =   QSUB03-SEXL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0109"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SEX   TO  SPA-Q01-SEX
               END-IF
      *
           END-IF
      *
           .
       2001-SEX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始最終受診日チェック
      *****************************************************************
       2001-STLASTYMD-CHK-SEC          SECTION.
      *
           IF  ( FINDREQ-STLASTYMD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-STLASTYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0105"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-STLASTYMD
               END-IF
           END-IF
      *
           .
       2001-STLASTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了最終受診日チェック
      *****************************************************************
       2001-EDLASTYMD-CHK-SEC          SECTION.
      *
           IF  ( FINDREQ-EDLASTYMD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-EDLASTYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0105"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-EDLASTYMD
               END-IF
           END-IF
      *
           .
       2001-EDLASTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクターチェック
      *****************************************************************
       2001-DRCD-CHK-SEC               SECTION.
      *
           IF    ( FINDREQ-DRCD      =   SPACE )
               CONTINUE
           ELSE
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-DRCDLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-DRCD  =   QSUB03-DRCDL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA        =   ZERO )
                   MOVE    "1010"          TO  SYS-KANRICD
                   MOVE    FINDREQ-DRCD      TO  SYS-KBNCD
                   MOVE    QSUB03-STSRYYMD TO  SYS-STYUKYMD
                   MOVE    QSUB03-EDSRYYMD TO  SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY26-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    "0162"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    FINDREQ-DRCD  TO  SPA-Q01-DRCD
                   MOVE    "2"           TO  FINDREQ-NYUGAIKBN
               END-IF
           END-IF
      *
           .
       2001-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外区分チェック
      *****************************************************************
       2001-NYUGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-NYUGAIKBN     =   SPACE )
               CONTINUE
           ELSE
               EVALUATE    FINDREQ-NYUGAIKBN
               WHEN    SPACE
                   CONTINUE
               WHEN    "1"
               WHEN    "I"
                   MOVE    "1"         TO  SPA-Q01-NYUGAIKBN
               WHEN    OTHER
                   MOVE    "2"         TO  SPA-Q01-NYUGAIKBN
               END-EVALUATE
           END-IF
      *
           .
       2001-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科チェック
      *****************************************************************
       2001-SRYKA-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-SRYKA       =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SRYKALST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SRYKA   =   QSUB03-SRYKAL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0117"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SRYKA TO  SPA-Q01-SRYKA
               END-IF
      *
           END-IF
      *
           .
       2001-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    死亡区分チェック
      *****************************************************************
       2001-DEATH-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-DEATH       =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-DEATHLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-DEATH   =   QSUB03-DEATHL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0128"         TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-DEATH  TO  SPA-Q01-DEATH
               END-IF
      *
           END-IF
      *
           .
       2001-DEATHKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    テスト患者区分チェック
      *****************************************************************
       2001-TSTPT-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-TSTPT       =   SPACE )
               MOVE    "a"         TO  SPA-Q01-CHKTSTPT
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-TSTPTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-TSTPT   =   QSUB03-TSTPTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0149"         TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-TSTPT TO  SPA-Q01-CHKTSTPT
               END-IF
      *
           END-IF
      *
           .
       2001-TSTPT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始患者番号チェック
      *****************************************************************
       2001-STPTNUM-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-PTNUM
           MOVE    1                   TO  WRK-PTNUM-LENGTH
      *
           IF    ( FINDREQ-STPTNUM       =   SPACE )
               CONTINUE
           ELSE
      *
               STRING  FINDREQ-STPTNUM   DELIMITED BY SPACE
               INTO    WRK-PTNUM
               WITH    POINTER WRK-PTNUM-LENGTH
               END-STRING
      *
               COMPUTE WRK-PTNUM-LENGTH = WRK-PTNUM-LENGTH - 1
      *
      *        拡張構成か自由構成かつ非数値型の場合は単純に桁数を行う。
      *        上記以外は桁数（溢れ）チェック＋ZERO詰め編集を行う。
               EVALUATE    SPA-1009-PTNUMKSIKBN ALSO SPA-1009-JIYKSIKBN
               WHEN    "3"     ALSO    ANY
               WHEN    "1"     ALSO    NOT "2"
      *
                   PERFORM 20011-PTNUM-LENGTH-CHK-SEC
      *
               WHEN    OTHER
      *
                   PERFORM 20012-PTNUM-LENGTH-CHK-SEC
      *
               END-EVALUATE
      *
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-PTNUM   TO  SPA-Q01-STPTNUM
               END-IF
           END-IF
      *
           .
       2001-STPTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了患者番号チェック
      *****************************************************************
       2001-EDPTNUM-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-PTNUM
           MOVE    1                   TO  WRK-PTNUM-LENGTH
      *
           IF    ( FINDREQ-EDPTNUM       =   SPACE )
               CONTINUE
           ELSE
      *
               STRING  FINDREQ-EDPTNUM   DELIMITED BY SPACE
               INTO    WRK-PTNUM
               WITH    POINTER WRK-PTNUM-LENGTH
               END-STRING
      *
               COMPUTE WRK-PTNUM-LENGTH = WRK-PTNUM-LENGTH - 1
      *
      *        拡張構成か自由構成かつ非数値型の場合は単純に桁数を行う。
      *        上記以外は桁数（溢れ）チェック＋ZERO詰め編集を行う。
               EVALUATE    SPA-1009-PTNUMKSIKBN ALSO SPA-1009-JIYKSIKBN
               WHEN    "3"     ALSO    ANY
               WHEN    "1"     ALSO    NOT "2"
      *
                   PERFORM 20011-PTNUM-LENGTH-CHK-SEC
      *
               WHEN    OTHER
      *
                   PERFORM 20012-PTNUM-LENGTH-CHK-SEC
      *
               END-EVALUATE
      *
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-PTNUM   TO  SPA-Q01-EDPTNUM
               END-IF
           END-IF
      *
           .
       2001-EDPTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    桁数チェック１
      *****************************************************************
       20011-PTNUM-LENGTH-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO      WRK-SYS-LENGTH1
                                           WRK-SYS-LENGTH2
      *
           EVALUATE    SPA-1009-PTNUMKSIKBN
           WHEN    "1"
               MOVE    SPA-1009-JIYKSIKETA
                                   TO      WRK-SYS-LENGTH1
           WHEN    "2"
               MOVE    SPA-1009-HJNKSIRENNUMKETA
                                   TO      WRK-SYS-LENGTH1
               IF  ( SPA-1009-HJNKSINENKBN     =   "1" )
      *            和暦
                   MOVE    3   TO      WRK-SYS-LENGTH2
               ELSE
      *            西暦
                   MOVE    4   TO      WRK-SYS-LENGTH2
               END-IF
      *
               EVALUATE    SPA-1009-HJNKSIKBN
               WHEN    "1"
                   COMPUTE WRK-SYS-LENGTH1
                       =   WRK-SYS-LENGTH1
                       +   WRK-SYS-LENGTH2
                       +   1
               WHEN    "2"
                   COMPUTE WRK-SYS-LENGTH1
                       =   WRK-SYS-LENGTH1
                       +   WRK-SYS-LENGTH2
               WHEN    "3"
                   COMPUTE WRK-SYS-LENGTH1
                       =   WRK-SYS-LENGTH1
                       +   1
               WHEN    "4"
                   CONTINUE
               END-EVALUATE
           WHEN    "3"
               COMPUTE WRK-SYS-LENGTH1
                    =  SPA-1009-KKCKSIMAEKETA
                    +  SPA-1009-KKCKSIRENNUMKETA
                    +  SPA-1009-KKCKSIATOKETA
           END-EVALUATE
      *
           IF  ( WRK-PTNUM-LENGTH  NOT =   WRK-SYS-LENGTH1 )
               MOVE    "0211"      TO  SPA-ERRCD
           END-IF
      *
           .
       20011-PTNUM-LENGTH-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    桁数チェック２
      *****************************************************************
       20012-PTNUM-LENGTH-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO      WRK-SYS-LENGTH1
      *
      *    自由構成の桁数編集
           IF  ( SPA-1009-PTNUMKSIKBN  =   "1" )
               MOVE    SPA-1009-JIYKSIKETA
                                   TO      WRK-SYS-LENGTH1
           ELSE
      *
      *        標準構成の桁数
      *        システム管理マスタで設定された桁数をカウント
               MOVE    SPA-1009-HJNKSIRENNUMKETA
                                   TO      WRK-SYS-LENGTH1
               IF  ( SPA-1009-HJNKSIKBN
                                   =   "1"  OR  "2" )
                   EVALUATE    SPA-1009-HJNKSINENKBN
                   WHEN    "1"
                       COMPUTE WRK-SYS-LENGTH1
                           =   WRK-SYS-LENGTH1
                           +   3
                   WHEN    "2"
                       COMPUTE WRK-SYS-LENGTH1
                           =   WRK-SYS-LENGTH1
                           +   4
                   END-EVALUATE
               END-IF
      *
               IF  ( SPA-1009-HJNKSIKBN    =   "1"  OR  "3" )
                   COMPUTE WRK-SYS-LENGTH1
                       =   WRK-SYS-LENGTH1
                       +   1
               END-IF
           END-IF
      *
      *    パラメータ（IN）の患者番号桁数が設定された桁数を超えた場合
           IF    ( WRK-PTNUM-LENGTH    >   WRK-SYS-LENGTH1 )
               MOVE    "0211"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
      *        開始位置算出
               COMPUTE WRK-ST
                   =   WRK-SYS-LENGTH1
                   -   WRK-PTNUM-LENGTH
                   +   1
      *
               IF  ( WRK-ST                =   ZERO )
                OR ( WRK-ST                >   20   )
                   MOVE    "0211"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    検索するため、患者番号をゼロ詰編集
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    ZERO                TO  WRK-PTNUM-WK
               MOVE    WRK-PTNUM  (1:WRK-PTNUM-LENGTH)
                                           TO  WRK-PTNUM-WK (WRK-ST:)
      *        ニューメリックチェック
               IF    ( WRK-PTNUM-WK (1:WRK-PTNUM-LENGTH)
                                       IS  NUMERIC )
                   MOVE    WRK-PTNUM-WK    TO  WRK-PTNUM
               ELSE
                   MOVE    "0106"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20012-PTNUM-LENGTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    地域連携情報提供区分チェック
      *****************************************************************
       2001-AGREEMENT-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-AGREEMENT       =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-AGREEMENTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-AGREEMENT
                                       =   QSUB03-AGREEMENTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0153"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-AGREEMENT
                                       TO  SPA-Q01-AGREEMENT
               END-IF
      *
           END-IF
      *
           .
       2001-AGREEMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    氏名チェック
      *****************************************************************
       2001-NAME-CHK-SEC               SECTION.
      *
           IF    ( SPA-ENCODING    =   CONST-UTF8 )
      *
      *        全角変換処理
               MOVE    FINDREQ-NAME (IDX1)
                                   TO  WRK-INSTR
               PERFORM 801-KANACHK-UTF8-SEC
               MOVE    WRK-OUTSTR  TO  WRK-INSTR
               PERFORM 801-ZENKAKU-UTF8-SEC
               MOVE    WRK-OUTSTR  TO  SPA-Q01-NAME (IDX1)
           ELSE
      *
      *        全角変換処理
               MOVE    FINDREQ-NAME (IDX1)
                                   TO  WRK-INSTR
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-OUTSTR  TO  WRK-INSTR
               PERFORM 800-ZENKAKU-SEC
               MOVE    WRK-OUTSTR  TO  SPA-Q01-NAME (IDX1)
           END-IF
      *
      *
           .
       2001-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    住所チェック
      *****************************************************************
       2001-JYUSHO-CHK-SEC             SECTION.
      *
           MOVE    FINDREQ-JYUSHO (IDX1)
                                   TO  WRK-INSTR
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-OUTSTR      TO  WRK-INSTR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-OUTSTR      TO  SPA-Q01-JYUSHO (IDX1)
      *
           .
       2001-JYUSHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電話番号識別チェック
      *****************************************************************
       2001-TEL-TO-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-TEL-TO       =   SPACE )
               MOVE    "1"         TO  SPA-Q01-TEL-TO-HOME1
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-TEL-TOLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-TEL-TO
                                       =   QSUB03-TEL-TOL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0164"      TO  SPA-ERRCD
               ELSE
                   EVALUATE    FINDREQ-TEL-TO
                   WHEN    "1"
                       MOVE    "1"     TO  SPA-Q01-TEL-TO-HOME1
                   WHEN    "2"
                       MOVE    "1"     TO  SPA-Q01-TEL-TO-HOME2
                   WHEN    "3"
                       MOVE    "1"     TO  SPA-Q01-TEL-TO-HOME1
                                           SPA-Q01-TEL-TO-HOME2
                   WHEN    OTHER
                       MOVE    "1"     TO  SPA-Q01-TEL-TO-HOME1
                   END-EVALUATE
               END-IF
      *
           END-IF
      *
           .
       2001-TEL-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電話番号チェック
      *****************************************************************
       2001-TEL-CHK-SEC               SECTION.
      *
           MOVE    FINDREQ-TEL (IDX1)
                                   TO  WRK-INSTR
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ENTRYCHK    =   ZERO )
               MOVE    FINDREQ-TEL (IDX1)
                                   TO  SPA-Q01-TEL (IDX1)
           ELSE
               MOVE    "0165"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-TEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態１チェック
      *****************************************************************
       2001-CONDITION1-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-CONDITION1 (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-CONDITION1LST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-CONDITION1 (IDX1)
                                       =   QSUB03-CONDITION1L (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0114"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-CONDITION1 (IDX1)
                                       TO  SPA-Q01-CONDITION1 (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-CONDITION1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態２チェック
      *****************************************************************
       2001-CONDITION2-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-CONDITION2 (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-CONDITION2LST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-CONDITION2 (IDX1)
                                       =   QSUB03-CONDITION2L (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0115"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-CONDITION2 (IDX1)
                                       TO  SPA-Q01-CONDITION2 (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-CONDITION2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    状態３チェック
      *****************************************************************
       2001-CONDITION3-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-CONDITION3 (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-CONDITION3LST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-CONDITION3 (IDX1)
                                       =   QSUB03-CONDITION3L (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0116"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-CONDITION3 (IDX1)
                                       TO  SPA-Q01-CONDITION3 (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-CONDITION3-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメント識別チェック
      *****************************************************************
       2001-SELCOMMENT-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-SELCOMMENT       =   SPACE )
               MOVE    "1"         TO  SPA-Q01-SELCOMMENT
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SELCOMMENTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SELCOMMENT
                                       =   QSUB03-SELCOMMENTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0166"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SELCOMMENT
                                       TO  SPA-Q01-SELCOMMENT
               END-IF
      *
           END-IF
      *
           .
       2001-SELCOMMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメントチェック
      *****************************************************************
       2001-COMMENT-CHK-SEC            SECTION.
      *
           MOVE    FINDREQ-COMMENT (IDX1)
                                   TO  SPA-Q01-COMMENT (IDX1)
      *
           .
       2001-COMMENT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    減免事由チェック
      *****************************************************************
       2001-GENMEN-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-GENMEN (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-GENMENLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-GENMEN (IDX1)
                                       =   QSUB03-GENMENL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0123"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-GENMEN (IDX1)
                                       TO  SPA-Q01-GENMEN (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-GENMEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得識別チェック
      *****************************************************************
       2001-TSY-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-TSY  =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-TSYLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-TSY
                                       =   QSUB03-TSYL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0124"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-TSY
                                       TO  SPA-Q01-TSY
               END-IF
      *
           END-IF
      *
           .
       2001-TSY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得者有効期間識別チェック
      *****************************************************************
       2001-STTSYSKBT-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-STTSYSKBT   =   SPACE )
               MOVE    "1"         TO  SPA-Q01-STTSYSKBT
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-STTSYSKBTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-STTSYSKBT
                                       =   QSUB03-STTSYSKBTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0163"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-STTSYSKBT
                                       TO  SPA-Q01-STTSYSKBT
               END-IF
      *
           END-IF
      *
           .
       2001-STTSYSKBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得者有効月チェック
      *****************************************************************
       2001-TSYYM-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-TSYYM     =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-TSYYM TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"        TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   EVALUATE    FINDREQ-STTSYSKBT
                   WHEN    "2"
                       MOVE    "0167"  TO  SPA-ERRCD
                   WHEN    "3"
                       MOVE    "0168"  TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-Q01-STTSYYM
               END-IF
           END-IF
      *
           .
       2001-TSYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得者有効期間開始チェック
      *****************************************************************
       2001-STTSYYMD-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-STTSYYMD      =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-STTSYSKBT =   "1" )
                   MOVE    FINDREQ-STTSYYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0169"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD(1:6)
                                           TO  SPA-Q01-STTSYYM
                   END-IF
               ELSE
                   MOVE    FINDREQ-STTSYYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0171"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-Q01-STTSYYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-STTSYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    低所得者有効期間終了チェック
      *****************************************************************
       2001-EDTSYYMD-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-EDTSYYMD      =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-STTSYSKBT =   "1" )
                   MOVE    FINDREQ-EDTSYYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0170"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD(1:6)
                                           TO  SPA-Q01-EDTSYYM
                   END-IF
               ELSE
                   MOVE    FINDREQ-EDTSYYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0172"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-Q01-EDTSYYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-EDTSYYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記事項コードチェック
      *****************************************************************
       2001-TOKKICD-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-TOKKICD       =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-TOKKICD   IS  NUMERIC )
                   MOVE    FINDREQ-TOKKICD   TO  SPA-Q01-TOKKICD
               ELSE
                   MOVE    "0137"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-TOKKICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特記事項名称チェック
      *****************************************************************
       2001-TOKKINAME-CHK-SEC          SECTION.
      *
           MOVE    FINDREQ-TOKKINAME     TO  WRK-INSTR
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-OUTSTR          TO  WRK-INSTR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-OUTSTR          TO  SPA-Q01-TOKKINAME
      *
           .
       2001-TOKKINAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    下限合計点数チェック
      *****************************************************************
       2001-STTOTALTEN-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-STTOTALTEN    =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    FINDREQ-STTOTALTEN
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0173"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-Q01-STTOTALTEN
               END-IF
           END-IF
      *
           .
       2001-STTOTALTEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    上限合計点数チェック
      *****************************************************************
       2001-EDTOTALTEN-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-EDTOTALTEN    =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    FINDREQ-EDTOTALTEN
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "0174"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  SPA-Q01-EDTOTALTEN
               END-IF
           END-IF
      *
           .
       2001-EDTOTALTEN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    災害該当チェック
      *****************************************************************
       2001-SAIGAIKBN-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-SAIGAIKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SAIGAIKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SAIGAIKBN
                                       =   QSUB03-SAIGAIKBNL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0143"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SAIGAIKBN
                                       TO  SPA-Q01-SAIGAIKBN
               END-IF
      *
           END-IF
      *
           .
       2001-SAIGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点検用レセプトチェック
      *****************************************************************
       2001-TENKENRECE-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-TENKENRECE     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-TENKENRECELST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-TENKENRECE
                                       =   QSUB03-TENKENRECEL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0145"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-TENKENRECE
                                       TO  SPA-Q01-TENKENRECE
               END-IF
      *
           END-IF
      *
           .
       2001-TENKENRECE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険有効期間終了月チェック
      *****************************************************************
       2001-HKNEDYM-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-HKNEDYM     =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-HKNEDYM TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"        TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0150"  TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-Q01-HKNEDYM
               END-IF
           END-IF
      *
           .
       2001-HKNEDYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    本人家族区分チェック
      *****************************************************************
       2001-HONKZKKBN-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-HONKZKKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-HONKZKKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-HONKZKKBN
                                       =   QSUB03-HONKZKKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0113"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-HONKZKKBN
                                       TO  SPA-Q01-HONKZKKBN
               END-IF
      *
           END-IF
      *
           .
       2001-HONKZKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担割合チェック
      *****************************************************************
       2001-FTNRATE-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-FTNRATE     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-FTNRATELST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-FTNRATE
                                       =   QSUB03-FTNRATEL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0152"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-FTNRATE
                                       TO  SPA-Q01-FTNRATE
               END-IF
      *
           END-IF
      *
           .
       2001-FTNRATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号検索区分チェック
      *****************************************************************
       2001-JKNHKNJANUM-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-JKNHKNJANUM     =   SPACE )
               MOVE    "2"             TO  SPA-Q01-HKNJANUM-JKN
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-JKNHKNJANUMLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-JKNHKNJANUM
                                       =   QSUB03-JKNHKNJANUML (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0175"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-JKNHKNJANUM
                                       TO  SPA-Q01-HKNJANUM-JKN
               END-IF
      *
           END-IF
      *
           .
       2001-JKNHKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号検索区分チェック
      *****************************************************************
       2001-HKNJANUM-CHK-SEC            SECTION.
      *
           MOVE    FINDREQ-HKNJANUM (IDX1)
                                   TO  WRK-INSTR
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ENTRYCHK    =   ZERO )
               MOVE    FINDREQ-HKNJANUM (IDX1)
                                   TO  SPA-Q01-HKNJANUM (IDX1)
           ELSE
               MOVE    "0176"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類検索区分チェック
      *****************************************************************
       2001-JKNHKNNUM-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-JKNHKNNUM   =   SPACE )
               MOVE    "2"             TO  SPA-Q01-HKNNUM-JKN
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-JKNHKNNUMLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-JKNHKNNUM
                                       =   QSUB03-JKNHKNNUML (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0175"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-JKNHKNNUM
                                       TO  SPA-Q01-HKNNUM-JKN
               END-IF
      *
           END-IF
      *
           .
       2001-JKNHKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険の種類チェック
      *****************************************************************
       2001-HKNNUM-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-HKNNUM (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    FINDREQ-HKNNUM (IDX1)
                                   TO  WRK-HKNNUM
               IF    ( FINDREQ-HKNNUM (IDX1) =   "SHA" )
                   MOVE    "SYA"   TO  WRK-HKNNUM
               END-IF
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-HKNNUMLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( WRK-HKNNUM  =   QSUB03-HKNNUML (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0110"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-HKNNUM  TO  SPA-Q01-HKNNUM (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    記号チェック
      *****************************************************************
       2001-KIGO-CHK-SEC               SECTION.
      *
           MOVE    FINDREQ-KIGO (IDX1)   TO  WRK-INSTR
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-OUTSTR          TO  WRK-INSTR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-OUTSTR          TO  SPA-Q01-KIGO (IDX1)
      *
           .
       2001-KIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    番号チェック
      *****************************************************************
       2001-NUM-CHK-SEC                SECTION.
      *
           MOVE    FINDREQ-NUM (IDX1)   TO  WRK-INSTR
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-OUTSTR          TO  WRK-INSTR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-OUTSTR          TO  SPA-Q01-NUM (IDX1)
      *
           .
       2001-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費適用期間終了月チェック
      *****************************************************************
       2001-KOHEDYM-CHK-SEC          SECTION.
      *
           IF    ( FINDREQ-KOHEDYM     =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-KOHEDYM TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"        TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0151"  TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-Q01-KOHEDYM
               END-IF
           END-IF
      *
           .
       2001-KOHEDYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類検索区分チェック
      *****************************************************************
       2001-JKNKOHNUM-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-JKNKOHNUM     =   SPACE )
               MOVE    "2"             TO  SPA-Q01-KOHNUM-JKN
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-JKNKOHNUMLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-JKNKOHNUM
                                       =   QSUB03-JKNKOHNUML (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0178"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-JKNKOHNUM
                                       TO  SPA-Q01-KOHNUM-JKN
               END-IF
      *
           END-IF
      *
           .
       2001-JKNKOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費の種類チェック
      *****************************************************************
       2001-KOHNUM-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-KOHNUM (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-KOHNUMLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-KOHNUM (IDX1)
                                       =   QSUB03-KOHNUML (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0111"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-KOHNUM (IDX1)
                                       TO  SPA-Q01-KOHNUM (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-KOHNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    負担者番号チェック
      *****************************************************************
       2001-FTNJANUM-CHK-SEC           SECTION.
      *
           MOVE    FINDREQ-FTNJANUM (IDX1)
                                   TO  WRK-INSTR
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ENTRYCHK    =   ZERO )
               MOVE    FINDREQ-FTNJANUM (IDX1)
                                   TO  SPA-Q01-FTNJANUM (IDX1)
           ELSE
               MOVE    "0179"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-FTNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険公費検索区分チェック
      *****************************************************************
       2001-JKNHKNKOH-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-JKNHKNKOH   =   SPACE )
               MOVE    "1"             TO  SPA-Q01-HKNKOH-JKN
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-JKNHKNKOHLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-JKNHKNKOH
                                       =   QSUB03-JKNHKNKOHL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0180"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-JKNHKNKOH
                                       TO  SPA-Q01-HKNKOH-JKN
               END-IF
      *
           END-IF
      *
           .
       2001-JKNHKNKOH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始診療開始日チェック
      *****************************************************************
       2001-STBYOYMD-CHK-SEC          SECTION.
      *
           IF  ( FINDREQ-STBYOYMD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-STBYOYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0146"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-STBYOYMD
               END-IF
           END-IF
      *
           .
       2001-STBYOYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了診療開始日チェック
      *****************************************************************
       2001-EDBYOYMD-CHK-SEC          SECTION.
      *
           IF  ( FINDREQ-EDBYOYMD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-EDBYOYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0147"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-EDBYOYMD
               END-IF
           END-IF
      *
           .
       2001-EDBYOYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    転帰区分チェック
      *****************************************************************
       2001-TENKIKBN-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-TENKIKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-TENKIKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-TENKIKBN
                                       =   QSUB03-TENKIKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0120"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-TENKIKBN
                                       TO  SPA-Q01-TENKIKBN
               END-IF
      *
           END-IF
      *
           .
       2001-TENKIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始転帰日チェック
      *****************************************************************
       2001-STTENKIYMD-CHK-SEC          SECTION.
      *
           IF  ( FINDREQ-STTENKIYMD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-STTENKIYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0121"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-STTENKIYMD
               END-IF
           END-IF
      *
           .
       2001-STTENKIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了転帰日チェック
      *****************************************************************
       2001-EDTENKIYMD-CHK-SEC          SECTION.
      *
           IF  ( FINDREQ-EDTENKIYMD       =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-EDTENKIYMD TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0122"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  SPA-Q01-EDTENKIYMD
               END-IF
           END-IF
      *
           .
       2001-EDTENKIYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    疑い病名区分チェック
      *****************************************************************
       2001-UTAGAIKBN-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-UTAGAIKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-UTAGAIKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-UTAGAIKBN
                                       =   QSUB03-UTAGAIKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0119"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-UTAGAIKBN
                                       TO  SPA-Q01-UTAGAIKBN
               END-IF
      *
           END-IF
      *
           .
       2001-UTAGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    急性病名区分チェック
      *****************************************************************
       2001-KYUSEIKBN-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-KYUSEIKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-KYUSEIKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-KYUSEIKBN
                                       =   QSUB03-KYUSEIKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0134"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-KYUSEIKBN
                                       TO  SPA-Q01-KYUSEIKBN
               END-IF
      *
           END-IF
      *
           .
       2001-KYUSEIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    主病名区分チェック
      *****************************************************************
       2001-SYUBYOKBN-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-SYUBYOKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SYUBYOKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SYUBYOKBN
                                       =   QSUB03-SYUBYOKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0135"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SYUBYOKBN
                                       TO  SPA-Q01-SYUBYOKBN
               END-IF
      *
           END-IF
      *
           .
       2001-SYUBYOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名条件区分チェック
      *****************************************************************
       2001-JKNBYOMEI-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-JKNBYOMEI   =   SPACE )
               MOVE    "2"             TO  SPA-Q01-BYOMEI-JKN
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-JKNBYOMEILST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-JKNBYOMEI
                                       =   QSUB03-JKNBYOMEIL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0135"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-JKNBYOMEI
                                       TO  SPA-Q01-BYOMEI-JKN
               END-IF
      *
           END-IF
      *
           .
       2001-JKNBYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名チェック
      *****************************************************************
       2001-BYOMEI-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-BYOMEI (IDX1)     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    FINDREQ-BYOMEI (IDX1)
                                       TO  WRK-INSTR
               PERFORM 800-KANACHK-SEC
               MOVE    WRK-OUTSTR      TO  WRK-INSTR
               PERFORM 800-ZENKAKU-SEC
               MOVE    WRK-OUTSTR      TO  SPA-Q01-BYOMEI (IDX1)
      *
               MOVE    FINDREQ-BYOMEI (IDX1)
                                       TO  WRK-INSTR
               PERFORM 800-LENGTH-SEC
      *
               MOVE    ZERO            TO  FLG-WILD-CARD
      *
               PERFORM VARYING IDX0    FROM    1   BY  2
                       UNTIL ( IDX0    >   WRK-LEN )
                   IF    ( FINDREQ-BYOMEI (IDX1)(IDX0 : 2)
                                       =   "＊" )
                       MOVE    1       TO  FLG-WILD-CARD
                   END-IF
               END-PERFORM
      *
               IF    ( FLG-WILD-CARD   =   ZERO )
                   MOVE    "4"         TO  SPA-Q01-BYOCOMP (IDX1)
               ELSE
                   MOVE    "L"         TO  SPA-Q01-BYOCOMP (IDX1)
               END-IF
      *
           END-IF
      *
           .
       2001-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療区分チェック
      *****************************************************************
       2001-SRYKBN-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-SRYKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SRYKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SRYKBN
                                       =   QSUB03-SRYKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0129"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SRYKBN
                                       TO  SPA-Q01-SRYKBN
               END-IF
      *
           END-IF
      *
           .
       2001-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別チェック
      *****************************************************************
       2001-SRYSYUKBN-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-SRYSYUKBN     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
      *
               EVALUATE    FINDREQ-SRYKBN
               WHEN    SPACE
                   PERFORM 2001-SRYSYUKBN20-CHK-SEC
                   PERFORM 2001-SRYSYUKBN30-CHK-SEC
                   PERFORM 2001-SRYSYUKBN90-CHK-SEC
               WHEN    "20"
                   PERFORM 2001-SRYSYUKBN20-CHK-SEC
               WHEN    "30"
                   PERFORM 2001-SRYSYUKBN30-CHK-SEC
               WHEN    "90"
                   PERFORM 2001-SRYSYUKBN90-CHK-SEC
               END-EVALUATE
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0130"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SRYSYUKBN
                                       TO  SPA-Q01-SRYSYUKBN
                   IF    ( SPA-Q01-SRYKBN  =   SPACE )
                       EVALUATE    SPA-Q01-SRYSYUKBN (1:1)
                       WHEN    "2"
                           MOVE    "20"    TO  SPA-Q01-SRYKBN
                       WHEN    "3"
                           MOVE    "30"    TO  SPA-Q01-SRYKBN
                       WHEN    "9"
                           MOVE    "90"    TO  SPA-Q01-SRYKBN
                       END-EVALUATE
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       2001-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別チェック（投薬）
      *****************************************************************
       2001-SRYSYUKBN20-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    > QSUB03-SRYSYUKBN20LST-MAX )
                    OR   ( IDXA    > ZERO )
               IF    ( FINDREQ-SRYSYUKBN
                                   =   QSUB03-SRYSYUKBN20L (IDXL) )
                   MOVE    IDXL    TO  IDXA
               END-IF
           END-PERFORM
      *
           .
       2001-SRYSYUKBN20-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別チェック（注射）
      *****************************************************************
       2001-SRYSYUKBN30-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    > QSUB03-SRYSYUKBN30LST-MAX )
                    OR   ( IDXA    > ZERO )
               IF    ( FINDREQ-SRYSYUKBN
                               =   QSUB03-SRYSYUKBN30L (IDXL) )
                   MOVE    IDXL    TO  IDXA
               END-IF
           END-PERFORM
      *
           .
       2001-SRYSYUKBN30-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療種別チェック（入院料）
      *****************************************************************
       2001-SRYSYUKBN90-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDXL    FROM    1   BY  1
                   UNTIL ( IDXL    > QSUB03-SRYSYUKBN90LST-MAX )
                    OR   ( IDXA    > ZERO )
               IF    ( FINDREQ-SRYSYUKBN
                               =   QSUB03-SRYSYUKBN90L (IDXL) )
                   MOVE    IDXL    TO  IDXA
               END-IF
           END-PERFORM
      *
           .
       2001-SRYSYUKBN90-CHK-EXT.
           EXIT.
      *****************************************************************
      *    包括分チェック
      *****************************************************************
       2001-HOKATSUKBN-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-HOKATSUKBN    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-HOKATSUKBNLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-HOKATSUKBN
                                       =   QSUB03-HOKATSUKBNL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0148"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-HOKATSUKBN
                                       TO  SPA-Q01-HOKATSUKBN
               END-IF
      *
           END-IF
      *
           .
       2001-HOKATSUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    数量・回数指定区分チェック
      *****************************************************************
       2001-SRYVALITEM-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-SRYVALITEM    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SRYVALITEMLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SRYVALITEM
                                       =   QSUB03-SRYVALITEML (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0140"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SRYVALITEM
                                       TO  SPA-Q01-SRYVALITEM
               END-IF
      *
           END-IF
      *
           .
       2001-SRYVALITEM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索区分チェック
      *****************************************************************
       2001-JKNSRYACT-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-JKNSRYACT   =   SPACE )
               MOVE    "2"             TO  SPA-Q01-SRYACT-JKN
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-JKNSRYACTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-JKNSRYACT
                                       =   QSUB03-JKNSRYACTL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0202"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-JKNSRYACT
                                       TO  SPA-Q01-SRYACT-JKN
               END-IF
      *
           END-IF
      *
           .
       2001-JKNSRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為チェック
      *****************************************************************
       2001-SRYCD-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-XSRYCDKBN
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 20011-SRYCD-NOTKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 20011-SRYCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 20011-SRYVAL-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 20011-SRYVALCOMP-CHK-SEC
           END-IF
      *
           .
       2001-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為チェック（NOT検索）
      *****************************************************************
       20011-SRYCD-NOTKBN-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-SRYCD-NOTKBN (IDX1) =   SPACE OR "1" )
               MOVE    FINDREQ-SRYCD-NOTKBN (IDX1)
                                           TO  WRK-XSRYCDKBN
           ELSE
               MOVE    "0209"              TO  SPA-ERRCD
           END-IF
      *
           .
       20011-SRYCD-NOTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為チェック
      *****************************************************************
       20011-SRYCD-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-SRYCD (IDX1)  =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    FINDREQ-SRYCD (IDX1)
                                       TO  TNS-SRYCD
               PERFORM 900-TENSU-KEY24-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
                   IF    ( WRK-XSRYCDKBN   =   "1" )
                       MOVE    FINDREQ-SRYCD (IDX1)
                                       TO   SPA-Q01-XSRYCD (IDX1)
                   ELSE
                       MOVE    FINDREQ-SRYCD (IDX1)
                                       TO   SPA-Q01-SRYCD (IDX1)
                   END-IF
               ELSE
                   MOVE    "0108"      TO   SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20011-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為数値チェック
      *****************************************************************
       20011-SRYVAL-CHK-SEC             SECTION.
      *
           IF    ( FINDREQ-SRYVAL (IDX1)  =   SPACE )
            OR   ( FINDREQ-SRYCD  (IDX1)  =   SPACE )
            OR   ( FINDREQ-SRYVALITEM     =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE          ORCSNUMAREA
               MOVE    FINDREQ-SRYVAL (IDX1)
                                   TO  SNUM-INX
               CALL    "ORCSNUM"   USING
                                   ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
      *
                   MOVE    "0141"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM   TO  SPA-Q01-SRYVALNUM (IDX1)
                                             WRK-SRYSURYO
                   IF    ( FINDREQ-SRYVALITEM  =   "2" )
                       MOVE    WRK-SRYSURYO  TO  WRK-KAISUZ
                       MOVE    WRK-KAISUZ    TO  SPA-Q01-SRYVAL (IDX1)
                   ELSE
                       MOVE    WRK-SRYSURYO  TO  WRK-SRYSURYOZ
                       MOVE    WRK-SRYSURYOZ TO  SPA-Q01-SRYVAL (IDX1)
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-SRYVAL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    数量（回数又は合計）比較区分チェック
      *****************************************************************
       20011-SRYVALCOMP-CHK-SEC        SECTION.
      *
           IF    ( FINDREQ-SRYVALCOMP (IDX1) =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SRYVALCOMPLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SRYVALCOMP (IDX1)
                                       =   QSUB03-SRYVALCOMPL (IDXL) )
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0142"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-SRYVALCOMP (IDX1)
                                       TO  SPA-Q01-SRYVALCOMP (IDX1)
               END-IF
      *
           END-IF
      *
           .
       20011-SRYVALCOMP-CHK-EXT.
           EXIT.
      *****************************************************************
      *    レセプトコメントチェック
      *****************************************************************
       2001-RECECOM-CHK-SEC            SECTION.
      *
           MOVE    FINDREQ-RECECOM (IDX1)
                                       TO  WRK-INSTR
           PERFORM 800-KANACHK-SEC
           MOVE    WRK-OUTSTR          TO  WRK-INSTR
           PERFORM 800-ZENKAKU-SEC
           MOVE    WRK-OUTSTR          TO  SPA-Q01-RECECOM (IDX1)
                                           WRK-RECECOM-ALL
      *
           INSPECT WRK-RECECOM-ALL REPLACING     ALL "　" BY "  "
           UNSTRING    WRK-RECECOM-ALL
                                   DELIMITED BY  ALL " "
           INTO        WRK-RECECOM (1)
                       WRK-RECECOM (2)
                       WRK-RECECOM (3)
                       WRK-RECECOM (4)
                       WRK-RECECOM (5)
           END-UNSTRING
      *
      *    レセコメントのキーワードは３語まで指定可能とする。
           IF    (     WRK-RECECOM (1) NOT =   SPACE )
               IF    ( WRK-RECECOM (4) NOT =   SPACE )
                   MOVE    "0132"      TO  SPA-ERRCD
               END-IF
           ELSE
               IF    ( WRK-RECECOM (5) NOT =   SPACE )
                   MOVE    "0132"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-RECECOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険有効期間識別チェック
      *****************************************************************
       2001-KHSKBT-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-KHSKBT     =   SPACE )
               MOVE    "1"         TO  SPA-Q01-KHSKBT
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-KHSKBTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-KHSKBT
                                       =   QSUB03-KHSKBTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0181"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-KHSKBT
                                       TO  SPA-Q01-KHSKBT
               END-IF
      *
           END-IF
      *
           .
       2001-KHSKBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険有効月チェック
      *****************************************************************
       2001-KHYM-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-KHYM     =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-KHYM TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"        TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   EVALUATE    FINDREQ-STTSYSKBT
                   WHEN    "2"
                       MOVE    "0182"  TO  SPA-ERRCD
                   WHEN    "3"
                       MOVE    "0183"  TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-Q01-STKHYM
               END-IF
           END-IF
      *
           .
       2001-KHYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険有効期間開始チェック
      *****************************************************************
       2001-STKHYMD-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-STKHYMD      =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-KHSKBT   =   "1" )
                   MOVE    FINDREQ-STKHYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0184"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD(1:6)
                                           TO  SPA-Q01-STKHYM
                   END-IF
               ELSE
                   MOVE    FINDREQ-STKHYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0186"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-Q01-STKHYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-STKHYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険有効期間終了チェック
      *****************************************************************
       2001-EDKHYMD-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-EDKHYMD      =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-KHSKBT   =   "1" )
                   MOVE    FINDREQ-EDKHYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0185"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD(1:6)
                                           TO  SPA-Q01-EDKHYM
                   END-IF
               ELSE
                   MOVE    FINDREQ-EDKHYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0187"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-Q01-EDKHYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-EDKHYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護保険保険者番号チェック
      *****************************************************************
       2001-KHHKNJANUM-CHK-SEC         SECTION.
      *
           MOVE    FINDREQ-KHHKNJANUM
                                   TO  WRK-INSTR
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ENTRYCHK    =   ZERO )
               MOVE    FINDREQ-KHHKNJANUM
                                   TO  SPA-Q01-KHHKNJANUM
           ELSE
               MOVE    "0188"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-KHHKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護認定有効期間識別チェック
      *****************************************************************
       2001-KNSKBT-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-KNSKBT     =   SPACE )
               MOVE    "1"         TO  SPA-Q01-KNSKBT
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-KNSKBTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-KNSKBT
                                       =   QSUB03-KNSKBTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0189"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-KNSKBT
                                       TO  SPA-Q01-KNSKBT
               END-IF
      *
           END-IF
      *
           .
       2001-KNSKBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護認定有効月チェック
      *****************************************************************
       2001-KNYM-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-KNYM     =   SPACE )
               CONTINUE
           ELSE
               MOVE    FINDREQ-KNYM TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               MOVE    "01"        TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   EVALUATE    FINDREQ-STTSYSKBT
                   WHEN    "2"
                       MOVE    "0190"  TO  SPA-ERRCD
                   WHEN    "3"
                       MOVE    "0191"  TO  SPA-ERRCD
                   END-EVALUATE
               ELSE
                   MOVE    WRK-SYMD(1:6)
                                   TO  SPA-Q01-STKNYM
               END-IF
           END-IF
      *
           .
       2001-KNYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護認定有効期間開始チェック
      *****************************************************************
       2001-STKNYMD-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-STKNYMD      =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-KNSKBT =   "1" )
                   MOVE    FINDREQ-STKNYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0192"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD(1:6)
                                           TO  SPA-Q01-STKNYM
                   END-IF
               ELSE
                   MOVE    FINDREQ-STKNYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0194"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-Q01-STKNYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-STKNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護認定有効期間終了チェック
      *****************************************************************
       2001-EDKNYMD-CHK-SEC           SECTION.
      *
           IF    ( FINDREQ-EDKNYMD      =   SPACE )
               CONTINUE
           ELSE
               IF    ( FINDREQ-KNSKBT =   "1" )
                   MOVE    FINDREQ-EDKNYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0193"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD(1:6)
                                           TO  SPA-Q01-EDKNYM
                   END-IF
               ELSE
                   MOVE    FINDREQ-EDKNYMD  TO  WRK-DATE
                   PERFORM 800-SYMD-SEC
                   PERFORM 800-HIZUKE-SEC
                   IF    ( WRK-EDTYMD1 =   SPACE )
                       MOVE    "0195"      TO  SPA-ERRCD
                   ELSE
                       MOVE    WRK-SYMD    TO  SPA-Q01-EDKNYM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-EDKNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護認定要介護状態チェック
      *****************************************************************
       2001-KNJOTAI-CHK-SEC            SECTION.
      *
           IF    ( FINDREQ-KNJOTAI     =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO        TO  IDXA
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-KNJOTAILST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-KNJOTAI
                                       =   QSUB03-KNJOTAIL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXA    =    ZERO )
                   MOVE    "0161"      TO  SPA-ERRCD
               ELSE
                   MOVE    FINDREQ-KNJOTAI
                                       TO  SPA-Q01-KNJOTAI
               END-IF
      *
           END-IF
      *
           .
       2001-KNJOTAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    並べ替え項目チェック
      *****************************************************************
       2001-SORT-KEYID-CHK-SEC            SECTION.
      *
           MOVE    "0196"              TO  WRK-ERRCD1 (1)
           MOVE    "0197"              TO  WRK-ERRCD2 (1)
      *
           MOVE    "0198"              TO  WRK-ERRCD1 (2)
           MOVE    "0199"              TO  WRK-ERRCD2 (2)
      *
           MOVE    "0200"              TO  WRK-ERRCD1 (3)
           MOVE    "0201"              TO  WRK-ERRCD2 (3)
      *
           IF    ( FINDREQ-SORT-KEYID (IDX1) =   SPACE )
               MOVE    IDX1            TO  WRK-SORT-KEYID9
               MOVE    WRK-SORT-KEYID  TO  FINDREQ-SORT-KEYID (IDX1)
           END-IF
      *
           MOVE    ZERO        TO  IDXA
      *
      *
           EVALUATE    FINDREQ-OUTKBN
           WHEN    "1"
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-PTSORTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SORT-KEYID (IDX1)
                                       =   QSUB03-PTSORTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
           WHEN    "2"
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-TENSORTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SORT-KEYID (IDX1)
                                       =   QSUB03-TENSORTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
           WHEN    "3"
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   QSUB03-SRYSORTLST-MAX )
                        OR   ( IDXA    >   ZERO )
      *
                   IF    ( FINDREQ-SORT-KEYID (IDX1)
                                       =   QSUB03-SRYSORTL (IDXL))
                       MOVE    IDXL    TO  IDXA
                   END-IF
      *
               END-PERFORM
           END-EVALUATE
      *
           IF    ( IDXA    =    ZERO )
               MOVE    WRK-ERRCD1 (IDX1)   TO  SPA-ERRCD
           ELSE
               MOVE    FINDREQ-SORT-KEYID (IDX1)
                                   TO  SPA-Q01-SORTITEM-G (IDX1)
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( FINDREQ-SORT-ORDER (IDX1) =   SPACE )
                   MOVE    "1"     TO  FINDREQ-SORT-ORDER (IDX1)
               END-IF
      *
               IF    ( FINDREQ-SORT-ORDER (IDX1)
                                   =   "1" OR "2" )
                   MOVE    FINDREQ-SORT-ORDER (IDX1)
                                   TO  SPA-Q01-SORTKBN-G (IDX1)
               ELSE
                   MOVE    WRK-ERRCD2 (IDX1)   TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
       2001-SORT-PT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基本情報関連チェック
      *****************************************************************
       2002-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF      ( SPA-Q01-STSRYYMD  NOT =   SPACE )
                AND    ( SPA-Q01-EDSRYYMD  NOT =   SPACE )
                   IF  ( SPA-Q01-STSRYYMD     <=   SPA-Q01-EDSRYYMD)
                       CONTINUE
                   ELSE
                       MOVE    "1001"      TO   SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF      ( SPA-Q01-STBIRTHDAY
                                       NOT =   SPACE  )
                AND    ( SPA-Q01-EDBIRTHDAY
                                       NOT =   SPACE  )
                   IF  ( SPA-Q01-STBIRTHDAY   <=  SPA-Q01-EDBIRTHDAY )
                       CONTINUE
                   ELSE
                       MOVE    "1002"      TO   SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF      ( SPA-Q01-STLASTYMD NOT =   SPACE )
                AND    ( SPA-Q01-EDLASTYMD NOT =   SPACE )
                   IF  ( SPA-Q01-STLASTYMD    <=  SPA-Q01-EDLASTYMD )
                       CONTINUE
                   ELSE
                       MOVE    "1008"      TO   SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      ( SPA-Q01-STPTNUM   NOT =   SPACE )
            AND    ( SPA-Q01-EDPTNUM   NOT =   SPACE )
               IF  ( SPA-Q01-STPTNUM      <=   SPA-Q01-EDPTNUM )
                   CONTINUE
               ELSE
                   MOVE    "1006"          TO   SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF      ( SPA-Q01-TSY       NOT =   SPACE )
                   IF    ( SPA-Q01-STTSYYM NOT =   SPACE )
                    AND  ( SPA-Q01-EDTSYYM NOT =   SPACE )
                       IF  ( SPA-Q01-STTSYYM  <= SPA-Q01-EDTSYYM )
                           CONTINUE
                       ELSE
                           MOVE    "1010"  TO   SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF      ( SPA-Q01-STTSYSKBT       =   "2" OR "3" )
                   IF  ( SPA-Q01-STTSYYM     NOT =   SPACE )
                       CONTINUE
                   ELSE
                       IF    ( SPA-Q01-STTSYSKBT =   "2" )
                           MOVE    "1011"  TO   SPA-ERRCD
                       ELSE
                           MOVE    "1012"  TO   SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               = SPACE )
               IF    ( SPA-Q01-STTOTALTEN  =   ZERO )
                AND  ( SPA-Q01-EDTOTALTEN  =   ZERO )
                   IF    ( FINDREQ-OUTKBN      = "2" )
                       MOVE    "1037"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF   ( SPA-Q01-STSRYYMD  NOT = SPACE )
                    AND ( SPA-Q01-STSRYYMD(1:6) = SPA-Q01-EDSRYYMD(1:6))
                       CONTINUE
                   ELSE
                       MOVE    "1013"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               = SPACE )
               IF    ( SPA-Q01-STTOTALTEN  NOT =   ZERO )
                AND  ( SPA-Q01-EDTOTALTEN  NOT =   ZERO )
                AND  ( SPA-Q01-STTOTALTEN  > SPA-Q01-EDTOTALTEN )
                   MOVE    "1050"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
      D    DISPLAY "SPA-ERRCD=" SPA-ERRCD
      D    DISPLAY "SPA-Q01-STTOTALTEN = " SPA-Q01-STTOTALTEN
      D    DISPLAY "SPA-Q01-EDTOTALTEN = " SPA-Q01-EDTOTALTEN
      D    DISPLAY "FINDREQ-OUTKBN     = " FINDREQ-OUTKBN    
      D    DISPLAY "SPA-Q01-STSRYYMD   = " SPA-Q01-STSRYYMD  
      D    DISPLAY "SPA-Q01-STSRYYMD   = " SPA-Q01-STSRYYMD  
      *
           .
       2002-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険・公費関連チェック
      *****************************************************************
       2002-HOKEN-KOHI-CHK-SEC         SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-HKNEDYM NOT =   SPACE )
                   PERFORM 20021-HKNEDYM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-KOHEDYM NOT =   SPACE )
                   PERFORM 20021-KOHEDYM-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-FTNRATE NOT =   SPACE )
                   IF   ( SPA-Q01-STSRYYMD
                                       NOT =   SPACE )
                     AND ( SPA-Q01-STSRYYMD(1:6)
                                           =   SPA-Q01-EDSRYYMD(1:6))
                       CONTINUE
                   ELSE
                       MOVE    "1027"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( FLG-KIKANOVER   =   1  )
                   EVALUATE    TRUE
                   WHEN    ( SPA-Q01-HKNJANUM-JKN      =   "9" )
                    AND    ( SPA-Q01-HKNJANUM-G    NOT =   SPACE )
                   WHEN    ( SPA-Q01-HKNNUM-JKN        =   "9" )
                    AND    ( SPA-Q01-HKNNUM-G      NOT =   SPACE )
                   WHEN    ( SPA-Q01-KOHNUM-JKN        =   "9" )
                    AND    ( SPA-Q01-KOHNUM-G      NOT =   SPACE )
                       MOVE    "1020"      TO  SPA-ERRCD
                   END-EVALUATE
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF   ( FLG-KIKANOVER  =   1  )
                   EVALUATE    TRUE
                   WHEN  ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
                       MOVE    "1018"      TO  SPA-ERRCD
                   WHEN  ( SPA-Q01-BYOMEI-JKN  =   "9" )
                    AND  ( SPA-Q01-BYOMEI-G    NOT =   SPACE )
                       MOVE    "1020"      TO  SPA-ERRCD
                   WHEN  ( SPA-Q01-XSRYCD-G        NOT =   SPACE )
                       MOVE    "1020"      TO  SPA-ERRCD
                   WHEN  ( SPA-Q01-DRCD            NOT =   SPACE )
                       MOVE    "1034"      TO  SPA-ERRCD
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       2002-HOKEN-KOHI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病名・診療行為関連チェック
      *****************************************************************
       2002-BYOMEI-SRYACT-CHK-SEC      SECTION.
      *
           IF    ( SPA-Q01-STBYOYMD  NOT =   SPACE  )
            AND  ( SPA-Q01-EDBYOYMD  NOT =   SPACE  )
               IF    ( SPA-Q01-STBYOYMD <=  SPA-Q01-EDBYOYMD )
                   CONTINUE
               ELSE
                   MOVE    "1021"      TO   SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-Q01-STTENKIYMD  NOT =   SPACE  )
            AND  ( SPA-Q01-EDTENKIYMD  NOT =   SPACE  )
               IF    ( SPA-Q01-STTENKIYMD <=   SPA-Q01-EDTENKIYMD )
                   CONTINUE
               ELSE
                   MOVE    "1009"      TO   SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-Q01-STTENKIYMD  NOT =   SPACE  )
            OR   ( SPA-Q01-EDTENKIYMD  NOT =   SPACE  )
               IF    ( SPA-Q01-TENKIKBN    =   SPACE  )
                   MOVE    "1047"      TO   SPA-ERRCD
               END-IF
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-Q01-SRYCD  (IDXA)   NOT =   SPACE )
                AND  ( SPA-Q01-SRYVAL (IDXA)   NOT =   SPACE )
                   IF    ( SPA-Q01-SRYVALCOMP (IDXA)
                                                   =   SPACE )
                       MOVE    "1014"  TO   SPA-ERRCD
                   ELSE
                       IF    ( SPA-Q01-SRYVALITEM  =   SPACE )
                           MOVE    "1015"  TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( SPA-Q01-XSRYCD (IDXA)       NOT =   SPACE )
                       IF    ( SPA-Q01-SRYVAL (IDXA)   NOT =   SPACE )
                        OR   ( SPA-Q01-SRYVALCOMP (IDXA) NOT = SPACE )
                           MOVE    "1048"  TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-HOKATSUKBN  NOT =   SPACE )
                   IF  ( SPA-Q01-SRYCD-G       =   SPACE )
                       MOVE    "1022"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
                   PERFORM VARYING IDXA    FROM    1   BY  1
                           UNTIL ( IDXA    >   4 )
                            OR   ( SPA-ERRCD   NOT =   SPACE )
                       COMPUTE IDXC =  IDXA    +   1
                       PERFORM VARYING IDXB    FROM    IDXC   BY  1
                               UNTIL ( IDXB    >   5 )
                                OR   ( SPA-ERRCD   NOT =   SPACE )
                           IF    ( SPA-Q01-SRYCD  (IDXA)
                                   NOT =   SPACE )
                            AND  ( SPA-Q01-SRYCD  (IDXA)
                                       =   SPA-NAI-Q01-SRYCD (IDXB))
                               MOVE    "1016"  TO  SPA-ERRCD
                           END-IF
                       END-PERFORM
                   END-PERFORM
      *
                   IF    ( SPA-ERRCD   =   SPACE )
                       IF    ( SPA-Q01-SRYCD-G =   SPACE )
                           MOVE    "1017"  TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-Q01-OUTKBN          =   "3" )
                AND  ( SPA-Q01-SRYVALITEM      =   SPACE )
                       MOVE    "1036"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-Q01-SRYVALITEM  NOT =   SPACE )
                AND  ( SPA-Q01-OUTKBN      NOT =   "3" )
                       MOVE    "1045"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2002-BYOMEI-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    介護関連チェック
      *****************************************************************
       2002-KAIGO-CHK-SEC              SECTION.
      *
           IF      ( SPA-Q01-STKHYM    NOT =   SPACE )
            AND    ( SPA-Q01-EDKHYM    NOT =   SPACE )
               IF  ( SPA-Q01-STKHYM       <=   SPA-Q01-EDKHYM )
                   CONTINUE
               ELSE
                   MOVE    "1028"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      ( SPA-Q01-KHSKBT        =   "2" OR "3" )
               IF  ( SPA-Q01-STKHYM    NOT =   SPACE )
                   CONTINUE
               ELSE
                   IF    ( SPA-Q01-KHSKBT  =   "2" )
                       MOVE    "1029"  TO  SPA-ERRCD
                   ELSE
                       MOVE    "1030"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      ( SPA-Q01-STKNYM    NOT =   SPACE )
            AND    ( SPA-Q01-EDKNYM    NOT =   SPACE )
               IF  ( SPA-Q01-STKNYM       <=   SPA-Q01-EDKNYM )
                   CONTINUE
               ELSE
                   MOVE    "1031"      TO      SPA-ERRCD
               END-IF
           END-IF
      *
           IF      ( SPA-Q01-KNSKBT        =   "2" OR "3" )
               IF  ( SPA-Q01-STKNYM    NOT =   SPACE )
                   CONTINUE
               ELSE
                   IF    ( SPA-Q01-KNSKBT  =   "2" )
                       MOVE    "1032"  TO  SPA-ERRCD
                   ELSE
                       MOVE    "1033"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2002-KAIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険有効終了月チェック処理
      *****************************************************************
       20021-HKNEDYM-CHK-SEC     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-HKNJANUM-G  =   SPACE )
                AND  ( SPA-Q01-HKNNUM-G    =   SPACE )
                   MOVE    "1023"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-HKNJANUM-G  NOT =   SPACE )
                AND  ( SPA-Q01-HKNJANUM-JKN    =   "9" )
                   MOVE    "1024"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-HKNNUM-G    NOT  =   SPACE )
                AND  ( SPA-Q01-HKNNUM-JKN  =   "9" )
                   MOVE    "1024"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20021-HKNEDYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費有効終了月チェック処理
      *****************************************************************
       20021-KOHEDYM-CHK-SEC           SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-FTNJANUM-G  =   SPACE )
                AND  ( SPA-Q01-KOHNUM-G    =   SPACE )
                   MOVE    "1025"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-Q01-KOHNUM-G    NOT =   SPACE )
                AND  ( SPA-Q01-KOHNUM-JKN  =   "9" )
                   MOVE    "1026"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       20021-KOHEDYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-SANITIZIZE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-UNIQUE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-GAITOINF-SET-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 20031-DB-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-SHELL-SEC
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
      *    サニタイズ処理
      *****************************************************************
       20031-SANITIZIZE-SEC            SECTION.
      *
      *    診療年月日
           MOVE    SPA-Q01-STSRYYMD    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-STSRYYMD
      *
           MOVE    SPA-Q01-EDSRYYMD    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-EDSRYYMD
      *
      *    基本情報
           MOVE    SPA-Q01-STBIRTHDAY  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-STBIRTHDAY
      *
           MOVE    SPA-Q01-EDBIRTHDAY  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-EDBIRTHDAY
      *
           MOVE    SPA-Q01-BIRTHMON    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-BIRTHMON
      *
           MOVE    SPA-Q01-SEX         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-SEX
      *
           MOVE    SPA-Q01-STLASTYMD   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-STLASTYMD
      *
           MOVE    SPA-Q01-EDLASTYMD   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-EDLASTYMD
      *
           MOVE    SPA-Q01-DRCD        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-DRCD
      *
           MOVE    SPA-Q01-NYUGAIKBN   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-NYUGAIKBN
      *
           MOVE    SPA-Q01-SRYKA       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-SRYKA
      *
           MOVE    SPA-Q01-DEATH       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-DEATH
      *
           MOVE    SPA-Q01-STPTNUM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-STPTNUM
      *
           MOVE    SPA-Q01-EDPTNUM     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-EDPTNUM
      *
           MOVE    SPA-Q01-AGREEMENT   TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-AGREEMENT
      *
           MOVE    SPA-Q01-CHKTSTPT    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-CHKTSTPT
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
      *
               MOVE    SPA-Q01-JYUSHO  (IDXC)
                                       TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR     TO  SPA-Q01-JYUSHO  (IDXC)
      *
               INSPECT SPA-Q01-JYUSHO (IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
               MOVE    SPA-Q01-NAME    (IDXC)
                                       TO  WRK-NAME
               PERFORM 20031-SRHNAME-EDIT-SEC
               MOVE    WRK-NAME            TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-NAME (IDXC)
      *
           END-PERFORM
      *
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   3 )
      *
               MOVE    SPA-Q01-CONDITION1 (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-CONDITION1 (IDXC)
      *
               MOVE    SPA-Q01-CONDITION2 (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-CONDITION2 (IDXC)
      *
               MOVE    SPA-Q01-CONDITION3 (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-CONDITION3 (IDXC)
      *
               MOVE    SPA-Q01-GENMEN (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-GENMEN (IDXC)
      *
               MOVE    SPA-Q01-SELCOMMENT  TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-SELCOMMENT
      *
               MOVE    SPA-Q01-COMMENT (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-COMMENT (IDXC)
      *
               INSPECT SPA-Q01-COMMENT (IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
               MOVE    SPA-Q01-TEL (IDXC)  TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-TEL (IDXC)
      *
               INSPECT SPA-Q01-TEL (IDXC)
                       REPLACING   ALL "*" BY  "%"
      *
           END-PERFORM
      *
           MOVE    SPA-Q01-TSY             TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-TSY
      *
           MOVE    SPA-Q01-STTSYYM         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-STTSYYM
      *
           MOVE    SPA-Q01-STTSYSKBT       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-STTSYSKBT
      *
           MOVE    SPA-Q01-EDTSYYM         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-EDTSYYM
      *
           MOVE    SPA-Q01-TOKKICD         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-TOKKICD
      *
           MOVE    SPA-Q01-TOKKINAME       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-TOKKINAME
      *
      *****MOVE    SPA-Q01-STTOTALTEN      TO  SPA-Q01-STTOTALTEN
      *****MOVE    SPA-Q01-EDTOTALTEN      TO  SPA-Q01-EDTOTALTEN
      *
           MOVE    SPA-Q01-SAIGAIKBN       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-SAIGAIKBN
      *
           MOVE    SPA-Q01-TENKENRECE      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-TENKENRECE
      *
      *    保険　公費
           MOVE    SPA-Q01-HKNJANUM-JKN    TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-HKNJANUM-JKN
      *
           MOVE    SPA-Q01-HKNNUM-JKN      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-HKNNUM-JKN
      *
           MOVE    SPA-Q01-KOHNUM-JKN      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-KOHNUM-JKN
      *
           MOVE    SPA-Q01-HKNKOH-JKN      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-HKNKOH-JKN
      *
           MOVE    SPA-Q01-HKNEDYM TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-HKNEDYM
      *
           MOVE    SPA-Q01-HONKZKKBN       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-HONKZKKBN
      *
           MOVE    SPA-Q01-FTNRATE         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-FTNRATE
      *
           MOVE    SPA-Q01-KOHEDYM         TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-KOHEDYM
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
      *
               MOVE    SPA-Q01-HKNJANUM(IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-HKNJANUM(IDXC)
      *
               MOVE    SPA-Q01-HKNNUM  (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-HKNNUM (IDXC)
      *
               MOVE    SPA-Q01-KIGO (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-KIGO (IDXC)
      *
               MOVE    SPA-Q01-NUM (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-NUM (IDXC)
      *
               MOVE    SPA-Q01-KOHNUM (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-KOHNUM (IDXC)
      *
               MOVE    SPA-Q01-FTNJANUM (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-FTNJANUM (IDXC)
      *
           END-PERFORM
      *
      *    病名　診療行為
           MOVE    SPA-Q01-STBYOYMD        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-STBYOYMD
      *
           MOVE    SPA-Q01-EDBYOYMD        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-EDBYOYMD
      *
           MOVE    SPA-Q01-TENKIKBN        TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-TENKIKBN
      *
           MOVE    SPA-Q01-STTENKIYMD      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-STTENKIYMD
      *
           MOVE    SPA-Q01-EDTENKIYMD      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-EDTENKIYMD
      *
           MOVE    SPA-Q01-UTAGAIKBN       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-UTAGAIKBN
      *
           MOVE    SPA-Q01-KYUSEIKBN       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-KYUSEIKBN
      *
           MOVE    SPA-Q01-SYUBYOKBN       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-SYUBYOKBN
      *
           MOVE    SPA-Q01-BYOMEI-JKN      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-BYOMEI-JKN
      *
           MOVE    SPA-Q01-SRYKBN          TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-SRYKBN
      *
           MOVE    SPA-Q01-SRYSYUKBN       TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-SRYSYUKBN
      *
           MOVE    SPA-Q01-SRYVALITEM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-SRYVALITEM
      *
           MOVE    SPA-Q01-HOKATSUKBN      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR             TO  SPA-Q01-HOKATSUKBN
      *
      *    MOVE    SPA-Q01-SRYACT-JKN      TO  SPA-Q01-SRYACT-JKN
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   5 )
               MOVE    SPA-Q01-BYOMEI(IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-BYOMEI(IDXC)
      *
               INSPECT SPA-Q01-BYOMEI(IDXC)
                       REPLACING   ALL "*" BY  "%"
      *
               INSPECT SPA-Q01-BYOMEI(IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
               MOVE    SPA-Q01-BYOCOMP (IDXC)
                                     TO  SPA-Q01-BYOCOMP (IDXC)
      *
               MOVE    SPA-Q01-SRYCD (IDXC)
                                     TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-SRYCD (IDXC)
      *
               MOVE    SPA-Q01-XSRYCD (IDXC)
                                     TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR   TO  SPA-Q01-XSRYCD (IDXC)
      *
               IF    ( SPA-Q01-SRYCD (IDXC)      NOT =   SPACE )
                AND  ( SPA-Q01-SRYVAL(IDXC)      NOT =   SPACE )
                AND  ( SPA-Q01-SRYVALCOMP (IDXC)
                                                 NOT =   SPACE )
                   MOVE    SPA-Q01-SRYVAL (IDXC)
                                           TO  PTSRH-DBSTR
                   PERFORM 900-PTSRH-DBESCAPE-SEC
                   MOVE    PTSRH-DBSTR   TO  SPA-Q01-SRYVAL (IDXC)
      *
      *************MOVE    SPA-Q01-SRYVALNUM (IDXC)
      *************                      TO  SPA-Q01-SRYVALNUM (IDXC)
      *
                   MOVE    SPA-Q01-SRYVALCOMP (IDXC)
                                           TO  PTSRH-DBSTR
                   PERFORM 900-PTSRH-DBESCAPE-SEC
                   MOVE    PTSRH-DBSTR     TO  SPA-Q01-SRYVALCOMP (IDXC)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   3 )
      *
               MOVE    SPA-Q01-RECECOM (IDXC)
                                           TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR         TO  SPA-Q01-RECECOM (IDXC)
      *
               INSPECT SPA-Q01-RECECOM (IDXC)
                       REPLACING   ALL "＊" BY  "%%"
      *
           END-PERFORM
      *
      *    介護
           MOVE    SPA-Q01-KHSKBT      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-KHSKBT
      *
           MOVE    SPA-Q01-STKHYM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-STKHYM
      *
           MOVE    SPA-Q01-EDKHYM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-EDKHYM
      *
           MOVE    SPA-Q01-KHHKNJANUM  TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-KHHKNJANUM
      *
           MOVE    SPA-Q01-KNSKBT      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-KNSKBT
      *
           MOVE    SPA-Q01-STKNYM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-STKNYM
      *
           MOVE    SPA-Q01-EDKNYM      TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-EDKNYM
      *
           MOVE    SPA-Q01-KNJOTAI     TO  PTSRH-DBSTR
           PERFORM 900-PTSRH-DBESCAPE-SEC
           MOVE    PTSRH-DBSTR         TO  SPA-Q01-KNJOTAI
      *
      *    並び順
           PERFORM VARYING IDXC    FROM    1   BY  1
                   UNTIL ( IDXC    >   3 )
      *
               MOVE    SPA-Q01-SORTITEM-G (IDXC)
                                         TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR       TO  SPA-Q01-SORTITEM-G (IDXC)
      *
               MOVE    SPA-Q01-SORTKBN-G (IDXC)
                                         TO  PTSRH-DBSTR
               PERFORM 900-PTSRH-DBESCAPE-SEC
               MOVE    PTSRH-DBSTR       TO  SPA-Q01-SORTKBN-G (IDXC)
      *
           END-PERFORM
      *
           .
       20031-SANITIZIZE-EXT.
           EXIT.
      *****************************************************************
      *    検索処理編集処理
      *****************************************************************
       20031-UNIQUE-SEC                 SECTION.
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   4 )
      *
               COMPUTE IDXC    =   IDXA    +   1
               PERFORM VARYING IDXB    FROM    IDXC   BY  1
                       UNTIL ( IDXB    >   5 )
      *
      *            住所
                   IF    ( SPA-Q01-JYUSHO (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-JYUSHO (IDXA)
                                       =   SPA-Q01-JYUSHO (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-JYUSHO (IDXB)
                       END-IF
                   END-IF
      *
      *            漢字氏名
                   IF    ( SPA-Q01-NAME (IDXA)     NOT =   SPACE )
                       IF    ( SPA-Q01-NAME (IDXA)
                                       =   SPA-Q01-NAME (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-NAME (IDXB)
                       END-IF
                   END-IF
      *
      *            保険者番号
                   IF    ( SPA-Q01-HKNJANUM (IDXA) NOT =   SPACE )
                       IF    ( SPA-Q01-HKNJANUM (IDXA)
                                       =   SPA-Q01-HKNJANUM (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-HKNJANUM (IDXB)
                       END-IF
                   END-IF
      *
      *            保険の種類
                   IF    ( SPA-Q01-HKNNUM (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-HKNNUM (IDXA)
                                       =   SPA-Q01-HKNNUM (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-HKNNUM (IDXB)
                       END-IF
                   END-IF
      *
      *            記号・番号
                   IF    ( SPA-Q01-KIGO (IDXA)     NOT =   SPACE )
                     OR  ( SPA-Q01-NUM  (IDXA)     NOT =   SPACE )
                       IF    ( SPA-Q01-KIGO (IDXA)
                                       =   SPA-Q01-KIGO (IDXB))
                         AND ( SPA-Q01-NUM  (IDXA)
                                       =   SPA-Q01-NUM  (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-KIGO (IDXB)
                           MOVE    SPACE   TO  SPA-Q01-NUM  (IDXB)
                       END-IF
                   END-IF
      *
      *            公費の種類・支払い区分
                   IF    ( SPA-Q01-KOHNUM (IDXA)   NOT =   SPACE )
                     OR  ( SPA-Q01-PAYKBN (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-KOHNUM (IDXA)
                                       =   SPA-Q01-KOHNUM (IDXB))
                         AND ( SPA-Q01-PAYKBN (IDXA)
                                       =   SPA-Q01-PAYKBN (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-KOHNUM (IDXB)
                           MOVE    SPACE   TO  SPA-Q01-PAYKBN (IDXB)
                       END-IF
                   END-IF
      *
      *            負担者番号
                   IF    ( SPA-Q01-FTNJANUM (IDXA) NOT =   SPACE )
                       IF    ( SPA-Q01-FTNJANUM (IDXA)
                                       =   SPA-Q01-FTNJANUM (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-FTNJANUM (IDXB)
                       END-IF
                   END-IF
      *
      *            病名
                   IF    ( SPA-Q01-BYOMEI (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-BYOMEI (IDXA)
                                       =   SPA-Q01-BYOMEI (IDXB))
                        AND  ( SPA-Q01-BYOCOMP (IDXA)
                                       =   SPA-Q01-BYOCOMP(IDXB))
                           MOVE    SPACE   TO  SPA-Q01-BYOMEI (IDXB)
                                               SPA-Q01-BYOCOMP(IDXB)
                       END-IF
                   END-IF
      *
      *            診療行為コード
                   IF    ( SPA-Q01-SRYCD (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-SRYCD (IDXA)
                                       =   SPA-Q01-SRYCD (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-SRYCD (IDXB)
                           MOVE    SPACE   TO  SPA-Q01-SRYVAL (IDXB)
                           MOVE    ZERO    TO  SPA-Q01-SRYVALNUM (IDXB)
                           MOVE    SPACE   TO  SPA-Q01-SRYVALCOMP
                                                              (IDXB)
                       END-IF
                   END-IF
      *
      *            診療行為コード
                   IF    ( SPA-Q01-XSRYCD (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-XSRYCD (IDXA)
                                       =   SPA-Q01-XSRYCD (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-XSRYCD (IDXB)
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
           END-PERFORM
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   2 )
      *
               COMPUTE IDXC    =   IDXA    +   1
               PERFORM VARYING IDXB    FROM   IDXC BY  1
                       UNTIL ( IDXB    >   3 )
      *
      *            状態１
                   IF    ( SPA-Q01-CONDITION1 (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-CONDITION1 (IDXA)
                                       =   SPA-Q01-CONDITION1 (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-CONDITION1 (IDXB)
                       END-IF
                   END-IF
      *
      *            状態２
                   IF    ( SPA-Q01-CONDITION2 (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-CONDITION2 (IDXA)
                                       =   SPA-Q01-CONDITION2 (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-CONDITION2 (IDXB)
                       END-IF
                   END-IF
      *
      *            状態３
                   IF    ( SPA-Q01-CONDITION3 (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-CONDITION3 (IDXA)
                                       =   SPA-Q01-CONDITION3 (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-CONDITION3 (IDXB)
                       END-IF
                   END-IF
      *
      *            減免事由
                   IF    ( SPA-Q01-GENMEN (IDXA)   NOT =   SPACE )
                       IF    ( SPA-Q01-GENMEN (IDXA)
                                       =   SPA-Q01-GENMEN (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-GENMEN (IDXB)
                       END-IF
                   END-IF
      *
      *            コメント
                   IF    ( SPA-Q01-COMMENT (IDXA)  NOT =   SPACE )
                       IF    ( SPA-Q01-COMMENT (IDXA)
                                       =   SPA-Q01-COMMENT (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-COMMENT (IDXB)
                       END-IF
                   END-IF
      *
      *            レセプトコメント
                   IF    ( SPA-Q01-RECECOM (IDXA)  NOT =   SPACE )
                       IF    ( SPA-Q01-RECECOM (IDXA)
                                       =   SPA-Q01-RECECOM (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-RECECOM (IDXB)
                       END-IF
                   END-IF
      *
      *            電話番号
                   IF    ( SPA-Q01-TEL (IDXA)  NOT =   SPACE )
                       IF    ( SPA-Q01-TEL (IDXA)
                                       =   SPA-Q01-TEL (IDXB))
                           MOVE    SPACE   TO  SPA-Q01-TEL (IDXB)
                       END-IF
                   END-IF
      *
               END-PERFORM
           END-PERFORM
      *
      *
           .
       20031-UNIQUE-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索条件編集処理
      *****************************************************************
       20031-SRHNAME-EDIT-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME-G
      *
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊　"              TO  ORCSSTR-STRING2
           MOVE    "＊"                TO  ORCSSTR-STRING3
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊"                TO  ORCSSTR-STRING2
           CALL    "orcsstring" USING  ORCSSTRINGAREA
           MOVE    ORCSSTR-NUM1        TO  WRK-ORCSSTR-NUM1
           IF    ( ORCSSTR-NUM1        >   ZERO )
               IF    ( ORCSSTR-NUM1    =   1 )
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM1    =   2
                   COMPUTE ORCSSTR-NUM2    =   0
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME2
               ELSE
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM2    =   ORCSSTR-NUM1    -   1
                   COMPUTE ORCSSTR-NUM1    =   1
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME1
                   MOVE    "substr"    TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME    TO  ORCSSTR-STRING1
                   COMPUTE ORCSSTR-NUM1    =   WRK-ORCSSTR-NUM1
                                           +   1
                   COMPUTE ORCSSTR-NUM2    =   0
                   CALL    "orcsstring" USING  ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2
                                       TO  WRK-NAME2
               END-IF
           ELSE
               MOVE    WRK-NAME        TO  WRK-NAME1
           END-IF
      *
           MOVE    SPACE           TO  WRK-NAME
           IF    ( WRK-NAME1       NOT =   SPACE )
            AND  ( WRK-NAME2           =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
           IF    ( WRK-NAME1       NOT =   SPACE )
            AND  ( WRK-NAME2       NOT =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
           IF    ( WRK-NAME1           =   SPACE )
            AND  ( WRK-NAME2       NOT =   SPACE )
               STRING  "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
               INTO    WRK-NAME
               END-STRING
           END-IF
      *
           .
       20031-SRHNAME-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    検索該当項目情報設定処理
      *****************************************************************
       20031-GAITOINF-SET-SEC           SECTION.
      *
           MOVE    SPACE                   TO  SPA-Q01-GAITO-TBL
      *
      *    該当（１）
           IF  ( SPA-Q01-STPTNUM           NOT =   SPACE )
            OR ( SPA-Q01-EDPTNUM           NOT =   SPACE )
               MOVE    "1"                 TO  SPA-Q01-GPTNUM
      *
           END-IF
      *
      *    該当（２）
           IF  ( SPA-Q01-NAME-G            NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GNAME
      *
           END-IF
      *
      *    該当（３）
           IF  ( SPA-Q01-STLASTYMD         NOT =   SPACE )
            OR ( SPA-Q01-EDLASTYMD         NOT =   SPACE )
               MOVE    "1"                 TO  SPA-Q01-GLASTYMD
           END-IF
      *
      *    該当（４）
           IF  ( SPA-Q01-UTAGAIKBN         NOT =   SPACE )
            OR ( SPA-Q01-KYUSEIKBN         NOT =   SPACE )
            OR ( SPA-Q01-TENKIKBN          NOT =   SPACE )
            OR ( SPA-Q01-SYUBYOKBN         NOT =   SPACE )
            OR ( SPA-Q01-STBYOYMD          NOT =   SPACE )
            OR ( SPA-Q01-EDBYOYMD          NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GBYOMEI
      *
           END-IF
      *
      *    該当（４，１３）
           IF  ( SPA-Q01-BYOMEI-G          NOT =   SPACE )
               IF    ( SPA-Q01-BYOMEI-JKN  NOT =   "9" )
                   MOVE    "1"     TO  SPA-Q01-GBYOMEI
               ELSE
                   MOVE    "1"     TO  SPA-Q01-GXBYOMEI
               END-IF
           END-IF
      *
      *    該当（５）
           IF  ( SPA-Q01-HONKZKKBN         NOT =   SPACE )
            OR ( SPA-Q01-FTNRATE           NOT =   SPACE )
            OR ( SPA-Q01-HKNJANUM-G        NOT =   SPACE )
            OR ( SPA-Q01-HKNNUM-G          NOT =   SPACE )
            OR ( SPA-Q01-KIGO-G            NOT =   SPACE )
            OR ( SPA-Q01-NUM-G             NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GHKNINF
      *
           END-IF
      *
      *    該当（６）
           IF  ( SPA-Q01-KOHNUM-G          NOT =   SPACE )
            OR ( SPA-Q01-FTNJANUM-G        NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GKOHINF
      *
           END-IF
      *
      *    該当（７）
           IF  ( SPA-Q01-JYUSHO-G          NOT =   SPACE )
            OR ( SPA-Q01-CONDITION1-G      NOT =   SPACE )
            OR ( SPA-Q01-CONDITION2-G      NOT =   SPACE )
            OR ( SPA-Q01-CONDITION3-G      NOT =   SPACE )
            OR ( SPA-Q01-GENMEN-G          NOT =   SPACE )
            OR ( SPA-Q01-COMMENT-G         NOT =   SPACE )
            OR ( SPA-Q01-STBIRTHDAY        NOT =   SPACE )
            OR ( SPA-Q01-EDBIRTHDAY        NOT =   SPACE )
            OR ( SPA-Q01-BIRTHMON          NOT =   "00" )
            OR ( SPA-Q01-SEX               NOT =   SPACE )
            OR ( SPA-Q01-TEL-G             NOT =   SPACE )
            OR ( SPA-Q01-DEATH             NOT =   SPACE )
            OR ( SPA-Q01-CHKTSTPT              =   "1"  )
      *
               MOVE    "1"                 TO  SPA-Q01-GPTINF
      *
           END-IF
      *
      *    該当（８）
           IF  ( SPA-Q01-STSRYYMD          NOT =   SPACE  )
            OR ( SPA-Q01-EDSRYYMD          NOT =   SPACE  )
            OR ( SPA-Q01-DRCD                NOT =   SPACE )
            OR ( SPA-Q01-NYUGAIKBN         NOT =   SPACE )
            OR ( SPA-Q01-SRYKA             NOT =   SPACE )
      *
               MOVE    "1"                 TO  SPA-Q01-GJYURRK
      *
           END-IF
      *
      *    該当（９）
           IF  ( SPA-Q01-SRYKBN            NOT =   SPACE )
            OR ( SPA-Q01-SRYCD-G           NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GSRYACT
      *
           END-IF
      *
      *    該当（１０）
           IF  ( SPA-Q01-TSY           NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GTSY
      *
           END-IF
      *
      *    該当（１１）
           IF   (  SPA-Q01-RECECOM-G       NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GRECECOM
      *
           END-IF
      *
      *    該当（１２）
           IF  ( SPA-Q01-TOKKI-G          NOT =   SPACE )
            OR ( SPA-Q01-STTOTALTEN       NOT =   ZERO  )
            OR ( SPA-Q01-EDTOTALTEN       NOT =   ZERO  )
            OR ( SPA-Q01-SAIGAIKBN        NOT =   SPACE )
            OR ( SPA-Q01-TENKENRECE       NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GSEIKYU
      *
           END-IF
      *
      *    該当（１４）
           IF  ( SPA-Q01-XSRYCD-G          NOT =   SPACE )
      *
              MOVE     "1"                 TO  SPA-Q01-GXSRYACT
      *
           END-IF
      *
      *    該当（１５）
           IF  ( SPA-Q01-AGREEMENT         NOT =   SPACE )
              MOVE     "1"                 TO  SPA-Q01-GPTNUM-PUBLIC
           END-IF
      *
      *    該当（１６）
           IF  ( SPA-Q01-KAIGO-HOKEN-G     NOT =   SPACE )
            OR ( SPA-Q01-KAIGO-NINTEI-G    NOT =   SPACE )
              MOVE     "1"                 TO  SPA-Q01-GKAIGO
           END-IF
      *
      *    検索条件が未入力のとき
           IF  ( SPA-Q01-GAITO-TBL         =   SPACE )
               MOVE    "1007"              TO  SPA-ERRCD
           END-IF
      *
           .
       20031-GAITOINF-SET-EXT.
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
      *    シェル実行処理
      *****************************************************************
       2003-SHELL-SEC                  SECTION.
      *
          INITIALIZE                   STEMPDIR-AREA
          CALL     "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
          MOVE     STEMPDIR-TEMPDIR    TO  MCP-TEMPDIR
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        ＳＱＬ文編集処理
               MOVE    SPA-TERMID      TO  WRK-TERMID
               MOVE    RES-ORCA-UID    TO  SPA-TERMID
               INITIALIZE                  QSUB02-LNK
               MOVE    SMCNDATE-YMD    TO  QSUB02-YMD
               MOVE    SMCNDATE-HMS    TO  QSUB02-HMS
               CALL    "ORCGQSUB02"    USING
                                       SPA-AREA
                                       SPA-Q01FREE
                                       QSUB02-LNK
               MOVE     WRK-TERMID     TO   SPA-TERMID
               MOVE     QSUB02-SQL     TO   WRK-INSTR
               PERFORM  800-LENGTH-SEC
               IF  ( WRK-LEN           >    15000 )
      *
      *            ＳＱＬ解析限界値オーバー
                   MOVE    "1039"      TO  SPA-ERRCD
               ELSE
      *
                   MOVE     SPACE      TO      PRFNAME-AREA
                                               PUSHNAME-AREA
                   INITIALIZE                  PRFNAME-AREA
                                               PUSHNAME-AREA
      *
                   MOVE    SPA-HOSPNUM     TO  PRFNAME-HOSPNUM
                   MOVE    "API01301"      TO  PRFNAME-FILE-ID
                   MOVE    2               TO  PRFNAME-SYOKBN1
                   MOVE    SPA-TERMID      TO  PRFNAME-TERMID
      *
                   MOVE    SPA-HOSPNUM     TO  PUSHNAME-HOSPNUM
                   MOVE    "API01302"      TO  PUSHNAME-FILE-ID
                   MOVE    2               TO  PUSHNAME-SYOKBN1
                   MOVE    SPA-TERMID      TO  PUSHNAME-TERMID
      *
                   INITIALIZE                  SGETTEMP-AREA
                   MOVE    PRFNAME-BASENAME TO SGETTEMP-BASENAMES(1)
                   MOVE    PUSHNAME-BASENAME TO SGETTEMP-BASENAMES(2)
                   CALL    "ORCSGETTEMP"       USING
                                               SGETTEMP-AREA
                   MOVE    SGETTEMP-FULLNAMES (1)
                                           TO  PRFNAME
                   MOVE    SGETTEMP-FULLNAMES (2)
                                           TO  PUSHNAME
                   MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
      *
                   OPEN    OUTPUT              BQ01-FILE
                   INITIALIZE                  BQ01
                   MOVE    QSUB02-SQL      TO  BQ01-SQL
                   MOVE    QSUB02-SQL-ORDER TO BQ01-SQL-ORDER
                   MOVE    SPA-Q01KYOUTU   TO  BQ01-Q01KYOUTU
                   WRITE   BQ01
                   CLOSE   BQ01-FILE
      *
                   MOVE    SPACE           TO  SHELLTBL
                   INITIALIZE                  SHELLTBL
      *
                   MOVE    SPA-HOSPNUM     TO  SHELLTBL-HOSPNUM
                   MOVE    "ORCBQ03.sh"    TO  SHELLTBL-NAME
                   MOVE    "ORCBQ03"       TO  SHELLTBL-SHELLID
                   MOVE    1               TO  SHELLTBL-JOBID
                   MOVE    RES-ORCA-UID    TO  SHELLTBL-TERMID
      *
                   MOVE    RES-ORCA-UID    TO  SHELLTBL-ARG1
                   MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
                   MOVE    PRFNAME (ASGNPARA-ST:)
                                           TO  SHELLTBL-ARG3
                   MOVE    SMCNDATE-YMD    TO  SHELLTBL-ARG4
                   MOVE    SMCNDATE-HMS    TO  SHELLTBL-ARG5
                   MOVE    SPA-OPID        TO  SHELLTBL-ARG6
                   MOVE    FINDREQ-KARTE-UID TO  SHELLTBL-ARG7
                   MOVE    PUSHNAME (ASGNPARA-ST:)
                                           TO  SHELLTBL-ARG8
      *
                   INITIALIZE                  ORCA-BLOB
                   MOVE    PRFNAME         TO  ORCA-BLOB-FILE
                   PERFORM 910-BLOBIMPORT-SEC
                   MOVE    ORCA-BLOB-OBJECT
                                           TO  SHELLTBL-ARG9
      *
                   MOVE    SHELLTBL        TO  MCPDATA-REC
                   MOVE    "SHELL"         TO  MCP-FUNC
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    "allways"       TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
                   DISPLAY "ORCBQ03.sh called"
               END-IF
           END-IF
      *
           .
       2003-SHELL-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-SEC                  SECTION.
      *
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       2004-STATE-UPD-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               INITIALIZE              PARA013-REC
               MOVE    "01"        TO  PARA013-STATE
               PERFORM 20041-STATE-UPD-SEC
           ELSE
               IF    ( RES-ORCA-UID    =   SPACE )
                   CONTINUE
               ELSE
                   INITIALIZE              PARA013-REC
                   MOVE    "00"        TO  PARA013-STATE
                   PERFORM 20041-STATE-UPD-SEC
               END-IF
           END-IF
      *
           .
       2004-STATE-UPD-EXT.
           EXIT.
      *****************************************************************
      *    状態更新処理
      *****************************************************************
       20041-STATE-UPD-SEC             SECTION.
      *
           INITIALIZE                  APIPARA-REC
           MOVE    SPA-HOSPNUM     TO  APIPARA-HOSPNUM
           MOVE    "api013"        TO  APIPARA-GYOUMUID
           MOVE    RES-ORCA-UID    TO  APIPARA-TERMID
           MOVE    SPA-OPID        TO  APIPARA-FILEMEI
           MOVE    1               TO  APIPARA-EDANUM
           MOVE    FINDREQ-KARTE-UID TO  APIPARA-KARTE-UID
           MOVE    PARA013-REC     TO  APIPARA-DATA-REC
           MOVE    SMCNDATE-YMD    TO  APIPARA-UPYMD
           MOVE    SMCNDATE-HMS    TO  APIPARA-UPHMS
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "del4"          TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    "4006"  TO  SPA-ERRCD
                   MOVE    SPACE   TO  RES-ORCA-UID
               END-IF
           END-IF
      *
           MOVE    APIPARA-REC     TO  MCPDATA-REC
           MOVE    "tbl_api_para"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    "4007"  TO  SPA-ERRCD
                   MOVE    SPACE   TO  RES-ORCA-UID
               END-IF
           END-IF
      *
           .
       20041-STATE-UPD-EXT.
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
      *    リスト項目取得処理
      *****************************************************************
       800-ORCGQSUB03-SEC              SECTION.
      *
           INITIALIZE                      QSUB03-LNK
           MOVE    SPACE               TO  QSUB03-REQUEST
           MOVE    "99999999"          TO  QSUB03-STSRYYMD
           MOVE    SPA-SYSYMD          TO  QSUB03-EDSRYYMD
      *
           IF    ( SPA-Q01-STSRYYMD    NOT =   SPACE )
               MOVE    SPA-Q01-STSRYYMD
                                       TO  QSUB03-STSRYYMD
           END-IF
      *
           IF    ( SPA-Q01-EDSRYYMD    NOT =   SPACE )
               MOVE    SPA-Q01-EDSRYYMD
                                       TO  QSUB03-EDSRYYMD
           END-IF
      *
           IF    ( QSUB03-EDSRYYMD         <   QSUB03-STSRYYMD )
               MOVE    QSUB03-EDSRYYMD     TO  WRK-YMD
               MOVE    -11                 TO  WRK-ZOGEN
               MOVE    "2"                 TO  WRK-ZOGENPTN
               PERFORM 800-CALENDAR-SEC
               MOVE    WRK-KEISANYMD       TO  QSUB03-STSRYYMD
               MOVE    "01"                TO  QSUB03-STSRYYMD(7:2)
           END-IF
      *
           MOVE    REQ-BASEYMD             TO  QSUB03-BASEYMD
           CALL    "ORCGQSUB03"    USING   QSUB03-LNK
                                           SPA-AREA
      *
           .
      *
       800-ORCGQSUB03-EXT.
           EXIT.
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
           CALL    "ORCGQERRMSG"   USING
                                   SPA-AREA
                                   SPA-Q01FREE
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
           MOVE    FINDREQ-REQNUM            TO  FINDRES-REQNUM
           IF    ( SPA-ERRCD       =   ZERO  OR  SPACE )
               MOVE    "02"                  TO  FINDRES-RESNUM
           ELSE
               MOVE    "01"                  TO  FINDRES-RESNUM
           END-IF
      *
           MOVE    FINDREQ-KARTE-UID         TO  FINDRES-KARTE-UID
           MOVE    RES-ORCA-UID              TO  FINDRES-ORCA-UID
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  FINDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  FINDRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  FINDRES-API-RESULT
           ELSE
               MOVE    "E"         TO  FINDRES-API-RESULT
               MOVE    SPA-ERRCD   TO  FINDRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-Q01-ERRMSG  TO  FINDRES-API-RESULT-MSG
      *
           MOVE    WRK-TIME        TO  FINDRES-INFORMATION-TIME
      *
           MOVE    "xml_findv3res"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  FINDRES-MODE
           MOVE    "findv3res"         TO  FINDRES-RECORDNAME
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-FINDV3RES
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    FINDRES-OBJECT  TO  QAPI01-BODY
               MOVE    "application/xml"   TO  QAPI01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY26-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key26"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY26-SEL-EXT.
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
           MOVE    "api013"            TO  APIPARA-GYOUMUID
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
      *    検索文字エスケープ処理
      *****************************************************************
       900-PTSRH-DBESCAPE-SEC          SECTION.
      *
           MOVE    PTSRH-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "DBESCAPE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           MOVE    MCPDATA-REC     TO  PTSRH-REC
      *
           .
      *
       900-PTSRH-DBESCAPE-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込処理
      *****************************************************************
       900-TENSU-KEY24-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-TENSU-KEY24-SEL-EXT.
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
      *    BLOBIMPORT処理
      *****************************************************************
       910-BLOBIMPORT-SEC              SECTION.
      *
           MOVE    ORCA-BLOB       TO  MCPDATA-REC
      *
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    "BLOBIMPORT"    TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           MOVE    MCPDATA-REC     TO  ORCA-BLOB
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       ORCA-BLOB-FILE
      *
           .
       910-BLOBIMPORT-EXT.
           EXIT.
