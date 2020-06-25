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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGK023.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為
      *  コンポーネント名  : 入力コード設定      （Ｋ０２３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/09/20  入力インターフェースの変更
      *  01.00.02    NACL-多々納  05/11/17  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  05.00.00    NACL-多々納  18/10/XX  登録済みエラ―表示対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(1500).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "K02SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PARA            PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-K023            PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
      *
           03  FLG-SYURYOU         PIC 9(01).
           03  FLG-MAX             PIC 9(01).
      *H28.11
           03  FLG-CHKERR          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-2               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXC                PIC 9(04).
      *
           03  ID1                 PIC 9(04).
           03  ID2                 PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDK                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-IDX2                PIC 9(02).
           03  WRK-WIDGET              PIC X(20).
      *
           03  WRK-INPUTCD         PIC X(20).
           03  WRK-CDSYU           PIC X(01).
           03  WRK-CNT             PIC 9(04).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-KIDMSG          PIC X(80).
      *
           03  WRK-DSPSEQ          PIC 9(04).
      *
           03  WRK-PAGE            PIC 9(04).
      *
           03  WRK-SP-FLG          PIC 9(01).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *
       01  WRK-INPUTCD-TBL.
           03  WRK-INPUTCD-OCC     OCCURS   20.
               05  WRK-K023-INPUTCD    PIC X(20).
               05  WRK-K023-CDSYU      PIC X(01).
      *
      *テーブルＭＡＸ
       01  MAX-LINE                PIC 9(04)   VALUE   300.
      *
       01  MAX-GMN                 PIC 9(04)   VALUE   20.
       01  MAX-PAGE                PIC 9(04)   VALUE   15.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *     パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    SPACE           TO  SPA-ERRMSG2
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
                                       OR  "KERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                    PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               PERFORM 5001-MAPCUR-SEC
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "K023"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   MOVE    SPACE               TO  SPA-KIDCD
      *
               WHEN    OTHER
      *            初期画面
                   PERFORM 300-INIT-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-INIT-SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           INITIALIZE                  SPA-K023GMN-AREA
           MOVE    1                   TO  FLG-SYORI
      *
           PERFORM                     310-SPA-SET-SEC
      *    入力コードなしの対象が０件の時、全体を表示
           IF      SPA-K023-MAX        =   ZERO
               MOVE    2                   TO  FLG-SYORI
               PERFORM                     310-SPA-SET-SEC
               MOVE    1                   TO  SPA-K023-SYORI
           END-IF
           MOVE    1                   TO  SPA-K023-PAGE
           MOVE    1                   TO  SPA-K023-MAP
           MOVE    1                   TO  SPA-K023-CUR
      *
           .
       300-INIT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       30010-KID1-SET-SEC                 SECTION.
      *
      *
           EVALUATE    SPA-KIDCD
               WHEN    "1001"
                   IF      SPA-KID1-FLG        =   "OK"
      *                 更新処理
                        PERFORM 41002-KOUSIN-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-K023-CUR
                       MOVE    1               TO  SPA-K023-MAP
                   END-IF
           END-EVALUATE
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-K02SPA-SET-SEC
      *
      *    対象コードセット
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  SPA-K023-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-GMN-MAX  )  OR
                              (SPA-K023-MAX    >   MAX-LINE )
               MOVE    ZERO                TO  FLG-OK
               IF     (SPA-GMN-INPUTCD (IDX)(1:1)  =   "." OR "S" )  OR
                      (SPA-COM-INPUTCD (IDX)       =   SPACE     )   OR
                      (SPA-GMN-INPUTCD (IDX)(1:1)  =   "#" OR "$" )
                   CONTINUE
               ELSE
                   IF      FLG-SYORI           =    1
                       IF      SPA-COM-ICD-INPUTCD(IDX)    =   SPACE
                           MOVE    1               TO  FLG-OK
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
               IF      FLG-OK              =   1
                   MOVE    ZERO                TO  FLG-K023
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   SPA-K023-MAX
                       IF      SPA-COM-INPUTCD (IDX)
                                               =   SPA-K023-SRYCD(IDY)
                           MOVE    1               TO  FLG-K023
                           MOVE    SPA-K023-MAX    TO  IDY
                       END-IF
                   END-PERFORM
                   IF      FLG-K023            =   ZERO
                       ADD     1               TO  SPA-K023-MAX
                       IF      SPA-K023-MAX    >   MAX-LINE
                           CONTINUE
                       ELSE
                           MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  SPA-K023-SRYCD
                                                        (SPA-K023-MAX)
                           MOVE    SPA-GMN-MEISYO (IDX)
                                               TO  SPA-K023-SRYCDMEI
                                                         (SPA-K023-MAX)
                           IF      SPA-COM-ICD-INPUTCD(IDX)
                                                   =   SPACE
                               CONTINUE
                           ELSE
                               PERFORM 3101-INPUTCD-SET-SEC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-K023-MAX        >   MAX-LINE
               MOVE    MAX-LINE            TO  SPA-K023-MAX
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードセット処理
      *****************************************************************
       3101-INPUTCD-SET-SEC             SECTION.
      *
      *    入力コードチェック
           INITIALIZE                      INPUTCD-REC
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-K023-SRYCD  (SPA-K023-MAX)
                                       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (FLG-INPUTCD     =   1   )  OR
                              (IDX2            >   3   )
               MOVE    ICD-INPUTCD         TO  SPA-K023-INPUTCD 
                                                   (SPA-K023-MAX IDX2)
               MOVE    ICD-CDSYU           TO  SPA-K023-CDSYU
                                                   (SPA-K023-MAX IDX2)
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-INPUTCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-K02SPA-SET-SEC             SECTION.
      *
      *
      *****MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *
      *    OPEN    INPUT               SPASCR-FILE
      *    IF      STS-SPASCR     NOT =   ZERO
      *        GO      TO      1002-K02SPA-SET-EXT
      *    END-IF
      *
      *    MOVE    ZERO                TO  FLG-SPASCR
      *    MOVE    ZERO                TO  IDX
      *    PERFORM UNTIL       FLG-SPASCR  =   1
      *        READ    SPASCR-FILE
      *            AT  END
      *                MOVE   1            TO  FLG-SPASCR
      *            NOT AT  END
      *                ADD    1                TO  IDX
      *                MOVE   SPA-DATA-REC     TO  SPA-GMN-TBL (IDX)
      *        END-READ
      *    END-PERFORM
      *****CLOSE                       SPASCR-FILE
      *
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC        TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
           .
       1002-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  K023
           INITIALIZE                      K023
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   MAX-GMN
               COMPUTE IDY             =   IDX
                                       + ((SPA-K023-PAGE   -   1 )
                                       *   MAX-GMN   )
               IF      IDY             >   SPA-K023-MAX
                   MOVE    MAX-GMN         TO  IDX
               ELSE
                   MOVE    SPA-K023-SRYCD   (IDY)  TO  K023-SRYCD (IDX)
                   MOVE    SPA-K023-SRYCDMEI (IDY) TO  K023-SRYCDMEI
                                                                (IDX)
      *
                   MOVE    SPA-K023-INPUTCD(IDY 1) TO  K023-INPUTCD1
                                                                (IDX)
                   MOVE    SPA-K023-INPUTCD(IDY 2) TO  K023-INPUTCD2
                                                                (IDX)
                   MOVE    SPA-K023-INPUTCD(IDY 3) TO  K023-INPUTCD3
                                                                (IDX)
               END-IF
           END-PERFORM
      *
           IF      SPA-K023-SYORI      =   1
               MOVE    "（全体）"          TO  K023-MIDASI-VALUE
               MOVE    "red"               TO  K023-MIDASI-STYLE
           ELSE
               MOVE    "（未設定）"        TO  K023-MIDASI-VALUE
               MOVE    "black"             TO  K023-MIDASI-STYLE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-K023-CUR        =   ZERO
               PERFORM 50011-INPUT-CURSET-SEC
           END-IF
           IF      SPA-K023-MAP        =   ZERO
               MOVE    1               TO  SPA-K023-MAP
           END-IF
      *
           IF      SPA-K023-MAP        >   MAX-GMN
               MOVE    5               TO  SPA-K023-CUR
           ELSE
               IF     (SPA-K023-IDX        >   SPA-K023-MAX )  OR
                      (K023-SRYCD (SPA-K023-MAP)   =   SPACE )
                   MOVE    4               TO  SPA-K023-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-K023-CUR
               WHEN    1
                   MOVE    "INPUTCD1"      TO  MCP-WIDGET
                   MOVE    SPA-K023-MAP    TO  MCP-WIDGET(9:2)
               WHEN    2
                   MOVE    "INPUTCD2"      TO  MCP-WIDGET
                   MOVE    SPA-K023-MAP    TO  MCP-WIDGET(9:2)
               WHEN    3
                   MOVE    "INPUTCD3"      TO  MCP-WIDGET
                   MOVE    SPA-K023-MAP    TO  MCP-WIDGET(9:2)
               WHEN    4
                   MOVE    "B12"           TO  MCP-WIDGET
               WHEN    5
                   MOVE    "B07"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力位置より画面カーソルセット処理
      *****************************************************************
       50011-INPUT-CURSET-SEC             SECTION.
      *
      *    入力が空白の時、下の段へ
           IF      WRK-SP-FLG          =   1
               ADD     1                   TO  WRK-IDX2
               MOVE    1                   TO  SPA-K023-CUR
               MOVE    WRK-IDX2            TO  SPA-K023-MAP
           ELSE
               EVALUATE    WRK-WIDGET
                   WHEN    "INPUTCD1"
                       MOVE    2               TO  SPA-K023-CUR
                       MOVE    WRK-IDX2        TO  SPA-K023-MAP
                   WHEN    "INPUTCD2"
                       MOVE    3               TO  SPA-K023-CUR
                       MOVE    WRK-IDX2        TO  SPA-K023-MAP
                   WHEN    "INPUTCD3"
                       MOVE    1               TO  SPA-K023-CUR
                       ADD     1               TO  WRK-IDX2
                       MOVE    WRK-IDX2        TO  SPA-K023-MAP
               END-EVALUATE
           END-IF
      *
           .
       50011-INPUT-CURSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *        全体
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-ALL-HEN-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4100-TOROKU-SEC
           END-EVALUATE
          .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 210-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-K023-CUR
           MOVE    ZERO                TO  SPA-K023-MAP
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
      *    入力項目の決定
           IF      MCP-WIDGET(1:7)     =   "INPUTCD"
      *        入力行NOの設定
               MOVE    MCP-WIDGET(1:8)     TO  WRK-WIDGET
               MOVE    MCP-WIDGET(9:2)     TO  WRK-IDX2
               COMPUTE SPA-K023-IDX        =   WRK-IDX2
                                           + ((SPA-K023-PAGE   -   1)
                                           *   20 )
           END-IF
      *
      *    入力が空白かの判定
           MOVE    ZERO                TO  WRK-SP-FLG
           EVALUATE    WRK-WIDGET
      *        入力コード１
               WHEN    "INPUTCD1"
                   IF      K023-INPUTCD1 (WRK-IDX2)    =   SPACE
                       MOVE    1               TO  WRK-SP-FLG
                   END-IF
      *        入力コード２
               WHEN    "INPUTCD2"
                   IF      K023-INPUTCD2 (WRK-IDX2)    =   SPACE
                       MOVE    1               TO  WRK-SP-FLG
                   END-IF
      *        入力コード３
               WHEN    "INPUTCD3"
                   IF      K023-INPUTCD3 (WRK-IDX2)    =   SPACE
                       MOVE    1               TO  WRK-SP-FLG
                   END-IF
           END-EVALUATE
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               COMPUTE IDX-T               =   IDX
                                           + ((SPA-K023-PAGE   -   1)
                                           *   20 )
               MOVE    K023-INPUTCD1 (IDX) TO  SPA-K023-INPUTCD
                                                             (IDX-T 1)
               MOVE    K023-INPUTCD2 (IDX) TO  SPA-K023-INPUTCD
                                                             (IDX-T 2)
               MOVE    K023-INPUTCD3 (IDX) TO  SPA-K023-INPUTCD
                                                             (IDX-T 3)
           END-PERFORM
           .
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    空白削除
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   MAX-LINE
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY             >   3
                   IF     (SPA-K023-INPUTCD (IDX IDY)  =   SPACE) AND
                          (IDY                         <   3    )
                       COMPUTE IDX-2           =   IDY     +   1
                       MOVE    SPA-K023-INPUTCD (IDX IDX-2)
                                               TO  SPA-K023-INPUTCD
                                                             (IDX IDY)
                       MOVE    SPA-K023-CDSYU (IDX IDX-2)
                                               TO  SPA-K023-CDSYU
                                                             (IDX IDY)
                       MOVE    SPACE           TO  SPA-K023-INPUTCD
                                                             (IDX IDX-2)
                       MOVE    SPACE           TO  SPA-K023-CDSYU
                                                             (IDX IDX-2)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    入力コードチェック
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1           >   MAX-LINE ) OR
                              (SPA-ERRCD   NOT =   SPACE )
               COMPUTE WRK-PAGE        =  ((IDX-1    -   1 )
                                       /   20  )
                                       +   1
               PERFORM VARYING     IDX-2     FROM    1   BY  1
                       UNTIL      (IDX-2             >   3   )   OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-K023-INPUTCD (IDX-1 IDX-2) NOT =   SPACE
                       MOVE    SPA-K023-INPUTCD (IDX-1 IDX-2)
                                                   TO  WRK-INPUTCD
                       PERFORM 41020-INPUTCD-CHK-SEC
                       MOVE    WRK-INPUTCD         TO  SPA-K023-INPUTCD
                                                          (IDX-1 IDX-2)
                       IF      SPA-ERRCD           =   SPACE
                           MOVE    WRK-CDSYU       TO  SPA-K023-CDSYU
                                                       (IDX-1 IDX-2)
                       ELSE
                           IF      WRK-PAGE        =   SPA-K023-PAGE
                               CONTINUE
                           ELSE
                               MOVE    WRK-PAGE        TO  SPA-K023-PAGE
                           END-IF
                           COMPUTE SPA-K023-MAP    =   IDX-1
                                                   -  ((SPA-K023-PAGE
                                                   -   1)
                                                   *   20 )
                           MOVE    IDX-2           TO  SPA-K023-CUR
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
      *
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力コード登録チェック処理
      *****************************************************************
       41020-INPUTCD-CHK-SEC                SECTION.
      *
      *    桁数チェック
           MOVE    ZERO                TO  WRK-CNT
      *
      *    漢字入力チェック
           MOVE    ZERO               TO  FLG-KANJI
           MOVE    ZERO               TO  FLG-KANA
           MOVE    ZERO               TO  FLG-SUJI
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-INPUTCD         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-INPUTCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0005"          TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX     TO  WRK-CNT
      *        間に空白がある時、エラー
               IF      KANACHK-RC2         =   8
                   MOVE    "0010"          TO  SPA-ERRCD
                   GO      TO      41020-INPUTCD-CHK-EXT
               END-IF
      *
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   IF      KANACHK-MAX         >   10
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
                   IF    KANACHK-RC3   NOT =   ZERO
      *                全角空白あり
                       MOVE    "0014"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        半角
               IF      KANACHK-SYUBETU     =   1
                   IF      WRK-INPUTCD (1:WRK-CNT) NUMERIC
      *                数字桁数チェック
                       IF      KANACHK-MAX         <   4   OR  >  6
                           MOVE    "0006"              TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                S,Pチェック
                       IF      WRK-INPUTCD (1:1)   =   "S"   OR  "P"
                           MOVE    "0007"      TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            複数科・保険チェック
                   IF      WRK-INPUTCD (1:1)   =   "#"   OR  "$"
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
      *D   IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      41020-INPUTCD-CHK-EXT
      *D   END-IF
      *    記号は入力不可とする
           IF      FLG-KANJI           =   1
               MOVE    2                   TO  IDX-C
           ELSE
               MOVE    1                   TO  IDX-C
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  IDX-C
                   UNTIL      (IDY         >   20  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDX-C               =   1
                   IF      WRK-INPUTCD (IDY:1) =   "+"   OR  "-"
                                               OR  "/"   OR  "!"
                                               OR  "."   OR  "*"
                                               OR  "_"
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF    WRK-INPUTCD (IDY:2)   =   "＋"   OR  "−"
                                               OR  "／"   OR  "！"
                                               OR  "．"   OR  "＊"
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41020-INPUTCD-CHK-EXT
           END-IF
      *
      *    同一コードチェック
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   SPA-K023-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               PERFORM VARYING     IDY2    FROM    1   BY  1
                       UNTIL      (IDY2        >   3         )  OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      SPA-K023-INPUTCD (IDY IDY2) NOT =   SPACE
                       IF     (IDX-1               =   IDY  )  AND
                              (IDX-2               =   IDY2 )
                           CONTINUE
                       ELSE
                           IF      WRK-INPUTCD     =   SPA-K023-INPUTCD
                                                            (IDY IDY2)
      *                表示中の画面をエラー
                           AND    (SPA-K023-PAGE   =   WRK-PAGE  )
                               MOVE    "0002"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41020-INPUTCD-CHK-EXT
           END-IF
      *
      *    入力コードチェック
           INITIALIZE                      INPUTCD-REC
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
      *    漢字入力
           IF      FLG-KANJI           =   1
               MOVE    "J"                 TO  ICD-CDSYU
           ELSE
               IF      WRK-INPUTCD (1:WRK-CNT)   NUMERIC
                   EVALUATE    WRK-CNT
                       WHEN    4
                           MOVE    "4"                 TO  ICD-CDSYU
                       WHEN    5
                           MOVE    "5"                 TO  ICD-CDSYU
                       WHEN    6
                           MOVE    "6"                 TO  ICD-CDSYU
                   END-EVALUATE
               ELSE
                   MOVE    "A"                 TO  ICD-CDSYU
               END-IF
           END-IF
           MOVE    ICD-CDSYU           TO  WRK-CDSYU
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
           IF      WRK-CNT             =   20
               MOVE    "key2"              TO  WRK-PATH
           ELSE
               COMPUTE IDK                 =   WRK-CNT  +  1
               MOVE    "%"                 TO  ICD-INPUTCD(IDK:1)
               MOVE    "key"               TO  WRK-PATH
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (WRK-INPUTCD         =   ICD-INPUTCD )
               IF      ICD-SRYCD       NOT =   SPA-K023-SRYCD (IDX-1)
                   MOVE    "0003"          TO  SPA-ERRCD
      *!!
      *H30.10
      *            登録済み診療コード
                   PERFORM 410201-CDERR-MSG-SEC
               END-IF
           END-IF
      *
           .
       41020-INPUTCD-CHK-EXT.
           EXIT.
      *
      *H30.10
      *****************************************************************
      *    登録済み診療コード処理
      *****************************************************************
       410201-CDERR-MSG-SEC        SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG2
           IF      ICD-SRYCD(1:1)      =   "P"
                                       OR  "S"
               STRING  "セットコード："
                       ICD-SRYCD
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG2
               END-STRING
               GO      TO      410201-CDERR-MSG-EXT
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    ICD-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
      *    開始日の降順に検索
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    SPACE               TO  SPA-ERRMSG2
               MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               STRING  "診療コード："
                       TNS-SRYCD
                       " 終了日："
                       WRK-HENYMDG     DELIMITED  BY  SIZE
                       "　"
                       TNS-NAME (1:36) DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG2
               END-STRING
           END-IF
           .
       410201-CDERR-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力２処理
      *****************************************************************
       41011-INPUTCD2-SYORI-SEC        SECTION.
      *
           MOVE    2                   TO  SPA-K023-CUR
      *
           MOVE    K023-INPUTCD2 (WRK-IDX2)    TO  SPA-K023-INPUTCD
                                                      (SPA-K023-IDX 2)
      *
      *    空白入力の時、２を編集後、次ぎの段へ
           IF      SPA-K023-INPUTCD (SPA-K023-IDX 2)
                                       =   SPACE
               MOVE    SPA-K023-INPUTCD-G (SPA-K023-IDX 3)
                                           TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 2)
               MOVE    SPACE               TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 3)
               MOVE    1                   TO  SPA-K023-CUR
               MOVE    WRK-IDX2            TO  SPA-K023-MAP
               ADD     1                   TO  SPA-K023-MAP
               GO      TO     41011-INPUTCD2-SYORI-EXT
           END-IF
      *
      *    登録済チェック
           MOVE    SPA-K023-INPUTCD (SPA-K023-IDX 2)
                                       TO  WRK-INPUTCD
           MOVE    SPA-K023-IDX        TO  IDX-1
           MOVE    2                   TO  IDX-2
           PERFORM 41020-INPUTCD-CHK-SEC
           MOVE    WRK-INPUTCD         TO  SPA-K023-INPUTCD
                                                   (SPA-K023-IDX 2)
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     41011-INPUTCD2-SYORI-EXT
           END-IF
           MOVE    WRK-CDSYU           TO  SPA-K023-CDSYU
                                                       (SPA-K023-IDX 2)
      *
      *    １が空白の時、２を１へ編集
           IF      SPA-K023-INPUTCD (SPA-K023-IDX 1)
                                       =   SPACE
               MOVE    SPA-K023-INPUTCD-G (SPA-K023-IDX 2)
                                           TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 1)
               MOVE    SPA-K023-INPUTCD-G (SPA-K023-IDX 3)
                                           TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 2)
               MOVE    SPACE               TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 3)
           END-IF
      *
           MOVE    3                   TO  SPA-K023-CUR
           MOVE    WRK-IDX2            TO  SPA-K023-MAP
      *
           .
       41011-INPUTCD2-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力３処理
      *****************************************************************
       41011-INPUTCD3-SYORI-SEC        SECTION.
      *
           MOVE    3                   TO  SPA-K023-CUR
      *
           MOVE    K023-INPUTCD3 (WRK-IDX2)    TO  SPA-K023-INPUTCD
                                                      (SPA-K023-IDX 3)
      *
      *    空白入力の時、２を編集後、次ぎの段へ
           IF      SPA-K023-INPUTCD (SPA-K023-IDX 3)
                                       =   SPACE
               MOVE    1                   TO  SPA-K023-CUR
               MOVE    WRK-IDX2            TO  SPA-K023-MAP
               ADD     1                   TO  SPA-K023-MAP
               GO      TO     41011-INPUTCD3-SYORI-EXT
           END-IF
      *
      *    登録済チェック
           MOVE    SPA-K023-INPUTCD (SPA-K023-IDX 3)
                                       TO  WRK-INPUTCD
           MOVE    SPA-K023-IDX        TO  IDX-1
           MOVE    3                   TO  IDX-2
           PERFORM 41020-INPUTCD-CHK-SEC
           MOVE    WRK-INPUTCD         TO  SPA-K023-INPUTCD
                                                 (SPA-K023-IDX 3)
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     41011-INPUTCD3-SYORI-EXT
           END-IF
           MOVE    WRK-CDSYU           TO  SPA-K023-CDSYU
                                                       (SPA-K023-IDX 3)
      *
      *    ２が空白の時、３を２へ編集
           IF      SPA-K023-INPUTCD (SPA-K023-IDX 2)
                                       =   SPACE
               MOVE    SPA-K023-INPUTCD-G (SPA-K023-IDX 3)
                                           TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 2)
               MOVE    SPACE               TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 3)
           END-IF
      *
      *    １が空白の時、２を１へ編集
           IF      SPA-K023-INPUTCD (SPA-K023-IDX 1)
                                       =   SPACE
               MOVE    SPA-K023-INPUTCD-G (SPA-K023-IDX 2)
                                           TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 1)
               MOVE    SPACE               TO   SPA-K023-INPUTCD-G
                                                      (SPA-K023-IDX 2)
           END-IF
      *
           MOVE    1                   TO  SPA-K023-CUR
           MOVE    WRK-IDX2            TO  SPA-K023-MAP
           ADD     1                   TO  SPA-K023-MAP
      *
           .
       41011-INPUTCD3-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       409-ALL-HEN-SEC             SECTION.
      *
      *    全体
           INITIALIZE                  SPA-K023GMN-AREA
           MOVE    2                   TO  FLG-SYORI
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                   TO  SPA-K023-SYORI
           MOVE    1                   TO  SPA-K023-PAGE
           MOVE    1                   TO  SPA-K023-MAP
           MOVE    1                   TO  SPA-K023-CUR
      *
           .
       409-ALL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      450-MAEGMN-EXT
           END-IF
      *
      *
           IF      SPA-K023-PAGE       =   1
               MOVE    1                   TO  SPA-K023-PAGE
               MOVE     "0013"             TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-K023-PAGE       =   SPA-K023-PAGE   -   1
           END-IF
      *
           MOVE    1                   TO  SPA-K023-MAP
           MOVE    1                   TO  SPA-K023-CUR
           .
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           COMPUTE IDX                 =   SPA-K023-PAGE   *   MAX-GMN
                                       +   1
      *
           IF      IDX                 >   SPA-K023-MAX
               MOVE     "0012"             TO  SPA-ERRCD
           ELSE
               ADD     1                   TO  SPA-K023-PAGE
               MOVE    1                   TO  SPA-K023-MAP
           END-IF
           MOVE    1                   TO  SPA-K023-CUR
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
      *
      *    更新確認メッセージ表示へ
           IF      SPA-ERRCD           =   SPACE
               MOVE    "1001"              TO  SPA-KIDCD
           END-IF
      *
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       41001-KIHON-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           MOVE    ZERO                TO  WRK-SP-FLG
           MOVE    ZERO                TO  SPA-K023-CUR
           MOVE    ZERO                TO  SPA-K023-MAP
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
      *    全体チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 230-INPUT-ALL-SEC
           END-IF
           .
       41001-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体チェック処理
      *****************************************************************
       230-INPUT-ALL-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-K023-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
      *        画面表示コードが全角１文字数字をエラーとする
               IF     (SPA-K023-INPUTCD (IDX 1)  NOT =   SPACE)  AND
                      (SPA-K023-CDSYU   (IDX 1)  =   "J"  )  AND
                      (SPA-K023-INPUTCD (IDX 1)(3:)
                                                  =   SPACE )
                   IF      SPA-K023-INPUTCD (IDX 1)(1:2)
                                                   >=  "０"   AND
                                                   <= "９"
                       MOVE    "0009"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-K023-CUR
                       MOVE    IDX                 TO  SPA-K023-MAP
                   END-IF
               END-IF
      *        画面表示コードが１０文字以上をエラーとする
               IF     (SPA-K023-INPUTCD (IDX 1)  NOT =   SPACE)  AND
                      (SPA-K023-INPUTCD (IDX 1)(11:)
                                                 NOT =   SPACE )
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K023-CUR
                   MOVE    IDX                 TO  SPA-K023-MAP
               END-IF
           END-PERFORM
           .
       230-INPUT-ALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-K023-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               PERFORM 410022-INPUTCD-HOZON-SEC
               MOVE    ZERO                TO  WRK-DSPSEQ
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY         >   20   )  OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      WRK-K023-INPUTCD (IDY)  =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    WRK-K023-INPUTCD(IDY)
                                                   TO  WRK-INPUTCD
                       MOVE    WRK-K023-CDSYU  (IDY)
                                                    TO  WRK-CDSYU
      *H28.11
      *                登録済みチェック
                       PERFORM 410020-INPUTCD-MAECHK-SEC
                       IF      FLG-CHKERR          =   ZERO
                           PERFORM 410021-INPUTCD-INS-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    戻る
           PERFORM 210-BACK
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *H28.11
      *****************************************************************
      *    入力コード登録済みチェック処理
      *****************************************************************
       410020-INPUTCD-MAECHK-SEC          SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-CDSYU           TO  ICD-CDSYU
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
      *        登録済み
               MOVE    1                   TO  FLG-CHKERR
           ELSE
               MOVE    ZERO                TO  FLG-CHKERR
           END-IF
           .
       410020-INPUTCD-MAECHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード保存・削除処理
      *****************************************************************
       410022-INPUTCD-HOZON-SEC          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           INITIALIZE                      WRK-INPUTCD-TBL
           MOVE    ZERO                TO  IDX-T
      *
      *    入力コードチェック
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-K023-SRYCD (IDX)
                                       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           PERFORM
                   UNTIL       FLG-INPUTCD     =   1
               ADD     1                   TO  IDX-T
               MOVE    ICD-INPUTCD         TO  WRK-K023-INPUTCD (IDX-T)
               MOVE    ICD-CDSYU           TO  WRK-K023-CDSYU (IDX-T)
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    画面内容のセット
           MOVE    SPA-K023-INPUTCD (IDX 1)    TO  WRK-K023-INPUTCD (1)
           MOVE    SPA-K023-INPUTCD (IDX 2)    TO  WRK-K023-INPUTCD (2)
           MOVE    SPA-K023-INPUTCD (IDX 3)    TO  WRK-K023-INPUTCD (3)
           MOVE    SPA-K023-CDSYU (IDX 1)  TO  WRK-K023-CDSYU (1)
           MOVE    SPA-K023-CDSYU (IDX 2)  TO  WRK-K023-CDSYU (2)
           MOVE    SPA-K023-CDSYU (IDX 3)  TO  WRK-K023-CDSYU (3)
      *
      *    ＤＢ削除
           IF     IDX-T            NOT =   ZERO
               MOVE    SPACE               TO  INPUTCD-REC
               INITIALIZE                      INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-K023-SRYCD (IDX)    TO  ICD-SRYCD
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
           .
       410022-INPUTCD-HOZON-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード追加処理
      *****************************************************************
       410021-INPUTCD-INS-SEC          SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-CDSYU           TO  ICD-CDSYU
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
           MOVE    SPA-K023-SRYCD (IDX)    TO  ICD-SRYCD
           ADD     1                   TO  WRK-DSPSEQ
           MOVE    WRK-DSPSEQ          TO  ICD-DSPSEQ
      *
           MOVE    SMCNDATE-YMD        TO  ICD-CREYMD
           MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
           MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
           MOVE    SPA-OPID            TO  ICD-OPID
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1001"          TO  SPA-ERRCD
               DISPLAY "K023 INPUTCD INS ERR:"  MCP-RC
                             ",KEY:"  ICD-KEY
           END-IF
           .
       410021-INPUTCD-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *****MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K023"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K023"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "同一コードが有ります"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "既に登録済のコードです。"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "漢字コードは５文字までです"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "漢字との混在はできません"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
              "数字のみの入力コードは４桁から６桁で入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "入力コードは始まりがＳ、Ｐ以外のコード"
                                                 TO  SPA-ERRMSG
                   MOVE    "にして下さい。"
                                                 TO  SPA-ERRMSG(39:)
               WHEN    "0008"
                   MOVE
                   "入力コードには使用できない記号があります。"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE
                   "表示入力コードに全角１文字の数字は使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "文字の間に空白は入力できません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "表示入力コードは半角１０文字、"
                                                 TO  SPA-ERRMSG
                   MOVE    "全角５文字までにして下さい。"
                                                 TO  SPA-ERRMSG(31:)
               WHEN    "0012"
                   MOVE    "次頁はありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "前頁はありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   STRING  "全角空白があります。"
                           "全角空白は入力できません。"
                                        DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        エラー画面２
               PERFORM 5101-KERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K023"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           INITIALIZE                      KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K023"              TO  SPA-MOTOPG
           MOVE    "KERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KERR2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "1001"
                   MOVE
                   "入力コードを登録します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K023"              TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KISSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
