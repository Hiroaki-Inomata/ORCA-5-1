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
       PROGRAM-ID.                 ORCGP97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 氏名検索（Ｐ９７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *   01.00.01    MCC-藤原     01/04/10　LINKAREAの初期化の削除
      *   01.00.02    MCC-太田     01/05/24  患者番号構成管理の追加
      *   01.00.03    MCC-多々納   01/08/20  仮名入力変換処理の追加
      *                                      漢字・かなの検索の一元化
      *   01.00.04    MCC-藤原     01/09/19  番号の編集    
      *   01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *   01.00.06    MCC-多々納   01/09/25  一覧表選択の追加
      *   01.00.07    NACL-多々納  02/12/12  検索条件の追加
      *   01.00.08    NACL-太田    03/02/06  入力引数に名前が設定されて
      *                                      いる場合は全角変換して使う
      *   01.00.09    NACL-多々納  04/07/22  保険の追加
      *   01.00.10    NACL-多々納  05/11/10  MONFUNCの修正
      *   03.03.00    NACL-多々納  06/10/06  名前検索の追加
      *   03.05.00    NACL-多々納  07/06/05  前回来院日の共通化
      *   04.01.00    NACL-多々納  08/01/30  次頁の画面表示追加
      *   04.05.00    NACL-多々納  09/10/27  拡張漢字対応
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
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  SPA-P97.
           03  SPA-NAI-MOTOPG          PIC X(08).
           03  SPA-P97-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
               05  SPA-GMN-NEXT        PIC 9(01).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-NAMEKEN     PIC X(30).
                   07  SPA-GMN-KBIRTHDAY   PIC X(10).
                   07  SPA-GMN-KSEX-G.
                       09  SPA-GMN-KSEX        PIC X(01).
                       09  SPA-GMN-K01         PIC X(01).
                       09  SPA-GMN-KSEXMEI     PIC X(04).
                   07  SPA-GMN-KSEX-LIST       PIC X(06)   OCCURS  3.
      *
                   07  SPA-GMN-KNYUGAIKBN-G.
                       09  SPA-GMN-KNYUGAIKBN      PIC X(01).
                       09  SPA-GMN-K02             PIC X(01).
                       09  SPA-GMN-KKNYUGAIKBNMEI  PIC X(08).
                   07  SPA-GMN-KNYUGAIKBN-LIST     PIC X(10)
                                                   OCCURS  3.
                   07  SPA-NYUGAI-MAX              PIC 9(02).
      *
                   07  SPA-GMN-TBLG.
                       09  SPA-GMN-TBL     OCCURS  100.
                           11  SPA-GMN-NO          PIC 9(04).
                           11  SPA-GMN-PTNUM       PIC X(20).
                           11  SPA-GMN-NAME        PIC X(50).
                           11  SPA-GMN-SEX         PIC X(04).
                           11  SPA-GMN-BIRTH       PIC X(10).
                           11  SPA-GMN-NENREI      PIC X(10).
                           11  SPA-GMN-RRKYMD      PIC X(10).
                           11  SPA-GMN-SRYKA       PIC X(04).
                           11  SPA-GMN-HKNCOMBI    PIC X(06).
                           11  SPA-GMN-HONKZKKBN   PIC X(02).
                           11  SPA-GMN-BRMNUM      PIC X(09).
                   07  SPA-GMN-SELNO          PIC 9(04).
      *
                   07  SPA-GMN-MCPCHK         PIC X(02).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-NAMEKEN        PIC X(32).
                   07  SPA-NAI-KBIRTHDAY      PIC X(08).
                   07  SPA-NAI-KBMAX          PIC 9(02).
                   07  SPA-NAI-TBLG.
                       09  SPA-NAI-TBL            OCCURS  100.
                           11  SPA-NAI-SEX        PIC X(01).
                           11  SPA-NAI-BIRTH      PIC X(08).
                           11  SPA-NAI-PTID       PIC X(10).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KNJHEN          PIC 9(01).
           03  FLG-KENSAKU         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-NYUIN           PIC 9(01).
           03  FLG-NAMEKEN         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(01).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDK                 PIC 9(04).
      *
           03  IDX-S1              PIC 9(04).
           03  IDX-S2              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-MOJISU          PIC 9(04).
           03  WRK-NAME            PIC X(30).
           03  WRK-YY              PIC 9(02).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-NAME1           PIC X(30).
           03  WRK-NAME2           PIC X(30).
      *
           03  WRK-STR             PIC 9(04).
      *
           03  WRK-LSTYMD              PIC X(08).
           03  WRK-LSTYMDW             PIC X(09).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-BRMNUM.
               05  WRK-BRMNUM1             PIC X(02).
               05  WRK-BRMNUM-H            PIC X(01).
               05  WRK-BRMNUM2             PIC X(06).
           03  WRK-Z6-X.
               05  WRK-Z6                  PIC ZZZZZZ.
           03  WRK-BRMNUM-9            PIC 9(06).
      *
           03  WRK-ZZ                  PIC ZZZ9.
      *
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-ORC-DBPATH          PIC X(64).
      *    年齢計算
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(03).
               05  WRK-NENREI-MM       PIC 9(02).
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
      *
           03  WRK-HKNCOMBI        PIC X(04).
           03  WRK-HKNCOMBI-MEI    PIC X(10).
           03  WRK-HKNNUM          PIC X(03).
           03  WRK-HONKZKKBN-MEI   PIC X(02).
      *
           03  WRK-E-NAME          PIC X(30).
           03  WRK-H-NAME          PIC X(30).
           03  WRK-H-NAME-G.
               05  WRK-H-NAME1         PIC X(02).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS   99
                                       INDEXED  SRYKA-IDX.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-SRYKAMEI        PIC X(04).
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
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張）
           COPY    "CPORCSKANACHK2.INC".
      *
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
           COPY    "CPORCSLOCK.INC".
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
           COPY    "P97.INC".
           COPY    "P97ERR.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
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
           MOVE    SPA-FREE        TO  SPA-P97.
      *
           MOVE    SPACE           TO  SPA-ERRCD
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
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P97         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "P97ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5011-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "P97"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-P97
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
      *    他のＬＤより
      *****IF      LINKAREA        NOT =   SPACE
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-P97-AREA
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    "0 全体"            TO  SPA-GMN-KSEX-LIST (01)
           MOVE    "1 男"              TO  SPA-GMN-KSEX-LIST (02)
           MOVE    "2 女"              TO  SPA-GMN-KSEX-LIST (03)
           MOVE    SPA-GMN-KSEX-LIST (01)  TO  SPA-GMN-KSEX-G
      *
           MOVE    "0 全体"            TO  SPA-GMN-KNYUGAIKBN-LIST (01)
           MOVE    "1 入院"            TO  SPA-GMN-KNYUGAIKBN-LIST (02)
           MOVE    "2 外来"            TO  SPA-GMN-KNYUGAIKBN-LIST (03)
           MOVE    SPA-GMN-KNYUGAIKBN-LIST (01)
                                       TO  SPA-GMN-KNYUGAIKBN-G
      *
           IF      SPA-BEDFLG          =   ZERO
               MOVE    1                   TO  SPA-NYUGAI-MAX
           ELSE
               MOVE    3                   TO  SPA-NYUGAI-MAX
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-KBIRTHDAY
           MOVE    SPACE               TO  SPA-NAI-KBIRTHDAY
      *
      *    前の画面より、氏名の受け取り
           IF      SPA-NAME        NOT =   SPACE
               MOVE    SPA-NAME            TO  SPA-GMN-NAMEKEN
      *
               PERFORM 209-NAMEKEN-SEC
      *
               PERFORM 2011-KNJKEN-HEN-SEC
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *            終了
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *            クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *            次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  FLG-KNJHEN
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *????    検索項目入力（変換）
               WHEN    "CHANGED"       ALSO    "NAMEKEN"
                   MOVE    1           TO  FLG-KNJHEN
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
                   IF      FLG-KNJHEN          =   1
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        番号選択入力
               WHEN    "ACTIVATE"      ALSO    "SELNO"
                   PERFORM 202-SELNO-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           IF      P97-NAMEKEN         =   SPA-GMN-NAMEKEN
               MOVE    ZERO            TO  FLG-NAMEKEN
           ELSE
               MOVE    1               TO  FLG-NAMEKEN
           END-IF
           MOVE    P97-NAMEKEN         TO  SPA-GMN-NAMEKEN
           MOVE    P97-KBIRTHDAY       TO  SPA-GMN-KBIRTHDAY
           MOVE    P97-KSEX            TO  SPA-GMN-KSEX-G
           MOVE    P97-KNYUGAIKBN      TO  SPA-GMN-KNYUGAIKBN-G
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           IF      FLG-NAMEKEN         =   1
               PERFORM 209-NAMEKEN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 201-BIRTHDAY-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 202-SEX-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 202-NYUGAIKBN-SEC
           END-IF
      *    検索処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 201-NAMEKEN-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名入力処理
      *****************************************************************
       209-NAMEKEN-SEC                 SECTION.
      *
      *    全角変換処理
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "2"                 TO  KANACHK2-SYORI
               MOVE    SPA-GMN-NAMEKEN     TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"      USING
                                           ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT  TO  SPA-GMN-NAMEKEN
               IF      ( KANACHK2-RC       =   ZERO )
                   MOVE    KANACHK2-OUT-INPUT
                                           TO  SPA-GMN-NAMEKEN
               END-IF
           ELSE
      *        拡張漢字不可
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-NAMEKEN     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-NAMEKEN
               IF      ( KANACHK-RC        =   ZERO )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  SPA-GMN-NAMEKEN
               END-IF
           END-IF
           IF      SPA-GMN-NAMEKEN     =   SPACE
               MOVE    SPACE               TO  SPA-NAI-NAMEKEN
           ELSE
               MOVE    SPACE               TO  SPA-NAI-NAMEKEN
      *        氏名検索用編集
               PERFORM 2091-NAME-HEN-SEC
           END-IF
      *
           .
       209-NAMEKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名編集処理
      *****************************************************************
       2091-NAME-HEN-SEC                 SECTION.
      *
           MOVE    SPA-GMN-NAMEKEN     TO  WRK-NAME
           MOVE    SPACE               TO  WRK-NAME1
           MOVE    SPACE               TO  WRK-NAME2
      *    ＊の次の　削除
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊　"              TO  ORCSSTR-STRING2
           MOVE    "＊"                TO  ORCSSTR-STRING3
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME
      *    ＊は名前とする
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1        =   ZERO
               MOVE    WRK-NAME            TO  WRK-NAME1
           ELSE
               MOVE    ORCSSTR-NUM1        TO  IDX-S1
               MOVE    ORCSSTR-NUM2        TO  IDX-S2
               IF      ORCSSTR-NUM1        =   1
                   MOVE    SPACE               TO  WRK-NAME1
               ELSE
                   INITIALIZE                  ORCSSTRINGAREA
                   MOVE    "substr"            TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME            TO  ORCSSTR-STRING1
                   MOVE    1                   TO  ORCSSTR-NUM1
                   COMPUTE ORCSSTR-NUM2        =   IDX-S1
                                               -   1
                   CALL    "orcsstring"        USING
                                               ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2     TO  WRK-NAME1
               END-IF
      *
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    WRK-NAME            TO  ORCSSTR-STRING1
               COMPUTE ORCSSTR-NUM1        =   IDX-S1
                                           +   1
               MOVE    0                   TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  WRK-NAME2
           END-IF
      *TEST
      *    DISPLAY "SPA-GMN-NAMEKEN:" SPA-GMN-NAMEKEN
      *    DISPLAY "WRK-NAME1:" WRK-NAME1
      *    DISPLAY "WRK-NAME2:" WRK-NAME2
      *     
      *    MOVE    SPACE               TO  WRK-NAME1
      *    MOVE    SPACE               TO  WRK-NAME2
      *    MOVE    1                   TO  IDX2
      *    PERFORM VARYING     IDX     FROM    1   BY  2
      *            UNTIL       IDX     >   KANACHK-MAX
      *        IF      SPA-GMN-NAMEKEN(IDX:2)  =   "＊"
      *            COMPUTE IDX3            =   IDX     +  2
      *            IF      SPA-GMN-NAMEKEN (IDX3:2)    =   "　"
      *                COMPUTE IDX3            =   IDX3     +  2
      *            END-IF
      *            IF      SPA-GMN-NAMEKEN (IDX3:) =   SPACE
      *                MOVE    SPACE               TO  WRK-NAME2
      *            ELSE
      *                MOVE    SPA-GMN-NAMEKEN (IDX3:) TO  WRK-NAME2
      *            END-IF
      *            MOVE    KANACHK-MAX             TO  IDX
      *        ELSE
      *            MOVE    SPA-GMN-NAMEKEN (IDX:2) TO  WRK-NAME1
      *                                                        (IDX2:2)
      *            ADD     2                   TO  IDX2
      *        END-IF
      **** END-PERFORM
      *
           MOVE    SPACE           TO  SPA-NAI-NAMEKEN
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2           =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    SPA-NAI-NAMEKEN
               END-STRING
           END-IF
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    SPA-NAI-NAMEKEN
               END-STRING
           END-IF
           IF     (WRK-NAME1           =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               STRING  "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    SPA-NAI-NAMEKEN
               END-STRING
           END-IF
      *
           .
       2091-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    生年月日入力　処理
      *****************************************************************
       201-BIRTHDAY-SEC                 SECTION.
      *
           IF      SPA-GMN-KBIRTHDAY   =   SPACE
               MOVE    SPACE               TO  SPA-NAI-KBIRTHDAY
               MOVE    ZERO                TO  SPA-NAI-KBMAX
               GO      TO      201-BIRTHDAY-EXT
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "2"                 TO  SGDAY-INTYPE
           MOVE    SPA-GMN-KBIRTHDAY   TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-KBIRTHDAY
               MOVE    SGDAY-SDAY          TO  SPA-NAI-KBIRTHDAY
               MOVE    8                   TO  SPA-NAI-KBMAX
               IF      SGDAY-SDAY(5:)      =   SPACE
                   MOVE    "%"             TO  SPA-NAI-KBIRTHDAY(5:1)
                   MOVE    4               TO  SPA-NAI-KBMAX
               ELSE
                   IF      SGDAY-SDAY(7:)  =   SPACE
                       MOVE      "%"       TO  SPA-NAI-KBIRTHDAY(7:1)
                       MOVE    6               TO  SPA-NAI-KBMAX
                   END-IF
               END-IF
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       201-BIRTHDAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    性別入力　処理
      *****************************************************************
       202-SEX-SEC                 SECTION.
      *
           IF      SPA-GMN-KSEX        =   SPACE
               MOVE    SPA-GMN-KSEX-LIST (1)   TO  SPA-GMN-KSEX-G
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-KSEX-LIST (IDX)(1:1)
                                           =   SPA-GMN-KSEX
                   MOVE    SPA-GMN-KSEX-LIST (IDX) TO  SPA-GMN-KSEX-G
                   MOVE    1                       TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KSEX-LIST (1)   TO  SPA-GMN-KSEX-G
           END-IF
      *
           .
       202-SEX-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外入力　処理
      *****************************************************************
       202-NYUGAIKBN-SEC                 SECTION.
      *
           IF      SPA-GMN-KNYUGAIKBN  =   SPACE
               MOVE    SPA-GMN-KNYUGAIKBN-LIST (1)
                                          TO  SPA-GMN-KNYUGAIKBN-G
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAI-MAX
               IF      SPA-GMN-KNYUGAIKBN-LIST (IDX)(1:1)
                                           =   SPA-GMN-KNYUGAIKBN
                   MOVE    SPA-GMN-KNYUGAIKBN-LIST (IDX)
                                               TO  SPA-GMN-KNYUGAIKBN-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    SPA-GMN-KNYUGAIKBN-LIST (1)
                                       TO  SPA-GMN-KNYUGAIKBN-G
           END-IF
      *
           .
       202-NYUGAIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索項目入力　処理
      *****************************************************************
       201-NAMEKEN-SEC                 SECTION.
      *
           INITIALIZE                  SPA-GMN-TBLG
           INITIALIZE                  SPA-NAI-TBLG
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF     (SPA-GMN-NAMEKEN     =   SPACE )  AND
                  (SPA-GMN-KBIRTHDAY   =   SPACE )
               IF      SPA-GMN-KSEX    NOT =   ZERO
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
                   IF      SPA-GMN-KNYUGAIKBN  NOT =   ZERO
                       MOVE    "0005"              TO  SPA-ERRCD
                   END-IF
               END-IF
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      201-NAMEKEN-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *    検索処理
           PERFORM 2011-KNJKEN-HEN-SEC
      *
      *    対象がある時、選択へ
           IF      SPA-GMN-MAX     >   ZERO
               MOVE    4               TO  SPA-GMN-CUR
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       201-NAMEKEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       2011-KNJKEN-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-TBLG
           INITIALIZE                  SPA-NAI-TBLG
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    診療科セット
           PERFORM 20119-SRYKA-HEN-SEC
      *
           MOVE    ZERO                TO  IDX
      *
      *    検索
           INITIALIZE                  PTINF-REC
      *****MOVE    SPA-HOSPID          TO  PTINF-HOSPID
           MOVE    SPACE               TO  PTINF-NAME
           MOVE    SPA-NAI-NAMEKEN     TO  PTINF-NAME
           MOVE    SPA-NAI-NAMEKEN     TO  PTINF-KANANAME
           MOVE    SPA-GMN-KSEX        TO  PTINF-SEX
           MOVE    SPA-NAI-KBIRTHDAY   TO  PTINF-BIRTHDAY
           MOVE    "key5"              TO  WRK-ORC-DBPATH
           IF     (SPA-GMN-NAMEKEN     NOT =   SPACE )  AND
                  (SPA-GMN-KSEX        NOT =   ZERO  )  AND
                  (SPA-NAI-KBIRTHDAY   NOT =   SPACE )
               MOVE    "key6"              TO  WRK-ORC-DBPATH
           END-IF
      *
           IF     (SPA-GMN-NAMEKEN         =   SPACE )  AND
                  (SPA-GMN-KSEX        NOT =   ZERO  )  AND
                  (SPA-NAI-KBIRTHDAY   NOT =   SPACE )
               MOVE    "key7"              TO  WRK-ORC-DBPATH
           END-IF
      *
           IF     (SPA-GMN-NAMEKEN         =   SPACE )  AND
                  (SPA-GMN-KSEX        NOT =   ZERO  )  AND
                  (SPA-NAI-KBIRTHDAY       =   SPACE )
               MOVE    "key8"              TO  WRK-ORC-DBPATH
           END-IF
      *
           IF     (SPA-GMN-NAMEKEN         =   SPACE )  AND
                  (SPA-GMN-KSEX            =   ZERO  )  AND
                  (SPA-NAI-KBIRTHDAY   NOT =   SPACE )
               MOVE    "key9"              TO  WRK-ORC-DBPATH
           END-IF
      *    患者マスターを漢字氏名で検索
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 810-PTINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           IF     (SPA-GMN-NEXT        =   1  )  AND
                  (SPA-GMN-PAGE        >   1  )
               COMPUTE WRK-STR         =   (SPA-GMN-PAGE  -  1)
                                       *   100
                                       +   1
           ELSE
               MOVE    1               TO  WRK-STR
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-NEXT
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           PERFORM UNTIL   (IDX        >   100       ) OR
                           (FLG-PTINF  =   1         )
               MOVE    1                   TO  FLG-OK
               IF      WRK-ORC-DBPATH      =   "key5"
                   IF     (SPA-GMN-KSEX    NOT =   ZERO  )  AND
                          (SPA-GMN-KSEX    NOT =   PTINF-SEX)
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
                   IF     (SPA-NAI-KBIRTHDAY   NOT =   SPACE )  AND
                          (SPA-NAI-KBIRTHDAY(1:SPA-NAI-KBMAX)
                                           NOT =   PTINF-BIRTHDAY
                                                     (1:SPA-NAI-KBMAX))
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               END-IF
               MOVE    SPACE               TO  WRK-HKNCOMBI
      *        病室編集
               PERFORM 20111-NYUINCHK-SEC
      *
               IF      FLG-OK          =   1
      *
                   ADD     1               TO  IDX2
                   IF      IDX2            >=  WRK-STR
                       PERFORM 2012-PTINF-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 810-PTINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      ***  IF     (SPA-GMN-NEXT    =   1    )  AND
      *           (FLG-KNJHEN      =   ZERO )
      *        MOVE    "0003"          TO  SPA-ERRCD
      **** END-IF
      *
           .
       2011-KNJKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       2012-PTINF-HENSYU-SEC          SECTION.
      *
           ADD     1                   TO  IDX
      *
           IF      IDX                 >   100
               MOVE    1                   TO  SPA-GMN-NEXT
           ELSE
               MOVE    IDX                 TO  SPA-GMN-NO  (IDX)
      *
      *    患者番号変換より、患者IDより患者NOを取得する
      *
               MOVE    PTINF-PTID          TO  SPA-NAI-PTID  (IDX)
      *
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    PTINF-PTID          TO  SPTID-PTID
               CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                               SPA-AREA
               IF      SPTID-RC        NOT =   ZERO
                   MOVE    SPACE           TO  SPA-GMN-PTNUM  (IDX)
               ELSE
                   MOVE    SPTID-PTNUM     TO  SPA-GMN-PTNUM  (IDX)
               END-IF
      *
               MOVE    PTINF-NAME          TO  SPA-GMN-NAME   (IDX)
               MOVE    PTINF-SEX           TO  SPA-NAI-SEX    (IDX)
               EVALUATE    SPA-NAI-SEX (IDX)
                   WHEN    "1"
                       MOVE    "男"        TO  SPA-GMN-SEX (IDX)
                   WHEN    "2"
                       MOVE    "女"        TO  SPA-GMN-SEX (IDX)
               END-EVALUATE  
               MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTH  (IDX)
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTH  (IDX)
      *****    最終受診日セット
      *        IF      FLG-NYUIN           =   ZERO
      *            PERFORM 20121-JYURRK-HEN-SEC
      *****    END-IF
               MOVE    WRK-LSTYMDW        TO  SPA-GMN-RRKYMD (IDX)
      *        診療科編集
               SET     SRYKA-IDX           TO  1
               SEARCH      TBL-SRYKA-OCC   VARYING   SRYKA-IDX
                   AT  END
                       MOVE    SPACE           TO  SPA-GMN-SRYKA (IDX)
                   WHEN    WRK-SRYKA       =   TBL-SRYKA (SRYKA-IDX)
                       MOVE    TBL-SRYKAMEI (SRYKA-IDX)
                                                TO  SPA-GMN-SRYKA (IDX)
               END-SEARCH
      *        病室
               MOVE    WRK-BRMNUM          TO  SPA-GMN-BRMNUM (IDX)
      *        年齢計算
               PERFORM 20122-NENREI-KEI-SEC
      *        保険表示
      ******   PERFORM 20123-HKNCOMBI-MEI-SEC
               MOVE    WRK-HKNCOMBI-MEI    TO  SPA-GMN-HKNCOMBI (IDX)
               MOVE    WRK-HONKZKKBN-MEI   TO  SPA-GMN-HONKZKKBN (IDX)
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
           END-IF
           .
       2012-PTINF-HENSYU-EXT.
           EXIT.
           .
      *****************************************************************
      *    最終受診日セット処理
      *****************************************************************
       20121-JYURRK-HEN-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-LSTYMD
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
      *****MOVE    PTINF-HOSPID        TO  JYURRK-HOSPID
           MOVE    "2"                 TO  JYURRK-NYUGAIKBN
           MOVE    PTINF-PTID          TO  JYURRK-PTID
           MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
      *
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-SRYYMD       TO  WRK-LSTYMD
               MOVE    JYURRK-SRYKA        TO  WRK-SRYKA
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
           END-IF
      *
           IF      WRK-LSTYMD          =   SPACE
      *        診療科履歴から
               PERFORM 201211-SRYKARRK-HEN-SEC
           END-IF
           IF      WRK-LSTYMD      NOT =   SPACE
               MOVE    WRK-LSTYMD          TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDX)
           END-IF
      *
           .
       20121-JYURRK-HEN-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    診療科履歴から編集処理
      *****************************************************************
       201211-SRYKARRK-HEN-SEC               SECTION.
      *    診療科歴を検索
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                  SRYKARRK-REC
      *****MOVE    PTINF-HOSPID        TO  KARRK-HOSPID
           MOVE    PTINF-PTID          TO  KARRK-PTID
      *
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                  SRYKARRK-REC
           END-IF
           PERFORM UNTIL       FLG-SRYKARRK    =   1
      *        最終受診日
               IF     (KARRK-SRYKA     NOT =   SPACE      ) AND
                      (KARRK-LASTYMD   NOT =   SPACE      ) AND
                      (KARRK-LASTYMD       >=  WRK-LSTYMD )
                   MOVE    KARRK-LASTYMD       TO  WRK-LSTYMD
                   MOVE    KARRK-SRYKA         TO  WRK-SRYKA
               END-IF
      *
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       201211-SRYKARRK-HEN-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    病室編集処理
      *****************************************************************
       20111-NYUINCHK-SEC               SECTION.
      *
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    PTINF-PTID          TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
           IF      ORCSLASTYMD-BRMNUM  =   SPACE
               MOVE    ZERO                TO  FLG-NYUIN
           ELSE
               MOVE    1                   TO  FLG-NYUIN
           END-IF
           MOVE    ORCSLASTYMD-BRMNUM      TO  WRK-BRMNUM
           MOVE    ORCSLASTYMD-SRYKA       TO  WRK-SRYKA
           MOVE    ORCSLASTYMD-LASTYMDW    TO  WRK-LSTYMDW
           MOVE    ORCSLASTYMD-HKNCOMBI-MEI    TO  WRK-HKNCOMBI-MEI
           MOVE    ORCSLASTYMD-HONKZKKBN   TO  WRK-HONKZKKBN-MEI
      *
           EVALUATE    SPA-GMN-KNYUGAIKBN
               WHEN    "1"
                   IF      FLG-NYUIN       =   ZERO
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               WHEN    "2"
                   IF      FLG-NYUIN       =   1
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
           END-EVALUATE
      *TEST
      *    以下削除
           GO      TO  20111-NYUINCHK-EXT.
      *    病室編集
           MOVE    ZERO                TO  FLG-NYUIN
      *
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
      *****MOVE    SPA-HOSPID          TO  PTNYUINRRK-HOSPID
           MOVE    PTINF-PTID          TO  PTNYUINRRK-PTID
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-SYSYMD          TO  PTNYUINRRK-TAIINYMD
      *
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    最初の１件目を対象とする
           IF      FLG-PTNYUINRRK      =   ZERO
               MOVE    SPACE               TO  WRK-BRMNUM
               MOVE    PTNYUINRRK-BRMNUM(1:2)      TO  WRK-BRMNUM1
               MOVE    "-"                         TO  WRK-BRMNUM-H
               IF      PTNYUINRRK-BRMNUM(3:6)  NUMERIC
                   MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM-9
                   MOVE    WRK-BRMNUM-9            TO  WRK-Z6
                   MOVE    ZERO                    TO  IDX3
                   PERFORM VARYING     IDX4    FROM    1   BY  1
                           UNTIL       IDX4    >   6
                       IF      WRK-Z6-X(IDX4:1)    NOT =   SPACE
                           ADD     1               TO  IDX3
                           MOVE    WRK-Z6-X(IDX4:1)    TO  WRK-BRMNUM2
                                                              (IDX3:1)
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    PTNYUINRRK-BRMNUM(3:6)  TO  WRK-BRMNUM2
               END-IF
               MOVE    PTNYUINRRK-NYUINKA  TO  WRK-SRYKA
               MOVE    PTNYUINRRK-HKNCOMBINUM  TO  WRK-HKNCOMBI
               MOVE    1                   TO  FLG-NYUIN
           ELSE
               MOVE    SPACE               TO  WRK-BRMNUM
               MOVE    SPACE               TO  WRK-SRYKA
               MOVE    SPACE               TO  WRK-HKNCOMBI
               MOVE    ZERO                TO  FLG-NYUIN
           END-IF
      *
           EVALUATE    SPA-GMN-KNYUGAIKBN
               WHEN    "1"
                   IF      FLG-NYUIN       =   ZERO
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
               WHEN    "2"
                   IF      FLG-NYUIN       =   1
                       MOVE    ZERO            TO  FLG-OK
                   END-IF
           END-EVALUATE
           .
       20111-NYUINCHK-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       20122-NENREI-KEI-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-NENREI
      *    年齢計算
           MOVE    PTINF-BIRTHDAY      TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                           -   WRK-MMS1
           END-IF
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    画面表示用
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI (IDX)
      *
           .
       20122-NENREI-KEI-EXT.
           EXIT.
      *****************************************************************
      *    診療科セット処理
      *****************************************************************
       20119-SRYKA-HEN-SEC               SECTION.
      *
      *    診療科セット
           INITIALIZE                  TBL-SRYKA-AREA
           SET     SRYKA-IDX           TO  ZERO
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
      *
               SET     SRYKA-IDX           TO  IDX
               MOVE    SYS-1005-KBNCD(1:2) TO  TBL-SRYKA (SRYKA-IDX)
               MOVE    SYS-1005-SRYKANAME3 TO  TBL-SRYKAMEI
                                                         (SRYKA-IDX)
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       20119-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険表示処理
      *****************************************************************
       20123-HKNCOMBI-MEI-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-HONKZKKBN-MEI
      *
           IF      WRK-HKNCOMBI    NOT =   SPACE
               PERFORM 201231-HKNCOMBI-CHK1-SEC
           END-IF
           IF      WRK-HKNCOMBI-MEI    =   SPACE
               PERFORM 201232-HKNCOMBI-CHK2-SEC
           END-IF
      *
           .
       20123-HKNCOMBI-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険表示処理
      *****************************************************************
       201231-HKNCOMBI-CHK1-SEC          SECTION.
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
      *****MOVE    PTINF-HOSPID        TO  COMB-HOSPID
           MOVE    PTINF-PTID          TO  COMB-PTID
           MOVE    WRK-HKNCOMBI        TO  COMB-HKNCOMBINUM
      *
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-HONKZKKBN-MEI
           IF     (FLG-HKNCOMBI        =   ZERO )  AND
                  (COMB-DELKBN     NOT =   "1"  )  AND
                 ((SPA-SYSYMD          >=  COMB-TEKSTYMD ) AND
                  (SPA-SYSYMD          <=  COMB-TEKEDYMD ))
               PERFORM 201231-HKNCOMBI-MEI-SEC
           END-IF
      *
           .
       201231-HKNCOMBI-CHK1-EXT.
           EXIT.
      *****************************************************************
      *    保険表示処理
      *****************************************************************
       201232-HKNCOMBI-CHK2-SEC          SECTION.
      *
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
      *****MOVE    PTINF-HOSPID        TO  COMB-HOSPID
           MOVE    PTINF-PTID          TO  COMB-PTID
      *
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    SPACE               TO  WRK-HKNCOMBI-MEI
           MOVE    SPACE               TO  WRK-HKNNUM
           MOVE    SPACE               TO  WRK-HONKZKKBN-MEI
           PERFORM
               UNTIL       FLG-HKNCOMBI        =   1
               IF     (COMB-DELKBN     NOT =   "1"  )  AND
                     ((SPA-SYSYMD          >=  COMB-TEKSTYMD ) AND
                      (SPA-SYSYMD          <=  COMB-TEKEDYMD ))
                   IF     (WRK-HKNNUM      =   SPACE )  OR
                          (COMB-HKNNUM     =   "971"  OR  "973")
                       PERFORM 201231-HKNCOMBI-MEI-SEC
                   END-IF
               END-IF
               IF      FLG-HKNCOMBI        =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 910-HKNCOMBI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       201232-HKNCOMBI-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       201231-HKNCOMBI-MEI-SEC               SECTION.
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    COMB-KOH1-TANSEIDONAME  TO  WRK-HKNCOMBI-MEI
           ELSE
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNCOMBI-MEI
               MOVE    COMB-HKNNUM             TO  WRK-HKNNUM
           END-IF
      *    本人家族
           EVALUATE    COMB-HONKZKKBN
               WHEN    "1"
                   MOVE    "本"            TO  WRK-HONKZKKBN-MEI
               WHEN    "2"
                   MOVE    "家"            TO  WRK-HONKZKKBN-MEI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-HONKZKKBN-MEI
           END-EVALUATE
      *
           .
       20123-HKNCOMBI-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索項目入力　処理
      *****************************************************************
       202-SELNO-SEC               SECTION.
      *
           MOVE    P97-SELNO           TO  SPA-GMN-SELNO
      *    選択番号チェック
           IF      SPA-GMN-SELNO       =   ZERO
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO              202-SELNO-EXT
           END-IF
           IF      SPA-GMN-SELNO       =   ZERO   OR
                                       >   SPA-GMN-MAX
               MOVE    4               TO  SPA-GMN-CUR
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *H25.6
      *    予約日検索のみ、排他チェックなし
           IF     (SPA-MOTOPG          =   "Y05" )
             OR   (SPA-NAI-MOTOPG      =   "Y05" )
               PERFORM 2021-SPA-OKSET-SEC
               PERFORM 210-BACK
               GO      TO      202-SELNO-EXT
           END-IF
      *
      *    排他制御チェック
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-SELNO       TO  IDX
           MOVE    SPA-NAI-PTID  (IDX) TO  SLOCK-PTID
           IF  SPA-MOTOPG              =   SPACE
               MOVE    SPA-NAI-MOTOPG      TO  SPA-MOTOPG
               CALL    "ORCSLOCK"          USING   MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
               MOVE    SPACE               TO  SPA-MOTOPG
           ELSE
               CALL "ORCSLOCK"             USING   MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
           END-IF
           IF      SLOCK-RETURN        NOT =   ZERO
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
      *    排他中で更新のみの時
           IF      SPA-LOCK            =   1
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *        ＯＫの時、ＳＰＡ確定後、元の画面へ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2021-SPA-OKSET-SEC
               PERFORM 210-BACK
           END-IF
           .
       202-SELNO-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面選択入力　処理
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX
              IF      P97-SELECT(IDX)        =  "T"
                  MOVE    SPA-GMN-NO (IDX)     TO  SPA-GMN-SELNO
              END-IF
           END-PERFORM
      *
      *    選択番号チェック
           IF      SPA-GMN-SELNO       =   ZERO   OR
                                       >   SPA-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
      *H25.6
      *    予約日検索のみ、排他チェックなし
           IF     (SPA-MOTOPG          =   "Y05" )
             OR   (SPA-NAI-MOTOPG      =   "Y05" )
               PERFORM 2021-SPA-OKSET-SEC
               PERFORM 210-BACK
               GO      TO      203-SELNO-EXT
           END-IF
      *
      *    排他制御チェック
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-SELNO       TO  IDX
           MOVE    SPA-NAI-PTID  (IDX) TO  SLOCK-PTID
           IF  SPA-MOTOPG              =  SPACE
               MOVE    SPA-NAI-MOTOPG      TO  SPA-MOTOPG
               CALL "ORCSLOCK"             USING   MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
               MOVE    SPACE               TO  SPA-MOTOPG
           ELSE
               CALL "ORCSLOCK"             USING   MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
           END-IF
           IF      SLOCK-RETURN      NOT =   ZERO
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
      *    排他中で更新のみの時
           IF      SPA-LOCK            =   1
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
      *    ＯＫの時、ＳＰＡ確定後、元の画面へ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 2021-SPA-OKSET-SEC
               PERFORM 210-BACK
           END-IF
           .
       203-SELNO-EXT.
           EXIT.
      *
      *****************************************************************
      *   画面移行用のＳＰＡ編集
      *****************************************************************
       2021-SPA-OKSET-SEC          SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
           IF      SPA-GMN-SELNO       =   ZERO
               MOVE    SPA-GMN-NAMEKEN     TO  SPA-NAME
           ELSE
               MOVE    SPA-GMN-SELNO       TO  IDX
               MOVE    SPA-NAI-PTID  (IDX) TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM (IDX) TO  SPA-PTNUM
               MOVE    SPA-GMN-NAME  (IDX) TO  SPA-NAME
               MOVE    SPA-GMN-SEX   (IDX) TO  SPA-SEX
               MOVE    SPA-NAI-BIRTH (IDX) TO  SPA-BIRTHDAY
               MOVE    SPA-GMN-BIRTH (IDX) TO  SPA-BIRTHDAYW
           END-IF
      *
           .
       2021-SPA-OKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-NAI-MOTOPG      TO  SPA-SAKIPG
           MOVE    "P97"               TO  SPA-MOTOPG
           IF      SPA-SAKIPG          =   SPACE  OR  "P97" 
               MOVE   "M01"            TO  SPA-SAKIPG
           END-IF
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-NEXT        =   ZERO
               GO      TO      440-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
      *    検索処理
           PERFORM 2011-KNJKEN-HEN-SEC
      *
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *    西暦和暦変更
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    ALL "9"             TO  WRK-WYMD
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-GENGO      TO  WRK-WYMD(1:1)
               MOVE    LNK-DAY1-YMD(3:6)   TO  WRK-WYMD(2:6)
           END-IF
      *
           .
       700-SEIWA-HEN-EXT.
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
               PERFORM 502-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *----(01.00.01) LINE DEL START ----------------------------------
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *----(01.00.01) LINE DEL END ------------------------------------
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "P97"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P97
           INITIALIZE                      P97
      *
           MOVE    SPA-GMN-NAMEKEN     TO  P97-NAMEKEN
           MOVE    SPA-GMN-KBIRTHDAY   TO  P97-KBIRTHDAY
           MOVE    SPA-GMN-KSEX-G      TO  P97-KSEX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   3
               MOVE    SPA-GMN-KSEX-LIST (IDX) TO  P97-KSEX-ITEM(IDX)
           END-PERFORM
           MOVE    3                   TO  P97-KSEX-COUNT
      *
           MOVE    SPA-GMN-KNYUGAIKBN-G    TO  P97-KNYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-NYUGAI-MAX
               MOVE    SPA-GMN-KNYUGAIKBN-LIST (IDX)
                                       TO  P97-KNYUGAIKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-NYUGAI-MAX      TO  P97-KNYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-SELNO       TO  P97-SELNO
      *
           MOVE    ZERO                TO  P97-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-NO    (IDX)     TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  P97-NO     (IDX)
               MOVE    SPA-GMN-PTNUM (IDX)     TO  P97-PTNUM  (IDX)
               MOVE    SPA-GMN-NAME  (IDX)     TO  P97-NAME   (IDX)
               MOVE    SPA-GMN-SEX   (IDX)     TO  P97-SEX    (IDX)
               MOVE    SPA-GMN-BIRTH (IDX)     TO  P97-BIRTH  (IDX)
               MOVE    SPA-GMN-NENREI(IDX)     TO  P97-NENREI (IDX)
               MOVE    SPA-GMN-RRKYMD(IDX)     TO  P97-RRKYMD (IDX)
               MOVE    SPA-GMN-SRYKA (IDX)     TO  P97-SRYKA  (IDX)
               MOVE    SPA-GMN-HKNCOMBI(IDX)   TO  P97-HKNCOMBI (IDX)
               MOVE    SPA-GMN-HONKZKKBN(IDX)  TO  P97-HONKZKKBN (IDX)
               MOVE    SPA-GMN-BRMNUM(IDX)     TO  P97-BRMNUM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  P97-COUNT
      *
      *    次頁あり
           IF      SPA-GMN-NEXT        =   1
               MOVE    "［次頁有り］"      TO  P97-NEXT-VALUE
               MOVE    "red"               TO  P97-NEXT-STYLE
           ELSE
               MOVE    SPACE               TO  P97-NEXT-VALUE
               MOVE    "black"             TO  P97-NEXT-STYLE
           END-IF
      *
           PERFORM 5011-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5011-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "NAMEKEN"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "KBIRTHDAY"     TO  MCP-WIDGET
               WHEN    03
                   MOVE    "KSEX"          TO  MCP-WIDGET
               WHEN    05
                   MOVE    "KNYUGAIKBN"    TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SELNO"         TO  MCP-WIDGET
           END-EVALUATE
          .
       5011-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       502-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "生年月日入力エラー"
                                                TO  SPA-ERRMSG
                WHEN    "0002"
                    MOVE    "選択番号が有りません"
                                                TO  SPA-ERRMSG
                WHEN    "0003"
                    MOVE    "対象者が１００人以上います"
                                                TO  SPA-ERRMSG
                WHEN    "0004"
                    MOVE    "性別のみの検索はできません。"
                                                TO  SPA-ERRMSG
                WHEN    "0005"
                    MOVE    "入外区分のみの検索はできません。"
                                                TO  SPA-ERRMSG
                WHEN    "9999"
                    MOVE    "他端末で使用中です"
                                                TO  SPA-ERRMSG
                    MOVE   SLOCK-MSG 
                                                TO  SPA-ERRMSG(19:)
                WHEN    OTHER
                    MOVE    SPA-ERRCD           TO  SPA-ERRMSG
            END-EVALUATE
      *
           MOVE    SPACE                TO  P97ERR
           MOVE    SPA-ERRCD            TO  P97ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  P97ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "P97"                TO  SPA-MOTOPG
           MOVE    "P97ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "P97ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       502-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       810-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       810-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                   USING
                                                    MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
