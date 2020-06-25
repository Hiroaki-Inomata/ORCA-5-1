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
       PROGRAM-ID.                 ORCGI48.
      ********************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院会計照会 
      *  コンポーネント名  : 診療行為一覧選択サブ（Ｉ４８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02.09.19    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    NACL-多々納  04.05.06  選択内容変更他
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    会計照会共通パラメタ
           COPY    "I4COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "I44SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
      *
           03  FLG-SYURYOU         PIC 9(01).
           03  FLG-MAX             PIC 9(01).
      *
           03  FLG-HENSYU          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
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
           03  IDY                 PIC 9(04).
      *
           03  IDX-1               PIC 9(01).
      *
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXC                PIC 9(04).
      *
           03  ID1                 PIC 9(04).
           03  ID2                 PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
      *
           03  IDX-KAI             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SEYMD.
               05  WRK-SEYY        PIC 9(04).
               05  WRK-SEMM        PIC 9(02).
               05  WRK-SEDD        PIC 9(02).
      *
           03  WRK-CNT-ENT         PIC 9(04).
           03  WRK-MOJI            PIC 9(04).
           03  WRK-MAP-INDEX       PIC 9(03).
           03  WRK-SELNO           PIC 9(04).
           03  WRK-GMN-SELNO       PIC 9(03).
           03  WRK-NOZ             PIC ZZ9.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS      PIC ZZZZZZ9.99.
      *
           03  WRK-INPUTCD         PIC X(22).
           03  WRK-GRP-MEI         PIC X(40).
           03  WRK-GRP-MEI-X       PIC X(40).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-KETA            PIC 9(04).
      *
           03  WRK-HEN-NAME        PIC X(70).
           03  WRK-KENSA-HEN       PIC X(02).
      *
           03  WRK-I48-ROW         PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *
           03  WRK-KEY-INPUT       PIC X(22).
      *
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    検索記号設定
           COPY    "CPORCSADDSIGN.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA32SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-I44
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD.
           MOVE    ZERO                TO  FLG-END.
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK.
           MOVE    SPA-AREA            TO  SPA-COMMON.
           MOVE    SPA-I44             TO  SPA-FREE.
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  CNT-AREA.
           INITIALIZE                  WRK-AREA.
      *    エラー画面より
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "I48"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
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
           MOVE    SPA-I48-INPUTCD     TO  SPA-I48-GMN-INPUT.
           MOVE    SPA-I48-INPUTCD     TO  WRK-INPUTCD.
           MOVE    ZERO                TO  SPA-I48-NEXT.
      *
           IF      (SPA-I48-GMN-INPUT  =   SPACE )  AND
                   (SPA-I48-GMN-SYORI  =   SPACE )
               CONTINUE
           ELSE
               PERFORM                     310-SPA-SET-SEC
           END-IF
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-I48-GMN-TBL.
           MOVE    ZERO                TO  SPA-I48-GMN-MAX.
           MOVE    ZERO                TO  FLG-MAX
      *
           IF       SPA-I48-TYPE         =   "1"
               IF      SPA-I48-GMN-INPUT(1:1)  =   "P"  OR  "S"
                   MOVE    6                   TO  SPA-I48-GMN-CUR
                   MOVE    "0001"              TO  SPA-ERRCD
                   GO      TO      310-SPA-SET-EXT
               ELSE
                   PERFORM 3101-INPUTCD-SET-SEC
               END-IF
           END-IF.
           IF     ( SPA-I48-GMN-MAX    =   ZERO  )
               PERFORM 3102-TENSU-SET-SEC
      *        点数マスタ検索時、自院がゼロ件の時、全体を検索
               IF     (SPA-I48-GMN-ALL     =   SPACE)  AND
                      (SPA-I48-GMN-MAX     =   ZERO)
                   MOVE    "1"             TO  SPA-I48-GMN-ALL
                   PERFORM 3102-TENSU-SET-SEC
               END-IF
           END-IF.
      *
           IF      SPA-I48-GMN-MAX     =   ZERO
               MOVE    6                   TO  SPA-I48-GMN-CUR
               MOVE    "0004"              TO  SPA-ERRCD
           ELSE
               MOVE    1                   TO  SPA-I48-GMN-CUR
               IF      FLG-MAX         NOT =   ZERO
                   MOVE    "0005"              TO  SPA-ERRCD
               ELSE
                   MOVE    ZERO                TO  SPA-I48-NEXT
               END-IF
           END-IF.
      *
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コードマスタ−より編集
      *****************************************************************
       3101-INPUTCD-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-ENT.
      *
           MOVE    SPACE               TO  INPUTCD-REC.
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM.
           MOVE    SPA-I48-CDSYUBETU   TO  ICD-CDSYU.
           MOVE    SPA-I48-GMN-INPUT   TO  ICD-INPUTCD.
      *    入力コードの桁数計算
           MOVE    ZERO                TO  WRK-KETA.
           PERFORM VARYING     IDX-K   FROM    20   BY  -1
                   UNTIL       IDX-K   <   1
               IF      SPA-I48-GMN-INPUT(IDX-K:1)  NOT =   SPACE
                   MOVE    IDX-K           TO  WRK-KETA
                   MOVE    1               TO  IDX-K
               END-IF
           END-PERFORM.
           IF      WRK-KETA            =   20
               MOVE    "key2"              TO  WRK-PATH
           ELSE
               COMPUTE IDX-K               =   WRK-KETA  +  1
               MOVE    "%"                 TO  ICD-INPUTCD (IDX-K:1)
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
           END-IF.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   200 )   OR
                              (FLG-INPUTCD     =   1   )
      *
               PERFORM 31011-INPUTCD-CHK-SEC
               IF      FLG-OK              =   1
                   IF      ICD-SRYCD(1:1)      =   "S"  OR  "P"
                       MOVE    "0001"          TO  SPA-ERRCD
                       MOVE    1               TO  FLG-INPUTCD
                   ELSE
      *                点数マスタより編集
                       INITIALIZE              TNS-TENSU-REC
                       MOVE    ICD-SRYCD       TO  TNS-SRYCD
                       MOVE    "key"           TO  WRK-PATH
                       PERFORM 910-TENSU-SELECT-SEC
                       MOVE    "tbl_tensu"         TO  MCP-TABLE
                       MOVE    WRK-PATH            TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
      *
                       IF      FLG-TENSU           =   ZERO
      *
                           ADD     1               TO  WRK-CNT-ENT
                           MOVE    IDX             TO  SPA-I48-GMN-NO
                                                                (IDX)
                           MOVE    ICD-INPUTCD     TO
                                                   SPA-I48-GMN-INPUTCD
                                                                (IDX)
                           MOVE    ICD-SRYCD       TO  SPA-I48-GMN-SRYCD
                                                                (IDX)
                           PERFORM 31021-TENSU-CHK-SEC
                           IF      FLG-CHK         =   ZERO
                               MOVE    TNS-NAME
                                               TO  SPA-I48-GMN-NAME
                                                                (IDX)
                               MOVE    TNS-TEN
                                               TO  SPA-I48-GMN-KISOTEN
                                                                (IDX)
      *                        薬剤・器材の時単位
                               IF      TNS-SRYCD (1:1) =   "6"  OR  "7"
                                   MOVE    TNS-TANINAME    TO
                                               SPA-I48-GMN-TANI(IDX)
                               END-IF
      *
      *                        診療行為で老人の時、再編集
                               IF     (TNS-SRYCD(1:1)      =   "1") AND
                                      (TNS-ROUTEKKBN       =   2  )
                                   MOVE    SPACE   TO  SPA-I48-GMN-NAME
                                                                (IDX)
                                   STRING  "［老］"
                                     TNS-NAME      DELIMITED   BY  SIZE
                                     INTO        SPA-I48-GMN-NAME(IDX)
                                   END-STRING
                               END-IF
                           ELSE
                               INITIALIZE          SPA-I48-GMN-TBLREC
                                                                (IDX)
                               COMPUTE IDX             =   IDX    -   1
                               COMPUTE WRK-CNT-ENT     =   WRK-CNT-ENT
                                                       -   1
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM                     930-INPUTCD-READ-SEC
           END-PERFORM.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-I48-GMN-MAX.
      *
       3101-INPUTCD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一コードチェック処理
      *****************************************************************
       31011-INPUTCD-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-OK.
           MOVE    ZERO                TO  FLG-ERR.
      *
           PERFORM VARYING     IDXC    FROM    1   BY  1
               UNTIL      (IDXC        >   200 )   OR
                          (SPA-I48-GMN-SRYCD(IDXC)
                                       =   SPACE)   OR
                          (FLG-ERR     =   1   )
               IF      ICD-SRYCD       =   SPA-I48-GMN-SRYCD(IDXC)
                   MOVE    1               TO  FLG-ERR
               END-IF
           END-PERFORM.
           IF      FLG-ERR             =   ZERO
               MOVE    1                   TO  FLG-OK
           END-IF.
      *
       31011-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ−より編集
      *****************************************************************
       3102-TENSU-SET-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-ENT.
           MOVE    ZERO                TO  FLG-MAX.
           MOVE    SPACE               TO  WRK-KEY-INPUT
      *
      *    ユーザー登録分
           IF      SPA-I48-GMN-SYORI   =   "U"
               CONTINUE
           ELSE
      *
               IF      SPA-I48-GMN-ALL     =   SPACE
      *        自院分のみ
                   MOVE    "key7j"         TO  WRK-PATH
               ELSE
                   MOVE    "key7"          TO  WRK-PATH
               END-IF
      *
      *        検索キー編集
               INITIALIZE                  ORCSADDSIGNAREA
               MOVE    "1"                 TO  SADDSIGN-KBN
               MOVE    SPA-I48-GMN-INPUT   TO  SADDSIGN-STR
               CALL    "ORCSADDSIGN"       USING
                                           ORCSADDSIGNAREA
               MOVE    SADDSIGN-STR        TO  WRK-KEY-INPUT
      *
          END-IF.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-KEY-INPUT       TO  TNS-NAME
           MOVE    WRK-KEY-INPUT       TO  TNS-KANANAME
      *    ユーザー登録分
           IF      SPA-I48-GMN-SYORI   =   "U"
               PERFORM 31021-ETC-PATH-SET-SEC
           ELSE
               IF     (SPA-I48-GMN-SYORI       =   "8"  )  AND
                      (SPA-I48-GMN-INPUT       =   SPACE)
                   MOVE    ZERO                TO  SPA-I48-GMN-MAX
                   GO      TO      3102-TENSU-SET-EXT
               END-IF
           END-IF.
      *
           IF      SPA-I48-GMN-INPUT(1:2)   =   "//"
               MOVE    "key10m"             TO  WRK-PATH
               MOVE    "100000000"          TO  TNS-SRYCD
               MOVE    "899999999"          TO  TNSUP-SRYCD
               MOVE    "90"                 TO  TNSXX-STSRYKBN
               MOVE    "99"                 TO  TNSXX-EDSRYKBN
           END-IF.
      *
           PERFORM 910-TENSU-SELECT-SEC
      *
           MOVE    ZERO                TO  IDX2.
           MOVE    ZERO                TO  IDX.
           PERFORM
               UNTIL      (IDX         >    200)   OR
                          (FLG-TENSU   =   1   )
      *
      *        点数マスタチェック
               PERFORM 31021-TENSU-CHK-SEC
      *
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  IDX2
      *            次ページの時、表示分は読み飛ばし
                   IF      IDX2            >=  SPA-I48-NEXT
                       PERFORM 31021-TENSU-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    WRK-CNT-ENT         TO  SPA-I48-GMN-MAX.
      *
      *    労災分表示の時、残りのマスタを表示する
           EVALUATE    SPA-I48-GMN-SYORI3
               WHEN    "U2"
                   PERFORM 31022-RSI-HENSYU-SEC
           END-EVALUATE.
      *
      *    点数マスタで対象が有った時、タイプを変更する
           IF      SPA-I48-GMN-MAX     >   ZERO
               MOVE    "2"             TO  SPA-I48-TYPE
           END-IF.
      *
       3102-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザー登録分処理
      *****************************************************************
       31021-ETC-PATH-SET-SEC          SECTION.
      *
           EVALUATE    SPA-I48-GMN-SYORI3
               WHEN    "U1"
      *            自院・コメント
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "1"                  TO  SPA-I48-GMN-ALL
                   MOVE    "800000000"          TO  TNS-SRYCD
                   MOVE    "899999999"          TO  TNSUP-SRYCD
               WHEN    "U2"
      *            労災診察
                   MOVE    "key12s"             TO  WRK-PATH
                   MOVE    "100000000"          TO  TNS-SRYCD
                   MOVE    "109999999"          TO  TNSUP-SRYCD
                   MOVE    "1"                  TO  SPA-I48-GMN-ALL
           END-EVALUATE.
      *
       31021-ETC-PATH-SET-EXT.
           EXIT.
      *****************************************************************
      *    労災分表示処理
      *****************************************************************
       31022-RSI-HENSYU-SEC                  SECTION.
      *
           PERFORM VARYING     IDX-KAI     FROM    1   BY  1
                   UNTIL       IDX-KAI     >   4
               INITIALIZE                      TNS-TENSU-REC
               EVALUATE    IDX-KAI
                   WHEN    1
      *            器材
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "788888000"          TO  TNS-SRYCD
                       MOVE    "788888999"          TO  TNSUP-SRYCD
                   WHEN    2
      *            コメント１
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "820800000"          TO  TNS-SRYCD
                       MOVE    "820899999"          TO  TNSUP-SRYCD
                   WHEN    3
      *            コメント２
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "830800000"          TO  TNS-SRYCD
                       MOVE    "830899999"          TO  TNSUP-SRYCD
                   WHEN    4
      *            コメント３
                       MOVE    "key12s"             TO  WRK-PATH
                       MOVE    "840800000"          TO  TNS-SRYCD
                       MOVE    "840899999"          TO  TNSUP-SRYCD
               END-EVALUATE
      *
               PERFORM 910-TENSU-SELECT-SEC
      *
               PERFORM
                    UNTIL      (IDX         >    200 )   OR
                               (FLG-TENSU   =   1   )
      *
      *            点数マスタチェック
                   PERFORM 31021-TENSU-CHK-SEC
      *
                   IF      FLG-CHK             =   ZERO
                       ADD     1               TO  IDX2
      *                次ページの時、表示分は読み飛ばし
                       IF      IDX2            >=  SPA-I48-NEXT
                           PERFORM 31021-TENSU-HEN-SEC
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PATH            TO  MCP-PATHNAME
                   PERFORM                     910-TENSU-READ-SEC
               END-PERFORM
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-PERFORM.
      *
           MOVE    WRK-CNT-ENT         TO  SPA-I48-GMN-MAX.
      *
       31022-RSI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタチェック処理
      *****************************************************************
       31021-TENSU-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK.
      *
      *    病院診療所区分判定
           IF      (TNS-HOSPSRYKBN     =   ZERO)        OR
                   (TNS-HOSPSRYKBN     =   SPA-HOSPSBT)
      *H30.4
      *       短期滞在手術基本２（病院・診療所）
              OR   (TNS-HOSPSRYKBN     =   3   )
      *       短期滞在手術基本３（病院のみ）
              OR  ((TNS-HOSPSRYKBN     =   4   )   AND
                   (SPA-HOSPSBT        =   1   ))
               CONTINUE
           ELSE
               MOVE   1                TO  FLG-CHK
           END-IF.
      *
      *    老人区分判定
           IF      (TNS-ROUTEKKBN      =   ZERO)   OR
                   ((TNS-ROUTEKKBN      =   1  )  AND
                    (SPA-ROUJIN         =   ZERO))  OR
                   ((TNS-ROUTEKKBN      =   2  )  AND
                    (SPA-ROUJIN         =   1  ))
               CONTINUE
           ELSE
               MOVE   1                TO  FLG-CHK
           END-IF.
      *
      *    入外区分
           IF      (TNS-NYUGAITEKKBN      =   ZERO)          OR
                   (TNS-NYUGAITEKKBN      =   SPA-NYUGAIKBN)
               CONTINUE
           ELSE
               MOVE   1                TO  FLG-CHK
           END-IF.
      *
      *    診療区分
           IF      (TNS-SRYKBN            =   "90")  OR
                   (TNS-SRYKBN            =   "99")
               CONTINUE
           ELSE
               IF      SPA-I48-GMN-SYORI3    =   "U2"
                   CONTINUE
               ELSE
                   IF      (SPA-I48-GMN-SYORI3    =   "U1")  AND
                           (TNS-SRYCD (4:1)   NOT =   "8")
                       CONTINUE
                   ELSE
                       MOVE   1                TO  FLG-CHK
                   END-IF
               END-IF
           END-IF.
      *
       31021-TENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       31021-TENSU-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDX.
      *    ２００件オーバーチェック
           IF      IDX                 >   200
               MOVE    1                   TO  FLG-MAX
               MOVE    IDX2                TO  SPA-I48-NEXT
               GO      TO      31021-TENSU-HEN-EXT
           END-IF.
      *
           ADD     1                   TO  WRK-CNT-ENT.
           MOVE    IDX2                TO  SPA-I48-GMN-NO   (IDX).
           MOVE    TNS-SRYCD           TO  SPA-I48-GMN-INPUTCD
                                                            (IDX).
           MOVE    TNS-SRYCD           TO  SPA-I48-GMN-SRYCD(IDX).
           MOVE    TNS-NAME            TO  SPA-I48-GMN-NAME
                                                             (IDX).
           MOVE    TNS-TEN             TO  SPA-I48-GMN-KISOTEN
                                                            (IDX).
      *     薬剤・器材の時単位
           IF      TNS-SRYCD (1:1)     =   "6"  OR  "7"
               MOVE    TNS-TANINAME        TO  SPA-I48-GMN-TANI(IDX)
           END-IF.
      *
      *    診療行為で老人の時、再編集
           IF     (TNS-ROUTEKKBN       =   2   )
               MOVE    SPACE               TO  SPA-I48-GMN-NAME(IDX)
               STRING  "［老］"
                       TNS-NAME            DELIMITED   BY  SIZE
                       INTO                    SPA-I48-GMN-NAME(IDX)
               END-STRING
           END-IF
      *
           .
       31021-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-I48-GMN-MAX     =   ZERO )
               MOVE    ZERO                TO  WRK-I48-ROW
           ELSE
               MOVE    I48-ROW             TO  WRK-I48-ROW
           END-IF
      *
           MOVE    SPACE               TO  I48.
           INITIALIZE                      I48.
      *    コラムリストの位置指定
           MOVE    ZERO                TO  I48-ROW
           MOVE    ZERO                TO  I48-ROWATTR
           IF      WRK-I48-ROW     NOT =   ZERO
               MOVE    WRK-I48-ROW         TO  I48-ROW
           END-IF
      *
           MOVE    SPA-I48-GMN-INPUT   TO  I48-INPUT.
      *
           IF     (SPA-I48-TYPE        =   "1" )  AND
                  (SPA-I48-CDSYUBETU   =   "6" )
               MOVE    SPACE               TO  SPA-I48-GMN-SYORI
           END-IF.
           IF     (SPA-I48-TYPE        =   "1" )  AND
                  (SPA-I48-CDSYUBETU   =   "A" )
               MOVE    SPACE               TO  SPA-I48-GMN-SYORI
           END-IF.
      *
           EVALUATE    SPA-I48-GMN-SYORI
      *        診療行為
               WHEN    "8"
                   IF      SPA-I48-GMN-ALL     =   SPACE
                       MOVE    "診療行為（自院）" 
                                               TO  I48-SENTAKU
                   ELSE
                       MOVE    "診療行為（全体）" 
                                               TO  I48-SENTAKU
                   END-IF
      *        ユーザー登録
               WHEN    "U"
                   EVALUATE    SPA-I48-GMN-SYORI3
                       WHEN    "U1"
                           MOVE    "コメント（全体）"
                                               TO  I48-SENTAKU
                       WHEN    "U2"
                           MOVE    "労災"
                                               TO  I48-SENTAKU
                   END-EVALUATE
      *        その他
               WHEN    OTHER
                   MOVE    "入力コード全体（自院）"
                                              TO  I48-SENTAKU
           END-EVALUATE.
      *
      *    漢字入力で、入力コードで検索
           IF     (SPA-I48-TYPE        =   "1" )  AND
                  (SPA-I48-CDSYUBETU   =   "J" )
               MOVE    "入力コード全体（自院）"
                                              TO  I48-SENTAKU
           END-IF.
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-I48-GMN-MAX
               MOVE    IDX                 TO  WRK-NOZ
               MOVE    WRK-NOZ             TO  I48-NO  (IDX)
               MOVE    IDX                 TO  SPA-I48-NAI-NO(IDX)
      *
               MOVE    SPA-I48-GMN-NAME (IDX)  TO  I48-NAME (IDX)
               MOVE    SPA-I48-GMN-KISOTEN(IDX)
                                           TO  WRK-TENSUS
               MOVE    WRK-TENSUS          TO  I48-KISOTEN (IDX)
      *
               MOVE    SPA-I48-GMN-TANI (IDX)  TO  I48-TANI (IDX)
           END-PERFORM
           MOVE    SPA-I48-GMN-MAX      TO  I48-COUNT
      *
           IF      SPA-I48-NAI-SELNUM  NOT =   ZERO
               MOVE    SPA-I48-NAI-SELNUM  TO  WRK-NOZ
               MOVE    WRK-NOZ             TO  I48-SELNUM
           END-IF
      *
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-I48-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    06
                   MOVE    "INPUT"         TO  MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
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
                   MOVE    SPACE               TO  SPA-I48-SRYCD-G
                   PERFORM 210-BACK
      *        診療行為
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 280-SINRYOU-SEC
      *        全体
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 290-ALL-SEL-SEC
      *        部分
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 291-BUB-SEC
      *        後方
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 292-ATO-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-NEXT-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        キー入力
               WHEN    "ACTIVATE"      ALSO    "INPUT"
                   PERFORM 41013-INPUT-CHK-SEC
      *        番号入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-SELNUM-ALLCHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 41012-LISTSEL-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー入力　処理
      *****************************************************************
       41013-INPUT-CHK-SEC                SECTION.
      *
           MOVE    6                   TO  SPA-I48-GMN-CUR.
      *
           MOVE    I48-INPUT           TO  SPA-I48-GMN-INPUT.
           MOVE    ZERO                TO  FLG-KANJI.
           MOVE    ZERO                TO  FLG-KANA.
           MOVE    ZERO                TO  WRK-MOJI.
      *
           INITIALIZE                  ORCSKANACHKAREA.
           MOVE    "1"                 TO  KANACHK-SYORI.
           MOVE    SPA-I48-GMN-INPUT   TO  KANACHK-MAE-INPUT.
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-I48-GMN-INPUT
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX     TO  WRK-MOJI
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF.
      *
           IF     (WRK-MOJI            =   ZERO  )  OR
                  ((FLG-KANJI           =   1    ) AND
                   (FLG-KANA            =   1    ))
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO                  41013-INPUT-CHK-EXT
           END-IF.
      *
           IF      FLG-KANJI           =   1
      *        全角入力（点数マスタより）
               MOVE    "J"                 TO  SPA-I48-CDSYUBETU
               MOVE    "1"                 TO  SPA-I48-TYPE
               MOVE    SPACE               TO  SPA-I48-GMN-ALL
           ELSE
               MOVE    SPACE               TO  SPA-I48-GMN-ALL
      *        半角入力（入力セットマスタより）
               MOVE    "1"                 TO  SPA-I48-TYPE
      *
               MOVE    SPACE               TO  SPA-I48-GMN-SYORI
               IF     SPA-I48-GMN-INPUT(1:WRK-MOJI)   NUMERIC
                   EVALUATE    WRK-MOJI
                       WHEN    4
                           MOVE    "4"         TO  SPA-I48-CDSYUBETU
                       WHEN    5
                           MOVE    "5"         TO  SPA-I48-CDSYUBETU
                       WHEN    3
                       WHEN    6
                           MOVE    "6"         TO  SPA-I48-CDSYUBETU
                       WHEN    1
                           MOVE    "6"         TO  SPA-I48-CDSYUBETU
                   END-EVALUATE
               ELSE
                   MOVE    "A"             TO  SPA-I48-CDSYUBETU
               END-IF
           END-IF.
           MOVE    1                   TO  SPA-I48-GMN-CUR.
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-I48-NEXT.
           PERFORM                     310-SPA-SET-SEC.
      *
       41013-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　処理（全体）
      *****************************************************************
       41011-SELNUM-ALLCHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-HENSYU.
           PERFORM 41011-SELNUM-CHK-SEC
           .
      *
       41011-SELNUM-ALLCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号選択　処理
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    1                   TO  SPA-I48-GMN-CUR.
      *
      *    ＊ 入力時、全対象とする
           IF      I48-SELNUM          =   "*  "  OR
                                           " * "  OR
                                           "  *" 
               MOVE    "1"                 TO  SPA-I48-GMN-ALL
               INITIALIZE                  SPA-I48-NAI-SELNUM-G
               INITIALIZE                  SPA-I48-GMN-SELNUM-G
               MOVE    SPACE               TO  SPA-I48-SRYCD-G
      *        ＳＰＡセット
               MOVE    ZERO                TO  SPA-I48-NEXT
               PERFORM                     310-SPA-SET-SEC
               GO      TO      41011-SELNUM-CHK-EXT
           END-IF.
      *
           MOVE    I48-SELNUM          TO  SPA-I48-GMN-SELNUM
      *    選択番号入力
           INITIALIZE                      ORCSNUMAREA.
           MOVE    I48-SELNUM          TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-I48-GMN-CUR
               GO      TO      41011-SELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-I48-NAI-SELNUM
           END-IF.
      *    １件のみとき、１とする
           IF     (SPA-I48-GMN-MAX     =   1  )  AND
                  (IDX-1               =   1  )  AND
                  (SPA-I48-NAI-SELNUM  =  ZERO)
               MOVE    1               TO  SPA-I48-NAI-SELNUM
           END-IF.
      *
           MOVE    ZERO                TO  WRK-SELNO.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-I48-GMN-MAX
               IF      SPA-I48-NAI-NO(IDX) =   SPA-I48-NAI-SELNUM
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    200             TO  IDX
               END-IF
           END-PERFORM.
      *
           PERFORM 41012-SRYCD-SEL-SEC.
      *
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード選択処理
      *****************************************************************
       41012-SRYCD-SEL-SEC       SECTION.
      *
           MOVE    1                   TO  SPA-I48-GMN-CUR.
      *
           PERFORM 41011-SELNO-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *         戻るへ
                PERFORM 210-BACK
           END-IF.
      *
       41012-SRYCD-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO.
           MOVE    ZERO                TO  WRK-GMN-SELNO.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      I48-SELECT (IDX)    =   "T"
                   MOVE    SPA-I48-GMN-NO(IDX)  TO  WRK-GMN-SELNO
                   MOVE    IDX                  TO  WRK-SELNO
                   MOVE    200             TO  IDX
               END-IF
           END-PERFORM.
      *
           IF      WRK-SELNO           <   01  OR
                                       >   SPA-I48-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
               GO      TO              41012-LISTSEL-EXT
           END-IF.
      *
           IF      SPA-I48-NAI-SELNUM  =   ZERO
               MOVE    WRK-SELNO           TO  SPA-I48-NAI-SELNUM
               MOVE    WRK-GMN-SELNO       TO  SPA-I48-GMN-SELNUM
           END-IF
      *
           PERFORM 41012-SRYCD-SEL-SEC.
      *
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細選択編集処理
      *****************************************************************
       41011-SELNO-SEC       SECTION.
      *
           IF      WRK-SELNO           <   01  OR
                                       >   SPA-I48-GMN-MAX
               MOVE    "0002"          TO  SPA-ERRCD
               GO      TO              41011-SELNO-EXT
           END-IF.
      *
           MOVE    WRK-SELNO           TO  IDX.
      *
           MOVE    SPA-I48-GMN-SRYCD (IDX)
                                       TO  SPA-I48-SRYCD
           .
      *
       41011-SELNO-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為選択処理
      *****************************************************************
       280-SINRYOU-SEC                   SECTION.
      *
           MOVE    "8"                 TO  SPA-I48-GMN-SYORI.
           MOVE    SPACE               TO  SPA-I48-GMN-SYORI3.
           MOVE    "2"                 TO  SPA-I48-TYPE.
      *
           INITIALIZE                      SPA-I48-NAI-SELNUM-G.
           INITIALIZE                      SPA-I48-GMN-SELNUM-G.
           MOVE    SPACE               TO  SPA-I48-SRYCD-G.
           MOVE    SPACE               TO  SPA-I48-GMN-ALL.
      *    ＳＰＡセット
           MOVE    ZERO                TO  SPA-I48-NEXT.
           PERFORM                     310-SPA-SET-SEC.
      *
       280-SINRYOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体一致処理
      *****************************************************************
       290-ALL-SEL-SEC                   SECTION.
      *
      *    入力コードより編集の時、
           IF      SPA-I48-TYPE        =   "1"
               MOVE    SPACE               TO  SPA-I48-GMN-ALL
      *
               INITIALIZE                  SPA-I48-NAI-SELNUM-G
               INITIALIZE                  SPA-I48-GMN-SELNUM-G
               MOVE    SPACE               TO  SPA-I48-SRYCD-G
               MOVE    ZERO                TO  SPA-I48-NEXT
               MOVE    "2"                 TO  SPA-I48-TYPE
      *        ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
           ELSE
               MOVE    "1"                 TO  SPA-I48-GMN-ALL
      *
               INITIALIZE                  SPA-I48-NAI-SELNUM-G
               INITIALIZE                  SPA-I48-GMN-SELNUM-G
               MOVE    SPACE               TO  SPA-I48-SRYCD-G
               MOVE    ZERO                TO  SPA-I48-NEXT
      *        ＳＰＡセット
               PERFORM                     310-SPA-SET-SEC
           END-IF.
      *
       290-ALL-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    次ページ処理
      *****************************************************************
       270-NEXT-SEC                   SECTION.
      *
           IF      SPA-I48-NEXT       >   ZERO
      *        ＳＰＡセット
               PERFORM                     3102-TENSU-SET-SEC
           ELSE
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF.
      *
       270-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    部分一致処理
      *****************************************************************
       291-BUB-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-I48-CDSYUBETU   NOT =   "J"
               GO      TO      291-BUB-EXT
           END-IF.
      *
           INSPECT SPA-I48-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   22
               IF      SPA-I48-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-I48-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
           STRING  "＊"                DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                   "＊"                DELIMITED  BY  SIZE
                                       INTO    SPA-I48-GMN-INPUT
           END-STRING.
      *
           INITIALIZE                  SPA-I48-NAI-SELNUM-G.
           INITIALIZE                  SPA-I48-GMN-SELNUM-G.
           MOVE    SPACE               TO  SPA-I48-SRYCD-G.
           MOVE    ZERO                TO  SPA-I48-NEXT.
           MOVE    SPACE               TO  SPA-I48-TYPE.
      *    ＳＰＡセット
           PERFORM                     310-SPA-SET-SEC.
      *
      *
       291-BUB-EXT.
           EXIT.
      ****************************************************************
      *    後分一致処理
      *****************************************************************
       292-ATO-SEC                   SECTION.
      *
      *    全角入力時、のみ
           IF      SPA-I48-CDSYUBETU   NOT =   "J"
               GO      TO      292-ATO-EXT
           END-IF.
      *    検査以外の時のみ
           IF      SPA-I48-GMN-SYORI   =   "9"
               GO      TO      292-ATO-EXT
           END-IF.
      *
           INSPECT SPA-I48-GMN-INPUT   REPLACING  ALL "＊"  BY  "  "
           MOVE    SPACE               TO  WRK-INPUTCD
           MOVE    ZERO                TO  ID2
           PERFORM VARYING     ID1     FROM    1   BY  1
                   UNTIL       ID1     >   22
               IF      SPA-I48-GMN-INPUT(ID1:1)    NOT =   SPACE
                   ADD     1                   TO  ID2
                   MOVE    SPA-I48-GMN-INPUT(ID1:1)
                                               TO  WRK-INPUTCD(ID2:1)
               END-IF
           END-PERFORM
           STRING  "＊"                DELIMITED  BY  SIZE
                   WRK-INPUTCD         DELIMITED  BY  SPACE
                                       INTO    SPA-I48-GMN-INPUT
           END-STRING
      *
           INITIALIZE                  SPA-I48-NAI-SELNUM-G.
           INITIALIZE                  SPA-I48-GMN-SELNUM-G.
           MOVE    SPACE               TO  SPA-I48-SRYCD-G.
           MOVE    ZERO                TO  SPA-I48-NEXT.
           MOVE    SPACE               TO  SPA-I48-TYPE.
      *    ＳＰＡセット
           PERFORM                     310-SPA-SET-SEC.
      *
           .
       292-ATO-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I44"               TO  SPA-SAKIPG.
           MOVE    "I48"               TO  SPA-MOTOPG.
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "I44"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF.
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "I48"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE
                   "対象データがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE
                   "対象データが２００件以上あります。"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
                   "次ページはありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                   "ユーザー登録区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE
                   "種別の違うデータです。選択できません。"
                                       TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I48"               TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索
      *****************************************************************
       910-TENSU-SELECT-SEC         SECTION.
      *
           MOVE    SPA-I40-SRYYMD      TO  TNS-YUKOSTYMD
           MOVE    SPA-I40-SRYYMD      TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF.
      *
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF.
      *
       920-INPUTCD-NEXT-EXT.
           EXIT.
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
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
