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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGW14.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 県単独事業設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/08/30  印刷指示項目の追加
      *  01.00.02    NACL-多々納  02/11/14  一括入力 
      *  01.00.03    NACL-藤原    04/08/31  パラメタ種別、プリンタ名
      *                                     の追加
      *  01.00.04    NACL-藤原    05/02/16  標準国保レセ電総括表印刷指示
      *                                     の追加
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.08.01    NACL-藤原    16/01/18  地方公費処理スクリプトの存在
      *                                     チェック対応
      *
      *  04.09.01    NACL-藤原    16/11/18  Ｄｏｃｋｅｒ対応
      *  04.09.02    NACL-藤原    17/02/24  レポジトリ名設定時は地方公費
      *                                     処理スクリプトの存在チェック
      *                                     を行わない
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    
      *    処理スクリプトチェック用ワークファイル
           SELECT  WRK-FILE        ASSIGN  WRKPARA
                                   FILE    STATUS  IS  STS-WRK.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    処理スクリプトチェック用ワークファイル
       FD  WRK-FILE.
       01  WRK-R                   PIC X(10).
      *
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
       01  STS-AREA.
           03  STS-WRK             PIC X(02).
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W14SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDZ1                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-KBNCD           PIC 9(04).
           03  WRK-IDX2            PIC 9(02).
           03  WRK-IDX3            PIC 9(03).
           03  WRK-WIDGET.
               05  WRK-WIDGET1     PIC X(06).
               05  WRK-WIDGET2     PIC 9(03).
               05  WRK-WIDGET-FIL  PIC X(03).
           03  WRK-WIDGET-R        REDEFINES    WRK-WIDGET.
               05  WRK-WIDGET-R1   PIC X(02).
               05  WRK-WIDGET-R2   PIC 9(02).
               05  WRK-WIDGET-FIL  PIC X(08).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-GMN-CUR         PIC 9(02).
           03  WRK-MCP-WIDGET      PIC X(64).
      *
       01  WRKPARA                 PIC X(512).
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
       01  WRK-CONS-LINE-MAX       PIC 9(02)   VALUE   25.              
      *
      *    県単独事業用シェル
       01  WRK-SHELL-KENTAN.
           03  FILLER             PIC X(15) VALUE
                                             "scripts/kentan/".
           03  WRK-SHELL-SHELL    PIC X(30).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    県単独事業情報コード
           COPY    "CPSK3003.INC".
      *    県単独名
           COPY    "CPSK3004.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W14FREE
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
           MOVE    SPA-W14FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF 
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "W14    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "W141"
               WHEN    "W142"
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
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
      *
           INITIALIZE              SPA-W14-AREA
      *
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
           MOVE    "0"                 TO  SPA-GMN-PARAKBNL    (1)
           MOVE    "なし"              TO  SPA-GMN-PARAKBNMEIL (1)  
           MOVE    "1"                 TO  SPA-GMN-PARAKBNL    (2)
           MOVE    "保険者番号"        TO  SPA-GMN-PARAKBNMEIL (2)  
           MOVE    "2"                 TO  SPA-GMN-PARAKBNL    (3)
           MOVE    "患者番号"          TO  SPA-GMN-PARAKBNMEIL (3)
           MOVE    3                   TO  SPA-GMN-PARAKBN-MAX  
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-CONS-LINE-MAX
               MOVE    IDX             TO  SPA-GMN-TCD    (IDX)
               MOVE    "F"             TO  SPA-GMN-CHKFLG (IDX 1)
                                           SPA-GMN-CHKFLG (IDX 2)
                                           SPA-GMN-CHKFLG (IDX 3)
                                           SPA-GMN-CHKFLG (IDX 4)
           END-PERFORM
           MOVE    "T"                 TO  SPA-GMN-PRTKBN11
           MOVE    "F"                 TO  SPA-GMN-PRTKBN12             
           MOVE    "T"                 TO  SPA-GMN-PRTKBN21
           MOVE    "F"                 TO  SPA-GMN-PRTKBN22             
           MOVE    "T"                 TO  SPA-GMN-PRTKBN31
           MOVE    "F"                 TO  SPA-GMN-PRTKBN32             
           MOVE    "T"                 TO  SPA-GMN-PRTKBN41
           MOVE    "F"                 TO  SPA-GMN-PRTKBN42             
           MOVE    "T"                 TO  SPA-GMN-PRTKBN51
           MOVE    "F"                 TO  SPA-GMN-PRTKBN52             
      *
      *    標準総括表印刷指示
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    "044"               TO  SYS-3003-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3003-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
           ELSE
               INITIALIZE                      SYS-3003-REC
           END-IF    
           IF      SYS-3003-PRTKBN1    =   SPACE
               MOVE    "T"                 TO  SPA-GMN-PRTKBN11  
               MOVE    "F"                 TO  SPA-GMN-PRTKBN12  
           ELSE
               MOVE    "F"                 TO  SPA-GMN-PRTKBN11  
               MOVE    "T"                 TO  SPA-GMN-PRTKBN12
           END-IF      
           IF      SYS-3003-PRTKBN2    =   SPACE
               MOVE    "T"                 TO  SPA-GMN-PRTKBN21  
               MOVE    "F"                 TO  SPA-GMN-PRTKBN22  
           ELSE
               MOVE    "F"                 TO  SPA-GMN-PRTKBN21  
               MOVE    "T"                 TO  SPA-GMN-PRTKBN22
           END-IF      
           IF      SYS-3003-PRTKBN3    =   SPACE
               MOVE    "F"                 TO  SPA-GMN-PRTKBN31  
               MOVE    "T"                 TO  SPA-GMN-PRTKBN32  
           ELSE
               MOVE    "T"                 TO  SPA-GMN-PRTKBN31  
               MOVE    "F"                 TO  SPA-GMN-PRTKBN32
           END-IF      
           IF      SYS-3003-PRTKBN4    =   SPACE
               MOVE    "T"                 TO  SPA-GMN-PRTKBN41  
               MOVE    "F"                 TO  SPA-GMN-PRTKBN42  
           ELSE
               MOVE    "F"                 TO  SPA-GMN-PRTKBN41  
               MOVE    "T"                 TO  SPA-GMN-PRTKBN42
           END-IF      
           IF      SYS-3003-PRTKBN5    =   SPACE
               MOVE    "F"                 TO  SPA-GMN-PRTKBN51  
               MOVE    "T"                 TO  SPA-GMN-PRTKBN52  
           ELSE
               MOVE    "T"                 TO  SPA-GMN-PRTKBN51  
               MOVE    "F"                 TO  SPA-GMN-PRTKBN52
           END-IF      
      * 
      *    県単独情報
           MOVE    SPACE               TO  SYS-3004-REC
           INITIALIZE                      SYS-3004-REC
           MOVE    SPA-W01-KANRICD     TO  SYS-3004-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3004-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3004-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3004-HOSPNUM
           MOVE    SYS-3004-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM         UNTIL   FLG-SYSKANRI   =   1
               MOVE    MCPDATA-REC         TO  SYS-3004-REC
               MOVE    SYS-3004-KBNCD(1:4) TO  IDX
               MOVE    SYS-3004-SHELLID    TO  SPA-GMN-SHELLID (IDX)
               MOVE    SYS-3004-SHORIMSG   TO  SPA-GMN-SHORIMSG(IDX)
               MOVE    SYS-3004-CHK        TO  SPA-GMN-CHK     (IDX)
               MOVE    SYS-3004-PRTNM      TO  SPA-GMN-PRTNM   (IDX)
               MOVE    SYS-3004-PARAKBN    TO  SPA-GMN-PARAKBN (IDX)
               MOVE    SYS-3004-NAME       TO  SPA-GMN-NAME    (IDX)
                                               SPA-GMN-NAME-BACK
                                                               (IDX)
               PERFORM VARYING IDZ FROM    1   BY  1
                       UNTIL   IDZ >       SPA-GMN-PARAKBN-MAX
                   IF      SPA-GMN-PARAKBN (IDX)
                                       =   SPA-GMN-PARAKBNL (IDZ)
                       MOVE    SPA-GMN-PARAKBNLST (IDZ)
                                           TO  SPA-GMN-PARAKBN-G (IDX)
                       MOVE    SPA-GMN-PARAKBN-MAX 
                                           TO  IDZ
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    処理スクリプトの存在チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       25
               PERFORM 3101-SCRIPT-CHECK-SEC
           END-PERFORM
      *
           MOVE    WRK-CONS-LINE-MAX   TO  SPA-GMN-MAX
      *
           COMPUTE SPA-GMN-PAGE-MAX    =   (SPA-GMN-MAX - 1)     /
                                            WRK-CONS-LINE-MAX    +   1
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *     
      *****************************************************************
      *    処理スクリプトの存在チェック
      *****************************************************************
       3101-SCRIPT-CHECK-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-NAI-SHELLID-CHK (IDX)
      *
           IF    ( SPA-MW          =   SPA-GINBEE )
           AND   ( SPA-GMN-NAME (IDX)
                               NOT =   SPACE      )
               GO  TO  3101-SCRIPT-CHECK-EXT
           END-IF
      *
           IF    ( SPA-GMN-SHELLID  (IDX)  =   SPACE  )
           OR    ( SPA-GMN-SHORIMSG (IDX)  =   SPACE  )
           OR    ( SPA-GMN-CHK      (IDX)  =   "FFFF" )
               CONTINUE
           ELSE
      *        ファイルデイレクトリ位置指定
               MOVE    SPA-GMN-SHELLID (IDX)
                                       TO  WRK-SHELL-SHELL
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    WRK-SHELL-KENTAN
                                       TO  MKPASS-IN-01
               CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01    TO  WRKPARA
      *
               OPEN    INPUT               WRK-FILE
               IF      STS-WRK         =   "00"
                   CLOSE   WRK-FILE
               ELSE
                   MOVE    "1"         TO  SPA-NAI-SHELLID-CHK (IDX)
               END-IF
           END-IF
           .
       3101-SCRIPT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               PERFORM 490-KOUSHIN-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *       Ｄｏｃｋｅｒ一覧
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 470-DOCER-LIST-SEC
      *       前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *       次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 410-KAKUTEI-SEC
      *        社保印刷指示
               WHEN    "CLICKED"       ALSO    "PRTKBN11"
                   PERFORM 450-PRTKBN11-SEC
               WHEN    "CLICKED"       ALSO    "PRTKBN12"
                   PERFORM 450-PRTKBN12-SEC
      *        国保印刷指示
               WHEN    "CLICKED"       ALSO    "PRTKBN21"
                   PERFORM 450-PRTKBN21-SEC
               WHEN    "CLICKED"       ALSO    "PRTKBN22"
                   PERFORM 450-PRTKBN22-SEC
      *        レセ電国保印刷指示
               WHEN    "CLICKED"       ALSO    "PRTKBN31"
                   PERFORM 450-PRTKBN31-SEC
               WHEN    "CLICKED"       ALSO    "PRTKBN32"
                   PERFORM 450-PRTKBN32-SEC
      *        広域連合印刷指示
               WHEN    "CLICKED"       ALSO    "PRTKBN41"
                   PERFORM 450-PRTKBN41-SEC
               WHEN    "CLICKED"       ALSO    "PRTKBN42"
                   PERFORM 450-PRTKBN42-SEC
      *        レセ電広域連合印刷指示
               WHEN    "CLICKED"       ALSO    "PRTKBN51"
                   PERFORM 450-PRTKBN51-SEC
               WHEN    "CLICKED"       ALSO    "PRTKBN52"
                   PERFORM 450-PRTKBN52-SEC
      *        処理対象
               WHEN    OTHER
                   EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET(1:6)
                       WHEN    "CLICKED"   ALSO    "CHKBTN"
                           MOVE    MCP-WIDGET          TO  WRK-WIDGET
                           MOVE    WRK-WIDGET2         TO  IDX 
                           PERFORM 420-CHKFLG-CHK-SEC
                  END-EVALUATE
      *        Ｄｏｃｋｅｒ登録
                   EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET(1:2)
                       WHEN    "CLICKED"   ALSO    "BD"
                           MOVE    MCP-WIDGET       TO  WRK-WIDGET
                           MOVE    WRK-WIDGET-R2    TO  SPA-NAI-SELCD
                           PERFORM 460-DOCER-SEC
                  END-EVALUATE    
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        管理コード
               WHEN    "ACTIVATE"      ALSO    "PARAKBN01"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN02"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN03"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN04"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN05"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN06"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN07"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN08"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN09"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN10"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN11"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN12"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN13"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN14"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN15"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN16"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN17"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN18"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN19"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN20"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN21"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN22"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN23"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN24"
               WHEN    "ACTIVATE"      ALSO    "PARAKBN25"
                   PERFORM  4100-PARAKBN-CHK-SEC
      *        一括分
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    一括分入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
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
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    処理スクリプトの存在チェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       25
               PERFORM 3101-SCRIPT-CHECK-SEC
           END-PERFORM
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
      *    シェル名
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   25
               MOVE    W14-SHELLID (IDX)   TO  SPA-GMN-SHELLID (IDX)
           END-PERFORM
      *    処理名
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   25
               MOVE    W14-SHORIMSG(IDX)   TO  SPA-GMN-SHORIMSG (IDX)
           END-PERFORM
      *    プリンタ名
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   25
               MOVE    W14-PRTNM   (IDX)   TO  SPA-GMN-PRTNM    (IDX)
           END-PERFORM
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
           PERFORM  4101-SHELLID-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM  4102-SHORIMSG-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM  4103-PRTNM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM  4104-PARAKBN-CHK-SEC
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W14"               TO  SPA-MOTOPG
      *
      ***  MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    シェル名入力処理
      *****************************************************************
       4101-SHELLID-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   25  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     SPA-GMN-SHELLID (IDX)    NOT =   SPACE
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    SPA-GMN-SHELLID (IDX)
                                               TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF     (KANACHK-RC      NOT =   ZERO  )  OR
                          (KANACHK-SYUBETU     =   2     )
                       MOVE    "0001"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =  (IDX   -   1)  *   6
                                               +   1
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4101-SHELLID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理名入力処理
      *****************************************************************
       4102-SHORIMSG-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   25  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     SPA-GMN-SHORIMSG (IDX)   NOT =   SPACE
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    SPA-GMN-SHORIMSG (IDX)
                                               TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   IF     (KANACHK-RC      NOT =   ZERO  )  OR
                          (KANACHK-SYUBETU     =   1     )
                       MOVE    "0002"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-CUR     =  (IDX   -   1)  *   6
                                               +   2
                   ELSE
                       MOVE    KANACHK-OUT-INPUT
                                               TO  SPA-GMN-SHORIMSG(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4102-SHORIMSG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ名入力処理
      *****************************************************************
       4103-PRTNM-CHK-SEC       SECTION.
      *
      *
           .
       4103-PRTNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ種別入力処理
      *****************************************************************
       4104-PARAKBN-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   25  )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-PARAKBN (IDX)
                                           NOT =   SPACE
                   MOVE    ZERO               TO  WRK-HENFLG
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       SPA-GMN-PARAKBN-MAX
                           OR      WRK-HENFLG  =   1
                       IF      SPA-GMN-PARAKBN (IDX)
                                           =   SPA-GMN-PARAKBNL (IDY)
                           MOVE    SPA-GMN-PARAKBNLST (IDY)
                                           TO  SPA-GMN-PARAKBN-G (IDX)
                           MOVE    1       TO  WRK-HENFLG
                       END-IF
                   END-PERFORM
      *
                   IF      WRK-HENFLG          =   ZERO 
                       MOVE    "0003"          TO  SPA-ERRCD
                   ELSE
                       IF    ( SPA-GMN-PARAKBN (IDX)
                                                   NOT =   SPACE )
                       AND   ( SPA-GMN-CHKFLG  (IDX 3) =   "F"   )
                           MOVE    "0050"          TO  SPA-ERRCD
                       END-IF 
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       COMPUTE SPA-GMN-CUR     =   IDX    *   2
                                                          +   200
                   END-IF
               END-IF
           END-PERFORM        
      *
           .
       4104-PARAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ種別入力処理
      *****************************************************************
       4100-PARAKBN-CHK-SEC       SECTION.
      *
           MOVE    MCP-WIDGET (8:2)    TO  WRK-IDX2
           MOVE    WRK-IDX2            TO  IDX
      * 
           COMPUTE WRK-GMN-CUR     =   IDX   *   2   +   200
      *
           MOVE    W14-PARAKBN (IDX)   TO  SPA-GMN-PARAKBN-G (IDX)
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD       =   SPACE
      *        次のカーソル位置指定
               MOVE    WRK-IDX2            TO  IDX
               MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
               IF      IDX                 =   WRK-CONS-LINE-MAX
                   MOVE    300                 TO  SPA-GMN-CUR
               ELSE    
                   COMPUTE SPA-GMN-CUR     =   IDX     *   6   +   1
               END-IF
           END-IF
      *
           .
       4100-PARAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理対象チェック処理
      *****************************************************************
       420-CHKFLG-CHK-SEC       SECTION.
      *
           MOVE    MCP-WIDGET (7:3)   TO  WRK-IDX3
           COMPUTE IDX     =   (WRK-IDX3  +   3)  /   4
           COMPUTE IDY     =    WRK-IDX3  -   (IDX    -   1)  *   4   
      *
      *****COMPUTE SPA-GMN-CUR        =  (IDX -   1)  *   6   +
           COMPUTE WRK-GMN-CUR        =  (IDX -   1)  *   6   +
                                          2   +   IDY
      *
           MOVE    W14-CHKBTN(IDX IDY)    TO  SPA-GMN-CHKFLG (IDX IDY)
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      420-CHKFLG-CHK-EXT
           END-IF
      *
           MOVE    WRK-GMN-CUR         TO  SPA-GMN-CUR
           COMPUTE IDX     =   (WRK-IDX3  +   3)  /   4
           COMPUTE IDY     =    WRK-IDX3  -   (IDX    -   1)  *   4   
      *
      *    処理スクリプトの存在チェック
           PERFORM 3101-SCRIPT-CHECK-SEC
      *
      *    次のカーソル位置指定
           IF      IDY                 <   3
               ADD     1                  TO  SPA-GMN-CUR
           ELSE
               IF      IDX                =   WRK-CONS-LINE-MAX
                   MOVE    1                  TO  SPA-GMN-CUR
               ELSE    
                   COMPUTE SPA-GMN-CUR    =   IDX   *   2   +   199
               END-IF    
           END-IF
           .
       420-CHKFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    "0006"             TO  SPA-ERRCD
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
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
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO  TO  440-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE       =   SPA-GMN-PAGE-MAX
               MOVE    "0007"             TO  SPA-ERRCD
               GO  TO  440-ATOGMN-EXT
           END-IF
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1       
      *
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準社保総括表印刷指示処理
      *****************************************************************
       450-PRTKBN11-SEC          SECTION.
      *
           MOVE    W14-PRTKBN11    TO  SPA-GMN-PRTKBN11
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       450-PRTKBN11-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準社保総括表印刷指示処理
      *****************************************************************
       450-PRTKBN12-SEC          SECTION.
      *
           MOVE    W14-PRTKBN12    TO  SPA-GMN-PRTKBN12
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN12-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準国保総括表印刷指示処理
      *****************************************************************
       450-PRTKBN21-SEC          SECTION.
      *
           MOVE    W14-PRTKBN21    TO  SPA-GMN-PRTKBN21
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN21-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準国保総括表印刷指示処理
      *****************************************************************
       450-PRTKBN22-SEC          SECTION.
      *
           MOVE    W14-PRTKBN22    TO  SPA-GMN-PRTKBN22
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN22-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準レセ電国保総括表印刷指示処理
      *****************************************************************
       450-PRTKBN31-SEC          SECTION.
      *
           MOVE    W14-PRTKBN31    TO  SPA-GMN-PRTKBN31
      *
      *    入力チェック処理
           MOVE    ZERO            TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN31-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準レセ電国保総括表印刷指示処理
      *****************************************************************
       450-PRTKBN32-SEC          SECTION.
      *
           MOVE    W14-PRTKBN32    TO  SPA-GMN-PRTKBN32
      *
      *    入力チェック処理
           MOVE    ZERO            TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN32-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準広域連合総括表印刷指示処理
      *****************************************************************
       450-PRTKBN41-SEC          SECTION.
      *
           MOVE    W14-PRTKBN41    TO  SPA-GMN-PRTKBN41
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN41-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準広域連合総括表印刷指示処理
      *****************************************************************
       450-PRTKBN42-SEC          SECTION.
      *
           MOVE    W14-PRTKBN42    TO  SPA-GMN-PRTKBN42
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN42-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準レセ電広域連合総括表印刷指示処理
      *****************************************************************
       450-PRTKBN51-SEC          SECTION.
      *
           MOVE    W14-PRTKBN51    TO  SPA-GMN-PRTKBN51
      *
      *    入力チェック処理
           MOVE    ZERO            TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN51-EXT.
           EXIT.
      *
      *****************************************************************
      *    標準レセ電広域連合総括表印刷指示処理
      *****************************************************************
       450-PRTKBN52-SEC            SECTION.
      *
           MOVE    W14-PRTKBN52    TO  SPA-GMN-PRTKBN52
      *
      *    入力チェック処理
           MOVE    ZERO            TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       450-PRTKBN52-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｄｏｃｋｅｒ登録画面
      *****************************************************************
       460-DOCER-SEC               SECTION.
      *
      *    入力チェック処理
           MOVE    ZERO            TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               MOVE    ZERO            TO  SPA-NAI-SELCD
               GO  TO  460-DOCER-EXT
           END-IF
      *
      *    シェル名等が未入力の場合は遷移しない
           IF    ( SPA-GMN-SHORIMSG (SPA-NAI-SELCD) 
                                   =   SPACE )
           OR    ( SPA-GMN-SHELLID  (SPA-NAI-SELCD) 
                                   =   SPACE )
               IF      SPA-GMN-NAME (SPA-NAI-SELCD)    =   SPACE
                   MOVE    ZERO            TO  SPA-NAI-SELCD
                   GO  TO  460-DOCER-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-MOTOPG
      *
           MOVE    SPA-GMN-NAME (SPA-NAI-SELCD)
                                   TO  SPA-GMN-NAME-BACK (SPA-NAI-SELCD)
      *
           MOVE    SPA-W14FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGW141"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W14FREE
      *
           MOVE    1               TO  FLG-END
      *
           .
       460-DOCER-EXT.
      *
      *****************************************************************
      *    Ｄｏｃｋｅｒ一覧画面
      *****************************************************************
       470-DOCER-LIST-SEC          SECTION.
      *
           IF      SPA-MW          =   SPA-GINBEE
               CONTINUE
           ELSE
               GO  TO  470-DOCER-LIST-EXT
           END-IF
      *
      *    Ｄｏｃｋｅｒが未登録の場合は遷移しない
           MOVE    ZERO            TO  FLG-CHK
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       25
               IF      SPA-GMN-NAME (IDX) 
                                   NOT =   SPACE
                   MOVE    1               TO  FLG-CHK
                   MOVE    25              TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK         =   ZERO
               GO  TO  470-DOCER-LIST-EXT
           END-IF
      *
      *    入力チェック処理
           MOVE    ZERO            TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  470-DOCER-LIST-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-W14FREE     TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGW142"      USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W14FREE
      *
           MOVE    1               TO  FLG-END
      *
           .
       470-DOCER-LIST-EXT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    印刷指示    
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    "044"               TO  SYS-3003-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3003-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
           ELSE    
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    "044"               TO  SYS-3003-KBNCD
               MOVE    ZERO                TO  SYS-3003-STYUKYMD
               MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           END-IF    
           IF      SPA-GMN-PRTKBN11    =   "T"
               MOVE    SPACE               TO  SYS-3003-PRTKBN1
           ELSE
               MOVE    "1"                 TO  SYS-3003-PRTKBN1
           END-IF        
           IF      SPA-GMN-PRTKBN21    =   "T"
               MOVE    SPACE               TO  SYS-3003-PRTKBN2
           ELSE
               MOVE    "1"                 TO  SYS-3003-PRTKBN2
           END-IF        
           IF      SPA-GMN-PRTKBN32    =   "T"
               MOVE    SPACE               TO  SYS-3003-PRTKBN3
           ELSE
               MOVE    "1"                 TO  SYS-3003-PRTKBN3
           END-IF        
           IF      SPA-GMN-PRTKBN41    =   "T"
               MOVE    SPACE               TO  SYS-3003-PRTKBN4
           ELSE
               MOVE    "1"                 TO  SYS-3003-PRTKBN4
           END-IF        
           IF      SPA-GMN-PRTKBN52    =   "T"
               MOVE    SPACE               TO  SYS-3003-PRTKBN5
           ELSE
               MOVE    "1"                 TO  SYS-3003-PRTKBN5
           END-IF        
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "1003"          TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      * 
      *    県単独情報
           MOVE    SPACE               TO  SYS-3004-REC
           INITIALIZE                      SYS-3004-REC
           MOVE    SPA-W01-KANRICD     TO  SYS-3004-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3004-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-3004-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3004-HOSPNUM
           MOVE    SYS-3004-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      * 
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      * 
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   IDX      >=  SPA-GMN-MAX
               ADD     1                       TO  IDX
                   MOVE    SPACE                   TO  SYS-3004-REC
                   INITIALIZE                      SYS-3004-REC
                   MOVE    SPA-W01-KANRICD         TO  SYS-3004-KANRICD
                   MOVE    SPA-GMN-TCD (IDX)       TO  SYS-3004-KBNCD
                   MOVE    SPA-W01-STYUKYMD        TO  SYS-3004-STYUKYMD
                   MOVE    SPA-W01-EDYUKYMD        TO  SYS-3004-EDYUKYMD
                   MOVE    SPA-GMN-SHORIMSG (IDX)  TO  SYS-3004-SHORIMSG
                   MOVE    SPA-GMN-SHELLID  (IDX)  TO  SYS-3004-SHELLID
                   MOVE    SPA-GMN-CHK      (IDX)  TO  SYS-3004-CHK
                   MOVE    SPA-GMN-PRTNM    (IDX)  TO  SYS-3004-PRTNM
                   MOVE    SPA-GMN-PARAKBN  (IDX)  TO  SYS-3004-PARAKBN
                   MOVE    SPA-GMN-NAME     (IDX)  TO  SYS-3004-NAME
     *
                   MOVE    SPA-HOSPNUM         TO  SYS-3004-HOSPNUM
                   MOVE    SYS-3004-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"              TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC              NOT =   ZERO 
                       MOVE    "1001"                  TO  SPA-ERRCD
                       GO  TO  490-KOUSHIN-EXT
                   END-IF
           END-PERFORM          
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
           MOVE    1                    TO  FLG-TOUROKU
           MOVE    SPACE                TO  SPA-ERRCD
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4901-KANRENCHK-EXT
           END-IF
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                            BY  "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD 
                   END-IF
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "12"                TO  LNK-DAY1-IRAI
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W14    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE              W14
      *
           COMPUTE IDY     =   SPA-GMN-PAGE    *   25 
           COMPUTE IDZ     =   IDY             -   24
      *
           PERFORM VARYING     IDX      FROM    1   BY  1
                   UNTIL       IDX      >       WRK-CONS-LINE-MAX 
               MOVE    SPA-GMN-TCD        (IDX) TO  W14-CD       (IDX)
               MOVE    SPA-GMN-SHORIMSG   (IDX) TO  W14-SHORIMSG (IDX)
               MOVE    SPA-GMN-SHELLID    (IDX) TO  W14-SHELLID  (IDX)
               MOVE    SPA-GMN-CHK        (IDX) TO  W14-CHK      (IDX)
               MOVE    SPA-GMN-PRTNM      (IDX) TO  W14-PRTNM    (IDX)
               MOVE    SPA-GMN-PARAKBN-G  (IDX) TO  W14-PARAKBN  (IDX)
      *
               PERFORM VARYING IDY FROM    1       BY  1
                       UNTIL   IDY >       SPA-GMN-PARAKBN-MAX
                   MOVE    SPA-GMN-PARAKBNLST (IDY)
                                       TO  W14-PARAKBN-ITEM (IDX IDY)
               END-PERFORM
      *
               IF      SPA-NAI-SHELLID-CHK (IDX)
                                       =   SPACE
                   MOVE    "black"         TO  W14-SHELLID-STYLE (IDX)
               ELSE
                  MOVE    "red"            TO  W14-SHELLID-STYLE (IDX)
                  MOVE    "処理スクリプトがインストールされていません"
                                           TO  W14-MSG
               END-IF
               MOVE    SPA-GMN-PARAKBN-MAX
                                       TO  W14-PARAKBN-COUNT (IDX)
      *
               MOVE    "black"         TO  W14-BD-STYLE (IDX)
               MOVE    SPACE           TO  W14-BD-LABEL (IDX)
               IF      SPA-MW          =   SPA-GINBEE
                   MOVE    "T"             TO  W14-BD-VISIBLE (IDX)
                   IF      SPA-GMN-NAME (IDX)
                                           =   SPACE
                       MOVE    "black"         TO  W14-BD-STYLE (IDX)
                   ELSE
                       MOVE    "blue"          TO  W14-BD-STYLE (IDX)
                       MOVE    "*"             TO  W14-BD-LABEL (IDX)
                   END-IF
               ELSE
                   MOVE    "F"             TO  W14-BD-VISIBLE (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-STYUKYMD    TO  W14-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  W14-EDYUKYMD
           MOVE    SPA-GMN-PRTKBN11    TO  W14-PRTKBN11
           MOVE    SPA-GMN-PRTKBN12    TO  W14-PRTKBN12
           MOVE    SPA-GMN-PRTKBN21    TO  W14-PRTKBN21
           MOVE    SPA-GMN-PRTKBN22    TO  W14-PRTKBN22
           MOVE    SPA-GMN-PRTKBN31    TO  W14-PRTKBN31
           MOVE    SPA-GMN-PRTKBN32    TO  W14-PRTKBN32
           MOVE    SPA-GMN-PRTKBN41    TO  W14-PRTKBN41
           MOVE    SPA-GMN-PRTKBN42    TO  W14-PRTKBN42
           MOVE    SPA-GMN-PRTKBN51    TO  W14-PRTKBN51
           MOVE    SPA-GMN-PRTKBN52    TO  W14-PRTKBN52
      *
           MOVE    "red"               TO  W14-MSG-STYLE
      *
           IF      SPA-MW          =   SPA-GINBEE
               MOVE    "一覧"          TO  W14-B05-LABEL
           ELSE
               MOVE    SPACE           TO  W14-B05-LABEL
           END-IF
      *
      *★★平成２０年４月改正対応
           MOVE    "F"                 TO  W14-PRTKBN51
           MOVE    "T"                 TO  W14-PRTKBN52
           MOVE    WIDGET-INSENSITIVE  TO  W14-PRTKBN51-STATE
                                           W14-PRTKBN52-STATE
      *★★
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN   1
                    MOVE  "SHELLID01"    TO  MCP-WIDGET
                WHEN   7
                    MOVE  "SHELLID02"    TO  MCP-WIDGET
                WHEN   13
                    MOVE  "SHELLID03"    TO  MCP-WIDGET
                WHEN   19
                    MOVE  "SHELLID04"    TO  MCP-WIDGET
                WHEN   25
                    MOVE  "SHELLID05"    TO  MCP-WIDGET
                WHEN   31
                    MOVE  "SHELLID06"    TO  MCP-WIDGET
                WHEN   37
                    MOVE  "SHELLID07"    TO  MCP-WIDGET
                WHEN   43
                    MOVE  "SHELLID08"    TO  MCP-WIDGET
                WHEN   49
                    MOVE  "SHELLID09"    TO  MCP-WIDGET
                WHEN   55
                    MOVE  "SHELLID10"    TO  MCP-WIDGET
                WHEN   61
                    MOVE  "SHELLID11"    TO  MCP-WIDGET
                WHEN   67
                    MOVE  "SHELLID12"    TO  MCP-WIDGET
                WHEN   73
                    MOVE  "SHELLID13"    TO  MCP-WIDGET
                WHEN   79
                    MOVE  "SHELLID14"    TO  MCP-WIDGET
                WHEN   85
                    MOVE  "SHELLID15"    TO  MCP-WIDGET
                WHEN   91
                    MOVE  "SHELLID16"    TO  MCP-WIDGET
                WHEN   97
                    MOVE  "SHELLID17"    TO  MCP-WIDGET
                WHEN   103
                    MOVE  "SHELLID18"    TO  MCP-WIDGET
                WHEN   109
                    MOVE  "SHELLID19"    TO  MCP-WIDGET
                WHEN   115
                    MOVE  "SHELLID20"    TO  MCP-WIDGET
                WHEN   121
                    MOVE  "SHELLID21"    TO  MCP-WIDGET
                WHEN   127
                    MOVE  "SHELLID22"    TO  MCP-WIDGET
                WHEN   133
                    MOVE  "SHELLID23"    TO  MCP-WIDGET
                WHEN   139
                    MOVE  "SHELLID24"    TO  MCP-WIDGET
                WHEN   145
                    MOVE  "SHELLID25"    TO  MCP-WIDGET
                WHEN   2
                    MOVE  "SHORIMSG01"    TO  MCP-WIDGET
                WHEN   8
                    MOVE  "SHORIMSG02"    TO  MCP-WIDGET
                WHEN   14
                    MOVE  "SHORIMSG03"    TO  MCP-WIDGET
                WHEN   20
                    MOVE  "SHORIMSG04"    TO  MCP-WIDGET
                WHEN   26
                    MOVE  "SHORIMSG05"    TO  MCP-WIDGET
                WHEN   32
                    MOVE  "SHORIMSG06"    TO  MCP-WIDGET
                WHEN   38
                    MOVE  "SHORIMSG07"    TO  MCP-WIDGET
                WHEN   44
                    MOVE  "SHORIMSG08"    TO  MCP-WIDGET
                WHEN   50
                    MOVE  "SHORIMSG09"    TO  MCP-WIDGET
                WHEN   56
                    MOVE  "SHORIMSG10"    TO  MCP-WIDGET
                WHEN   62
                    MOVE  "SHORIMSG11"    TO  MCP-WIDGET
                WHEN   68
                    MOVE  "SHORIMSG12"    TO  MCP-WIDGET
                WHEN   74
                    MOVE  "SHORIMSG13"    TO  MCP-WIDGET
                WHEN   80
                    MOVE  "SHORIMSG14"    TO  MCP-WIDGET
                WHEN   86
                    MOVE  "SHORIMSG15"    TO  MCP-WIDGET
                WHEN   92
                    MOVE  "SHORIMSG16"    TO  MCP-WIDGET
                WHEN   98
                    MOVE  "SHORIMSG17"    TO  MCP-WIDGET
                WHEN   104
                    MOVE  "SHORIMSG18"    TO  MCP-WIDGET
                WHEN   110
                    MOVE  "SHORIMSG19"    TO  MCP-WIDGET
                WHEN   116
                    MOVE  "SHORIMSG20"    TO  MCP-WIDGET
                WHEN   122
                    MOVE  "SHORIMSG21"    TO  MCP-WIDGET
                WHEN   128
                    MOVE  "SHORIMSG22"    TO  MCP-WIDGET
                WHEN   134
                    MOVE  "SHORIMSG23"    TO  MCP-WIDGET
                WHEN   140
                    MOVE  "SHORIMSG24"    TO  MCP-WIDGET
                WHEN   146
                    MOVE  "SHORIMSG25"    TO  MCP-WIDGET
                WHEN   3
                    MOVE  "CHKBTN001"    TO  MCP-WIDGET
                WHEN   4
                    MOVE  "CHKBTN002"    TO  MCP-WIDGET
                WHEN   5
                    MOVE  "CHKBTN003"    TO  MCP-WIDGET
                WHEN   6
                    MOVE  "CHKBTN004"    TO  MCP-WIDGET
                WHEN   9
                    MOVE  "CHKBTN005"    TO  MCP-WIDGET
                WHEN   10
                    MOVE  "CHKBTN006"    TO  MCP-WIDGET
                WHEN   11
                    MOVE  "CHKBTN007"    TO  MCP-WIDGET
                WHEN   12
                    MOVE  "CHKBTN008"    TO  MCP-WIDGET
                WHEN   15
                    MOVE  "CHKBTN009"    TO  MCP-WIDGET
                WHEN   16
                    MOVE  "CHKBTN010"    TO  MCP-WIDGET
                WHEN   17
                    MOVE  "CHKBTN011"    TO  MCP-WIDGET
                WHEN   18
                    MOVE  "CHKBTN012"    TO  MCP-WIDGET
                WHEN   21
                    MOVE  "CHKBTN013"    TO  MCP-WIDGET
                WHEN   22
                    MOVE  "CHKBTN014"    TO  MCP-WIDGET
                WHEN   23
                    MOVE  "CHKBTN015"    TO  MCP-WIDGET
                WHEN   24
                    MOVE  "CHKBTN016"    TO  MCP-WIDGET
                WHEN   27
                    MOVE  "CHKBTN017"    TO  MCP-WIDGET
                WHEN   28
                    MOVE  "CHKBTN018"    TO  MCP-WIDGET
                WHEN   29
                    MOVE  "CHKBTN019"    TO  MCP-WIDGET
                WHEN   30
                    MOVE  "CHKBTN020"    TO  MCP-WIDGET
                WHEN   33
                    MOVE  "CHKBTN021"    TO  MCP-WIDGET
                WHEN   34
                    MOVE  "CHKBTN022"    TO  MCP-WIDGET
                WHEN   35
                    MOVE  "CHKBTN023"    TO  MCP-WIDGET
                WHEN   36
                    MOVE  "CHKBTN024"    TO  MCP-WIDGET
                WHEN   39
                    MOVE  "CHKBTN025"    TO  MCP-WIDGET
                WHEN   40
                    MOVE  "CHKBTN026"    TO  MCP-WIDGET
                WHEN   41
                    MOVE  "CHKBTN027"    TO  MCP-WIDGET
                WHEN   42
                    MOVE  "CHKBTN028"    TO  MCP-WIDGET
                WHEN   45
                    MOVE  "CHKBTN029"    TO  MCP-WIDGET
                WHEN   46
                    MOVE  "CHKBTN030"    TO  MCP-WIDGET
                WHEN   47
                    MOVE  "CHKBTN031"    TO  MCP-WIDGET
                WHEN   48
                    MOVE  "CHKBTN032"    TO  MCP-WIDGET
                WHEN   51
                    MOVE  "CHKBTN033"    TO  MCP-WIDGET
                WHEN   52
                    MOVE  "CHKBTN034"    TO  MCP-WIDGET
                WHEN   53
                    MOVE  "CHKBTN035"    TO  MCP-WIDGET
                WHEN   54
                    MOVE  "CHKBTN036"    TO  MCP-WIDGET
                WHEN   57
                    MOVE  "CHKBTN037"    TO  MCP-WIDGET
                WHEN   58
                    MOVE  "CHKBTN038"    TO  MCP-WIDGET
                WHEN   59
                    MOVE  "CHKBTN039"    TO  MCP-WIDGET
                WHEN   60
                    MOVE  "CHKBTN040"    TO  MCP-WIDGET
                WHEN   63
                    MOVE  "CHKBTN041"    TO  MCP-WIDGET
                WHEN   64
                    MOVE  "CHKBTN042"    TO  MCP-WIDGET
                WHEN   65
                    MOVE  "CHKBTN043"    TO  MCP-WIDGET
                WHEN   66
                    MOVE  "CHKBTN044"    TO  MCP-WIDGET
                WHEN   69
                    MOVE  "CHKBTN045"    TO  MCP-WIDGET
                WHEN   70
                    MOVE  "CHKBTN046"    TO  MCP-WIDGET
                WHEN   71
                    MOVE  "CHKBTN047"    TO  MCP-WIDGET
                WHEN   72
                    MOVE  "CHKBTN048"    TO  MCP-WIDGET
                WHEN   75
                    MOVE  "CHKBTN049"    TO  MCP-WIDGET
                WHEN   76
                    MOVE  "CHKBTN050"    TO  MCP-WIDGET
                WHEN   77
                    MOVE  "CHKBTN051"    TO  MCP-WIDGET
                WHEN   78
                    MOVE  "CHKBTN052"    TO  MCP-WIDGET
                WHEN   81
                    MOVE  "CHKBTN053"    TO  MCP-WIDGET
                WHEN   82
                    MOVE  "CHKBTN054"    TO  MCP-WIDGET
                WHEN   83
                    MOVE  "CHKBTN055"    TO  MCP-WIDGET
                WHEN   84
                    MOVE  "CHKBTN056"    TO  MCP-WIDGET
                WHEN   87
                    MOVE  "CHKBTN057"    TO  MCP-WIDGET
                WHEN   88
                    MOVE  "CHKBTN058"    TO  MCP-WIDGET
                WHEN   89
                    MOVE  "CHKBTN059"    TO  MCP-WIDGET
                WHEN   90
                    MOVE  "CHKBTN060"    TO  MCP-WIDGET
                WHEN   93
                    MOVE  "CHKBTN061"    TO  MCP-WIDGET
                WHEN   94
                    MOVE  "CHKBTN062"    TO  MCP-WIDGET
                WHEN   95
                    MOVE  "CHKBTN063"    TO  MCP-WIDGET
                WHEN   96
                    MOVE  "CHKBTN064"    TO  MCP-WIDGET
                WHEN   99
                    MOVE  "CHKBTN065"    TO  MCP-WIDGET
                WHEN   100
                    MOVE  "CHKBTN066"    TO  MCP-WIDGET
                WHEN   101
                    MOVE  "CHKBTN067"    TO  MCP-WIDGET
                WHEN   102
                    MOVE  "CHKBTN068"    TO  MCP-WIDGET
                WHEN   105
                    MOVE  "CHKBTN069"    TO  MCP-WIDGET
                WHEN   106
                    MOVE  "CHKBTN070"    TO  MCP-WIDGET
                WHEN   107
                    MOVE  "CHKBTN071"    TO  MCP-WIDGET
                WHEN   108
                    MOVE  "CHKBTN072"    TO  MCP-WIDGET
                WHEN   111
                    MOVE  "CHKBTN073"    TO  MCP-WIDGET
                WHEN   112
                    MOVE  "CHKBTN074"    TO  MCP-WIDGET
                WHEN   113
                    MOVE  "CHKBTN075"    TO  MCP-WIDGET
                WHEN   114
                    MOVE  "CHKBTN076"    TO  MCP-WIDGET
                WHEN   117
                    MOVE  "CHKBTN077"    TO  MCP-WIDGET
                WHEN   118
                    MOVE  "CHKBTN078"    TO  MCP-WIDGET
                WHEN   119
                    MOVE  "CHKBTN079"    TO  MCP-WIDGET
                WHEN   120
                    MOVE  "CHKBTN080"    TO  MCP-WIDGET
                WHEN   123
                    MOVE  "CHKBTN081"    TO  MCP-WIDGET
                WHEN   124
                    MOVE  "CHKBTN082"    TO  MCP-WIDGET
                WHEN   125
                    MOVE  "CHKBTN083"    TO  MCP-WIDGET
                WHEN   126
                    MOVE  "CHKBTN084"    TO  MCP-WIDGET
                WHEN   129
                    MOVE  "CHKBTN085"    TO  MCP-WIDGET
                WHEN   130
                    MOVE  "CHKBTN086"    TO  MCP-WIDGET
                WHEN   131
                    MOVE  "CHKBTN087"    TO  MCP-WIDGET
                WHEN   132
                    MOVE  "CHKBTN088"    TO  MCP-WIDGET
                WHEN   135
                    MOVE  "CHKBTN089"    TO  MCP-WIDGET
                WHEN   136
                    MOVE  "CHKBTN090"    TO  MCP-WIDGET
                WHEN   137
                    MOVE  "CHKBTN091"    TO  MCP-WIDGET
                WHEN   138
                    MOVE  "CHKBTN092"    TO  MCP-WIDGET
                WHEN   141
                    MOVE  "CHKBTN093"    TO  MCP-WIDGET
                WHEN   142
                    MOVE  "CHKBTN094"    TO  MCP-WIDGET
                WHEN   143
                    MOVE  "CHKBTN095"    TO  MCP-WIDGET
                WHEN   144
                    MOVE  "CHKBTN096"    TO  MCP-WIDGET
                WHEN   147
                    MOVE  "CHKBTN097"    TO  MCP-WIDGET
                WHEN   148
                    MOVE  "CHKBTN098"    TO  MCP-WIDGET
                WHEN   149
                    MOVE  "CHKBTN099"    TO  MCP-WIDGET
                WHEN   150
                    MOVE  "CHKBTN100"    TO  MCP-WIDGET
                WHEN   201
                    MOVE  "PRTNM01"    TO  MCP-WIDGET
                WHEN   203
                    MOVE  "PRTNM02"    TO  MCP-WIDGET
                WHEN   205
                    MOVE  "PRTNM03"    TO  MCP-WIDGET
                WHEN   207
                    MOVE  "PRTNM04"    TO  MCP-WIDGET
                WHEN   209
                    MOVE  "PRTNM05"    TO  MCP-WIDGET
                WHEN   211
                    MOVE  "PRTNM06"    TO  MCP-WIDGET
                WHEN   213
                    MOVE  "PRTNM07"    TO  MCP-WIDGET
                WHEN   215
                    MOVE  "PRTNM08"    TO  MCP-WIDGET
                WHEN   217
                    MOVE  "PRTNM09"    TO  MCP-WIDGET
                WHEN   219
                    MOVE  "PRTNM10"    TO  MCP-WIDGET
                WHEN   221
                    MOVE  "PRTNM11"    TO  MCP-WIDGET
                WHEN   223
                    MOVE  "PRTNM12"    TO  MCP-WIDGET
                WHEN   225
                    MOVE  "PRTNM13"    TO  MCP-WIDGET
                WHEN   227
                    MOVE  "PRTNM14"    TO  MCP-WIDGET
                WHEN   229
                    MOVE  "PRTNM15"    TO  MCP-WIDGET
                WHEN   231
                    MOVE  "PRTNM16"    TO  MCP-WIDGET
                WHEN   233
                    MOVE  "PRTNM17"    TO  MCP-WIDGET
                WHEN   235
                    MOVE  "PRTNM18"    TO  MCP-WIDGET
                WHEN   237
                    MOVE  "PRTNM19"    TO  MCP-WIDGET
                WHEN   239
                    MOVE  "PRTNM20"    TO  MCP-WIDGET
                WHEN   241
                    MOVE  "PRTNM21"    TO  MCP-WIDGET
                WHEN   243
                    MOVE  "PRTNM22"    TO  MCP-WIDGET
                WHEN   245
                    MOVE  "PRTNM23"    TO  MCP-WIDGET
                WHEN   247
                    MOVE  "PRTNM24"    TO  MCP-WIDGET
                WHEN   249
                    MOVE  "PRTNM25"    TO  MCP-WIDGET
                WHEN   202
                    MOVE  "PARAKBN01"  TO  MCP-WIDGET
                WHEN   204
                    MOVE  "PARAKBN02"  TO  MCP-WIDGET
                WHEN   206
                    MOVE  "PARAKBN03"  TO  MCP-WIDGET
                WHEN   208
                    MOVE  "PARAKBN04"  TO  MCP-WIDGET
                WHEN   210
                    MOVE  "PARAKBN05"  TO  MCP-WIDGET
                WHEN   212
                    MOVE  "PARAKBN06"  TO  MCP-WIDGET
                WHEN   214
                    MOVE  "PARAKBN07"  TO  MCP-WIDGET
                WHEN   216
                    MOVE  "PARAKBN08"  TO  MCP-WIDGET
                WHEN   218
                    MOVE  "PARAKBN09"  TO  MCP-WIDGET
                WHEN   220
                    MOVE  "PARAKBN10"  TO  MCP-WIDGET
                WHEN   222
                    MOVE  "PARAKBN11"  TO  MCP-WIDGET
                WHEN   224
                    MOVE  "PARAKBN12"  TO  MCP-WIDGET
                WHEN   226
                    MOVE  "PARAKBN13"  TO  MCP-WIDGET
                WHEN   228
                    MOVE  "PARAKBN14"  TO  MCP-WIDGET
                WHEN   230
                    MOVE  "PARAKBN15"  TO  MCP-WIDGET
                WHEN   232
                    MOVE  "PARAKBN16"  TO  MCP-WIDGET
                WHEN   234
                    MOVE  "PARAKBN17"  TO  MCP-WIDGET
                WHEN   236
                    MOVE  "PARAKBN18"  TO  MCP-WIDGET
                WHEN   238
                    MOVE  "PARAKBN19"  TO  MCP-WIDGET
                WHEN   240
                    MOVE  "PARAKBN20"  TO  MCP-WIDGET
                WHEN   242
                    MOVE  "PARAKBN21"  TO  MCP-WIDGET
                WHEN   244
                    MOVE  "PARAKBN22"  TO  MCP-WIDGET
                WHEN   246
                    MOVE  "PARAKBN23"  TO  MCP-WIDGET
                WHEN   248
                    MOVE  "PARAKBN24"  TO  MCP-WIDGET
                WHEN   250
                    MOVE  "PARAKBN25"  TO  MCP-WIDGET
                 WHEN   300
                    MOVE  "B12"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           IF      WRK-MCP-WIDGET(1:7) =   "SHELLID"
               MOVE    WRK-MCP-WIDGET(8:2) TO  WRK-IDX2
               COMPUTE SPA-GMN-CUR         =  (WRK-IDX2   -   1)  *   6
                                           +   1
               ADD     1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      WRK-MCP-WIDGET(1:8) =   "SHORIMSG"
               MOVE    WRK-MCP-WIDGET(9:2) TO  WRK-IDX2
               COMPUTE SPA-GMN-CUR         =  (WRK-IDX2   -   1)  *   6
                                           +   2
               ADD     1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      WRK-MCP-WIDGET(1:5) =   "PRTNM"
               MOVE    WRK-MCP-WIDGET(6:2) TO  WRK-IDX2
               COMPUTE SPA-GMN-CUR         =  (WRK-IDX2   -   1)  *   2
                                           +   201
               ADD     1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "シェル名は半角で入力して下さい"
                                                      TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "処理名は全角で入力して下さい"
                                                      TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "パラメタ種別が違います"   TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
               WHEN    "0050"
                   MOVE
                "処理対象が公費請求書のときパラメタ種別の選択ができます"
                                                      TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "印刷指示の更新ができませんでした"
                                                      TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W14"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "登録処理をします"    TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W14"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *    ＤＢクロース処理
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

