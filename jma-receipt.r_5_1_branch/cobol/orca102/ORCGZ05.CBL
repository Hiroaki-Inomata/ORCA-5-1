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
       PROGRAM-ID.                               ORCGZ05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ特定器材設定画面（Ｚ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.00.02    MCC-多々納   01/07/18  正式名称等追加
      *  01.01.01    MCC-藤原     01/09/26  JOINの修正      
      *  01.01.02    MCC-多々納   02/07/18  服用方法画面追加
      *  01.01.03    NACL-多々納  02/10/09  半角空白→全角空白追加
      *  02.00.00    NACL-多々納  03/06/13  一括入力へ変更
      *  02.00.01    NACL-多々納  05/12/09  登録コードチェック追加
      *                                     MONFUNC対応 他
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.05.00    NACL-多々納  10/04/XX  帳票名称追加対応
      *  04.06.00    NACL-多々納  12/08/16  期間変更警告追加
      *  04.07.00    NACL-多々納  12/08/21  ユーザ登録複写機能追加
      *  04.07.00    NACL-多々納  13/02/01  特定器材２を上限価格へ
      *  05.01.00    NACL-多々納  17/01/XX  ユーザー設定他GINBEE対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位表示対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    ＳＰＡエリア
           COPY    "Z010SCR-SPA".
      *
       01  SPA-Z05.
      *
      *    第五画面ＳＰＡエリア
           03  SPA-GMN-AREA.
               05  SPA-GMN-SRYCDH              PIC X(09).
               05  SPA-GMN-NAMEH               PIC X(100).
               05  SPA-GMN-YUKOSTYMDH          PIC X(09).
               05  SPA-GMN-YUKOEDYMDH          PIC X(09).
               05  SPA-GMN-YUKOSTYMD           PIC X(10).
               05  SPA-GMN-YUKOEDYMD           PIC X(10).
               05  SPA-GMN-KANANAME            PIC X(100).
               05  SPA-GMN-NAME                PIC X(100).
               05  SPA-GMN-FORMALNAME          PIC X(200).
               05  SPA-GMN-SHONAME             PIC X(100).
               05  SPA-GMN-TEN                 PIC 9(8)V9(2).
      *
               05  SPA-GMN-TENSIKIBETU-G.
                   07  SPA-GMN-TENSIKIBETU         PIC 9(1).
                   07  SPA-GMN-F1                  PIC X(1).
                   07  SPA-GMN-TENSIKIBETUMEI      PIC X(20).
               05  SPA-GMN-TENSIKIBETUCOB-G.
                   07  SPA-GMN-TENSIKIBETUCOB      PIC X(26)
                                                   OCCURS   7.
               05  SPA-TENSIKI-MAX             PIC 9(04).
      *
               05  SPA-GMN-JGNTEN              PIC 9(7).
               05  SPA-GMN-OLDTEN              PIC 9(8)V9(2).
      *
               05  SPA-GMN-OLDTENSKB-G.
                   07  SPA-GMN-OLDTENSKB           PIC 9(1).
                   07  SPA-GMN-F1                  PIC X(1).
                   07  SPA-GMN-OLDTENSKBMEI        PIC X(20).
               05  SPA-GMN-OLDTENSKBCOB-G.
                   07  SPA-GMN-OLDTENSKBCOB        PIC X(26)
                                                   OCCURS   7.
               05  SPA-OLDTENSIKI-MAX              PIC 9(04).
      *
               05  SPA-GMN-TANICD-G.
                   07  SPA-GMN-TANICD              PIC X(3).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TANINAME            PIC X(26).
               05  SPA-GMN-TANICDCOB-G.
                   07  SPA-GMN-TANICDCOB           PIC X(30)
                                                   OCCURS  200.
               05  SPA-TANICD-MAX             PIC 9(04).
      *
               05  SPA-GMN-TOKUKIZAIAGEKSNKBN-G.
                   07  SPA-GMN-TOKUKIZAIAGEKSNKBN  PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TOKUIZAIMEI         PIC X(26).
               05  SPA-GMN-TOKUKIZAIAGEKSNCOB-G.
                   07  SPA-GMN-TOKUKIZAIAGEKSNCOB  PIC X(30)
                                                   OCCURS   2.
               05  SPA-TOKUKIZAI-MAX             PIC 9(04).
      *
               05  SPA-GMN-KGNAGE              PIC X(2).
               05  SPA-GMN-JGNAGE              PIC X(2).
      *
               05  SPA-GMN-MEIUSESKB-G.
                   07  SPA-GMN-MEIUSESKB           PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-MEIUSESKBMEI        PIC X(26).
               05  SPA-GMN-MEIUSESKBCOB-G.
                   07  SPA-GMN-MEIUSESKBCOB        PIC X(30)
                                                   OCCURS   2.
               05  SPA-MEIUSESKB-MAX             PIC 9(04).
      *
               05  SPA-GMN-SANSOKBN-G.
                   07  SPA-GMN-SANSOKBN            PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-SANSOKBNMEI         PIC X(26).
               05  SPA-GMN-SANSOKBNCOB-G.
                   07  SPA-GMN-SANSOKBNCOB         PIC X(30)
                                                   OCCURS   7.
               05  SPA-SANSOKBN-MAX             PIC 9(04).
      *
               05  SPA-GMN-TOKUKIZAISBT1-G.
                   07  SPA-GMN-TOKUKIZAISBT1       PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TOKUKIZAISBT1MEI    PIC X(26).
               05  SPA-GMN-TOKUKIZAISBT1COB-G.
                   07  SPA-GMN-TOKUKIZAISBT1COB    PIC X(30)
                                                   OCCURS   4.
               05  SPA-TOKUKIZAISBT1-MAX             PIC 9(04).
      *
               05  SPA-GMN-TOKUKIZAISBT2-G.
                   07  SPA-GMN-TOKUKIZAISBT2       PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-TOKUKIZAISBT2MEI    PIC X(26).
               05  SPA-GMN-TOKUKIZAISBT2COB-G.
                   07  SPA-GMN-TOKUKIZAISBT2COB    PIC X(30)
                                                   OCCURS   3.
               05  SPA-TOKUKIZAISBT2-MAX             PIC 9(04).
      *
               05  SPA-GMN-DATAKBN-G.
                   07  SPA-GMN-DATAKBN             PIC 9(1).
                   07  SPA-GMN-F3                  PIC X(1).
                   07  SPA-GMN-DATAKBNMEI          PIC X(14).
               05  SPA-GMN-DATAKBNCOB-G.
                   07  SPA-GMN-DATAKBNCOB          PIC X(16)
                                                   OCCURS   3.
               05  SPA-DATAKBN-MAX             PIC 9(04).
      *
               05  SPA-GMN-KOUHYOJYUNNUM       PIC 9(09).
      ******   05  SPA-GMN-INPCHKKBN           PIC 9(01).
               05  SPA-GMN-KEIKASOCHIYMD       PIC X(10).
               05  SPA-GMN-CDKBN-BU            PIC 9(02).
               05  SPA-GMN-CDKBN-KBNNUM        PIC 9(03).
      *        複写元コード
               05  SPA-GMN-COPYCD              PIC X(09).
      *R02.2
      *        ユーザー単位
               05  SPA-GMN-USER-TANICD-G.
                   07  SPA-GMN-USER-TANICD     PIC X(03).
                   07  SPA-GMN-USER-F1         PIC X(01).
                   07  SPA-GMN-USER-TANICDMEI  PIC X(24).
      *
               05  SPA-GMN-CUR                 PIC 9(02).
      *
      *        金額履歴情報
               05  SPA-GMN-PRICE-AREA.
                   07  SPA-NAI-PRCSYORI        PIC 9(01).
                   07  SPA-GMN-PRICE-LIST-G.
                       09  SPA-GMN-PRICE-LIST  OCCURS   50.
                           11  SPA-GMN-TRENNUM    PIC 9(03).
                           11  SPA-GMN-TPRCYUKOSTYMD   PIC X(10).
                           11  SPA-GMN-TPRCYUKOEDYMD   PIC X(10).
                           11  SPA-GMN-TPRICE          PIC X(12).
      *
                           11  SPA-NAI-TPRCYUKOSTYMD   PIC X(08).
                           11  SPA-NAI-TPRCYUKOEDYMD   PIC X(08).
                           11  SPA-NAI-TPRICE          PIC 9(09)V99.
                   07  SPA-GMN-PRICEMAX        PIC 9(03).
                   07  SPA-GMN-SELNUM          PIC 9(03).
                   07  SPA-GMN-PRINPUT-G.
                       09  SPA-GMN-PRCYUKOSTYMD   PIC X(10).
                       09  SPA-GMN-PRCYUKOEDYMD   PIC X(10).
                       09  SPA-GMN-PRICE          PIC X(12).
      *
                       09  SPA-NAI-PRCYUKOSTYMD   PIC X(08).
                       09  SPA-NAI-PRCYUKOEDYMD   PIC X(08).
                       09  SPA-NAI-PRICE          PIC 9(09)V99.
      *
                       05  SPA-NAI-ENDKEIFLG      PIC 9(01).
      *
      *    第五画面内容
           03  SPA-NAI-AREA.
               05  SPA-NAI-YUKOSTYMD           PIC X(08).
               05  SPA-NAI-YUKOEDYMD           PIC X(08).
      *
               05  SPA-NAI-KEIFLG1             PIC 9(01).
               05  SPA-NAI-KEIFLG2             PIC 9(01).
      *
               05  SPA-NAI-KEIKASOCHIYMD       PIC X(08).
      *
               05  SPA-NAI-KEIFLG3             PIC 9(01).
      *!!!
               05  SPA-NAI-ERRCD               PIC X(04).
               05  SPA-NAI-KEIFLG4             PIC 9(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
           03  FLG-TOUROKU                       PIC 9(01).
           03  FLG-CHK                           PIC 9(01).
      *
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
           03  FLG-TENSUPRICE                    PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDX-1                             PIC 9(04).
           03  IDX-2                             PIC 9(04).
           03  IDXX                              PIC 9(04).
           03  IDXY                              PIC 9(04).
           03  IDXZ                              PIC 9(04).
      *
           03  IDC                               PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-GMN-CUR                       PIC X(30).
           03  WRK-ZIDMSG                        PIC X(70).
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 200.
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-YUKOEDYMD       PIC X(08).
      *v47
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-CD-MCNT             PIC 9(02).
           03  FLG-KANJI               PIC 9(01).
      *
           03  WRK-Z9                  PIC ZZ9.
           03  WRK-ZZ99                PIC ZZZZZZZZ9.99.
           03  WRK-SELNUM              PIC 9(03).
      *
           03  WRK-SRYLST-DD           PIC X(02).
      *
      *    更新テーブル名称
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
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
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *    ワーク点数マスタ
       01  WRK-TNS-TENSU-REC           PIC X(1290).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    点数マスタ器材金額
        01 TENSU-PRICE-REC.
           COPY    "CPTENSU-PRICE.INC".
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
      *----(01.00.01) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    点数附加情報更新内容
      *****COPY    "CPORCGZS02.INC".
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           COPY    "ORCA102SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            WRK-AREA
           INITIALIZE                            IDX-AREA
           INITIALIZE                            CNT-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z05
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z05                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z010                      TO  SPA-Z010FREE
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
      *
      *!!!
      *H29.11      金額一覧エラー
                   IF      SPA-ERRCD       NOT =  SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
      *!!!
                END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE                TO  LINKAREA
      *
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "Z05"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *    入力ＣＤより
               WHEN    "Z99"
                   MOVE    1                   TO  SPA-GMN-CUR
      *
      *    検索画面より
               WHEN    "Z98"
                   PERFORM 320-Z98SPASET-SEC
      *
      *H29.01
      *        ユーザ設定画面
               WHEN    "Z010"
                   PERFORM 310-SPASET-SEC
                   MOVE    1                   TO  SPA-GMN-CUR
      *
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *    点数マスタ該当データ読み出し
           INITIALIZE                            SPA-Z05
           INITIALIZE                            SPA-GMN-AREA
      *
           MOVE  SPA-Z01-SRYCD                   TO  SPA-GMN-SRYCDH
      *
           MOVE  SPA-Z01-YUKOSTYMD               TO  SPA-NAI-YUKOSTYMD
           IF  SPA-Z01-SYORIKBN                  =  2
               MOVE  SPA-Z01-YUKOSTYMDH          TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE  SPA-Z01-YUKOSTYMDH              TO  SPA-GMN-YUKOSTYMD
      *
           MOVE  SPA-Z01-YUKOEDYMD               TO  SPA-NAI-YUKOEDYMD
           IF  SPA-Z01-SYORIKBN                  =  2
                MOVE  SPA-Z01-YUKOEDYMDH         TO  SPA-GMN-YUKOEDYMDH
           END-IF
           MOVE  SPA-Z01-YUKOEDYMDH              TO  SPA-GMN-YUKOEDYMD
      *
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           IF      SPA-Z01-SELNUM      =   ZERO
               MOVE    ALL "9"             TO  TNS-YUKOSTYMD
               MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           ELSE
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           ELSE
               MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           END-IF
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
      *****    MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 3001-TENSU-SET-SEC
           ELSE
      *        単位コードをゼロとする
               MOVE    ZERO            TO  SPA-GMN-TANICD
      *        金額種別
               IF      SPA-Z01-SRYCD(1:3)  =   "059"
                   MOVE    1           TO  SPA-GMN-TENSIKIBETU
               END-IF
      *        経過措置年月日
               MOVE    "00000000"          TO  SPA-GMN-KEIKASOCHIYMD
           END-IF
      *    各リストボックス編集
           PERFORM 30012-COB-HEN-SEC
      *
      *****PERFORM  813-TENSUCLS-SEC
      *    点数マスタ付加情報検索
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ付加情報検索
      *        PERFORM 8001-TENSUPLUS-KENS-SEC
      *        IF      FLG-TENSUPLUS       =   ZERO
      *            MOVE    TNSP-INPCHKKBN  TO  SPA-GMN-INPCHKKBN
      *            帳票名称
      *            MOVE    TNSP-SHONAME    TO  SPA-GMN-SHONAME
      *        END-IF
      *H29.1
      *        点数マスタ付加情報情報
               PERFORM 30012-TENSUPLUS-HEN-SEC
      *        点数マスタ金額情報
               PERFORM 30013-TENSUPRICE-HEN-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット
      *****************************************************************
       3001-TENSU-SET-SEC                     SECTION.
      *
           MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    TNS-TEN             TO  SPA-GMN-TEN
           MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU
           MOVE    TNS-JGNTEN          TO  SPA-GMN-JGNTEN
           MOVE    TNS-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    TNS-OLDTENSKB       TO  SPA-GMN-OLDTENSKB
           MOVE    TNS-TANICD          TO  SPA-GMN-TANICD
           MOVE    TNS-TANINAME        TO  SPA-GMN-TANINAME
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN
           MOVE    TNS-KGNAGE          TO  SPA-GMN-KGNAGE
           MOVE    TNS-JGNAGE          TO  SPA-GMN-JGNAGE
           MOVE    TNS-MEIUSESKB       TO  SPA-GMN-MEIUSESKB
           MOVE    TNS-SANSOKBN        TO  SPA-GMN-SANSOKBN
           MOVE    TNS-TOKUKIZAISBT1   TO  SPA-GMN-TOKUKIZAISBT1
           MOVE    TNS-TOKUKIZAISBT2   TO  SPA-GMN-TOKUKIZAISBT2
           MOVE    TNS-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
           MOVE    TNS-DATAKBN         TO  SPA-GMN-DATAKBN  
           MOVE    TNS-KEIKASOCHIYMD   TO  SPA-NAI-KEIKASOCHIYMD
                                           WRK-LNKYMD
           IF      TNS-KEIKASOCHIYMD   =   SPACE
      *        経過措置日　空白はゼロとする
               MOVE    "00000000"          TO  SPA-GMN-KEIKASOCHIYMD
               MOVE    ZERO                TO  SPA-NAI-KEIKASOCHIYMD
           ELSE
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-KEIKASOCHIYMD
           END-IF
           MOVE    TNS-CDKBN-BU        TO  SPA-GMN-CDKBN-BU
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-GMN-CDKBN-KBNNUM
      *
           .
       3001-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   各リストボックス編集処理
      *****************************************************************
       30012-COB-HEN-SEC            SECTION.
      *
      *    データ区分
           MOVE    "0 なし"            TO  SPA-GMN-DATAKBNCOB (1)
           MOVE    "3 フィルム"        TO  SPA-GMN-DATAKBNCOB (2)
           MOVE    2                   TO  SPA-DATAKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               IF      SPA-GMN-DATAKBN     =   SPA-GMN-DATAKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                           TO  SPA-GMN-DATAKBN-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *    データ規格
      **** MOVE    1               TO  IDX-1
      **** MOVE    "000 "          TO  SPA-GMN-TANICDCOB(IDX-1)
           MOVE    ZERO            TO  SPA-TANICD-MAX
           MOVE    ZERO            TO  IDX-1
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       MTANI-MAX
               ADD     1               TO  IDX-1
               MOVE    MTANI-TANICD  (IDX)
                                       TO  SPA-GMN-TANICDCOB(IDX-1)(1:3)
               MOVE    MTANI-TANINAME(IDX)
                                       TO  SPA-GMN-TANICDCOB(IDX-1)(5:)
               MOVE    IDX-1           TO  SPA-TANICD-MAX
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               IF      SPA-GMN-TANICD     =   SPA-GMN-TANICDCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                           TO  SPA-GMN-TANICD-G
                   MOVE    SPA-TANICD-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    点数識別
           MOVE    "0     "            TO  SPA-GMN-TENSIKIBETUCOB (1)
           MOVE    "1 金額"            TO  SPA-GMN-TENSIKIBETUCOB (2)
           MOVE    "2 都道府県購入価格"
                                       TO  SPA-GMN-TENSIKIBETUCOB (3)
           MOVE    "5 ％加算"          TO  SPA-GMN-TENSIKIBETUCOB (4)
           MOVE    "9 乗算割合"        TO  SPA-GMN-TENSIKIBETUCOB (5)
           MOVE    5                   TO  SPA-TENSIKI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               IF      SPA-GMN-TENSIKIBETU =   SPA-GMN-TENSIKIBETUCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                           TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    SPA-TENSIKI-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
      *    旧点数識別
           MOVE    "0     "            TO  SPA-GMN-OLDTENSKBCOB (1)
           MOVE    "1 金額"            TO  SPA-GMN-OLDTENSKBCOB (2)
           MOVE    "2 都道府県購入価格"
                                       TO  SPA-GMN-OLDTENSKBCOB (3)
           MOVE    "5 ％加算"          TO  SPA-GMN-OLDTENSKBCOB (4)
           MOVE    "9 乗算割合"        TO  SPA-GMN-OLDTENSKBCOB (5)
           MOVE    5                   TO  SPA-OLDTENSIKI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSIKI-MAX
               IF      SPA-GMN-OLDTENSKB =   SPA-GMN-OLDTENSKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                           TO  SPA-GMN-OLDTENSKB-G
                   MOVE    SPA-OLDTENSIKI-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    加算年齢
           MOVE    "0 年齢加算なし"    TO  SPA-GMN-TOKUKIZAIAGEKSNCOB(1)
           MOVE    "1 年齢加算あり"    TO  SPA-GMN-TOKUKIZAIAGEKSNCOB(2)
           MOVE    2                   TO  SPA-TOKUKIZAI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAI-MAX
               IF      SPA-GMN-TOKUKIZAIAGEKSNKBN
                                       =   SPA-GMN-TOKUKIZAIAGEKSNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAIAGEKSNCOB (IDX)
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN-G
                   MOVE    SPA-TOKUKIZAI-MAX   TO  IDX
               END-IF
           END-PERFORM
      *    名称識別
           MOVE    "0 通常"            TO  SPA-GMN-MEIUSESKBCOB(1)
           MOVE    "1 医療機関登録"    TO  SPA-GMN-MEIUSESKBCOB(2)
           MOVE    2                   TO  SPA-MEIUSESKB-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIUSESKB-MAX
               IF      SPA-GMN-MEIUSESKB
                                       =   SPA-GMN-MEIUSESKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-MEIUSESKBCOB (IDX)
                                               TO  SPA-GMN-MEIUSESKB-G
                   MOVE    SPA-MEIUSESKB-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    酸素
           MOVE    "0 酸素、窒素以外"
                                       TO  SPA-GMN-SANSOKBNCOB (1)
           MOVE    "1 酸素補正率、高気圧酸素加算"
                                       TO  SPA-GMN-SANSOKBNCOB (2)
           MOVE    "2 定置式液化酸素貯槽"
                                       TO  SPA-GMN-SANSOKBNCOB (3)
           MOVE    "3 可搬式液化酸素容器"
                                       TO  SPA-GMN-SANSOKBNCOB (4)
           MOVE    "4 大型ボンベ"
                                       TO  SPA-GMN-SANSOKBNCOB (5)
           MOVE    "5 小型ボンベ"
                                       TO  SPA-GMN-SANSOKBNCOB (6)
           MOVE    "9 窒素"
                                       TO  SPA-GMN-SANSOKBNCOB (7)
           MOVE    7                   TO  SPA-SANSOKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANSOKBN-MAX
               IF      SPA-GMN-SANSOKBN    =   SPA-GMN-SANSOKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SANSOKBNCOB (IDX)
                                           TO  SPA-GMN-SANSOKBN-G
                   MOVE    SPA-SANSOKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
      *    特定器材１
           MOVE    "0 通常"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(1)
           MOVE    "2 酸素"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(2)
           MOVE    "3 高線量率イリジウム"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(3)
           MOVE    "4 コバルト"
                                       TO  SPA-GMN-TOKUKIZAISBT1COB(4)
           MOVE    4                   TO  SPA-TOKUKIZAISBT1-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT1-MAX
               IF      SPA-GMN-TOKUKIZAISBT1
                                           =   SPA-GMN-TOKUKIZAISBT1COB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAISBT1COB (IDX)
                                           TO  SPA-GMN-TOKUKIZAISBT1-G
                   MOVE    SPA-TOKUKIZAISBT1-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    特定器材２（上限価格（酸素））
           MOVE    "0 下記以外"        TO  SPA-GMN-TOKUKIZAISBT2COB (1)
           MOVE    "1 上限価格設定"    TO  SPA-GMN-TOKUKIZAISBT2COB (2)
           IF      SPA-GMN-TOKUKIZAISBT2   =   "1"
               MOVE    SPA-GMN-TOKUKIZAISBT2COB (2)
                                       TO  SPA-GMN-TOKUKIZAISBT2-G
           ELSE
               MOVE    SPA-GMN-TOKUKIZAISBT2COB (1)
                                       TO  SPA-GMN-TOKUKIZAISBT2-G
           END-IF
           MOVE    2                   TO  SPA-TOKUKIZAISBT2-MAX
           .
      *
       30012-COB-HEN-EXT.
           EXIT.
      *H29.1
      *****************************************************************
      *    点数マスタ付加情報セット処理
      *****************************************************************
       30012-TENSUPLUS-HEN-SEC         SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                 TENSUPLUS-REC
               MOVE    1                  TO  FLG-TENSUPLUS
           END-IF
           PERFORM UNTIL       FLG-TENSUPLUS  =   1
               IF     (TNSP-YUKOSTYMD      >   TNS-YUKOEDYMD )
      *R01.10
                 OR   (TNSP-YUKOEDYMD      <   TNS-YUKOSTYMD )
                   CONTINUE
               ELSE
      *            処方名
                   MOVE    TNSP-SHONAME    TO  SPA-GMN-SHONAME
      *
      *R02.2
      *            ユーザー単位
                   MOVE    SPACE           TO  SPA-GMN-USER-TANICD-G
                   MOVE    TNSP-USERTANICD TO  SPA-GMN-USER-TANICD
                   MOVE    TNSP-USERTANINAME
                                           TO  SPA-GMN-USER-TANICDMEI
      * 
                   MOVE    1               TO  FLG-TENSUPLUS
               END-IF
               IF      FLG-TENSUPLUS       =   ZERO
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30012-TENSUPLUS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額履歴編集処理
      *****************************************************************
       30013-TENSUPRICE-HEN-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-PRICE-AREA
      *
           MOVE    SPACE               TO  TENSU-PRICE-REC
           INITIALIZE                  TENSU-PRICE-REC
           MOVE    SPA-Z01-SRYCD       TO  TNSPR-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSPR-HOSPNUM
           MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
           MOVE    "tbl_tensu_price"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensu_price"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPRICE-READ-SEC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL   (FLG-TENSUPRICE       =   1 )
                       OR  (IDX                 >=  50 )
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-GMN-PRICEMAX
               MOVE    IDX                 TO  SPA-GMN-TRENNUM
                                                          (IDX)
               MOVE    TNSPR-YUKOSTYMD     TO  SPA-NAI-TPRCYUKOSTYMD
                                                          (IDX)
               MOVE    TNSPR-YUKOEDYMD     TO  SPA-NAI-TPRCYUKOEDYMD
                                                          (IDX)
               MOVE    TNSPR-PRICE         TO  SPA-NAI-TPRICE
                                                          (IDX)
               MOVE    TNSPR-PRICE         TO  WRK-ZZ99
               MOVE    WRK-ZZ99            TO  SPA-GMN-TPRICE
                                                          (IDX)
      *
               MOVE    TNSPR-YUKOSTYMD     TO  WRK-LNKYMD
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TPRCYUKOSTYMD
                                                          (IDX)
               MOVE    TNSPR-YUKOEDYMD     TO  WRK-LNKYMD
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TPRCYUKOEDYMD
                                                          (IDX)
      *
               MOVE    "tbl_tensu_price"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-TENSUPRICE-READ-SEC
           END-PERFORM
           MOVE    "tbl_tensu_price"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *!!!!!!!!!!!!!!!!!!!!!!!!!!
      *H29.11
      *    期間の空チェック
           MOVE    SPACE               TO  SPA-NAI-ERRCD
           IF      IDX                 >   ZERO
               PERFORM 300131-PRICE-KIKANCHEK-SEC
           END-IF
           .
       30013-TENSUPRICE-HEN-EXT.
           EXIT.
      *
      *H29.11
      *****************************************************************
      *    期間の空チェック処理
      *****************************************************************
       300131-PRICE-KIKANCHEK-SEC      SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-ERRCD
           MOVE    ZERO                TO  SPA-NAI-KEIFLG4
      *
           PERFORM VARYING     IDC     FROM    1   BY   1
                   UNTIL      (IDC     >   IDX )
                          OR  (SPA-NAI-ERRCD   NOT =   SPACE )
               IF      IDC             =   1
      *            １件目　マスタ開始日
                   IF      SPA-NAI-TPRCYUKOSTYMD (IDC)
                                       >   SPA-Z01-TYUKOSTYMD
                                               (SPA-Z01-SELNUM)
                       MOVE    "0059"          TO  SPA-NAI-ERRCD
                   END-IF
               END-IF
               IF      IDC             =   IDX
      *            最終履歴
                   IF      SPA-NAI-TPRCYUKOEDYMD (IDC)
                                       <   SPA-Z01-TYUKOEDYMD
                                               (SPA-Z01-SELNUM)
                       MOVE    "0059"          TO  SPA-NAI-ERRCD
                   END-IF
               END-IF
               IF     (SPA-ERRCD       =   SPACE )
                  AND (IDC             >   1     )
      *            前履歴の終了日の＋１日
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    SPA-NAI-TPRCYUKOEDYMD(IDC - 1 )
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY6-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       IF      LNK-DAY6-KEISAN NOT =
                                            SPA-NAI-TPRCYUKOSTYMD (IDC)
                           MOVE    "0059"          TO  SPA-NAI-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       300131-PRICE-KIKANCHEK-EXT.
           EXIT.
      *!!!!!!!!!!!!!!!!!!!!!!!!!!
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
      *        複写
               MOVE    SPA-Z98-SRYCD       TO  SPA-GMN-COPYCD
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-GMN-COPYCD
           END-IF 
           .
       320-Z98SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
      *            削除
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4031-DEL-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG1
                   END-IF
               WHEN    "0002"
      *            登録
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4121-TOUROKU-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG2
                   END-IF
      *H29.1
               WHEN    "0003"
      *            金額削除
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4203-RRK-DEL-SEC
                   END-IF
               WHEN    "0004"
      *            金額登録
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4212-RRK-TOUROKU-SEC
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
      *H29.11
           IF     (SPA-ZIDCD           =   "0003"
                                       OR  "0004" )
              AND (SPA-NAI-ERRCD   NOT =   SPACE  )
               MOVE    SPA-NAI-ERRCD       TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
           .
       330-ZID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE  "CHANGE"      TO  MCP-PUTTYPE
                   PERFORM  401-BACK-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        入力コード設定
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-INPUTCD-SEC
      *v47
      *        前回複写
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM  406-ZENCOPY-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE       TO  WRK-INPUTCD
                   PERFORM 409-KENSAKU-SEC
      *
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *ver.5.0
      *H29.01
      *        ユーザー項目登録
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM  407-B10-SYORI-SEC
      *H29.01
      *        金額削除
               WHEN    "CLICKED"       ALSO    "BS03"
                   PERFORM  4010-BS03-SYORI-SEC
      *        金額クリア
               WHEN    "CLICKED"       ALSO    "BS02"
                   PERFORM  4010-BS02-SYORI-SEC
      *        金額登録
               WHEN    "CLICKED"       ALSO    "BS12"
                   PERFORM  4011-BS12-SYORI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        複写先コード
               WHEN    "ACTIVATE"      ALSO    "COPYCD"
                   PERFORM 502-COPYCD-CHK-SEC
      *H29.1
      *        金額履歴選択
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 504-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "PRICELIST"
                   PERFORM 41015-LIST1SEL-SEC
      *
      *        一括入力分
               WHEN    OTHER
                   PERFORM 2001-ENTRY-SYORI-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *  　選択番号チェック
      *****************************************************************
       504-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    50                  TO  SPA-GMN-CUR
           INITIALIZE                      SPA-GMN-PRINPUT-G
           MOVE    ZERO                TO  SPA-NAI-PRCSYORI
      *
           MOVE    Z05-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF      SPA-GMN-SELNUM      =   ZERO
      *        新規
               MOVE    1                   TO  SPA-NAI-PRCSYORI
               MOVE    51                  TO  SPA-GMN-CUR
           ELSE
      *
               IF      (SPA-GMN-SELNUM     <   1   )  OR
                       (SPA-GMN-SELNUM     >   SPA-GMN-PRICEMAX)
  	           MOVE    "0051"              TO  SPA-ERRCD
               ELSE
                   PERFORM 3104-SELNUM-SYORI-SEC
                   MOVE    51                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       504-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       3104-SELNUM-SYORI-SEC       SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX
           MOVE    SPA-GMN-TPRCYUKOSTYMD (IDX)
                                       TO  SPA-GMN-PRCYUKOSTYMD
           MOVE    SPA-GMN-TPRCYUKOEDYMD (IDX)
                                       TO  SPA-GMN-PRCYUKOEDYMD
           MOVE    SPA-GMN-TPRICE (IDX)
                                       TO  SPA-GMN-PRICE
      *
           MOVE    SPA-NAI-TPRCYUKOSTYMD (IDX)
                                       TO  SPA-NAI-PRCYUKOSTYMD
           MOVE    SPA-NAI-TPRCYUKOEDYMD (IDX)
                                       TO  SPA-NAI-PRCYUKOEDYMD
           MOVE    SPA-NAI-TPRICE (IDX)
                                       TO  SPA-NAI-PRICE
      *
           .
       3104-SELNUM-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41015-LIST1SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-PRICEMAX
               IF      Z05-PRICELIST-SELECT (IDX) =   "T"
                   IF      IDX             =   SPA-GMN-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-PRICEMAX    TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
           END-IF
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    SPA-GMN-SELNUM      TO  Z05-SELNUM
               PERFORM 504-SELNUM-CHK-SEC
           END-IF
           .
       41015-LIST1SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       2001-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       2001-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
      *        登録時、内容変更の無い場合、エラーとしない
               IF     (FLG-TOUROKU         =   1  )   AND
                      (SPA-Z01-SYORIKBN    =   2  )
      *            更新処理の時のみ
                   PERFORM 4103-HENKOU-CHK-SEC
               END-IF
           END-IF
      *
      *    登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-TOUROKU         =   1     )
               PERFORM 4109-TOUROKU-CHK-SEC
           END-IF
      *
      *    金額登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-TOUROKU         =   2     )
               PERFORM 4109-PRICETOUROKU-CHK-SEC
           END-IF
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *H29.1
      *    ユーザー点数識別（2 ：ユーザーマスタ）のみ反映
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               CONTINUE
           ELSE
      *        金額SPA編集
               PERFORM 41011-GMN-SPA-SET02-SEC
               GO      TO      4101-GMN-SPA-SET-EXT
           END-IF
      *
           MOVE    Z05-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z05-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
           MOVE    Z05-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z05-NAME            TO  SPA-GMN-NAME
           MOVE    Z05-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *****MOVE    Z05-SHONAME         TO  SPA-GMN-SHONAME
      *
           MOVE    Z05-TEN             TO  SPA-GMN-TEN
           MOVE    Z05-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU-G
           MOVE    Z05-JGNTEN          TO  SPA-GMN-JGNTEN
           MOVE    Z05-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    Z05-OLDTENSKB       TO  SPA-GMN-OLDTENSKB-G
           MOVE    Z05-DATAKBN         TO  SPA-GMN-DATAKBN-G
           MOVE    Z05-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN-G
           MOVE    Z05-TANICD          TO  SPA-GMN-TANICD-G
           MOVE    Z05-KGNAGE          TO  SPA-GMN-KGNAGE
           MOVE    Z05-JGNAGE          TO  SPA-GMN-JGNAGE
           MOVE    Z05-MEIUSESKB       TO  SPA-GMN-MEIUSESKB-G
           MOVE    Z05-SANSOKBN        TO  SPA-GMN-SANSOKBN-G
           MOVE    Z05-TOKUKIZAISBT1   TO  SPA-GMN-TOKUKIZAISBT1-G
           MOVE    Z05-TOKUKIZAISBT2   TO  SPA-GMN-TOKUKIZAISBT2-G
           MOVE    Z05-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
     ***** MOVE    Z05-INPCHKKBN       TO  SPA-GMN-INPCHKKBN
           MOVE    Z05-KEIKASOCHIYMD   TO  SPA-GMN-KEIKASOCHIYMD
           MOVE    Z05-CDKBN-BU        TO  SPA-GMN-CDKBN-BU
           MOVE    Z05-CDKBN-KBNNUM    TO  SPA-GMN-CDKBN-KBNNUM
      *    金額SPA編集
           PERFORM 41011-GMN-SPA-SET02-SEC
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額SPA編集処理
      *****************************************************************
       41011-GMN-SPA-SET02-SEC          SECTION.
      *
           MOVE    Z05-PRCYUKOSTYMD    TO  SPA-GMN-PRCYUKOSTYMD
           MOVE    Z05-PRCYUKOEDYMD    TO  SPA-GMN-PRCYUKOEDYMD
           MOVE    Z05-PRICE           TO  SPA-GMN-PRICE
           .
      *
       41011-GMN-SPA-SET02-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    有効開始年月日チェック
           PERFORM 501-YUKOSTYMDCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    有効開始年月日チェック
           PERFORM 502-YUKOEDYMDCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    カナ名称チェック
           PERFORM 503-KANANAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    漢字名称チェック
           PERFORM 504-NAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    通称名称チェック
           PERFORM 505-FORMALNAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    帳票名称チェック
           PERFORM 505-SHONAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    金額種別チェック
           PERFORM 506-TENSIKIBETU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    旧金額種別チェック
           PERFORM 506-OLDTENSKB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    単位コード
           PERFORM 507-TANICD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    データ区分
           PERFORM 507-DATAKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    年齢加算
           PERFORM 507-TOKUKIZAI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    下限年齢
           PERFORM 5002-KGNAGE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    上限年齢
           PERFORM 5003-JGNAGE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    名称識別
           PERFORM 5003-MEIUSESKB-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    酸素等
           PERFORM 5003-SANSOKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    特定器材種別１
           PERFORM 5003-TOKUKIZAISBT1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    特定器材種別２
           PERFORM 5003-TOKUKIZAISBT2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    経過措置年月日チェック
           PERFORM 5003-KEIKASOCHIYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    別表番号チェック
           PERFORM 5003-CDKBN-BU-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    区分番号チェック
           PERFORM 5003-CDKBN-KBNNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *H29.1
      *    金額履歴
           IF     (SPA-ERRCD               =   SPACE )
               PERFORM 5003-PRCYUKOSTYMD-CHK-SEC
           END-IF
           IF     (SPA-ERRCD               =   SPACE )
               PERFORM 5003-PRCYUKOEDYMD-CHK-SEC
           END-IF
           IF     (SPA-ERRCD               =   SPACE )
               PERFORM 5003-PRICE-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC            SECTION.
      *
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       501-YUKOSTYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       502-YUKOEDYMDCHK-SEC                      SECTION.
      *
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "9999999"           TO  SPA-GMN-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ名称チェック処理
      *****************************************************************
       503-KANANAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    漢字名称チェック処理
      *****************************************************************
       504-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       504-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通称名称チェック処理
      *****************************************************************
       505-FORMALNAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-FORMALNAME  TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       505-FORMALNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    帳票名称チェック処理
      *****************************************************************
       505-SHONAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SHONAME     TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 410-ZENCHK-HEN-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SHONAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SHONAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    25                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       505-SHONAME-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　金額種別チェック
      *****************************************************************
       506-TENSIKIBETU-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               IF      SPA-GMN-TENSIKIBETU =   SPA-GMN-TENSIKIBETUCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                               TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TENSIKI-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-TENSIKIBETU-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　金額種別チェック
      *****************************************************************
       506-OLDTENSKB-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSIKI-MAX
               IF      SPA-GMN-OLDTENSKB =   SPA-GMN-OLDTENSKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                               TO  SPA-GMN-OLDTENSKB-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-OLDTENSIKI-MAX  TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-OLDTENSKB-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　単位コードチェック
      *****************************************************************
       507-TANICD-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-TANICD      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-TANICD
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-TANICD      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
               GO      TO              507-TANICD-CHK-EXT
           ELSE
               MOVE    SNUM-OUTEDT(13:3)       TO  SPA-GMN-TANICD
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TANICD-MAX  )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-TANICD     =   SPA-GMN-TANICDCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                               TO  SPA-GMN-TANICD-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TANICD-MAX      TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    11                  TO  SPA-GMN-CUR
               MOVE    "0008"              TO  SPA-ERRCD
           END-IF
           .
      *
       507-TANICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　データ区分チェック
      *****************************************************************
       507-DATAKBN-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               IF      SPA-GMN-DATAKBN =   SPA-GMN-DATAKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                               TO  SPA-GMN-DATAKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-DATAKBN-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       507-DATAKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　加算年齢チェック
      *****************************************************************
       507-TOKUKIZAI-CHK-SEC                      SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAI-MAX
               IF      SPA-GMN-TOKUKIZAIAGEKSNKBN
                                       =   SPA-GMN-TOKUKIZAIAGEKSNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAIAGEKSNCOB (IDX)
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TOKUKIZAI-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       507-TOKUKIZAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　下限年齢チェック
      *****************************************************************
       5002-KGNAGE-CHK-SEC                      SECTION.
      *
           IF      SPA-GMN-KGNAGE      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-KGNAGE
           END-IF
      *
           IF      SPA-GMN-KGNAGE      =   "AA"
                                       OR  "B3"
                                       OR  "BF"
               GO      TO      5002-KGNAGE-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-KGNAGE      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    14                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-KGNAGE
           END-IF
           .
      *
       5002-KGNAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　上限年齢チェック
      *****************************************************************
       5003-JGNAGE-CHK-SEC                      SECTION.
      *
           IF      SPA-GMN-JGNAGE      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-JGNAGE
           END-IF
      *
           IF      SPA-GMN-JGNAGE      =   "AA"
                                       OR  "B3"
                                       OR  "BF"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JGNAGE      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    15                  TO  SPA-GMN-CUR
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-JGNAGE
               END-IF
           END-IF
           .
      *
       5003-JGNAGE-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    名称識別チェック処理
      *****************************************************************
       5003-MEIUSESKB-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIUSESKB-MAX
               IF      SPA-GMN-MEIUSESKB
                                       =   SPA-GMN-MEIUSESKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-MEIUSESKBCOB (IDX)
                                               TO  SPA-GMN-MEIUSESKB-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-MEIUSESKB-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    16                  TO  SPA-GMN-CUR
           END-IF
           .
      *
      *
       5003-MEIUSESKB-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    酸素等チェック処理
      *****************************************************************
       5003-SANSOKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANSOKBN-MAX
               IF      SPA-GMN-SANSOKBN    =   SPA-GMN-SANSOKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SANSOKBNCOB (IDX)
                                           TO  SPA-GMN-SANSOKBN-G
                   MOVE    SPA-SANSOKBN-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5003-SANSOKBN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    特定器材種別１チェック処理
      *****************************************************************
       5003-TOKUKIZAISBT1-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT1-MAX
               IF      SPA-GMN-TOKUKIZAISBT1
                                           =   SPA-GMN-TOKUKIZAISBT1COB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAISBT1COB (IDX)
                                           TO  SPA-GMN-TOKUKIZAISBT1-G
                   MOVE    SPA-TOKUKIZAISBT1-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5003-TOKUKIZAISBT1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定器材種別２チェック処理
      *****************************************************************
       5003-TOKUKIZAISBT2-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT2-MAX
               IF      SPA-GMN-TOKUKIZAISBT2
                                           =   SPA-GMN-TOKUKIZAISBT2COB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TOKUKIZAISBT2COB (IDX)
                                           TO  SPA-GMN-TOKUKIZAISBT2-G
                   MOVE    SPA-TOKUKIZAISBT2-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    19                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5003-TOKUKIZAISBT2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　経過措置年月日チェック
      *****************************************************************
       5003-KEIKASOCHIYMD-CHK-SEC  SECTION.
      *
           IF      SPA-GMN-KEIKASOCHIYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-KEIKASOCHIYMD
           END-IF
      *
           MOVE    SPA-GMN-KEIKASOCHIYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-KEIKASOCHIYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-KEIKASOCHIYMD
           ELSE
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    21                  TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       5003-KEIKASOCHIYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  別表番号チェック
      *****************************************************************
       5003-CDKBN-BU-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-CDKBN-BU      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CDKBN-BU
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-CDKBN-BU    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    22                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-CDKBN-BU
           END-IF
           .
      *
       5003-CDKBN-BU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  区分番号チェック
      *****************************************************************
       5003-CDKBN-KBNNUM-CHK-SEC       SECTION.
      *
           IF      SPA-GMN-CDKBN-KBNNUM    =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CDKBN-KBNNUM
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-CDKBN-KBNNUM    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    23                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(13:3)   TO  SPA-GMN-CDKBN-KBNNUM
           END-IF
           .
      *
       5003-CDKBN-KBNNUM-CHK-EXT.
           EXIT.
      *H29.1
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       5003-PRCYUKOSTYMD-CHK-SEC            SECTION.
      *
           IF      WRK-MCP-WIDGET      =   "PRCYUKOSTYMD"
               IF      SPA-GMN-PRCYUKOSTYMD    =   SPACE
      *            点数マスタの開始日
                   MOVE    SPA-Z01-YUKOSTYMD
                                           TO  SPA-GMN-PRCYUKOSTYMD
               END-IF
           END-IF
           IF      SPA-GMN-PRCYUKOSTYMD    =   SPACE
               MOVE    SPACE           TO  SPA-NAI-PRCYUKOSTYMD
               GO      TO      5003-PRCYUKOSTYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PRCYUKOSTYMD    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD        TO  SPA-NAI-PRCYUKOSTYMD
               MOVE    WRK-LNKYMD      TO  SPA-GMN-PRCYUKOSTYMD
           ELSE
               MOVE    "0052"          TO  SPA-ERRCD
               MOVE    51              TO  SPA-GMN-CUR
           END-IF
      *
           IF     (SPA-ERRCD           =  SPACE )
             AND  (SPA-Z01-YUKOSTYMD   NOT =   SPA-NAI-PRCYUKOSTYMD)
      *        １日であること
               IF     SPA-NAI-PRCYUKOSTYMD(7:2)    =   "01"
                                                   OR  "00"
                   CONTINUE
               ELSE
                   MOVE    "0058"          TO  SPA-ERRCD
                   MOVE    51              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       5003-PRCYUKOSTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       5003-PRCYUKOEDYMD-CHK-SEC                      SECTION.
      *
           IF      WRK-MCP-WIDGET      =   "PRCYUKOEDYMD"
               IF      SPA-GMN-PRCYUKOEDYMD    =   SPACE
                   MOVE    "9999999"           TO  SPA-GMN-PRCYUKOEDYMD
               END-IF
           END-IF
           IF      SPA-GMN-PRCYUKOEDYMD    =   SPACE
               MOVE    SPACE               TO  SPA-NAI-PRCYUKOEDYMD
               GO      TO      5003-PRCYUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PRCYUKOEDYMD    TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-PRCYUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-PRCYUKOEDYMD
           ELSE
               MOVE    "0053"              TO  SPA-ERRCD
               MOVE    52                  TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-NAI-PRCYUKOEDYMD    =   "99999999"
               CONTINUE
           ELSE
      *        月末日を求める
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SPA-NAI-PRCYUKOEDYMD(1:6)
                                           TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-SRYLST-DD
               ELSE
                   MOVE    "31"                TO  WRK-SRYLST-DD
               END-IF
               IF     (SPA-NAI-PRCYUKOEDYMD(7:2)
                                       NOT =   WRK-SRYLST-DD)
                 AND  (SPA-NAI-ENDKEIFLG   =   ZERO  )
                   MOVE    "K053"              TO  SPA-ERRCD
                   MOVE    52                  TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-ENDKEIFLG
               END-IF
           END-IF
      *
           .
       5003-PRCYUKOEDYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　金額チェック
      *****************************************************************
       5003-PRICE-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-PRICE       =   SPACE
               MOVE    ZERO            TO  SPA-NAI-PRICE
               GO  TO  5003-PRICE-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-PRICE    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN         >   2     )
               MOVE    "0054"              TO  SPA-ERRCD
               MOVE    53                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI-PRICE
               MOVE    SPA-NAI-PRICE       TO  WRK-ZZ99
               MOVE    WRK-ZZ99            TO  SPA-GMN-PRICE
           END-IF
           .
        5003-PRICE-CHK-EXT.
           EXIT.
      *
      *v47
      ****************************************************************
      *    複写先コード処理
      ****************************************************************
       502-COPYCD-CHK-SEC      SECTION.
      *
           MOVE    Z05-COPYCD          TO  SPA-GMN-COPYCD
      *
           IF      SPA-GMN-COPYCD      =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      502-COPYCD-CHK-EXT
           END-IF
      *
           MOVE    26                  TO  SPA-GMN-CUR
      *    全角入力の時、４文字まで
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    Z05-COPYCD          TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-COPYCD
      *
      *    "//" 入力の時、検索画面へ
           IF     SPA-GMN-COPYCD(1:2)  =  "//"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
           ELSE
      *        入力コードチェック
               IF      SPA-GMN-COPYCD      NOT =   SPACE
                   MOVE    SPA-GMN-COPYCD      TO  WRK-INPUTCD
                   PERFORM 4010-SRYCD-CHK-SEC
               END-IF
           END-IF
      *
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-COPYCD  NOT =   SPACE )
      *        複写処理
               PERFORM 5021-COPYCD-SYORI-SEC
           END-IF
           .
       502-COPYCD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    複写チェック処理
      ****************************************************************
       5022-COPYCD-CDCHK-SEC      SECTION.
      *
      *    器材コード
           IF      (SPA-GMN-COPYCD(1:3)    =   "059" )  OR
                   (SPA-GMN-COPYCD(1:1)    =   "7"   )
               CONTINUE
           ELSE
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    26                  TO  SPA-GMN-CUR
           END-IF
           .
       5022-COPYCD-CDCHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    複写処理処理
      ****************************************************************
       5021-COPYCD-SYORI-SEC      SECTION.
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-GMN-COPYCD      TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *!!!!!!!!!!!!!!
      *H29.11
      *    器材は金額を反映しない
           IF     (FLG-TENSU           =   ZERO )
             AND  (SPA-GMN-COPYCD(1:1) =   "7"  )
               IF      TNS-MASTER-CLASS    =   2
                   MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
               ELSE
                   MOVE    "tbl_tensu_master"   TO  WRK-MCP-TABLE
               END-IF
      *        点数マスタ再検索
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                      TNS-TENSU-REC
               END-IF
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
      * 
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 50211-COPYTENSU-SET-SEC
               MOVE    SPACE               TO  SPA-GMN-COPYCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
           .
       5021-COPYCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡに複写
      *****************************************************************
       50211-COPYTENSU-SET-SEC         SECTION.
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
           MOVE    TNS-TEN             TO  SPA-GMN-TEN
           MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU
      *    都道府県購入価格は、１ 金額へ
           IF      TNS-TENSIKIBETU     =   2
               MOVE    1                   TO  SPA-GMN-TENSIKIBETU
           END-IF
           MOVE    TNS-TANICD          TO  SPA-GMN-TANICD
           MOVE    TNS-TANINAME        TO  SPA-GMN-TANINAME
           MOVE    TNS-DATAKBN         TO  SPA-GMN-DATAKBN
      *
           MOVE    TNS-JGNTEN          TO  SPA-GMN-JGNTEN
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  SPA-GMN-TOKUKIZAIAGEKSNKBN
           MOVE    TNS-KGNAGE          TO  SPA-GMN-KGNAGE
           MOVE    TNS-JGNAGE          TO  SPA-GMN-JGNAGE
      *
      *!!! 以下は複写対象外
      *    MOVE    TNS-OLDTEN          TO  SPA-GMN-OLDTEN
      *    MOVE    TNS-OLDTENSKB       TO  SPA-GMN-OLDTENSKB
      *    MOVE    TNS-MEIUSESKB       TO  SPA-GMN-MEIUSESKB
      *
           IF      (SPA-GMN-COPYCD(1:3)    =   "059" )
               MOVE    TNS-SANSOKBN        TO  SPA-GMN-SANSOKBN
               MOVE    TNS-TOKUKIZAISBT1   TO  SPA-GMN-TOKUKIZAISBT1
               MOVE    TNS-TOKUKIZAISBT2   TO  SPA-GMN-TOKUKIZAISBT2
           END-IF
      *
      *    各リストボックス編集
           PERFORM 30012-COB-HEN-SEC
      *
      *H29.1 出力名称は複写不可
      *    点数マスタ付加情報検索
      *    PERFORM 8001-TENSUPLUS-KENS-SEC
      *    IF      FLG-TENSUPLUS       =   ZERO
      *        帳票名称
      *        MOVE    TNSP-SHONAME        TO  SPA-GMN-SHONAME
      *!   END-IF
           .
       50211-COPYTENSU-SET-EXT.
           EXIT.
      *****************************************************************
      *  　入力コードチェック処理
      *****************************************************************
       4010-SRYCD-CHK-SEC                        SECTION.
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-INPUTCD         TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-INPUTCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
               GO      TO    4010-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9    )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (WRK-INPUTCD         NUMERIC  )
               CONTINUE
           ELSE
      *        入力コード検索
               PERFORM 409-KENSAKU-SEC
           END-IF
      *
           .
       4010-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           INITIALIZE                  SPA-Z98GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
      *        器材
               MOVE    "7"                 TO  SPA-Z98-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Z98-INPUTCD
               MOVE    "1"                 TO  SPA-Z98-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Z98-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-Z98-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-Z98-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-Z98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-Z98-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           ELSE
      *        画面編集
               PERFORM 41011-Z98-SET-SEC
           END-IF
      *
      *
           MOVE    "Z05"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z05"               TO  SPA-Z98-MOTOPG
      *
           IF      SPA-Z98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "Z98    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-Z98-SET-SEC                           SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z05             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z05
      *
           .
       41011-Z98-SET-EXT.
           EXIT.
      *****************************************************************
      *    前回コード複写処理
      *****************************************************************
       406-ZENCOPY-SEC          SECTION.
      *
           IF      SPA-Z08MAE-SRYCD        =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-Z08MAE-SRYCD    TO  SPA-GMN-COPYCD
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
               IF     (FLG-END             =   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )
      *            複写処理
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF 
           END-IF 
           .
       406-ZENCOPY-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >=  SPA-NAI-YUKOEDYMD
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    有効期限範囲チェック
           IF      SPA-Z01-SYORIKBN    =   2
      *        前チェック
               IF      SPA-Z01-SELNUM      >   1
                   MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM - 1)
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      **********   IF      SPA-NAI-YUKOSTYMD   NOT =   WRK-OTYMD
                   IF      SPA-NAI-YUKOSTYMD       <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *        後チェック
               IF      SPA-Z01-SELNUM      <   SPA-Z01-MAX
                   MOVE    SPA-NAI-YUKOEDYMD
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      **********   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
      ********                                 NOT =   WRK-OTYMD
                   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
                                                   <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *    改正チェック
           IF      SPA-Z01-SYORIKBN    =   3
               IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           >=  SPA-NAI-YUKOSTYMD
                   MOVE    "1012"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
               IF  SPA-NAI-YUKOEDYMD    NOT    =  ALL "9"
                   MOVE    "1013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
      *H24.8
      *        器材は期間変更警告
               IF      SPA-Z01-SRYCD(1:1)  =   "7"
                   IF      SPA-NAI-KEIFLG3     =   ZERO
                       MOVE    "K005"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *H24.8
      *    器材の期間変更チェック
           IF      SPA-Z01-SELNUM      >=  1
               IF      (SPA-Z01-SRYCD(1:1)  =   "7"      )
               AND    ((SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOSTYMD)
               OR      (SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOEDYMD))
                   IF      SPA-NAI-KEIFLG3     =   ZERO
                       MOVE    "K004"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    "1014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-PRICETOUROKU-CHK-SEC      SECTION.
      *
           IF     (SPA-Z01-SRYCD(1:1)  =   "7" )
      *        酸素等、都道府県購入価格
               IF    (SPA-GMN-TOKUKIZAISBT1   =   "2"
                                              OR  "3"
                                              OR  "4" )
                  OR (SPA-GMN-TENSIKIBETU     =   "2" )
                   CONTINUE
               ELSE
                   MOVE    "0056"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    "0056"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
      *        入力不可の時、クリア
               INITIALIZE                  SPA-GMN-PRINPUT-G
               GO      TO      4109-PRICETOUROKU-CHK-EXT
           END-IF
      *    登録時、必須入力チェック
           IF     (SPA-GMN-PRCYUKOSTYMD    =   SPACE )
            AND   (SPA-ERRCD               =   SPACE )
               MOVE    "0052"              TO  SPA-ERRCD
               MOVE    51                  TO  SPA-GMN-CUR
           END-IF
           IF     (SPA-GMN-PRCYUKOEDYMD    =   SPACE )
            AND   (SPA-ERRCD               =   SPACE )
               MOVE    "0053"              TO  SPA-ERRCD
               MOVE    52                  TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-NAI-PRCYUKOSTYMD   >=  SPA-NAI-PRCYUKOEDYMD
            AND   (SPA-ERRCD               =   SPACE )
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    52                  TO  SPA-GMN-CUR
           END-IF
      *
           IF     (SPA-NAI-PRICE       =   ZERO  )
            AND   (SPA-ERRCD           =   SPACE )
               MOVE    "0057"              TO  SPA-ERRCD
               MOVE    53                  TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4109-PRICETOUROKU-CHK-EXT
           END-IF
      *
      *    有効期限範囲チェック
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX  FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-PRICEMAX
               IF     IDX              =   SPA-GMN-SELNUM
                   CONTINUE
               ELSE
                   IF     (SPA-NAI-PRCYUKOEDYMD
                                       <   SPA-NAI-TPRCYUKOSTYMD
                                                             (IDX))
                     OR   (SPA-NAI-PRCYUKOSTYMD
                                       >   SPA-NAI-TPRCYUKOEDYMD
                                                             (IDX))
                       CONTINUE
                   ELSE
                       MOVE    "0055"        TO  SPA-ERRCD
                       MOVE    51            TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       4109-PRICETOUROKU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
      *    金額一覧期間エラーあり
           IF      SPA-NAI-ERRCD   NOT =   SPACE
               IF      SPA-NAI-KEIFLG4     NOT =   2
                   MOVE    "K059"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-NAI-KEIFLG4
                   MOVE    50                  TO  SPA-GMN-CUR
                   GO      TO      401-BACK-EXT
               END-IF
           END-IF
      *
           MOVE  "Z01"                           TO  SPA-SAKIPG.
      *
           MOVE  "Z05"                           TO  SPA-MOTOPG.
      *
      ***  MOVE  "CHANGE"                        TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG                      TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       401-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                     SECTION.
      *
           IF      SPA-NAI-KEIFLG1     =   ZERO
               IF      SPA-Z01-SRYCD(1:1)  =   "7"
                   MOVE    "K003"          TO  SPA-ERRCD
               END-IF
               MOVE    1               TO  SPA-NAI-KEIFLG1
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0001"              TO  SPA-ZIDCD
           END-IF
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設定分
               PERFORM 40311-TENSU-USER-DEL-SEC
           ELSE
      *        点数マスタ削除
               PERFORM 40312-TENSU-MAST-DEL-SEC
           END-IF
      *
           IF     (SPA-MASTER-CLASS    =   2  )
             AND  (SPA-Z01-SRYCD(1:1)  =   "7")
               CONTINUE
           ELSE
               IF     (SPA-Z01-SELNUM      =   1    )  AND
                      (SPA-Z01-MAX         =   1    )
      *            入力ＣＤ、付加情報等の削除
                   PERFORM 40313-ALL-DELETE-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               MOVE    SPACE               TO  SPA-Z08MAE-SRYCD
               MOVE  "JOIN"               TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ点数設定分削除処理
      *****************************************************************
       40311-TENSU-USER-DEL-SEC        SECTION.
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF     MCP-RC           NOT =   ZERO
               DISPLAY "Z05 TENSU DEL ERR!!"     MCP-RC
                       ",KEY:" TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
           .
       40311-TENSU-USER-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ点数設定分削除処理
      *****************************************************************
       40312-TENSU-MAST-DEL-SEC        SECTION.
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF     MCP-RC           NOT =   ZERO
               DISPLAY "Z05 TENSU DEL ERR!!"     MCP-RC
                       ",KEY:" TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
           .
       40312-TENSU-MAST-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　付加情報等の削除処理
      *****************************************************************
       40313-ALL-DELETE-SEC            SECTION.
      *
      *    入力コード削除
           IF     (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  ICD-SRYCD
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
      *        点数付加情報削除
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    点数付加情報削除
           MOVE    SPA-Z01-SRYCD       TO  TNSPR-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNSPR-HOSPNUM
           MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_price"   TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           END-IF
           .
       40312-TENSU-MAST-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                              SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z05             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z05
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z05"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z99GMN-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       405-INPUTCD-EXT.
           EXIT.
      *ver.5.0
      *****************************************************************
      *    ユーザー項目登録処理
      *****************************************************************
       407-B10-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *    変更はエラーでも遷移する
           IF      SPA-Z01-SYORIKBN    =   2
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
      *    IF     (SPA-Z01-SRYCD(1:1)  =   "7" )
      *        CONTINUE
      *    ELSE
      *        MOVE    "0050"              TO  SPA-ERRCD
      **   END-IF
      *    変更以外はエラー
           IF      SPA-Z01-SYORIKBN    NOT =   2
               MOVE    "0050"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      407-B10-SYORI-EXT
           END-IF
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    "Z010"              TO  SPA-SAKIPG
           MOVE    "Z05"               TO  SPA-MOTOPG
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ010"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z05
           MOVE    SPA-Z010FREE        TO  SPA-Z010
      *
      *    初期カーソル
           IF      SPA-Z010-RRKMAX     =   ZERO
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    "Z010"              TO  SPA-SAKIPG
           MOVE    "Z05"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
      *
       407-B10-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SYORIKBN    =   1
      *        新規処理
                   IF      SPA-NAI-KEIFLG2     =   ZERO
                       IF      SPA-Z01-SRYCD(1:1)  =   "7"
                           MOVE    "K001"          TO  SPA-ERRCD
                       END-IF
                       MOVE    1               TO  SPA-NAI-KEIFLG2
                   END-IF
               END-IF
           END-IF
      *
      *
      *    金額一覧期間エラーあり
           IF      SPA-NAI-ERRCD   NOT =   SPACE
               IF      SPA-NAI-KEIFLG4     NOT =   1
                   MOVE    "K059"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-NAI-KEIFLG4
                   MOVE    50                  TO  SPA-GMN-CUR
                   GO      TO      412-TOUROKU-EXT
               END-IF
           END-IF
      *
      *
      *    登録確認メッセージ
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0002"              TO  SPA-ZIDCD
           END-IF
           .    
       412-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、内容変更チェック処理
      ****************************************************************
       4103-HENKOU-CHK-SEC      SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           ELSE
               MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           END-IF
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-TENSU-REC(1:1290)   TO  WRK-TNS-TENSU-REC
      *        更新項目編集
               PERFORM 4121-KOUMOKUISO-SEC
               IF      TNS-TENSU-REC(1:1290)   =   WRK-TNS-TENSU-REC
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4103-HENKOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-TOUROKU-SEC                          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設登録
               PERFORM 41211-TENSU-USER-UPD-SEC
           ELSE
      *        点数マスタ登録
               PERFORM 41212-TENSU-MAST-UPD-SEC
           END-IF
      *    前画面に戻る
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-Z01-SRYCD       TO  SPA-Z08MAE-SRYCD
      *
               MOVE    "JOIN"              TO  MCP-PUTTYPE
               PERFORM 401-BACK-SEC
           END-IF
           .
       4121-TOUROKU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       41211-TENSU-USER-UPD-SEC        SECTION.
      *
           MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *H29.1
      *    点数マスタ付加情報更新
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 4129-TENSUPLUS-UPD-SEC
      *    END-IF
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
           .
       41211-TENSU-USER-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       41212-TENSU-MAST-UPD-SEC        SECTION.
      *
           IF     (SPA-MENTE-OK        =   1 )
      *    メンテナンスモードのみ、マスタ更新
           MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
           END-IF
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
           .
       41212-TENSU-MAST-UPD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    改正時処理
      *****************************************************************
       41211-KAISEI-SYORI-SEC                       SECTION.
      *
      *    有効終了日を決定する
      *    今回の有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOSTYMD   TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-YUKOEDYMD
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41211-KAISEI-SYORI-EXT
           END-IF
      *
      *    有効年月日を更新する
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新
               MOVE    WRK-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z05 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41211-KAISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41212-UPDATE-SYORI-SEC                       SECTION.
      *
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新項目編集
               MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
               PERFORM 4121-KOUMOKUISO-SEC
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z05 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41212-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41213-INSERT-SYORI-SEC          SECTION.
      *
      *    データＩＮＳＥＲＴ
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
           PERFORM 4121-KOUMOKUISO-SEC
      *
           MOVE    SMCNDATE-YMD        TO  TNS-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
           MOVE    SPA-OPID            TO  TNS-OPID
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z05 TENSU INS ERR:" MCP-RC
                       ",KEY:" TNS-KEY
           END-IF
      *
           .
       41213-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録編集処理
      *****************************************************************
       4121-KOUMOKUISO-SEC                       SECTION.
      *
           MOVE  SPA-GMN-KANANAME          TO  TNS-KANANAME
                                               WRK-STRINGTBL
           MOVE  100                       TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-KANAYUKOKETA
      *
           MOVE  SPA-GMN-NAME              TO  TNS-NAME
                                               WRK-STRINGTBL
           MOVE  100                       TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-YUKOKETA
      *
           MOVE  SPA-GMN-FORMALNAME        TO  TNS-FORMALNAME
                                               WRK-STRINGTBL
           MOVE  200                       TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-FORMALYUKOKETA
      *
           MOVE  SPA-GMN-TEN               TO  TNS-TEN
           MOVE  SPA-GMN-TENSIKIBETU       TO  TNS-TENSIKIBETU
           MOVE  SPA-GMN-JGNTEN            TO  TNS-JGNTEN
           MOVE  SPA-GMN-OLDTEN            TO  TNS-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB         TO  TNS-OLDTENSKB
           MOVE  SPA-GMN-TANICD            TO  TNS-TANICD
           MOVE  SPA-GMN-TANINAME          TO  TNS-TANINAME
           MOVE  SPA-GMN-TANINAME          TO  WRK-STRINGTBL
           MOVE  12                        TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-TANIMOJISU
           MOVE  SPA-GMN-TOKUKIZAIAGEKSNKBN
                                           TO  TNS-TOKUKIZAIAGEKSNKBN
           MOVE  SPA-GMN-KGNAGE            TO  TNS-KGNAGE
           MOVE  SPA-GMN-JGNAGE            TO  TNS-JGNAGE
           MOVE  SPA-GMN-MEIUSESKB         TO  TNS-MEIUSESKB
           MOVE  SPA-GMN-SANSOKBN          TO  TNS-SANSOKBN
           MOVE  SPA-GMN-TOKUKIZAISBT1     TO  TNS-TOKUKIZAISBT1
           MOVE  SPA-GMN-TOKUKIZAISBT2     TO  TNS-TOKUKIZAISBT2
           MOVE  SPA-GMN-KOUHYOJYUNNUM     TO  TNS-KOUHYOJYUNNUM
           MOVE  SPA-GMN-DATAKBN           TO  TNS-DATAKBN  
           MOVE  SPA-NAI-KEIKASOCHIYMD     TO  TNS-KEIKASOCHIYMD
           MOVE  SPA-GMN-CDKBN-BU          TO  TNS-CDKBN-BU
           MOVE  SPA-GMN-CDKBN-KBNNUM      TO  TNS-CDKBN-KBNNUM
           .
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタ付加情報更新処理
      *****************************************************************
      *4129-TENSUPLUS-UPD-SEC                       SECTION.
      *
      *
      *    INITIALIZE                  ORCGZS02AREA
      *    MOVE    SPA-NAI-YUKOSTYMD   TO  ZS02-NAI-YUKOSTYMD
      *    MOVE    SPA-NAI-YUKOEDYMD   TO  ZS02-NAI-YUKOEDYMD
      *    MOVE    WRK-YUKOEDYMD       TO  ZS02-YUKOEDYMD
      *
      *    INITIALIZE                  TENSUPLUS-REC
      *    MOVE    SPA-GMN-INPCHKKBN   TO  TNSP-INPCHKKBN
      *    処方名称
      *    MOVE    SPA-GMN-SHONAME     TO  TNSP-SHONAME
      *                                    WRK-STRINGTBL
      *    MOVE    100                 TO  CNT-STRING
      *    PERFORM 891-STRCOUNT-SEC
      *    MOVE    CNT-STRING          TO  TNSP-SHOYUKOKETA
      *    入力コード更新
      *    CALL    "ORCGZS02"          USING
      *                                MCPAREA
      *                                SPAAREA
      *                                ORCGZS02AREA
      *                                TENSUPLUS-REC
      *
      *    .
      *4129-TENSUPLUS-UPD-EXT.
      *****EXIT.
      *H29.1
      *****************************************************************
      *    金額クリア処理
      *****************************************************************
       4010-BS02-SYORI-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-PRINPUT-G
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    50                  TO  SPA-GMN-CUR
           .
       4010-BS02-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    金額削除処理
      *****************************************************************
       4010-BS03-SYORI-SEC             SECTION.
      *
           IF      (SPA-GMN-SELNUM     >=  1   )    AND
                   (SPA-GMN-SELNUM     <=  SPA-GMN-PRICEMAX)
      *        登録確認メッセージ
               MOVE    "0003"              TO  SPA-ZIDCD
           ELSE
               MOVE    "0051"              TO  SPA-ERRCD
               MOVE    50                  TO  SPA-GMN-CUR
           END-IF
           .    
       4010-BS03-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額登録処理
      *****************************************************************
       4011-BS12-SYORI-SEC             SECTION.
      *
           MOVE    2                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4011-BS12-SYORI-EXT
           END-IF
      *
      *    登録確認メッセージ
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0004"              TO  SPA-ZIDCD
           END-IF
           .    
       4011-BS12-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数金額情報削除処理
      *****************************************************************
       4203-RRK-DEL-SEC          SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  IDX
      *
      *    点数付加情報削除
           MOVE    SPA-Z01-SRYCD       TO  TNSPR-SRYCD
           MOVE    SPA-NAI-TPRCYUKOSTYMD(IDX)
                                       TO  TNSPR-YUKOSTYMD
           MOVE    SPA-NAI-TPRCYUKOEDYMD(IDX)
                                       TO  TNSPR-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNSPR-HOSPNUM
           MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_price"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z05 TENSU-PRICE DEL ERR!!"   MCP-RC
                       ",KEY:"   TNSPR-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        点数マスタ金額情報
               PERFORM 30013-TENSUPRICE-HEN-SEC
               MOVE    50              TO  SPA-GMN-CUR
           END-IF
           .
       4203-RRK-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ金額付加情報登録処理
      *****************************************************************
       4212-RRK-TOUROKU-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    ZERO                TO  SPA-NAI-PRCSYORI
               MOVE    SPA-GMN-SELNUM      TO  IDX
               MOVE    SPACE               TO  TENSU-PRICE-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSPR-SRYCD
               MOVE    SPA-NAI-TPRCYUKOSTYMD(IDX)
                                           TO  TNSPR-YUKOSTYMD
               MOVE    SPA-NAI-TPRCYUKOEDYMD(IDX)
                                           TO  TNSPR-YUKOEDYMD
      *
               MOVE    SPA-HOSPNUM         TO  TNSPR-HOSPNUM
               MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
               MOVE    "tbl_tensu_price"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensu_price"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSUPRICE
               END-IF
               MOVE    "tbl_tensu_price"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           ELSE
      *        新規
               MOVE    1                   TO  FLG-TENSUPRICE
               MOVE    1                   TO  SPA-NAI-PRCSYORI
           END-IF
      *
           EVALUATE    SPA-NAI-PRCSYORI
               WHEN    1
      *        新規
                   IF      FLG-TENSUPRICE      =   ZERO
                       MOVE    "9006"          TO  SPA-ERRCD
                   END-IF
               WHEN    0
      *        更新
                   IF      FLG-TENSUPRICE      =   1
                       MOVE    "9006"          TO  SPA-ERRCD
                   END-IF
           END-EVALUATE
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4212-RRK-TOUROKU-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-PRCSYORI
      *    新規
           WHEN    1
               INITIALIZE                  TENSU-PRICE-REC
               MOVE    SPA-Z01-SRYCD       TO  TNSPR-SRYCD
               MOVE    SPA-NAI-PRCYUKOSTYMD   TO  TNSPR-YUKOSTYMD
               MOVE    SPA-NAI-PRCYUKOEDYMD   TO  TNSPR-YUKOEDYMD
               MOVE    SPA-NAI-PRICE       TO  TNSPR-PRICE
      *
               MOVE    SPA-OPID            TO  TNSPR-OPID
               MOVE    SMCNDATE-YMD        TO  TNSPR-CREYMD
               MOVE    SMCNDATE-YMD        TO  TNSPR-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSPR-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSPR-HOSPNUM
               MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_tensu_price"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   MOVE    "9006"          TO  SPA-ERRCD
                   DISPLAY "Z010 TENSU-PRICE INS ERR:" MCP-RC
                          ",KEY:" TNSPR-KEY
               END-IF
      *    更新
           WHEN    0
               MOVE    SPA-Z01-SRYCD       TO  TNSPR-SRYCD
               MOVE    SPA-NAI-PRCYUKOSTYMD   TO  TNSPR-YUKOSTYMD
               MOVE    SPA-NAI-PRCYUKOEDYMD   TO  TNSPR-YUKOEDYMD
               MOVE    SPA-NAI-PRICE       TO  TNSPR-PRICE
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSPR-UP-SRYCD
               MOVE    SPA-NAI-TPRCYUKOSTYMD(IDX)
                                           TO  TNSPR-UP-YUKOSTYMD
               MOVE    SPA-NAI-TPRCYUKOEDYMD(IDX)
                                           TO  TNSPR-UP-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNSPR-UP-HOSPNUM
      *
               MOVE    SPA-OPID            TO  TNSP-OPID
               MOVE    SMCNDATE-YMD        TO  TNSP-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNSP-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSU-PRICE-REC     TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tensu_price"     TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "9006"          TO  SPA-ERRCD
                   DISPLAY "Z010 TENSU-PRICE UPD ERR:" MCP-RC
                                  ",KEY:" TNSPR-KEY
               END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
      *        点数マスタ金額情報
               PERFORM 30013-TENSUPRICE-HEN-SEC
               MOVE    50              TO  SPA-GMN-CUR
           END-IF
           .
      *
       4212-RRK-TOUROKU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       400-ZENCHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT 
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT 
           END-IF
           .
      *
       400-ZENCHK-EXT.
           EXIT.
      *****************************************************************
      *    半角変換（チェック）処理
      *****************************************************************
       410-ZENCHK-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT 
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT 
           END-IF
           .
      *
       410-ZENCHK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC             SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *
      *     画面ワークエリアにセット
           MOVE    SPACE               TO  Z05
           INITIALIZE                      Z05
      *
           MOVE  SPA-GMN-SRYCDH                TO  Z05-SRYCDH
           MOVE  SPA-GMN-NAMEH                 TO  Z05-NAMEH
           MOVE  SPA-GMN-YUKOSTYMDH            TO  Z05-YUKOSTYMDH
           MOVE  SPA-GMN-YUKOEDYMDH            TO  Z05-YUKOEDYMDH
           MOVE  SPA-GMN-YUKOSTYMD             TO  Z05-YUKOSTYMD
           MOVE  SPA-GMN-YUKOEDYMD             TO  Z05-YUKOEDYMD
           MOVE  SPA-GMN-KANANAME              TO  Z05-KANANAME
           MOVE  SPA-GMN-NAME                  TO  Z05-NAME
           MOVE  SPA-GMN-FORMALNAME            TO  Z05-FORMALNAME
           MOVE  SPA-GMN-SHONAME               TO  Z05-SHONAME
           MOVE  SPA-GMN-TEN                   TO  Z05-TEN
           MOVE  SPA-GMN-TENSIKIBETU-G         TO  Z05-TENSIKIBETU
           MOVE  SPA-GMN-JGNTEN                TO  Z05-JGNTEN
           MOVE  SPA-GMN-OLDTEN                TO  Z05-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB-G           TO  Z05-OLDTENSKB
           MOVE  SPA-GMN-TANICD-G              TO  Z05-TANICD
     ******MOVE  SPA-GMN-TANINAME              TO  Z05-TANINAME
           MOVE  SPA-GMN-TOKUKIZAIAGEKSNKBN-G  TO
                                               Z05-TOKUKIZAIAGEKSNKBN
           MOVE  SPA-GMN-KGNAGE                TO  Z05-KGNAGE
           MOVE  SPA-GMN-JGNAGE                TO  Z05-JGNAGE
           MOVE  SPA-GMN-MEIUSESKB-G           TO  Z05-MEIUSESKB
           MOVE  SPA-GMN-SANSOKBN-G            TO  Z05-SANSOKBN
           MOVE  SPA-GMN-TOKUKIZAISBT1-G       TO  Z05-TOKUKIZAISBT1
           MOVE  SPA-GMN-TOKUKIZAISBT2-G       TO  Z05-TOKUKIZAISBT2
           MOVE  SPA-GMN-KOUHYOJYUNNUM         TO  Z05-KOUHYOJYUNNUM
           MOVE  SPA-GMN-DATAKBN-G             TO  Z05-DATAKBN
      *
      *****MOVE  SPA-GMN-INPCHKKBN             TO  Z05-INPCHKKBN
      *
           MOVE  SPA-GMN-KEIKASOCHIYMD         TO  Z05-KEIKASOCHIYMD
           MOVE  SPA-GMN-CDKBN-BU              TO  Z05-CDKBN-BU
           MOVE  SPA-GMN-CDKBN-KBNNUM          TO  Z05-CDKBN-KBNNUM
      *
      *    データ区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                           TO  Z05-DATAKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-DATAKBN-MAX     TO  Z05-DATAKBN-COUNT
      *    データ規格
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               IF      SPA-GMN-TANICDCOB (IDX) NOT =   SPACE
                   ADD     1               TO  IDX-1
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                           TO  Z05-TANICD-ITEM (IDX-1)
               END-IF
           END-PERFORM
           MOVE    SPA-TANICD-MAX      TO  Z05-TANICD-COUNT
      *    点数識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                       TO  Z05-TENSIKIBETU-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-TENSIKI-MAX     TO  Z05-TENSIKIBETU-COUNT
      *     旧点数識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSIKI-MAX
               MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                           TO  Z05-OLDTENSKB-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-OLDTENSIKI-MAX      TO  Z05-OLDTENSKB-COUNT
      *
      *    加算年齢
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAI-MAX
               MOVE    SPA-GMN-TOKUKIZAIAGEKSNCOB (IDX)
                                           TO  Z05-TOKUKIZAIAGEKSN-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-TOKUKIZAI-MAX   TO  Z05-TOKUKIZAIAGEKSN-COUNT
      *    名称識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIUSESKB-MAX
               MOVE    SPA-GMN-MEIUSESKBCOB (IDX)
                                           TO  Z05-MEIUSESKB-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-MEIUSESKB-MAX   TO  Z05-MEIUSESKB-COUNT
      *    加算年齢
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANSOKBN-MAX
               MOVE    SPA-GMN-SANSOKBNCOB (IDX)
                                           TO  Z05-SANSOKBN-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-SANSOKBN-MAX    TO  Z05-SANSOKBN-COUNT
      *    特定器材１
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT1-MAX
               MOVE    SPA-GMN-TOKUKIZAISBT1COB (IDX)
                                           TO  Z05-TOKUKIZAISBT1-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-TOKUKIZAISBT1-MAX  TO  Z05-TOKUKIZAISBT1-COUNT
      *    特定器材１
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TOKUKIZAISBT2-MAX
               MOVE    SPA-GMN-TOKUKIZAISBT2COB (IDX)
                                           TO  Z05-TOKUKIZAISBT2-ITEM
                                                                (IDX)
           END-PERFORM
           MOVE    SPA-TOKUKIZAISBT2-MAX   TO  Z05-TOKUKIZAISBT2-COUNT
      *
      *H29.1
      *    金額履歴
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  SPA-GMN-PRICEMAX
               MOVE    SPA-GMN-TRENNUM (IDX)    TO  WRK-Z9
               MOVE    WRK-Z9              TO  Z05-TRENNUM (IDX)
               MOVE    SPA-GMN-TPRCYUKOSTYMD(IDX)
                                           TO  Z05-TPRCYUKOSTYMD(IDX)
               MOVE    SPA-GMN-TPRCYUKOEDYMD(IDX)
                                           TO  Z05-TPRCYUKOEDYMD(IDX)
               MOVE    SPA-GMN-TPRICE (IDX)
                                           TO  Z05-TPRICE (IDX)
               IF      SPA-GMN-SELNUM      =   IDX
                   MOVE    "T"             TO  Z05-PRICELIST-SELECT
                                                               (IDX)
               ELSE
                   MOVE    "F"             TO  Z05-PRICELIST-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-PRICEMAX    TO  Z05-PRICELIST-COUNT
           MOVE    SPA-GMN-SELNUM      TO  Z05-SELNUM
           MOVE    SPA-GMN-PRCYUKOSTYMD    TO  Z05-PRCYUKOSTYMD
           MOVE    SPA-GMN-PRCYUKOEDYMD    TO  Z05-PRCYUKOEDYMD
           MOVE    SPA-GMN-PRICE       TO  Z05-PRICE
      *v47
      *    複写元コード
           MOVE    SPA-GMN-COPYCD      TO  Z05-COPYCD
           MOVE    "前回複写"          TO  Z05-B06-LABEL
           MOVE    "検索"              TO  Z05-B09-LABEL
      *H29.1
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"  TO  Z05-MIDASHI-DATA
               MOVE    "red"               TO  Z05-MIDASHI-STYLE
           END-IF
      *!!!
      *    金額一覧エラー
           IF      SPA-NAI-ERRCD  NOT  =   SPACE
               STRING  "※金額一覧の期間とマスタの期間が違います。"
                       "金額一覧の期間外はマスタが無効です。"
                                         DELIMITED  BY  SPACE
                                           INTO  Z05-PRICEMSG-DATA
               END-STRING
               MOVE    "red"               TO  Z05-PRICEMSG-STYLE
           END-IF
      *R02.2
      *    ユーザー単位
           MOVE    SPA-GMN-USER-TANICD-G   TO  Z05-USER-TANICD
           MOVE    "black"             TO  Z05-USER-TANICD-STYLE
           IF      SPA-GMN-USER-TANICD     =   SPACE
                                           OR  ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-TANICD      =   SPACE
                                           OR  ZERO
                   CONTINUE
               ELSE
                   MOVE    "red"       TO  Z05-USER-TANICD-STYLE
               END-IF
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  Z05-COPYCD-STATE
           MOVE    WIDGET-NORMAL       TO  Z05-B06-STATE
           MOVE    WIDGET-NORMAL       TO  Z05-B09-STATE
      *    複写機能不可
           IF      SPA-Z01-SRYCD (1:1) =   "7"
               MOVE    WIDGET-INSENSITIVE      TO  Z05-COPYCD-STATE
               MOVE    WIDGET-INSENSITIVE      TO  Z05-B06-STATE
               MOVE    WIDGET-INSENSITIVE      TO  Z05-B09-STATE
      *
               MOVE    SPACE                   TO  Z05-B06-LABEL
               MOVE    SPACE                   TO  Z05-B09-LABEL
           END-IF
      *H29.1
      *    ユーザー点数識別（2 ：ユーザーマスタ）のみ更新
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               MOVE    WIDGET-NORMAL           TO  Z05-B12-STATE
               MOVE    WIDGET-NORMAL           TO  Z05-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL           TO  Z05-B12-STATE
               MOVE    WIDGET-INSENSITIVE      TO  Z05-B03-STATE
           END-IF
      *H30.9
      *    新規・改正処理はなし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               MOVE    WIDGET-INSENSITIVE      TO  Z05-B03-STATE
           END-IF
      *
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
               WHEN    2
                   MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
               WHEN    3
                   MOVE    "KANANAME"          TO  MCP-WIDGET
               WHEN    4
                   MOVE    "NAME"              TO  MCP-WIDGET
               WHEN    5
                   MOVE    "FORMALNAME"        TO  MCP-WIDGET
               WHEN    25
                   MOVE    "SHONAME"           TO  MCP-WIDGET
               WHEN    6
                   MOVE    "TEN"               TO  MCP-WIDGET
               WHEN    7
                   MOVE    "TENSIKIBETU"       TO  MCP-WIDGET
               WHEN    8
                   MOVE    "JGNTEN"               TO  MCP-WIDGET
               WHEN    9
                   MOVE    "OLDTEN"                TO  MCP-WIDGET
               WHEN    10
                   MOVE    "OLDTENSKB"             TO  MCP-WIDGET
               WHEN    11
                   MOVE    "TANICD"                TO  MCP-WIDGET
               WHEN    12
                   MOVE    "DATAKBN"               TO  MCP-WIDGET
               WHEN    13
                   MOVE    "TOKUKIZAIAGEKSNKBN"    TO  MCP-WIDGET
               WHEN    14
                   MOVE    "KGNAGE"                TO  MCP-WIDGET
               WHEN    15
                   MOVE    "JGNAGE"                TO  MCP-WIDGET
               WHEN    16
                   MOVE    "MEIUSESKB"             TO  MCP-WIDGET
               WHEN    17
                   MOVE    "SANSOKBN"              TO  MCP-WIDGET
               WHEN    18
                   MOVE    "TOKUKIZAISBT1"         TO  MCP-WIDGET
               WHEN    19
                   MOVE    "TOKUKIZAISBT2"         TO  MCP-WIDGET
               WHEN    20
                   MOVE    "KOUHYOJYUNNUM"         TO  MCP-WIDGET
               WHEN    21
                   MOVE    "KEIKASOCHIYMD"         TO  MCP-WIDGET
               WHEN    22
                   MOVE    "CDKBN_BU"              TO  MCP-WIDGET
               WHEN    23
                   MOVE    "CDKBN_KBNNUM"          TO  MCP-WIDGET
      ***      WHEN    24
      ***          MOVE    "INPCHKKBN"             TO  MCP-WIDGET
               WHEN    26
                   MOVE    "COPYCD"                TO  MCP-WIDGET
               WHEN    40
                   MOVE    "B12"                   TO  MCP-WIDGET
               WHEN    50
                   MOVE    "SELNUM"                TO  MCP-WIDGET
               WHEN    51
                   MOVE    "PRCYUKOSTYMD"          TO  MCP-WIDGET
               WHEN    52
                   MOVE    "PRCYUKOEDYMD"          TO  MCP-WIDGET
               WHEN    53
                   MOVE    "PRICE"                 TO  MCP-WIDGET
               WHEN    54
                   MOVE    "BS12"                  TO  MCP-WIDGET
             END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルセット処理
      *****************************************************************
       50011-CUR-SET-SEC                            SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOSTYMD"
                   MOVE    2                   TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    3                   TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    4                   TO  SPA-GMN-CUR
               WHEN    "NAME"
                   MOVE    5                   TO  SPA-GMN-CUR
               WHEN    "FORMALNAME"
      *****        MOVE    6                   TO  SPA-GMN-CUR
                   MOVE    25                  TO  SPA-GMN-CUR
               WHEN    "SHONAME"
                   MOVE    6                   TO  SPA-GMN-CUR
               WHEN    "TEN"
                   MOVE    7                   TO  SPA-GMN-CUR
               WHEN    "TENSIKIBETU"
                   MOVE    8                   TO  SPA-GMN-CUR
               WHEN    "JGNTEN"
                   MOVE    9                   TO  SPA-GMN-CUR
               WHEN    "OLDTEN"
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "OLDTENSKB"
                   MOVE    11                  TO  SPA-GMN-CUR
               WHEN    "TANICD"
                   MOVE    12                  TO  SPA-GMN-CUR
               WHEN    "DATAKBN"
                   MOVE    13                  TO  SPA-GMN-CUR
               WHEN    "TOKUKIZAIAGEKSNKBN"
                   MOVE    14                  TO  SPA-GMN-CUR
               WHEN    "KGNAGE"
                   MOVE    15                  TO  SPA-GMN-CUR
               WHEN    "JGNAGE"
                   MOVE    16                  TO  SPA-GMN-CUR
               WHEN    "MEIUSESKB"
                   MOVE    17                  TO  SPA-GMN-CUR
               WHEN    "SANSOKBN"
                   MOVE    18                  TO  SPA-GMN-CUR
               WHEN    "TOKUKIZAISBT1"
                   MOVE    19                  TO  SPA-GMN-CUR
               WHEN    "TOKUKIZAISBT2"
                   MOVE    20                  TO  SPA-GMN-CUR
               WHEN    "KOUHYOJYUNNUM"
                   MOVE    21                  TO  SPA-GMN-CUR
               WHEN    "KEIKASOCHIYMD"
                   MOVE    22                  TO  SPA-GMN-CUR
               WHEN    "CDKBN_BU"
                   MOVE    23                  TO  SPA-GMN-CUR
               WHEN    "CDKBN_KBNNUM"
      *H29.1       MOVE    24                  TO  SPA-GMN-CUR
                   MOVE    40                  TO  SPA-GMN-CUR
      *            金額履歴あり
                   IF     (SPA-Z01-SRYCD(1:1)  =   "7" )
      *                酸素等、都道府県購入価格
                       IF    (SPA-GMN-TOKUKIZAISBT1   =   "2"
                                                  OR  "3"
                                                  OR  "4"  )
                          OR (SPA-GMN-TENSIKIBETU  =   "2" )
                           MOVE    50              TO  SPA-GMN-CUR
                       END-IF
                   END-IF
      ***      WHEN    "INPCHKKBN"
      ***          MOVE    40                  TO  SPA-GMN-CUR
      *
               WHEN    "SELNUM"
                   MOVE    51                  TO  SPA-GMN-CUR
               WHEN    "PRCYUKOSTYMD"
                   MOVE    52                  TO  SPA-GMN-CUR
               WHEN    "PRCYUKOEDYMD"
                   MOVE    53                  TO  SPA-GMN-CUR
               WHEN    "PRICE"
                   MOVE    54                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効開始年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効終了年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "カナ名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "漢字名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "正式名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "出力名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "金額種別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "旧金額種別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "単位コードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "データ区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "年齢加算がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "下限年齢がエラーです"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "上限年齢がエラーです"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "名称識別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "酸素等がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "特定器材種別１がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
      ***********  MOVE    "特定器材種別２がありません"
                   MOVE    "上限価格（酸素）がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "経過措置年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "別表番号が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "区分番号が違います"
                                                 TO  SPA-ERRMSG
      *v47
               WHEN    "0020"
                   STRING  "複写できるコードではありません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0021"
                   STRING  "複写元コードが登録されていません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
      *H29.1
               WHEN    "0050"
                   STRING   "ユーザー設定はできません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "0051"
                   STRING   "選択番号エラー"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0052"
                   MOVE     "有効開始日エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0053"
                   MOVE     "有効終了日エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0054"
                   MOVE     "金額入力エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0057"
                   MOVE     "金額の入力がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0055"
                   MOVE     "有効期間が重複します"
                                                 TO  SPA-ERRMSG
               WHEN    "0056"
                   STRING   "金額履歴の登録できないコードです。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0058"
                   MOVE    "有効開始年月日は１日にしてください"
                                                 TO  SPA-ERRMSG
               WHEN    "K053"
                   STRING  "警告！"
                           "有効終了年月日が月末日ではありません"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0059"
                   STRING   "金額一覧とマスタの期間が違います。"
                            "金額一覧の期間外はマスタが無効です。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K059"
                   STRING  "警告！"
                            "金額一覧とマスタの期間が違います。"
                            "金額一覧の期間外はマスタが無効です。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *
      *        以下共通
               WHEN    "1010"
                   MOVE
                   "有効開始年月日≧有効終了年月日で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "有効年月日の範囲が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "有効年月日が重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "改正の終了年月日はＡＬＬ９のみ入力できます"
                                                 TO  SPA-ERRMSG
               WHEN    "1014"
                   MOVE    "漢字名称を入力してください"
                                                 TO  SPA-ERRMSG
              WHEN    "1001"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "追加処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "点数マスタ付加情報が更新できません"
                                                 TO  SPA-ERRMSG
               WHEN    "9006"
                   MOVE    "点数マスタ金額情報が更新できません"
                                                 TO  SPA-ERRMSG
               WHEN    "K001"
                   STRING    "警告！厚生労働省から提供される"
                                                   DELIMITED BY  SIZE
                             "コードの範囲です。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING    "警告！ユーザ登録できる範囲のコード"
                                                   DELIMITED BY  SIZE
                             "ではありません。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING   "警告！厚生労働省から提供されたデータです。"
                                                   DELIMITED BY  SIZE
                            "削除しないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H24.8
               WHEN    "K004"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K005"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。改正処理はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *
          END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                TO  ZERR-ERRMSG
      *
           MOVE    "Z05"                         TO  SPA-MOTOPG
           MOVE    "ZERR"                        TO  SPA-SAKIPG
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE
           MOVE    "ZERR"                        TO  MCP-WINDOW
           MOVE    "B01"                         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END    
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0003"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0004"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z05"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　処理
      *****************************************************************
      *
      *****************************************************************
      *   点数マスタ付加情報検索処理
      *****************************************************************
       8001-TENSUPLUS-KENS-SEC                        SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                 TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       8001-TENSUPLUS-KENS-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPRICE-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSU-PRICE-REC
               MOVE    ZERO            TO  FLG-TENSUPRICE
           ELSE
               INITIALIZE                  TENSU-PRICE-REC
               MOVE    1               TO  FLG-TENSUPRICE
           END-IF
           .    
      *
       990-TENSUPRICE-READ-EXT.
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
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    日付変換　処理
      *****************************************************************
      *****************************************************************
      *     対象日＋１日計算処理
      *****************************************************************
       800-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-YMD
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-OTYMD
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-OTYMD
               WHEN    OTHER
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
                   ELSE
                       MOVE    SPACE                TO  WRK-OTYMD
                   END-IF
           END-EVALUATE
      *
           .
       800-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:7)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "9999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                       MOVE    ZERO                TO  FLG-ERR
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
           END-EVALUATE
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→和暦（機能２１）
      *****************************************************************
       832-YMDCONV21-SEC                          SECTION.
      *
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO 
               WHEN  "99999999"
                   MOVE    WRK-LNKYMD            TO  WRK-HENYMDG
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY2-AREA
                  MOVE  "21"                     TO  LNK-DAY2-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY2-YMD
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  IF      SGDAY-RC            =   ZERO
                      MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
                  ELSE
                      MOVE    SPACE                  TO  WRK-HENYMDG
                  END-IF
           END-EVALUATE
           .
      *
       832-YMDCONV21-EXT.
           EXIT.
      *
      *****************************************************************
      *   文字数カウント処理（全角のみ）
      *****************************************************************
       891-STRCOUNT-SEC                         SECTION.
      *
           PERFORM  UNTIL  WRK-STRING(CNT-STRING)  NOT  =  SPACE
                    OR  CNT-STRING  =  1
               COMPUTE  CNT-STRING  =  CNT-STRING  -  1
           END-PERFORM
      *
           IF  CNT-STRING                        =  1
               MOVE  ZERO                        TO  CNT-STRING
           ELSE
               COMPUTE  CNT-STRING               =  CNT-STRING  /  2
           END-IF
           .
       891-STRCOUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
