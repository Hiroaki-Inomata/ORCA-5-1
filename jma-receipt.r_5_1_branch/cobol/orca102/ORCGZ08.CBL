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
       PROGRAM-ID.                               ORCGZ08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ特定器材商品名設定画面（Ｚ０８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/20    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  11/12/15  金額・単位対応
      *  04.08.00    NACL-多々納  17/03/XX  商品名一覧対応
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
       01  SPA-Z08.
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
      *
               05  SPA-GMN-TEN                 PIC 9(8)V9(2).
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
               05  SPA-GMN-IDOKANREN           PIC X(09).
               05  SPA-GMN-IDOKIKAN            PIC X(30).
               05  SPA-GMN-IDOKANRENNAME       PIC X(100).
      *
               05  SPA-GMN-MSGLABEL            PIC X(100).
      *
               05  SPA-GMN-COPYCD              PIC X(09).
      *
               05  SPA-GMN-CUR                 PIC 9(02).
      *H29.3
      *        商品名一覧
               05  SPA-GMN-SHONAME-LIST-G.
                   07  SPA-GMN-SHONAME-LIST    OCCURS   200.
                       09  SPA-GMN-RENNUM      PIC 9(03).
                       09  SPA-GMN-SHONAME     PIC X(200).
                       09  SPA-GMN-KIKAKU      PIC X(120).
      *
                       09  SPA-NAI-KANRINO     PIC X(08).
               05  SPA-GMN-SHONAME-MAX         PIC 9(03).
      *
               05  SPA-SHO-PAGE                PIC 9(03).
               05  SPA-SHO-NEXT                PIC 9(01).
      *        選択
               05  SPA-GMN-SELNUM              PIC X(03).
               05  SPA-NAI-SELNUM              PIC 9(03).
      *
      *    第五画面内容
           03  SPA-NAI-AREA.
               05  SPA-NAI-YUKOSTYMD           PIC X(08).
               05  SPA-NAI-YUKOEDYMD           PIC X(08).
      *
               05  SPA-NAI-IDOKAN              PIC 9(01).
               05  SPA-NAI-KEIFLG1             PIC 9(01).
               05  SPA-NAI-KEIFLG2             PIC 9(01).
      *
               05  SPA-NAI-IDOSTYMD            PIC X(08).
               05  SPA-NAI-IDOEDYMD            PIC X(08).
               05  SPA-MAE-IDOKANREN           PIC X(09).
               05  SPA-NAI-COPY                PIC X(01).
               05  SPA-NAI-TENSIKIBETU         PIC X(01).
               05  SPA-NAI-TANICD              PIC X(03).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
           03  FLG-TOUROKU                       PIC 9(01).
           03  FLG-KANJI                         PIC 9(01).
      *
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
           03  FLG-MATERIAL-H                    PIC 9(01).
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
           03  IDX-STR                           PIC 9(04).
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
      *
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-CD-MCNT             PIC 9(02).
      *    関連算定コード
           03  WRK-TNS-COPY-G.
               05  WRK-TNS-YUKOSTYMD       PIC X(08).
               05  WRK-TNS-YUKOEDYMD       PIC X(08).
               05  WRK-TNS-NAME            PIC X(100).
               05  WRK-TNS-TEN             PIC 9(09)V9(2).
               05  WRK-TNS-TENSIKIBETU     PIC X(01).
               05  WRK-TNS-TANICD          PIC X(03).
               05  WRK-TNS-TANINAME        PIC X(30).
           03  WRK-HEN-STYMD           PIC X(09).
           03  WRK-HEN-EDYMD           PIC X(09).
      *
           03  WRK-H-TEN               PIC ZZZZZZZ9.99.
           03  WRK-H-TANINAME          PIC X(30).
           03  WRK-H-TENSIKIMEI        PIC X(30).
      *
           03  WRK-Z9                  PIC ZZ9.
           03  WRK-ZZZ                 PIC ZZZ.
           03  WRK-SELNUM              PIC 9(03).
      *
           03  WRK-MATERH-SNAME        PIC X(200).
           03  WRK-MATERH-KIKAKU       PIC X(120).
      *
           03  WRK-MCP-TABLE       PIC X(64).
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
      * 01 TENSUPLUS-REC.
      *    COPY    "CPTENSUPLUS.INC".
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *    特定器材品目ビュー
        01  MATERIAL-H-REC.
           COPY    "CPMATERIAL-H.INC".
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
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z08
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
           MOVE    SPA-Z08                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
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
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE                TO  LINKAREA
      *
               MOVE    SPACE                TO  MCP-PUTTYPE
               MOVE    "Z08"                TO  MCP-WINDOW
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
      *    検索画面より
               WHEN    "Z98"
                   PERFORM 320-Z98SPASET-SEC
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *    入力ＣＤより
               WHEN    "Z99"
                   MOVE    1                   TO  SPA-GMN-CUR
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
      *
      *    点数マスタ該当データ読み出し
           INITIALIZE                            SPA-Z08
           INITIALIZE                            SPA-GMN-AREA
      *
           MOVE  SPA-Z01-SRYCD                   TO  SPA-GMN-SRYCDH
      *
           MOVE  SPA-Z01-YUKOSTYMD               TO  SPA-NAI-YUKOSTYMD
           IF    SPA-Z01-SYORIKBN                  =  2
               MOVE  SPA-Z01-YUKOSTYMDH          TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE  SPA-Z01-YUKOSTYMDH              TO  SPA-GMN-YUKOSTYMD
      *
           MOVE  SPA-Z01-YUKOEDYMD               TO  SPA-NAI-YUKOEDYMD
           IF     SPA-Z01-SYORIKBN                  =  2
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *        点数マスタ編集
               PERFORM 3001-TENSU-SET-SEC
      *
               MOVE    SPA-Z01-SRYCD       TO  SPA-Z08MAE-SRYCD
           END-IF
      *    各リストボックス編集
           PERFORM 30012-COB-HEN-SEC
      *
      *    点数マスタ付加情報検索
      *    IF      FLG-TENSU           =   ZERO
      *        MOVE    SPACE               TO  TENSUPLUS-REC
      *        MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *        MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
      *        MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
      *        MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *        MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *        MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        PERFORM 910-DBSELECT-SEC
      *        IF      MCP-RC               =   ZERO
      *            MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *            MOVE    "key"               TO  MCP-PATHNAME
      *            PERFORM 990-TENSUPLUS-READ-SEC
      *        ELSE
      *            INITIALIZE                 TENSUPLUS-REC
      *        END-IF
      *        MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        PERFORM 990-DBLOSEC-SEC
      **** END-IF
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
      *****MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    TNS-IDOKANREN       TO  SPA-GMN-IDOKANREN
           MOVE    TNS-IDOKANREN       TO  SPA-MAE-IDOKANREN
      *    金額
           MOVE    TNS-TEN             TO  SPA-GMN-TEN
      *    単位コード
           MOVE    TNS-TANICD          TO  SPA-GMN-TANICD
           MOVE    TNS-TANINAME        TO  SPA-GMN-TANINAME
      *    関連算定コード編集
           IF      SPA-GMN-IDOKANREN   NOT =   SPACE
               PERFORM 30011-IDOKANREN-HEN-SEC
      *H29.3
               IF     (SPA-ERRCD           =   SPACE)
                   MOVE    1               TO  SPA-SHO-PAGE
                   MOVE    ZERO            TO  SPA-SHO-NEXT
      *            商品名一覧編集
                   PERFORM 5011-MATERIALH-HEN-SEC
               END-IF
           END-IF
      *
           .
       3001-TENSU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   関連算定コード編集処理
      *****************************************************************
       30011-IDOKANREN-HEN-SEC            SECTION.
      *
      *H29.11
      *    ユーザーで検索
           MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           PERFORM 300111-IDOKANREN-TENSU-SEC
           IF      WRK-TNS-NAME        =   SPACE
      *        マスターで検索
               MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
               PERFORM 300111-IDOKANREN-TENSU-SEC
           END-IF
      *
           MOVE    WRK-TNS-NAME        TO  SPA-GMN-IDOKANRENNAME
           MOVE    WRK-TNS-YUKOSTYMD   TO  SPA-NAI-IDOSTYMD
           MOVE    WRK-TNS-YUKOEDYMD   TO  SPA-NAI-IDOEDYMD
           MOVE    WRK-TNS-TENSIKIBETU TO  SPA-NAI-TENSIKIBETU
           MOVE    WRK-TNS-TANICD      TO  SPA-NAI-TANICD
           IF      WRK-TNS-YUKOSTYMD   =   SPACE
               MOVE    SPACE               TO  SPA-GMN-IDOKIKAN
           ELSE
      *        期間編集
               MOVE    SPACE               TO  SPA-GMN-IDOKIKAN
               MOVE    WRK-TNS-YUKOSTYMD   TO  WRK-LNKYMD
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  WRK-HEN-STYMD
               MOVE    WRK-TNS-YUKOEDYMD   TO  WRK-LNKYMD
               PERFORM 832-YMDCONV21-SEC
               MOVE    WRK-HENYMDG         TO  WRK-HEN-EDYMD
               STRING  WRK-HEN-STYMD
                       "〜"
                       WRK-HEN-EDYMD       DELIMITED  BY  SIZE
                                           INTO  SPA-GMN-IDOKIKAN
               END-STRING
           END-IF
      *    金額・点数識別・単位編集
           PERFORM 30011-MSGLABELHEN-SEC
           .
      *
       30011-IDOKANREN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   関連算定コード編集処理
      *****************************************************************
       300111-IDOKANREN-TENSU-SEC      SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-GMN-IDOKANREN   TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
      ******   MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    SPACE               TO  WRK-TNS-YUKOSTYMD
           MOVE    SPACE               TO  WRK-TNS-YUKOEDYMD
           MOVE    SPACE               TO  WRK-TNS-NAME
           INITIALIZE                      WRK-TNS-COPY-G
           MOVE    ZERO                TO  SPA-NAI-IDOKAN
           PERFORM
                       UNTIL   (FLG-TENSU      =   1  )
                           OR  (SPA-ERRCD  NOT =   SPACE )
      *        酸素・窒素、点数％加算は対象外
               IF     (TNS-SANSOKBN        =   0  )  AND
                      (TNS-TENSIKIBETU     =   1
                                           OR  2  )
      *        期間内
                   MOVE    TNS-YUKOSTYMD       TO  WRK-TNS-YUKOSTYMD
                   MOVE    TNS-YUKOEDYMD       TO  WRK-TNS-YUKOEDYMD
                   MOVE    TNS-NAME            TO  WRK-TNS-NAME
      *
                   MOVE    TNS-TEN             TO  WRK-TNS-TEN
                   MOVE    TNS-TANICD          TO  WRK-TNS-TANICD
                   MOVE    TNS-TANINAME        TO  WRK-TNS-TANINAME
                   MOVE    TNS-TENSIKIBETU     TO  WRK-TNS-TENSIKIBETU
      *
                   MOVE    1                   TO  SPA-NAI-IDOKAN
                   IF      (TNS-YUKOEDYMD      >=  SPA-NAI-YUKOEDYMD )
                       MOVE    1                   TO  FLG-TENSU
                       MOVE    2                   TO  SPA-NAI-IDOKAN
                   END-IF
               ELSE
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
               IF     (SPA-ERRCD               =   SPACE )  AND
                      (FLG-TENSU               =   ZERO  )
      **           MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
               END-IF
           END-PERFORM
     ***** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
           .
      *
       300111-IDOKANREN-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *   金額・点数識別・単位編集処理
      *****************************************************************
       30011-MSGLABELHEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-MSGLABEL
           MOVE    WRK-TNS-TEN         TO  WRK-H-TEN
      *    単位コード
           MOVE    SPACE               TO  WRK-H-TANINAME
           IF      WRK-TNS-TANICD      =   SPACE
                                       OR  ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       MTANI-MAX
                   IF      MTANI-TANICD  (IDX) =   WRK-TNS-TANICD
                       MOVE    MTANI-TANINAME(IDX)
                                           TO  WRK-H-TANINAME
                       MOVE    MTANI-MAX   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *    金額種類
           EVALUATE    WRK-TNS-TENSIKIBETU
               WHEN    "1"
                   MOVE    "金額"              TO  WRK-H-TENSIKIMEI
               WHEN    "2"
                   MOVE    "都道府県購入価格"  TO  WRK-H-TENSIKIMEI
               WHEN    OTHER
                   MOVE    SPACE               TO  WRK-H-TENSIKIMEI
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-GMN-MSGLABEL
           STRING  "金額："
                   WRK-H-TEN           DELIMITED  BY  SIZE
                   "　金額種類："
                   WRK-TNS-TENSIKIBETU
                   "　"                DELIMITED  BY  SIZE
                   WRK-H-TENSIKIMEI    DELIMITED  BY  SPACE
                   "　単位："
                   WRK-TNS-TANICD      DELIMITED  BY  SIZE
                   "　"
                   WRK-H-TANINAME      DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-MSGLABEL
           END-STRING
           .
      *
       30011-MSGLABELHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   各リストボックス編集処理
      *****************************************************************
       30012-COB-HEN-SEC            SECTION.
      *
      *    単位コード
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
               IF      SPA-GMN-TANICD      =   SPA-GMN-TANICDCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                           TO  SPA-GMN-TANICD-G
                   MOVE    SPA-TANICD-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           .
       30012-COB-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
               IF      SPA-NAI-COPY        =   SPACE
      *            関連算定コード
                   MOVE    SPA-Z98-SRYCD       TO  Z08-IDOKANREN
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 501-IDOKANREN-CHK-SEC
               ELSE
      *            複写
                   MOVE    SPA-Z98-SRYCD       TO  SPA-GMN-COPYCD
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF
           ELSE
               IF      SPA-NAI-COPY        =   SPACE
                   MOVE    6                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
               MOVE    SPACE               TO  SPA-GMN-COPYCD
           END-IF 
           MOVE    SPACE               TO  SPA-NAI-COPY
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
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
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
      *        前回複写
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM  406-ZENCOPY-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE       TO  WRK-INPUTCD
                   PERFORM  409-KENSAKU-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *H29.5
      *        前頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM  407-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM  408-ATO-PAGE-SEC
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
      *        関連コード
               WHEN    "ACTIVATE"       ALSO    "IDOKANREN"
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 501-IDOKANREN-CHK-SEC
      *        複写先コード
               WHEN    "ACTIVATE"       ALSO    "COPYCD"
                   PERFORM 502-COPYCD-CHK-SEC
      *H29.3
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM 503-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "SHONAMELIST"
                   PERFORM 504-LIST1SEL-SEC
      *        一括入力分
               WHEN    OTHER
                   PERFORM 5000-ENTRY-SYORI-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       5000-ENTRY-SYORI-SEC         SECTION.
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
       5000-ENTRY-SYORI-EXT.
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
           MOVE    Z08-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z08-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
           MOVE    Z08-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z08-NAME            TO  SPA-GMN-NAME
           MOVE    Z08-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    Z08-TEN             TO  SPA-GMN-TEN
           MOVE    Z08-TANICD          TO  SPA-GMN-TANICD-G
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
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
      *    商品名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 505-FORMALNAME-CHK-SEC
           END-IF
      *    金額チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 505-TENSU-CHK-SEC
           END-IF
      *    単位コードチェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 505-TANICD-CHK-SEC
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
      *  　金額チェック
      *****************************************************************
       505-TENSU-CHK-SEC              SECTION.
      *
           .
       505-TENSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　単位コードチェック
      *****************************************************************
       505-TANICD-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-TANICD      =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TANICD-G
               GO      TO      505-TANICD-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-TANICD      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTEDT(13:3)       TO  SPA-GMN-TANICD
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TANICD-MAX  )   OR
                              (SPA-ERRCD   NOT =   SPACE   )   OR
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
           IF     (FLG-OK              =   ZERO    )  AND
                  (SPA-ERRCD           =   SPACE   )
               MOVE    11                  TO  SPA-GMN-CUR
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-TANICD-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　関連コードチェック
      *****************************************************************
       501-IDOKANREN-CHK-SEC           SECTION.
      *
           MOVE    Z08-IDOKANREN       TO  SPA-GMN-IDOKANREN
           MOVE    SPACE               TO  SPA-NAI-COPY
      *    一括入力分
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      501-IDOKANREN-CHK-EXT
           END-IF
      *
           IF     (SPA-GMN-IDOKANREN   =   SPA-MAE-IDOKANREN )  OR
                  (SPA-GMN-IDOKANREN   =   SPACE             )
               MOVE    SPA-GMN-IDOKANREN   TO  SPA-MAE-IDOKANREN
               IF     (SPA-GMN-IDOKANREN   =   SPACE             )
                   MOVE    SPACE               TO  SPA-GMN-IDOKIKAN
                   MOVE    SPACE               TO  SPA-GMN-IDOKANRENNAME
                   MOVE    SPACE               TO  SPA-GMN-MSGLABEL
      *H29.3       商品名称一覧クリア
                   MOVE    ZERO                TO  SPA-GMN-SHONAME-MAX
                   INITIALIZE                  SPA-GMN-SHONAME-LIST-G
                   MOVE    ZERO                TO  SPA-SHO-NEXT
                   MOVE    ZERO                TO  SPA-SHO-PAGE
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
               GO      TO      501-IDOKANREN-CHK-EXT
           END-IF
      *
           MOVE    6                   TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-NAI-IDOKAN
           MOVE    SPACE               TO  SPA-GMN-IDOKIKAN
           MOVE    SPACE               TO  SPA-GMN-IDOKANRENNAME
      *    全角入力の時、４文字まで
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    Z08-IDOKANREN       TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-IDOKANREN
           MOVE    SPACE               TO  SPA-MAE-IDOKANREN
      *
      *    "//" 入力の時、検索画面へ
           IF      SPA-GMN-IDOKANREN(1:2)  =  "//"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
           ELSE
      *        入力コードチェック
               MOVE    SPA-GMN-IDOKANREN   TO  WRK-INPUTCD
               PERFORM 4010-SRYCD-CHK-SEC
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               IF      SPA-GMN-IDOKANREN(1:1)  =   "7"
                   CONTINUE
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        関連算定コード編集
               PERFORM 30011-IDOKANREN-HEN-SEC
               IF     (SPA-ERRCD           =   SPACE)  AND
                      (SPA-NAI-IDOKAN      =   ZERO )
      *            点数マスタなし
                   MOVE    "0006"          TO  SPA-ERRCD
               END-IF
      *H29.3
               IF     (SPA-ERRCD           =   SPACE)
                   MOVE    1               TO  SPA-SHO-PAGE
                   MOVE    ZERO            TO  SPA-SHO-NEXT
      *            商品名一覧編集
                   PERFORM 5011-MATERIALH-HEN-SEC
               END-IF
      *
               IF     (SPA-ERRCD           =   SPACE)  AND
                      (SPA-NAI-IDOKAN  NOT =   ZERO )
                   IF      SPA-NAI-IDOKAN      =   1
      *                終了日切れ
                       MOVE    "0010"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    SPA-GMN-IDOKANREN   TO  SPA-MAE-IDOKANREN
               MOVE    40                  TO  SPA-GMN-CUR
           END-IF
           .
       501-IDOKANREN-CHK-EXT.
           EXIT.
      *H29.3
      *****************************************************************
      *   商品名一覧編集編集処理
      *****************************************************************
       5011-MATERIALH-HEN-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-SHONAME-MAX
           INITIALIZE                      SPA-GMN-SHONAME-LIST-G
      *
           IF      SPA-SHO-PAGE        =   ZERO
               MOVE    1                   TO  SPA-SHO-PAGE
           END-IF
           COMPUTE IDX-STR             =  (SPA-SHO-PAGE   -  1 )
                                       *   200
                                       +   1
           MOVE    ZERO                TO  SPA-SHO-NEXT
      *
           MOVE    SPACE               TO  MATERIAL-H-REC
           INITIALIZE                      MATERIAL-H-REC
           MOVE    SPA-GMN-IDOKANREN   TO  MATERH-SRYCD
      *
           MOVE    MATERIAL-H-REC      TO  MCPDATA-REC
           MOVE    "tbl_material_h"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_material_h"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 901-MATERIAL-H-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-MATERIAL-H
               INITIALIZE                      MATERIAL-H-REC
           END-IF
           MOVE    ZERO                TO  IDXX
           MOVE    ZERO                TO  IDXY
           MOVE    SPACE               TO  WRK-MATERH-SNAME
           MOVE    SPACE               TO  WRK-MATERH-KIKAKU
           PERFORM UNTIL   (FLG-MATERIAL-H =   1  )
                        OR (SPA-SHO-NEXT   =   1  )
      *        前と同じ内容
               IF     (MATERH-SNAME        =   WRK-MATERH-SNAME)
                  AND (MATERH-KIKAKU       =   WRK-MATERH-KIKAKU)
                   CONTINUE
               ELSE
      *
               ADD     1                   TO  IDXX
               IF      IDXX                <   IDX-STR
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDXY
                   IF      IDXY                >   200
                       MOVE    1               TO  SPA-SHO-NEXT
                   ELSE
                       MOVE    IDXY                TO  SPA-GMN-RENNUM
                                                               (IDXY)
                       MOVE    MATERH-SNAME        TO  SPA-GMN-SHONAME
                                                               (IDXY)
                       MOVE    MATERH-KIKAKU       TO  SPA-GMN-KIKAKU
                                                                (IDXY)
                       MOVE    MATERH-KANRINO      TO  SPA-NAI-KANRINO
                                                                (IDXY)
      *
                       MOVE    IDXY            TO  SPA-GMN-SHONAME-MAX
                   END-IF
               END-IF
      *
               END-IF
               MOVE    MATERH-SNAME        TO  WRK-MATERH-SNAME
               MOVE    MATERH-KIKAKU       TO  WRK-MATERH-KIKAKU
      *
               MOVE    "tbl_material_h"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 901-MATERIAL-H-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_material_h"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
           .
       5011-MATERIALH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *  　選択番号チェック
      *****************************************************************
       503-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-SELNUM
      *
           MOVE    Z08-SELNUM          TO  SPA-GMN-SELNUM
      *
           IF      SPA-GMN-SELNUM      =   SPACE
               MOVE    ZERO                TO  SPA-NAI-SELNUM
               GO      TO   503-SELNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-SELNUM      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT           =  ZERO  )   OR
                  (SNUM-MINKBN     NOT           =  SPACE )   OR
                  (SNUM-SYOKBN     NOT           =  SPACE )
               MOVE    "0025"                    TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT         TO  SPA-NAI-SELNUM
               MOVE    SPA-NAI-SELNUM      TO  WRK-ZZZ
               MOVE    WRK-ZZZ             TO  SPA-GMN-SELNUM
           END-IF
      *
           MOVE    ZERO                TO  IDXX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-SHONAME-MAX )
                           OR (SPA-ERRCD   NOT =   SPACE )
                           OR (IDXX        >   ZERO   )
               IF      SPA-GMN-RENNUM (IDX)    =   SPA-NAI-SELNUM
                   MOVE    IDX             TO  IDXX
               END-IF
           END-PERFORM
           IF      (IDXX               =   ZERO )
               IF      SPA-ERRCD           =   SPACE
  	           MOVE    "0025"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO   503-SELNUM-CHK-EXT
           END-IF
           MOVE    SPACE           TO  SPA-GMN-FORMALNAME
           IF      SPA-GMN-KIKAKU  (IDXX)  =   "−"
               MOVE    SPA-GMN-SHONAME (IDXX)
                                   TO  SPA-GMN-FORMALNAME
           ELSE
               STRING  SPA-GMN-SHONAME (IDXX)
                       "［"
                       SPA-GMN-KIKAKU  (IDXX)
                       "］"
                                   DELIMITED  BY  SPACE
                                   INTO  SPA-GMN-FORMALNAME
               END-STRING
           END-IF
           MOVE    SPACE           TO  SPA-GMN-SELNUM
           MOVE    ZERO            TO  SPA-NAI-SELNUM
           MOVE    5                   TO  SPA-GMN-CUR
           .
       503-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       504-LIST1SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SHONAME-MAX
               IF      Z08-SHONAMELIST-SELECT (IDX) =   "T"
                   MOVE    SPA-GMN-RENNUM (IDX)
                                               TO  WRK-SELNUM
                   MOVE    SPA-GMN-SHONAME-MAX TO  IDX
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
           END-IF
      *
           IF      SPA-NAI-SELNUM      >   ZERO
               MOVE    SPA-NAI-SELNUM      TO  Z08-SELNUM
               PERFORM 503-SELNUM-CHK-SEC
           END-IF
           .
       504-LIST1SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       407-MAE-PAGE-SEC       SECTION.
      *
           IF      SPA-SHO-PAGE        >=  2
               COMPUTE SPA-SHO-PAGE    =   SPA-SHO-PAGE   -   1
      *        商品名一覧編集
               PERFORM 5011-MATERIALH-HEN-SEC
           END-IF
           .
       407-MAE-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       408-ATO-PAGE-SEC       SECTION.
      *
      *
           IF      SPA-SHO-NEXT        =   1
               COMPUTE SPA-SHO-PAGE    =   SPA-SHO-PAGE   +   1
      *        商品名一覧編集
               PERFORM 5011-MATERIALH-HEN-SEC
           END-IF
           .
       408-ATO-PAGE-EXT.
           EXIT.
      *
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
      ****************************************************************
      *    複写先コード処理
      ****************************************************************
       502-COPYCD-CHK-SEC      SECTION.
      *
           MOVE    Z08-COPYCD          TO  SPA-GMN-COPYCD
      *
           MOVE    7                   TO  SPA-GMN-CUR
           MOVE    "1"                 TO  SPA-NAI-COPY
      *    全角入力の時、４文字まで
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    Z08-COPYCD          TO  KANACHK-MAE-INPUT
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
               IF      SPA-GMN-COPYCD(1:3)     =   "058"
                   CONTINUE
               ELSE
                   MOVE    "0011"              TO  SPA-ERRCD
               END-IF
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
      *    複写処理処理
      ****************************************************************
       5021-COPYCD-SYORI-SEC      SECTION.
      *
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
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 3001-TENSU-SET-SEC
               MOVE    SPACE               TO  SPA-NAI-COPY
               MOVE    SPACE               TO  SPA-GMN-COPYCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0008"              TO  SPA-ERRCD
           END-IF
           .
       5021-COPYCD-SYORI-EXT.
           EXIT.
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
           END-IF
      *
           IF     (SPA-GMN-NAME        =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "1014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           IF     (SPA-GMN-FORMALNAME  =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *    算定器材コード必須
           IF     (SPA-GMN-IDOKANREN   =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *    金額チェック必須
           IF     (SPA-GMN-TEN         >   ZERO  )
              AND (SPA-ERRCD           =   SPACE )
               IF      SPA-NAI-TENSIKIBETU NOT =   "2"
                   MOVE    "0022"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    単位コードチェック必須
           IF     (SPA-GMN-TANICD      =   ZERO
                                       OR  SPACE  )
               OR (SPA-ERRCD       NOT =   SPACE  )
               CONTINUE
           ELSE
               IF     (SPA-NAI-TANICD      =   ZERO
                                           OR  SPACE  )
                   CONTINUE
               ELSE
                   MOVE    "0023"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
           MOVE  "Z01"                           TO  SPA-SAKIPG.
      *
           MOVE  "Z08"                           TO  SPA-MOTOPG.
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
       403-DEL-SEC                               SECTION.
      *
      *    新規・改正は削除処理なし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               GO      TO   403-DEL-EXT
           END-IF
      *
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
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF     MCP-RC           NOT =   ZERO
               DISPLAY "Z08 TENSU DEL ERR!!"     MCP-RC
                       ",KEY:" TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      **** 点数付加情報削除
      *    MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
      *    MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
      *                                TO  TNSP-YUKOSTYMD
      *    MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
      *                                TO  TNSP-YUKOEDYMD
      *    MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
      *    MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
      *    MOVE    "DBDELETE"          TO  MCP-FUNC
      *    MOVE    "tbl_tensuplus"     TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                        MCPDATA-REC
      *                                        SPA-AREA
      *    IF      MCP-RC          NOT =   ZERO
      *        DISPLAY "Z03 TENSUPLUS DEL ERR!!"   MCP-RC
      *                ",KEY:"   TNS-KEY
      *        MOVE    "9001"                TO  SPA-ERRCD
      **** END-IF
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
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               MOVE    SPACE               TO  SPA-Z08MAE-SRYCD
               MOVE    "JOIN"              TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                 SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z08             TO  SPA-Z01SONOTA
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
           MOVE    SPA-Z01SONOTA       TO  SPA-Z08
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z08"               TO  SPA-MOTOPG.
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
           MOVE    "Z08"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z08"               TO  SPA-Z98-MOTOPG
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
           MOVE    SPA-Z08             TO  SPA-Z01SONOTA
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
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z08
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
               MOVE    7                   TO  SPA-GMN-CUR
           ELSE
      *        点数マスタ読込み
               MOVE    SPACE               TO  TNS-TENSU-REC
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SPA-Z08MAE-SRYCD    TO  TNS-SRYCD
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
               PERFORM 990-DBLOSEC-SEC
      *
               IF      FLG-TENSU           =   ZERO
      *            点数マスタ編集
                   PERFORM 3001-TENSU-SET-SEC
               END-IF
           END-IF 
           .
       406-ZENCOPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
      **** PERFORM 410-INPUT-CHK-SEC
           PERFORM 501-IDOKANREN-CHK-SEC
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
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
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
      *    点数マスタ付加情報更新
      *現在なし
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
    ****   MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
    ******     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
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
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z08 TENSU UPD ERR:" MCP-RC
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
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ***      MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      ***  MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
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
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z08 TENSU UPD ERR:" MCP-RC
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
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z08 TENSU INS ERR:" MCP-RC
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
           MOVE    SPA-GMN-IDOKANREN       TO  TNS-IDOKANREN
      *
           MOVE    SPA-GMN-TEN             TO  TNS-TEN
      *    金額種別（金額）
           IF      SPA-GMN-TEN         NOT =   ZERO
               MOVE    1                   TO  TNS-TENSIKIBETU
           END-IF
      *    単位コード
           MOVE  SPA-GMN-TANICD            TO  TNS-TANICD
           MOVE  SPA-GMN-TANINAME          TO  TNS-TANINAME
           MOVE  SPA-GMN-TANINAME          TO  WRK-STRINGTBL
           MOVE  12                        TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                TO  TNS-TANIMOJISU
           .
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタ付加情報更新処理
      *****************************************************************
      *4129-TENSUPLUS-UPD-SEC          SECTION.
      *
      *
      *    INITIALIZE                  ORCGZS02AREA
      *    MOVE    SPA-NAI-YUKOSTYMD   TO  ZS02-NAI-YUKOSTYMD
      *    MOVE    SPA-NAI-YUKOEDYMD   TO  ZS02-NAI-YUKOEDYMD
      *    MOVE    WRK-YUKOEDYMD       TO  ZS02-YUKOEDYMD
      *
      *    INITIALIZE                  TENSUPLUS-REC
      *    入力コード更新
      *    CALL    "ORCGZS02"          USING
      *                                MCPAREA
      *                                SPAAREA
      *                                ORCGZS02AREA
      *                                TENSUPLUS-REC
      *
      *    .
      *4129-TENSUPLUS-UPD-EXT.
      ***  EXIT.
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       400-ZENCHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
      **** MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    "2"                 TO  KANACHK-SYORI
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
           MOVE    "Z08    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  Z08
           INITIALIZE                  Z08
      *
           MOVE    SPA-GMN-SRYCDH          TO  Z08-SRYCDH
           MOVE    SPA-GMN-NAMEH           TO  Z08-NAMEH
           MOVE    SPA-GMN-YUKOSTYMDH      TO  Z08-YUKOSTYMDH
           MOVE    SPA-GMN-YUKOEDYMDH      TO  Z08-YUKOEDYMDH
           MOVE    SPA-GMN-YUKOSTYMD       TO  Z08-YUKOSTYMD
           MOVE    SPA-GMN-YUKOEDYMD       TO  Z08-YUKOEDYMD
           MOVE    SPA-GMN-KANANAME        TO  Z08-KANANAME
           MOVE    SPA-GMN-NAME            TO  Z08-NAME
           MOVE    SPA-GMN-FORMALNAME      TO  Z08-FORMALNAME
      *
           MOVE    SPA-GMN-TEN             TO  Z08-TEN
           MOVE    SPA-GMN-TANICD-G        TO  Z08-TANICD
      *    データ規格
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               IF      SPA-GMN-TANICDCOB (IDX) NOT =   SPACE
                   ADD     1               TO  IDX-1
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                           TO  Z08-TANICD-ITEM (IDX-1)
               END-IF
           END-PERFORM
           MOVE    SPA-TANICD-MAX          TO  Z08-TANICD-COUNT
      *
           MOVE    SPA-GMN-IDOKANREN       TO  Z08-IDOKANREN
           MOVE    SPA-GMN-IDOKIKAN        TO  Z08-IDOKIKAN-VALUE
           MOVE    "black"                 TO  Z08-IDOKIKAN-STYLE
           IF     (SPA-GMN-IDOKIKAN    NOT =   SPACE )
               EVALUATE    SPA-NAI-IDOKAN
                   WHEN    1
                       MOVE    "red"           TO  Z08-IDOKIKAN-STYLE
                   WHEN    2
                       IF      (SPA-NAI-IDOSTYMD
                                               >   SPA-NAI-YUKOSTYMD)
      *                開始日不一致
                           MOVE    "blue"      TO  Z08-IDOKIKAN-STYLE
                       END-IF
               END-EVALUATE
           END-IF
           MOVE    SPA-GMN-IDOKANRENNAME   TO  Z08-IDOKANRENNAME
           MOVE    SPA-GMN-MSGLABEL        TO  Z08-MSGLBEL
      *
           MOVE    SPA-GMN-COPYCD          TO  Z08-COPYCD
      *H29.3
      *    商品名一覧
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SHONAME-MAX
               MOVE    SPA-GMN-RENNUM (IDX)    TO  WRK-Z9
               MOVE    WRK-Z9                  TO  Z08-RENNUM (IDX)
               MOVE    SPA-GMN-SHONAME(IDX)    TO  Z08-SNAME  (IDX)
               MOVE    SPA-GMN-KIKAKU (IDX)    TO  Z08-KIKAKU (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SHONAME-MAX TO  Z08-SHONAMELIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  Z08-SELNUM
      *
           IF      SPA-SHO-NEXT        =   1
               IF      SPA-SHO-PAGE    =   1
                   MOVE    "次頁有り"      TO  Z08-MNEXT-DATA
                   MOVE    "red"           TO  Z08-MNEXT-STYLE
               ELSE
                   MOVE    "前頁・次頁有り"    TO  Z08-MNEXT-DATA
                   MOVE    "red"           TO  Z08-MNEXT-STYLE
               END-IF
           ELSE
               IF      SPA-SHO-PAGE    >   1
                   MOVE    "前頁有り"      TO  Z08-MNEXT-DATA
                   MOVE    "red"           TO  Z08-MNEXT-STYLE
               END-IF
           END-IF
      *
      *H29.1
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"  TO  Z08-MIDASHI-DATA
               MOVE    "red"               TO  Z08-MIDASHI-STYLE
           END-IF
      *
      *H30.9
      *    新規・改正処理は削除なし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               MOVE    WIDGET-INSENSITIVE  TO  Z08-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  Z08-B03-STATE
           END-IF
      *
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
               WHEN    10
                   MOVE    "TEN"               TO  MCP-WIDGET
               WHEN    11
                   MOVE    "TANICD"            TO  MCP-WIDGET
               WHEN    6
                   MOVE    "IDOKANREN"         TO  MCP-WIDGET
               WHEN    7
                   MOVE    "COPYCD"            TO  MCP-WIDGET
               WHEN    40
                   MOVE    "B12"               TO  MCP-WIDGET
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
      ****         MOVE    6                   TO  SPA-GMN-CUR
                   MOVE    10                  TO  SPA-GMN-CUR
               WHEN    "TEN"
                   MOVE    11                  TO  SPA-GMN-CUR
               WHEN    "TANICD"
                   MOVE    6                   TO  SPA-GMN-CUR
               WHEN    "IDOKANREN"
                   MOVE    40                  TO  SPA-GMN-CUR
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
                   MOVE    "商品名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "算定器材コードがありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "算定器材コードが器材ではありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "複写元コードはありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "複写できるコードではありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "算定できる器材コードではありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   STRING  "算定器材コードの終了日以降です。"
                           "期間を変更して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0015"
                   MOVE    "商品名称を入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "算定器材コードを入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "単位コードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0022"
                   STRING  "金額は算定器材コードが都道府県購入価格の"
                           "時のみ設定できます。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0023"
                   STRING  "単位コードは算定器材コードの単位コードが"
                           "未設定の時のみ設定できます。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0025"
                   STRING  "選択番号がありません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
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
          END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                TO  ZERR-ERRMSG
      *
           MOVE    "Z08"                         TO  SPA-MOTOPG
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
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z08"                TO  SPA-MOTOPG
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
      *    特定器材品目ビュー読込処理
      *****************************************************************
       901-MATERIAL-H-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  MATERIAL-H-REC
               MOVE    ZERO            TO  FLG-MATERIAL-H
           ELSE
               INITIALIZE                  MATERIAL-H-REC
               MOVE    1               TO  FLG-MATERIAL-H
           END-IF
           .
      *
       901-MATERIAL-H-READ-EXT.
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
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
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
                  MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
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
                  MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
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
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
