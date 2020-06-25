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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGG03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次統計
      *  コンポーネント名  : 月次統計パラメータ説明画面
      *  管理者            :
      *  04/10/05    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07.05.28  グループ診療対応
      *  05.00.01    NACL-太田    18/07/11  クラウド対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    日次統計共通パラメタ
           COPY    "G01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "G01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-TOUKEIMEMO                  PIC 9(01).
           03  FLG-ERR                         PIC 9(01).
           03  FLG-HIT                         PIC 9(01).
           03  FLG-UPDATE-SYSKANRI             PIC 9(01).
           03  FLG-FIXED                       PIC 9(01).
           03  FLG-ENABLED                     PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX1                            PIC 9(05).
           03  IDX2                            PIC 9(05).
           03  IDXA                            PIC 9(05).
           03  IDXI                            PIC 9(04).
           03  IDXK                            PIC 9(04).
           03  IDXT                            PIC 9(04).
           03  IDXU                            PIC 9(04).
           03  IDX-STR                         PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-ERR-CUR                     PIC 9(03).
           03  WRK-GIDMSG                      PIC X(70).
           03  WRK-YMD                         PIC X(10).
           03  WRK-SYMD.
               05  WRK-SYY                     PIC 9(04).
               05  WRK-SMM                     PIC 9(02).
               05  WRK-SDD                     PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO                     PIC X(01).
               05  WRK-WYY                     PIC 9(02).
               05  WRK-WMM                     PIC 9(02).
               05  WRK-WDD                     PIC 9(02).
      *
           03  WRK-KBNCD-G.
               05  WRK-KBNCD                   PIC 9(03).
           03  WRK-CHK-CUR                     PIC 9(05).
           03  WRK-MCP-WIDGET                  PIC X(64).
           03  WRK-PARA-NUM                    PIC 9(01).
           03  WRK-MCP-TABLE                   PIC X(64).
           03  WRK-MCP-PATHNAME                PIC X(64).
           03  WRK-MAE-INPUT                   PIC X(100).
           03  WRK-ATO-INPUT                   PIC X(100).
           03  WRK-STR                         PIC X(100).
           03  WRK-ERRMSG                      PIC X(200).
           03  WRK-ZERO                        PIC X(10).
           03  WRK-NUM99-G.
               05  WRK-NUM99                   PIC 99.
           03  WRK-NUMZ9                       PIC Z9.
           03  WRK-NUMMSG                      PIC X(04).
           03  WRK-MAX                         PIC 9(03).
           03  WRK-FMTNUM-G.
               05  WRK-FMTNUM-X10              PIC X(10).
               05  WRK-FMTNUM-R1   REDEFINES   WRK-FMTNUM-X10.
                   07  WRK-FMTNUM-Z10          PIC Z(9)9.
               05  WRK-FMTNUM-R2   REDEFINES   WRK-FMTNUM-X10.
                   07  WRK-FMTNUM-910          PIC 9(10).
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
       01  WRK-CONS-AREA.
      *    パラメータ最大個数
           03  WRK-CONS-PARA-MAX               PIC 9(05) VALUE 10.
           03  WRK-CONS-NUMMSG-G.
               05  WRK-CONS-NUMMSG-VAL.
                   07  WRK-CONS-NUMMSG-01      PIC X(04) VALUE "１".
                   07  WRK-CONS-NUMMSG-02      PIC X(04) VALUE "２".
                   07  WRK-CONS-NUMMSG-03      PIC X(04) VALUE "３".
                   07  WRK-CONS-NUMMSG-04      PIC X(04) VALUE "４".
                   07  WRK-CONS-NUMMSG-05      PIC X(04) VALUE "５".
                   07  WRK-CONS-NUMMSG-06      PIC X(04) VALUE "６".
                   07  WRK-CONS-NUMMSG-07      PIC X(04) VALUE "７".
                   07  WRK-CONS-NUMMSG-08      PIC X(04) VALUE "８".
                   07  WRK-CONS-NUMMSG-09      PIC X(04) VALUE "９".
                   07  WRK-CONS-NUMMSG-10      PIC X(04) VALUE "１０".
               05  WRK-CONS-NUMMSG-R  REDEFINES   WRK-CONS-NUMMSG-VAL.
                   07  WRK-CONS-NUMMSG         PIC X(04) OCCURS 10.
      *
       01  WTMEMO-AREA.
           COPY    "CPTOUKEIMEMO.INC"  REPLACING   //TKIMEMO-//
                                              BY   //WTMEMO-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK0044.INC".
           COPY    "CPSK3002.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *    統計メモ
       01  TOUKEIMEMO-REC.
           COPY  "CPTOUKEIMEMO.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
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
           COPY    "ORCA51SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-G01FREE
           MOVE    SPA-WORK        TO  SPA-G01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-G01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-G01FREE     TO  SPA-FREE
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
      *    エラー画面より
           IF      SPA-MOTOPG          =   "GERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
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
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "G03"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "GID1"
               PERFORM 330-GID1-SET-SEC
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
       310-SPASET-SEC                  SECTION.
      *
           INITIALIZE                      SPA-G03-AREA
      *
           MOVE    SPA-G01-SEL         TO  IDX0
      *
      *    MOVE    SPA-GMN-G01-CHK-VALUE (IDX0)
      *                                TO  SPA-GMN-G03-CHK-VALUE
           MOVE    "T"
                                       TO  SPA-GMN-G03-CHK-VALUE
      *
           MOVE    SPA-GMN-G01-NUM     (IDX0)
                                       TO  SPA-GMN-G03-NUM
           MOVE    SPA-GMN-G01-HCNAME  (IDX0)
                                       TO  SPA-GMN-G03-HCNAME
           MOVE    SPA-GMN-G01-PRGNAME (IDX0)
                                       TO  SPA-GMN-G03-PRGNAME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   WRK-CONS-PARA-MAX )
      *
               MOVE    SPA-GMN-G01-PARAHYOJI (IDX0 IDX1)
                                       TO  SPA-GMN-G03-PARAHYOJI (IDX1)
      *
               MOVE    SPA-GMN-G01-PARA  (IDX0 IDX1)
                                       TO  SPA-GMN-G03-PARA (IDX1)
      *
               MOVE    SPA-NAI-G01-PARA  (IDX0 IDX1)
                                       TO  SPA-NAI-G03-PARA (IDX1)
      *
               MOVE    SPA-NAI-G01-HENPARA (IDX0 IDX1)
                                       TO  SPA-NAI-G03-HENPARA (IDX1)
      *
               MOVE    SPA-NAI-G01-HISSU-KBN  (IDX0 IDX1)
                                       TO  SPA-NAI-G03-HISSU-KBN (IDX1)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-G01-REPOS-NAME (IDX0)
                                       TO  SPA-GMN-G03-REPOS-NAME
      *
           PERFORM 800-IS-ENABLED-SEC
           IF    ( FLG-ENABLED         =   ZERO )
               MOVE    "F"             TO  SPA-GMN-G03-CHK-VALUE
           END-IF
      *
           MOVE    SPA-G01-SEL         TO  WRK-KBNCD
      *
           PERFORM 3102-TOKEIMEMO-SEL-SEC
           MOVE    TOUKEIMEMO-REC  TO  WTMEMO-AREA
           IF    ( FLG-TOUKEIMEMO  =   ZERO )
               MOVE    1               TO  SPA-NAI-G03-PARAARI
           END-IF
      *
           PERFORM 3101-TOKEIMEMO-SEL-SEC
           IF    ( FLG-TOUKEIMEMO  =   ZERO )
               MOVE    TKIMEMO-MEMO    TO  SPA-GMN-G03-MEMO
               MOVE    TKIMEMO-USERMEMO TO SPA-GMN-G03-USERMEMO
           ELSE
               MOVE    WTMEMO-MEMO     TO  SPA-GMN-G03-MEMO
           END-IF
      *
           PERFORM 240-PARAUPDATE-SEC
      *
           MOVE    2                   TO  SPA-GMN-G03-CUR
      *
           PERFORM 4201-CHK-CHK-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計メモテーブル検索処理
      *****************************************************************
       3101-TOKEIMEMO-SEL-SEC          SECTION.
      *
           INITIALIZE                      TOUKEIMEMO-REC
           MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
           MOVE    SPA-GMN-G03-PRGNAME TO  TKIMEMO-PGID
           MOVE    "3002"              TO  TKIMEMO-KANRICD
           MOVE    WRK-KBNCD-G         TO  TKIMEMO-KBNCD
           MOVE    SPA-SYSYMD          TO  TKIMEMO-STYUKYMD
                                           TKIMEMO-EDYUKYMD
           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
           MOVE    "tbl_toukeimemo"    TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-TOUKEIMEMO-SEL-SEC
           .
      *
       3101-TOKEIMEMO-SEL-EXT.
           EXIT.
      *****************************************************************
      *    統計メモテーブル検索処理(標準パラメタ検索）
      *****************************************************************
       3102-TOKEIMEMO-SEL-SEC          SECTION.
      *
           INITIALIZE                  TOUKEIMEMO-REC
           MOVE    SPA-HOSPNUM     TO  TKIMEMO-HOSPNUM
           MOVE    SPA-GMN-G03-PRGNAME
                                   TO  TKIMEMO-PGID
           MOVE    "0000"          TO  TKIMEMO-KANRICD
           MOVE    "000"           TO  TKIMEMO-KBNCD
           MOVE    "00000000"      TO  TKIMEMO-STYUKYMD
           MOVE    "99999999"      TO  TKIMEMO-EDYUKYMD
           MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
           MOVE    "tbl_toukeimemo"    TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-TOUKEIMEMO-SEL-SEC
      *
           .
      *
       3102-TOKEIMEMO-SEL-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＧＩＤ１）ＯＫ処理
      *****************************************************************
       330-GID1-SET-SEC            SECTION.
      *
           IF    ( SPA-GID1-FLG    =   "OK" )
               EVALUATE    SPA-GIDCD
               WHEN    "1001"
                   PERFORM 2301-DEL-SEC
                   IF    ( SPA-ERRCD   =   SPACE )
                       MOVE    "JOIN"      TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   END-IF               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-G99-FLG
           MOVE    SPACE               TO  SPA-GID1-FLG
           MOVE    SPACE               TO  SPA-GIDCD
      *
           .
       330-GID1-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 230-CLEAR-SEC
      *    入力欄
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    2       TO  SPA-GMN-G03-CUR
      *    メモ欄
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    81      TO  SPA-GMN-G03-CUR
      *    OK
           WHEN    "CLICKED"       ALSO    "B12"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    個別出力
           WHEN    "CLICKED"       ALSO    "CHK01"
               PERFORM 420-CHK-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    WHEN    "XXXX"
      *        入力個所のセット
      *        PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
      *        PERFORM 410-INPUT-CHK-SEC
      *    END-EVALUATE
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-G03-CUR
      *
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   WRK-CONS-PARA-MAX )
               MOVE    G03-PARA (IDX0)
                                   TO  SPA-GMN-G03-PARA(IDX0)
           END-PERFORM
      *
           MOVE    G03-USERMEMO    TO  SPA-GMN-G03-USERMEMO
      *
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           MOVE    ZERO                        TO  WRK-PARA-NUM
           IF    ( WRK-MCP-WIDGET (1:4)    =   "PARA" )
            AND  ( WRK-MCP-WIDGET (5:4)    IS  NUMERIC )
               MOVE    WRK-MCP-WIDGET (7:2)    TO  WRK-PARA-NUM
           END-IF
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   WRK-CONS-PARA-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-GMN-G03-PARAHYOJI (IDXA)
                               NOT =   SPACE )
      *            入力パラメータ
                   PERFORM 4101-PARA-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-USERMEMO-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パラメタチェック
      *****************************************************************
       4101-PARA-CHK-SEC                         SECTION.
      *
           IF    ( SPA-GMN-G03-PARA (IDXA)    =   SPACE )
               IF    ( SPA-NAI-G03-HISSU-KBN (IDXA)
                                               =   "1" )
                   IF    ( WRK-MCP-WIDGET (1:4)    =   "PARA" )
                    AND  ( IDXA            =   WRK-PARA-NUM )
                       PERFORM 41011-PARA-KIHON-SET-SEC
                       IF    ( SPA-ERRCD       NOT =   SPACE )
                           COMPUTE SPA-GMN-G03-CUR
                               =   IDXA    +   1
                           GO  TO  4101-PARA-CHK-EXT
                       END-IF
                   END-IF
               ELSE
                   MOVE    SPACE       TO  SPA-NAI-G03-HENPARA (IDXA)
               END-IF
           ELSE
               PERFORM 41012-PARA-NAIYO-CHK-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   COMPUTE SPA-GMN-G03-CUR
                       =   IDXA    +   1
                   GO  TO  4101-PARA-CHK-EXT
               END-IF
           END-IF
      *
           .
       4101-PARA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ基本値設定処理
      *****************************************************************
       41011-PARA-KIHON-SET-SEC        SECTION.
      *
           IF    ( SPA-GMN-G03-CHK-VALUE       =   "F" )
               GO  TO 41011-PARA-KIHON-SET-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-G03-PARA (IDXA)
           WHEN    "YM"
               MOVE    SPA-SYSYMD  (1:6)
                                       TO  SPA-NAI-G03-HENPARA(IDXA)
               MOVE    SPA-SYSYMDWH(1:6)
                                       TO  SPA-GMN-G03-PARA   (IDXA)
           WHEN    "YMD"
               MOVE    SPA-SYSYMD      TO  SPA-NAI-G03-HENPARA(IDXA)
               MOVE    SPA-SYSYMDWH    TO  SPA-GMN-G03-PARA   (IDXA)
           WHEN    "N1"
           WHEN    "N2"
           WHEN    "N3"
           WHEN    "N4"
           WHEN    "N5"
           WHEN    "N6"
           WHEN    "N7"
           WHEN    "N8"
           WHEN    "N9"
           WHEN    "N10"
           WHEN    "PSN1"
               MOVE    "0"             TO  SPA-NAI-G03-HENPARA(IDXA)
               MOVE    "0"
                                       TO  SPA-GMN-G03-PARA   (IDXA)
           WHEN    OTHER
               MOVE    SPACE           TO  SPA-NAI-G03-HENPARA(IDXA)
               MOVE    SPACE           TO  SPA-GMN-G03-PARA   (IDXA)
           END-EVALUATE
      *
           .
       41011-PARA-KIHON-SET-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ入力内容チェック処理
      *****************************************************************
       41012-PARA-NAIYO-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-G03-PARA (IDXA)
                                   TO  WRK-MAE-INPUT
           PERFORM 800-KANA-CHK-SEC
           MOVE    WRK-ATO-INPUT   TO  SPA-GMN-G03-PARA (IDXA)
      *
           EVALUATE    SPA-NAI-G03-PARA (IDXA)
           WHEN    "YM"
               MOVE    SPA-GMN-G03-PARA (IDXA) TO  WRK-YMD
               PERFORM 5003-NENGETSU-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    WRK-SYMD(1:6)
                                       TO  SPA-NAI-G03-HENPARA (IDXA)
                   MOVE    WRK-HENYMDG(1:6)
                                       TO  SPA-GMN-G03-PARA    (IDXA)
               END-IF
           WHEN    "KKNYM"
               MOVE    SPA-GMN-G03-PARA (IDXA) TO  WRK-YMD
               PERFORM 5003-NENGETSU-KKN-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    WRK-SYMD(1:6)
                                       TO  SPA-NAI-G03-HENPARA (IDXA)
                   MOVE    WRK-HENYMDG(1:6)
                                       TO  SPA-GMN-G03-PARA    (IDXA)
               END-IF
           WHEN    "YMD"
               MOVE    SPA-GMN-G03-PARA (IDXA)    TO  WRK-YMD
               PERFORM 5002-HIZUKE-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   MOVE    WRK-SYMD    TO  SPA-NAI-G03-HENPARA (IDXA)
                   MOVE    WRK-HENYMDG TO  SPA-GMN-G03-PARA    (IDXA)
               END-IF
           WHEN    "PSN1"
                   IF    ( SPA-GMN-G03-PARA (IDXA)(2:)    =   SPACE )
                       MOVE    SPA-GMN-G03-PARA(IDXA)(1:1)
                           TO  SPA-NAI-G03-HENPARA (IDXA)
                   ELSE
                       MOVE    "0002"  TO  SPA-ERRCD
                   END-IF
           WHEN    "N1"
           WHEN    "N2"
           WHEN    "N3"
           WHEN    "N4"
           WHEN    "N5"
           WHEN    "N6"
           WHEN    "N7"
           WHEN    "N8"
           WHEN    "N9"
           WHEN    "N10"
               MOVE    ZERO            TO  FLG-FIXED
               PERFORM 410121-PARA-NUM-CHK-SEC
           WHEN    "N01"
           WHEN    "N02"
           WHEN    "N03"
           WHEN    "N04"
           WHEN    "N05"
           WHEN    "N06"
           WHEN    "N07"
           WHEN    "N08"
           WHEN    "N09"
           WHEN    "N010"
               MOVE    1               TO  FLG-FIXED
               PERFORM 410121-PARA-NUM-CHK-SEC
           WHEN    "A"
           WHEN    "A1"
           WHEN    "A2"
           WHEN    "A3"
           WHEN    "A4"
           WHEN    "A5"
           WHEN    "A6"
           WHEN    "A7"
           WHEN    "A8"
           WHEN    "A9"
           WHEN    "A10"
               MOVE    SPA-GMN-G03-PARA (IDXA)
                                       TO  WRK-MAE-INPUT
               PERFORM 800-HANKAKU-CHK-SEC
               MOVE    WRK-ATO-INPUT   TO  SPA-GMN-G03-PARA (IDXA)
               IF    ( FLG-ERR     NOT =   ZERO )
                   MOVE    "0002"  TO  SPA-ERRCD
               END-IF
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( SPA-NAI-G03-PARA (IDXA)
                               NOT =   "A" )
                   INITIALIZE                  ORCSNUMAREA
                   MOVE    SPA-NAI-G03-PARA (IDXA)(2:)
                                           TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   MOVE    SNUM-OUTNUM     TO  WRK-MAX
                   IF    ( SPA-GMN-G03-PARA (IDXA)(WRK-MAX + 1 :)
                                        NOT =   SPACE )
                       MOVE    "0103"      TO  SPA-ERRCD
                       MOVE   WRK-CONS-NUMMSG (WRK-MAX)
                                           TO  WRK-NUMMSG
                   END-IF
               END-IF
               IF    ( SPA-ERRCD   =   SPACE )
                   MOVE    SPA-GMN-G03-PARA (IDXA)
                                       TO  SPA-NAI-G03-HENPARA (IDXA)
               END-IF
           WHEN    "PTNUM"
               IF    ( SPA-GMN-G03-PARA (IDXA)    =   "*" or "＊" )
                   MOVE    "*"         TO  SPA-NAI-G03-HENPARA (IDXA)
                                           SPA-GMN-G03-PARA (IDXA)
               ELSE
                   INITIALIZE                  ORCSPTNUMAREA
                   MOVE    SPA-GMN-G03-PARA (IDXA)
                                       TO  SPTNUM-PTNUM
                   CALL    "ORCSPTNUM" USING   ORCSPTNUMAREA
                                               SPA-AREA
                   IF    ( SPTNUM-RC       NOT =   20 )
                       MOVE    SPTNUM-PTNUM
                                       TO  SPA-NAI-G03-HENPARA (IDXA)
                                           SPA-GMN-G03-PARA    (IDXA)
                   ELSE
                       MOVE    "0002"      TO  SPA-ERRCD
                   END-IF
               END-IF
           WHEN    OTHER
               MOVE    SPA-GMN-G03-PARA (IDXA)
                                       TO  SPA-NAI-G03-HENPARA (IDXA)
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
              MOVE    SPACE                TO  WRK-ERRMSG
              STRING  SPA-GMN-G03-PARAHYOJI (IDXA)
                                                   DELIMITED BY SPACE
              INTO    WRK-ERRMSG
              END-STRING
           END-IF
      *
           .
       41012-PARA-NAIYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ数値項目入力内容チェック処理
      *****************************************************************
       410121-PARA-NUM-CHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE   SPA-NAI-G03-PARA (IDXA)(2:)
                                   TO  SNUM-INX
           CALL   "ORCSNUM"            USING
                                       ORCSNUMAREA
           MOVE   SNUM-OUTNUM      TO  WRK-MAX
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE   SPA-GMN-G03-PARA (IDXA)
                                   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           EVALUATE    TRUE
           WHEN  ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-SYOKBN         =   "1"   )
            OR   ( SNUM-MINKBN         =   "1"   )
               MOVE    "0002"      TO  SPA-ERRCD
               MOVE   WRK-CONS-NUMMSG (WRK-MAX)
                                   TO  WRK-NUMMSG
           WHEN  ( SNUM-SEISUU         >   WRK-MAX )
           WHEN  ( SPA-GMN-G03-PARA (IDXA)(WRK-MAX + 1 :)
                               NOT =   SPACE )
               MOVE    "0103"      TO  SPA-ERRCD
               MOVE   WRK-CONS-NUMMSG (WRK-MAX)
                                   TO  WRK-NUMMSG
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( FLG-FIXED       =   ZERO )
                   MOVE    SPA-GMN-G03-PARA (IDXA)
                                   TO  WRK-FMTNUM-X10
                   PERFORM VARYING IDXK    FROM    1   BY  1
                           UNTIL ( IDXK    >   10 )
                       IF    ( WRK-FMTNUM-X10(IDXK :1)
                                       NOT =   SPACE )
                           MOVE    WRK-FMTNUM-X10 (IDXK :)
                               TO   SPA-NAI-G03-HENPARA (IDXA)
                                    SPA-GMN-G03-PARA    (IDXA)
                           MOVE    10      TO  IDXK
                       END-IF
                   END-PERFORM
               ELSE
                   MOVE    SNUM-OUTNUM TO   WRK-FMTNUM-910
                   MOVE    WRK-FMTNUM-X10(11 - WRK-MAX : WRK-MAX)
                               TO   SPA-NAI-G03-HENPARA (IDXA)
                                     SPA-GMN-G03-PARA    (IDXA)
               END-IF
           END-IF
      *
           .
       410121-PARA-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザーメモ欄チェック処理
      *****************************************************************
       4101-USERMEMO-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    5000                TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    SPA-GMN-G03-USERMEMO
                                       TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  SPA-GMN-G03-USERMEMO
      *
           .
       4101-USERMEMO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-MCP-WIDGET  =   "B12" )
               PERFORM 4101-GMN-SPA-SET-SEC
               PERFORM 2101-TOUKEIMEMO-UPD-SEC
               MOVE    1           TO  SPA-NAI-G03-STATUS
           WHEN  ( SPA-GIDCD       =   "1001" )
               MOVE    3           TO  SPA-NAI-G03-STATUS
           WHEN    OTHER
               MOVE    ZERO        TO  SPA-NAI-G03-STATUS
           END-EVALUATE
       *
           MOVE    "G01"               TO  SPA-SAKIPG
           MOVE    "G03"               TO  SPA-MOTOPG
       *
           MOVE    "G01"               TO  MCP-WINDOW
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
      *    統計メモ更新処理
      *****************************************************************
       2101-TOUKEIMEMO-UPD-SEC         SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           INITIALIZE                SYSKANRI-REC
           MOVE    SPA-HOSPNUM   TO  SYS-HOSPNUM
           MOVE    "3002"        TO  SYS-KANRICD
           MOVE    SPA-G01-SEL   TO  WRK-KBNCD
           MOVE    WRK-KBNCD-G   TO  SYS-KBNCD
           MOVE    SPA-SYSYMD    TO  SYS-STYUKYMD
                                     SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI  =   ZERO )
      *
               PERFORM 3101-TOKEIMEMO-SEL-SEC
               IF    ( FLG-TOUKEIMEMO  =   ZERO )
                   MOVE    SPA-GMN-G03-USERMEMO
                                           TO  TKIMEMO-USERMEMO
                   MOVE    SMCNDATE-YMD    TO  TKIMEMO-UPYMD
                   MOVE    SMCNDATE-HMS    TO  TKIMEMO-UPHMS
                   MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
                   MOVE    "tbl_toukeimemo" TO MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
               ELSE
                   INITIALIZE          TOUKEIMEMO-REC
                   MOVE    SPA-GMN-G03-PRGNAME
                                           TO  TKIMEMO-PGID
                   MOVE    SPA-GMN-G03-MEMO
                                           TO  TKIMEMO-MEMO
                   MOVE    SPA-GMN-G03-USERMEMO
                                           TO  TKIMEMO-USERMEMO
                   MOVE    "1"             TO  TKIMEMO-SAVEPARA
                   MOVE    SPACE           TO  TKIMEMO-PARA-G
                   MOVE    SYS-KANRICD     TO  TKIMEMO-KANRICD
                   MOVE    SYS-KBNCD       TO  TKIMEMO-KBNCD
                   MOVE    SYS-STYUKYMD    TO  TKIMEMO-STYUKYMD
                   MOVE    SYS-EDYUKYMD    TO  TKIMEMO-EDYUKYMD
                   MOVE    SMCNDATE-YMD    TO  TKIMEMO-CREYMD
                                               TKIMEMO-UPYMD
                   MOVE    SMCNDATE-HMS    TO  TKIMEMO-UPHMS
                   MOVE    SPA-HOSPNUM     TO  TKIMEMO-HOSPNUM
                   MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
                   MOVE    "tbl_toukeimemo" TO MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
               END-IF
           END-IF
      *
           .
       2101-TOUKEIMEMO-UPD-EXT.
           EXIT.
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       230-CLEAR-SEC                   SECTION.
      *
           MOVE    "F"             TO  SPA-GMN-G03-CHK-VALUE
      *
           INITIALIZE                  SPA-GMN-G03-PARA-G
                                       SPA-NAI-G03-HENPARA-G
                                       SPA-GMN-G03-USERMEMO
      *
           .
       230-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       230-DEL-SEC                     SECTION.
      *
           MOVE    "1001"          TO  SPA-GIDCD
      *
           .
       230-DEL-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       2301-DEL-SEC                     SECTION.
      *
      *
           .
       2301-DEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ説明更新
      *****************************************************************
       240-PARAUPDATE-SEC              SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SPA-G01-SEL         TO  WRK-KBNCD
      *
           IF    ( SPA-NAI-G03-PARAARI =   1 )
               PERFORM 2401-PARAUPDATE-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( FLG-UPDATE-SYSKANRI =   1 )
                       PERFORM 2402-PARAUPDATE-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "3002"              TO  SYS-KANRICD
               MOVE    WRK-KBNCD-G         TO  SYS-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
      *
                   MOVE    SYSKANRI-REC    TO  SYS-3002-REC
      *
                   MOVE    SPA-G01-SEL     TO  IDX0
                   MOVE    SYS-3002-HCNAME TO  SPA-GMN-G01-HCNAME (IDX0)
		                               SPA-GMN-G03-HCNAME
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   WRK-CONS-PARA-MAX )
                       MOVE    SYS-3002-PARAHYOJI (IDX1)
                                   TO  SPA-GMN-G01-PARAHYOJI (IDX0 IDX1)
                                       SPA-GMN-G03-PARAHYOJI (IDX1)
                       MOVE    SYS-3002-PARA (IDX1)
                                   TO  SPA-NAI-G01-PARA  (IDX0 IDX1)
                                       SPA-NAI-G03-PARA  (IDX1)
                       MOVE    SYS-3002-HISSU-KBN (IDX1)
                                   TO  SPA-NAI-G01-HISSU-KBN (IDX0 IDX1)
                                       SPA-NAI-G03-HISSU-KBN (IDX1)
                   END-PERFORM
               END-IF
      *
      *
               PERFORM 3101-TOKEIMEMO-SEL-SEC
               IF    ( FLG-TOUKEIMEMO      =   ZERO )
                   MOVE    TKIMEMO-MEMO    TO  SPA-GMN-G03-MEMO
               END-IF
      *
               PERFORM 4201-CHK-CHK-SEC
           END-IF
      *
           .
       240-PARAUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ説明更新（統計メモ）
      *****************************************************************
       2401-PARAUPDATE-SEC             SECTION.
      *
           MOVE    1               TO  FLG-UPDATE-SYSKANRI
      *
           PERFORM 3102-TOKEIMEMO-SEL-SEC
           MOVE    TOUKEIMEMO-REC  TO  WTMEMO-AREA
           IF    ( FLG-TOUKEIMEMO  =   ZERO )
      *
               PERFORM 3101-TOKEIMEMO-SEL-SEC
               IF    ( FLG-TOUKEIMEMO  =   ZERO )
                   MOVE    WTMEMO-MEMO     TO  TKIMEMO-MEMO
                   MOVE    SMCNDATE-YMD    TO  TKIMEMO-UPYMD
                   MOVE    SMCNDATE-HMS    TO  TKIMEMO-UPHMS
                   MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
                   MOVE    "tbl_toukeimemo" TO MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBUPDATE-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0006"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2401-PARAUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ説明更新（システム管理）
      *****************************************************************
       2402-PARAUPDATE-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-HIT
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3002"              TO  SYS-KANRICD
           MOVE    WRK-KBNCD-G         TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3002-REC
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    "0044"              TO  SYS-KANRICD
               MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY2-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-0044-REC
      *
               PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                        OR   ( FLG-HIT         NOT =   ZERO )
      *
                   IF    ( SYS-0044-PRGNAME    =   SYS-3002-PRGNAME )
      *                MOVE    SYS-0044-HCNAME TO  SYS-3002-HCNAME
                       PERFORM VARYING IDX2    FROM    1   BY  1
                               UNTIL ( IDX2    >   10 )
                           MOVE    SYS-0044-PARAHYOJI (IDX2)
                                       TO  SYS-3002-PARAHYOJI (IDX2)
                           MOVE    SYS-0044-PARA (IDX2)
                                       TO  SYS-3002-PARA (IDX2)
                           MOVE    SYS-0044-HISSU-KBN (IDX2)
                                       TO  SYS-3002-HISSU-KBN (IDX2)
                       END-PERFORM
      *
                       MOVE    SYS-3002-REC    TO  SYSKANRI-REC
                       MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF    ( MCP-RC          NOT =   ZERO )
                           MOVE    "0007"      TO  SPA-ERRCD
                       END-IF
      *
                       MOVE    1               TO  FLG-HIT
      *
                   END-IF
      *
                   PERFORM 900-SYSKANRI-KEY2-FET-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-0044-REC
               END-PERFORM
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       2402-PARAUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    個別出力処理
      *****************************************************************
       420-CHK-CHK-SEC                 SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    ZERO              TO  SPA-GMN-G03-CUR
      *
           MOVE    G03-CHK-VALUE     TO  SPA-GMN-G03-CHK-VALUE
      *
           PERFORM 4201-CHK-CHK-SEC
      *
           .
       420-CHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    個別出力処理
      *****************************************************************
       4201-CHK-CHK-SEC                SECTION.
      *
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                       UNTIL ( IDXA    >   WRK-CONS-PARA-MAX )
      *
               IF    ( SPA-GMN-G03-PARAHYOJI (IDXA)
                                   NOT =   SPACE )
                AND  ( SPA-GMN-G03-PARA (IDXA)
                                       =   SPACE )
                AND  ( SPA-NAI-G03-HISSU-KBN (IDXA)  =   "1" )
      *
      *            パラメータ基本値設定
                   PERFORM 41011-PARA-KIHON-SET-SEC
      *
               END-IF
           END-PERFORM
      *
           .
       4201-CHK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-GIDCD       NOT =   SPACE )
               PERFORM 520-GIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "G03    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  G03
           INITIALIZE                      G03
      *
           MOVE    "メモ欄"            TO  G03-B04-LABEL
      *
           MOVE    "red"               TO  G03-MARKHISSU-STYLE
           MOVE    "*"                 TO  G03-MARKHISSU
      *
           MOVE    "blue"              TO  G03-MARKNINI-STYLE
           MOVE    "*"                 TO  G03-MARKNINI
      *
           MOVE    SPA-GMN-G03-NUM     TO  G03-NUM
           MOVE    SPA-GMN-G03-PRGNAME TO  G03-PRGNAME
           MOVE    SPA-GMN-G03-REPOS-NAME
                                       TO  G03-REPOS-NAME
           MOVE    SPA-GMN-G03-CHK-VALUE
                                       TO  G03-CHK-VALUE
           MOVE    SPA-GMN-G03-HCNAME  TO  G03-CHK-LABEL
      *
           PERFORM 800-IS-ENABLED-SEC
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >       WRK-CONS-PARA-MAX )
      *
               MOVE    SPA-GMN-G03-PARAHYOJI (IDX0)
                                       TO  G03-PARAHYOJI (IDX0)
               MOVE    SPA-GMN-G03-PARA      (IDX0)
                                       TO  G03-PARA      (IDX0)
      *
               MOVE    WIDGET-INSENSITIVE
                                       TO  G03-PARA-STATE(IDX0)
      *
               MOVE    SPACE           TO  G03-PARAMARK-STYLE(IDX0)
               MOVE    SPACE           TO  G03-PARAMARK      (IDX0)
      *
               IF    ( SPA-GMN-G03-PARAHYOJI (IDX0)
                                   NOT =   SPACE )
                   IF    ( FLG-ENABLED     =   1 )
                       MOVE    WIDGET-NORMAL
                                       TO  G03-PARA-STATE(IDX0)
                   END-IF
      *
                   IF  ( SPA-NAI-G03-HISSU-KBN(IDX0)
                                       =   "1" )
                       MOVE    "red"   TO  G03-PARAMARK-STYLE(IDX0)
                       MOVE    "*"     TO  G03-PARAMARK      (IDX0)
                   ELSE
                       MOVE    "blue"  TO  G03-PARAMARK-STYLE(IDX0)
                       MOVE    "*"     TO  G03-PARAMARK      (IDX0)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL           TO  G03-CHK-STATE
           IF    ( FLG-ENABLED         =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                           TO  G03-CHK-STATE
               MOVE    "F"                 TO  G03-CHK-VALUE
           END-IF
      *
           MOVE    SPA-GMN-G03-MEMO        TO  G03-MEMO
      *
           MOVE    SPA-GMN-G03-USERMEMO    TO  G03-USERMEMO
      *
      *    カーソル位置
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル位置処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-G03-CUR         =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-CUR-SKIP-SEC
      *
           EVALUATE    SPA-GMN-G03-CUR
           WHEN    01
               MOVE    "CHK01"             TO  MCP-WIDGET
           WHEN    02
               MOVE    "PARA0101"          TO  MCP-WIDGET
           WHEN    03
               MOVE    "PARA0102"          TO  MCP-WIDGET
           WHEN    04
               MOVE    "PARA0103"          TO  MCP-WIDGET
           WHEN    05
               MOVE    "PARA0104"          TO  MCP-WIDGET
           WHEN    06
               MOVE    "PARA0105"          TO  MCP-WIDGET
           WHEN    07
               MOVE    "PARA0106"          TO  MCP-WIDGET
           WHEN    08
               MOVE    "PARA0107"          TO  MCP-WIDGET
           WHEN    09
               MOVE    "PARA0108"          TO  MCP-WIDGET
           WHEN    10
               MOVE    "PARA0109"          TO  MCP-WIDGET
           WHEN    11
               MOVE    "PARA0110"          TO  MCP-WIDGET
           WHEN    81
               MOVE    "USERMEMO"          TO  MCP-WIDGET
           WHEN   201
               MOVE    "B01"               TO  MCP-WIDGET
           WHEN   212
               MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "CHK01"
               MOVE    02                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0101"
               MOVE    03                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0102"
               MOVE    04                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0103"
               MOVE    05                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0104"
               MOVE    06                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0105"
               MOVE    07                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0106"
               MOVE    08                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0107"
               MOVE    09                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0108"
               MOVE    10                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0109"
               MOVE    11                  TO  SPA-GMN-G03-CUR
           WHEN    "PARA0110"
               MOVE   212                  TO  SPA-GMN-G03-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-CUR-SKIP-SEC               SECTION.
      *
           IF    ( SPA-GMN-G03-CUR =   1 )
               IF    ( G03-CHK-STATE   =   WIDGET-INSENSITIVE )
                   MOVE   201      TO  SPA-GMN-G03-CUR
               END-IF
           END-IF
      *
           MOVE    1               TO  WRK-CHK-CUR
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   WRK-CONS-PARA-MAX )
      *
               COMPUTE WRK-CHK-CUR = WRK-CHK-CUR   +   1
      *
               IF    ( SPA-GMN-G03-CUR     = WRK-CHK-CUR  )
                AND  ( G03-PARA-STATE(IDX0)
                                           =   WIDGET-INSENSITIVE )
                   IF    ( SPA-GMN-G03-CUR     =   WRK-CONS-PARA-MAX )
                       MOVE   212          TO  SPA-GMN-G03-CUR
                   ELSE
                       COMPUTE SPA-GMN-G03-CUR =   SPA-GMN-G03-CUR   + 1
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-G03-CUR =   212 )
               IF    ( G03-CHK-STATE   =   WIDGET-INSENSITIVE )
                   MOVE   201      TO  SPA-GMN-G03-CUR
               END-IF
           END-IF
      *
           .
      *
       5012-CUR-SKIP-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0002"
               STRING  "入力エラー"            DELIMITED BY  SIZE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0003"
               MOVE    "項目に入力がありません"    TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "必須パラメタで入力されてない項目があります"
                                                   TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "対象の処理が選択されていません"
                                                   TO  SPA-ERRMSG
           WHEN    "0006"
           WHEN    "0007"
               MOVE    "パラメタ説明の更新に失敗しました"
                                                   TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "半角文字を規定文字数以内で入力してください"
                                                   TO  SPA-ERRMSG
           WHEN    "0102"
               STRING  "数字（０以上）を規定桁数以内で入力してください"
                                               DELIMITED BY  SPACE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0103"
               STRING  "項目が規定の長さ（"    DELIMITED BY  SIZE
                       WRK-NUMMSG              DELIMITED BY  SPACE
                       "）を超えて入力されています"
                                               DELIMITED BY  SPACE
                       "【"                    DELIMITED BY  SIZE
                       WRK-ERRMSG              DELIMITED BY  SPACE
                       "】"                    DELIMITED BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "4001" THRU "4003"
               MOVE    "更新処理に失敗しました"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  GERR
           MOVE    SPA-ERRCD           TO  GERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  GERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "G03"               TO  SPA-MOTOPG
           MOVE    "GERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "GERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-GIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-GIDMSG
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           EVALUATE    SPA-GIDCD
           WHEN    "1001"
               MOVE    "帳票の登録を削除します。"
                                        TO  WRK-GIDMSG
               MOVE    "B01"            TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    SPACE                TO  GID1
           INITIALIZE                       GID1
           MOVE    SPA-GIDCD            TO  GID1-ID1CODE
           MOVE    WRK-GIDMSG           TO  GID1-ID1MSG
      *
           MOVE    "G03"                TO  SPA-MOTOPG
           MOVE    "GID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "GID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-GIDSET-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    年月チェック・編集処理
      *****************************************************************
       5003-NENGETSU-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    WRK-YMD             TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
           .
       5003-NENGETSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年月（期間）チェック・編集処理
      *****************************************************************
       5003-NENGETSU-KKN-CHK-SEC       SECTION.
      *
           IF    ( WRK-YMD (1:6)     =     "000000" )
               MOVE    "000000"       TO   WRK-HENYMDG
                                           WRK-SYMD
               GO  TO  5003-NENGETSU-KKN-CHK-EXT
           END-IF
      *
           IF    ( WRK-YMD (1:6)     =     "999999" )
               MOVE    "999999"       TO   WRK-HENYMDG
                                           WRK-SYMD
               GO  TO  5003-NENGETSU-KKN-CHK-EXT
           END-IF
      *
           PERFORM 5003-NENGETSU-CHK-SEC
      *
           .
       5003-NENGETSU-KKN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カナチェック処理
      *****************************************************************
       800-KANA-CHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    WRK-MAE-INPUT       TO  WRK-ATO-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                           TO  WRK-ATO-INPUT
               END-IF
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       800-KANA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       800-HANKAKU-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-ERR
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU NOT =   1     )
               MOVE    1                   TO  FLG-ERR
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    使用可否チェック処理
      *****************************************************************
       800-IS-ENABLED-SEC        SECTION.
      *
           MOVE    1               TO  FLG-ENABLED
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
            AND  ( SPA-GMN-G03-REPOS-NAME
                                   NOT =   SPACE )
            AND  ( SPA-G01-CUSTOM-ENABLED
                                   NOT =   "1" )
               MOVE    ZERO        TO  FLG-ENABLED
           END-IF
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
             AND ( SPA-GMN-G03-REPOS-NAME
                                       =   SPACE )
               MOVE    ZERO        TO  IDXU
               PERFORM VARYING IDXT    FROM    1   BY  1
                       UNTIL ( IDXT    >   SPA-G01-PGLST-MAX )
                        OR   ( IDXU    >   ZERO )
      *
                   IF    ( SPA-GMN-G03-PRGNAME
                                       =   SPA-G01-TPGID (IDXT))
                       MOVE    IDXT    TO  IDXU
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDXU            =   ZERO )
                   MOVE    ZERO        TO  FLG-ENABLED
               END-IF
      *
           END-IF
      *
           .
       800-IS-ENABLED-EXT.
           EXIT.
      *****************************************************************
      *    統計メモテーブル検索処理
      *****************************************************************
       900-TOUKEIMEMO-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUKEIMEMO
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                       SPA-AREA
               IF    ( MCP-RC          =   ZERO )
                   MOVE    ZERO        TO  FLG-TOUKEIMEMO
                   MOVE    MCPDATA-REC TO  TOUKEIMEMO-REC
               ELSE
                   INITIALIZE              TOUKEIMEMO-REC
                   MOVE    1           TO  FLG-TOUKEIMEMO
               END-IF
           ELSE
               INITIALIZE                  TOUKEIMEMO-REC
               MOVE    1               TO  FLG-TOUKEIMEMO
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-TOUKEIMEMO-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
