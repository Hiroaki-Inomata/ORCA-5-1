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
       PROGRAM-ID.             ORCGW10.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 統計帳票出力情報（Ｗ１０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/07/12    MCC−森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/09/06  パラメタ項目追加
      *  01.00.02    MCC-藤原     01/09/11  削除処理の修正 
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     02/03/11  月次統計パラメタの追加     
      *  01.01.03    NACL-多々納  02/11/13  一括入力 
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計メモ更新データ
           SELECT  MST-TMEMO   ASSIGN              ASS-TMEMO
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-TMEMO.
      *
      *    システム管理更新データ
           SELECT  MST-SYSKANRI
                               ASSIGN              ASS-SYSKANRI
                               ORGANIZATION    IS  LINE SEQUENTIAL
                               FILE    STATUS  IS  STS-SYSKANRI.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  MST-TMEMO.
       01  TOUKEIMEMOWK-REC.
           03  TKIMEMOWK-SYORIKBN             PIC  X(01).
           COPY    "CPTOUKEIMEMO.INC"  REPLACING   //TKIMEMO-//
                                              BY   //TKIMEMOWK-//.
       FD  MST-SYSKANRI.
       01  SYSWK-REC.
           COPY    "CPSYSKANRIWK.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "W10SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-TOUKEIMEMO      PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-ZENKAKU         PIC 9(01).
           03  FLG-HANKAKU         PIC 9(01).
           03  FLG-CHK-END         PIC 9(01).
           03  FLG-PARA-SET        PIC 9(01).
      *
       01  STS-AREA.
           03  STS-TMEMO           PIC X(02).
           03  STS-SYSKANRI        PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX0                PIC 9(06).
           03  IDX1                PIC 9(06).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    アサイン名領域
       01  ASS-AREA.
           03  ASS-TMEMO           PIC X(100).
           03  ASS-SYSKANRI        PIC X(100).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
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
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-MAE-INPUT       PIC X(200).
           03  WRK-OUT-INPUT       PIC X(200).
           03  WRK-PGID            PIC X(20).
           03  WRK-MEMO-LEN        PIC 9(05).
           03  WRK-SENI-FLG        PIC 9(01).
           03  WRK-KANRICD         PIC X(04).
           03  WRK-KBNCD           PIC X(08).
           03  WRK-PRGNAME         PIC X(100).
           03  WRK-LEN             PIC 9(08).
           03  WRK-MNTCD-G.
               05  WRK-MNTCD-GRP   PIC X(01).
               05  WRK-MNTCD-NUM   PIC 9(03).
           03  WRK-PARA-VISIBLE    PIC X(01).
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
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET                          PIC X(20).
      *
           03  WRK-WIDGET-R7   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R7-DAMMY1            PIC X(07).
               05  WRK-WIDGET7                     PIC 9(02).
               05  WRK-WIDGET-R7-DAMMY2            PIC X(11).
      *
           03  WRK-WIDGET-R8   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R8-DAMMY1            PIC X(08).
               05  WRK-WIDGET8                     PIC 9(02).
               05  WRK-WIDGET-R8-DAMMY2            PIC X(10).
      *
       01  CONST-AREA.
           03  CONST-3001-PARA-MAX PIC 9(05)   VALUE 8.
           03  CONST-3002-PARA-MAX PIC 9(05)   VALUE 10.
           03  CONST-PARA-MAX      PIC 9(05)   VALUE 10.
           03  CONST-MEMO-MAX    PIC 9(05)     VALUE 6000.
      *
           COPY    "CPSK0043.INC"              REPLACING    //0043//
                                               BY           //TOUKEI//.
      *
       01  WTMEMO-REC.
           COPY    "CPTOUKEIMEMO.INC"      REPLACING    //TKIMEMO-//
                                           BY           //WTMEMO-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0043.INC".
           COPY    "CPSK0044.INC".
           COPY    "CPSK3001.INC".
           COPY    "CPSK3002.INC".
           COPY    "CPSK3101.INC".
           COPY    "CPSK3102.INC".
      *
      *    統計メモ
       01  TOUKEIMEMO-REC.
           COPY  "CPTOUKEIMEMO.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日レセ選択式プログラム一覧
           COPY    "CPORCSORCAPG.INC".
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W10FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           END-IF
      *
           MOVE    SPA-W10FREE     TO  SPA-FREE
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
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      * 
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "W10"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
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
           WHEN    "W19"
               IF    ( SPA-NAI-W19-RET =   1 )
                   PERFORM 330-W19-RETURN-SEC
               END-IF
               GO  TO  300-SCREEN-EXT
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票一覧画面より戻り時の処理
      *****************************************************************
       330-W19-RETURN-SEC              SECTION.
      *
           IF    ( SPA-NAI-W10-W19-SENI-FLG    =   1 )
               MOVE    SPA-NAI-W19-KBNCD
                                       TO  SPA-W10-KBNCD
           END-IF
      *
           MOVE    SPA-NAI-W19-KANRICD TO  WRK-KANRICD
           MOVE    SPA-NAI-W19-KBNCD   TO  WRK-KBNCD
      *
           PERFORM 3301-PARA-SET-SEC
      *
           .
       330-W19-RETURN-EXT.
           EXIT.
      *****************************************************************
      *    パラメータ設定処理
      *****************************************************************
       3301-PARA-SET-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA-SET
      *
           PERFORM 3101-GMN-CLEAR-SEC
      *
           INITIALIZE              SYS-TOUKEI-REC
      *
           MOVE    WRK-KANRICD         TO  SYS-TOUKEI-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-TOUKEI-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD
                                       TO  SYS-TOUKEI-STYUKYMD
                                           SYS-TOUKEI-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-TOUKEI-HOSPNUM
           MOVE    SYS-TOUKEI-REC      TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           IF    ( FLG-SYSKANRI        =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  SYS-TOUKEI-REC
      *
               MOVE    SYS-TOUKEI-HCNAME
                                       TO  SPA-GMN-W10-HCNAME
               MOVE    SYS-TOUKEI-PRGNAME
                                       TO  SPA-GMN-W10-PRGNAME
                                           SPA-W10-PRGNAME
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-W10-PARA-MAX
                   MOVE    SYS-TOUKEI-PARAHYOJI(IDX)
                                       TO  SPA-GMN-W10-PARAHYOJI(IDX)
                   MOVE    SYS-TOUKEI-PARA     (IDX)
                                       TO  SPA-GMN-W10-PARA     (IDX)
                   IF    ( SYS-TOUKEI-HISSU-KBN(IDX)
                                           =   SPACE OR "1" )
                       MOVE    "T" TO  SPA-GMN-W10-OPTHISSU (IDX)
                       MOVE    "F" TO  SPA-GMN-W10-OPTNINI  (IDX)
                   ELSE
                       MOVE    "F" TO  SPA-GMN-W10-OPTHISSU (IDX)
                       MOVE    "T" TO  SPA-GMN-W10-OPTNINI  (IDX)
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-KANRICD         =   "3001" OR "3002" )
                   MOVE    SPACE           TO  SYSKANRI-REC
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
                   MOVE    SPA-W10-KANRICD2
                                           TO  SYS-KANRICD
                   MOVE    WRK-KBNCD       TO  SYS-KBNCD
                   MOVE    SPA-W10-SEL-STYUKYMD
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   MOVE    SYSKANRI-REC    TO  SYS-3101-REC
                   IF    ( SPA-MW          =   SPA-GINBEE )
                       MOVE    SYS-3101-REPOS-NAME
                                           TO  SPA-GMN-W10-REPOS-NAME
                   END-IF
               END-IF
      *
               MOVE    SYS-TOUKEI-REC      TO  SYSKANRI-REC
               MOVE    SYS-TOUKEI-PRGNAME
                                           TO  WRK-PGID
      *
               PERFORM 3101-TOKEIMEMO-SEL-SEC
               IF    ( TKIMEMO-SAVEPARA =   "1" )
                   MOVE    "T"     TO  SPA-GMN-W10-CHK-SAVEPARA
               ELSE
                   MOVE    "F"     TO  SPA-GMN-W10-CHK-SAVEPARA
               END-IF
               IF    ( SYS-TOUKEI-KANRICD  =   "0043" OR "0044" )
                   MOVE    "T"     TO  SPA-GMN-W10-CHK-SAVEPARA
               END-IF
      *
               MOVE    TKIMEMO-MEMO    TO  SPA-GMN-W10-MEMO
      *
               MOVE    1               TO  SPA-GMN-W10-CUR
      *
           ELSE
                   MOVE    1               TO  FLG-PARA-SET
           END-IF
      *
           .
       3301-PARA-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF      SPA-WID1-FLG    =   "OK"
               IF      SPA-WIDCD            =   "1002"
                   PERFORM 4031-DEL-SEC
               ELSE
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-W10-AREA
      *
           CALL    "ORCSUPDTOUKEIPRM"  USING   SPA-AREA
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
               IF    ( SBC-BILLINGUSER =   "1" )
                   MOVE    "1"         TO  SPA-W10-CUSTOM-ENABLED
               END-IF
           ELSE
               MOVE    "1"             TO  SPA-W10-CUSTOM-ENABLED
           END-IF
      *
           MOVE    SPA-W01-STYUKYMD    TO  SPA-W10-STYUKYMD
           MOVE    SPA-W01-EDYUKYMD    TO  SPA-W10-EDYUKYMD
           MOVE    SPA-W01-STYUKYMDH   TO  SPA-W10-STYUKYMDH
           MOVE    SPA-W01-EDYUKYMDH   TO  SPA-W10-EDYUKYMDH
      *
           MOVE    SPA-W01-SELNUM      TO  SPA-W10-SELNUM
      *
           IF    ( SPA-W10-SELNUM      >   ZERO )
               MOVE    SPA-W01-SEL-STYUKYMD
                                       TO  SPA-W10-SEL-STYUKYMD
               MOVE    SPA-W01-SEL-EDYUKYMD
                                       TO  SPA-W10-SEL-EDYUKYMD
           ELSE
               MOVE    SPA-W01-STYUKYMD
                                       TO  SPA-W10-SEL-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD
                                       TO  SPA-W10-SEL-EDYUKYMD
           END-IF
      *
           MOVE    SPA-W01-KANRICD     TO  SPA-W10-KANRICD
           IF    ( SPA-W01-KANRICD     =   "3001" )
               MOVE    "3101"          TO  SPA-W10-KANRICD2
           ELSE
               MOVE    "3102"          TO  SPA-W10-KANRICD2
           END-IF
           MOVE    SPA-W01-KBNCD       TO SPA-W10-KBNCD
           IF    ( SPA-W01-KANRICD     =  "3001" )
               MOVE    CONST-3001-PARA-MAX TO  SPA-W10-PARA-MAX
           ELSE
               MOVE    CONST-3002-PARA-MAX TO  SPA-W10-PARA-MAX
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-W10-CUR
      *
           PERFORM 3101-GMN-CLEAR-SEC
      *
           INITIALIZE                      SYS-3001-REC
           MOVE    SPA-W10-KANRICD     TO  SYS-3001-KANRICD
           MOVE    SPA-W10-KBNCD       TO  SYS-3001-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD
                                       TO  SYS-3001-STYUKYMD
                                           SYS-3001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3001-HOSPNUM
           MOVE    SYS-3001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-3001-REC
               MOVE    SYS-3001-HCNAME TO  SPA-GMN-W10-HCNAME
               MOVE    SYS-3001-PRGNAME
                                       TO  SPA-GMN-W10-PRGNAME
                                           SPA-W10-PRGNAME
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-W10-PARA-MAX
                   MOVE    SYS-3001-PARAHYOJI(IDX)
                                       TO  SPA-GMN-W10-PARAHYOJI(IDX)
                   MOVE    SYS-3001-PARA     (IDX)
                                       TO  SPA-GMN-W10-PARA     (IDX)
                   IF    ( SYS-3001-HISSU-KBN(IDX)
                                           =   SPACE OR "1" )
                       MOVE    "T"     TO  SPA-GMN-W10-OPTHISSU (IDX)
                       MOVE    "F"     TO  SPA-GMN-W10-OPTNINI  (IDX)
                   ELSE
                       MOVE    "F"     TO  SPA-GMN-W10-OPTHISSU (IDX)
                       MOVE    "T"     TO  SPA-GMN-W10-OPTNINI  (IDX)
                   END-IF
               END-PERFORM
      *
               INITIALIZE                 SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    SPA-W10-KANRICD2 TO SYS-KANRICD
               MOVE    SPA-W10-KBNCD   TO  SYS-KBNCD
               MOVE    SPA-W10-SEL-STYUKYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC    TO  SYS-3101-REC
               MOVE    SYS-3101-REPOS-NAME
                                        TO  SPA-GMN-W10-REPOS-NAME
      *
               MOVE    SYS-3001-REC        TO  SYSKANRI-REC
               MOVE    SYS-3001-PRGNAME    TO  WRK-PGID
               PERFORM 3101-TOKEIMEMO-SEL-SEC
               IF    ( TKIMEMO-SAVEPARA =   "1" )
                   MOVE    "T"         TO  SPA-GMN-W10-CHK-SAVEPARA
               ELSE
                   MOVE    "F"         TO  SPA-GMN-W10-CHK-SAVEPARA
               END-IF
               MOVE    TKIMEMO-MEMO    TO  SPA-GMN-W10-MEMO
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-W10-CUR
      *
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面初期化処理
      *****************************************************************
       3101-GMN-CLEAR-SEC              SECTION.
      *
           INITIALIZE   SPA-GMN-W10-AREA
                        SPA-NAI-W10-AREA
      *
           MOVE        "T"             TO  SPA-GMN-W10-CHK-SAVEPARA
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX >       CONST-PARA-MAX )
               MOVE    "T"             TO  SPA-GMN-W10-OPTHISSU (IDX)
               MOVE    "F"             TO  SPA-GMN-W10-OPTNINI  (IDX)
           END-PERFORM
      *
           MOVE    SPA-W10-STYUKYMDH   TO  SPA-GMN-W10-STYUKYMD
           MOVE    SPA-W10-EDYUKYMDH   TO  SPA-GMN-W10-EDYUKYMD
      *
           EVALUATE    SPA-W10-KANRICD
           WHEN    "0043"
           WHEN    "0044"
               MOVE    SPA-W10-KBNCD(2:3)
                                       TO  SPA-GMN-W10-HCNUM
               MOVE    1               TO  SPA-GMN-W10-CUR
           WHEN    "3001"
           WHEN    "3002"
               MOVE    SPA-W10-KBNCD   TO  SPA-GMN-W10-HCNUM
               MOVE    1               TO  SPA-GMN-W10-CUR
           END-EVALUATE
      *
           . 
       3101-GMN-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    統計メモテーブル検索処理
      *****************************************************************
       3101-TOKEIMEMO-SEL-SEC          SECTION.
      *
           IF    ( SYS-KANRICD         =   "3001" OR "3002" )
               INITIALIZE                      TOUKEIMEMO-REC
               MOVE    WRK-PGID            TO  TKIMEMO-PGID
               MOVE    SYS-KANRICD         TO  TKIMEMO-KANRICD
               MOVE    SYS-KBNCD           TO  TKIMEMO-KBNCD
               MOVE    SYS-STYUKYMD        TO  TKIMEMO-STYUKYMD
               MOVE    SYS-EDYUKYMD        TO  TKIMEMO-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
               MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
               PERFORM 900-TOUKEIMEMO-KEY-SEL-SEC
               IF    ( FLG-TOUKEIMEMO  =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  TKIMEMO-SAVEPARA
               END-IF
      *
               MOVE    TOUKEIMEMO-REC      TO  WTMEMO-REC
      *
               INITIALIZE                      TOUKEIMEMO-REC
               MOVE    WRK-PGID            TO  TKIMEMO-PGID
               MOVE    "0000"              TO  TKIMEMO-KANRICD
               MOVE    "000"               TO  TKIMEMO-KBNCD
               MOVE    "00000000"          TO  TKIMEMO-STYUKYMD
               MOVE    "99999999"          TO  TKIMEMO-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
               MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
               PERFORM 900-TOUKEIMEMO-KEY-SEL-SEC
               IF    ( FLG-TOUKEIMEMO  =   ZERO )
                   MOVE    WTMEMO-SAVEPARA
                                       TO  TKIMEMO-SAVEPARA
               ELSE
                   MOVE    WTMEMO-REC  TO  TOUKEIMEMO-REC
               END-IF
           ELSE
                   INITIALIZE                  TOUKEIMEMO-REC
                   MOVE    WRK-PGID        TO  TKIMEMO-PGID
                   MOVE    "0000"          TO  TKIMEMO-KANRICD
                   MOVE    "000"           TO  TKIMEMO-KBNCD
                   MOVE    "00000000"      TO  TKIMEMO-STYUKYMD
                   MOVE    "99999999"      TO  TKIMEMO-EDYUKYMD
                   MOVE    SPA-HOSPNUM     TO  TKIMEMO-HOSPNUM
                   MOVE    TOUKEIMEMO-REC  TO  MCPDATA-REC
                   PERFORM 900-TOUKEIMEMO-KEY-SEL-SEC
           END-IF
           .
      *
       3101-TOKEIMEMO-SEL-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *    画面初期化
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 3101-GMN-CLEAR-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 403-DEL-SEC
      *    帳票一覧画面へ
               WHEN    "CLICKED"       ALSO    "B05"
                   MOVE    ZERO        TO  SPA-NAI-W10-W19-SENI-FLG
                   PERFORM 405-W19-SENI-SEC
      *    帳票一覧画面へ
               WHEN    "CLICKED"       ALSO    "B06"
                   IF    ( SPA-W10-MODE    NOT =   ZERO )
                       MOVE    1       TO  SPA-NAI-W10-W19-SENI-FLG
                       PERFORM 405-W19-SENI-SEC
                   END-IF
               WHEN    "CLICKED"       ALSO    "B07"
                   IF    ( SPA-W10-MODE    NOT =   ZERO )
                       PERFORM 407-NEW-FORM-INIT-SEC
                   END-IF
               WHEN    "CLICKED"       ALSO    "B08"
      *
                   MOVE    ZERO            TO      SPA-NAI-W10-B11-CNT
                   IF    ( SPA-NAI-W10-B08-CNT >   5 )
                       MOVE    ZERO        TO      SPA-NAI-W10-B08-CNT
                   ELSE
                       COMPUTE SPA-NAI-W10-B08-CNT
                           =   SPA-NAI-W10-B08-CNT +   1
                   END-IF
      *    
               WHEN    "CLICKED"       ALSO    "B11"
                   IF    ( SPA-W10-MODE    =   ZERO )
                    AND  ( SPA-MW      NOT =   SPA-GINBEE )
                       PERFORM 411-MAINTENANCE-MODE-CHK-SEC
                   END-IF
      *
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    必須オプションボタン
               WHEN    "CLICKED"       ALSO    "OPTHISSU01"
                                         THRU  "OPTHISSU10"
                   MOVE    MCP-WIDGET  TO      WRK-WIDGET
                   PERFORM 4201-OPTHISSU-CHK-SEC
      *    任意オプションボタン
               WHEN    "CLICKED"       ALSO    "OPTNINI01"
                                         THRU  "OPTNINI10"
                   MOVE    MCP-WIDGET  TO      WRK-WIDGET
                   PERFORM 4202-OPTNINI-CHK-SEC
               WHEN    "CLICKED"       ALSO    "CHK_SAVEPARA"
                   PERFORM 4101-GMN-SPA-SET-SEC
                   MOVE    W10-CHK-SAVEPARA
                                       TO  SPA-GMN-W10-CHK-SAVEPARA
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
           MOVE    ZERO                TO  SPA-GMN-W10-CUR
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
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
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
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
           MOVE    W10-HCNAME          TO  SPA-GMN-W10-HCNAME
           MOVE    W10-PROGNAME        TO  SPA-GMN-W10-PRGNAME
           MOVE    W10-REPOS-NAME      TO  SPA-GMN-W10-REPOS-NAME
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL       IDZ     >   SPA-W10-PARA-MAX
               MOVE    W10-PARAHYOJI (IDZ)
                                       TO  SPA-GMN-W10-PARAHYOJI(IDZ)
               MOVE    W10-PARA (IDZ)  TO  SPA-GMN-W10-PARA(IDZ)
           END-PERFORM
           MOVE    W10-MEMO            TO  SPA-GMN-W10-MEMO
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
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4101-HCNAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4102-PRGNAME-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               IF    ( SPA-MW  =   SPA-GINBEE )
                   PERFORM 4102-REPOS-NAME-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4103-PARAHYOJI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4104-PARA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 4105-MEMO-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票名チェック
      *****************************************************************
       4101-HCNAME-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-W10-HCNAME      TO  WRK-MAE-INPUT
           PERFORM 5002-ZENKAKU-CHK-SEC
           MOVE    WRK-OUT-INPUT           TO  SPA-GMN-W10-HCNAME
           IF    ( FLG-ZENKAKU NOT =   ZERO )
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-W10-CUR
               GO  TO  4101-HCNAME-CHK-EXT
           END-IF
      *
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-W10-HCNAME
      *
           .
       4101-HCNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラム名チェック
      *****************************************************************
       4102-PRGNAME-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-W10-PRGNAME     TO  WRK-MAE-INPUT
           PERFORM 5002-HANKAKU-CHK-SEC
           MOVE    WRK-OUT-INPUT           TO  SPA-GMN-W10-PRGNAME
           IF    ( FLG-HANKAKU NOT =   ZERO )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-W10-CUR
               GO  TO  4102-PRGNAME-CHK-EXT
           END-IF
      *
           .
       4102-PRGNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レポジトリ名チェック
      *****************************************************************
       4102-REPOS-NAME-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-W10-REPOS-NAME  TO  WRK-MAE-INPUT
           PERFORM 5002-HANKAKU-CHK-SEC
           MOVE    WRK-OUT-INPUT           TO  SPA-GMN-W10-REPOS-NAME
           IF    ( FLG-HANKAKU NOT =   ZERO )
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    24                  TO  SPA-GMN-W10-CUR
           END-IF
      *
           .
       4102-REPOS-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ表示名称チェック
      *****************************************************************
       4103-PARAHYOJI-CHK-SEC     SECTION.
      *
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL     ( IDZ         >   SPA-W10-PARA-MAX )
                          OR ( SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-W10-PARAHYOJI(IDZ)  =   SPACE
                   MOVE    SPACE       TO  SPA-GMN-W10-PARA (IDZ)
                   MOVE    "T"         TO  SPA-GMN-W10-OPTHISSU (IDZ)
                   MOVE    "F"         TO  SPA-GMN-W10-OPTNINI  (IDZ)
               ELSE
                   MOVE    SPA-GMN-W10-PARAHYOJI(IDZ)
                                       TO  WRK-MAE-INPUT
                   PERFORM 5002-ZENKAKU-CHK-SEC
                   MOVE    WRK-OUT-INPUT
                                       TO  SPA-GMN-W10-PARAHYOJI(IDZ)
                   IF    ( FLG-ZENKAKU NOT =   ZERO )
                       MOVE    "0004"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                       COMPUTE SPA-GMN-W10-CUR
                           =   ( IDZ - 1 ) * 2 + 4
               END-IF
      *
           END-PERFORM
      *
           .
       4103-PARAHYOJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタチェック
      *****************************************************************
       4104-PARA-CHK-SEC     SECTION.
      *
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ     >   SPA-W10-PARA-MAX )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    SPA-GMN-W10-PARA (IDZ)
                                           TO  WRK-MAE-INPUT
               PERFORM 5002-HANKAKU-CHK-SEC
               MOVE    WRK-OUT-INPUT       TO  SPA-GMN-W10-PARA (IDZ)
               IF    ( FLG-HANKAKU NOT =   ZERO )
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE SPA-GMN-W10-CUR
                       =   ( IDZ - 1 ) * 2 + 5
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4104-PARA-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   ZERO
               GO      TO      4104-PARA-CHK-EXT
           END-IF
      *
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ     >   SPA-W10-PARA-MAX   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-W10-PARAHYOJI(IDZ)  =   SPACE
                   IF      SPA-GMN-W10-PARA (IDZ)  NOT =   SPACE
                       MOVE    "0001"              TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      SPA-GMN-W10-PARA (IDZ)   =   SPACE
                       MOVE    "0001"           TO  SPA-ERRCD
                   END-IF
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   COMPUTE SPA-GMN-W10-CUR
                       =   ( IDZ - 1 ) * 2 + 5
               END-IF
           END-PERFORM
           .
       4104-PARA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ欄チェック処理
      *****************************************************************
       4105-MEMO-CHK-SEC               SECTION.
      *
           IF    ( SPA-GMN-W10-MEMO (CONST-MEMO-MAX : 1)
                                      >=   X"A1" )
               MOVE    SPACE   TO  SPA-GMN-W10-MEMO (CONST-MEMO-MAX: )
           END-IF
      *
           .
       4105-MEMO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    必須オプションボタンチェック
      *****************************************************************
       4201-OPTHISSU-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET8         TO  IDZ
      *
           IF    ( SPA-GMN-W10-PARAHYOJI(IDZ)  =   SPACE )
               MOVE    "T"             TO  SPA-GMN-W10-OPTHISSU (IDZ)
               MOVE    "F"             TO  SPA-GMN-W10-OPTNINI  (IDZ)
           ELSE
               MOVE    W10-OPTHISSU (IDZ)
                                       TO  SPA-GMN-W10-OPTHISSU (IDZ)
               MOVE    W10-OPTNINI  (IDZ)
                                       TO  SPA-GMN-W10-OPTNINI  (IDZ)
           END-IF
      *
           .
       4201-OPTHISSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    任意オプションボタンチェック
      *****************************************************************
       4202-OPTNINI-CHK-SEC        SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    WRK-WIDGET7         TO  IDZ
      *
           IF    ( SPA-GMN-W10-PARAHYOJI(IDZ)  =   SPACE )
               MOVE    "T"             TO  SPA-GMN-W10-OPTHISSU (IDZ)
               MOVE    "F"             TO  SPA-GMN-W10-OPTNINI  (IDZ)
           ELSE
               MOVE    W10-OPTHISSU (IDZ)
                                       TO  SPA-GMN-W10-OPTHISSU (IDZ)
               MOVE    W10-OPTNINI  (IDZ)
                                       TO  SPA-GMN-W10-OPTNINI  (IDZ)
           END-IF
      *
           .
       4202-OPTNINI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W10"               TO  SPA-MOTOPG
      *
      *    INITIALIZE                  SPA-W01KYOUTU
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
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票一覧画面遷移処理
      *****************************************************************
       405-W19-SENI-SEC                    SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-W10FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGW19"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK
           MOVE    SPA-FREE            TO  SPA-W10FREE
      *
           MOVE    "W10"               TO  SPA-MOTOPG
           MOVE    "W19"               TO  SPA-SAKIPG
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "W19"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .    
       405-W19-SENI-EXT.
           EXIT.
      *****************************************************************
      *    帳票追加モード
      *****************************************************************
       407-NEW-FORM-INIT-SEC           SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SPA-W10-KANRICD TO  SYS-KANRICD
           IF    ( SPA-W10-MODE    =   1 )
               MOVE    "1%"        TO  SYS-KBNCD
           ELSE
               MOVE    "2%"        TO  SYS-KBNCD
           END-IF
           PERFORM 900-SYSKANRI-KEY22-SEL-SEC
           MOVE    SYS-KBNCD       TO  WRK-MNTCD-G
           IF    ( WRK-MNTCD-NUM   IS  NOT NUMERIC )
               MOVE    ZERO        TO  WRK-MNTCD-NUM
           END-IF
           COMPUTE WRK-MNTCD-NUM   =   WRK-MNTCD-NUM   +   1
           MOVE    WRK-MNTCD-NUM   TO  SPA-W10-KBNCD
           PERFORM 4110-MAINTENANCE-INIT-SEC
           MOVE    1               TO  SPA-GMN-W10-CUR
      *
           .    
       407-NEW-FORM-INIT-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-3001-REC
           INITIALIZE                      SYS-3001-REC
           MOVE    SPA-W10-KANRICD         TO  SYS-3001-KANRICD
           MOVE    SPA-W10-KBNCD           TO  SYS-3001-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD    TO  SYS-3001-STYUKYMD
           MOVE    SPA-W10-SEL-EDYUKYMD    TO  SYS-3001-EDYUKYMD
           MOVE    SPA-HOSPNUM             TO  SYS-3001-HOSPNUM
           MOVE    SYS-3001-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_syskanri"          TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              NOT =   ZERO  )
               MOVE    "4002"              TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
            AND  ( SPA-W10-KANRICD         =   "3001" OR "3002" )
               MOVE    SPACE               TO  SYS-3101-REC
               INITIALIZE                      SYS-3101-REC
               MOVE    SPA-W10-KANRICD2    TO  SYS-3101-KANRICD
               MOVE    SPA-W10-KBNCD       TO  SYS-3101-KBNCD
               MOVE    SPA-W10-SEL-STYUKYMD TO SYS-3101-STYUKYMD
               MOVE    SPA-W10-SEL-EDYUKYMD TO SYS-3101-EDYUKYMD
               MOVE    SPA-HOSPNUM          TO SYS-3101-HOSPNUM
               MOVE    SYS-3101-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF    ( MCP-RC              NOT =   ZERO  )
                   MOVE    "4008"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
      *
               IF    ( SPA-W10-MODE            =   ZERO )
                   PERFORM 4032-TOUKEIMEMO-DEL-SEC
               ELSE
                   MOVE    SPA-W10-PRGNAME     TO  WRK-PRGNAME
                   PERFORM 4032-TOUKEIMEMO-ORG-DEL-SEC
               END-IF
      *
               IF    ( SPA-ERRCD           NOT =   SPACE )
                   MOVE    "4004"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計メモ削除処理
      *****************************************************************
       4032-TOUKEIMEMO-DEL-SEC         SECTION.
      *
           INITIALIZE                      TOUKEIMEMO-REC
           MOVE    SPA-W10-KANRICD         TO  TKIMEMO-KANRICD
           MOVE    SPA-W10-KBNCD           TO  TKIMEMO-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD    TO  TKIMEMO-STYUKYMD
           MOVE    SPA-W10-SEL-EDYUKYMD    TO  TKIMEMO-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
           MOVE    TOUKEIMEMO-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"        TO  MCP-TABLE
           MOVE    "del1"                  TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "4005"              TO  SPA-ERRCD
               GO  TO  4032-TOUKEIMEMO-DEL-EXT
           END-IF
      *
      *
           .
       4032-TOUKEIMEMO-DEL-EXT.
           EXIT.
      *****************************************************************
      *    統計メモ元データ削除処理
      *****************************************************************
       4032-TOUKEIMEMO-ORG-DEL-SEC     SECTION.
      *
           INITIALIZE                      TOUKEIMEMO-REC
           MOVE    WRK-PRGNAME             TO  TKIMEMO-PGID
           MOVE    "0000"                  TO  TKIMEMO-KANRICD
           MOVE    "000"                   TO  TKIMEMO-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD    TO  TKIMEMO-STYUKYMD
           MOVE    SPA-W10-SEL-EDYUKYMD    TO  TKIMEMO-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
           MOVE    TOUKEIMEMO-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"              TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"        TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    "4006"              TO  SPA-ERRCD
               GO  TO  4032-TOUKEIMEMO-ORG-DEL-EXT
           END-IF
      *
      *
           .
       4032-TOUKEIMEMO-ORG-DEL-EXT.
           EXIT.
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *    入力チェック処理
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           PERFORM 4900-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-WIDCD
           END-IF
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           MOVE    SPACE                   TO  WRK-PRGNAME
      *
           MOVE    SPACE                   TO  SYS-3001-REC 
           INITIALIZE                          SYS-3001-REC
      *
           MOVE    SPA-W10-KANRICD         TO  SYS-3001-KANRICD
           MOVE    SPA-W10-KBNCD           TO  SYS-3001-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD    TO  SYS-3001-STYUKYMD
                                               SYS-3001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3001-HOSPNUM
           MOVE    SYS-3001-REC            TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-3001-REC
           ELSE
               MOVE    SPACE           TO  SYS-3001-REC
               INITIALIZE                  SYS-3001-REC
               MOVE    SPA-W10-KANRICD TO  SYS-3001-KANRICD
               MOVE    SPA-W10-KBNCD   TO  SYS-3001-KBNCD
           END-IF 
           MOVE    SPA-W10-STYUKYMD    TO  SYS-3001-STYUKYMD
           MOVE    SPA-W10-EDYUKYMD    TO  SYS-3001-EDYUKYMD
           MOVE    SPA-GMN-W10-HCNAME  TO  SYS-3001-HCNAME 
           MOVE    SPA-GMN-W10-PRGNAME TO  SYS-3001-PRGNAME
                                           WRK-PRGNAME
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-W10-PARA-MAX
               MOVE    SPA-GMN-W10-PARAHYOJI (IDX)
                                       TO  SYS-3001-PARAHYOJI(IDX)
               MOVE    SPA-GMN-W10-PARA (IDX)
                                       TO  SYS-3001-PARA     (IDX)
               IF    ( SPA-GMN-W10-PARAHYOJI (IDX) NOT = SPACE )
                   IF    ( SPA-GMN-W10-OPTHISSU(IDX)   =   "T" )
                       MOVE    "1"     TO  SYS-3001-HISSU-KBN(IDX)
                   ELSE
                       MOVE    "2"     TO  SYS-3001-HISSU-KBN(IDX)
                   END-IF
               ELSE
                       MOVE    SPACE   TO  SYS-3001-HISSU-KBN(IDX)
               END-IF
           END-PERFORM
           MOVE    SYS-3001-REC        TO  SYSKANRI-REC
      *     
           IF      FLG-SYSKANRI            =   ZERO
               MOVE    SYS-KANRICD             TO  SYSUP-KANRICD
               MOVE    SYS-KBNCD               TO  SYSUP-KBNCD
               MOVE    SPA-W10-SEL-STYUKYMD    TO  SYSUP-STYUKYMD
               MOVE    SPA-W10-SEL-EDYUKYMD    TO  SYSUP-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  SYSUP-HOSPNUM
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "upd1"                  TO  MCP-PATHNAME
               MOVE    "DBUPDATE"              TO  MCP-FUNC
           ELSE
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD            TO  SYS-CREYMD
                                                   SYS-UPYMD
               MOVE    SMCNDATE-HMS            TO  SYS-UPHMS
               MOVE    "tbl_syskanri"          TO  MCP-TABLE
               MOVE    "key"                   TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
           END-IF
           MOVE    SPA-HOSPNUM             TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC            TO  MCPDATA-REC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "4001"                  TO  SPA-ERRCD
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
           IF    ( SYS-KANRICD             =   "3001" OR "3002" )
               PERFORM 491-SYS-3101-3102-UPD-SEC
           END-IF
      *
           IF    ( SYS-KANRICD             =   "0043" OR "0044" )
               PERFORM 800-MCNDATE-SEC
               MOVE    SPACE               TO  ASS-SYSKANRI
               INITIALIZE  ASS-SYSKANRI
               STRING      "/tmp/ORCADBR-syskanri-"
                                           DELIMITED   BY  SIZE
                           WRK-PRGNAME     DELIMITED   BY  SPACE
                           ".dat"          DELIMITED   BY  SIZE
               INTO    ASS-SYSKANRI
               END-STRING
      *
               OPEN    OUTPUT  MST-SYSKANRI
               MOVE    SPACE   TO  SYSWK-REC
               INITIALIZE      SYSWK-REC
               IF    ( SPA-W10-SELNUM  >   ZERO )
                   MOVE    "U"         TO  SYSWK-SYORIKBN
               ELSE
                   MOVE    "I"         TO  SYSWK-SYORIKBN
               END-IF
               MOVE    SYS-KANRICD     TO  SYSWK-KANRICD
               MOVE    SYS-KBNCD       TO  SYSWK-KBNCD
               MOVE    SYS-STYUKYMD    TO  SYSWK-STYUKYMD
               MOVE    SYS-EDYUKYMD    TO  SYSWK-EDYUKYMD
               MOVE    SYS-KANRITBL    TO  SYSWK-KANRITBL
               WRITE   SYSWK-REC
               CLOSE   MST-SYSKANRI
           END-IF
      *
           PERFORM 491-TOUKEIMEMO-UPD-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  490-KOUSHIN-EXT
           END-IF
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           PERFORM 210-BACK 
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日次・月次統計付加情報
      *****************************************************************
       491-SYS-3101-3102-UPD-SEC        SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SPACE                   TO  SYS-3101-REC
           INITIALIZE                          SYS-3101-REC
           MOVE    SPA-HOSPNUM             TO  SYS-3101-HOSPNUM
           MOVE    SPA-W10-KANRICD2        TO  SYS-3101-KANRICD
           MOVE    SYS-KBNCD               TO  SYS-3101-KBNCD
           MOVE    SPA-W10-SEL-STYUKYMD    TO  SYS-3101-STYUKYMD
                                               SYS-3101-EDYUKYMD
           MOVE    SPA-GMN-W10-REPOS-NAME  TO  SYS-3101-REPOS-NAME
           MOVE    SMCNDATE-YMD            TO  SYS-3101-CREYMD
                                               SYS-3101-UPYMD
           MOVE    SMCNDATE-HMS            TO  SYS-3101-UPHMS
           MOVE    SYS-3101-REC            TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYS-KEY                 TO  SYS-3101-UPD-KEY
           MOVE    SPA-W10-STYUKYMD        TO  SYS-3101-STYUKYMD
           MOVE    SPA-W10-EDYUKYMD        TO  SYS-3101-EDYUKYMD
           IF    ( FLG-SYSKANRI            =   ZERO )
               MOVE    SYS-CREYMD          TO  SYS-3101-CREYMD
               MOVE    SYS-3101-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4009"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SYS-3101-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "4010"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       491-SYS-3101-3102-UPD-EXT.
           EXIT.
      *****************************************************************
      *    統計メモテーブル更新処理
      *****************************************************************
       491-TOUKEIMEMO-UPD-SEC      SECTION.
      *
           INITIALIZE                          WTMEMO-REC
      *
           IF    ( SPA-W10-MODE            =   ZERO )
      *
               INITIALIZE                          TOUKEIMEMO-REC
               MOVE    SPA-GMN-W10-PRGNAME     TO  TKIMEMO-PGID
               MOVE    SPA-W10-KANRICD         TO  TKIMEMO-KANRICD
               MOVE    SPA-W10-KBNCD           TO  TKIMEMO-KBNCD
               MOVE    SPA-W10-SEL-STYUKYMD    TO  TKIMEMO-STYUKYMD
               MOVE    SPA-W10-SEL-EDYUKYMD    TO  TKIMEMO-EDYUKYMD
               MOVE    SPA-HOSPNUM             TO  TKIMEMO-HOSPNUM
               MOVE    TOUKEIMEMO-REC          TO  MCPDATA-REC
               PERFORM 900-TOUKEIMEMO-KEY-SEL-SEC
               MOVE    TOUKEIMEMO-REC          TO  WTMEMO-REC
      *
               PERFORM 4032-TOUKEIMEMO-DEL-SEC
           ELSE
               MOVE    SPA-W10-PRGNAME TO  WRK-PRGNAME
               PERFORM 4032-TOUKEIMEMO-ORG-DEL-SEC
      *
               MOVE    SPA-GMN-W10-PRGNAME
                                       TO  WRK-PRGNAME
               PERFORM 4032-TOUKEIMEMO-ORG-DEL-SEC
           END-IF
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    "4007"          TO  SPA-ERRCD
               GO  TO  491-TOUKEIMEMO-UPD-EXT
           END-IF
      *
           INITIALIZE              TOUKEIMEMO-REC
      *
           MOVE    SPA-GMN-W10-PRGNAME
                                       TO  TKIMEMO-PGID
           MOVE    SPA-GMN-W10-MEMO    TO  TKIMEMO-MEMO
           MOVE    WTMEMO-USERMEMO     TO  TKIMEMO-USERMEMO
      *
           IF    ( SPA-GMN-W10-CHK-SAVEPARA    =   "T" )
               MOVE    "1"             TO  TKIMEMO-SAVEPARA
           ELSE
               MOVE    " "             TO  TKIMEMO-SAVEPARA
           END-IF
      *
      *    IF    ( SPA-GMN-W10-CHK-SAVEPARA    =   "F" )
               MOVE    SPACE           TO  TKIMEMO-PARA-G
      *    END-IF
      *
           IF    ( SPA-W10-MODE        =   ZERO )
               MOVE    SPA-W10-KANRICD TO  TKIMEMO-KANRICD
               MOVE    SPA-W10-KBNCD   TO  TKIMEMO-KBNCD
           ELSE
               MOVE    "0000"          TO  TKIMEMO-KANRICD
               MOVE    "000"           TO  TKIMEMO-KBNCD
           END-IF
      *
           MOVE    SPA-W10-STYUKYMD    TO  TKIMEMO-STYUKYMD
           MOVE    SPA-W10-EDYUKYMD    TO  TKIMEMO-EDYUKYMD
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  TKIMEMO-CREYMD
                                           TKIMEMO-UPYMD
           MOVE    SMCNDATE-HMS        TO  TKIMEMO-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  TKIMEMO-HOSPNUM
           MOVE    TOUKEIMEMO-REC      TO  MCPDATA-REC
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF    ( MCP-RC              =   ZERO )
               IF    ( SPA-W10-MODE    =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 800-MCNDATE-SEC
                   INITIALIZE  ASS-TMEMO
                   STRING      "/tmp/ORCADBR-toukeimemo-"
                                               DELIMITED   BY  SIZE
                               TKIMEMO-PGID    DELIMITED   BY  SPACE
                               ".dat"          DELIMITED   BY  SIZE
                   INTO        ASS-TMEMO
                   END-STRING
                   OPEN OUTPUT MST-TMEMO
                   MOVE    SPACE           TO  TOUKEIMEMOWK-REC
                   INITIALIZE                  TOUKEIMEMOWK-REC
                   IF    ( SPA-W10-SELNUM  >   ZERO )
                       MOVE    "U"         TO  TKIMEMOWK-SYORIKBN
                   ELSE
                       MOVE    "I"         TO  TKIMEMOWK-SYORIKBN
                   END-IF
                   MOVE    TOUKEIMEMO-REC  TO  TOUKEIMEMOWK-REC (2:)
                   MOVE    ZERO            TO  TKIMEMOWK-HOSPNUM
                   MOVE    SPACE           TO  TKIMEMOWK-MEMO
                   MOVE    1               TO  WRK-LEN
                   STRING  TKIMEMO-MEMO    DELIMITED   BY  SIZE
                   INTO    TKIMEMOWK-MEMO
                   WITH    POINTER     WRK-LEN
                   END-STRING
                   COMPUTE WRK-LEN =   WRK-LEN -   1
      *
                   MOVE    SPACE           TO  TKIMEMOWK-MEMO
                   MOVE    1               TO  IDX1
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0  > WRK-LEN )
                            OR   ( IDX1  > WRK-LEN )
                            OR   ( TKIMEMO-MEMO (IDX0 :) = SPACE )
      *
                       IF    ( TKIMEMO-MEMO (IDX0:1) = X"0A" )
                           MOVE    "\n"    TO TKIMEMOWK-MEMO (IDX1:2)
                           COMPUTE IDX1    =   IDX1    +   2
                       ELSE
                           MOVE    TKIMEMO-MEMO (IDX0:1)
                                           TO TKIMEMOWK-MEMO (IDX1:2)
                           COMPUTE IDX1    =   IDX1    +   1
                       END-IF
      *
                   END-PERFORM
      *
                   MOVE    SPACE           TO TKIMEMOWK-SAVEPARA
                   WRITE   TOUKEIMEMOWK-REC
      *
                   CLOSE   MST-TMEMO
               END-IF
           ELSE
               MOVE    "4003"          TO  SPA-ERRCD
           END-IF
      *
           .
       491-TOUKEIMEMO-UPD-EXT.
           EXIT.
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4900-KANRENCHK-SEC           SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRCD
      *
           IF      SPA-GMN-W10-HCNAME      =   SPACE    
           OR      SPA-GMN-W10-PRGNAME     =   SPACE   
               MOVE  "0099"                TO  SPA-ERRCD
               MOVE   1                    TO  SPA-GMN-W10-CUR
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-MW              =   SPA-GINBEE )
                AND  ( SPA-GMN-W10-REPOS-NAME  =   SPACE )
                   INITIALIZE                      SORCAPG-AREA
                   MOVE    SPA-W10-STYUKYMD    TO  SORCAPG-SYSYMD
                   IF    ( SPA-W10-KANRICD     =   "3001" )
                       MOVE    "DAILY"         TO  SORCAPG-TYPE
                   ELSE
                       MOVE    "MONTHLY"       TO  SORCAPG-TYPE
                   END-IF
                   MOVE    SPA-GMN-W10-PRGNAME TO  SORCAPG-PGID
                   CALL    "ORCSORCAPG"        USING
                                               SORCAPG-AREA
                                               SPA-AREA
                   IF    ( SORCAPG-RC      NOT =   ZERO )
                       MOVE    "0007"          TO  SPA-ERRCD
                       MOVE    3               TO  SPA-GMN-W10-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       4900-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   メンテナンスモードチェック処理
      *****************************************************************
       411-MAINTENANCE-MODE-CHK-SEC    SECTION.
      *
           IF    ( SPA-NAI-W10-B08-CNT     =   2)
               MOVE    1           TO  SPA-W10-MODE
           END-IF
      *
           IF    ( SPA-NAI-W10-B08-CNT     =   3)
               MOVE    2           TO  SPA-W10-MODE
           END-IF
      *
           IF    ( SPA-W10-MODE =   ZERO )
               MOVE    ZERO        TO  SPA-NAI-W10-B08-CNT
           ELSE
               PERFORM 4110-MAINTENANCE-INIT-SEC
           END-IF
      *
           .
       411-MAINTENANCE-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    メンテナンスモード初期処理
      *****************************************************************
       4110-MAINTENANCE-INIT-SEC           SECTION.
      *
           IF    ( SPA-W10-KANRICD     =   "3001" OR "3002" )
               IF    ( SPA-W10-KANRICD =   "3001" )
                   MOVE    "0043"      TO  SPA-W10-KANRICD
               ELSE
                   MOVE    "0044"      TO  SPA-W10-KANRICD
               END-IF
           END-IF
      *
           MOVE    SPA-W10-KBNCD       TO  WRK-KBNCD
           IF    ( SPA-W10-MODE =   1 )
               MOVE    "1"             TO  SPA-W10-KBNCD (1:1)
           ELSE
               MOVE    "2"             TO  SPA-W10-KBNCD (1:1)
           END-IF
           MOVE    WRK-KBNCD           TO  SPA-W10-KBNCD (2:)
      *
           MOVE    "00000000"          TO  SPA-W10-STYUKYMD
                                           SPA-W10-STYUKYMDH
                                           SPA-W10-SEL-STYUKYMD
           MOVE    "99999999"          TO  SPA-W10-EDYUKYMD
                                           SPA-W10-EDYUKYMDH
                                           SPA-W10-SEL-EDYUKYMD
      *
           MOVE    SPACE               TO  SPA-W10-PRGNAME
      *
           MOVE    ZERO                TO  SPA-W10-SELNUM
      *
           MOVE    SPA-W10-KANRICD     TO  WRK-KANRICD
           MOVE    SPA-W10-KBNCD       TO  WRK-KBNCD
      *
           PERFORM 3301-PARA-SET-SEC
      *
           IF    ( FLG-PARA-SET    =   ZERO )
               MOVE    1           TO  SPA-W10-SELNUM
           ELSE
               MOVE    ZERO        TO  SPA-W10-SELNUM
           END-IF
      *
           .
       4110-MAINTENANCE-INIT-EXT.
           EXIT.
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
           MOVE    "W10    "           TO  MCP-WINDOW.
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
           INITIALIZE                       W10
      *
           IF    ( SPA-W01-KANRICD      =   "3001" )
               MOVE    "日次統計"       TO  W10-LBLTITLE
            ELSE
               MOVE    "月次統計"       TO  W10-LBLTITLE
            END-IF
      *
           IF    ( SPA-W10-MODE     NOT =   ZERO )
      *
               MOVE    "帳票選択"           TO  W10-B06
      *
               MOVE    "帳票追加"           TO  W10-B07
      *
               IF    ( SPA-W10-MODE     =   1 )
                   MOVE    "red"            TO  W10-LBLKANRICD-STYLE
               ELSE
                   MOVE    "blue"           TO  W10-LBLKANRICD-STYLE
               END-IF
      *
               IF    ( SPA-W01-KANRICD      =   "3001" )
                   MOVE    "００４３"       TO  W10-LBLKANRICD
               ELSE
                   MOVE    "００４４"       TO  W10-LBLKANRICD
               END-IF
      *
           ELSE
                   MOVE    "black"          TO  W10-LBLKANRICD-STYLE
           END-IF
      *
      *
           EVALUATE    SPA-W10-MODE
           WHEN    1
               MOVE    "red"                TO  W10-LBLKIKAN-STYLE
           WHEN    2
               MOVE    "blue"               TO  W10-LBLKIKAN-STYLE
           WHEN    OTHER
               MOVE    "black"              TO  W10-LBLKIKAN-STYLE
           END-EVALUATE
      *
           MOVE    SPA-GMN-W10-STYUKYMD     TO  W10-STYUKYMD
           MOVE    SPA-GMN-W10-EDYUKYMD     TO  W10-EDYUKYMD
      *
           MOVE    SPA-GMN-W10-HCNUM        TO  W10-HCNUM
           MOVE    SPA-GMN-W10-CHK-SAVEPARA TO  W10-CHK-SAVEPARA
           IF    ( SPA-W10-MODE     NOT =   ZERO )
               MOVE    WIDGET-INSENSITIVE   TO  W10-CHK-SAVEPARA-STATE
           ELSE
               MOVE    WIDGET-NORMAL        TO  W10-CHK-SAVEPARA-STATE
           END-IF
           MOVE    SPA-GMN-W10-HCNAME       TO  W10-HCNAME
           MOVE    SPA-GMN-W10-PRGNAME      TO  W10-PROGNAME
           MOVE    SPA-GMN-W10-REPOS-NAME   TO  W10-REPOS-NAME
           PERFORM VARYING IDX FROM    1    BY  1
                   UNTIL   IDX >            CONST-PARA-MAX
               MOVE    SPA-GMN-W10-PARAHYOJI (IDX)
                                            TO  W10-PARAHYOJI(IDX)
               MOVE    SPA-GMN-W10-PARA (IDX)
                                            TO  W10-PARA (IDX)
               MOVE    SPA-GMN-W10-OPTHISSU (IDX)
                                        TO  W10-OPTHISSU (IDX)
               MOVE    SPA-GMN-W10-OPTNINI  (IDX)
                                        TO  W10-OPTNINI  (IDX)
      *
               IF    ( IDX              <=  SPA-W10-PARA-MAX )
                   MOVE    "T"          TO  WRK-PARA-VISIBLE
               ELSE
                   MOVE    "F"          TO  WRK-PARA-VISIBLE
               END-IF
               MOVE    WRK-PARA-VISIBLE TO  W10-PARAHYOJI-VISIBLE (IDX)
                                            W10-PARA-VISIBLE(IDX)
                                            W10-OPTHISSU-VISIBLE (IDX)
                                            W10-OPTNINI-VISIBLE  (IDX)
      *
           END-PERFORM
      *
           IF    ( SPA-W01-KANRICD      =   "3002" )
               MOVE    "　　　　　　　　９" TO  W10-LBLPARA09
               MOVE    "　　　　　　　１０" TO  W10-LBLPARA10
           END-IF
      *
           MOVE    "F"                      TO  W10-REPOS-NAME-VISIBLE
           IF    ( SPA-MW               =   SPA-GINBEE )
               IF    ( SPA-W10-CUSTOM-ENABLED  =   "1" )
                OR   ( SPA-GMN-W10-REPOS-NAME
                                       NOT =   SPACE )
                   MOVE    "T"              TO  W10-REPOS-NAME-VISIBLE
                   MOVE    "レポジトリ名"   TO  W10-LBLREPOS-NAME
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-W10-MEMO         TO  W10-MEMO
      *
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-INSENSITIVE     TO  W10-B03-STATE
           MOVE    WIDGET-INSENSITIVE     TO  W10-B12-STATE
      *
           IF      SPA-W10-SELNUM          >   ZERO
               MOVE    WIDGET-NORMAL      TO  W10-B03-STATE
           END-IF
      *
           IF    ( SPA-MW           =     SPA-GINBEE )
               IF    ( SPA-GMN-W10-REPOS-NAME
                                        =     SPACE )
                OR   ( SPA-W10-CUSTOM-ENABLED  =   "1" )
                   MOVE    WIDGET-NORMAL  TO  W10-B12-STATE
               END-IF
           ELSE
               MOVE    WIDGET-NORMAL      TO  W10-B12-STATE
           END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "帳票名は全角で指定してください" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "プログラム名は半角で指定してください" 
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "帳票パラメータ名は全角で指定してください" 
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "帳票パラメータＩＤは半角で指定してください" 
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "レポジトリ名は半角で指定してください" 
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                 MOVE "プログラムが複写可能な統計帳票一覧に存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "4001"    THRU    "4010"
                   MOVE    "更新処理に失敗しました"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W10"                TO  SPA-MOTOPG
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
               WHEN    "1002"
                   MOVE    "削除処理をします"    TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W10"                TO  SPA-MOTOPG
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
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF      SPA-GMN-W10-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
          END-IF
      *
          IF    ( SPA-GMN-W10-CUR              =   24 )
           AND  ( W10-REPOS-NAME-VISIBLE       =   "F" )
              MOVE    4                        TO  SPA-GMN-W10-CUR
          END-IF
      *
          IF    ( SPA-GMN-W10-CUR              =   20 )
           AND  ( W10-PARAHYOJI-VISIBLE (9)    =   "F" )
              MOVE    999                  TO  SPA-GMN-W10-CUR
          END-IF
      *
          IF    ( SPA-GMN-W10-CUR              =   999 )
              IF    ( W10-B12-STATE            =   WIDGET-INSENSITIVE )
                  MOVE    1                TO  SPA-GMN-W10-CUR
              END-IF
          END-IF
      *
          EVALUATE    SPA-GMN-W10-CUR
                WHEN    01
                    MOVE  "HCNAME"         TO   MCP-WIDGET
                WHEN    03
                    MOVE  "PROGNAME"       TO   MCP-WIDGET
                WHEN    04
                    MOVE  "PARAHYOJI01"    TO   MCP-WIDGET
                WHEN    05
                    MOVE  "PARA01"         TO   MCP-WIDGET
                WHEN    06
                    MOVE  "PARAHYOJI02"    TO   MCP-WIDGET
                WHEN    07
                    MOVE  "PARA02"         TO   MCP-WIDGET
                WHEN    08
                    MOVE  "PARAHYOJI03"    TO   MCP-WIDGET
                WHEN    09
                    MOVE  "PARA03"         TO   MCP-WIDGET
                WHEN    10
                    MOVE  "PARAHYOJI04"    TO   MCP-WIDGET
                WHEN    11
                    MOVE  "PARA04"         TO   MCP-WIDGET
                WHEN    12
                    MOVE  "PARAHYOJI05"    TO   MCP-WIDGET
                WHEN    13
                    MOVE  "PARA05"         TO   MCP-WIDGET
                WHEN    14
                    MOVE  "PARAHYOJI06"    TO   MCP-WIDGET
                WHEN    15
                    MOVE  "PARA06"         TO   MCP-WIDGET
                WHEN    16
                    MOVE  "PARAHYOJI07"    TO   MCP-WIDGET
                WHEN    17
                    MOVE  "PARA07"         TO   MCP-WIDGET
                WHEN    18
                    MOVE  "PARAHYOJI08"    TO   MCP-WIDGET
                WHEN    19
                    MOVE  "PARA08"         TO   MCP-WIDGET
                WHEN    20
                    MOVE  "PARAHYOJI09"    TO   MCP-WIDGET
                WHEN    21
                    MOVE  "PARA09"         TO   MCP-WIDGET
                WHEN    22
                    MOVE  "PARAHYOJI10"    TO   MCP-WIDGET
                WHEN    23
                    MOVE  "PARA10"         TO   MCP-WIDGET
                WHEN    24
                    MOVE  "REPOS_NAME"     TO   MCP-WIDGET
      *
                WHEN    999
                    MOVE  "B12"            TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "HCNAME"
                   MOVE    3           TO  SPA-GMN-W10-CUR
               WHEN    "PROGNAME"
                   MOVE    24          TO  SPA-GMN-W10-CUR
               WHEN    "REPOS_NAME"
                   MOVE    4           TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI01"
                   MOVE    5           TO  SPA-GMN-W10-CUR
               WHEN    "PARA01 "
                   MOVE    6           TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI02 "
                   MOVE    7           TO  SPA-GMN-W10-CUR
               WHEN    "PARA02 "
                   MOVE    8           TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI03 "
                   MOVE    9           TO  SPA-GMN-W10-CUR
               WHEN    "PARA03 "
                   MOVE    10          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI04 "
                   MOVE    11          TO  SPA-GMN-W10-CUR
               WHEN    "PARA04 "
                   MOVE    12          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI05 "
                   MOVE    13          TO  SPA-GMN-W10-CUR
               WHEN    "PARA05 "
                   MOVE    14          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI06 "
                   MOVE    15          TO  SPA-GMN-W10-CUR
               WHEN    "PARA06 "
                   MOVE    16          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI07 "
                   MOVE    17          TO  SPA-GMN-W10-CUR
               WHEN    "PARA07 "
                   MOVE    18          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI08 "
                   MOVE    19          TO  SPA-GMN-W10-CUR
               WHEN    "PARA08 "
                   MOVE    20          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI09 "
                   MOVE    21          TO  SPA-GMN-W10-CUR
               WHEN    "PARA09 "
                   MOVE    22          TO  SPA-GMN-W10-CUR
               WHEN    "PARAHYOJI10 "
                   MOVE    23          TO  SPA-GMN-W10-CUR
               WHEN    "PARA10 "
                   MOVE    999         TO  SPA-GMN-W10-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           IF      WRK-YMD  (1:7)  NOT NUMERIC
               INSPECT     WRK-YMD      REPLACING   ALL " " BY  "0"
               MOVE    WRK-YMD(1:1)     TO  WRK-WGO
               MOVE    WRK-YMD(2:2)     TO  WRK-WYY
               MOVE    WRK-YMD(5:2)     TO  WRK-WMM
               MOVE    WRK-YMD(8:2)     TO  WRK-WDD
           ELSE
               MOVE    WRK-YMD(1:7)     TO  WRK-WYMD
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
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
           END-IF
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
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
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       5002-ZENKAKU-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-ZENKAKU
           MOVE    SPACE               TO  WRK-OUT-INPUT
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1               TO  FLG-ZENKAKU
               MOVE    WRK-MAE-INPUT
                                       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       5002-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       5002-HANKAKU-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-HANKAKU
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   2     )
               MOVE    1               TO  FLG-HANKAKU
               MOVE    WRK-MAE-INPUT
                                       TO  WRK-OUT-INPUT
               IF    ( KANACHK-RC2     =   7 )
                   MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-OUT-INPUT
               END-IF
           ELSE
               MOVE    WRK-MAE-INPUT   TO  WRK-OUT-INPUT
           END-IF
      *
           .
       5002-HANKAKU-CHK-EXT.
           EXIT.
      *
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
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター検索処理
      *****************************************************************
       900-SYSKANRI-KEY22-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key22"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO        TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC TO  SYSKANRI-REC
               ELSE
                   MOVE    1           TO  FLG-SYSKANRI
               END-IF
           ELSE
                   MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
       	    .
       900-SYSKANRI-KEY22-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計メモテーブル検索処理
      *****************************************************************
       900-TOUKEIMEMO-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-TOUKEIMEMO
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC              =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_toukeimemo" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "tbl_toukeimemo"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-TOUKEIMEMO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO      MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
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
