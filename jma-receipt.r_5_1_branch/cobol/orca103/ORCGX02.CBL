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
       PROGRAM-ID.                               ORCGX02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ情報設定（Ｘ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/02/01  病名コードを７桁に変更
      *                                     削除ボタンの追加
      *  01.00.02    MCC-藤原     02/04/05  項目数を１００に変更
      *  01.00.03    MCC-多々納   02/04/26  印刷の追加      
      *  01.00.04    NACL-伊藤    02/10/24  薬剤と病名の時明細表示頁に      
      *                                     よる添字のバグ修正
      *  01.00.05    NACL-多々納  02/11/18  一括入力変更
      *  01.00.06    NACL-太田    03/11/19  有効期間編集修正
      *  01.00.07    NACL-太田    04/03/15  名称での検索も可能とする
      *                                     チェック項目複写画面への
      *                                     遷移を追加
      *  01.00.08    NACL-太田    04/07/07  検索画面戻り時のカーソル制御
      *                                     修正
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
      *  05.01.00    NACL-多々納  17/09/XX  GINBEE対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    チェックマスタ情報設定用共通パラメタ
           COPY    "X01COMMON-SPA".
      *
      *    チェックマスタ情報用画面スパ
           COPY    "X01SCR-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-X02.
           03  SPA-X02-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-STYUKYMD    PIC X(09).
                   07  SPA-GMN-EDYUKYMD    PIC X(09).
                   07  SPA-GMN-CHKKBN-G.
                       09  SPA-GMN-CHKKBN      PIC X(01).
                       09  SPA-GMN-CHKKBNFIL   PIC X(01).
                       09  SPA-GMN-CHKKBNANME  PIC X(48).
                   07  SPA-GMN-SRYCD       PIC X(09).
                   07  SPA-GMN-NAME        PIC X(100).              
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-MAX        PIC 9(05).
      *****************09  SPA-GMN-TBLO    OCCURS  60.
                       09  SPA-GMN-TBL-OCC OCCURS  400.
                           11  SPA-GMN-TTEKCD     PIC X(20).
                           11  SPA-GMN-TNAME      PIC X(100).
                           11  SPA-GMN-TSYORIKBN  PIC X(01).
                           11  SPA-GMN-TNGBNDKBN  PIC X(01).
                           11  SPA-GMN-TCHKERR    PIC 9(01).
                           11  SPA-GMN-TDATAKBN   PIC X(01).
                           11  SPA-GMN-TCHKERR2   PIC S9(01).
                           11  SPA-GMN-TTEKCD-BF  PIC X(20).
      *
           COPY    "ENUM-VALUE".
      *
      *
       01  CNT-AREA.
           03  CNT-BYOMEI                         PIC 9(03).
           03  CNT-SRYCD                          PIC 9(03).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-CHKMST                         PIC 9(01).
           03  FLG-TENSU                          PIC 9(01).
           03  FLG-BYOMEI                         PIC 9(01).
      *
           03  FLG-OK                             PIC 9(01).
           03  FLG-ZENKAKU                        PIC 9(01).
           03  FLG-INPUTCD                        PIC 9(01).
           03  FLG-KANJI                          PIC 9(01).
           03  FLG-KENSAKU-SENI                   PIC 9(01).
           03  FLG-CD-TUIKA                       PIC 9(01).
           03  FLG-TEKCDDEL                       PIC 9(01).
           03  FLG-MSTDATDISP                     PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(05).
           03  IDY                               PIC 9(05).
           03  IDZ                               PIC 9(05).
           03  IDZ1                              PIC 9(05).
           03  IDX0                              PIC 9(05).
           03  IDX1                              PIC 9(05).
           03  IDX2                              PIC 9(05).
           03  IDX3                              PIC 9(05).
           03  IDX4                              PIC 9(05).
           03  IDX5                              PIC 9(05).
           03  IDX6                              PIC 9(05).
           03  IDX7                              PIC 9(05).
           03  IDX8                              PIC 9(05).
           03  IDX-ERR                           PIC 9(05).
           03  IDX-CHK                           PIC 9(05).
           03  IDX-LINE                          PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-XIDMSG          PIC X(70). 
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
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *    最終行
           03  WRK-MAX                 PIC 9(04).
      *
           03  WRK-NAME                PIC X(200).
           03  WRK-INPUTCD             PIC X(20).
           03  WRK-SRYCD               PIC X(20).
           03  WRK-TEKCD               PIC X(20).
           03  WRK-MOJI                PIC 9(03).
           03  WRK-CDSYUBETU           PIC X(01).
           03  WRK-BYOMEICD            PIC X(07).
           03  WRK-BYOMEI              PIC X(200).
           03  WRK-TNS-NAME            PIC X(70).
           03  WRK-CURSOR              PIC 9(03).
           03  WRK-PAGE-MAX            PIC 9(03).
           03  WRK-PAGE-DSP-G.
               05  WRK-PAGE-DSP-ACTIVE PIC Z9.
               05  WRK-PAGE-DSP-LBL    PIC X(01).
               05  WRK-PAGE-DSP-MAX    PIC Z9.
           03  WRK-STIDX               PIC 9(03).
           03  WRK-EDIDX               PIC 9(03).
           03  WRK-GMN-PAGE            PIC 9(03).
           03  WRK-ERR-NAME            PIC X(100).
           03  WRK-TNS-SRYCD           PIC X(09).
           03  WRK-LINE-MIN            PIC 9(05).
           03  WRK-LINE-MAX            PIC 9(05).
           03  WRK-BASEPOS             PIC 9(05).
           03  WRK-TEKCD-WIDGET.
               05  WRK-TEKCDMEI        PIC X(05).
               05  WRK-TEKCDNUM        PIC 9(02).
           03  WRK-NGBNDKBN-WIDGET.
               05  WRK-NGBNDKBNMEI     PIC X(08).
               05  WRK-NGBNDKBNNUM     PIC 9(02).
           03  WRK-CHKERR-WIDGET.
               05  WRK-CHKERRMEI       PIC X(06).
               05  WRK-CHKERRNUM       PIC 9(02).
           03  WRK-GMN-TBL-MAX         PIC 9(05).
           03  WRK-STYLE               PIC X(20).
      *
       01  TBL-AREA.
           03  TBL-GMN-AREA.
               05  TBL-GMN-MAX         PIC 9(05).
               05  TBL-GMN-OCC    OCCURS  400.
                   07  TBL-GMN-TTEKCD     PIC X(20).
                   07  TBL-GMN-TNAME      PIC X(100).
                   07  TBL-GMN-TSYORIKBN  PIC X(01).
                   07  TBL-GMN-TNGBNDKBN  PIC X(01).
                   07  TBL-GMN-TCHKERR    PIC 9(01).
                   07  TBL-GMN-TDATAKBN   PIC X(01).
                   07  TBL-GMN-TCHKERR2   PIC S9(01).
                   07  TBL-MAE-TTEKCD     PIC X(20).
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
       01  CONST-AREA.
           03  CONST-TEKCD-MAX     PIC 9(05)   VALUE   400.
           03  CONST-LINE-MAX      PIC 9(03)   VALUE   20. 
           03  CONST-PAGE-MAX      PIC 9(02)   VALUE   20.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *    自院マスター
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
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
      *    一覧印刷変換領域
           COPY    "CPORCHXLST.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
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
           COPY    "X01.INC".
           COPY    "X02.INC".
           COPY    "X03.INC".
           COPY    "X04.INC".
           COPY    "X05.INC".
           COPY    "X06.INC".
           COPY    "X07.INC".
           COPY    "X08.INC".
           COPY    "X91.INC".
           COPY    "X97.INC".
           COPY    "X98.INC".
           COPY    "X99.INC".
           COPY    "X100.INC".
           COPY    "XERR.INC".
           COPY    "XID1.INC".
           COPY    "XID2.INC".
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
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X02 
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
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X02         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE    
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
           IF      SPA-MOTOPG          =   "XERR"
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
      **** MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "X02    "           TO  MCP-WINDOW.
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "X98"
      *        病名セット処理
                   PERFORM 320-X98SPASET-SEC
               WHEN    "XID1"
                   PERFORM 330-XID1-SET-SEC
               WHEN    "X03"
                   PERFORM 340-X03-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
                   MOVE    1               TO  SPA-GMN-PAGE
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
      *TEST
      *
           INITIALIZE              SPA-X02-AREA
      *
           MOVE    SPA-X01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-X01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-X01-CHKKBN-G    TO  SPA-GMN-CHKKBN-G
           MOVE    SPA-X01-SRYCD       TO  SPA-GMN-SRYCD
           MOVE    SPA-X01-NAME        TO  SPA-GMN-NAME
      *
      *    チェックマスタ読み込み 
           PERFORM 900-CHKMST-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( SPA-GMN-TBL-MAX >=  CONST-TEKCD-MAX  )
                    OR   ( FLG-CHKMST      =   1 )
      *
               COMPUTE SPA-GMN-TBL-MAX
                   =   SPA-GMN-TBL-MAX     +   1
      *
               MOVE    CHK-CD          TO  SPA-GMN-TTEKCD
                                                  (SPA-GMN-TBL-MAX)
               MOVE    CHK-SYORIKBN    TO  SPA-GMN-TSYORIKBN
                                                  (SPA-GMN-TBL-MAX)
               MOVE    CHK-NYUGAIBNDKBN
                                       TO  SPA-GMN-TNGBNDKBN
                                                  (SPA-GMN-TBL-MAX)
               MOVE    CHK-CHKERR      TO  SPA-GMN-TCHKERR
                                                  (SPA-GMN-TBL-MAX)
               MOVE    CHK-DATAKBN     TO  SPA-GMN-TDATAKBN
                                                  (SPA-GMN-TBL-MAX)
      *
               MOVE    SPA-GMN-TTEKCD (SPA-GMN-TBL-MAX)
                                       TO  SPA-GMN-TTEKCD-BF
                                                  (SPA-GMN-TBL-MAX)
      *
      *        点数マスタ読み込み
               PERFORM 3101-TENNAME-SEC
      *
               PERFORM 900-CHKMST-KEY2-FET-SEC
      *
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 3101-CHKERR2-SET-SEC
      *
           COMPUTE SPA-GMN-PAGE    =   (SPA-GMN-TBL-MAX - 1)
                                   /   CONST-LINE-MAX  +   1
           MOVE    1               TO  SPA-GMN-CUR 
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       3101-TENNAME-SEC          SECTION.
      *
           MOVE    CHK-CD              TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY4-SEL-SEC
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-NAME
           PERFORM
               UNTIL     FLG-TENSU     =   1
      *
               MOVE    SPACE           TO  WRK-TNS-NAME
               IF    ( TNS-SRYCD(1:1)      =   "1") AND
                     ( TNS-ROUTEKKBN       =   2  )
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    WRK-TNS-NAME
                   END-STRING
               ELSE
                   MOVE    TNS-NAME    TO  WRK-TNS-NAME
               END-IF
      *
      *        有効終了日が対象のマスタを対象とする
      *********IF      (SPA-X01-STYUKYMD   >=  TNS-YUKOSTYMD )  AND
               IF      (SPA-X01-KJNYMD     <=  TNS-YUKOEDYMD )
                   MOVE    WRK-TNS-NAME    TO  SPA-GMN-TNAME
                                                      (SPA-GMN-TBL-MAX)
                   MOVE    1               TO  FLG-TENSU
                   MOVE    1               TO  FLG-OK
               ELSE
                   MOVE    WRK-TNS-NAME    TO  WRK-NAME
               END-IF
      *
               PERFORM 900-TENSU-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF     (FLG-OK              =   ZERO ) AND
                  (WRK-NAME        NOT =   SPACE)
               MOVE    SPACE               TO  SPA-GMN-TNAME
                                                      (SPA-GMN-TBL-MAX)
               STRING      "★削除済★"    DELIMITED   BY  SIZE
                           WRK-NAME        DELIMITED   BY  SIZE
                                           INTO    SPA-GMN-TNAME
                                                      (SPA-GMN-TBL-MAX)
               END-STRING
           END-IF
      *
           .
       3101-TENNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    逆設定のエラー区分設定処理
      *****************************************************************
       3101-CHKERR2-SET-SEC            SECTION.
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   SPA-GMN-TBL-MAX   )
      *
               MOVE    SPA-GMN-TTEKCD (IDX-LINE)
                                       TO  WRK-SRYCD
               PERFORM 900-CHKMST-KEY21-SEL-SEC
               IF    ( FLG-CHKMST      =   ZERO )
                   MOVE    CHK-CHKERR  TO  SPA-GMN-TCHKERR2
                                                  (IDX-LINE)
               ELSE
                   MOVE    -1          TO  SPA-GMN-TCHKERR2
                                                  (IDX-LINE)
               END-IF
           END-PERFORM
      *
           .
       3101-CHKERR2-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-X98SPASET-SEC               SECTION.
      *
      *
           MOVE    ZERO            TO  CNT-SRYCD
           INITIALIZE                  TBL-AREA
      *
           IF    ( SPA-X98-SRYCD (1)   =   SPACE )
               GO  TO  320-X98SPASET-EXT
           END-IF
      *
           IF    ( SPA-X98-SYORIKBN    =   "1" )
               MOVE    SPA-X98-CHK-IDX TO  IDX7
           ELSE
               COMPUTE IDX7    =       SPA-GMN-TBL-MAX +   1
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   5 )
      *
               IF    ( SPA-X98-SRYCD (IDX0)    NOT =   SPACE )
                   IF    ( IDX7    <=  CONST-TEKCD-MAX )
                       MOVE    SPA-X98-SRYCD (IDX0)
                                       TO  TBL-GMN-TTEKCD (IDX7)
                       COMPUTE IDX7    =   IDX7    +   1
                       COMPUTE CNT-SRYCD   =   CNT-SRYCD   +   1
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  IDX0
                                       SPA-GMN-TBL-MAX
      *
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7    >   CONST-TEKCD-MAX )
      *
               IF    ( TBL-GMN-TTEKCD (IDX7)   =   SPACE )
      *
                   COMPUTE IDX0    =   IDX0    +   1
      *
                   IF    ( SPA-X98-SYORIKBN    =   "1" )
                    AND  ( SPA-X98-CHK-IDX     =   IDX0 )
                       COMPUTE IDX0    =   IDX0    +   1
                   END-IF
      *
                   IF    ( IDX0    <=  CONST-TEKCD-MAX )
                       IF    ( SPA-GMN-TTEKCD (IDX0)
                                                   NOT =   SPACE )
                           MOVE    SPA-GMN-TBL-OCC (IDX0)
                                   TO  TBL-GMN-OCC (IDX7)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           MOVE    TBL-GMN-AREA        TO  SPA-GMN-TBL
      *
           PERFORM 41011-KUHAKU-SYORI-SEC
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   SPA-GMN-TBL-MAX   )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
      *            適用コード
                   PERFORM 4101-TEKCD-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
                   PERFORM 4101-NGBNDKBN-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
                   PERFORM 4101-CHKERR-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   SPA-GMN-TBL-MAX   )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
      *            同一コードチェック
                   PERFORM 41014-JYUFUKU-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  320-X98SPASET-EXT
           END-IF
      *
           IF    ( SPA-X98-SYORIKBN    =   "1" )
               COMPUTE WRK-CURSOR  =   SPA-X98-CHK-IDX +   CNT-SRYCD
           ELSE
               COMPUTE WRK-CURSOR  =   SPA-GMN-TBL-MAX +   1
           END-IF
      *
           COMPUTE WRK-GMN-PAGE
               = ( WRK-CURSOR      -   1 )
               /   CONST-LINE-MAX   +   1
      *
           COMPUTE SPA-GMN-CUR
               =   WRK-CURSOR
               - ( SPA-GMN-PAGE    -   1 )
               *   CONST-LINE-MAX
      *
           IF    ( WRK-GMN-PAGE    >   SPA-GMN-PAGE )
               IF    ( SPA-GMN-PAGE    =   CONST-PAGE-MAX )
                   MOVE    212             TO  SPA-GMN-CUR
               ELSE
                   MOVE    207             TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       320-X98SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-XID1-SET-SEC            SECTION.
      *
           IF      SPA-XID1-FLG        =   "OK"
               EVALUATE    SPA-XIDCD
               WHEN    "1001"
                   PERFORM 490-KOUSHIN-SEC
               WHEN    "1002"
                   PERFORM 4031-DEL-SEC
               WHEN    "2001"
                   PERFORM 4111-PRINT-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-XID1-FLG
           MOVE    SPACE           TO  SPA-XIDCD
           .
       330-XID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック項目複写画面戻り時処理
      *****************************************************************
       340-X03-SET-SEC             SECTION.
      *
           IF    ( SPA-X03-GMN-KAKUTEI     NOT =   1 )
               GO  TO  340-X03-SET-EXT
           END-IF
      *
           INITIALIZE              SPA-GMN-TBL
      *
           IF    ( SPA-X03-GMN-LCHKLST-MAX >   CONST-TEKCD-MAX )
               MOVE    CONST-TEKCD-MAX    TO  SPA-GMN-TBL-MAX
           ELSE
               MOVE    SPA-X03-GMN-LCHKLST-MAX
                                       TO  SPA-GMN-TBL-MAX
           END-IF
      *
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   SPA-GMN-TBL-MAX )
      *
               MOVE    SPA-X03-NAI-TLCHK-SRYCD (IDX5)
                                   TO  SPA-GMN-TTEKCD(IDX5)
                                       SPA-GMN-TTEKCD-BF(IDX5)
               MOVE    SPA-X03-GMN-TLCHK-NAME (IDX5)
                                   TO  SPA-GMN-TNAME (IDX5)
               MOVE    SPA-X03-NAI-TLCHK-SYORIKBN  (IDX5)
                                   TO  SPA-GMN-TSYORIKBN (IDX5)
               MOVE    SPA-X03-NAI-TLCHK-NGBNDKBN(IDX5)
                                   TO  SPA-GMN-TNGBNDKBN (IDX5)
               MOVE    SPA-X03-NAI-TLCHK-CHKERR  (IDX5)
                                   TO  SPA-GMN-TCHKERR (IDX5)
               MOVE    SPA-X03-NAI-TLCHK-DATAKBN (IDX5)
                                   TO  SPA-GMN-TDATAKBN(IDX5)
      *
           END-PERFORM
      *
           PERFORM 3101-CHKERR2-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       340-X03-SET-EXT.
           EXIT.
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
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *    前画面
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次画面
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *        チェック項目複写
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM  409-X03-SENI-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM  409-KENSAKU-CHK-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  410-KAKUTEI-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM  411-PRINT-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
      *
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   CONST-LINE-MAX
               COMPUTE IDZ
                   =   IDX
                   +  (SPA-GMN-PAGE - 1 )  *   CONST-LINE-MAX
      *
               IF    ( SPA-GMN-TDATAKBN (IDZ)  =   "1" )
                   CONTINUE
               ELSE
                   MOVE    X02-TEKCD (IDX)     TO  SPA-GMN-TTEKCD
                                                                 (IDZ)
               END-IF
      *
               IF    ( X02-NGBNDKBN (IDX)      =   ZERO )
                  MOVE    SPACE                TO  SPA-GMN-TNGBNDKBN
                                                                 (IDZ)
               ELSE
                  MOVE    X02-NGBNDKBN (IDX)   TO  SPA-GMN-TNGBNDKBN
                                                                 (IDZ)
               END-IF
      *
               MOVE    X02-CHKERR   (IDX)      TO  SPA-GMN-TCHKERR
                                                                 (IDZ)
      *
               MOVE    SPA-GMN-TTEKCD (IDZ)    TO  WRK-TEKCD
               INSPECT WRK-TEKCD       REPLACING   ALL "-"  BY  " "
               IF    ( SPA-GMN-TTEKCD (IDZ)    NOT =   WRK-TEKCD )
                   MOVE    SPACE               TO  SPA-GMN-TTEKCD (IDZ)
               END-IF
      *
      *        IF    ( SPA-GMN-TCHKERR (IDZ)   =   8 )
      *            IF    ( SPA-GMN-TDATAKBN (IDZ)  =   "1" )
      *                CONTINUE
      *            ELSE
      *                MOVE    SPACE           TO  SPA-GMN-TTEKCD (IDZ)
      *            END-IF
      *        END-IF
      *
      *        全角入力の時、１０文字まで
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                     TO  KANACHK-SYORI
               MOVE    SPA-GMN-TTEKCD (IDZ)    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT       TO  SPA-GMN-TTEKCD
                                                                 (IDZ)
      *
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
      *    適用コード欄空行削除
           PERFORM 41011-KUHAKU-SYORI-SEC
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   SPA-GMN-TBL-MAX   )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
      *            適用コード
                   PERFORM 4101-TEKCD-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
                   PERFORM 4101-NGBNDKBN-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
                   PERFORM 4101-CHKERR-CHK-SEC
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   SPA-GMN-TBL-MAX - 1  )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
                    OR   ( FLG-END     NOT =   ZERO  )
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *
      *            同一コードチェック
                   PERFORM 41014-JYUFUKU-CHK-SEC
               END-IF
      *
           END-PERFORM
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
           MOVE    "X01"               TO  SPA-SAKIPG
           MOVE    "X02"               TO  SPA-MOTOPG
     *
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
      *    診療行為コードチェック
      *****************************************************************
       4101-TEKCD-CHK-SEC                         SECTION.
      *
           MOVE    ZERO            TO  IDX-ERR
      *
           IF    ( SPA-GMN-TTEKCD(IDX-LINE)(1:1)
                                               =   "/" )
      *
               COMPUTE SPA-GMN-CUR =   IDX-LINE
                                   -  ((SPA-GMN-PAGE  -  1)
                                   *   CONST-LINE-MAX )
      *
               MOVE    SPACE       TO  WRK-INPUTCD
               MOVE    ZERO        TO  WRK-MOJI
               MOVE    IDX-LINE    TO  IDX-CHK
               PERFORM 409-KENSAKU-SEC
           ELSE
               IF    ( SPA-GMN-TTEKCD(IDX-LINE)
                                   NOT =  SPA-GMN-TTEKCD-BF (IDX-LINE))
                   PERFORM 41013-TENSU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-TEKCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    空白削除処理
      *****************************************************************
       41011-KUHAKU-SYORI-SEC             SECTION.
      *
           INITIALIZE                      TBL-AREA
      *
           MOVE    SPA-GMN-TBL         TO  TBL-GMN-AREA
      *
           INITIALIZE                      SPA-GMN-TBL
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   CONST-TEKCD-MAX )
      *
               IF    ( TBL-GMN-TTEKCD (IDX-LINE)
                                       NOT =   SPACE )
                   COMPUTE SPA-GMN-TBL-MAX
                       =   SPA-GMN-TBL-MAX     +   1
                   MOVE    TBL-GMN-OCC (IDX-LINE)
                                       TO  SPA-GMN-TBL-OCC
                                               (SPA-GMN-TBL-MAX)
               END-IF
      *
           END-PERFORM
      *
           IF    ( TBL-GMN-MAX     >   SPA-GMN-TBL-MAX )
               MOVE    1               TO  FLG-TEKCDDEL
           END-IF
      *
           .
       41011-KUHAKU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタチェック処理
      *****************************************************************
       41013-TENSU-CHK-SEC             SECTION.
      *
      *    セットコードでの入力はエラーとする。
           IF  ( SPA-GMN-TTEKCD (IDX-LINE)(1:1) =   "P"  OR  "S" )
               MOVE    "0012"          TO  SPA-ERRCD
               MOVE    IDX-LINE        TO  IDX-ERR
               GO  TO  41013-TENSU-CHK-EXT
           END-IF
      *
      *    入力コードマスタ−検索条件編集処理
           PERFORM 410131-INPUTCD-SELECT-EDIT-SEC
           IF  ( SPA-ERRCD         NOT =   SPACE )
               MOVE    IDX-LINE        TO  IDX-ERR
               GO  TO  41013-TENSU-CHK-EXT
           END-IF
      *
           IF  ( WRK-MOJI              =   ZERO )
               GO  TO  41013-TENSU-CHK-EXT
           END-IF
      *
           INITIALIZE              INPUTCD-REC
           MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
           MOVE    WRK-CDSYUBETU   TO  ICD-CDSYU
           MOVE    WRK-SRYCD       TO  ICD-INPUTCD
      *
      *    入力コードマスタ−読み込み処理
           PERFORM 900-INPUTCD-SEL-KEY2-SEC
      *
      *    入力コードマスターに存在せず、
      *    入力コードの桁数が９桁の場合、点数マスターを検索する
           IF  ( FLG-INPUTCD       =   ZERO )
               MOVE   ICD-SRYCD        TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   1 )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    IDX-LINE    TO  IDX-ERR
                   GO  TO  41013-TENSU-CHK-EXT
               END-IF
           ELSE
               IF    ( WRK-MOJI              =   9 )
                AND  ( FLG-KANJI             =   ZERO )
      *
                   MOVE   WRK-SRYCD        TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF  ( FLG-TENSU         =   1 )
                       MOVE    "0001"      TO  SPA-ERRCD
                       MOVE    IDX-LINE    TO  IDX-ERR
                       GO  TO  41013-TENSU-CHK-EXT
                   END-IF
               ELSE
                   COMPUTE SPA-GMN-CUR         =   IDX-LINE
                                               -  ((SPA-GMN-PAGE  -  1)
                                               *   CONST-LINE-MAX )
                   MOVE    WRK-SRYCD       TO  WRK-INPUTCD
                   MOVE    IDX-LINE        TO  IDX-CHK
                   PERFORM 409-KENSAKU-SEC
                   GO  TO  41013-TENSU-CHK-EXT
               END-IF
           END-IF
      *
           IF     (TNS-SRYCD(1:1)      =   "1") AND
                  (TNS-ROUTEKKBN       =   2  )
               MOVE    SPACE       TO  WRK-NAME
               STRING  "［老］"        DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  SIZE
               INTO    WRK-NAME
               END-STRING
           ELSE
               MOVE    TNS-NAME    TO  WRK-NAME
           END-IF
      *
           MOVE    TNS-SRYCD           TO  SPA-GMN-TTEKCD(IDX-LINE)
           MOVE    WRK-NAME            TO  SPA-GMN-TNAME (IDX-LINE)
      *
      *    診療行為コードのチェック
           EVALUATE    SPA-GMN-CHKKBN
           WHEN    "4"
               IF    ( SPA-GMN-TTEKCD (IDX-LINE) (1:1) NOT =   "6" )
                   MOVE    "0008"      TO  SPA-ERRCD
                   MOVE    IDX-LINE    TO  IDX-ERR
               END-IF
           WHEN    "5"
           WHEN    "A"
           WHEN    "B"
               IF    ( SPA-GMN-TTEKCD (IDX-LINE) (1:1)     =   "6" )
                   MOVE    "0008"      TO  SPA-ERRCD
                   MOVE    IDX-LINE    TO  IDX-ERR
               END-IF
           END-EVALUATE
      *
           IF    ( SPA-ERRCD           =   SPACE )
      *
               PERFORM 900-CHKMST-KEY21-SEL-SEC
               IF    ( FLG-CHKMST      =   ZERO )
                   MOVE    CHK-CHKERR  TO  SPA-GMN-TCHKERR2 (IDX-LINE)
               ELSE
                   MOVE    -1          TO  SPA-GMN-TCHKERR2 (IDX-LINE)
               END-IF
      *
               MOVE    SPA-GMN-TTEKCD (IDX-LINE)
                                       TO  SPA-GMN-TTEKCD-BF (IDX-LINE)
           END-IF
      *
           .
       41013-TENSU-CHK-EXT.
           EXIT.
           .
      *****************************************************************
      *    入力コードマスタ−検索条件編集処理
      *****************************************************************
       410131-INPUTCD-SELECT-EDIT-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  WRK-MOJI
      *
           MOVE    SPA-GMN-TTEKCD (IDX-LINE)
                                       TO  WRK-SRYCD
      *
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL  (IDX3    >   20  )   OR
                          (WRK-SRYCD (IDX3:)
                                   =   SPACE  )
      *
               COMPUTE WRK-MOJI    =   WRK-MOJI    +   1
      *
           END-PERFORM
      *
           IF  ( WRK-MOJI          =   ZERO )
               GO  TO  410131-INPUTCD-SELECT-EDIT-EXT
           END-IF
      *
      *    全角半角混在チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-SRYCD       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    "0001"      TO  SPA-ERRCD
               GO  TO  410131-INPUTCD-SELECT-EDIT-EXT
           END-IF
      *
           EVALUATE    KANACHK-SYUBETU
           WHEN    1
      *        半角入力
               MOVE    ZERO            TO  FLG-KANJI
      *
               IF  ( WRK-SRYCD (1:WRK-MOJI) IS  NUMERIC )
                   EVALUATE    WRK-MOJI
                   WHEN    4
                       MOVE    "4"     TO  WRK-CDSYUBETU
                   WHEN    5
                       MOVE    "5"     TO  WRK-CDSYUBETU
                   WHEN    6
                   WHEN    3
                        MOVE   "6"     TO  WRK-CDSYUBETU
                   WHEN    OTHER
                       MOVE    "6"     TO  WRK-CDSYUBETU
                   END-EVALUATE
               ELSE
                   MOVE    "A"         TO  WRK-CDSYUBETU
               END-IF
      *    全角入力
           WHEN    2
               MOVE    1               TO  FLG-KANJI
               MOVE    "J"             TO  WRK-CDSYUBETU
           WHEN    OTHER
               MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
      *
       410131-INPUTCD-SELECT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    同一コードチェック処理
      *****************************************************************
       41014-JYUFUKU-CHK-SEC            SECTION.
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-PAGE    *   CONST-LINE-MAX
      *
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           COMPUTE WRK-STIDX   =   IDX-LINE    +   1
      *
           PERFORM VARYING     IDX6    FROM    WRK-STIDX   BY  1
                   UNTIL     ( IDX6            >   SPA-GMN-TBL-MAX  )
                    OR       ( SPA-ERRCD   NOT =   SPACE)
      *
               IF    ( SPA-GMN-TTEKCD (IDX-LINE)
                                           =   SPA-GMN-TTEKCD (IDX6))
      *
                   MOVE    SPA-GMN-TNAME (IDX-LINE)
                                           TO  WRK-ERR-NAME
                   MOVE    "0005"          TO  SPA-ERRCD
      *
                   IF    ( IDX-LINE        >=  WRK-LINE-MIN )
                    AND  ( IDX-LINE        <=  WRK-LINE-MAX )
                    AND  ( SPA-GMN-TDATAKBN (IDX-LINE) NOT =   "1" )
                       MOVE    IDX-LINE    TO  IDX-ERR
                   ELSE
                       MOVE    IDX6        TO  IDX-ERR
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       41014-JYUFUKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    同一入外制限区分チェック処理
      *****************************************************************
       4101-NGBNDKBN-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-TNGBNDKBN (IDX-LINE)
                                       =   SPACE OR "1" )
               CONTINUE
           ELSE
               MOVE    "0013"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    20              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-NGBNDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    エラー区分チェック処理
      *****************************************************************
       4101-CHKERR-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-TCHKERR (IDX-LINE)
                                       =   ZERO OR 8 OR 9 )
               CONTINUE
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    40              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-CHKERR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    適用欄エラー時カーソル位置決定処理
      *****************************************************************
       4101-ERRCUR-POS-SEC             SECTION.
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-PAGE    *   CONST-LINE-MAX
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           EVALUATE    TRUE
           WHEN  ( IDX-ERR     <   WRK-LINE-MIN )
      *
               MOVE    206     TO  SPA-GMN-CUR
      *
           WHEN  ( IDX-ERR     >   WRK-LINE-MAX )
      *
               MOVE    207     TO  SPA-GMN-CUR
      *
           WHEN    OTHER
      *
               COMPUTE SPA-GMN-CUR
                   =   IDX-ERR
                   -   WRK-LINE-MIN +   1
                   +   WRK-BASEPOS
      *
           END-EVALUATE
      *
           .
       4101-ERRCUR-POS-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF      SPA-GMN-PAGE       =   1
               MOVE    "0006"             TO  SPA-ERRCD
               GO  TO  430-MAEGMN-EXT
           END-IF
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
           MOVE    1                  TO  SPA-GMN-CUR
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF      SPA-GMN-PAGE       =   CONST-PAGE-MAX
               MOVE    "0007"          TO  SPA-ERRCD
               GO  TO  440-ATOGMN-EXT
           ELSE
               COMPUTE IDX     =   SPA-GMN-PAGE   *   CONST-LINE-MAX
               IF      SPA-GMN-TTEKCD (IDX)   =   SPACE
                   MOVE    "0007"      TO  SPA-ERRCD
                   GO  TO  440-ATOGMN-EXT
               END-IF
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    +   1
      *
           COMPUTE IDX
               = ( SPA-GMN-PAGE    -   1 )
               *   CONST-LINE-MAX
               +   1
      *
           PERFORM VARYING IDX8    FROM    1   BY  1
                   UNTIL ( IDX8           >    CONST-LINE-MAX )
                    OR   ( SPA-GMN-TTEKCD (IDX)    =   SPACE )
      *
               COMPUTE IDX =   IDX +   1
      *
           END-PERFORM
      *
           IF    ( IDX8           >    CONST-LINE-MAX )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    IDX8            TO  SPA-GMN-CUR
           END-IF
      *
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-XIDCD
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
           PERFORM 40311-DEL-SEC
           IF    ( SPA-ERRCD    =  SPACE )
               MOVE   "JOIN"   TO   MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      * 
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       40311-DEL-SEC                   SECTION.
      *
      *    チェックマスタ削除
           INITIALIZE                  CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-X01-SEL-STYUKYMD TO CHK-YUKOSTYMD
           MOVE    SPA-X01-SEL-EDYUKYMD TO CHK-YUKOEDYMD
           MOVE    CHK-REC             TO  MCPDATA-REC
     ******MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO  )
               MOVE    "1002"          TO  SPA-ERRCD
           END-IF
      *
           .
       40311-DEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目複写処理
      *****************************************************************
       409-X03-SENI-SEC                SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
      *
      *        チェック項目複写画面初期処理
               PERFORM 4091-X03-INIT-SEC
      *
               MOVE    "X03"               TO  SPA-SAKIPG
               MOVE    "X02"               TO  SPA-MOTOPG
      *
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       409-X03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目複写画面初期処理
      *****************************************************************
       4091-X03-INIT-SEC           SECTION.
      *
      *    画面の初期化と左コラムリストの設定を行う
           INITIALIZE                  SPA-X03-GMN-AREA
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  SPA-X03-GMN-LCHKKBNMEI
           MOVE    SPA-X01-STYUKYMDH   TO  SPA-X03-GMN-LYUKOSTYMD
           MOVE    SPA-X01-EDYUKYMDH   TO  SPA-X03-GMN-LYUKOEDYMD
           MOVE    SPA-X01-NAME        TO  SPA-X03-GMN-LNAME
      *
           MOVE    SPA-GMN-TBL-MAX         TO  SPA-X03-GMN-LCHKLST-MAX
      *
           PERFORM VARYING IDX4    FROM    1   BY  1
                   UNTIL ( IDX4    >   SPA-X03-GMN-LCHKLST-MAX )
      *
               MOVE    IDX4            TO  SPA-X03-GMN-TLCHK-NUM  (IDX4)
               MOVE    SPA-GMN-TNAME  (IDX4)
                                       TO  SPA-X03-GMN-TLCHK-NAME (IDX4)
      *
               MOVE    SPA-GMN-TTEKCD(IDX4)
                                       TO  SPA-X03-NAI-TLCHK-SRYCD(IDX4)
      *
               MOVE    SPA-GMN-TSYORIKBN (IDX4)
                                       TO  SPA-X03-NAI-TLCHK-SYORIKBN
                                                                  (IDX4)
      *
               MOVE    SPA-GMN-TNGBNDKBN (IDX4)
                                       TO  SPA-X03-NAI-TLCHK-NGBNDKBN
                                                                  (IDX4)
               MOVE    SPA-GMN-TCHKERR (IDX4)
                                       TO  SPA-X03-NAI-TLCHK-CHKERR
                                                                  (IDX4)
               MOVE    SPA-GMN-TDATAKBN(IDX4)
                                       TO  SPA-X03-NAI-TLCHK-DATAKBN
                                                                  (IDX4)
      *
           END-PERFORM
      *
           .
       4091-X03-INIT-EXT.
           EXIT.
      *****************************************************************
      *    検索ボタン押下時チェック処理
      *****************************************************************
       409-KENSAKU-CHK-SEC         SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO      409-KENSAKU-CHK-EXT
           END-IF
      *
           IF    ( SPA-GMN-TBL-MAX     >=  CONST-TEKCD-MAX )
               MOVE    "0011"      TO  SPA-ERRCD
               GO  TO      409-KENSAKU-CHK-EXT
           END-IF
      *
           MOVE    1               TO  FLG-CD-TUIKA
      *
           PERFORM 409-KENSAKU-SEC
      *
           .
       409-KENSAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-X98-AREA
           MOVE    "2"                 TO  SPA-X98-TYPE
      *
           MOVE    IDX-CHK             TO  SPA-X98-CHK-IDX
      *
           INITIALIZE                  SPA-X98GMN-AREA
      *
           IF    ( FLG-CD-TUIKA        =   ZERO )
               MOVE    "1"             TO  SPA-X98-SYORIKBN
           ELSE
               MOVE    "2"             TO  SPA-X98-SYORIKBN
           END-IF
      *
           IF    ( WRK-INPUTCD     NOT =   SPACE )
      *
               EVALUATE    SPA-X01-CHKKBN
               WHEN    "4"
                   MOVE    "2"             TO  SPA-X98-GMN-KBN
                   MOVE    "3"             TO  SPA-X98-GMN-SYORI
               WHEN    "5"
               WHEN    "A"
               WHEN    "B"
                   MOVE    "2"             TO  SPA-X98-GMN-KBN
                   MOVE    "8"             TO  SPA-X98-GMN-SYORI
               END-EVALUATE
      *
               MOVE    WRK-INPUTCD         TO  SPA-X98-INPUTCD
               IF    ( SPA-X98-GMN-KBN     =   "2" )
                   MOVE    "1"             TO  SPA-X98-TYPE
               ELSE
                   MOVE    SPACE           TO  SPA-X98-TYPE
               END-IF
      *
               IF    ( FLG-KANJI           =   1 )
                   MOVE    "J"             TO  SPA-X98-CDSYUBETU
               ELSE
                   IF    ( WRK-INPUTCD(1:1)    NUMERIC )
                       EVALUATE    WRK-MOJI
                       WHEN    3
                       WHEN    6
                           MOVE    "6"     TO  SPA-X98-CDSYUBETU
                       WHEN    4
                           MOVE    "4"     TO  SPA-X98-CDSYUBETU
                       WHEN    5
                           MOVE    "5"     TO  SPA-X98-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-X98-CDSYUBETU
                   END-IF
               END-IF
           END-IF
      *
      *    画面編集
           PERFORM 41011-X98-SET-SEC
      *
           MOVE    "X02"               TO  SPA-MOTOPG
           MOVE    "X98"               TO  SPA-SAKIPG
           MOVE    "X02"               TO  SPA-X98-MOTOPG
      *
           IF    ( SPA-X98-GMN-MAX     =   ZERO )
               MOVE    "INPUT"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM1"       TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "X98    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-X98-SET-SEC                           SECTION.
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X02         TO  SPA-X01SONOTA
           MOVE    SPA-X01FREE     TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGX98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-X01KYOUTU 
           MOVE    SPA-FREE        TO  SPA-X01FREE
           MOVE    SPA-X01SONOTA   TO  SPA-X02
      *
           .
       41011-X98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       411-PRINT-SEC                   SECTION.
      *
          PERFORM 410-KAKUTEI-SEC
          IF    ( SPA-ERRCD        =   SPACE )
              MOVE    "2001"       TO  SPA-XIDCD
          END-IF
      *
           .
       411-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4111-PRINT-SEC                  SECTION.
      *
           PERFORM 4901-KOUSHIN-SEC
           IF    ( SPA-ERRCD      =   SPACE )
      *
               IF    ( SPA-X01-SELNUM      =   ZERO )
                   MOVE    99              TO  SPA-X01-SELNUM
                   MOVE    SPA-X01-STYUKYMD
                                           TO  SPA-X01-SEL-STYUKYMD
                   MOVE    SPA-X01-EDYUKYMD
                                           TO  SPA-X01-SEL-EDYUKYMD
               END-IF
      *
               INITIALIZE                  ORCHXLSTAREA
               MOVE    SPA-GMN-CHKKBN      TO  HXLST-CHKKBN
               MOVE    SPA-GMN-SRYCD       TO  HXLST-SRYCD
               MOVE    "1"                 TO  HXLST-PROCKBN
               MOVE    SPA-X01-KJNYMD      TO  HXLST-KJNYMD
               CALL    "ORCHXLST"      USING   SPA-AREA
                                               ORCHXLSTAREA
           END-IF
      *
           .
       4111-PRINT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC           SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-KAKUTEI-EXT
           END-IF
      *
           PERFORM 4901-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-XIDCD
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
           PERFORM 4901-KOUSHIN-SEC
           IF    ( SPA-ERRCD      =   SPACE )
               MOVE   "JOIN"     TO   MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       4901-KOUSHIN-SEC            SECTION.
      *
      *    チェックマスタ削除 
           PERFORM 40311-DEL-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  4901-KOUSHIN-EXT
           END-IF
      *
      *    チェックマスタ追加
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-X01-STYUKYMD    TO  CHK-YUKOSTYMD
           MOVE    SPA-X01-EDYUKYMD    TO  CHK-YUKOEDYMD
           MOVE    "2"                 TO  CHK-CDKBN
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  CHK-CREYMD
                                           CHK-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHK-UPHMS
      *
           PERFORM UNTIL ( CHK-RENNUM  >=  SPA-GMN-TBL-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               COMPUTE CHK-RENNUM  =   CHK-RENNUM  +   1
      *
               MOVE    SPA-GMN-TTEKCD (CHK-RENNUM)
                                       TO  CHK-CD
      *
               MOVE    SPA-GMN-TSYORIKBN (CHK-RENNUM)
                                       TO  CHK-SYORIKBN
               MOVE    SPACE           TO  CHK-NYUGAIKBN
               MOVE    SPA-GMN-TNGBNDKBN (CHK-RENNUM)
                                       TO CHK-NYUGAIBNDKBN
               MOVE    SPA-GMN-TCHKERR   (CHK-RENNUM)
                                       TO CHK-CHKERR
               IF    ( SPA-GMN-TDATAKBN  (CHK-RENNUM)  =   SPACE )
                   MOVE    "2"         TO CHK-DATAKBN
               ELSE
                   MOVE    SPA-GMN-TDATAKBN  (CHK-RENNUM)
                                       TO CHK-DATAKBN
               END-IF
      *
               MOVE    CHK-REC         TO  MCPDATA-REC
      ******   MOVE    "tbl_chk"       TO  MCP-TABLE
               MOVE    "tbl_chk_user"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "1001"      TO  SPA-ERRCD
               END-IF
      *
           END-PERFORM
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-TBL-MAX =   ZERO )
               MOVE    "0004"      TO  SPA-ERRCD
           END-IF
      *
           .
       4901-KANRENCHK-EXT.
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
           IF      SPA-XIDCD       NOT =   SPACE
               PERFORM 520-XIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X02    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  X02
           INITIALIZE                      X02
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-PAGE    *   CONST-LINE-MAX
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  WRK-MAX
           MOVE    ZERO                TO  FLG-MSTDATDISP
      *
           PERFORM VARYING IDX1     FROM    WRK-LINE-MIN   BY  1
                   UNTIL ( IDX1     >       WRK-LINE-MAX )
      *
               COMPUTE IDX2    =    IDX2    +   1
      *
               MOVE    IDX1                    TO  X02-NO      (IDX2)
               MOVE    SPA-GMN-TTEKCD  (IDX1)  TO  X02-TEKCD   (IDX2)
               MOVE    SPA-GMN-TNAME   (IDX1)  TO  X02-DSPNAME (IDX2)
               IF    ( SPA-GMN-TNGBNDKBN(IDX1) =   SPACE )
                   MOVE    ZERO                TO  X02-NGBNDKBN(IDX2)
               ELSE
                   MOVE    SPA-GMN-TNGBNDKBN(IDX1)
                                               TO  X02-NGBNDKBN(IDX2)
               END-IF
               MOVE    SPA-GMN-TCHKERR  (IDX1) TO  X02-CHKERR  (IDX2)
      *
               IF    ( SPA-GMN-TTEKCD  (IDX1)  NOT =   SPACE )
                   MOVE    IDX2                TO  WRK-MAX
               END-IF
      *
               IF    ( SPA-X01-CHKKBN          =   "5" OR "A" OR "B" )
                   EVALUATE    SPA-GMN-TCHKERR2 (IDX1)
                   WHEN    -1
                       MOVE    "-"         TO  X02-LBLCHKERR(IDX2)
                   WHEN    SPA-GMN-TCHKERR (IDX1)
                       MOVE    SPACE       TO  X02-LBLCHKERR(IDX2)
                   WHEN    OTHER
                       MOVE    SPA-GMN-TCHKERR2 (IDX1)
                                           TO  X02-LBLCHKERR(IDX2)
                   END-EVALUATE
               END-IF
      *
               MOVE    "black"             TO  WRK-STYLE
               IF    ( SPA-GMN-TDATAKBN  (IDX1)    =   "1" )
                   MOVE    "blue"          TO  WRK-STYLE
                   MOVE    1               TO  FLG-MSTDATDISP
               END-IF
      *
               MOVE    WRK-STYLE           TO  X02-NO-STYLE      (IDX2)
                                               X02-TEKCD-STYLE   (IDX2)
                                               X02-DSPNAME-STYLE (IDX2)
                                               X02-NGBNDKBN-STYLE(IDX2)
                                               X02-CHKERR-STYLE  (IDX2)
                                               X02-LBLCHKERR-STYLE
                                                                 (IDX2)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  X02-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  X02-SRYCD
           MOVE    SPA-GMN-NAME        TO  X02-NAME
           MOVE    SPA-GMN-STYUKYMD    TO  X02-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  X02-EDYUKYMD
      *
           IF    ( SPA-GMN-TBL-MAX     <=  CONST-LINE-MAX )
               MOVE    1               TO  WRK-PAGE-MAX
           ELSE
               COMPUTE WRK-PAGE-MAX
                   = ( SPA-GMN-TBL-MAX -   1 )
                   /   CONST-LINE-MAX
                   +   1
           END-IF
      *
           MOVE    "black"             TO  X02-LBLCAUTION-STYLE
           IF    ( FLG-MSTDATDISP      =   1 )
               MOVE    "blue"          TO  X02-LBLCAUTION-STYLE
               STRING  "※マスタ提供適用コードは削除できません。"
                       DELIMITED   BY  SIZE
                       "入外、エラー区分の変更は可能です。"
                       DELIMITED   BY  SIZE
               INTO    X02-LBLCAUTION
               END-STRING
           END-IF
      *
           INITIALIZE                  WRK-PAGE-DSP-G
           MOVE    SPA-GMN-PAGE        TO  WRK-PAGE-DSP-ACTIVE
           MOVE    "/"                 TO  WRK-PAGE-DSP-LBL
           MOVE    WRK-PAGE-MAX        TO  WRK-PAGE-DSP-MAX
           MOVE    WRK-PAGE-DSP-G      TO  X02-LBLPAGE
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
           MOVE    WIDGET-NORMAL           TO  X02-B03-STATE
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LINE-MAX )
      *
               MOVE    WIDGET-NORMAL       TO  X02-NGBNDKBN-STATE (IDX0)
                                               X02-CHKERR-STATE   (IDX0)
      *
           END-PERFORM
      *
           IF    ( SPA-X01-CHKKBN          =   "4" )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-LINE-MAX )
      *
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X02-NGBNDKBN-STATE (IDX0)
                                               X02-CHKERR-STATE   (IDX0)
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-X01-SELNUM          =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  X02-B03-STATE
           END-IF 
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-TBL-MAX )
                    OR   ( X02-B03-STATE   =   WIDGET-INSENSITIVE )
      *
               IF    ( SPA-GMN-TDATAKBN  (IDX1)    =   "1" )
                AND  ( SPA-X01-ID                  =   ZERO )
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X02-B03-STATE
               END-IF
      *
           END-PERFORM
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
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN   1    THRU        20
               MOVE    SPACE       TO  WRK-TEKCD-WIDGET
               MOVE    "TEKCD"     TO  WRK-TEKCDMEI
               MOVE    SPA-GMN-CUR TO  WRK-TEKCDNUM
               MOVE    WRK-TEKCD-WIDGET
                                   TO  MCP-WIDGET
           WHEN   21   THRU        40
               MOVE    SPACE       TO  WRK-NGBNDKBN-WIDGET
               MOVE    "NGBNDKBN"  TO  WRK-NGBNDKBNMEI
               COMPUTE WRK-NGBNDKBNNUM
                   =   SPA-GMN-CUR -   20
               MOVE    WRK-NGBNDKBN-WIDGET
                                   TO  MCP-WIDGET
           WHEN   41   THRU        60
               MOVE    SPACE       TO  WRK-CHKERR-WIDGET
               MOVE    "CHKERR"    TO  WRK-CHKERRMEI
               COMPUTE WRK-CHKERRNUM
                   =   SPA-GMN-CUR -   40
               MOVE    WRK-CHKERR-WIDGET
                                   TO  MCP-WIDGET
           WHEN   206
               MOVE  "B06"         TO  MCP-WIDGET
           WHEN   207
               MOVE  "B07"         TO  MCP-WIDGET
           WHEN   212
               MOVE  "B12"         TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-MCP-WIDGET  >=  "TEKCD01" )
             AND ( WRK-MCP-WIDGET  <=  "TEKCD20" )
      *
               MOVE    WRK-MCP-WIDGET TO   WRK-TEKCD-WIDGET
      *
               IF    ( WRK-MAX    <    WRK-TEKCDNUM )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-MAX     +   1
               ELSE
                   IF    ( FLG-TEKCDDEL   =    ZERO )
                       COMPUTE SPA-GMN-CUR
                           =   WRK-TEKCDNUM
                           +   CONST-LINE-MAX
                   ELSE
                       MOVE    WRK-TEKCDNUM    TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
           WHEN  ( WRK-MCP-WIDGET  >=  "NGBNDKBN01" )
             AND ( WRK-MCP-WIDGET  <=  "NGBNDKBN20" )
      *
               MOVE    WRK-MCP-WIDGET  TO  WRK-NGBNDKBN-WIDGET
      *
               EVALUATE    TRUE
               WHEN  ( WRK-MAX =   ZERO )
                   COMPUTE SPA-GMN-CUR     =   1
               WHEN  ( WRK-MAX     <   WRK-NGBNDKBNNUM )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-MAX +   1
               WHEN  OTHER
                   COMPUTE SPA-GMN-CUR
                       =   WRK-NGBNDKBNNUM
                       +   CONST-LINE-MAX  *   2
               END-EVALUATE
      *
           WHEN  ( WRK-MCP-WIDGET  >=  "CHKERR01" )
             AND ( WRK-MCP-WIDGET  <=  "CHKERR20" )
      *
               MOVE    WRK-MCP-WIDGET  TO  WRK-CHKERR-WIDGET
      *
               EVALUATE    TRUE
               WHEN  ( WRK-MAX =   ZERO )
                   COMPUTE SPA-GMN-CUR     =   1
               WHEN  ( WRK-CHKERRNUM      >=   CONST-LINE-MAX )
                   IF    ( SPA-GMN-PAGE    >=  CONST-PAGE-MAX )
                       COMPUTE SPA-GMN-CUR     =   212
                   ELSE
                       COMPUTE SPA-GMN-CUR     =   207
                   END-IF
               WHEN  ( WRK-MAX     <   WRK-CHKERRNUM )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-MAX +   1
               WHEN  OTHER
                   COMPUTE SPA-GMN-CUR
                       =   WRK-CHKERRNUM   +   1
               END-EVALUATE
      *
           WHEN    OTHER
               MOVE    1           TO  SPA-GMN-CUR
      *
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LINE-MAX )
      *
               COMPUTE WRK-CURSOR  =   IDX0    +   20
               IF    ( SPA-GMN-CUR =   WRK-CURSOR )
                   IF    ( X02-NGBNDKBN-STATE (IDX0)
                                   =   WIDGET-INSENSITIVE )
                       EVALUATE    TRUE
                       WHEN  ( WRK-MAX =   ZERO )
                           COMPUTE SPA-GMN-CUR     =   1
                       WHEN  ( WRK-MAX     <   IDX0 )
                           COMPUTE SPA-GMN-CUR
                               =   WRK-MAX +   1
                       WHEN  OTHER
                           COMPUTE SPA-GMN-CUR
                               =   IDX0
                               +   CONST-LINE-MAX  *   2
                       END-EVALUATE
                   END-IF
               END-IF
      *
               COMPUTE WRK-CURSOR  =   IDX0    +   40
               IF    ( SPA-GMN-CUR =   WRK-CURSOR )
                   IF    ( X02-CHKERR-STATE (IDX0)
                                   =   WIDGET-INSENSITIVE )
                       EVALUATE    TRUE
                       WHEN  ( WRK-MAX =   ZERO )
                           COMPUTE SPA-GMN-CUR     =   1
                       WHEN  ( IDX0   >=   CONST-LINE-MAX )
                           IF    ( SPA-GMN-PAGE    >=  CONST-PAGE-MAX )
                               COMPUTE SPA-GMN-CUR     =   212
                           ELSE
                               COMPUTE SPA-GMN-CUR     =   207
                           END-IF
                       WHEN  ( WRK-MAX     <   IDX0 )
                           COMPUTE SPA-GMN-CUR
                               =   WRK-MAX +   1
                       WHEN  OTHER
                           COMPUTE SPA-GMN-CUR
                               =   IDX0    +   1
                       END-EVALUATE
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
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
                   MOVE    "点数マスタにありません"   TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "エラー区分入力エラー"     TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "項目に入力がありません"   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "適用コードに入力がありません"
                                                      TO  SPA-ERRMSG
               WHEN    "0005"
      *
                   IF    ( WRK-ERR-NAME    NOT =   SPACE )
                       MOVE    SPACE       TO  SPA-ERRMSG
                       STRING  "適用コードが重複しています【"
                                                   DELIMITED BY SIZE
                               WRK-ERR-NAME        DELIMITED BY SPACE
                               "】"                DELIMITED BY SIZE
                       INTO    SPA-ERRMSG
                       END-STRING
                   ELSE
                       MOVE    "適用コードが重複しています"
                                           TO  SPA-ERRMSG
                   END-IF
      *
               WHEN    "0006"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "チェック区分に対して入力できないコードです"
                                                      TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "適用コードの登録上限数に到達しています" 
                                                      TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "セットコードは入力できません"
                                                      TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "同一入外制限区分入力エラー"   TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    SPACE                      TO  SPA-ERRMSG
                   STRING  "適用コードの登録が１００件を超えている場合"
                                                  DELIMITED    BY  SIZE
                           "、複写機能は使用できません"
                                                  DELIMITED    BY  SIZE
                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "更新ができませんでした"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "更新処理前の削除ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XERR
           MOVE    SPA-ERRCD            TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "X02"                TO  SPA-MOTOPG
           MOVE    "XERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XERR"               TO  MCP-WINDOW
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
       520-XIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-XIDMSG
      *
           EVALUATE    SPA-XIDCD
               WHEN    "1001"
                   MOVE    "チェックマスタ設定情報を登録します"
                                       TO  WRK-XIDMSG
               WHEN    "1002"
                   MOVE    "チェックマスタ設定情報を削除します"
                                       TO  WRK-XIDMSG
               WHEN    "2001"
                   MOVE    "チェックマスタを登録後、印刷を行います"
                                       TO  WRK-XIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XID1
           INITIALIZE                       XID1
           MOVE    SPA-XIDCD            TO  XID1-ID1CODE
           MOVE    "戻る"               TO  XID1-B01
           MOVE    "ＯＫ"               TO  XID1-B12
      *
           MOVE    WRK-XIDMSG           TO  XID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "X02"                TO  SPA-MOTOPG
           MOVE    "XID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-XIDSET-EXT.
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
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY21-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
      *    チェックマスタ読み込み
           INITIALIZE                  CHK-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    WRK-SRYCD           TO  CHK-SRYCD
      *
           IF    ( SPA-X01-SEL-STYUKYMD    =    SPACE )
               MOVE    SPA-X01-STYUKYMD        TO  CHK-YUKOEDYMD
               MOVE    SPA-X01-EDYUKYMD        TO  CHK-YUKOSTYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD    TO  CHK-YUKOEDYMD
               MOVE    SPA-X01-SEL-EDYUKYMD    TO  CHK-YUKOSTYMD
           END-IF
           MOVE    "2"                 TO  CHK-CDKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-CD
      *
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHKMST-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理 (KEY2)
      *****************************************************************
       900-CHKMST-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
      *    チェックマスタ読み込み
           INITIALIZE                  CHK-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-SRYCD
      *
           IF    ( SPA-X01-SEL-STYUKYMD    =    SPACE )
               MOVE    SPA-X01-STYUKYMD        TO  CHK-YUKOSTYMD
                                                   CHK-YUKOEDYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD    TO  CHK-YUKOSTYMD
                                                   CHK-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           .
       900-CHKMST-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理 (KEY2)
      *****************************************************************
       900-CHKMST-KEY2-FET-SEC         SECTION.
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           .
       900-CHKMST-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索（KEY4）
      *****************************************************************
       900-TENSU-KEY4-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込（KEY4）
      *****************************************************************
       900-TENSU-KEY4-FET-SEC          SECTION.
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           .
       900-TENSU-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター検索（KEY）
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-TNS-SRYCD   TO  TNS-SRYCD
           MOVE    SPA-X01-KJNYMD  TO  TNS-YUKOSTYMD
                                       TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  TNS-TENSU-REC
           ELSE
               MOVE    1           TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込処理
      *****************************************************************
       900-INPUTCD-SEL-KEY2-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC        =   ZERO )
               MOVE    MCPDATA-REC TO  INPUTCD-REC
           ELSE
               MOVE    1           TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-INPUTCD-SEL-KEY2-EXT.
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
       910-DBCLOSECURSOR-SEC           SECTION.
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
      *    DBDELETE処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    DBINSERT処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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

