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
       PROGRAM-ID.                               ORCGX05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ情報設定（Ｘ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/10/28    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-多々納  07/05/08  グループ診療対応
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
       01  SPA-X05.
           03  SPA-X05-AREA.
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
                   07  SPA-GMN-SRYCD       PIC X(200).
                   07  SPA-GMN-NAME        PIC X(100).
                   07  SPA-GMN-NYUGAIKBN-G.
                       09  SPA-GMN-NYUGAIKBN   PIC X(01).
                       09  SPA-GMN-NYUGAIKBNFL PIC X(01).
                       09  SPA-GMN-NYUGAIKBNMEI
                                               PIC X(20).
                   07  SPA-GMN-NYUGAIBNDKBN-G.
                       09  SPA-GMN-NYUGAIBNDKBN
                                               PIC X(01).
                       09  SPA-GMN-NYUGAIBNDKBNFL
                                               PIC X(01).
                       09  SPA-GMN-NYUGAIBNDKBNMEI
                                               PIC X(20).
                   07  SPA-GMN-JYKNKBN-G.
                       09  SPA-GMN-JYKNKBN     PIC X(01).
                       09  SPA-GMN-JYKNKBNFL   PIC X(01).
                       09  SPA-GMN-JYKNKBNMEI  PIC X(10).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-MAX     PIC 9(05).
                       09  SPA-GMN-TBL-OCC     OCCURS  400.
                           11  SPA-GMN-TTEKCD     PIC X(20).
                           11  SPA-GMN-TNAME      PIC X(100).
                           11  SPA-GMN-TDMKBN-G.
                               13  SPA-GMN-TDMKBN     PIC 9(01).
                               13  SPA-GMN-TDMKBN-X   REDEFINES
                                                      SPA-GMN-TDMKBN
                                                      PIC X(01).
                           11  SPA-GMN-TTEKCD-BF  PIC X(20).
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-NYUGAIKBNLST-G.
                       09  SPA-GMN-NYUGAIKBN-MAX       PIC 9(05).
                       09  SPA-GMN-NYUGAIKBNLST        OCCURS   5.
                           11  SPA-GMN-NYUGAIKBNL      PIC X(01).
                           11  SPA-GMN-NYUGAIKBNFLL    PIC X(01).
                           11  SPA-GMN-NYUGAIKBNMEIL   PIC X(20).
                   07  SPA-GMN-NYUGAIBNDKBNLST-G.
                       09  SPA-GMN-NYUGAIBNDKBN-MAX    PIC 9(05).
                       09  SPA-GMN-NYUGAIBNDKBNLST     OCCURS   5.
                           11  SPA-GMN-NYUGAIBNDKBNL   PIC X(01).
                           11  SPA-GMN-NYUGAIBNDKBNFLL PIC X(01).
                           11  SPA-GMN-NYUGAIBNDKBNMEIL    PIC X(20).
                   07  SPA-GMN-JYKNKBNLST-G.
                       09  SPA-GMN-JYKNKBN-MAX         PIC 9(05).
                       09  SPA-GMN-JYKNKBNLST          OCCURS   5.
                           11  SPA-GMN-JYKNKBNL        PIC X(01).
                           11  SPA-GMN-JYKNKBNFLL      PIC X(01).
                           11  SPA-GMN-JYKNKBNMEIL     PIC X(10).
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
           03  FLG-CHKTRD                         PIC 9(01).
           03  FLG-CHK005                         PIC 9(01).
           03  FLG-TENSU                          PIC 9(01).
      *
           03  FLG-OK                             PIC 9(01).
           03  FLG-ZENKAKU                        PIC 9(01).
           03  FLG-INPUTCD                        PIC 9(01).
           03  FLG-KANJI                          PIC 9(01).
           03  FLG-KENSAKU-SENI                   PIC 9(01).
           03  FLG-CD-TUIKA                       PIC 9(01).
           03  FLG-TEKCDDEL                       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(05).
           03  IDX0                              PIC 9(05).
           03  IDX1                              PIC 9(05).
           03  IDX2                              PIC 9(05).
           03  IDX3                              PIC 9(05).
           03  IDX4                              PIC 9(05).
           03  IDX5                              PIC 9(05).
           03  IDX6                              PIC 9(05).
           03  IDX7                              PIC 9(05).
           03  IDX8                              PIC 9(05).
           03  IDX-CHK                           PIC 9(05).
           03  IDX-ERR                           PIC 9(05).
           03  IDX-LINE                          PIC 9(05).
           03  IDX-CMB                           PIC 9(05).
           03  IDX-PNT                           PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-XIDMSG              PIC X(70). 
           03  WRK-YMD                 PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
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
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM         PIC X(200).
           03  WRK-BASEPOS             PIC 9(05).
           03  WRK-TEKCD-WIDGET.
               05  WRK-TEKCDMEI        PIC X(05).
               05  WRK-TEKCDNUM        PIC 9(02).
           03  WRK-DMKBN-WIDGET.
               05  WRK-DMKBNMEI        PIC X(05).
               05  WRK-DMKBNNUM        PIC 9(02).
           03  WRK-GMN-TBL-MAX         PIC 9(05).
           03  WRK-SORT-DMKBN          PIC 9(01).
           03  WRK-ID                  PIC 9(09).
      *
       01  TBL-AREA.
           03  TBL-GMN-AREA.
               05  TBL-GMN-MAX         PIC 9(05).
               05  TBL-GMN-OCC         OCCURS  400.
                   07  TBL-GMN-TTEKCD     PIC X(20).
                   07  TBL-GMN-TNAME      PIC X(100).
                   07  TBL-GMN-TDMKBN     PIC 9(01).
                   07  TBL-GMN-TTEKCD-BF  PIC X(20).
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
       01  CONST-AREA.
           03  CONST-TEKCD-MAX     PIC 9(05)   VALUE   400.
           03  CONST-LINE-MAX      PIC 9(03)   VALUE   20. 
           03  CONST-PAGE-MAX      PIC 9(02)   VALUE   20.
           03  CONST-MKCD-MAX      PIC 9(03)   VALUE   21. 
           03  CONST-SRY           PIC 9(01)   VALUE   0.
           03  CONST-BYO           PIC 9(01)   VALUE   1.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    自院マスター
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    チェックマスタ
       01  CHKTRD-REC.
           COPY    "CPCHKTRD.INC".
      *
       01  CHK005-REC.
           COPY    "CPCHK005.INC".
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
           MOVE    SPA-X01SONOTA   TO  SPA-X05 
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
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-X01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-X05         TO  SPA-X01SONOTA
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
           IF    ( SPA-MOTOPG          =   "XERR" )
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      **** MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "X05    "           TO  MCP-WINDOW.
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
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "X98"
               PERFORM 320-X98SPASET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "XID1"
               PERFORM 330-XID1-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    "X03"
               PERFORM 340-X03-SET-SEC
               GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
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
           INITIALIZE                      SPA-X05-AREA
      *
           MOVE    SPA-X01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
           MOVE    SPA-X01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
           MOVE    SPA-X01-CHKKBN-G    TO  SPA-GMN-CHKKBN-G
           MOVE    SPA-X01-NAME        TO  SPA-GMN-NAME
      *
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
               PERFORM 3101-BYOMEICD-SEC
           ELSE
               MOVE    SPA-X01-SRYCD       TO  SPA-GMN-SRYCD
           END-IF
      *
      *    コンボボックス初期処理
           PERFORM 3101-COMBO-SYOKI-SEC
      *
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
      *
               MOVE    "1"             TO  WRK-CMB-CD
               PERFORM 4200-JYKNKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYKNKBN-G
      *
               PERFORM 3101-SPASET-CHK005-SEC
           ELSE
      *
               MOVE    "2"             TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIBNDKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUGAIBNDKBN-G
               PERFORM 3101-SPASET-CHKTRD-SEC
           END-IF
      *
           COMPUTE SPA-GMN-PAGE        =   (SPA-GMN-TBL-MAX - 1)
                                       /   CONST-LINE-MAX   +   1
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名コード欄編集処理
      *****************************************************************
       3101-BYOMEICD-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-SRYCD
           MOVE    1                   TO  IDX-PNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       CONST-MKCD-MAX )
                    OR   ( SPA-X01-BYOCD (IDX1)
                                   =       SPACE OR "0000999" )
      *
               IF    ( IDX1        =       1 )
                   STRING    SPA-X01-BYOCD (IDX1)  DELIMITED   BY  SPACE
                   INTO      SPA-GMN-SRYCD
                   WITH POINTER IDX-PNT
                   END-STRING
               ELSE
                   STRING    ","                   DELIMITED   BY  SIZE
                             SPA-X01-BYOCD (IDX1)  DELIMITED   BY  SPACE
                   INTO      SPA-GMN-SRYCD
                   WITH POINTER IDX-PNT
                   END-STRING
               END-IF
      *
           END-PERFORM
      *
           .
      *
       3101-BYOMEICD-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３からスパ設定処理
      *****************************************************************
       3101-SPASET-CHKTRD-SEC          SECTION.
      *
      *    チェックマスタ読み込み
           PERFORM 900-CHKTRD-KEY2-SEL-SEC
           IF    ( FLG-CHKTRD      =   ZERO )
               MOVE    CHKTRD-NYUGAIKBN
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUGAIKBN-G
      *
               MOVE    CHKTRD-NYUGAIBNDKBN
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIBNDKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-NYUGAIBNDKBN-G
      *
           END-IF
     *
           INITIALIZE                      SPA-GMN-TBL
      *
           PERFORM UNTIL ( SPA-GMN-TBL-MAX >=   CONST-TEKCD-MAX )
                    OR   ( FLG-CHKTRD       =   1 )
      *
               COMPUTE SPA-GMN-TBL-MAX
                   =   SPA-GMN-TBL-MAX      +    1
      *
                   MOVE    CHKTRD-CD   TO  SPA-GMN-TTEKCD
                                                (SPA-GMN-TBL-MAX)
                                           SPA-GMN-TTEKCD-BF
                                                (SPA-GMN-TBL-MAX)
      *
                   MOVE    CHKTRD-DAYMONTHKBN
                                       TO  SPA-GMN-TDMKBN-X
                                                (SPA-GMN-TBL-MAX)
                   MOVE    CHKTRD-CD   TO  WRK-SRYCD
                   PERFORM 3101-TENNAME-SEC
      *
               PERFORM 900-CHKTRD-KEY2-FET-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SPASET-CHKTRD-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５からスパ設定処理
      *****************************************************************
       3101-SPASET-CHK005-SEC          SECTION.
      *
      *    チェックマスタ読み込み
           PERFORM 900-CHK005-KEY3-SEL-SEC
           IF    ( FLG-CHK005      =   ZERO )
      *
               MOVE    CHK005-COMPKBN  TO  WRK-CMB-CD
               PERFORM 4200-JYKNKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-JYKNKBN-G
      *
           END-IF
     *
           INITIALIZE                      SPA-GMN-TBL
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-TEKCD-MAX )
               MOVE    1           TO  SPA-GMN-TDMKBN (IDX1)
           END-PERFORM
      *
           PERFORM UNTIL ( SPA-GMN-TBL-MAX >=   CONST-TEKCD-MAX )
                    OR   ( FLG-CHK005       =   1 )
      *
               COMPUTE SPA-GMN-TBL-MAX
                   =   SPA-GMN-TBL-MAX      +    1
      *
                   MOVE    CHK005-SRYCD
                                       TO  SPA-GMN-TTEKCD
                                                (SPA-GMN-TBL-MAX)
                                           SPA-GMN-TTEKCD-BF
                                                (SPA-GMN-TBL-MAX)
      *
                   MOVE    CHK005-SRYCD TO WRK-SRYCD
                   PERFORM 3101-TENNAME-SEC
                   MOVE    CHK005-DAYMONTHKBN
                                       TO  SPA-GMN-TDMKBN
                                                (SPA-GMN-TBL-MAX)
      *
               PERFORM 900-CHK005-KEY3-FET-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SPASET-CHK005-EXT.
           EXIT.
      *****************************************************************
      *    コンボボックス初期処理
      *****************************************************************
       3101-COMBO-SYOKI-SEC            SECTION.
      *
      *    入外区分コンボ設定処理
           PERFORM 3101-NYUGAIKBNLST-SEC
      *
      *    入外別区分コンボ設定処理
           PERFORM 3101-NYUGAIBNDKBNLST-SEC
      *
      *    条件区分コンボ設定処理
           PERFORM 3101-JYKNKBNLST-SEC
      *
           .
       3101-COMBO-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入外区分コンボ設定処理
      *****************************************************************
       3101-NYUGAIKBNLST-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUGAIKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-NYUGAIKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-NYUGAIKBNL    (1)
           MOVE    "入院限定"      TO  SPA-GMN-NYUGAIKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-NYUGAIKBNL    (2)
           MOVE    "外来限定"      TO  SPA-GMN-NYUGAIKBNMEIL (2)
      *
           . 
       3101-NYUGAIKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    入外別区分コンボ設定処理
      *****************************************************************
       3101-NYUGAIBNDKBNLST-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-NYUGAIBNDKBNLST-G
      *
           MOVE    2               TO  SPA-GMN-NYUGAIBNDKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-NYUGAIBNDKBNL    (1)
           MOVE    "しない"        TO  SPA-GMN-NYUGAIBNDKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-NYUGAIBNDKBNL    (2)
           MOVE    "する"          TO  SPA-GMN-NYUGAIBNDKBNMEIL (2)
      *
           . 
       3101-NYUGAIBNDKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    条件区分コンボ設定処理
      *****************************************************************
       3101-JYKNKBNLST-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-JYKNKBNLST-G
      *
           MOVE    4               TO  SPA-GMN-JYKNKBN-MAX
      *
           MOVE    "1"             TO  SPA-GMN-JYKNKBNL    (1)
           MOVE    "部分一致"      TO  SPA-GMN-JYKNKBNMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-JYKNKBNL    (2)
           MOVE    "完全一致"      TO  SPA-GMN-JYKNKBNMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-JYKNKBNL    (3)
           MOVE    "前方一致"      TO  SPA-GMN-JYKNKBNMEIL (3)
      *
           MOVE    "4"             TO  SPA-GMN-JYKNKBNL    (4)
           MOVE    "後方一致"      TO  SPA-GMN-JYKNKBNMEIL (4)
      *
           . 
       3101-JYKNKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       3101-TENNAME-SEC          SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    WRK-SRYCD           TO  WRK-TNS-SRYCD
           PERFORM 900-TENSU-KEY4-SEL-SEC
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-NAME
           PERFORM UNTIL ( FLG-TENSU   =   1 )
      *
               MOVE    SPACE           TO  WRK-TNS-NAME
      *
               IF    ( TNS-SRYCD(1:1)      =   "1") AND
                     ( TNS-ROUTEKKBN       =   2  )
      *
                   STRING  "［老］"        DELIMITED   BY  SIZE
                           TNS-NAME        DELIMITED   BY  SIZE
                   INTO    WRK-TNS-NAME
                   END-STRING
      *
               ELSE
                   MOVE    TNS-NAME    TO  WRK-TNS-NAME
               END-IF
      *
      *        有効終了日が対象のマスタを対象とする
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
           IF    ( FLG-OK              =   ZERO )
            AND  ( WRK-NAME        NOT =   SPACE)
      *
               MOVE    SPACE           TO  SPA-GMN-TNAME
                                                  (SPA-GMN-TBL-MAX)
               STRING  "★削除済★"    DELIMITED   BY  SIZE
                       WRK-NAME        DELIMITED   BY  SIZE
               INTO    SPA-GMN-TNAME (SPA-GMN-TBL-MAX)
               END-STRING
      *
           END-IF
      *
           .
       3101-TENNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-X98SPASET-SEC               SECTION.
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
                       MOVE    ZERO    TO  TBL-GMN-TDMKBN (IDX7)
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
           IF    ( SPA-XID1-FLG        =   "OK" )
               EVALUATE    SPA-XIDCD
               WHEN    "1001"
                   IF    ( SPA-X01-SRYBYO  =   CONST-BYO )
                       PERFORM 491-KOUSHIN-SEC
                   ELSE
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
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
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-TEKCD-MAX )
      *
                   MOVE    1           TO  SPA-GMN-TDMKBN (IDX1)
      *
               END-PERFORM
      *
           END-IF
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
                                       SPA-GMN-TTEKCD-BF (IDX5)
               MOVE    SPA-X03-GMN-TLCHK-NAME (IDX5)
                                   TO  SPA-GMN-TNAME (IDX5)
               MOVE    SPA-X03-GMN-TLCHK-JYKN (IDX5)
                                   TO  SPA-GMN-TDMKBN (IDX5)
      *
           END-PERFORM
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
           EVALUATE    MCP-EVENT   ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"         TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    削除
           WHEN    "CLICKED"       ALSO    "B03"
               PERFORM  403-DEL-SEC
      *    並べ替え
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM  450-SORT-SEC
      *    前画面
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 430-MAEGMN-SEC
      *    次画面
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 440-ATOGMN-SEC
      *    チェック項目複写
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM  409-X03-SENI-SEC
      *    検索
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM  409-KENSAKU-CHK-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM  410-KAKUTEI-SEC
      *    印刷
           WHEN    "CLICKED"       ALSO    "B11"
      *        IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
      *            CONTINUE
      *        ELSE
                   PERFORM  411-PRINT-SEC
      *        END-IF
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
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
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
           MOVE    X05-NYUGAIKBN       TO  SPA-GMN-NYUGAIKBN-G
      *
           MOVE    X05-NYUGAIBNDKBN    TO  SPA-GMN-NYUGAIBNDKBN-G
      *
           MOVE    X05-JYKNKBN         TO  SPA-GMN-JYKNKBN-G
      *
           PERFORM VARYING IDX-LINE    FROM    1   BY  1
                   UNTIL ( IDX-LINE    >   CONST-LINE-MAX )
      *
               COMPUTE IDX7
                   =   IDX-LINE
                   +  (SPA-GMN-PAGE - 1 )  *   CONST-LINE-MAX
      *
               MOVE    X05-TEKCD (IDX-LINE)    TO  SPA-GMN-TTEKCD
                                                                 (IDX7)
               MOVE    X05-DMKBN (IDX-LINE)    TO  SPA-GMN-TDMKBN(IDX7)
      *
               MOVE    SPA-GMN-TTEKCD (IDX7)    TO  WRK-TEKCD
               INSPECT WRK-TEKCD       REPLACING   ALL "-"  BY  " "
               IF    ( SPA-GMN-TTEKCD (IDX7)    NOT =   WRK-TEKCD )
                   MOVE    SPACE               TO  SPA-GMN-TTEKCD (IDX7)
               END-IF
      *
      *        全角入力の時、１０文字まで
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                     TO  KANACHK-SYORI
               MOVE    SPA-GMN-TTEKCD (IDX7)   TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT       TO  SPA-GMN-TTEKCD
                                                                 (IDX7)
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
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 4101-JYKNKBN-CHK-SEC
               END-IF
           ELSE
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 4101-NYUGAIKBN-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                   PERFORM 4101-NYUGAIBNDKBN-CHK-SEC
               END-IF
           END-IF
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
                   IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
                       CONTINUE
                   ELSE
      *                日・月別区分チェック処理
                       PERFORM 4101-DMKBN-CHK-SEC
                   END-IF
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
           MOVE    "X05"               TO  SPA-MOTOPG
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
      *    入外区分チェック
      *****************************************************************
       4101-NYUGAIKBN-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NYUGAIKBN   TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIKBN-G
      *
           .
       4101-NYUGAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外別区分チェック
      *****************************************************************
       4101-NYUGAIBNDKBN-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-NYUGAIBNDKBN    TO  WRK-CMB-CD
           PERFORM 4200-NYUGAIBNDKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "2"                 TO  WRK-CMB-CD
               PERFORM 4200-NYUGAIBNDKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIBNDKBN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-NYUGAIBNDKBN-G
           END-IF
      *
           .
       4101-NYUGAIBNDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外別区分チェック
      *****************************************************************
       4101-JYKNKBN-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-JYKNKBN    TO  WRK-CMB-CD
           PERFORM 4200-JYKNKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM             =   SPACE )
               MOVE    "1"                 TO  WRK-CMB-CD
               PERFORM 4200-JYKNKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYKNKBN-G
           ELSE
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-JYKNKBN-G
           END-IF
      *
           .
       4101-JYKNKBN-CHK-EXT.
           EXIT.
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
                                   NOT =  SPA-GMN-TTEKCD-BF (IDX-LINE) )
                   PERFORM 41013-TENSU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO            TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
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
           MOVE    SPA-GMN-TBL-MAX     TO  WRK-GMN-TBL-MAX
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
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-TEKCD-MAX )
      *
                   MOVE    1           TO  SPA-GMN-TDMKBN (IDX1)
      *
               END-PERFORM
      *
           END-IF
      *
           IF    ( WRK-GMN-TBL-MAX     >   SPA-GMN-TBL-MAX )
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
               IF    ( WRK-MOJI        =   9 )
                AND  ( FLG-KANJI       =   ZERO )
      *
                   MOVE   WRK-SRYCD        TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF  ( FLG-TENSU     =   1 )
                       MOVE    "0001"  TO  SPA-ERRCD
                       MOVE    IDX-LINE    TO  IDX-ERR
                       GO  TO  41013-TENSU-CHK-EXT
                   END-IF
               ELSE
                   COMPUTE SPA-GMN-CUR     =   IDX-LINE
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
           EVALUATE    SPA-X01-CHKKBN
      *    WHEN    "8"
      *        IF    ( SPA-GMN-TTEKCD (IDX-LINE) (1:1) NOT =   "6" )
      *            MOVE    "0008"      TO  SPA-ERRCD
      *            MOVE    IDX-LINE    TO  IDX-ERR
      *        END-IF
           WHEN    "9"
               IF    ( SPA-GMN-TTEKCD (IDX-LINE) (1:1) =   "6" )
                   MOVE    "0008"      TO  SPA-ERRCD
                   MOVE    IDX-LINE    TO  IDX-ERR
               END-IF
           END-EVALUATE
      *
           IF    ( SPA-ERRCD           =   SPACE )
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
                   UNTIL ( IDX3    >   20  )
                    OR   ( WRK-SRYCD (IDX3:)
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
       41014-JYUFUKU-CHK-SEC           SECTION.
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
      *    日・月別区分チェック処理
      *****************************************************************
       4101-DMKBN-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-TDMKBN (IDX-LINE)
                                       =   ZERO OR 1 )
               CONTINUE
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    20              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-DMKBN-CHK-EXT.
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
      *    並べ替え処理
      *****************************************************************
       450-SORT-SEC                    SECTION.
      *
      *    入力チェック処理
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 410-INPUT-CHK-SEC
           END-IF
      *
           INITIALIZE  TBL-AREA
      *
           MOVE    ZERO        TO  WRK-SORT-DMKBN
           PERFORM 4501-SORT-SEC
      *
           MOVE    1           TO  WRK-SORT-DMKBN
           PERFORM 4501-SORT-SEC
      *
           MOVE    TBL-AREA    TO  SPA-GMN-TBL
      *
           .
       450-SORT-EXT.
           EXIT.
      *****************************************************************
      *    並べ替え処理
      *****************************************************************
       4501-SORT-SEC                   SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-TBL-MAX )
      *
               IF    ( SPA-GMN-TTEKCD (IDX0)
                                   NOT =   SPACE )
                AND  ( SPA-GMN-TDMKBN(IDX0)
                                       =   WRK-SORT-DMKBN  )
                   COMPUTE TBL-GMN-MAX =   TBL-GMN-MAX +   1
                   MOVE    SPA-GMN-TBL-OCC (IDX0)
                                       TO  TBL-GMN-OCC (TBL-GMN-MAX)
               END-IF
      *
           END-PERFORM
      *
           .
       4501-SORT-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-GMN-PAGE   =   1 )
               MOVE    "0006"     TO  SPA-ERRCD
               GO  TO  430-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-PAGE   =   SPA-GMN-PAGE    -   1
      *
           MOVE    1              TO  SPA-GMN-CUR
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
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-GMN-PAGE    =   CONST-PAGE-MAX )
               MOVE    "0007"      TO  SPA-ERRCD
               GO  TO  440-ATOGMN-EXT
           ELSE
               COMPUTE IDX     =   SPA-GMN-PAGE   *   CONST-LINE-MAX
               IF    ( SPA-GMN-TTEKCD (IDX)   =   SPACE )
                   MOVE    "0007"  TO  SPA-ERRCD
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
                   UNTIL ( IDX8    >   CONST-LINE-MAX )
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
           IF    ( SPA-X01-SRYBYO  =   CONST-BYO )
               PERFORM 40321-DEL-SEC
               IF    ( SPA-ERRCD    =  SPACE )
                   MOVE   "JOIN"   TO   MCP-PUTTYPE
                   PERFORM 210-BACK
               END-IF
           ELSE
               PERFORM 40311-DEL-SEC
               IF    ( SPA-ERRCD    =  SPACE )
                   MOVE   "JOIN"   TO   MCP-PUTTYPE
                   PERFORM 210-BACK
               END-IF
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
           INITIALIZE                  CHKTRD-REC
           MOVE    SPA-X01-CHKKBN      TO  CHKTRD-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHKTRD-SRYCD
           MOVE    SPA-X01-SEL-STYUKYMD TO CHKTRD-YUKOSTYMD
           MOVE    SPA-X01-SEL-EDYUKYMD TO CHKTRD-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
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
      *    削除処理
      *****************************************************************
       40321-DEL-SEC                   SECTION.
      *
      *    チェックマスタ削除
           INITIALIZE                  CHK005-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK005-CHKKBN
           MOVE    SPA-X01-ID          TO  CHK005-ID
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC          NOT =   ZERO  )
               MOVE    "1002"          TO  SPA-ERRCD
           END-IF
      *
           .
       40321-DEL-EXT.
           EXIT.
      *****************************************************************
      *    チェック項目複写処理
      *****************************************************************
       409-X03-SENI-SEC                SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO      TO      409-X03-SENI-EXT
           END-IF
      *
      *    チェック項目複写画面初期処理
           PERFORM 4091-X03-INIT-SEC
      *
           MOVE    "X03"               TO  SPA-SAKIPG
           MOVE    "X05"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
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
               MOVE    SPA-GMN-TDMKBN-X(IDX4)
                                       TO  SPA-X03-GMN-TLCHK-JYKN (IDX4)
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
      *
           IF    ( WRK-INPUTCD     NOT =   SPACE )
               EVALUATE    SPA-X01-CHKKBN
               WHEN    "1"
                   MOVE    "1"             TO  SPA-X98-GMN-KBN
                   MOVE    SPACE           TO  SPA-X98-GMN-SYORI
               WHEN    "3"
                   MOVE    "2"             TO  SPA-X98-GMN-KBN
                   MOVE    "3"             TO  SPA-X98-GMN-SYORI
               WHEN    "4"
               WHEN    "8"
                   MOVE    "2"             TO  SPA-X98-GMN-KBN
                   MOVE    "3"             TO  SPA-X98-GMN-SYORI
               WHEN    "2"
                   MOVE    "1"             TO  SPA-X98-GMN-KBN
                   MOVE    SPACE           TO  SPA-X98-GMN-SYORI
               WHEN    "5"
               WHEN    "7"
               WHEN    "9"
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
                       MOVE    "A"         TO  SPA-X98-CDSYUBETU
                   END-IF
               END-IF
           END-IF
      *
      *    画面編集
           PERFORM 41011-X98-SET-SEC
      *
           MOVE    "X05"               TO  SPA-MOTOPG
           MOVE    "X98"               TO  SPA-SAKIPG
           MOVE    "X05"               TO  SPA-X98-MOTOPG
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
           MOVE    SPA-X05         TO  SPA-X01SONOTA
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
           MOVE    SPA-X01SONOTA   TO  SPA-X05
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
           .
       41011-X98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       411-PRINT-SEC               SECTION.
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
       4111-PRINT-SEC               SECTION.
      *
           IF    ( SPA-X01-SRYBYO  =   CONST-BYO )
               PERFORM 4911-KOUSHIN-SEC
           ELSE
               PERFORM 4901-KOUSHIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               IF    ( SPA-X01-SELNUM      =   ZERO )
                   MOVE    99              TO  SPA-X01-SELNUM
                   MOVE    SPA-X01-STYUKYMD
                                           TO  SPA-X01-SEL-STYUKYMD
                   MOVE    SPA-X01-EDYUKYMD
                                           TO  SPA-X01-SEL-EDYUKYMD
               END-IF
      *
               INITIALIZE              ORCHXLSTAREA
               MOVE    SPA-GMN-CHKKBN  TO  HXLST-CHKKBN
               IF    ( SPA-X01-SRYBYO  =   CONST-BYO )
                   MOVE    WRK-ID      TO  HXLST-ID
               ELSE
                   MOVE    SPA-GMN-SRYCD
                                       TO  HXLST-SRYCD
               END-IF
               MOVE    "1"             TO  HXLST-PROCKBN
               MOVE    SPA-X01-KJNYMD  TO  HXLST-KJNYMD
               CALL    "ORCHXLST"  USING   SPA-AREA
                                           ORCHXLSTAREA
           END-IF
      *
           .
       4111-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定
      *****************************************************************
       410-KAKUTEI-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO      410-KAKUTEI-EXT
           END-IF
      *
           PERFORM 4901-KANRENCHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "1001"          TO  SPA-XIDCD
           END-IF
      *
           .
       410-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NYUGAIKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUGAIKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NYUGAIKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUGAIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入外別区分コンボチェック処理
      *****************************************************************
       4200-NYUGAIBNDKBNLST-CHK-SEC       SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-NYUGAIBNDKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-NYUGAIBNDKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-NYUGAIBNDKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-NYUGAIBNDKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    条件区分コンボチェック処理
      *****************************************************************
       4200-JYKNKBNLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   SPA-GMN-JYKNKBN-MAX)
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-JYKNKBNL (IDX-CMB)
                                   =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-JYKNKBNLST (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-JYKNKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
           PERFORM 4901-KOUSHIN-SEC
           IF    ( SPA-ERRCD    =  SPACE )
               MOVE   "JOIN"     TO   MCP-PUTTYPE 
               PERFORM 210-BACK
           END-IF
      *
           .
       490-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       4901-KOUSHIN-SEC            SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 40311-DEL-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  4901-KOUSHIN-EXT
           END-IF
      *
           INITIALIZE                  CHKTRD-REC
           MOVE    SPA-X01-CHKKBN      TO  CHKTRD-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHKTRD-SRYCD
           MOVE    SPA-X01-STYUKYMD    TO  CHKTRD-YUKOSTYMD
           MOVE    SPA-X01-EDYUKYMD    TO  CHKTRD-YUKOEDYMD
           MOVE    "2"                 TO  CHKTRD-CDKBN
           MOVE    SPA-GMN-NYUGAIKBN   TO  CHKTRD-NYUGAIKBN
           MOVE    SPA-GMN-NYUGAIBNDKBN TO CHKTRD-NYUGAIBNDKBN
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-TBL-MAX )
      *
               MOVE    IDX1            TO  CHKTRD-RENNUM
               MOVE    SPA-GMN-TTEKCD (IDX1)
                                       TO  CHKTRD-CD
               MOVE    SPA-GMN-TDMKBN-X (IDX1)
                                       TO  CHKTRD-DAYMONTHKBN
               MOVE    SMCNDATE-YMD    TO  CHKTRD-CREYMD
                                           CHKTRD-UPYMD
               MOVE    SMCNDATE-HMS    TO  CHKTRD-UPHMS
               MOVE    SPA-HOSPNUM     TO  CHKTRD-HOSPNUM
               MOVE    CHKTRD-REC      TO  MCPDATA-REC
               MOVE    "tbl_chktrd"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO  )
                   MOVE    "1001"      TO  SPA-ERRCD
                   GO  TO  4901-KOUSHIN-EXT
               END-IF
      *
           END-PERFORM
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       登録処理
      *****************************************************************
       491-KOUSHIN-SEC             SECTION.
      *
           PERFORM 4911-KOUSHIN-SEC
           IF    ( SPA-ERRCD    =  SPACE )
               MOVE   "JOIN"     TO   MCP-PUTTYPE 
               PERFORM 210-BACK
           END-IF
      *
           .
       491-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       4911-KOUSHIN-SEC                SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 40321-DEL-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  4911-KOUSHIN-EXT
           END-IF
      *
           IF    ( SPA-X01-SELNUM      =   ZERO )
               PERFORM 900-CHK005-KEY4-SEL-SEC
               COMPUTE WRK-ID      =   CHK005-ID   +   1
           ELSE
               COMPUTE WRK-ID      =   SPA-X01-ID
           END-IF
      *
           INITIALIZE                  CHK005-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK005-CHKKBN
           MOVE    WRK-ID              TO  CHK005-ID
           MOVE    SPA-X01-STYUKYMD    TO  CHK005-YUKOSTYMD
           MOVE    SPA-X01-EDYUKYMD    TO  CHK005-YUKOEDYMD
           MOVE    "2"                 TO  CHK005-CDKBN
           MOVE    SPA-GMN-NYUGAIKBN   TO  CHK005-NYUGAIKBN
           MOVE    SPA-X01-NAME        TO  CHK005-BYOMEI
           MOVE    SPA-GMN-JYKNKBN     TO  CHK005-COMPKBN
           MOVE    SPA-X01-BYOSTATUS   TO  CHK005-BYOMEIHENFLG
           MOVE    SPA-X01-BYOCD-G     TO  CHK005-CD
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-TBL-MAX )
               MOVE    IDX1            TO  CHK005-RENNUM
               MOVE    SPA-GMN-TTEKCD (IDX1)
                                       TO  CHK005-SRYCD
               MOVE    "1"
                                       TO  CHK005-DAYMONTHKBN
               MOVE    SMCNDATE-YMD    TO  CHK005-CREYMD
                                           CHK005-UPYMD
               MOVE    SMCNDATE-HMS    TO  CHK005-UPHMS
               MOVE    SPA-HOSPNUM     TO  CHK005-HOSPNUM
               MOVE    CHK005-REC      TO  MCPDATA-REC
               MOVE    "tbl_chk005"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC          NOT =   ZERO  )
                   MOVE    "1001"      TO  SPA-ERRCD
                   GO  TO  4911-KOUSHIN-EXT
               END-IF
           END-PERFORM
      *
           .
       4911-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       4901-KANRENCHK-SEC              SECTION.
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
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-XIDCD       NOT =   SPACE )
               PERFORM 520-XIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "X05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE              X05
      *
           COMPUTE WRK-LINE-MAX
               =   SPA-GMN-PAGE    *   CONST-LINE-MAX
           COMPUTE WRK-LINE-MIN
               =   WRK-LINE-MAX    - ( CONST-LINE-MAX  - 1 )
      *
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  WRK-MAX
      *
           PERFORM VARYING IDX1     FROM    WRK-LINE-MIN   BY  1
                   UNTIL ( IDX1     >       WRK-LINE-MAX )
      *
               COMPUTE IDX2    =    IDX2    +   1
      *
               MOVE    IDX1                    TO  X05-NO      (IDX2)
               MOVE    SPA-GMN-TTEKCD  (IDX1)  TO  X05-TEKCD   (IDX2)
               MOVE    SPA-GMN-TNAME   (IDX1)  TO  X05-DSPNAME (IDX2)
               MOVE    SPA-GMN-TDMKBN  (IDX1)  TO  X05-DMKBN   (IDX2)
      *
               IF    ( SPA-GMN-TTEKCD  (IDX1)  NOT =   SPACE )
                   MOVE    IDX2                TO  WRK-MAX
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  X05-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  X05-SRYCD
           MOVE    SPA-GMN-NAME        TO  X05-NAME
           MOVE    SPA-GMN-STYUKYMD    TO  X05-STYUKYMD
           MOVE    SPA-GMN-EDYUKYMD    TO  X05-EDYUKYMD
      *
           MOVE    SPA-GMN-NYUGAIKBN-G TO  X05-NYUGAIKBN
           MOVE    SPA-GMN-NYUGAIKBN-MAX
                                       TO  X05-NYUGAIKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUGAIKBN-MAX )
               MOVE    SPA-GMN-NYUGAIKBNLST (IDX1)
                                       TO  X05-NYUGAIKBN-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUGAIBNDKBN-G
                                       TO  X05-NYUGAIBNDKBN
           MOVE    SPA-GMN-NYUGAIBNDKBN-MAX
                                       TO  X05-NYUGAIBNDKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-NYUGAIBNDKBN-MAX )
               MOVE    SPA-GMN-NYUGAIBNDKBNLST (IDX1)
                                       TO  X05-NYUGAIBNDKBN-ITEM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-JYKNKBN-G   TO  X05-JYKNKBN
           MOVE    SPA-GMN-JYKNKBN-MAX TO  X05-JYKNKBN-COUNT
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-JYKNKBN-MAX )
               MOVE    SPA-GMN-JYKNKBNLST (IDX1)
                                       TO  X05-JYKNKBN-ITEM (IDX1)
           END-PERFORM
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
           INITIALIZE                  WRK-PAGE-DSP-G
           MOVE    SPA-GMN-PAGE        TO  WRK-PAGE-DSP-ACTIVE
           MOVE    "/"                 TO  WRK-PAGE-DSP-LBL
           MOVE    WRK-PAGE-MAX        TO  WRK-PAGE-DSP-MAX
           MOVE    WRK-PAGE-DSP-G      TO  X05-LBLPAGE
      *
           MOVE    "black"             TO  X05-LBLSRYCD-STYLE
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
               IF    ( SPA-X01-BYOSTATUS   =   "1" )
                   MOVE    "＊病名編集＊"  TO  X05-LBLSRYCD
                   MOVE    "blue"          TO  X05-LBLSRYCD-STYLE
               ELSE
                   MOVE    "病名コード"    TO  X05-LBLSRYCD
               END-IF
               MOVE    "一致条件"          TO  X05-LBLJYKN
               MOVE    "日／月（０／１）"  TO  X05-LBLDM
           ELSE
               MOVE    "診療行為コード"    TO  X05-LBLSRYCD
               MOVE    "入外別にチェック"  TO  X05-LBLNYUGAIBND
               MOVE    "日／月（０／１）"  TO  X05-LBLDM
           END-IF
           MOVE    "印刷"                  TO  X05-B11-LABEL
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
           MOVE    WIDGET-INSENSITIVE  TO  X05-B03-STATE
                                           X05-NYUGAIKBN-STATE
                                           X05-NYUGAIKBN-ITEM-STATE
                                           X05-NYUGAIBNDKBN-STATE
                                           X05-NYUGAIBNDKBN-ITEM-STATE
                                           X05-JYKNKBN-STATE
                                           X05-JYKNKBN-ITEM-STATE
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   CONST-LINE-MAX )
      *
               MOVE    WIDGET-INSENSITIVE
                                       TO  X05-DMKBN-STATE (IDX1)
      *
           END-PERFORM
      *
           IF    ( SPA-X01-SRYBYO      =   CONST-BYO )
               MOVE    WIDGET-NORMAL   TO  X05-JYKNKBN-STATE
                                           X05-JYKNKBN-ITEM-STATE
           ELSE
               MOVE    WIDGET-NORMAL   TO  X05-NYUGAIKBN-STATE
                                           X05-NYUGAIKBN-ITEM-STATE
                                           X05-NYUGAIBNDKBN-STATE
                                           X05-NYUGAIBNDKBN-ITEM-STATE
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   CONST-LINE-MAX )
      *
                   MOVE    WIDGET-NORMAL
                                       TO  X05-DMKBN-STATE (IDX1)
      *
               END-PERFORM
           END-IF
      *
           IF    ( SPA-X01-SELNUM          >   ZERO    )
               MOVE    WIDGET-NORMAL   TO  X05-B03-STATE
           END-IF 
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
           WHEN   1
                MOVE  "TEKCD01"    TO  MCP-WIDGET
           WHEN   2
                MOVE  "TEKCD02"    TO  MCP-WIDGET
           WHEN   3
                MOVE  "TEKCD03"    TO  MCP-WIDGET
           WHEN   4
                MOVE  "TEKCD04"    TO  MCP-WIDGET
           WHEN   5
                MOVE  "TEKCD05"    TO  MCP-WIDGET
           WHEN   6
                MOVE  "TEKCD06"    TO  MCP-WIDGET
           WHEN   7
                MOVE  "TEKCD07"    TO  MCP-WIDGET
           WHEN   8
                MOVE  "TEKCD08"    TO  MCP-WIDGET
           WHEN   9
                MOVE  "TEKCD09"    TO  MCP-WIDGET
           WHEN   10
                MOVE  "TEKCD10"    TO  MCP-WIDGET
           WHEN   11
                MOVE  "TEKCD11"    TO  MCP-WIDGET
           WHEN   12
                MOVE  "TEKCD12"    TO  MCP-WIDGET
           WHEN   13
                MOVE  "TEKCD13"    TO  MCP-WIDGET
           WHEN   14
                MOVE  "TEKCD14"    TO  MCP-WIDGET
           WHEN   15
                MOVE  "TEKCD15"    TO  MCP-WIDGET
           WHEN   16
                MOVE  "TEKCD16"    TO  MCP-WIDGET
           WHEN   17
                MOVE  "TEKCD17"    TO  MCP-WIDGET
           WHEN   18
                MOVE  "TEKCD18"    TO  MCP-WIDGET
           WHEN   19
                MOVE  "TEKCD19"    TO  MCP-WIDGET
           WHEN   20
                MOVE  "TEKCD20"    TO  MCP-WIDGET
           WHEN   21
                MOVE  "DMKBN01"    TO  MCP-WIDGET
           WHEN   22
                MOVE  "DMKBN02"    TO  MCP-WIDGET
           WHEN   23
                MOVE  "DMKBN03"    TO  MCP-WIDGET
           WHEN   24
                MOVE  "DMKBN04"    TO  MCP-WIDGET
           WHEN   25
                MOVE  "DMKBN05"    TO  MCP-WIDGET
           WHEN   26
                MOVE  "DMKBN06"    TO  MCP-WIDGET
           WHEN   27
                MOVE  "DMKBN07"    TO  MCP-WIDGET
           WHEN   28
                MOVE  "DMKBN08"    TO  MCP-WIDGET
           WHEN   29
                MOVE  "DMKBN09"    TO  MCP-WIDGET
           WHEN   30
                MOVE  "DMKBN10"    TO  MCP-WIDGET
           WHEN   31
                MOVE  "DMKBN11"    TO  MCP-WIDGET
           WHEN   32
                MOVE  "DMKBN12"    TO  MCP-WIDGET
           WHEN   33
                MOVE  "DMKBN13"    TO  MCP-WIDGET
           WHEN   34
                MOVE  "DMKBN14"    TO  MCP-WIDGET
           WHEN   35
                MOVE  "DMKBN15"    TO  MCP-WIDGET
           WHEN   36
                MOVE  "DMKBN16"    TO  MCP-WIDGET
           WHEN   37
                MOVE  "DMKBN17"    TO  MCP-WIDGET
           WHEN   38
                MOVE  "DMKBN18"    TO  MCP-WIDGET
           WHEN   39
                MOVE  "DMKBN19"    TO  MCP-WIDGET
           WHEN   40
                MOVE  "DMKBN20"    TO  MCP-WIDGET
           WHEN   101
                MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
           WHEN   102
                MOVE  "NYUGAIBNDKBN"
                                   TO  MCP-WIDGET
           WHEN   103
                MOVE  "JYKNKBN"
                                   TO  MCP-WIDGET
           WHEN   206
                MOVE  "B06"        TO  MCP-WIDGET
           WHEN   207
                MOVE  "B07"        TO  MCP-WIDGET
           WHEN   212
                MOVE  "B12"        TO  MCP-WIDGET
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
           WHEN  ( WRK-MCP-WIDGET  >=  "DMKBN01" )
             AND ( WRK-MCP-WIDGET  <=  "DMKBN20" )
      *
               MOVE    WRK-MCP-WIDGET TO   WRK-DMKBN-WIDGET
      *
               COMPUTE WRK-LINE-MAX
                   =   SPA-GMN-PAGE    *   CONST-LINE-MAX
               COMPUTE WRK-LINE-MIN
                   =   WRK-LINE-MAX
                   - ( CONST-LINE-MAX  - WRK-DMKBNNUM )
      *
               MOVE    ZERO           TO   WRK-CURSOR
               COMPUTE IDX2    =   WRK-DMKBNNUM    -   1
      *
               PERFORM VARYING IDX1     FROM    WRK-LINE-MIN   BY  1
                       UNTIL ( IDX1     >       WRK-LINE-MAX )
                        OR   ( WRK-CURSOR  >    ZERO )
      *
                   COMPUTE IDX2    =    IDX2    +   1
      *
                   IF    ( SPA-GMN-TTEKCD  (IDX1)  NOT =   SPACE )
                       MOVE    IDX2                TO  WRK-CURSOR
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-CURSOR  =   ZERO )
                       MOVE    WRK-MAX TO  WRK-CURSOR
               ELSE
                   IF    ( WRK-MAX     <   WRK-CURSOR )
                       MOVE    WRK-MAX TO  WRK-CURSOR
                   END-IF
               END-IF
      *
               IF    ( WRK-CURSOR  <   CONST-LINE-MAX )
                   COMPUTE SPA-GMN-CUR
                     =     WRK-CURSOR  +   1
               ELSE
                   MOVE    207     TO  SPA-GMN-CUR
               END-IF
      *
               IF    ( WRK-MAX     <   WRK-DMKBNNUM )
                   COMPUTE SPA-GMN-CUR
                     =     WRK-MAX     +   1
               ELSE
                   IF    ( WRK-DMKBNNUM    <   CONST-LINE-MAX )
                       COMPUTE SPA-GMN-CUR
                           =   WRK-DMKBNNUM    +   1
                   ELSE
                       MOVE    207     TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *
           WHEN    OTHER
               EVALUATE    WRK-MCP-WIDGET
      *
               WHEN    "NYUGAIKBN"
                   MOVE    102         TO  SPA-GMN-CUR
               WHEN    "NYUGAIBNDKBN"
                   MOVE    1           TO  SPA-GMN-CUR
      *
               WHEN    "JYKNKBN"
                   MOVE    1           TO  SPA-GMN-CUR
      *
               END-EVALUATE
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
           PERFORM VARYING IDX1    FROM    1  BY  1
                   UNTIL ( IDX1 >  CONST-LINE-MAX  )
               COMPUTE WRK-CURSOR  =   CONST-LINE-MAX      +  IDX1
               IF    ( SPA-GMN-CUR =   WRK-CURSOR )
                   IF    ( X05-DMKBN-STATE (IDX1)
                                   =   WIDGET-INSENSITIVE )
                       IF    ( IDX1    =   CONST-LINE-MAX )
                           COMPUTE SPA-GMN-CUR =   207
                       ELSE
                           COMPUTE SPA-GMN-CUR =   IDX1    +   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF    ( SPA-GMN-CUR     =   101 )
               IF    ( X05-NYUGAIKBN-STATE     =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-CUR =   102
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   102 )
               IF    ( X05-NYUGAIBNDKBN-STATE  =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-CUR =   1
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   103 )
               IF    ( X05-JYKNKBN-STATE       =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-CUR =   1
               END-IF
           END-IF
      *
           .
      *
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                   TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "点数マスタにありません"
                                           TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "日・月別区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "項目に入力がありません"
                                           TO  SPA-ERRMSG
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
               MOVE    "前頁はありません"  TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "次頁はありません"  TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "チェック区分に対して入力できないコードです"
                                           TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "適用コードの登録上限数に到達しています" 
                                           TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "セットコードは入力できません"
                                           TO  SPA-ERRMSG
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
           MOVE    SPACE               TO  XERR
           MOVE    SPA-ERRCD           TO  XERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "X05"               TO  SPA-MOTOPG
           MOVE    "XERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XERR"              TO  MCP-WINDOW
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
           MOVE    WRK-XIDMSG           TO  XID1-ID1MSG
           MOVE    "戻る"               TO  XID1-B01
           MOVE    "ＯＫ"               TO  XID1-B12
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "X05"                TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    チェックマスタ３検索処理 (KEY2)
      *****************************************************************
       900-CHKTRD-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
      *    チェックマスタ読み込み
           INITIALIZE                  CHKTRD-REC
           MOVE    SPA-X01-CHKKBN      TO  CHKTRD-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHKTRD-SRYCD
      *
           IF    ( SPA-X01-SEL-STYUKYMD    =    SPACE )
               MOVE    SPA-X01-STYUKYMD        TO  CHKTRD-YUKOSTYMD
                                                   CHKTRD-YUKOEDYMD
           ELSE
               MOVE    SPA-X01-SEL-STYUKYMD    TO  CHKTRD-YUKOSTYMD
                                                   CHKTRD-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKTRD
           END-IF
      *
           .
       900-CHKTRD-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理 (KEY2)
      *****************************************************************
       900-CHKTRD-KEY2-FET-SEC         SECTION.
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               INITIALIZE                  CHKTRD-REC
               MOVE    1               TO  FLG-CHKTRD
           END-IF
      *
           .
       900-CHKTRD-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
      *    チェックマスタ読み込み
           INITIALIZE                  CHK005-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK005-CHKKBN
           MOVE    SPA-X01-ID          TO  CHK005-ID
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
           .
       900-CHK005-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY3-FET-SEC         SECTION.
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
           .
       900-CHK005-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY4-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
      *    チェックマスタ読み込み
           INITIALIZE                  CHK005-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK005-CHKKBN
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK005-REC
           ELSE
               INITIALIZE                  CHK005-REC
               MOVE    1               TO  FLG-CHK005
           END-IF
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-CHK005-KEY4-SEL-EXT.
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

