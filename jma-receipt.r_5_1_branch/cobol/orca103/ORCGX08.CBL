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
       PROGRAM-ID.                               ORCGX08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : チェックマスタ情報設定
      *  コンポーネント名  : チェックマスタ情報設定（Ｘ０８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/08/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
       01  SPA-X08.
           03  SPA-X08-AREA.
               05  SPA-GMN-PAGE        PIC 9(03).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(03).
      *
               05  SPA-GMN-AREA.
      ***          07  SPA-GMN-STYUKYMD    PIC X(09).
      ***          07  SPA-GMN-EDYUKYMD    PIC X(09).
                   07  SPA-GMN-CHKKBN-G.
                       09  SPA-GMN-CHKKBN      PIC X(01).
                       09  SPA-GMN-CHKKBNFIL   PIC X(01).
                       09  SPA-GMN-CHKKBNANME  PIC X(48).
                   07  SPA-GMN-SRYCD       PIC X(09).
                   07  SPA-GMN-NAME        PIC X(100).
      *            基準日
                   07  SPA-GMN-KIJUNYM            PIC X(07).
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBL-MAX         PIC 9(05).
                       09  SPA-GMN-TBL-OCC         OCCURS  600.
                           11  SPA-GMN-TTEKCD      PIC X(20).
                           11  SPA-GMN-TNAME       PIC X(100).
                           11  SPA-GMN-TSYORIKBN   PIC X(01).
                           11  SPA-GMN-TNGBNDKBN   PIC X(01).
                           11  SPA-GMN-TCHKERR     PIC 9(01).
                           11  SPA-GMN-TDATAKBN    PIC X(01).
                           11  SPA-GMN-TCHKERR2    PIC S9(01).
                           11  SPA-GMN-TTEKCD-BF   PIC X(20).
      *                    有効期間
                           11  SPA-GMN-TYUKOSTYMD  PIC X(10).
                           11  SPA-GMN-TYUKOEDYMD  PIC X(10).
      *
      *                    有効期間
                           11  SPA-NAI-TYUKOSTYMD  PIC X(08).
                           11  SPA-NAI-TYUKOEDYMD  PIC X(08).
      *                    初期値
                           11  SPA-MAE-TCHKERR     PIC X(01).
                           11  SPA-MAE-TNGBNDKBN   PIC X(01).
      *                    点数マスタの開始日
                           11  SPA-NAI-TNSYUKOSTYMD PIC X(08).
               05  SPA-NAI-AREA.
      *            マスター登録あり
                   07  SPA-NAI-MASTER             PIC 9(01).
      *            基準日
                   07  SPA-NAI-KIJUNYM            PIC X(06).
                   07  SPA-MAE-KIJUNYM            PIC X(06).
      *            基準日の１日から月末日
                   07  SPA-NAI-STKIJUNYMD         PIC X(08).
                   07  SPA-NAI-EDKIJUNYMD         PIC X(08).
      *            複写画面用
                   07  SPA-GMN-STKIJUNYMD         PIC X(10).
                   07  SPA-GMN-EDKIJUNYMD         PIC X(10).
      *
                   07  SPA-GMN-KIJUNYM-BF         PIC X(06).
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
      *
           03  FLG-OK                             PIC 9(01).
           03  FLG-ZENKAKU                        PIC 9(01).
           03  FLG-INPUTCD                        PIC 9(01).
           03  FLG-KANJI                          PIC 9(01).
           03  FLG-KENSAKU-SENI                   PIC 9(01).
           03  FLG-CD-TUIKA                       PIC 9(01).
           03  FLG-TEKCDDEL                       PIC 9(01).
           03  FLG-MSTDATDISP                     PIC 9(01).
           03  FLG-MASTER-OK                      PIC 9(01).
           03  FLG-INTYPE                         PIC 9(01).
           03  FLG-CHK                            PIC 9(01).
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
           03  IDX-C                             PIC 9(04).
           03  IDX-U                             PIC 9(04).
           03  IDX-STR                           PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-XIDMSG          PIC X(70). 
           03  WRK-YMD             PIC X(10). 
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
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
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
      *
           03  WRK-STYLE               PIC X(20).
           03  WRK-NUM                 PIC 9(02).
           03  WRK-RENNUM              PIC 9(03).
      *
           03  WRK-KIJUNYM.
               05  WRK-KIJ-YY          PIC 9(04).
               05  WRK-KIJ-MM          PIC 9(02).
           03  WRK-ST-YMDG             PIC X(09).
           03  WRK-ED-YMDG             PIC X(09).
           03  WRK-TNS-YUKOSETYMD      PIC X(08).
           03  WRK-CHECK               PIC X(04).
      *
       01  TBL-AREA.
           03  TBL-GMN-AREA.
               05  TBL-GMN-MAX         PIC 9(05).
               05  TBL-GMN-OCC         OCCURS  600.
                   07  TBL-GMN-TTEKCD      PIC X(20).
                   07  TBL-GMN-TNAME       PIC X(100).
                   07  TBL-GMN-TSYORIKBN   PIC X(01).
                   07  TBL-GMN-TNGBNDKBN   PIC X(01).
                   07  TBL-GMN-TCHKERR     PIC 9(01).
                   07  TBL-GMN-TDATAKBN    PIC X(01).
                   07  TBL-GMN-TCHKERR2    PIC S9(01).
                   07  TBL-MAE-TTEKCD      PIC X(20).
      *            有効期間
                   07  TBL-GMN-TYUKOSTYMD  PIC X(10).
                   07  TBL-GMN-TYUKOEDYMD  PIC X(10).
      *            有効期間
                   07  TBL-NAI-TYUKOSTYMD  PIC X(08).
                   07  TBL-NAI-TYUKOEDYMD  PIC X(08).
      *            初期値
                   07  TBL-MAE-TCHKERR     PIC X(01).
                   07  TBL-MAE-TNGBNDKBN   PIC X(01).
      *            点数マスタの開始日
                   07  TBL-NAI-TNSYUKOSTYMD PIC X(08).
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
           03  CONST-TEKCD-MAX     PIC 9(05)   VALUE   600.
           03  CONST-LINE-MAX      PIC 9(03)   VALUE   20. 
           03  CONST-PAGE-MAX      PIC 9(02)   VALUE   30.
      *
      *    チェックマスタ―
       01  HZN-CHK-REC.
           COPY    "CPCHK.INC"     REPLACING
                                       //CHK-// BY //HZN-CHK-//.
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
      *01  BYOMEI-REC.
      *    COPY    "CPBYOMEI.INC".
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
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
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
           MOVE    SPA-X01SONOTA   TO  SPA-X08 
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
           MOVE    SPA-X08         TO  SPA-X01SONOTA
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
           MOVE    "X08    "           TO  MCP-WINDOW.
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
           INITIALIZE              SPA-X08-AREA
      *
           MOVE    SPACE               TO  SPA-X01-STYUKYMDH
           MOVE    SPACE               TO  SPA-X01-EDYUKYMDH
           MOVE    ZERO                TO  SPA-X01-STYUKYMD
           MOVE    ZERO                TO  SPA-X01-EDYUKYMD
      *
           MOVE    SPA-X01-CHKKBN-G    TO  SPA-GMN-CHKKBN-G
           MOVE    SPA-X01-SRYCD       TO  SPA-GMN-SRYCD
           MOVE    SPA-X01-NAME        TO  SPA-GMN-NAME
      *    基準日の設定
           MOVE    SPA-SYSYMD(1:6)     TO  SPA-NAI-KIJUNYM
           MOVE    SPA-SYSYMDWH(1:6)   TO  SPA-GMN-KIJUNYM
           MOVE    SPA-NAI-KIJUNYM     TO  SPA-MAE-KIJUNYM
      *    基準日期間設定
           PERFORM 3102-KIJUNKIKAN-SEC
      *
      *    チェックマスタ一覧編集
           PERFORM 3101-CHK-INITHEN-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準日期間設定処理
      *****************************************************************
       3102-KIJUNKIKAN-SEC          SECTION.
      *
           MOVE    SPA-NAI-KIJUNYM     TO  SPA-NAI-STKIJUNYMD
                                                         (1:6)
           MOVE    "01"                TO  SPA-NAI-STKIJUNYMD
                                                         (7:2)
      *    月末日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-NAI-KIJUNYM     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  SPA-NAI-EDKIJUNYMD
      *    画面表示
           MOVE    SPA-NAI-STKIJUNYMD  TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-STKIJUNYMD
      *
           MOVE    SPA-NAI-EDKIJUNYMD  TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-EDKIJUNYMD
           .
       3102-KIJUNKIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ一覧編集処理
      *****************************************************************
       3101-CHK-INITHEN-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-TBL
           INITIALIZE              TBL-AREA
      *
      *    標準チェックマスタ読み込み 
           MOVE    "tbl_chk_master"    TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 3101-CHK-HENSYU-SEC
      *
      *    ユーザチェックマスタ読み込み 
           MOVE    "tbl_chk_user"      TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           PERFORM 3101-CHK-HENSYU-SEC
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
      *    標準コード編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-GMN-MAX
               IF     (TBL-GMN-TTEKCD (IDX)    NOT =   SPACE)
                  AND (TBL-GMN-TDATAKBN(IDX)   =   "1"      )
                   ADD     1                   TO  IDY
                   MOVE    TBL-GMN-OCC (IDX)   TO  SPA-GMN-TBL-OCC(IDY)
                   MOVE    SPACE               TO  TBL-GMN-TTEKCD
                                                             (IDX)
      *            適応コード毎にまとめる
                   PERFORM VARYING     IDZ     FROM    1   BY  1
                           UNTIL     ( IDZ     >   TBL-GMN-MAX )
                       IF     (TBL-GMN-TTEKCD (IDZ)    =
                                               SPA-GMN-TTEKCD (IDY))
                          AND (TBL-GMN-TDATAKBN(IDZ)   =   "2"      )
                           ADD     1               TO  IDY
                           MOVE    TBL-GMN-OCC (IDZ)
                                                   TO  SPA-GMN-TBL-OCC
                                                                (IDY)
                           MOVE    SPACE           TO  TBL-GMN-TTEKCD
                                                                 (IDZ)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *    ユーザコード編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-GMN-MAX
               IF     (TBL-GMN-TTEKCD (IDX)    NOT =   SPACE)
                  AND (TBL-GMN-TDATAKBN(IDX)   =   "2"      )
                   ADD     1                   TO  IDY
                   MOVE    TBL-GMN-OCC (IDX)   TO  SPA-GMN-TBL-OCC(IDY)
                   MOVE    SPACE               TO  TBL-GMN-TTEKCD
                                                             (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-GMN-TBL-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   CONST-TEKCD-MAX )
                         OR  ( SPA-GMN-TTEKCD (IDX)    =   SPACE)
               ADD     1               TO  SPA-GMN-TBL-MAX
      *
               IF     (IDX             =   1  )
                   OR (SPA-GMN-TTEKCD (IDX - 1)
                                   NOT =   SPA-GMN-TTEKCD (IDX))
      *            点数マスタ読み込み
                   MOVE    SPA-GMN-TTEKCD (IDX)    TO  WRK-TNS-SRYCD
                   PERFORM 3101-TENNAME-SEC
               ELSE
      *            名称複写
                   MOVE    SPA-GMN-TNAME (IDX - 1) TO  SPA-GMN-TNAME
                                                      (SPA-GMN-TBL-MAX)
               END-IF
      *
               MOVE    SPA-GMN-TTEKCD (IDX)
                                       TO  SPA-GMN-TTEKCD-BF
                                                  (SPA-GMN-TBL-MAX)
           END-PERFORM
           PERFORM 3101-CHKERR2-SET-SEC
      *    マスター登録あり
           MOVE    ZERO                TO  SPA-NAI-MASTER
           IF     (SPA-GMN-TBL-MAX     >   ZERO )
               IF     (SPA-GMN-TDATAKBN(1) =   "1"  )
                   MOVE    1                   TO  SPA-NAI-MASTER
               ELSE
                   MOVE    2                   TO  SPA-NAI-MASTER
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-PAGE
      *
           .
       3101-CHK-INITHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ編集処理
      *****************************************************************
       3101-CHK-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                  CHK-REC
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-NAI-STKIJUNYMD  TO  CHK-YUKOSTYMD
           MOVE    SPA-NAI-STKIJUNYMD  TO  CHK-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           PERFORM UNTIL ( FLG-CHKMST      =   1 )
                    OR   ( TBL-GMN-MAX     >=  CONST-TEKCD-MAX)
      *
               ADD     1               TO  TBL-GMN-MAX
      *
               MOVE    CHK-CD          TO  TBL-GMN-TTEKCD
                                                  (TBL-GMN-MAX)
               MOVE    CHK-SYORIKBN    TO  TBL-GMN-TSYORIKBN
                                                  (TBL-GMN-MAX)
               MOVE    CHK-NYUGAIBNDKBN
                                       TO  TBL-GMN-TNGBNDKBN
                                                  (TBL-GMN-MAX)
               MOVE    CHK-CHKERR      TO  TBL-GMN-TCHKERR
                                                  (TBL-GMN-MAX)
               MOVE    CHK-DATAKBN     TO  TBL-GMN-TDATAKBN
                                                  (TBL-GMN-MAX)
               MOVE    CHK-YUKOSTYMD   TO  TBL-NAI-TYUKOSTYMD
                                                  (TBL-GMN-MAX)
               MOVE    CHK-YUKOEDYMD   TO  TBL-NAI-TYUKOEDYMD
                                                  (TBL-GMN-MAX)
      *        和暦
               MOVE    CHK-YUKOSTYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  TBL-GMN-TYUKOSTYMD
                                                  (TBL-GMN-MAX)
               MOVE    CHK-YUKOEDYMD   TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  TBL-GMN-TYUKOEDYMD
                                                  (TBL-GMN-MAX)
      *        初期値保存
               MOVE    CHK-NYUGAIBNDKBN
                                       TO  TBL-MAE-TNGBNDKBN
                                                  (TBL-GMN-MAX)
               MOVE    CHK-CHKERR      TO  TBL-MAE-TCHKERR
                                                  (TBL-GMN-MAX)
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-CHKMST-FETCH-SEC
      *
           END-PERFORM
      *    カーソルクロース
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-CHK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       3101-TENNAME-SEC          SECTION.
      *
      *****MOVE    CHK-CD              TO  WRK-TNS-SRYCD
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
      *******  IF      (SPA-X01-KJNYMD     <=  TNS-YUKOEDYMD )
      *        基準日で有効のマスタ
               IF      (SPA-NAI-STKIJUNYMD <=  TNS-YUKOEDYMD )
                   MOVE    WRK-TNS-NAME    TO  SPA-GMN-TNAME
                                                      (SPA-GMN-TBL-MAX)
                   MOVE    1               TO  FLG-TENSU
                   MOVE    1               TO  FLG-OK
               ELSE
                   MOVE    WRK-TNS-NAME    TO  WRK-NAME
               END-IF
      *
               MOVE    TNS-YUKOSTYMD   TO  SPA-NAI-TNSYUKOSTYMD
                                                      (SPA-GMN-TBL-MAX)
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
      *    編集開始位置
           MOVE    IDX7                TO  IDX-STR
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
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  320-X98SPASET-EXT
           END-IF
      *
      *    有効開始日へ
           MOVE    IDX-STR             TO  IDX-ERR
           MOVE    20                  TO  WRK-BASEPOS
           PERFORM 4101-ERRCUR-POS-SEC
      *
      *
           .
       320-X98SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-XID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-XIDCD
           WHEN    "1001"
               IF      SPA-XID1-FLG        =   "OK"
                   PERFORM 490-KOUSHIN-SEC
               END-IF
           WHEN    "1002"
               IF      SPA-XID1-FLG        =   "OK"
                   PERFORM 4031-DEL-SEC
               END-IF
           WHEN    "2001"
               IF      SPA-XID1-FLG        =   "OK"
                   PERFORM 4111-PRINT-SEC
               END-IF
      *    前月処理
           WHEN    "3001"
               IF      SPA-XID1-FLG        =   "OK"
      *            更新処理
                   PERFORM 4901-KOUSHIN-SEC
                   IF    ( SPA-ERRCD      =   SPACE )
      *                前月編集処理
                       PERFORM 4402-MAE-HENSYU-SYORI-SEC
                   END-IF
               ELSE
      *            前月編集処理
                   PERFORM 4402-MAE-HENSYU-SYORI-SEC
               END-IF
      *    次月処理
           WHEN    "3002"
               IF      SPA-XID1-FLG        =   "OK"
      *            更新処理
                   PERFORM 4901-KOUSHIN-SEC
                   IF    ( SPA-ERRCD      =   SPACE )
      *                次月編集処理
                       PERFORM 4502-ATO-HENSYU-SYORI-SEC
                   END-IF
               ELSE
      *            次月編集処理
                   PERFORM 4502-ATO-HENSYU-SYORI-SEC
               END-IF
      *    基準月変更処理
           WHEN    "3003"
               IF      SPA-XID1-FLG        =   "OK"
      *            更新処理
                   PERFORM 4901-KOUSHIN-SEC
                   IF    ( SPA-ERRCD      =   SPACE )
      *                基準月変更編集
                       PERFORM 41012-KIJUNCHG-SYORI-SEC
                   END-IF
               ELSE
      *            基準月変更編集
                   PERFORM 41012-KIJUNCHG-SYORI-SEC
               END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-XID1-FLG
           MOVE    SPACE           TO  SPA-XIDCD
           IF      SPA-ERRCD   NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  100-INIT-EXT
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
      *!!!
               MOVE    SPA-X03-NAI-TLCHK-YUKOSTYMD (IDX5)
                                   TO  SPA-GMN-TYUKOSTYMD(IDX5)
               MOVE    SPA-X03-NAI-TLCHK-YUKOEDYMD (IDX5)
                                   TO  SPA-GMN-TYUKOEDYMD(IDX5)
               MOVE    SPA-X03-NAI-TLCHK-NGBNDKBN (IDX5)
                                   TO  SPA-MAE-TNGBNDKBN (IDX5)
               MOVE    SPA-X03-NAI-TLCHK-CHKERR (IDX5)
                                   TO  SPA-MAE-TCHKERR (IDX5)
      *
               MOVE    IDX5            TO  IDX-LINE
      *        点数マスタ検索
               PERFORM 41013-TENSU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 3101-TENNAME-SEC
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
      *        開始日
               MOVE    SPACE           TO  SPA-ERRCD
               PERFORM 4101-YUKSTYMD-CHK-SEC
      *        終了日
               MOVE    SPACE           TO  SPA-ERRCD
               PERFORM 4101-YUKEDYMD-CHK-SEC
      *
               MOVE    SPA-GMN-TTEKCD (IDX5)
                                       TO  SPA-GMN-TTEKCD-BF
                                                  (IDX5)
      *
           END-PERFORM
      *
      **   PERFORM 3101-CHKERR2-SET-SEC
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
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
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
      *        前月
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-MAE-TUKI-SEC
      *        次月
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-ATO-TUKI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    基準月
           WHEN    "ACTIVATE"       ALSO    "KIJUNYM"
               PERFORM  4101-KIJUNYM-CHK-SEC
      *    一括入力分
           WHEN    "ACTIVATE"       ALSO    ANY
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
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
      *    基準日チェック処理
      *****************************************************************
       4101-KIJUNYM-CHK-SEC       SECTION.
      *
      *    変更あり
           MOVE    SPA-GMN-KIJUNYM     TO  SPA-GMN-KIJUNYM-BF
      *
           MOVE    X08-KIJUNYM         TO  SPA-GMN-KIJUNYM
      *
           MOVE    SPA-GMN-KIJUNYM     TO  WRK-YMD(1:6)
           MOVE    2                   TO  FLG-INTYPE
           PERFORM 5002-HIZUKE-CHK-SEC
           IF     (SPA-ERRCD            =   SPACE )
               IF     (WRK-SYMD             =   ZERO
                                            OR  ALL "9" )
                   MOVE    "0007"           TO  SPA-ERRCD
               END-IF
           END-IF
           IF     (SPA-ERRCD            =   SPACE )
               MOVE    WRK-SYMD(1:6)        TO  SPA-NAI-KIJUNYM
               MOVE    WRK-HENYMDG(1:6)     TO  SPA-GMN-KIJUNYM
           ELSE
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    ZERO            TO  SPA-NAI-KIJUNYM
               MOVE    200             TO  SPA-GMN-CUR
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-NAI-KIJUNYM NOT =   SPA-MAE-KIJUNYM)
      *        更新前チェック
               PERFORM 410-KAKUTEI-SEC
               IF      SPA-ERRCD       NOT =   SPACE
      *            エラー時元の基準日へ
                   MOVE    SPA-GMN-KIJUNYM-BF  TO  SPA-GMN-KIJUNYM
                   MOVE    SPA-MAE-KIJUNYM     TO  SPA-NAI-KIJUNYM
                   GO      TO   4101-KIJUNYM-CHK-EXT
               END-IF
               MOVE    SPACE               TO  SPA-XIDCD
           END-IF
      *
      *    変更チェック
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-NAI-KIJUNYM NOT =   SPA-MAE-KIJUNYM)
               PERFORM 4401-HENKOU-CHECK-SEC
               IF      WRK-CHECK       NOT =   SPACE
                   MOVE    "3003"              TO  SPA-XIDCD
                   GO      TO   4101-KIJUNYM-CHK-EXT
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        基準月変更編集
               PERFORM 41012-KIJUNCHG-SYORI-SEC
           END-IF
      *
           .
       4101-KIJUNYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基準月変更編集処理
      *****************************************************************
       41012-KIJUNCHG-SYORI-SEC       SECTION.
      *
           MOVE    SPA-NAI-KIJUNYM     TO  SPA-MAE-KIJUNYM
      *    基準日期間設定
           PERFORM 3102-KIJUNKIKAN-SEC
      *    チェックマスタ一覧編集
           PERFORM 3101-CHK-INITHEN-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       41012-KIJUNCHG-SYORI-EXT.
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
                   MOVE    X08-TEKCD (IDX)     TO  SPA-GMN-TTEKCD
                                                                 (IDZ)
                   MOVE    X08-YUKOSTYMD (IDX) TO  SPA-GMN-TYUKOSTYMD
                                                                 (IDZ)
                   MOVE    X08-YUKOEDYMD (IDX) TO  SPA-GMN-TYUKOEDYMD
                                                                 (IDZ)
               END-IF
      *
               IF    ( X08-NGBNDKBN (IDX)      =   ZERO )
                  MOVE    SPACE                TO  SPA-GMN-TNGBNDKBN
                                                                 (IDZ)
               ELSE
                  MOVE    X08-NGBNDKBN (IDX)   TO  SPA-GMN-TNGBNDKBN
                                                                 (IDZ)
               END-IF
      *
               MOVE    X08-CHKERR   (IDX)      TO  SPA-GMN-TCHKERR
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
      *            開始日
                   PERFORM 4101-YUKSTYMD-CHK-SEC
               END-IF
      *
               IF    ( SPA-ERRCD   =   SPACE )
                AND  ( FLG-END     =   ZERO  )
      *            終了日
                   PERFORM 4101-YUKEDYMD-CHK-SEC
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
           MOVE    "X08"               TO  SPA-MOTOPG
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
       4101-TEKCD-CHK-SEC              SECTION.
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
           MOVE    SPACE               TO  WRK-TNS-YUKOSETYMD
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
           IF     (WRK-CDSYUBETU       =   SPACE )
              AND (WRK-MOJI            =   9     )
              AND (FLG-KANJI           =   ZERO  )
      *        診療コード
               MOVE   WRK-SRYCD        TO  WRK-TNS-SRYCD
               PERFORM 900-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   1 )
                   MOVE    "0001"      TO  SPA-ERRCD
                   MOVE    IDX-LINE    TO  IDX-ERR
                   GO  TO  41013-TENSU-CHK-EXT
               END-IF
           ELSE
      *
               INITIALIZE              INPUTCD-REC
               MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
               MOVE    WRK-CDSYUBETU   TO  ICD-CDSYU
               MOVE    WRK-SRYCD       TO  ICD-INPUTCD
      *
      *        入力コードマスタ−読み込み処理
               PERFORM 900-INPUTCD-SEL-KEY2-SEC
      *
               IF  (   FLG-INPUTCD       =   ZERO )
                   MOVE    ICD-SRYCD        TO  WRK-TNS-SRYCD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU       =   1 )
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
           MOVE    TNS-YUKOSTYMD       TO  SPA-NAI-TNSYUKOSTYMD
                                                      (IDX-LINE)
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
                   WHEN    9
      *                診療コード
                       MOVE    SPACE   TO  WRK-CDSYUBETU
      *
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
                AND  ((SPA-GMN-TDATAKBN(IDX-LINE)
                                       =   SPA-GMN-TDATAKBN(IDX6)) OR
                      (SPA-GMN-TDATAKBN(IDX-LINE)  =   SPACE   ) OR
                      (SPA-GMN-TDATAKBN(IDX6)      =   SPACE   ))
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
               MOVE    IDX-LINE        TO  IDX-ERR
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-GMN-TDATAKBN (IDX-LINE)  =   "1" )
              IF       SPA-GMN-TNGBNDKBN (IDX-LINE)
                                       NOT =   SPA-MAE-TNGBNDKBN
                                                       (IDX-LINE)
      *            標準で変更があった時ユーザに登録がないこと
                   PERFORM 41012-USER-CD-CHK-SEC
                END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
      ****     MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    60              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-NGBNDKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ユーザ登録チェック処理
      *****************************************************************
       41012-USER-CD-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-TBL-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE   )
               IF      SPA-GMN-TDATAKBN (IDX)  =   "2"
                   IF      SPA-GMN-TTEKCD (IDX)
                                       =   SPA-GMN-TTEKCD(IDX-LINE)
                       MOVE    SPA-MAE-TNGBNDKBN (IDX-LINE)
                                               TO  SPA-GMN-TNGBNDKBN
                                                           (IDX-LINE)
                       MOVE    IDX             TO  IDX-ERR
                       MOVE    "0009"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       41012-USER-CD-CHK-EXT.
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
               MOVE    "0006"          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-GMN-TDATAKBN (IDX-LINE)  =   "1" )
              IF       SPA-GMN-TCHKERR (IDX-LINE)
                                       NOT =   SPA-MAE-TCHKERR
                                                       (IDX-LINE)
      *            標準で変更があった時ユーザに登録がないこと
                   PERFORM 41012-USER-CD-CHK-SEC
                END-IF
           END-IF
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    80              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
      *
           .
       4101-CHKERR-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始日チェック処理
      *****************************************************************
       4101-YUKSTYMD-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-TYUKOSTYMD (IDX-LINE) =   SPACE
      *        点数マスタの開始日
               MOVE    SPA-NAI-TNSYUKOSTYMD(IDX-LINE)
                                           TO  SPA-GMN-TYUKOSTYMD
                                                    (IDX-LINE)
           END-IF
      *
           MOVE    SPA-GMN-TYUKOSTYMD(IDX-LINE)
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD            =   SPACE
               MOVE    WRK-SYMD             TO  SPA-NAI-TYUKOSTYMD
                                                        (IDX-LINE)
               MOVE    WRK-HENYMDG          TO  SPA-GMN-TYUKOSTYMD
                                                        (IDX-LINE)
      *        １日であること
               IF     (WRK-SYMD        NOT =   ZERO )
                  AND (WRK-SDD         NOT =   01   )
                   MOVE    "0017"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    ZERO               TO  SPA-NAI-TYUKOSTYMD
                                                        (IDX-LINE)
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    20              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
           .
       4101-YUKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了日チェック処理
      *****************************************************************
       4101-YUKEDYMD-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-TYUKOEDYMD (IDX-LINE) =   SPACE
               IF     (SPA-GMN-TYUKOSTYMD(IDX-LINE) =   SPACE)
                   GO      TO      4101-YUKEDYMD-CHK-EXT
               END-IF
      *        終了日の設定
               MOVE    "99999999"          TO  SPA-GMN-TYUKOEDYMD
                                                        (IDX-LINE)
           END-IF
      *
           MOVE    SPA-GMN-TYUKOEDYMD (IDX-LINE)
                                       TO  WRK-YMD
           PERFORM 5002-HIZUKE-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-SYMD            TO  SPA-NAI-TYUKOEDYMD
                                                        (IDX-LINE)
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TYUKOEDYMD
                                                        (IDX-LINE)
      *        月末日であること
               IF     (WRK-SYMD        NOT =   "99999999" )
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY7-AREA
                   MOVE    "71"                TO  LNK-DAY7-IRAI
                   MOVE    WRK-SYMD(1:6)       TO  LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY7-AREA
                   IF      LNK-DAY7-KOYOMI     NOT =   WRK-SYMD
                       MOVE    "0018"          TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    ZERO               TO  SPA-NAI-TYUKOEDYMD
                                                        (IDX-LINE)
               MOVE    "0003"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-TYUKOSTYMD (IDX-LINE)
                                       >   SPA-NAI-TYUKOEDYMD
                                                    (IDX-LINE)
                   MOVE    "0019"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    IDX-LINE        TO  IDX-ERR
               MOVE    40              TO  WRK-BASEPOS
               PERFORM 4101-ERRCUR-POS-SEC
           END-IF
           .
       4101-YUKEDYMD-CHK-EXT.
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
               MOVE    "0015"             TO  SPA-ERRCD
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
               MOVE    "0016"          TO  SPA-ERRCD
               GO  TO  440-ATOGMN-EXT
           ELSE
               COMPUTE IDX     =   SPA-GMN-PAGE   *   CONST-LINE-MAX
               IF      SPA-GMN-TTEKCD (IDX)   =   SPACE
                   MOVE    "0016"      TO  SPA-ERRCD
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
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC                               SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    1               TO  SPA-GMN-PAGE
           .
       402-CLEAR-EXT.
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
           MOVE    SPA-NAI-STKIJUNYMD  TO  CHK-YUKOSTYMD
           MOVE    SPA-NAI-EDKIJUNYMD  TO  CHK-YUKOEDYMD
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
      **** MOVE    "tbl_chk"           TO  MCP-TABLE
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
      *    前月処理
      *****************************************************************
       440-MAE-TUKI-SEC                SECTION.
      *
      *    更新前チェック
           PERFORM 410-KAKUTEI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO   440-MAE-TUKI-EXT
           END-IF
           MOVE    SPACE               TO  SPA-XIDCD
      *
      *    変更チェック
           PERFORM 4401-HENKOU-CHECK-SEC
           IF      WRK-CHECK       NOT =   SPACE
               MOVE    "3001"              TO  SPA-XIDCD
               GO      TO   440-MAE-TUKI-EXT
           END-IF
      *
      *    前月編集処理
           PERFORM 4402-MAE-HENSYU-SYORI-SEC
      *
           .
       440-MAE-TUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更チェック処理
      *****************************************************************
       4401-HENKOU-CHECK-SEC         SECTION.
      *
           INITIALIZE              TBL-AREA
      *
      *    標準チェックマスタ読み込み 
           MOVE    "tbl_chk_master"    TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
           PERFORM 3101-CHK-HENSYU-SEC
      *
      *    ユーザチェックマスタ読み込み 
           MOVE    "tbl_chk_user"      TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           PERFORM 3101-CHK-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-CHECK 
           IF     (SPA-GMN-TBL-MAX     NOT =   TBL-GMN-MAX)
               MOVE    "ERR"               TO  WRK-CHECK
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-TBL-MAX )
                          OR  (WRK-CHECK   NOT =   SPACE)
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   TBL-GMN-MAX )
                          OR  (WRK-CHECK   NOT =   SPACE)
                          OR  (FLG-CHK     NOT =   ZERO )
                   IF     (TBL-GMN-TTEKCD (IDY)
                                           =   SPA-GMN-TTEKCD(IDX))
                      AND (TBL-GMN-TDATAKBN(IDY)
                                           =   SPA-GMN-TDATAKBN(IDX))
                       IF      (TBL-NAI-TYUKOSTYMD(IDY)
                                           =   SPA-NAI-TYUKOSTYMD
                                                               (IDX))
                           AND (TBL-NAI-TYUKOEDYMD(IDY)
                                           =   SPA-NAI-TYUKOEDYMD
                                                               (IDX))
                           AND (TBL-GMN-TNGBNDKBN(IDY)
                                           =   SPA-GMN-TNGBNDKBN
                                                               (IDX))
                           AND (TBL-GMN-TCHKERR(IDY)
                                           =   SPA-GMN-TCHKERR
                                                               (IDX))
                           MOVE    1           TO  FLG-CHK
                       ELSE
                           MOVE    2           TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
               IF      FLG-CHK         =   ZERO
      *            新規追加
                   MOVE    "ERR"               TO  WRK-CHECK
               END-IF
               IF      FLG-CHK         =   2
      *            変更
                   MOVE    "ERR"               TO  WRK-CHECK
               END-IF
           END-PERFORM
      *
           .
       4401-HENKOU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月編集処理
      *****************************************************************
       4402-MAE-HENSYU-SYORI-SEC         SECTION.
      *
           MOVE    SPA-NAI-KIJUNYM     TO  WRK-KIJUNYM
           IF      WRK-KIJ-MM          =   01
               COMPUTE WRK-KIJ-YY      =   WRK-KIJ-YY
                                       -   1
               MOVE    12              TO  WRK-KIJ-MM
           ELSE
               COMPUTE WRK-KIJ-MM      =   WRK-KIJ-MM
                                       -   1
           END-IF
      *    H02まで
           IF      WRK-KIJ-YY          <   "1989"
               CONTINUE
           ELSE
               MOVE    WRK-KIJUNYM         TO  SPA-NAI-KIJUNYM
      *
               MOVE    SPA-NAI-KIJUNYM     TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-KIJUNYM
      *
      *        基準月変更編集
               PERFORM 41012-KIJUNCHG-SYORI-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       4402-MAE-HENSYU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次月処理
      *****************************************************************
       450-ATO-TUKI-SEC                SECTION.
      *
      *    更新前チェック
           PERFORM 410-KAKUTEI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO   450-ATO-TUKI-EXT
           END-IF
           MOVE    SPACE               TO  SPA-XIDCD
      *
      *    変更チェック
           PERFORM 4401-HENKOU-CHECK-SEC
           IF      WRK-CHECK       NOT =   SPACE
               MOVE    "3002"              TO  SPA-XIDCD
               GO      TO   450-ATO-TUKI-EXT
           END-IF
      *
      *    次月編集処理
           PERFORM 4502-ATO-HENSYU-SYORI-SEC
      *
           .
       450-ATO-TUKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月編集処理
      *****************************************************************
       4502-ATO-HENSYU-SYORI-SEC       SECTION.
      *
           MOVE    SPA-NAI-KIJUNYM     TO  WRK-KIJUNYM
           IF      WRK-KIJ-MM          =   12
               COMPUTE WRK-KIJ-YY      =   WRK-KIJ-YY
                                       +   1
               MOVE    01              TO  WRK-KIJ-MM
           ELSE
               COMPUTE WRK-KIJ-MM      =   WRK-KIJ-MM
                                       +   1
           END-IF
      *
           IF      WRK-KIJ-YY          =   9999
               CONTINUE
           ELSE
               MOVE    WRK-KIJUNYM         TO  SPA-NAI-KIJUNYM
      *
               MOVE    SPA-NAI-KIJUNYM     TO  WRK-SYMD(1:6)
               MOVE    "01"                TO  WRK-SYMD(7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-KIJUNYM
      *        基準月変更編集
               PERFORM 41012-KIJUNCHG-SYORI-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       4502-ATO-HENSYU-SYORI-EXT.
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
      *        基準月の１日を基準日
               MOVE    SPA-NAI-STKIJUNYMD  TO  SPA-X01-KJNYMD
      *        チェック項目複写画面初期処理
               PERFORM 4091-X03-INIT-SEC
      * 
               MOVE    "X03"               TO  SPA-SAKIPG
               MOVE    "X08"               TO  SPA-MOTOPG
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
                                       SPA-X03-NAI-AREA
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  SPA-X03-GMN-LCHKKBNMEI
      *    期間
           MOVE    SPA-GMN-STKIJUNYMD  TO  SPA-X03-GMN-LYUKOSTYMD
           MOVE    "99999999"          TO  SPA-X03-GMN-LYUKOEDYMD
      **
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
      *!!!
               MOVE    SPA-NAI-TYUKOSTYMD(IDX4)
                                       TO  SPA-X03-NAI-TLCHK-YUKOSTYMD
                                                                  (IDX4)
               MOVE    SPA-NAI-TYUKOEDYMD(IDX4)
                                       TO  SPA-X03-NAI-TLCHK-YUKOEDYMD
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
      *    基準月の１日を基準日
           MOVE    SPA-NAI-STKIJUNYMD  TO  SPA-X01-KJNYMD
      *    画面編集
           PERFORM 41011-X98-SET-SEC
      *
           MOVE    "X08"               TO  SPA-MOTOPG
           MOVE    "X98"               TO  SPA-SAKIPG
           MOVE    "X08"               TO  SPA-X98-MOTOPG
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
           MOVE    SPA-X08         TO  SPA-X01SONOTA
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
           MOVE    SPA-X01SONOTA   TO  SPA-X08
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
      *****    MOVE    SPA-X01-KJNYMD      TO  HXLST-KJNYMD
               MOVE    SPA-NAI-STKIJUNYMD  TO  HXLST-KJNYMD
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
      *    関連チェック
           PERFORM 420-KANRENCHK-SEC
           IF      SPA-ERRCD        =   SPACE
               MOVE    "1001"           TO  SPA-XIDCD
           END-IF
           .
       410-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *       入力項目の関連チェック処理
      *****************************************************************
       420-KANRENCHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-TBL-MAX =   ZERO )
               MOVE    "0004"      TO  SPA-ERRCD
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL ( IDX         >    SPA-GMN-TBL-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-TYUKOSTYMD(IDX)  =   SPACE
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    20              TO  WRK-BASEPOS
               END-IF
               IF      SPA-GMN-TYUKOEDYMD(IDX) =   SPACE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0003"          TO  SPA-ERRCD
                       MOVE    40              TO  WRK-BASEPOS
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    ZERO                TO  FLG-MASTER-OK
                   IF      SPA-GMN-TDATAKBN (IDX)  =   "1"
      *                標準で変更があった時ユーザー登録
                       PERFORM 49012-MASTER-CHK-SEC
                   END-IF
                   IF      FLG-MASTER-OK       =   ZERO
      *                ユーザテーブルの登録内容チェック
                       PERFORM 4201-USERTBL-CHK-SEC
                       IF      SPA-ERRCD       NOT =   SPACE
                           MOVE    20              TO  WRK-BASEPOS
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD   NOT =   SPACE
                   MOVE    IDX             TO  IDX-ERR
                   PERFORM 4101-ERRCUR-POS-SEC
               END-IF
           END-PERFORM
      *
           .
       420-KANRENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザテーブルの登録内容チェック処理
      *****************************************************************
       4201-USERTBL-CHK-SEC           SECTION.
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-GMN-TTEKCD (IDX)
                                       TO  CHK-CD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
               MOVE    ZERO            TO  FLG-CHKMST
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
           PERFORM UNTIL    ( FLG-CHKMST   =   1   )
                        OR  ( SPA-ERRCD    NOT =   SPACE )
      *        重複期間チェック
               IF     (CHK-YUKOSTYMD   <=  SPA-NAI-STKIJUNYMD)
                  AND (CHK-YUKOEDYMD   >=  SPA-NAI-EDKIJUNYMD)
      *            基準月対象はチェック対象外
                   CONTINUE
               ELSE
                   IF      (CHK-YUKOEDYMD  <   SPA-NAI-TYUKOSTYMD
                                                        (IDX))
                        OR (CHK-YUKOSTYMD  >   SPA-NAI-TYUKOEDYMD
                                                        (IDX))
                        CONTINUE
                   ELSE
                       PERFORM 4211-ERRMSG-HEN-SEC
                       MOVE    "0020"          TO  SPA-ERRCD
                   END-IF
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "tbl_chk_user"      TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 900-CHKMST-FETCH-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4201-USERTBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録日付編集処理
      *****************************************************************
       4211-ERRMSG-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-ERR-NAME
      *    和暦
           MOVE    CHK-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-ST-YMDG
      *
           MOVE    CHK-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-ED-YMDG
      *
           STRING  "【"
                   WRK-ST-YMDG
                   "〜"
                   WRK-ED-YMDG
                  " 】"                DELIMITED  BY  SIZE
                                       INTO  WRK-ERR-NAME
           END-STRING
           .
       4211-ERRMSG-HEN-EXT.
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
      *    チェックマスタ削除 （ユーザ）
           PERFORM 40311-DEL-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    "1003"      TO  SPA-ERRCD
               GO  TO  4901-KOUSHIN-EXT
           END-IF
      *
      *    処理日付
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL ( IDX         >   SPA-GMN-TBL-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    ZERO            TO  FLG-MASTER-OK
      *        標準データの内容変更チェック
               IF      SPA-GMN-TDATAKBN (IDX)  =   "1"
                   PERFORM 49012-MASTER-CHK-SEC
               END-IF
      *
      *        マスター内容と違う場合、ユーザー登録する
               IF     FLG-MASTER-OK    =   ZERO
                   PERFORM 49012-CHEK-USER-INS-SEC
               END-IF
           END-PERFORM
      *
           .
       4901-KOUSHIN-EXT.
           EXIT.
      *****************************************************************
      *    ユーザーチェックマスタ登録処理
      *****************************************************************
       49012-CHEK-USER-INS-SEC           SECTION.
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    SPA-X01-CHKKBN      TO  CHK-CHKKBN
           MOVE    SPA-X01-SRYCD       TO  CHK-SRYCD
           MOVE    "2"                 TO  CHK-CDKBN
      *
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  CHK-RENNUM
      *
           MOVE    SPA-NAI-TYUKOSTYMD(IDX)
                                       TO  CHK-YUKOSTYMD
           MOVE    SPA-NAI-TYUKOEDYMD(IDX)
                                       TO  CHK-YUKOEDYMD
           MOVE    SPA-GMN-TTEKCD (IDX)
                                       TO  CHK-CD
           MOVE    "2"                 TO  CHK-DATAKBN
      *
           MOVE    SPA-GMN-TSYORIKBN (IDX)
                                       TO  CHK-SYORIKBN
           MOVE    SPACE               TO  CHK-NYUGAIKBN
           MOVE    SPA-GMN-TNGBNDKBN (IDX)
                                       TO  CHK-NYUGAIBNDKBN
           MOVE    SPA-GMN-TCHKERR (IDX)
                                       TO  CHK-CHKERR
      *
           MOVE    SMCNDATE-YMD        TO  CHK-CREYMD
                                           CHK-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHK-UPHMS
      *
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk_user"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "1001"      TO  SPA-ERRCD
           END-IF
           .
       49012-CHEK-USER-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *     マスターの変更チェック処理
      *****************************************************************
       49012-MASTER-CHK-SEC           SECTION.
      *
           IF     (SPA-GMN-TCHKERR (IDX)   =   SPA-MAE-TCHKERR(IDX))
              AND (SPA-GMN-TNGBNDKBN(IDX)  =   SPA-MAE-TNGBNDKBN(IDX))
      *        提供内容の変更なし
               MOVE    1                   TO  FLG-MASTER-OK
           ELSE
               MOVE    ZERO                TO  FLG-MASTER-OK
           END-IF
      *
           .
       49012-MASTER-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
           WHEN  ZERO
               MOVE  ZERO              TO  WRK-SYMD
               MOVE  WRK-YMD(1:8)      TO  WRK-HENYMDG
           WHEN  "99999999"
               MOVE  ALL "9"           TO  WRK-SYMD
               MOVE  WRK-YMD(1:8)      TO  WRK-HENYMDG
           WHEN    OTHER
      *        日付画面チェックサブ
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
      *        年月入力
               IF      FLG-INTYPE      =   2
                   MOVE    "2"             TO  SGDAY-INTYPE
               END-IF
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF    ( SGDAY-RC        =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    "0002"      TO  SPA-ERRCD
               END-IF
           END-EVALUATE
           MOVE    ZERO                TO  FLG-INTYPE
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC             SECTION.
      *
           IF    ( WRK-SYMD        =   ALL "9"  OR   ZERO )
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
           MOVE    "X08    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  X08
           INITIALIZE                      X08
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
               MOVE    IDX1                    TO  X08-NO      (IDX2)
               MOVE    SPA-GMN-TTEKCD  (IDX1)  TO  X08-TEKCD   (IDX2)
               MOVE    SPA-GMN-TNAME   (IDX1)  TO  X08-DSPNAME (IDX2)
               MOVE    SPA-GMN-TYUKOSTYMD(IDX1)
                                               TO  X08-YUKOSTYMD(IDX2)
               MOVE    SPA-GMN-TYUKOEDYMD(IDX1)
                                               TO  X08-YUKOEDYMD(IDX2)
               IF    ( SPA-GMN-TNGBNDKBN(IDX1) =   SPACE )
                   MOVE    ZERO                TO  X08-NGBNDKBN(IDX2)
               ELSE
                   MOVE    SPA-GMN-TNGBNDKBN(IDX1)
                                               TO  X08-NGBNDKBN(IDX2)
               END-IF
               MOVE    SPA-GMN-TCHKERR  (IDX1) TO  X08-CHKERR  (IDX2)
      *
               IF    ( SPA-GMN-TTEKCD  (IDX1)  NOT =   SPACE )
                   MOVE    IDX2                TO  WRK-MAX
               END-IF
      *
               IF    ( SPA-X01-CHKKBN          =   "5" OR "A" OR "B" )
                   EVALUATE    SPA-GMN-TCHKERR2 (IDX1)
                   WHEN    -1
                       MOVE    "-"         TO  X08-LBLCHKERR(IDX2)
                   WHEN    SPA-GMN-TCHKERR (IDX1)
                       MOVE    SPACE       TO  X08-LBLCHKERR(IDX2)
                   WHEN    OTHER
                       MOVE    SPA-GMN-TCHKERR2 (IDX1)
                                           TO  X08-LBLCHKERR(IDX2)
                   END-EVALUATE
               END-IF
      *
               MOVE    "black"             TO  WRK-STYLE
               IF    ( SPA-GMN-TDATAKBN  (IDX1)    =   "1" )
                   MOVE    "blue"          TO  WRK-STYLE
                   MOVE    1               TO  FLG-MSTDATDISP
               END-IF
               IF    ( SPA-GMN-TDATAKBN  (IDX1)    =   "2" )
                   MOVE    "red"           TO  WRK-STYLE
               END-IF
      *
               MOVE    WRK-STYLE           TO  X08-NO-STYLE      (IDX2)
                                               X08-TEKCD-STYLE   (IDX2)
                                               X08-DSPNAME-STYLE (IDX2)
                                               X08-YUKOSTYMD-STYLE(IDX2)
                                               X08-YUKOEDYMD-STYLE(IDX2)
                                               X08-NGBNDKBN-STYLE(IDX2)
                                               X08-CHKERR-STYLE  (IDX2)
                                               X08-LBLCHKERR-STYLE
                                                                 (IDX2)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-CHKKBN-G    TO  X08-CHKKBN
           MOVE    SPA-GMN-SRYCD       TO  X08-SRYCD
           MOVE    SPA-GMN-NAME        TO  X08-NAME
           MOVE    SPA-GMN-KIJUNYM     TO  X08-KIJUNYM
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
           MOVE    "black"             TO  X08-LBLCAUTION-STYLE
           IF    ( FLG-MSTDATDISP      =   1 )
               MOVE    "blue"          TO  X08-LBLCAUTION-STYLE
               STRING  "※マスタ提供適用コードは削除できません。"
                       DELIMITED   BY  SIZE
                       "入外、エラー区分の変更は可能です。"
                       DELIMITED   BY  SIZE
               INTO    X08-LBLCAUTION
               END-STRING
           END-IF
      *
           INITIALIZE                  WRK-PAGE-DSP-G
           MOVE    SPA-GMN-PAGE        TO  WRK-PAGE-DSP-ACTIVE
           MOVE    "/"                 TO  WRK-PAGE-DSP-LBL
           MOVE    WRK-PAGE-MAX        TO  WRK-PAGE-DSP-MAX
           MOVE    WRK-PAGE-DSP-G      TO  X08-LBLPAGE
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
           MOVE    WIDGET-NORMAL           TO  X08-B03-STATE
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LINE-MAX )
      *
               MOVE    WIDGET-NORMAL       TO  X08-NGBNDKBN-STATE (IDX0)
                                               X08-CHKERR-STATE   (IDX0)
      *
           END-PERFORM
      *
           IF    ( SPA-X01-CHKKBN          =   "4" )
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-LINE-MAX )
      *
                   MOVE    WIDGET-INSENSITIVE
                                           TO  X08-NGBNDKBN-STATE (IDX0)
                                               X08-CHKERR-STATE   (IDX0)
      *
               END-PERFORM
           END-IF
      *
           IF      SPA-NAI-MASTER      =   1
                                       OR  ZERO
      *        新規、標準提供ありは削除なし
               MOVE    WIDGET-INSENSITIVE  TO  X08-B03-STATE
           END-IF
      *    IF    ( SPA-X01-SELNUM          =   ZERO )
      *        MOVE    WIDGET-INSENSITIVE  TO  X08-B03-STATE
      *    END-IF 
      *
      *    PERFORM VARYING IDX1    FROM    1   BY  1
      *            UNTIL ( IDX1    >   SPA-GMN-TBL-MAX )
      *             OR   ( X08-B03-STATE   =   WIDGET-INSENSITIVE )
      *
      *        IF    ( SPA-GMN-TDATAKBN  (IDX1)    =   "1" )
      *         AND  ( SPA-X01-ID                  =   ZERO )
      *            MOVE    WIDGET-INSENSITIVE
      *                                    TO  X08-B03-STATE
      *        END-IF
      *
      **   END-PERFORM
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
      *    コード
           WHEN   1    THRU        20
               MOVE    SPACE           TO  MCP-WIDGET
               MOVE    SPA-GMN-CUR     TO  WRK-NUM
               STRING  "TEKCD"
                       WRK-NUM         DELIMITED   BY  SIZE
                                       INTO  MCP-WIDGET
               END-STRING
      *    開始日
           WHEN   21   THRU        40
               MOVE    SPACE           TO  MCP-WIDGET
               COMPUTE WRK-NUM         =   SPA-GMN-CUR
                                       -   20
               STRING  "YUKOSTYMD"
                       WRK-NUM         DELIMITED   BY  SIZE
                                       INTO  MCP-WIDGET
               END-STRING
      *    終了日
           WHEN   41   THRU        60
               MOVE    SPACE           TO  MCP-WIDGET
               COMPUTE WRK-NUM         =   SPA-GMN-CUR
                                       -   40
               STRING  "YUKOEDYMD"
                       WRK-NUM         DELIMITED   BY  SIZE
                                       INTO  MCP-WIDGET
               END-STRING
      *    入外
           WHEN   61   THRU        80
               MOVE    SPACE           TO  MCP-WIDGET
               COMPUTE WRK-NUM         =   SPA-GMN-CUR
                                       -   60
               STRING  "NGBNDKBN"
                       WRK-NUM         DELIMITED   BY  SIZE
                                       INTO  MCP-WIDGET
               END-STRING
           WHEN   81   THRU        100
               MOVE    SPACE           TO  MCP-WIDGET
               COMPUTE WRK-NUM         =   SPA-GMN-CUR
                                       -   80
               STRING  "CHKERR"
                       WRK-NUM         DELIMITED   BY  SIZE
                                       INTO  MCP-WIDGET
               END-STRING
           WHEN   200
               MOVE  "KIJUNYM"     TO  MCP-WIDGET
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
           WHEN  ( WRK-MCP-WIDGET  >=  "YUKOSTYMD01" )
             AND ( WRK-MCP-WIDGET  <=  "YUKOSTYMD20" )
      *
               MOVE    WRK-MCP-WIDGET(10:2)    TO  WRK-NUM
      *
               EVALUATE    TRUE
               WHEN  ( WRK-MAX =   ZERO )
                   COMPUTE SPA-GMN-CUR     =   1
               WHEN  ( WRK-MAX     <   WRK-NUM )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-MAX +   1
               WHEN  OTHER
                   COMPUTE SPA-GMN-CUR
                       =   WRK-NUM
                       +   CONST-LINE-MAX  *   2
               END-EVALUATE
      *
           WHEN  ( WRK-MCP-WIDGET  >=  "YUKOEDYMD01" )
             AND ( WRK-MCP-WIDGET  <=  "YUKOEDYMD20" )
      *
               MOVE    WRK-MCP-WIDGET(10:2)    TO  WRK-NUM
      *
               EVALUATE    TRUE
               WHEN  ( WRK-MAX =   ZERO )
                   COMPUTE SPA-GMN-CUR     =   1
               WHEN  ( WRK-MAX     <   WRK-NUM )
                   COMPUTE SPA-GMN-CUR
                       =   WRK-MAX +   1
               WHEN  OTHER
                   COMPUTE SPA-GMN-CUR
                       =   WRK-NUM
                       +   CONST-LINE-MAX  *   3
               END-EVALUATE
      *
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
                       +   CONST-LINE-MAX  *   4
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
                   IF    ( X08-NGBNDKBN-STATE (IDX0)
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
                   IF    ( X08-CHKERR-STATE (IDX0)
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
                   MOVE    "開始日付エラー"        TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "開始日付は１日で入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "終了日付エラー"        TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "終了日付は月末日で入力して下さい。"
                                                   TO  SPA-ERRMSG
               WHEN    "0019"
                   STRING  "開始日付＞終了日付です。"
                                                  DELIMITED    BY  SIZE
                                                  INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0020"
                   STRING  "登録済みの適応コードと期間重複します。"
                                                  DELIMITED    BY  SIZE
                           WRK-ERR-NAME           DELIMITED BY SIZE
                                                  INTO    SPA-ERRMSG
                   END-STRING
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
               WHEN    "0006"
                   MOVE    "エラー区分入力エラー"     TO  SPA-ERRMSG
      *
               WHEN    "0007"
                   MOVE    "基準月エラー"             TO  SPA-ERRMSG
      *
               WHEN    "0008"
                   MOVE    "チェック区分に対して入力できないコードです"
                                                      TO  SPA-ERRMSG
               WHEN    "0009"
                   STRING  "同じ適応コードがユーザー登録済みです。"
                           "ユーザコードを変更して下さい。"
                                                  DELIMITED    BY  SIZE
                                                  INTO    SPA-ERRMSG
                   END-STRING
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
               WHEN    "0015"
                   MOVE    "前頁はありません"         TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "次頁はありません"         TO  SPA-ERRMSG
      *
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
           MOVE    "X08"                TO  SPA-MOTOPG
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
               WHEN    "3001"
                   STRING  "登録内容に変更があります。"
                           "ＯＫで登録後に前月表示を行います。"
                                                  DELIMITED    BY  SIZE
                                                  INTO    WRK-XIDMSG
                   END-STRING
               WHEN    "3002"
                   STRING  "登録内容に変更があります。"
                           "ＯＫで登録後に次月表示を行います。"
                                                  DELIMITED    BY  SIZE
                                                  INTO    WRK-XIDMSG
                   END-STRING
               WHEN    "3003"
                   STRING  "登録内容に変更があります。"
                           "ＯＫで登録後に基準月表示を行います。"
                                                  DELIMITED    BY  SIZE
                                                  INTO    WRK-XIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  XID1
           INITIALIZE                       XID1
           MOVE    SPA-XIDCD            TO  XID1-ID1CODE
           MOVE    WRK-XIDMSG           TO  XID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "戻る"               TO  XID1-B01
           MOVE    "ＯＫ"               TO  XID1-B12
           EVALUATE    SPA-XIDCD
               WHEN    "3001"
               WHEN    "3002"
               WHEN    "3003"
                   MOVE    "ＮＯ"           TO  XID1-B01
           END-EVALUATE
      *
           MOVE    "X08"                TO  SPA-MOTOPG
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
      *    基準日で有効あり
           MOVE    SPA-NAI-STKIJUNYMD  TO  CHK-YUKOSTYMD
           MOVE    SPA-NAI-STKIJUNYMD  TO  CHK-YUKOEDYMD
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
           MOVE    SPA-NAI-STKIJUNYMD  TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
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
      *    チェックマスタ検索処理 
      *****************************************************************
       900-CHKMST-FETCH-SEC         SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               INITIALIZE                  CHK-REC
               MOVE    1               TO  FLG-CHKMST
           END-IF
      *
           .
       900-CHKMST-FETCH-EXT.
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
           MOVE    SPA-NAI-STKIJUNYMD  TO  TNS-YUKOSTYMD
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

