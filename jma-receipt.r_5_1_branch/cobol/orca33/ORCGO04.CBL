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
       PROGRAM-ID.                 ORCGO04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 請求確認　（Ｏ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/12/09    NACL-太田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-小豆沢  05.07.19  自費１０項目対応
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    定期請求共通パラメタ
           COPY    "O01COMMON-SPA".
      *
           COPY    ENUM-VALUE.
      *
           COPY    "O01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYUNOU                          PIC 9(01).
           03  FLG-SYUMEI                          PIC 9(01).
           03  FLG-SYORI                           PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-SYURRK                          PIC 9(01).
      *
           03  FLG-SMEILIST1-ROW                   PIC 9(01).
           03  FLG-SKYLIST-ROW                     PIC 9(01).
           03  FLG-HIZUKE-ERR                      PIC 9(01).
           03  FLG-PTINF                           PIC 9(01).
           03  FLG-PTTEIKIRRK                      PIC 9(01).
           03  FLG-SRYYM-CHG                       PIC 9(01).
           03  FLG-MEIVISIBLE                      PIC 9(01).
           03  FLG-ORCSNUMERR                      PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PTTEIKIRRK                      PIC 9(05).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                                PIC 9(05).
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
           03  IDX4                                PIC 9(05).
           03  IDX5                                PIC 9(05).
           03  IDX6                                PIC 9(05).
           03  IDX-SKY                             PIC 9(05).
           03  IDX-SRY                             PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-LST                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SKYMONEY                        PIC S9(10).
           03  WRK-NYUKIN                          PIC S9(10).
           03  WRK-KAKO-NYUKIN                     PIC S9(10).
           03  WRK-MISYU-ZAN                       PIC S9(10).
           03  WRK-MOTOPG                          PIC X(08).
           03  WRK-SYMD                            PIC X(10).
           03  WRK-HENYMDG                         PIC X(10).
           03  WRK-NYUKIN-Z9                       PIC --,---,---.
           03  WRK-Z9                              PIC ZZ.
           03  WRK-Z99                             PIC --,---,--9.
           03  WRK-ZZ9                             PIC ZZ9.
           03  WRK-MISYU                           PIC 9(08).
           03  WRK-NYUGAIKBN                       PIC X(01).
           03  WRK-GMN-RITU.
               05  FILLER                         PIC X(02) VALUE SPACE.
               05  WRK-GMN-FTNRITU                 PIC ZZZ.
               05  WRK-GMN-PA                      PIC X(01).
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
               05  WRK-CMB-ITM3                    PIC X(200).
           03  WRK-JIHI-IDX-X.
               05  WRK-JIHI-IDX                    PIC 9(01).
           03  WRK-MCP-WIDGET                      PIC X(64).
           03  WRK-DENPEDANUM                      PIC 9(02).
           03  WRK-SKYPRT-DENPEDANUM               PIC 9(02).
           03  WRK-SRYPRT-DENPEDANUM               PIC 9(02).
           03  WRK-SKYPRT-SKYMONEY                 PIC S9(10).
           03  WRK-SKYPRT-NYUKIN                   PIC S9(10).
           03  WRK-SKYPRT-TAX-SAI                  PIC S9(10).
           03  WRK-SEIKYU-PG                       PIC X(20).
           03  WRK-MEISAI-PG                       PIC X(20).
           03  WRK-SKYPRTKBN                       PIC X(01).
           03  WRK-SRYPRTKBN                       PIC X(01).
           03  WRK-TSKYMONEY-G.
               05  WRK-TSKYMONEY                   PIC --,---,---.
               05  WRK-TSKYMONEY-MRK               PIC X(01).
           03  WRK-SKIP-MAX                        PIC 9(05).
           03  WRK-SKYKKN-G.
               05  WRK-SKYKKN-YM                   PIC X(06).
               05  WRK-SKYKKN-KBT                  PIC X(03).
               05  WRK-SKYKKN-MRK                  PIC X(01).
           03  WRK-SYU-NYUGAIKBN                   PIC X(01).
           03  WRK-SYU-PTID                        PIC 9(10).
           03  WRK-SYU-DENPNUM                     PIC 9(07).
           03  WRK-OIDMSG                          PIC X(100).
           03  WRK-SAGAKU                          PIC S9(10).
           03  WRK-MEIDSPKBN                       PIC X(01).
           03  WRK-OLD-NYUHEN-YMD                  PIC X(08).
           03  WRK-MONEY9                          PIC --,---,--9.
           03  WRK-MONEYZ                          PIC --,---,---.
           03  WRK-KINGAKUX                        PIC X(10).
           03  WRK-KINGAKU9                        PIC S9(10).
           03  WRK-KINGAKUZ                        PIC ----------.
           03  WRK-GMN-CUR                         PIC 9(3).
      *
       01  WRK-ROW.
           03  WRK-SMEILIST1-ROW                   PIC S9(9)   BINARY.
           03  WRK-SKYLIST-ROW                     PIC S9(9)   BINARY.
      *
       01  SHR-1005-AREA.
           03  SHR-1005-MAX                        PIC 9(05).
           03  SHR-1005-KEY-KBNCD                  PIC X(02).
           03  SHR-1005-KEY-STYUKYMD               PIC X(08).
           03  SHR-1005-KEY-EDYUKYMD               PIC X(08).
           03  SHR-1005-OCC                        OCCURS  50.
               05   SHR-1005-KBNCD                 PIC X(02).
               05   SHR-1005-STYUKYMD              PIC X(08).
               05   SHR-1005-EDYUKYMD              PIC X(08).
               05   SHR-1005-SRYKANAME1            PIC X(20).
               05   SHR-1005-SRYKANAME3            PIC X(04).
      *
       01  CONST-AREA.
           03  CONST-KINGAKU-MAX       PIC 9(10) VALUE 9999999.
           03  CONST-H180401           PIC X(08)   VALUE "20060401".
           03  CONST-SMEILIST-MAX      PIC 9(03)   VALUE   99.
           03  CONST-SKYLIST-MAX       PIC 9(03)   VALUE   50.
           03  CONST-JIHI-MAX          PIC 9(03)   VALUE   10.
           03  CONST-PROC-NO-G.
      *        05  CONST-PROC-NYUKIN   PIC X(02)   VALUE   "1".
      *        05  CONST-PROC-RRKSYU   PIC X(02)   VALUE   "2".
      *        05  CONST-PROC-CHOSEI   PIC X(02)   VALUE   "3".
               05  CONST-PROC-NYUKIN   PIC X(02)   VALUE   "9".
               05  CONST-PROC-RRKSYU   PIC X(02)   VALUE   "1".
               05  CONST-PROC-CHOSEI   PIC X(02)   VALUE   "9".
      *
      *    スパ領域 ワーク
           COPY "COMMON-SPA"       REPLACING //SPA-//
                                   BY        //WRK-SPA-//.
      *
      *************************************************************
      *    ファイルレイアウト
      *************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1013.INC".
      *
           COPY    "CPSK1035.INC".
      *
           COPY    "CPSK1041.INC".
      *
           COPY    "CPSK5000.INC".
      *
           COPY    "CPSK5010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *    収納明細マスター
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *    収納履歴
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *
      *************************************************************
      *    サブプロ用　領域
      *************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    定期請求差額取得サブ
           COPY    "CPORCSTEIKISGK.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    請求書印刷パラメタ
           COPY    "CPORCHCN03.INC".
      *
      *    明細書印刷パラメタ
           COPY    "CPORCHCN04.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    日報キー取得サブ
           COPY "CPORCSDAILYKEY.INC".
      *
      *    収納更新差額取得
           COPY  "CPORCSS002.INC".
      *
      *    分娩入院チェックサブ
           COPY  "CPORCSBUNBENCHK.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *************************************************************
      *    連絡　領域
      *************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "O01.INC".
           COPY    "O02.INC".
           COPY    "O03.INC".
           COPY    "O04.INC".
           COPY    "O05.INC".
           COPY    "O99.INC".
           COPY    "OERR.INC".
           COPY    "OID1.INC".
           COPY    "OID2.INC".
           COPY    "OID3.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *************************************************************
      *    主　　処理
      *************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
           MOVE    SPA-FREE        TO  SPA-O01FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
      *
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *
      *        画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *
      *    入力
            WHEN   OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-O01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-O01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *************************************************************
      *    初期　処理
      *************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "OERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF 
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "O04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
          .
       100-INIT-EXT.
           EXIT.
      *
      *
      *************************************************************
      *    初期画面処理
      *************************************************************
       300-SCREEN-SEC                  SECTION.
      *
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               IF    ( WRK-MOTOPG          =   "P97" )
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-O01FREE
                   MOVE    SPA-WORK        TO  SPA-O01KYOUTU
                   PERFORM 320-P97-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "OID1"
               PERFORM 320-O04-SET-SEC
               GO  TO  300-SCREEN-EXT
           WHEN    OTHER
               PERFORM 310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *************************************************************
      *    確認画面より戻り時処理
      *************************************************************
       320-O04-SET-SEC                 SECTION.
      *
           IF    ( SPA-OID1-FLG    =   "OK" )
               IF      SPA-OIDCD   =   "2001"
                   PERFORM 4101-TOROKU-SEC
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  SPA-OID1-FLG
           MOVE    SPACE           TO  SPA-OIDCD
      *
           .
       320-O04-SET-EXT.
           EXIT.
      *************************************************************
      *    氏名検索画面より戻り時処理
      *************************************************************
       320-P97-SET-SEC                 SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *
      *    人が選ばれなかった場合、
      *    同一の人が選択された場合は、画面内容の変更は行わない
           IF    ( WRK-SPA-PTID    =   ZERO )
             OR  ( WRK-SPA-PTID    =   SPA-NAI-O04-PTID )
               GO  TO  320-P97-SET-EXT
           END-IF
      *
           MOVE    WRK-SPA-PTNUM       TO  O04-PTNUM
      *
           PERFORM 4100-PTNUM-CHK-SEC
      *
           .
       320-P97-SET-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           MOVE    1                   TO  FLG-SMEILIST1-ROW
           MOVE    1                   TO  FLG-SKYLIST-ROW
      *
           INITIALIZE                  SPA-O04-AREA
      *
           MOVE    SPA-SYSYMD          TO  SPA-NAI-O04-SRYYMD
      *
      *    コンボ編集処理
           PERFORM 3101-COMBO-SET-SEC
      *
      *    MOVE    CONST-PROC-CHOSEI   TO  WRK-CMB-CD
           MOVE    CONST-PROC-RRKSYU   TO  WRK-CMB-CD
           PERFORM 4200-SYORIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-O04-SYORIKBN-G
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-O04-NYUKINYMD
      *
           MOVE    SPA-GMN-O04-NYKN-HOHOLST (1)
                                       TO  SPA-GMN-O04-NYKN-HOHO-G
           MOVE    SPA-GMN-O04-SKYPRTLST (1)
                                       TO  SPA-GMN-O04-SKYPRT-G
           MOVE    SPA-GMN-O04-SRYPRTLST (1)
                                       TO  SPA-GMN-O04-SRYPRT-G
      *
           MOVE    1                   TO  SPA-GMN-O04-CUR
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *************************************************************
      *    コンボ項目設定処理
      *************************************************************
       3101-COMBO-SET-SEC              SECTION.
      *
           PERFORM 3101-SYORIKBNLST-SET-SEC
      *
           PERFORM 3101-NYKN-HOHOLST-SET-SEC
      *
           PERFORM 3101-SKYPRTLST-SET-SEC
      *
           PERFORM 3101-SRYPRTLST-SET-SEC
      *
           .
       3101-COMBO-SET-EXT.
           EXIT.
      *************************************************************
      *    処理区分コンボ設定処理
      *************************************************************
       3101-SYORIKBNLST-SET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-SYORIKBNLST-G
      *
           MOVE    "1"             TO  SPA-GMN-O04-SYORIKBNL (1)
           MOVE    "履歴修正"      TO  SPA-GMN-O04-SYORIKBNMEIL (1)
      *
           MOVE    1               TO  SPA-GMN-O04-SYORIKBN-MAX
      *
           .
       3101-SYORIKBNLST-SET-EXT.
           EXIT.
      *************************************************************
      *    入金方法コンボ設定処理
      *************************************************************
       3101-NYKN-HOHOLST-SET-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-NYKN-HOHOLST-G
                                       SPA-NAI-O04-NYKN-HOHOLST-G
      *    入金方法
           MOVE    ZERO            TO  FLG-SYSKANRI
           INITIALIZE                  SYS-1041-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1041-HOSPNUM
           MOVE    "1041"          TO  SYS-1041-KANRICD
           MOVE    SPA-NAI-O04-SRYYMD
                                   TO  SYS-1041-STYUKYMD
                                       SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           MOVE    SYS-1041-REC    TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC            =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-1041-REC
           ELSE
               INITIALIZE              SYS-1041-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO            TO  IDX3
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               COMPUTE IDX3    =   IDX3    +   1
      *
               MOVE    SYS-1041-KBNCD
                                   TO  SPA-GMN-O04-NYKN-HOHOL    (IDX3)
               MOVE    SYS-1041-NYKN-NAME
                                   TO  SPA-GMN-O04-NYKN-HOHOMEIL (IDX3)
               MOVE    SYS-1041-NYKN-TANNAME
                                   TO  SPA-NAI-O04-NYKN-HOHOMEIL (IDX3)
               MOVE    SYS-1041-NYKN-BUNRUI
                                   TO  SPA-NAI-O04-NYKN-BUNRUIL  (IDX3)
      *
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-FETCH-SEC
               MOVE    MCPDATA-REC     TO  SYS-1041-REC
      *
           END-PERFORM
      *
           MOVE    IDX3            TO  SPA-GMN-O04-NYKN-HOHO-MAX
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           .
       3101-NYKN-HOHOLST-SET-EXT.
           EXIT.
      *************************************************************
      *    請求書発行コンボ設定処理
      *************************************************************
       3101-SKYPRTLST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-SKYPRTLST-G
      *
           MOVE    "0"             TO  SPA-GMN-O04-SKYPRTL (1)
           MOVE    "発行なし"      TO  SPA-GMN-O04-SKYPRTMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-O04-SKYPRTL (2)
           MOVE    "発行する"      TO  SPA-GMN-O04-SKYPRTMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-O04-SKYPRT-MAX
      *
           .
       3101-SKYPRTLST-SET-EXT.
           EXIT.
      *************************************************************
      *    明細書発行コンボ設定処理
      *************************************************************
       3101-SRYPRTLST-SET-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-SRYPRTLST-G
      *
           MOVE    "0"             TO  SPA-GMN-O04-SRYPRTL (1)
           MOVE    "発行なし"      TO  SPA-GMN-O04-SRYPRTMEIL (1)
      *
           MOVE    "1"             TO  SPA-GMN-O04-SRYPRTL (2)
           MOVE    "発行する"      TO  SPA-GMN-O04-SRYPRTMEIL (2)
      *
           MOVE    2               TO  SPA-GMN-O04-SRYPRT-MAX
      *
           .
       3101-SRYPRTLST-SET-EXT.
           EXIT.
      *************************************************************
      *    収納項目設定処理
      *************************************************************
       3101-SYUNOU-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-KINGAKU-AREA
                                       SPA-NAI-O04-KINGAKU-AREA
      *
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  WRK-SYU-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  WRK-SYU-PTID
           MOVE    SPA-NAI-O04-DENPNUM TO  WRK-SYU-DENPNUM
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           IF    ( SYU-HKNTEKKBN       =   "1" )
               MOVE    SYU-KOH-FTN     TO  SYU-HKNTEKMONEY
               MOVE    ZERO            TO  SYU-KOH-FTN
           END-IF
      *
           MOVE    SYU-DENPPRTYMD      TO  SPA-NAI-O04-DENPPRTYMD
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-O04-HEAD-DENPJTIKBN
           MOVE    SYU-SYUHKNNUM       TO  SPA-NAI-O04-HKNNUM
      *
           PERFORM 41001-ORCSHKNSET-SEC
           IF    ( ORCSHKN-IDX    >=   1 )
            AND  ( ORCSHKN-IDX    <=   ORCSHKN-HKN-MAX )
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (ORCSHKN-IDX)
                                       TO  SPA-GMN-O04-KUMI
               MOVE    ORCSHKN-NAI-RSI-HKNKBN  (ORCSHKN-IDX)
                                       TO  SPA-NAI-O04-RSI-HKNKBN
           END-IF
      *
      *    診療年月
           MOVE    SYU-SRYYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-O04-SRYYM
      *
      *    発行日
           MOVE    SYU-DENPPRTYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-O04-HAKYMD
      *
      *    患者負担率
           IF    ( SYU-PTFTNRATE       =   ZERO )
               MOVE    SPACE           TO  SPA-GMN-O04-RITU
           ELSE
               MOVE    SPACE           TO  WRK-GMN-RITU
               MOVE    SYU-PTFTNRATE   TO  WRK-GMN-FTNRITU
               MOVE    "%"             TO  WRK-GMN-PA
               MOVE    WRK-GMN-RITU    TO  SPA-GMN-O04-RITU
           END-IF.
      *
      *    伝票状態区分
           EVALUATE   SYU-DENPJTIKBN
           WHEN   SPACE
               MOVE  "請求額なし"      TO  SPA-GMN-O04-JOUTAI
           WHEN   "1"
               MOVE  "入金済"          TO  SPA-GMN-O04-JOUTAI
           WHEN   "2"
               MOVE  "未入金"          TO  SPA-GMN-O04-JOUTAI
           WHEN   "3"
               MOVE  "請求取消し"      TO  SPA-GMN-O04-JOUTAI
           WHEN   "7"
               MOVE  "継続"            TO  SPA-GMN-O04-JOUTAI
           END-EVALUATE
      *
      *    保険料
           MOVE    ZERO                TO  SPA-GMN-O04-HKNRYO (12)
           PERFORM VARYING     IDX0    FROM    1   BY  1
                   UNTIL     ( IDX0    >   16 )
               MOVE    SYU-HKNTEN (IDX0)
                                       TO  SPA-GMN-O04-HKNRYO (IDX0)
      *
           END-PERFORM
      *
           MOVE    SYU-TOTAL-HKNTEN    TO  SPA-GMN-O04-HKNRYO (17)
      *
           MOVE    SYU-DENPJTIKBN      TO  SPA-NAI-O04-HEAD-DENPJTIKBN
      *
      *    保険適用外
           PERFORM VARYING     IDX0     FROM    1   BY  1
                   UNTIL     ( IDX0     >   16 )
      *
               MOVE    ZERO            TO   SPA-GMN-O04-TGMONEY (IDX0)
               COMPUTE SPA-GMN-O04-TGMONEY (IDX0)
                     = SYU-TGMONEY (IDX0)  +   SYU-TGMONEY-TAX (IDX0)
           END-PERFORM
      *
      *    保険適用分
           MOVE    SYU-HKNTEKMONEY     TO  SPA-GMN-O04-HKNTEKI
      *
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  SPA-GMN-O04-ROUFTN
      *
      *    公費一部負担金
           MOVE    SYU-KOH-FTN         TO  SPA-GMN-O04-KOHFTN
      *
      *    その他自費
           PERFORM VARYING IDX0 FROM    1   BY  1
                   UNTIL ( IDX0 >   10)
      *
               MOVE    SYU-JIHI-TAXNASI (IDX0)
                                       TO  SPA-GMN-O04-JIHI (1 IDX0)
               MOVE    SYU-JIHI-TAXARI  (IDX0)
                                       TO  SPA-GMN-O04-JIHI (2 IDX0)
      *
           END-PERFORM
      *
           MOVE    SYU-JIHI-TOTAL      TO  SPA-GMN-O04-JIHIKEI(1)
      *
           MOVE    SYU-JIHI-TOTAL-TAX  TO  SPA-GMN-O04-JIHIKEI(2)
      *
           MOVE    SYU-JIHI-TAX        TO  SPA-GMN-O04-JIHIZEI
      *
      *    保険適用外
           MOVE    ZERO                TO  SPA-GMN-O04-HKNGAI.
           COMPUTE  SPA-GMN-O04-HKNGAI
                 =  SYU-TOTAL-TGMONEY  +   SYU-TOTAL-TGMONEY-TAX
      *
      *    労災保険適用分
           MOVE    SYU-RSISHOSHIN-MONEY
                                       TO SPA-GMN-O04-RSISHOSHIN
           MOVE    SYU-RSISAISHIN-MONEY
                                       TO SPA-GMN-O04-RSISAISHIN
           MOVE    SYU-RSISDO-MONEY    TO SPA-GMN-O04-RSISDO
           MOVE    SYU-RSIETC-MONEY    TO SPA-GMN-O04-RSIETC
      *
      *    食事療養費
           COMPUTE SPA-GMN-O04-SKJRYOYO
               =   SYU-RYOYOHI-SKJ
               +   SYU-RYOYOHI-SKJ-JIHI
      *
      *    生活療養費
           COMPUTE SPA-GMN-O04-LIFERYOYO
               =   SYU-RYOYOHI-LIFE
               +   SYU-RYOYOHI-LIFE-JIHI
      *
      *    食事負担額
           COMPUTE SPA-GMN-O04-SKJFTN
               =   SYU-SKYMONEY-SKJ-KEI
               +   SYU-SKYMONEY-SKJ-JIHI-KEI
      *
      *    生活負担額
           COMPUTE SPA-GMN-O04-LIFEFTN
               =   SYU-SKYMONEY-LIFE-KEI
               +   SYU-SKYMONEY-LIFE-JIHI-KEI
      *
      *    薬剤一部負担  
           MOVE    SYU-YKZ-FTN         TO  SPA-GMN-O04-YAKFTN.
      *
      *    一部負担金
           COMPUTE SPA-GMN-O04-ICHIFTN     =   SPA-GMN-O04-ROUFTN
                                       +   SPA-GMN-O04-KOHFTN
                                       +   SPA-GMN-O04-YAKFTN
                                       +   SPA-GMN-O04-SKJFTN
                                       +   SPA-GMN-O04-LIFEFTN
      *    室料差額
           MOVE    SYU-RMSAGAKU        TO  SPA-GMN-O04-RMSAGAKU
      *
      *    調整金１
           MOVE    SYU-CHOSEI1         TO  SPA-GMN-O04-CHOSEI1
      *
      *    調整金２
           MOVE    SYU-CHOSEI2         TO  SPA-GMN-O04-CHOSEI2
      *
      *    入金額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-O04-NYUKIN-TTL
      *
      *    未収残
           COMPUTE SPA-GMN-O04-MISYU-ZAN
               =   SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL
      *
      *    減免情報
           MOVE    SYU-DISCOUNT-KBN    TO  SPA-NAI-O04-DISCOUNT-KBN
           MOVE    SYU-DISCOUNT-BODY   TO  SPA-NAI-O04-DISCOUNT-BODY
           MOVE    SYU-DISCOUNT-RATEKBN TO SPA-NAI-O04-DISCOUNT-RATEKBN
           MOVE    SYU-DISCOUNT-TEIRITU TO SPA-NAI-O04-DISCOUNT-TEIRITU
           MOVE    SYU-DISCOUNT-RATE   TO  SPA-NAI-O04-DISCOUNT-RATE
           MOVE    SYU-DISCOUNT-MONEY  TO  SPA-NAI-O04-DISCOUNT-MONEY
      *
      *    合計請求額計算処理
           PERFORM 3102-SEIKYU-KEI-SEC
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *
      *    前回未収
           PERFORM 3101-ZENMISYU-SEC
      *
      *    合計未収額
           IF    ( SPA-GMN-O04-MISYU-ZAN   >   ZERO )
               COMPUTE SPA-GMN-O04-SEIGOK
                   =   SPA-GMN-O04-MISYU-ZAN
                   +   SPA-GMN-O04-ZENMISYU-GAI
                   +   SPA-GMN-O04-ZENMISYU-NYU
           ELSE
               COMPUTE SPA-GMN-O04-SEIGOK
                   =   SPA-GMN-O04-ZENMISYU-GAI
                   +   SPA-GMN-O04-ZENMISYU-NYU
           END-IF
      *
           .
       3101-SYUNOU-SET-EXT.
           EXIT.
      *************************************************************
      *    期別編集処理
      *************************************************************
       3101-KBT-HEN-SEC         SECTION.
      *
           INITIALIZE                      SPA-GMN-O04-KBT-AREA
      *
           INITIALIZE                      STEIKISGK-AREA
           MOVE    SPA-NAI-O04-PTID    TO  STEIKISGK-PTID
           MOVE    SPA-NAI-O04-SRYYMD  TO  STEIKISGK-SRYYMD
           MOVE    SPA-NAI-O04-RRKNUM  TO  STEIKISGK-RRKNUM
           CALL    "ORCSTEIKISGK"          USING
                                           STEIKISGK-AREA
                                           SPA-AREA
           IF    ( STEIKISGK-RC        =   ZERO )
      *
               MOVE    STEIKISGK-SAGAKU(1)
                                       TO  SPA-GMN-O04-SKY1ST
               MOVE    STEIKISGK-SAGAKU(2)
                                       TO  SPA-GMN-O04-SKY2ND
               MOVE    STEIKISGK-SAGAKU(3)
                                       TO  SPA-GMN-O04-SKY3RD
           END-IF
      *
           .
       3101-KBT-HEN-EXT.
           EXIT.
      *************************************************************
      *    期別差額編集処理
      *************************************************************
       3101-KBT-SAGAKU-HEN-SEC         SECTION.
      *
           COMPUTE SPA-GMN-O04-SKYSGK
               =   SPA-GMN-O04-SKY1ST
               +   SPA-GMN-O04-SKY2ND
               +   SPA-GMN-O04-SKY3RD
      *
           .
       3101-KBT-SAGAKU-HEN-EXT.
           EXIT.
      *************************************************************
      *    収納明細項目設定処理
      *************************************************************
       3101-SYUMEI-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-SMEITBL-AREA
                                       SPA-NAI-O04-SMEITBL-AREA
      *
      *    収納明細マスターを検索し内容をスパ領域にセット
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  SMEI-PTID
           MOVE    SPA-NAI-O04-DENPNUM TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE     ZERO               TO  IDX1
           PERFORM UNTIL ( FLG-SYUMEI NOT  =   ZERO )
                    OR   ( IDX1           >=   CONST-SMEILIST-MAX )
      *
             MOVE    1                   TO  FLG-MEIVISIBLE
      *
             IF    ( SMEI-JOUTAIKBN      =   "2" OR "4" OR "6" )
              AND  ( SMEI-NYUHEN-MONEY   =   ZERO )
                 MOVE      "1"           TO  SPA-NAI-O04-MEIDSPCTRL
                 IF  ( SPA-NAI-O04-MEIDSPKBN =   "1" )
                     MOVE    ZERO        TO  FLG-MEIVISIBLE
                 END-IF
             END-IF
      *
             IF    ( FLG-MEIVISIBLE      =   1 )
      *
      *
               COMPUTE IDX1    =   IDX1    +   1
      *
      *
               MOVE    SMEI-DENPEDANUM TO  SPA-GMN-O04-TDENPEDANUM(IDX1)
               MOVE    SMEI-SKYMONEY   TO  SPA-GMN-O04-TSKYMONEY (IDX1)
               MOVE    SMEI-NYUHEN-MONEY
                                       TO  SPA-GMN-O04-TNYUKIN (IDX1)
               MOVE    SMEI-JOUTAIKBN  TO  SPA-NAI-O04-TJOUTAIKBN (IDX1)
               MOVE    SMEI-SYURRKUPDKBN
                                   TO  SPA-NAI-O04-TSYURRKUPDKBN(IDX1)
               MOVE    SMEI-SYURRKNUM
                                   TO  SPA-NAI-O04-TSYURRKNUM   (IDX1)
               MOVE    SMEI-SYUEDANUM
                                   TO  SPA-NAI-O04-TSYUEDANUM   (IDX1)
      *
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    SPACE
                   MOVE   "請求額なし"
                                       TO  SPA-GMN-O04-TJOUTAI (IDX1)
               WHEN    "1"
                   IF   ( SMEI-SKYMONEY    >   SMEI-NYUHEN-MONEY )
                       MOVE   "請求・未収"
                                       TO  SPA-GMN-O04-TJOUTAI (IDX1)
                   ELSE
                       MOVE   "請求・入金"
                                       TO  SPA-GMN-O04-TJOUTAI (IDX1)
                   END-IF
      *
               WHEN    "2"
                   MOVE    "入金"      TO  SPA-GMN-O04-TJOUTAI (IDX1)
      *
               WHEN    "3"
                   MOVE    "再計算"    TO  SPA-GMN-O04-TJOUTAI (IDX1)
      *
               WHEN    "4"
                   MOVE    "返金"      TO  SPA-GMN-O04-TJOUTAI (IDX1)
      *
               WHEN    "5"
                   MOVE    "請求取消"  TO  SPA-GMN-O04-TJOUTAI (IDX1)
      *
               WHEN    "6"
                   MOVE    "入金取消"  TO  SPA-GMN-O04-TJOUTAI (IDX1)
      *
               WHEN    "7"
                   MOVE    "継続"      TO  SPA-GMN-O04-TJOUTAI (IDX1)
      *
               WHEN    "8"
                   MOVE    "診療訂正"  TO  SPA-GMN-O04-TJOUTAI (IDX1)
               WHEN    "9"
                   MOVE    "請求"      TO  SPA-GMN-O04-TJOUTAI (IDX1)
               END-EVALUATE
      *
               MOVE    SMEI-NYUHEN-YMD TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-O04-TSYORIYMD (IDX1)
      *
               MOVE    SMEI-NYUHEN-YMD TO  SPA-NAI-O04-TSYORIYMD (IDX1)
      *
               MOVE    SMEI-NYUKIN-HOHO
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYKN-HOHOLST-CHK-SEC
               MOVE    WRK-CMB-ITM2    TO SPA-GMN-O04-TNYKN-HOHO (IDX1)
      *
               MOVE    SMEI-NYUKIN-HOHO
                                       TO SPA-NAI-O04-TNYKN-HOHO (IDX1)
               EVALUATE    SPA-NAI-O04-TJOUTAIKBN (IDX1)
               WHEN    " "
               WHEN    "1"
               WHEN    "9"
                   MOVE    1           TO  SPA-NAI-O04-TPROC-FLG (IDX1)
               WHEN    "8"
                   MOVE    2           TO  SPA-NAI-O04-TPROC-FLG (IDX1)
               WHEN    "2"
                   MOVE    3           TO  SPA-NAI-O04-TPROC-FLG (IDX1)
               WHEN    "4"
               WHEN    "6"
                   MOVE    4           TO  SPA-NAI-O04-TPROC-FLG (IDX1)
               END-EVALUATE
      *
               IF    ( SMEI-SYURRKNUM  NOT =   ZERO )
                   PERFORM 900-SYURRK-KEY-SEL-SEC
                   IF    ( FLG-SYURRK      =   ZERO )
                       MOVE    SYURRK-CHOSEI1
                                       TO  SPA-NAI-O04-TCHOSEI1(IDX1)
                       MOVE    SYURRK-CHOSEI2
                                       TO  SPA-NAI-O04-TCHOSEI2(IDX1)
                   ELSE
                       MOVE    ZERO    TO  SPA-NAI-O04-TSYURRKNUM(IDX1)
                                           SPA-NAI-O04-TSYUEDANUM(IDX1)
                   END-IF
               END-IF
      *
             END-IF
      *
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUMEI-REC
               ELSE
                   MOVE    1           TO  FLG-SYUMEI
                   INITIALIZE              SYUMEI-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    IDX1                TO  SPA-GMN-O04-SMEILIST-MAX
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYUMEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC            SECTION.
      *
      *    医療機関情報−自費名称
           MOVE    ZERO                TO  FLG-SYSKANRI
           INITIALIZE                      SYS-1013-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-NAI-O04-SRYYMD  TO  SYS-1013-STYUKYMD
                                           SYS-1013-EDYUKYMD
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1013-REC
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               IF    ( SPA-NAI-O04-NYUGAIKBN   =   SYS-1013-KBNCD(1:1) )
                   MOVE   SYS-1013-KBNCD(2:1)
                                           TO  WRK-JIHI-IDX-X
                   IF    ( WRK-JIHI-IDX-X >=   "1" )
                    AND  ( WRK-JIHI-IDX-X <=   "9" )
                       MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-O04-JIHIMSG
                                                    (WRK-JIHI-IDX)
                   ELSE
                       IF  WRK-JIHI-IDX-X  =   "0"
                           MOVE    SYS-1013-JIHINAME
                                           TO  SPA-GMN-O04-JIHIMSG(10)
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_syskanri"            TO  MCP-TABLE
               MOVE    "key2"                    TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-FETCH-SEC
               MOVE    MCPDATA-REC         TO  SYS-1013-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"            TO  MCP-TABLE
           MOVE    "key2"                    TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *************************************************************
      *    前回未収額算出処理
      *************************************************************
       3101-ZENMISYU-SEC               SECTION.
      *
           MOVE    "1"             TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SEC
           MOVE    WRK-MISYU       TO  SPA-GMN-O04-ZENMISYU-NYU
      *
           MOVE    "2"             TO  WRK-NYUGAIKBN
           PERFORM 3101-MISYU-SEC
           MOVE    WRK-MISYU       TO  SPA-GMN-O04-ZENMISYU-GAI
      *
           .
       3101-ZENMISYU-EXT.
           EXIT.
      *
      *************************************************************
      *    未収額算出処理
      *************************************************************
       3101-MISYU-SEC                  SECTION.
      *
           MOVE   ZERO                 TO  WRK-MISYU
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           PERFORM UNTIL ( FLG-SYUNOU  =  1 )
      *
               MOVE    ZERO            TO  FLG-SYORI
      *
               IF     (SPA-NAI-O04-DENPPRTYMD  <  SYU-DENPPRTYMD )
                OR   ((SPA-NAI-O04-NYUGAIKBN   =  SYU-NYUGAIKBN )
                  AND (SPA-NAI-O04-DENPNUM     <= SYU-DENPNUM   ))
                       MOVE    1     TO      FLG-SYORI
               END-IF
      *
               IF    ( FLG-SYORI           =   ZERO )
                   IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                       COMPUTE WRK-MISYU
                           =   WRK-MISYU
                           + ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYUNOU-FETCH-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-MISYU-EXT.
           EXIT.
      *************************************************************
      *    合計請求額計算処理
      *************************************************************
       3102-SEIKYU-KEI-SEC             SECTION.
      *
      *    今回請求額
           MOVE   SYU-SKYMONEY         TO  SPA-GMN-O04-SKYMONEY 
      *
           .
       3102-SEIKYU-KEI-EXT.
           EXIT.
      *************************************************************
      *    戻る　処理
      *************************************************************
       210-BACK                        SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPA-O01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "O04"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-O01-MOTOPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *************************************************************
      *    自画面編集処理
      *************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-OIDCD       NOT =   SPACE )
               PERFORM 520-OIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "O04    "          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    O04-CLIST1-ROW     TO  WRK-SMEILIST1-ROW
           MOVE    O04-SKYLIST-ROW    TO  WRK-SKYLIST-ROW
      *
           INITIALIZE                      O04
      *
      *    EVALUATE SPA-GMN-O04-SYORIKBN
      *
      *    WHEN    CONST-PROC-NYUKIN
      *        MOVE    "履歴修正"      TO  O04-B03-LABEL
      *
      *    WHEN    CONST-PROC-RRKSYU
      *        MOVE    "調整金"        TO  O04-B03-LABEL
      *
      *    WHEN    CONST-PROC-CHOSEI
      *        MOVE    "入金"          TO  O04-B03-LABEL
      *
      *    WHEN    OTHER
      *        MOVE    "調整金"        TO  O04-B03-LABEL
      *    END-EVALUATE
      *
           MOVE    SPA-GMN-O04-PTNUM       TO  O04-PTNUM
           MOVE    SPA-GMN-O04-KANANAME    TO  O04-KANANAME
           MOVE    SPA-GMN-O04-NAME        TO  O04-NAME
           MOVE    SPA-GMN-O04-SEX         TO  O04-SEX
           MOVE    SPA-GMN-O04-KUMI        TO  O04-HKNCOMBI
           MOVE    SPA-GMN-O04-RITU        TO  O04-RATE
           MOVE    SPA-GMN-O04-SRYYM       TO  O04-SRYYM
           MOVE    SPA-GMN-O04-BIRTHDAY    TO  O04-BIRTHDAY
           MOVE    SPA-GMN-O04-SRYKA       TO  O04-SRYKA
      *
           MOVE    SPA-GMN-O04-SYORIKBN-G (3:)
                                           TO  O04-NAIYO
      *    MOVE    SPA-GMN-O04-SYORIKBN-MAX
      *                                TO  O04-NAIYO-COUNT
      *    PERFORM VARYING IDX0    FROM    1   BY  1
      *            UNTIL ( IDX0    >   SPA-GMN-O04-SYORIKBN-MAX )
      *        MOVE    SPA-GMN-O04-SYORIKBNLST (IDX0)
      *                                TO  O04-NAIYOLST(IDX0)
      *    END-PERFORM
      *
           MOVE    SPA-GMN-O04-JOUTAI      TO  O04-DENPJTI
           MOVE    SPA-GMN-O04-HAKYMD      TO  O04-HAKYMD
           IF    ( SPA-NAI-O04-DENPNUM NOT =   ZERO )
               MOVE    SPA-NAI-O04-DENPNUM TO  O04-DENPNUM (3:)
           END-IF
      *
           MOVE    SPA-GMN-O04-HKNRYO (01) TO  O04-SSUHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (02) TO  O04-SDOHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (03) TO  O04-ZTKHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (04) TO  O04-TYKHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (05) TO  O04-CSYHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (06) TO  O04-SYCHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (07) TO  O04-SJTHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (08) TO  O04-MSIHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (09) TO  O04-KNSHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (10) TO  O04-GZUHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (11) TO  O04-ETCHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (12) TO  O04-SSNHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (13) TO  O04-HOUHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (14) TO  O04-BYRHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (15) TO  O04-NYUHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (16) TO  O04-RYOHKNTEN
           MOVE    SPA-GMN-O04-HKNRYO (17) TO  O04-TOTALHKNTEN
           MOVE    SPA-GMN-O04-HKNTEKI     TO  O04-HKNTEKMONEY
           MOVE    SPA-GMN-O04-ROUFTN      TO  O04-RJNFTN
           MOVE    SPA-GMN-O04-KOHFTN      TO  O04-KOHFTN
           MOVE    SPA-GMN-O04-JIHI (1 1)  TO  O04-JIHI1
           MOVE    SPA-GMN-O04-JIHI (1 2)  TO  O04-JIHI2
           MOVE    SPA-GMN-O04-JIHI (1 3)  TO  O04-JIHI3
           MOVE    SPA-GMN-O04-JIHI (1 4)  TO  O04-JIHI4
           MOVE    SPA-GMN-O04-JIHI (1 5)  TO  O04-JIHI5
           MOVE    SPA-GMN-O04-JIHI (1 6)  TO  O04-JIHI6
           MOVE    SPA-GMN-O04-JIHI (1 7)  TO  O04-JIHI7
           MOVE    SPA-GMN-O04-JIHI (1 8)  TO  O04-JIHI8
           MOVE    SPA-GMN-O04-JIHI (1 9)  TO  O04-JIHI9
           MOVE    SPA-GMN-O04-JIHI (1 10) TO  O04-JIHI10
           MOVE    SPA-GMN-O04-JIHIKEI(1)  TO  O04-JIHITOTAL
           MOVE    SPA-GMN-O04-JIHI (2 1)  TO  O04-JIHI1TAX
           MOVE    SPA-GMN-O04-JIHI (2 2)  TO  O04-JIHI2TAX
           MOVE    SPA-GMN-O04-JIHI (2 3)  TO  O04-JIHI3TAX
           MOVE    SPA-GMN-O04-JIHI (2 4)  TO  O04-JIHI4TAX
           MOVE    SPA-GMN-O04-JIHI (2 5)  TO  O04-JIHI5TAX
           MOVE    SPA-GMN-O04-JIHI (2 6)  TO  O04-JIHI6TAX
           MOVE    SPA-GMN-O04-JIHI (2 7)  TO  O04-JIHI7TAX
           MOVE    SPA-GMN-O04-JIHI (2 8)  TO  O04-JIHI8TAX
           MOVE    SPA-GMN-O04-JIHI (2 9)  TO  O04-JIHI9TAX
           MOVE    SPA-GMN-O04-JIHI (2 10) TO  O04-JIHI10TAX
           MOVE    SPA-GMN-O04-JIHIKEI(2)  TO  O04-JIHITAXTOTAL
           MOVE    SPA-GMN-O04-JIHIZEI     TO  O04-JIHITAX
           MOVE    SPA-GMN-O04-TGMONEY (01) TO  O04-SSUTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (02) TO  O04-SDOTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (03) TO  O04-ZTKTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (04) TO  O04-TYKTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (05) TO  O04-CSYTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (06) TO  O04-SYCTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (07) TO  O04-SJTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (08) TO  O04-MSITGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (09) TO  O04-KNSTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (10) TO  O04-GZUTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (11) TO  O04-ETCTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (12) TO  O04-SSNTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (13) TO  O04-HOUTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (14) TO  O04-BYRTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (15) TO  O04-NYUTGMONEY
           MOVE    SPA-GMN-O04-TGMONEY (16) TO  O04-RYOTGMONEY
           MOVE    SPA-GMN-O04-HKNGAI      TO  O04-HKNTEKGAI
           MOVE    SPA-GMN-O04-ICHIFTN     TO  O04-FTNTOTAL
      *    MOVE    SPA-GMN-O04-YAKFTN      TO  O04-YKZFTN 
           MOVE    SPA-GMN-O04-SKJFTN      TO  O04-SKJFTN
           MOVE    SPA-GMN-O04-LIFEFTN     TO  O04-LIFEFTN
           MOVE    SPA-GMN-O04-SKJRYOYO    TO  O04-SKJRYOYO
           MOVE    SPA-GMN-O04-LIFERYOYO   TO  O04-LIFERYOYO
           MOVE    SPA-GMN-O04-RMSAGAKU    TO  O04-RMSAGAKU
           MOVE    SPA-GMN-O04-CHOSEI1     TO  O04-CHOSEI1
           MOVE    SPA-GMN-O04-CHOSEI2     TO  O04-CHOSEI2
           MOVE    SPA-GMN-O04-SKYMONEY    TO  O04-SYKMONEY
           MOVE    SPA-GMN-O04-ZENMISYU-GAI
                                       TO  O04-ZENMISYU-GAI
           MOVE    SPA-GMN-O04-ZENMISYU-NYU
                                       TO  O04-ZENMISYU-NYU
           MOVE    SPA-GMN-O04-SEIGOK      TO  O04-GOKMISYU
           MOVE    SPA-GMN-O04-MISYU-ZAN   TO  O04-MISYUZAN
           MOVE    SPA-GMN-O04-NYUKIN-TTL  TO  O04-NYUKINGK
      *
      *    入金欄
           MOVE    SPA-GMN-O04-CHOSEI1INX  TO  O04-CHOSEI1IN
           MOVE    SPA-GMN-O04-CHOSEI2INX  TO  O04-CHOSEI2IN
           MOVE    SPA-GMN-O04-SKYMONEYIN  TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ            TO  O04-SKYMONEYIN
           MOVE    SPA-GMN-O04-NYUKINX     TO  O04-KINGAKU
           MOVE    SPA-GMN-O04-NYUKINYMD   TO  O04-NYUKINYMD
      *
           IF   (  SPA-NAI-O04-DENPJTIKBN  =   "4" )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-O04-NYKN-HOHO-G
                                           TO  O04-NYUKINHOUHOU
               MOVE    SPA-GMN-O04-NYKN-HOHO-MAX
                                           TO  O04-NYUKINHOUHOU-COUNT
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-O04-NYKN-HOHO-MAX )
                   MOVE    SPA-GMN-O04-NYKN-HOHOLST (IDX0)
                                           TO  O04-NYUKINHOUHOULST(IDX0)
               END-PERFORM
           END-IF
      *
           MOVE    SPA-GMN-O04-SKYPRT-G    TO  O04-SKYPRT
           MOVE    SPA-GMN-O04-SKYPRT-MAX  TO  O04-SKYPRT-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-O04-SKYPRT-MAX )
               MOVE    SPA-GMN-O04-SKYPRTLST (IDX0)
                                           TO  O04-SKYPRTLST(IDX0)
           END-PERFORM
      *
           MOVE    SPA-GMN-O04-SRYPRT-G    TO  O04-SRYPRT
           MOVE    SPA-GMN-O04-SRYPRT-MAX  TO  O04-SRYPRT-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-O04-SRYPRT-MAX )
               MOVE    SPA-GMN-O04-SRYPRTLST (IDX0)
                                           TO  O04-SRYPRTLST(IDX0)
           END-PERFORM
      *
           MOVE    SPA-GMN-O04-SELNUM          TO  O04-RIREKIIN
      *
           MOVE    SPA-GMN-O04-SMEILIST-MAX    TO  O04-CLIST1-COUNT
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-O04-SMEILIST-MAX )
      *
               MOVE    IDX0                TO  WRK-Z9
               MOVE    WRK-Z9              TO  O04-TRENNUM (IDX0)
               INITIALIZE                      WRK-TSKYMONEY-G
               MOVE    SPA-GMN-O04-TSKYMONEY    (IDX0)
                                           TO  WRK-TSKYMONEY
               IF    ( SPA-NAI-O04-MEISAIJOUTAIKBN =   "1" )
                   MOVE    "*"             TO  WRK-TSKYMONEY-MRK
               END-IF
               MOVE    WRK-TSKYMONEY-G     TO  O04-TSKYMONY(IDX0)
               MOVE    SPA-GMN-O04-TNYUKIN  (IDX0)
                                           TO  WRK-NYUKIN-Z9
               MOVE    WRK-NYUKIN-Z9       TO  O04-TNYUKINMONEY(IDX0)
               MOVE    SPA-GMN-O04-TSYORIYMD (IDX0)
                                           TO  O04-TSYORIYMD   (IDX0)
               MOVE    SPA-GMN-O04-TJOUTAI  (IDX0)
                                           TO  O04-TJTI        (IDX0)
               MOVE    SPA-GMN-O04-TNYKN-HOHO(IDX0)
                                           TO  O04-TNYKN-NAME  (IDX0)
               IF    ( SPA-GMN-O04-SMEILIST-SEL    =   IDX0 )
                   MOVE    "T"             TO  O04-SELECT (IDX0)
               ELSE
                   MOVE    "F"             TO  O04-SELECT (IDX0)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-O04-MEIDSPCTRL      =   1 )
                   MOVE    "表示切替"      TO  O04-B05-LABEL
           END-IF
      *
      *    自費名称
           PERFORM     VARYING IDX0    FROM    1   BY  1      
                       UNTIL ( IDX0    >       CONST-JIHI-MAX )
               MOVE    SPA-GMN-O04-JIHIMSG(IDX0)
                                       TO  O04-JIHIMSG (IDX0)
           END-PERFORM
      *
      *    労災保険適用分
           MOVE    SPA-GMN-O04-RSISHOSHIN  TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-RSISHOSHIN
           MOVE    SPA-GMN-O04-RSISAISHIN  TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-RSISAISHIN
           MOVE    SPA-GMN-O04-RSISDO      TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-RSISDO
           MOVE    SPA-GMN-O04-RSIETC      TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-RSIETC
      *
           MOVE    SPA-GMN-O04-SKY1ST      TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-SKY1ST
           MOVE    SPA-GMN-O04-SKY2ND      TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-SKY2ND
           MOVE    SPA-GMN-O04-SKY3RD      TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-SKY3RD
           MOVE    SPA-GMN-O04-SKYSGK      TO  WRK-MONEYZ
           MOVE    WRK-MONEYZ              TO  O04-SKYSGK
           MOVE    SPA-GMN-O04-SKYSELNUM   TO  O04-SKYSELNUM
      *
           MOVE    SPA-GMN-O04-SKYLIST-MAX TO  O04-SKYLIST-COUNT
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-O04-SKYLIST-MAX )
      *
               MOVE    SPA-GMN-O04-TSK-RENNUM (IDX0)
                                       TO  WRK-Z9
               MOVE    WRK-Z9          TO  O04-TSK-RENNUM (IDX0)
               MOVE    SPA-GMN-O04-TSK-SKYKKN (IDX0)
                                       TO  O04-TSK-SKYKKN (IDX0)
               MOVE    SPA-GMN-O04-TSK-SRYKAMEI (IDX0)
                                       TO  O04-TSK-SRYKA (IDX0)
               MOVE    SPA-GMN-O04-TSK-HKNCOMBI (IDX0)
                                       TO  O04-TSK-HKNCOMBI (IDX0)
      *
               IF    ( SPA-GMN-O04-SKYLIST-SEL =   IDX0 )
                   MOVE    "T"         TO  O04-SKYLIST-SELECT (IDX0)
               ELSE
                   MOVE    "F"         TO  O04-SKYLIST-SELECT (IDX0)
               END-IF
      *
           END-PERFORM
      *
           MOVE    "black"         TO  O04-LBLGENMEN-STYLE
           IF    ( SPA-NAI-O04-DISCOUNT-KBN  NOT =   SPACE )
               MOVE    "blue"  TO  O04-LBLGENMEN-STYLE
               EVALUATE    SPA-NAI-O04-DISCOUNT-TEIRITU
               WHEN    "1"
                   MOVE    SPA-NAI-O04-DISCOUNT-RATE
                                   TO  WRK-ZZ9
                   STRING  "減免有（"  DELIMITED   BY  SIZE
                           WRK-ZZ9     DELIMITED   BY  SIZE
                           "％）"      DELIMITED   BY  SIZE
                   INTO    O04-LBLGENMEN
                   END-STRING
               WHEN    "2"
                   MOVE    SPA-NAI-O04-DISCOUNT-RATE
                                   TO  WRK-Z99
                   STRING  "減免有（"  DELIMITED   BY  SIZE
                           WRK-Z99     DELIMITED   BY  SIZE
                           "円）"      DELIMITED   BY  SIZE
                   INTO    O04-LBLGENMEN
                   END-STRING
               END-EVALUATE
           END-IF
      *
           MOVE    "black"             TO  O04-LBLSKYSGK-STYLE
                                           O04-SKYSGK-STYLE
           IF    ( SPA-NAI-O04-SAKKBN-MAX  >=  1 )
      *
               MOVE    SPA-GMN-O04-SKYSGK      TO  WRK-MONEY9
               MOVE    WRK-MONEY9              TO  O04-SKYSGK
      *
               MOVE    "blue"          TO  O04-LBLSKYSGK-STYLE
                                           O04-SKYSGK-STYLE
               MOVE    "１期"          TO  O04-LBLSKY1ST
               MOVE    "差額"          TO  O04-LBLSKYSGK
           END-IF
      *
           IF    ( SPA-NAI-O04-SAKKBN-MAX  >=  2 )
               MOVE    "２期"          TO  O04-LBLSKY2ND
           END-IF
      *
           IF    ( SPA-NAI-O04-SAKKBN-MAX  >=  3 )
               MOVE    "３期"          TO  O04-LBLSKY3RD
           END-IF
      *
           MOVE    "black"             TO  O04-SKYMONEYIN-STYLE
           MOVE    WIDGET-NORMAL       TO  O04-B06-STATE
                                           O04-B07-STATE
                                           O04-BTNCHOSEI1-STATE
                                           O04-BTNCHOSEI2-STATE
                                           O04-CHOSEI1IN-STATE
                                           O04-CHOSEI2IN-STATE
                                           O04-SKYMONEYIN-STATE
                                           O04-NYUKINHOUHOU-STATE
                                           O04-NYUKINHOUHOULST-STATE
      *
           IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-RRKSYU )
      *
               MOVE    "blue"          TO  O04-SKYMONEYIN-STYLE
      *
               IF    ( SPA-NAI-O04-DENPJTIKBN  =   "4" )
      *
                   MOVE    WIDGET-INSENSITIVE
                                       TO  O04-NYUKINHOUHOU-STATE
                                           O04-NYUKINHOUHOULST-STATE
               END-IF
      *
               IF    ( SPA-NAI-O04-PROC-FLG    =   1  OR 2 )
                   IF    ( SPA-NAI-O04-SYURRKNUM   =   ZERO )
                       MOVE    WIDGET-INSENSITIVE
                                       TO  O04-BTNCHOSEI1-STATE
                                           O04-BTNCHOSEI2-STATE
                                           O04-CHOSEI1IN-STATE
                                           O04-CHOSEI2IN-STATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-O04-SKYTBL-PAGE  <=   1 )
               MOVE    WIDGET-INSENSITIVE  TO  O04-B06-STATE
           END-IF
      *
           IF    ( SPA-NAI-O04-SKYTBL-NEXT   =   ZERO )
               MOVE    WIDGET-INSENSITIVE  TO  O04-B07-STATE
           END-IF
      *
           IF    ( SPA-GMN-O04-SYORIKBN      =   CONST-PROC-RRKSYU )
            AND  ( SPA-NAI-O04-PROC-FLG      =   3  OR 4)
               MOVE    WIDGET-INSENSITIVE
                                       TO  O04-BTNCHOSEI1-STATE
                                           O04-BTNCHOSEI2-STATE
                                           O04-CHOSEI1IN-STATE
                                           O04-CHOSEI2IN-STATE
                                           O04-SKYMONEYIN-STATE
           END-IF
      *
           IF    ( SPA-GMN-O04-SYORIKBN      =   CONST-PROC-NYUKIN )
               MOVE    WIDGET-INSENSITIVE
                                       TO  O04-BTNCHOSEI1-STATE
                                           O04-BTNCHOSEI2-STATE
                                           O04-CHOSEI1IN-STATE
                                           O04-CHOSEI2IN-STATE
                                           O04-SKYMONEYIN-STATE
           END-IF
      *
           EVALUATE    FLG-SMEILIST1-ROW
           WHEN    1
               MOVE    ZERO               TO  O04-CLIST1-ROW
               MOVE    ZERO               TO  O04-CLIST1-ROWATTR
           WHEN    2
               MOVE    O04-CLIST1-COUNT   TO  O04-CLIST1-ROW
               MOVE    1                  TO  O04-CLIST1-ROWATTR
           WHEN    OTHER
               MOVE    WRK-SMEILIST1-ROW  TO  O04-CLIST1-ROW
               MOVE    ZERO               TO  O04-CLIST1-ROWATTR
           END-EVALUATE
      *
           EVALUATE    FLG-SKYLIST-ROW
           WHEN    1
               MOVE    ZERO               TO  O04-SKYLIST-ROW
               MOVE    ZERO               TO  O04-SKYLIST-ROWATTR
           WHEN    OTHER
               MOVE    WRK-SKYLIST-ROW    TO  O04-SKYLIST-ROW
               MOVE    ZERO               TO  O04-SKYLIST-ROWATTR
           END-EVALUATE
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *************************************************************
      *    画面カーソルセット処理
      *************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
             AND ( SPA-GMN-O04-CUR         =   ZERO  )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 50012-CUR-SKIP-SEC
      *
           EVALUATE   SPA-GMN-O04-CUR
           WHEN   01
               MOVE  "PTNUM"           TO  MCP-WIDGET
           WHEN   02
               MOVE  "NAIYO"           TO  MCP-WIDGET
           WHEN   03
               MOVE  "CHOSEI1IN"       TO  MCP-WIDGET
           WHEN   04
               MOVE  "KINGAKU"         TO  MCP-WIDGET
           WHEN   05
               MOVE  "NYUKINYMD"       TO  MCP-WIDGET
           WHEN   06
               MOVE  "NYUKINHOUHOU"    TO  MCP-WIDGET
           WHEN   07
               MOVE  "SKYPRT"          TO  MCP-WIDGET
           WHEN   08
               MOVE  "RIREKIIN"        TO  MCP-WIDGET
           WHEN   09
               MOVE  "SKYSELNUM"       TO  MCP-WIDGET
           WHEN   10
               MOVE  "SRYPRT"          TO  MCP-WIDGET
           WHEN   11
               MOVE  "CHOSEI2IN"       TO  MCP-WIDGET
           WHEN   112
               MOVE  "B12"             TO  MCP-WIDGET
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
           WHEN    "PTNUM"
               MOVE    03              TO  SPA-GMN-O04-CUR
           WHEN    "NAIYO"
               MOVE    03              TO  SPA-GMN-O04-CUR
           WHEN    "CHOSEI1IN"
               MOVE    11              TO  SPA-GMN-O04-CUR
           WHEN    "CHOSEI2IN"
               MOVE    04              TO  SPA-GMN-O04-CUR
           WHEN    "KINGAKU"
               MOVE    06              TO  SPA-GMN-O04-CUR
           WHEN    "NYUKINHOUHOU"
               MOVE    05              TO  SPA-GMN-O04-CUR
           WHEN    "NYUKINYMD"
               MOVE    07              TO  SPA-GMN-O04-CUR
           WHEN    "SKYPRT"
               MOVE    10              TO  SPA-GMN-O04-CUR
           WHEN    "SRYPRT"
               MOVE    112             TO  SPA-GMN-O04-CUR
           WHEN    "RIREKIIN"
               MOVE    03              TO  SPA-GMN-O04-CUR
           WHEN    "SKYSELNUM"
               MOVE    03              TO  SPA-GMN-O04-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソルスキップ処理
      *****************************************************************
       50012-CUR-SKIP-SEC              SECTION.
      *
           IF    ( SPA-GMN-O04-CUR         =   3 )
               IF   ( O04-CHOSEI1IN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    11          TO  SPA-GMN-O04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-O04-CUR         =   11 )
               IF   ( O04-CHOSEI2IN-STATE  =   WIDGET-INSENSITIVE )
                   MOVE    4           TO  SPA-GMN-O04-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-O04-CUR             =   6 )
               IF   ( O04-NYUKINHOUHOU-STATE
                                           =   WIDGET-INSENSITIVE )
                   MOVE    5           TO  SPA-GMN-O04-CUR
               END-IF
           END-IF
      *
           .
      *
       50012-CUR-SKIP-EXT.
           EXIT.
      *************************************************************
      *    画面遷移処理
      *************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"           ALSO    "B01"
               PERFORM 210-BACK
      *    前回患者
           WHEN    "CLICKED"           ALSO    "B03"
               PERFORM 430-LAST-PTNUM-SET-SEC
      *    印刷
           WHEN    "CLICKED"           ALSO    "B11S"
               PERFORM 411-PRINT-SEC
      *    印刷
           WHEN    "CLICKED"           ALSO    "B12S"
               PERFORM 410-PRINT-SEC
      *    登録
           WHEN    "CLICKED"           ALSO    "B12"
               PERFORM 410-TOROKU-SEC
      *    明細表示区分
           WHEN    "CLICKED"           ALSO    "B05"
               PERFORM 450-MEIDSPKBN-SEC
      *    前ページ処理
           WHEN    "CLICKED"           ALSO    "B06"
               PERFORM 4101-PREV-PAGE-SEC
      *    次ページ処理
           WHEN    "CLICKED"           ALSO    "B07"
               PERFORM 4101-NEXT-PAGE-SEC
      *    氏名検索
           WHEN    "CLICKED"           ALSO    "B09"
               PERFORM 410-P97-SENI-SEC
      *    差額取込
           WHEN    "CLICKED"           ALSO    "BTNCHOSEI1"
               PERFORM 4101-BTNCHOSEI1-CHK-SEC
      *    差額取込
           WHEN    "CLICKED"           ALSO    "BTNCHOSEI2"
               PERFORM 4101-BTNCHOSEI2-CHK-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *************************************************************
      *    会話　処理
      *************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *
           WHEN    ANY             ALSO    "clist1"
               PERFORM 4100-SMEILIST-CHK-SEC
      *
           WHEN    ANY             ALSO    "SKYLIST"
               PERFORM 4100-SKYLIST-CHK-SEC
      *
           WHEN    ANY             ALSO    "PTNUM"
               PERFORM 4100-PTNUM-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "NAIYO"
               PERFORM 4100-SYORIKBN-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "RIREKIIN"
               PERFORM 4100-SELNUM-CHK-SEC
      *
           WHEN    "ACTIVATE"      ALSO    "SKYSELNUM"
               PERFORM 4100-SKYSELNUM-CHK-SEC
      *
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-O04-CUR
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
           MOVE    O04-CHOSEI1IN      TO  SPA-GMN-O04-CHOSEI1INX
           MOVE    O04-CHOSEI2IN      TO  SPA-GMN-O04-CHOSEI2INX
           MOVE    O04-KINGAKU        TO  SPA-GMN-O04-NYUKINX
           MOVE    O04-NYUKINYMD      TO  SPA-GMN-O04-NYUKINYMD
           MOVE    O04-NYUKINHOUHOU   TO  SPA-GMN-O04-NYKN-HOHO-G
           MOVE    O04-SKYPRT         TO  SPA-GMN-O04-SKYPRT-G
           MOVE    O04-SRYPRT         TO  SPA-GMN-O04-SRYPRT-G
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
           IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-RRKSYU )
               PERFORM 4102-RRKSYU-KIHON-CHK-SEC
           ELSE
               PERFORM 4102-SKY-NYKN-KIHON-CHK-SEC
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金入金処理時基本チェック処理
      *****************************************************************
       4102-SKY-NYKN-KIHON-CHK-SEC     SECTION.
      *
           PERFORM 41001-PTNUM-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI1IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI2IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYLIST-SEL-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-SKY-NYKN-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴修正処理時基本チェック処理
      *****************************************************************
       4102-RRKSYU-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41001-PTNUM-CHK-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-KINGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYLIST-SEL-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI1IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-CHOSEI2IN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-O04-DENPJTIKBN  =   "4" OR  "6" )
                   PERFORM 4102-NYUKIN-CHK-SEC
               ELSE
                   PERFORM 4101-NYUKIN-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYUKINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-NYKN-HOHO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SKYPRT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 4101-SRYPRT-CHK-SEC
           END-IF
      *
           .
       4102-RRKSYU-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納明細リストチェック
      *****************************************************************
       4100-SMEILIST-CHK-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-SMEILIST-SEL
                                       SPA-GMN-O04-SELNUM
      *
           IF    ( SPA-GMN-O04-SYORIKBN     =   CONST-PROC-RRKSYU )
               INITIALIZE              SPA-GMN-O04-INPUT-AREA
                                       SPA-NAI-O04-INPUT-AREA
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-O04-SMEILIST-MAX )
      *
               IF    ( O04-SELECT (IDX2)   =   "T" )
                   MOVE    IDX2    TO  SPA-GMN-O04-SMEILIST-SEL
                                       SPA-GMN-O04-SELNUM
                   IF    ( SPA-GMN-O04-SYORIKBN
                                           =   CONST-PROC-RRKSYU )
                       PERFORM 41001-INPUT-AREA-HEN-SEC
                   END-IF
      *
                   MOVE    3       TO  SPA-GMN-O04-CUR
      *
                   MOVE    SPA-GMN-O04-SMEILIST-MAX
                                   TO  IDX2
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-O04-SYORIKBN     =   CONST-PROC-RRKSYU )
               IF    ( SPA-NAI-O04-DENPJTIKBN
                                   =   " " OR "1" OR "2" OR "4" OR "6"
                                           OR "8" OR "9" )
                   CONTINUE
               ELSE
      *
                   MOVE    "0009"  TO  SPA-ERRCD
                   MOVE    8       TO  SPA-GMN-O04-CUR
      *
                   INITIALIZE          SPA-GMN-O04-INPUT-AREA
                                       SPA-NAI-O04-INPUT-AREA
      *
                   MOVE    SPA-SYSYMD  TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG TO  SPA-GMN-O04-NYUKINYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-O04-NYUKINYMD
      *
               END-IF
           END-IF
      *
           .
       4100-SMEILIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求期間リストチェック処理
      *****************************************************************
       4100-SKYLIST-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-O04-SKYLIST-SEL
                                           SPA-GMN-O04-SKYSELNUM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-O04-SKYLIST-MAX )
      *
      *
               IF    ( O04-SKYLIST-SELECT (IDX2)   =   "T" )
                   MOVE    IDX2        TO  SPA-GMN-O04-SKYLIST-SEL
                                           SPA-GMN-O04-SKYSELNUM
      *
                   PERFORM 4100-SYUNOU-DSP-SEC
      *
                   MOVE    SPA-GMN-O04-SKYLIST-MAX
                                   TO  IDX2
               END-IF
      *
           END-PERFORM
      *
           .
       4100-SKYLIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収納情報表示処理
      *****************************************************************
       4100-SYUNOU-DSP-SEC             SECTION.
      *
           MOVE    SPA-GMN-O04-SKYLIST-SEL TO  IDX4
      *
           MOVE    ZERO                TO  FLG-SRYYM-CHG
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-O04-DENPNUM =   ZERO )
           WHEN  ( SPA-NAI-O04-SRYYMD (1:6)
                               NOT =   SPA-NAI-O04-TSK-SRYYM (IDX4))
              OR ( SPA-NAI-O04-RRKNUM
                               NOT =   SPA-NAI-O04-TSK-RRKNUM(IDX4))
               MOVE    1               TO  FLG-SRYYM-CHG
           END-EVALUATE
      *
           MOVE    SPA-NAI-O04-TSK-SRYKAMEI  (IDX4)
                                       TO  SPA-GMN-O04-SRYKA
           MOVE    SPA-NAI-O04-TSK-SRYKACD   (IDX4)
                                       TO  SPA-NAI-O04-SRYKA
           MOVE    SPA-NAI-O04-TSK-DENPNUM (IDX4)
                                       TO  SPA-NAI-O04-DENPNUM
           MOVE    SPA-NAI-O04-TSK-RRKNUM (IDX4)
                                       TO  SPA-NAI-O04-RRKNUM
           MOVE    SPA-NAI-O04-TSK-SAKKBN  (IDX4)
                                       TO  SPA-NAI-O04-SAKKBN
           MOVE    SPA-NAI-O04-TSK-SAKKBN-MAX  (IDX4)
                                       TO  SPA-NAI-O04-SAKKBN-MAX
           MOVE    SPA-NAI-O04-TSK-SKYSTYMD(IDX4)
                                       TO  SPA-NAI-O04-SRYYMD
      *
      *
           PERFORM 3101-SYUNOU-SET-SEC
      *
           PERFORM 3101-SYUMEI-SET-SEC
      *
           PERFORM 3101-NYKN-HOHOLST-SET-SEC
      *
      *    MOVE    CONST-PROC-CHOSEI   TO  SPA-GMN-O04-SYORIKBN
           MOVE    CONST-PROC-RRKSYU   TO  SPA-GMN-O04-SYORIKBN
           PERFORM 4200-INPUT-AREA-CLEAR-SEC
      *
           PERFORM 4200-SAISIN-SKY-CHK-SEC
      *
           IF    ( SPA-GMN-O04-SMEILIST-SEL    =   ZERO )
               MOVE    8               TO  SPA-GMN-O04-CUR
           ELSE
               MOVE    3               TO  SPA-GMN-O04-CUR
           END-IF
      *
           IF    ( FLG-SRYYM-CHG       =   1 )
               PERFORM 3101-KBT-HEN-SEC
               PERFORM 3101-KBT-SAGAKU-HEN-SEC
           END-IF
      *
           .
       4100-SYUNOU-DSP-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ編集サブ呼出処理
      *****************************************************************
       41001-ORCSHKNSET-SEC            SECTION.
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    SPA-AREA        TO  WRK-SPA-AREA
      *
           MOVE    SYU-SRYYMD      TO  WRK-SPA-SRYYMD
           MOVE    SYU-SRYKA       TO  WRK-SPA-SRYKA
           MOVE    SYU-NYUGAIKBN   TO  WRK-SPA-NYUGAIKBN
           MOVE    SYU-PTID        TO  WRK-SPA-PTID
           MOVE    SPA-NAI-O04-BIRTHDAY
                                   TO  WRK-SPA-BIRTHDAY
           MOVE    SYU-HKNCOMBINUM TO  WRK-SPA-HKNCOMBINUM
           MOVE    "1"             TO  ORCSHKN-KBN
      *
           CALL    "ORCSHKNSET"        USING   WRK-SPA-AREA
                                               ORCSHKNSETAREA
           IF    ( ORCSHKN-ERRCD   NOT =   ZERO )
               INITIALIZE              ORCSHKNSETAREA
           END-IF
      *
           .
       41001-ORCSHKNSET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4100-PTNUM-CHK-SEC              SECTION.
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    O04-PTNUM           TO  SPA-GMN-O04-PTNUM
      *
           PERFORM 41001-PTNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-PTNUM-CHK-EXT
           END-IF
      *
      *    患者番号変換サブ呼び出し
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-GMN-O04-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           MOVE    SPTNUM-PTNUM        TO  SPA-GMN-O04-PTNUM
           EVALUATE    SPTNUM-RC
           WHEN    00
               MOVE    SPTNUM-PTNUM        TO  SPA-GMN-O04-PTNUM
               MOVE    SPTNUM-PTID         TO  SPA-NAI-O04-PTID
           WHEN   10
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-O04-CUR
               GO  TO  4100-PTNUM-CHK-EXT
           WHEN    20
      *        氏名検索へ
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPA-KYOUTU          TO  LNK-COMMON
               MOVE    SPA-O01FREE         TO  LNK-SCRSPA
      *
               MOVE    "O04"               TO  SPA-MOTOPG
               MOVE    "P97"               TO  SPA-SAKIPG
               MOVE    ZERO                TO  SPA-PTID
      *        患者氏名セット
               MOVE    SPA-GMN-O04-PTNUM   TO  SPA-NAME
               MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
               MOVE    "P97"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
               GO  TO  4100-PTNUM-CHK-EXT
           WHEN    OTHER
               MOVE    "0002"              TO  SPA-ERRCD
               GO  TO  4100-PTNUM-CHK-EXT
           END-EVALUATE
      *
      *    患者マスタ検索
           MOVE    ZERO                TO  FLG-PTINF
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-O04-PTID    TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
      *    患者マスタクローズ
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTINF           =   ZERO )
      *
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF  (   SPA-ERRCD   NOT =   SPACE )
                   GO  TO  4100-PTNUM-CHK-EXT
               END-IF
      *
      *        患者情報設定
               PERFORM 4101-PTINF-SET-SEC
      *
      *        前回患者
               MOVE    SPA-NAI-O04-PTID    TO  SPA-LAST-PTID
               MOVE    SPA-GMN-O04-PTNUM   TO  SPA-LAST-PTNUM
      *
               MOVE    1               TO  SPA-NAI-O04-SKYTBL-PAGE
      *
      *        請求期間コラムリスト編集
               PERFORM 4101-SKYLIST-HEN-SEC
      *
               MOVE    9               TO  SPA-GMN-O04-CUR
      *
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-O04-CUR
           END-IF
      *
           .
       4100-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       4101-PTINF-SET-SEC              SECTION.
      *
      *    患者ＩＤ
           MOVE    PTINF-PTID          TO  SPA-NAI-O04-PTID
      *
      *    患者氏名
           MOVE    PTINF-NAME          TO  SPA-GMN-O04-NAME
      *
      *    患者カナ氏名
           MOVE    PTINF-KANANAME      TO  SPA-GMN-O04-KANANAME
      *
      *    患者性別
           EVALUATE    PTINF-SEX
           WHEN    "1"
               MOVE    "男"            TO  SPA-GMN-O04-SEX
           WHEN    "2"
               MOVE    "女"            TO  SPA-GMN-O04-SEX
           END-EVALUATE
      *
      *    患者生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-O04-BIRTHDAY
      *
           MOVE    "1"                 TO  SPA-NAI-O04-NYUGAIKBN
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-O04-BIRTHDAY
      *
           .
       4101-PTINF-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求期間コラムリスト編集
      *****************************************************************
       4101-SKYLIST-HEN-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-O04-SKYTBL-AREA
                                           SPA-NAI-O04-SKYTBL-AREA
      *
           MOVE    ZERO                TO  CNT-PTTEIKIRRK
      *
           COMPUTE WRK-SKIP-MAX
               = ( SPA-NAI-O04-SKYTBL-PAGE -   1 )
               *   CONST-SKYLIST-MAX
      *
           PERFORM 900-PTTEIKIRRK-KEY8-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK  NOT =   ZERO )
                    OR   ( CNT-PTTEIKIRRK     >=   WRK-SKIP-MAX )
      *
               COMPUTE CNT-PTTEIKIRRK  =   CNT-PTTEIKIRRK  +   1
      *
               PERFORM 900-PTTEIKIRRK-KEY8-FET-SEC
      *
           END-PERFORM
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK  NOT =   ZERO )
                    OR   ( SPA-GMN-O04-SKYLIST-MAX
                                              >=   CONST-SKYLIST-MAX )
      *
               COMPUTE SPA-GMN-O04-SKYLIST-MAX
                   =   SPA-GMN-O04-SKYLIST-MAX +   1
      *
               MOVE    SPA-GMN-O04-SKYLIST-MAX TO  IDX-LST
      *
               MOVE    PTTEIKIRRK-SRYYM
                                   TO  SPA-NAI-O04-TSK-SRYYM (IDX-LST)
      *
               MOVE    PTTEIKIRRK-SKYSTYMD
                                   TO  SPA-NAI-O04-TSK-SKYSTYMD(IDX-LST)
      *
               MOVE    SPA-NAI-O04-NYUGAIKBN
                                           TO  WRK-SYU-NYUGAIKBN
               MOVE    PTTEIKIRRK-PTID     TO  WRK-SYU-PTID
               MOVE    PTTEIKIRRK-DENPNUM  TO  WRK-SYU-DENPNUM
               PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5010"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC        TO  SYS-5010-REC
      *
               EVALUATE    TRUE
               WHEN    SYS-5010-SEIKYU-DAY (1) =   99
                   MOVE    "1" TO  SPA-NAI-O04-TSK-SAKKBN-MAX
                                                           (IDX-LST)
               WHEN    SYS-5010-SEIKYU-DAY (2) =   99
                   MOVE    "2" TO  SPA-NAI-O04-TSK-SAKKBN-MAX
                                                           (IDX-LST)
               WHEN    OTHER
                   MOVE    "3" TO  SPA-NAI-O04-TSK-SAKKBN-MAX
                                                           (IDX-LST)
               END-EVALUATE
      *
      *
               MOVE    IDX-LST     TO  SPA-GMN-O04-TSK-RENNUM  (IDX-LST)
      *
               MOVE    PTTEIKIRRK-DENPNUM
                                   TO  SPA-NAI-O04-TSK-DENPNUM (IDX-LST)
               MOVE    PTTEIKIRRK-RRKNUM
                                   TO  SPA-NAI-O04-TSK-RRKNUM  (IDX-LST)
               IF    ( PTTEIKIRRK-SAKKBN   =   "9" )
                   MOVE    SPA-NAI-O04-TSK-SAKKBN-MAX  (IDX-LST)
                                   TO  SPA-NAI-O04-TSK-SAKKBN  (IDX-LST)
               ELSE
                   MOVE    PTTEIKIRRK-SAKKBN
                                   TO  SPA-NAI-O04-TSK-SAKKBN  (IDX-LST)
               END-IF
      *
      *
               INITIALIZE                  SYSKANRI-REC
               MOVE    "1005"              TO  SYS-KANRICD
               MOVE    SYU-SRYKA           TO  SYS-KBNCD
               MOVE    SYU-SRYYMD          TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME3
                               TO  SPA-GMN-O04-TSK-SRYKAMEI (IDX-LST)
               MOVE    SYS-1005-SRYKANAME1
                               TO  SPA-NAI-O04-TSK-SRYKAMEI (IDX-LST)
               MOVE    SYU-SRYKA
                               TO  SPA-NAI-O04-TSK-SRYKACD  (IDX-LST)
               MOVE    PTTEIKIRRK-HKNCOMBINUM
                               TO  SPA-GMN-O04-TSK-HKNCOMBI (IDX-LST)
      *
               MOVE    SPACE               TO  WRK-SKYKKN-G
               MOVE    PTTEIKIRRK-SRYYM    TO  WRK-SYMD
               MOVE    "01"                TO  WRK-SYMD (7:2)
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-SKYKKN-YM
               EVALUATE    SPA-NAI-O04-TSK-SAKKBN  (IDX-LST)
               WHEN    "1"
                   MOVE    " -1"           TO  WRK-SKYKKN-KBT
               WHEN    "2"
                   MOVE    " -2"           TO  WRK-SKYKKN-KBT
               WHEN    OTHER
                   MOVE    " -3"           TO  WRK-SKYKKN-KBT
               END-EVALUATE
      *
               IF    ( PTTEIKIRRK-SAKKBN   =   "9" )
                   MOVE    "*"             TO  WRK-SKYKKN-MRK
               END-IF
      *
               MOVE    WRK-SKYKKN-G
                                   TO  SPA-GMN-O04-TSK-SKYKKN (IDX-LST)
      *
               PERFORM 900-PTTEIKIRRK-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptteikirrk"        TO  MCP-TABLE
           MOVE    "key8"                  TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-PTTEIKIRRK  =   ZERO )
               MOVE    1               TO  SPA-NAI-O04-SKYTBL-NEXT
           END-IF
      *
           .
       4101-SKYLIST-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41001-PTNUM-CHK-SEC              SECTION.
      *
           IF    ( SPA-GMN-O04-PTNUM    =    SPACE )
               MOVE    "0001"      TO   SPA-ERRCD
               MOVE    1           TO   SPA-GMN-O04-CUR
               GO  TO  41001-PTNUM-CHK-EXT
           END-IF
      *
           .
       41001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       4100-SYORIKBN-CHK-SEC           SECTION.
      *
      *    MOVE    O04-NAIYO       TO  SPA-GMN-O04-SYORIKBN-G
      *
      *    EVALUATE SPA-GMN-O04-SYORIKBN
      *    WHEN    "1"
      *        MOVE    CONST-PROC-NYUKIN   TO  SPA-GMN-O04-SYORIKBN
      *    WHEN    "2"
      *        MOVE    CONST-PROC-RRKSYU   TO  SPA-GMN-O04-SYORIKBN
      *    WHEN    OTHER
      *        MOVE    CONST-PROC-CHOSEI   TO  SPA-GMN-O04-SYORIKBN
      *    END-EVALUATE
      *
      *    PERFORM 4200-INPUT-AREA-CLEAR-SEC
      *
           .
       4100-SYORIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求期間コラムリストチェック処理
      *****************************************************************
       4100-SKYSELNUM-CHK-SEC          SECTION.
      *
           MOVE    O04-SKYSELNUM   TO  SPA-GMN-O04-SKYSELNUM
      *
           PERFORM 41001-SKYSELNUM-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4100-SKYSELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-O04-SKYSELNUM
                                   TO  SPA-GMN-O04-SKYLIST-SEL
      *
           PERFORM 4100-SYUNOU-DSP-SEC
      *
           .
       4100-SKYSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求期間コラムリストチェック処理
      *****************************************************************
       41001-SKYSELNUM-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-O04-SKYSELNUM   =   ZERO )
                   MOVE    "0013"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-O04-CUR
           ELSE
               IF    ( SPA-GMN-O04-SKYSELNUM
                                           >   SPA-GMN-O04-SKYLIST-MAX )
                   MOVE    "0014"      TO  SPA-ERRCD
                   MOVE    9           TO  SPA-GMN-O04-CUR
               END-IF
           END-IF
      *
           .
       41001-SKYSELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-O04-SMEILIST-SEL
      *
           IF    ( SPA-GMN-O04-SYORIKBN     =   CONST-PROC-RRKSYU )
               INITIALIZE              SPA-GMN-O04-INPUT-AREA
                                       SPA-NAI-O04-INPUT-AREA
           END-IF
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-O04-NYUKINYMD
           MOVE    WRK-SYMD        TO  SPA-NAI-O04-NYUKINYMD
      *
           MOVE    O04-RIREKIIN   TO  SPA-GMN-O04-SELNUM
      *
           IF    ( SPA-GMN-O04-SELNUM  NOT =   ZERO )
               PERFORM 41001-SELNUM-CHK-SEC
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( SPA-GMN-O04-SYORIKBN
                                           =   CONST-PROC-RRKSYU )
                       PERFORM 41001-INPUT-AREA-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    3           TO  SPA-GMN-O04-CUR
           END-IF
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-O04-SELNUM  >=  1 )
            AND  ( SPA-GMN-O04-SELNUM  <=  SPA-GMN-O04-SMEILIST-MAX )
               MOVE   SPA-GMN-O04-SELNUM
                                   TO  SPA-GMN-O04-SMEILIST-SEL
           ELSE
               MOVE    8           TO  SPA-GMN-O04-CUR
               MOVE    "0005"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-O04-SYORIKBN     =   CONST-PROC-RRKSYU )
                   IF    ( SPA-NAI-O04-TJOUTAIKBN (SPA-GMN-O04-SELNUM)
                                   =   " " OR "1" OR "2" OR "4" OR "6"
                                           OR "8" OR "9" )
                       CONTINUE
                   ELSE
                       MOVE    "0009"  TO  SPA-ERRCD
                       MOVE    8       TO  SPA-GMN-O04-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力領域編集処理
      *****************************************************************
       41001-INPUT-AREA-HEN-SEC        SECTION.
      *
           MOVE    SPA-GMN-O04-TSKYMONEY (SPA-GMN-O04-SELNUM)
                                       TO  SPA-GMN-O04-SKYMONEYIN
                                           SPA-NAI-O04-SKYMONEY-MOTO
      *
           MOVE    SPA-NAI-O04-TCHOSEI1 (SPA-GMN-O04-SELNUM)
                                       TO  SPA-GMN-O04-CHOSEI1IN
                                           SPA-NAI-O04-CHOSEI1-MOTO
           MOVE    SPA-GMN-O04-CHOSEI1IN
                                       TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-O04-CHOSEI1INX
      *
           MOVE    SPA-NAI-O04-TCHOSEI2 (SPA-GMN-O04-SELNUM)
                                       TO  SPA-GMN-O04-CHOSEI2IN
                                           SPA-NAI-O04-CHOSEI2-MOTO
           MOVE    SPA-GMN-O04-CHOSEI2IN
                                       TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-O04-CHOSEI2INX
      *
           COMPUTE SPA-NAI-O04-SKYMONEY
               =   SPA-GMN-O04-SKYMONEYIN
               -   SPA-NAI-O04-CHOSEI1-MOTO
               -   SPA-NAI-O04-CHOSEI2-MOTO
      *
           MOVE    SPA-GMN-O04-TNYUKIN   (SPA-GMN-O04-SELNUM)
                                       TO  SPA-GMN-O04-NYUKIN
                                           SPA-NAI-O04-NYUKIN-MOTO
           MOVE    SPA-GMN-O04-NYUKIN  TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-O04-NYUKINX
      *
           MOVE    SPA-GMN-O04-TSYORIYMD (SPA-GMN-O04-SELNUM)
                                       TO  SPA-GMN-O04-NYUKINYMD
      *
           MOVE    SPA-NAI-O04-TNYKN-HOHO(SPA-GMN-O04-SELNUM)
                                       TO  WRK-CMB-CD
           PERFORM 4200-NYKN-HOHOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-O04-NYKN-HOHO-G
      *
           MOVE    SPA-NAI-O04-TSYORIYMD (SPA-GMN-O04-SELNUM)
                                       TO  SPA-NAI-O04-NYUKINYMD
           MOVE    SPA-GMN-O04-TDENPEDANUM(SPA-GMN-O04-SELNUM)
                                       TO  SPA-NAI-O04-DENPEDANUM
           MOVE    SPA-NAI-O04-TJOUTAIKBN(SPA-GMN-O04-SELNUM)
                                       TO  SPA-NAI-O04-DENPJTIKBN
           MOVE    SPA-NAI-O04-TSYURRKUPDKBN(SPA-GMN-O04-SELNUM)
                                       TO  SPA-NAI-O04-SYURRKUPDKBN
           MOVE    SPA-NAI-O04-TSYURRKNUM(SPA-GMN-O04-SELNUM)
                                       TO  SPA-NAI-O04-SYURRKNUM
           MOVE    SPA-NAI-O04-TSYUEDANUM(SPA-GMN-O04-SELNUM)
                                       TO  SPA-NAI-O04-SYUEDANUM
      *
           MOVE    ZERO                TO  SPA-NAI-O04-PROC-FLG
      *
           EVALUATE    SPA-NAI-O04-DENPJTIKBN
           WHEN    " "
           WHEN    "1"
           WHEN    "9"
               MOVE    1               TO  SPA-NAI-O04-PROC-FLG
           WHEN    "8"
               MOVE    2               TO  SPA-NAI-O04-PROC-FLG
           WHEN    "2"
               MOVE    3               TO  SPA-NAI-O04-PROC-FLG
           WHEN    "4"
           WHEN    "6"
               MOVE    4               TO  SPA-NAI-O04-PROC-FLG
           END-EVALUATE
      *
           .
       41001-INPUT-AREA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4101-SELNUM-CHK-SEC             SECTION.
      *
           PERFORM 41011-SELNUM-CHK-SEC
      *
           IF    ( SPA-ERRCD   NOT =   SPACE )
      *
               INITIALIZE              SPA-GMN-O04-INPUT-AREA
                                       SPA-NAI-O04-INPUT-AREA
      *
               MOVE    SPA-SYSYMD  TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-O04-NYUKINYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-O04-NYUKINYMD
           END-IF
      *
           .
       4101-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       41011-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-O04-SMEILIST-SEL
      *
           IF    ( SPA-GMN-O04-SELNUM      =   ZERO )
               MOVE    8           TO  SPA-GMN-O04-CUR
               MOVE    "0006"      TO  SPA-ERRCD
           ELSE
               PERFORM 41001-SELNUM-CHK-SEC
           END-IF
      *
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求期間リスト選択チェック処理
      *****************************************************************
       4101-SKYLIST-SEL-CHK-SEC        SECTION.
      *
           IF    ( SPA-GMN-O04-SKYLIST-SEL =   ZERO )
               MOVE    "0013"      TO  SPA-ERRCD
               MOVE    9           TO  SPA-GMN-O04-CUR
           END-IF
      *
           .
       4101-SKYLIST-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額欄妥当性チェック処理
      *****************************************************************
       4101-KINGAKU-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-O04-CHOSEI1IN
               MOVE    SPA-GMN-O04-CHOSEI1INX
                                       TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    3           TO  SPA-GMN-O04-CUR
                   MOVE    "0019"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-O04-CHOSEI1IN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-O04-CHOSEI1INX
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-O04-CHOSEI2IN
               MOVE    SPA-GMN-O04-CHOSEI2INX
                                   TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    11          TO  SPA-GMN-O04-CUR
                   MOVE    "0020"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-O04-CHOSEI2IN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-O04-CHOSEI2INX
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-O04-NYUKIN
               MOVE    SPA-GMN-O04-NYUKINX
                                       TO  WRK-KINGAKUX
               PERFORM 800-KINGAKU-HEN-SEC
               IF    ( FLG-ORCSNUMERR  =   1 )
                   MOVE    4           TO  SPA-GMN-O04-CUR
                   MOVE    "0021"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-KINGAKU9    TO  SPA-GMN-O04-NYUKIN
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-O04-NYUKINX
               END-IF
           END-IF
      *
           .
       4101-KINGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金１チェック処理
      *****************************************************************
       4101-CHOSEI1IN-CHK-SEC          SECTION.
      *
      *
           .
       4101-CHOSEI2IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    調整金２チェック処理
      *****************************************************************
       4101-CHOSEI2IN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-GMN-CUR
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-O04-CHOSEI1IN   NOT =   ZERO )
               MOVE    3               TO  WRK-GMN-CUR
           WHEN  ( SPA-GMN-O04-CHOSEI2IN   NOT =   ZERO )
               MOVE    11              TO  WRK-GMN-CUR
           END-EVALUATE
      *
           IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-RRKSYU )
               COMPUTE SPA-GMN-O04-SKYMONEYIN
                   =   SPA-NAI-O04-SKYMONEY
                   +   SPA-GMN-O04-CHOSEI1IN
                   +   SPA-GMN-O04-CHOSEI2IN
           ELSE
               COMPUTE SPA-GMN-O04-SKYMONEYIN
                   =   SPA-GMN-O04-CHOSEI1IN
                   +   SPA-GMN-O04-CHOSEI2IN
           END-IF
      *
           IF    ( SPA-GMN-O04-SKYMONEYIN  >   CONST-KINGAKU-MAX )
                   MOVE    "0023"      TO  SPA-ERRCD
                   MOVE    WRK-GMN-CUR TO  SPA-GMN-O04-CUR
           END-IF
      *
           .
       4101-CHOSEI2IN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理
      *****************************************************************
       4101-NYUKIN-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-NYUKIN
                                       WRK-MISYU-ZAN
      *
           IF    ( SPA-GMN-O04-SYORIKBN  =   CONST-PROC-RRKSYU )
      *
               COMPUTE WRK-MISYU-ZAN
                   =   SPA-GMN-O04-SKYMONEY
                   -   SPA-NAI-O04-SKYMONEY-MOTO
                   +   SPA-GMN-O04-SKYMONEYIN
                   -   SPA-GMN-O04-NYUKIN-TTL
      *
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-O04-NYUKIN-TTL
                   -   SPA-NAI-O04-NYUKIN-MOTO
                   +   SPA-GMN-O04-NYUKIN
           ELSE
      *
               COMPUTE WRK-MISYU-ZAN
                   =   SPA-GMN-O04-SKYMONEY
                   +   SPA-GMN-O04-SKYMONEYIN
                   -   SPA-GMN-O04-NYUKIN-TTL
      *
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-O04-NYUKIN-TTL
                   +   SPA-GMN-O04-NYUKIN
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-O04-NYUKIN  <  ZERO )
                   MOVE    4       TO  SPA-GMN-O04-CUR
                   MOVE    "0018"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  >   CONST-KINGAKU-MAX )
                   MOVE    2           TO  SPA-GMN-O04-CUR
                   MOVE    "0025"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  <  ZERO )
                   MOVE    4       TO  SPA-GMN-O04-CUR
                   MOVE    "0008"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-MCP-WIDGET  =   "KINGAKU" )
                   IF    ( SPA-GMN-O04-SYORIKBN
                                       NOT =   CONST-PROC-RRKSYU )
                    AND  ( SPA-GMN-O04-NYUKIN  =   ZERO )
                    AND  ( WRK-MISYU-ZAN       >   ZERO )
                       MOVE    WRK-MISYU-ZAN
                                   TO  SPA-GMN-O04-NYUKIN
                       MOVE    SPA-GMN-O04-NYUKIN
                                               TO  WRK-KINGAKUZ
                       MOVE    WRK-KINGAKUZ    TO  SPA-GMN-O04-NYUKINX
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理（入金取消、返金の履歴修正）
      *****************************************************************
       4102-NYUKIN-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-NYUKIN
      *
           IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-RRKSYU )
      *
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-O04-NYUKIN-TTL
                   -   SPA-NAI-O04-NYUKIN-MOTO
                   +   SPA-GMN-O04-NYUKIN
           ELSE
      *
               COMPUTE WRK-NYUKIN
                   =   SPA-GMN-O04-NYUKIN-TTL
                   +   SPA-GMN-O04-NYUKIN
           END-IF
      *
           IF    ( SPA-GMN-O04-NYUKIN  >   ZERO )
               MOVE    2           TO  SPA-GMN-O04-CUR
               MOVE    "0017"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  <  ZERO )
                   MOVE    2           TO  SPA-GMN-O04-CUR
                   MOVE    "0018"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-NYUKIN  >   CONST-KINGAKU-MAX )
                   MOVE    2           TO  SPA-GMN-O04-CUR
                   MOVE    "0025"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4102-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金日チェック処理
      *****************************************************************
       4101-NYUKINYMD-CHK-SEC          SECTION.
      *
           MOVE    SPACE                   TO  SPA-NAI-O04-NYUKINYMD
      *
           IF    ( SPA-GMN-O04-NYUKINYMD   =   SPACE )
               MOVE    SPA-SYSYMD          TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-O04-NYUKINYMD
               MOVE    WRK-SYMD            TO  SPA-NAI-O04-NYUKINYMD
           ELSE
               MOVE    SPA-GMN-O04-NYUKINYMD   TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               IF    ( FLG-HIZUKE-ERR      =   ZERO )
                   MOVE    WRK-HENYMDG     TO  SPA-GMN-O04-NYUKINYMD
                   MOVE    WRK-SYMD        TO  SPA-NAI-O04-NYUKINYMD
               ELSE
                   MOVE    5               TO  SPA-GMN-O04-CUR
                   MOVE    "0010"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101-NYUKINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金方法チェック処理
      *****************************************************************
       4101-NYKN-HOHO-CHK-SEC          SECTION.
      *
           IF    ( SPA-GMN-O04-NYKN-HOHO-G     =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPA-GMN-O04-NYKN-HOHO
                                       TO  WRK-CMB-CD
               PERFORM 4200-NYKN-HOHOLST-CHK-SEC
               IF    (  WRK-CMB-ITM    =   SPACE )
                   MOVE    6           TO  SPA-GMN-O04-CUR
                   MOVE    "0011"      TO  SPA-ERRCD
               ELSE
                   MOVE WRK-CMB-ITM    TO  SPA-GMN-O04-NYKN-HOHO-G
               END-IF
           END-IF
      *
           .
       4101-NYKN-HOHO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行コンボチェック処理
      *****************************************************************
       4101-SKYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-SKY
      *
           IF    ( SPA-GMN-O04-SKYPRT-G    =   SPACE )
               MOVE    SPA-GMN-O04-SKYPRTLST (IDX-SKY)
                                       TO  SPA-GMN-O04-SKYPRT-G
           ELSE
               MOVE    SPA-GMN-O04-SKYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SKYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-O04-SKYPRTLST (IDX-SKY)
                                       TO  SPA-GMN-O04-SKYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-O04-SKYPRT-G
               END-IF
           END-IF
      *
           .
       4101-SKYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行コンボチェック処理
      *****************************************************************
       4101-SRYPRT-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  IDX-SRY
      *
           IF    ( SPA-GMN-O04-SRYPRT-G    =   SPACE )
               MOVE    SPA-GMN-O04-SRYPRTLST (IDX-SRY)
                                       TO  SPA-GMN-O04-SRYPRT-G
           ELSE
               MOVE    SPA-GMN-O04-SRYPRT  TO  WRK-CMB-CD
               PERFORM 4200-SRYPRTLST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    SPA-GMN-O04-SRYPRTLST (IDX-SRY)
                                       TO  SPA-GMN-O04-SRYPRT-G
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-O04-SRYPRT-G
               END-IF
           END-IF
      *
           .
       4101-SRYPRT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       4101-PREV-PAGE-SEC              SECTION.
      *
           IF    ( SPA-NAI-O04-SKYTBL-PAGE >   1 )
               COMPUTE SPA-NAI-O04-SKYTBL-PAGE
                   =   SPA-NAI-O04-SKYTBL-PAGE -   1
               PERFORM 4101-SKYLIST-HEN-SEC
           END-IF
      *
           .
       4101-PREV-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       4101-NEXT-PAGE-SEC              SECTION.
      *
           IF    ( SPA-NAI-O04-SKYTBL-NEXT =   1 )
               COMPUTE SPA-NAI-O04-SKYTBL-PAGE
                   =   SPA-NAI-O04-SKYTBL-PAGE +   1
               PERFORM 4101-SKYLIST-HEN-SEC
           END-IF
      *
           .
       4101-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    差額ボタンチェック処理
      *****************************************************************
       4101-BTNCHOSEI1-CHK-SEC         SECTION.
      *
           IF    ( SPA-NAI-O04-PROC-FLG    =   1 OR 2 )
               COMPUTE WRK-SAGAKU
                   =   SPA-NAI-O04-CHOSEI1-MOTO
                   +   SPA-GMN-O04-SKYSGK
                   - ( SPA-GMN-O04-CHOSEI2IN
                   -   SPA-NAI-O04-CHOSEI2-MOTO )
      *
               COMPUTE WRK-SKYMONEY
                   =   SPA-GMN-O04-SKYMONEY
                   -   SPA-NAI-O04-SKYMONEY-MOTO
                   +   SPA-NAI-O04-SKYMONEY
                   +   WRK-SAGAKU
                   +   SPA-GMN-O04-CHOSEI2IN
      *
               IF    ( WRK-SKYMONEY   >=   ZERO )
                   COMPUTE    SPA-GMN-O04-CHOSEI1IN
                       =      WRK-SAGAKU
                   MOVE    WRK-SAGAKU      TO  WRK-KINGAKUZ
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-O04-CHOSEI1INX
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-O04-CHOSEI1IN
                   MOVE    SPACE       TO  SPA-GMN-O04-CHOSEI1INX
               END-IF
      *
               COMPUTE SPA-GMN-O04-SKYMONEYIN
                   =   SPA-NAI-O04-SKYMONEY
                   +   SPA-GMN-O04-CHOSEI1IN
                   +   SPA-GMN-O04-CHOSEI2IN
      *
           END-IF
      *
           .
       4101-BTNCHOSEI1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    差額ボタンチェック処理
      *****************************************************************
       4101-BTNCHOSEI2-CHK-SEC         SECTION.
      *
           IF    ( SPA-NAI-O04-PROC-FLG    =   1 OR 2 )
      *
               COMPUTE WRK-SAGAKU
                   =   SPA-NAI-O04-CHOSEI2-MOTO
                   +   SPA-GMN-O04-SKYSGK
                   - ( SPA-GMN-O04-CHOSEI1IN
                   -   SPA-NAI-O04-CHOSEI1-MOTO )
      *
               COMPUTE WRK-SKYMONEY
                   =   SPA-GMN-O04-SKYMONEY
                   -   SPA-NAI-O04-SKYMONEY-MOTO
                   +   SPA-NAI-O04-SKYMONEY
                   +   SPA-GMN-O04-CHOSEI1IN
                   +   WRK-SAGAKU
      *
               IF    ( WRK-SKYMONEY   >=   ZERO )
                   COMPUTE    SPA-GMN-O04-CHOSEI2IN
                       =      WRK-SAGAKU
                   MOVE    WRK-SAGAKU      TO  WRK-KINGAKUZ
                   MOVE    WRK-KINGAKUZ    TO  SPA-GMN-O04-CHOSEI2INX
               ELSE
                   MOVE    ZERO        TO  SPA-GMN-O04-CHOSEI2IN
                   MOVE    SPACE       TO  SPA-GMN-O04-CHOSEI2INX
               END-IF
      *
               COMPUTE SPA-GMN-O04-SKYMONEYIN
                   =   SPA-NAI-O04-SKYMONEY
                   +   SPA-GMN-O04-CHOSEI1IN
                   +   SPA-GMN-O04-CHOSEI2IN
           END-IF
      *
           .
       4101-BTNCHOSEI2-CHK-EXT.
           EXIT.
      *************************************************************
      *    印刷処理
      *************************************************************
       410-PRINT-SEC                   SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-GMN-O04-SELNUM  =   ZERO )
               MOVE    "0006"      TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-O04-SELNUM
                                  >=  1 )
                AND  ( SPA-GMN-O04-SELNUM
                                  <=  SPA-GMN-O04-SMEILIST-MAX )
                   MOVE    SPA-GMN-O04-TDENPEDANUM(SPA-GMN-O04-SELNUM)
                                   TO  WRK-SKYPRT-DENPEDANUM
      *
                   PERFORM 800-ORCSPRTCTRL-SEC
      *
                   PERFORM 4101-SKYPRT-PRINT-SEC
      *
                   PERFORM 800-ORCHCDUMMY-SEC
      *
               ELSE
                   MOVE    8       TO  SPA-GMN-O04-CUR
                   MOVE    "0005"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410-PRINT-EXT.
           EXIT.
      *************************************************************
      *    請求書兼領収書印刷処理
      *************************************************************
       4101-SKYPRT-PRINT-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-PRT-FLG
           MOVE    ZERO            TO  SPA-CLIENT-TEMP-FLG
      *    IF    ( SPA-CLIENT-PRT-FLG  =   1 )
               MOVE    "01"        TO  SPA-PRT-FLG
               MOVE    2           TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *
           INITIALIZE                  WRK-SKYPRT-SKYMONEY
                                       WRK-SKYPRT-NYUKIN
                                       WRK-SKYPRT-TAX-SAI
                                       WRK-KAKO-NYUKIN
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  SMEI-PTID
           MOVE    SPA-NAI-O04-DENPNUM TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
      *
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( SMEI-DENPEDANUM >   WRK-SKYPRT-DENPEDANUM )
      *
               COMPUTE WRK-SKYPRT-SKYMONEY
                   =   WRK-SKYPRT-SKYMONEY
                   +   SMEI-SKYMONEY
      *
               IF    ( SMEI-DENPEDANUM     =   WRK-SKYPRT-DENPEDANUM )
                   MOVE    SMEI-NYUHEN-MONEY
                                           TO  WRK-SKYPRT-NYUKIN
               END-IF
      *
               IF    ( SMEI-DENPEDANUM     <   WRK-SKYPRT-DENPEDANUM )
                   COMPUTE WRK-SKYPRT-SKYMONEY
                       =   WRK-SKYPRT-SKYMONEY
                       -   SMEI-NYUHEN-MONEY
      *
                   COMPUTE WRK-KAKO-NYUKIN
                       =   WRK-KAKO-NYUKIN
                       +   SMEI-NYUHEN-MONEY
               END-IF
      *
               IF    ( SMEI-SYURRKNUM      NOT =   ZERO )
      *
                   PERFORM 900-SYURRK-KEY3-SEL-SEC
      *
                   PERFORM UNTIL ( FLG-SYURRK  NOT =   ZERO )
      *
                       COMPUTE WRK-SKYPRT-TAX-SAI
                           =   WRK-SKYPRT-TAX-SAI
                           +   SYURRK-SKYMONEY-TAX-SAI
      *
                       PERFORM 900-SYURRK-KEY3-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_syurrk"        TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
               END-IF
      *
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF     ( MCP-RC         =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUMEI-REC
               ELSE
                   INITIALIZE              SYUMEI-REC
                   MOVE    1           TO  FLG-SYUMEI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-NAI-O04-SRYYMD  TO  WRK-SPA-SRYYMD
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  WRK-SPA-PTID
           MOVE    SPA-GMN-O04-PTNUM   TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-O04-NAME    TO  WRK-SPA-NAME
      *
      *    労災判定
           IF   (  SPA-NAI-O04-HKNNUM  =   "971" OR "973" )
               MOVE    1               TO  WRK-SPA-HKNKBN
           ELSE
               MOVE    ZERO            TO  WRK-SPA-HKNKBN
           END-IF
      *
           MOVE    SPA-NAI-O04-RSI-HKNKBN
                                       TO  WRK-SPA-RSI-HKNKBN
      *
           PERFORM 41011-PRINT-PG-SEC
      *
           INITIALIZE      ORCHCN03AREA
      *
           MOVE    WRK-SPA-HOSPNUM TO  ORCHCN03-HEAD-HOSPNUM
           MOVE    WRK-SPA-STAFFCD TO  ORCHCN03-STAFFCD
           MOVE    WRK-SPA-SYSYMD  TO  ORCHCN03-SYSYMD
           MOVE    WRK-SPA-TERMID  TO  ORCHCN03-TERMID
           MOVE    1               TO  ORCHCN03-CNT
           MOVE    1               TO  ORCHCN03-SKYPRTFLG
           MOVE    7               TO  ORCHCN03-MOTOID
      *
           MOVE    WRK-SPA-HOSPNUM  TO  ORCHCN03-HOSPNUM (1)
           MOVE    WRK-SPA-PTID    TO  ORCHCN03-PTID   (1)
           MOVE    WRK-SPA-PTNUM   TO  ORCHCN03-PTNUM  (1)
           MOVE    "1"             TO  ORCHCN03-NYUGAIKBN (1)
      *
           MOVE    SPA-NAI-O04-SRYYMD
                                   TO  ORCHCN03-KJNYMD  (1)
           MOVE    SPA-NAI-O04-DENPNUM
                                   TO  ORCHCN03-DENPNUM (1)
           IF    ( WRK-KAKO-NYUKIN >   ZERO )
               CONTINUE
           ELSE
               MOVE    WRK-SKYPRT-TAX-SAI
                                   TO  ORCHCN03-SEIKYU-TAX-SAI
                                                        (1)
           END-IF
           MOVE    WRK-SKYPRT-SKYMONEY
                                   TO  ORCHCN03-SEIKYU  (1)
           MOVE    WRK-SKYPRT-NYUKIN
                                   TO  ORCHCN03-NYUKIN  (1)
           MOVE    3               TO  ORCHCN03-HAKKOFLG(1)
           MOVE    WRK-SKYPRT-DENPEDANUM
                                   TO  ORCHCN03-DENPEDANUM (1)
      *
           CALL    WRK-SEIKYU-PG   USING   ORCHCN03AREA
                                           SPA-AREA
           MOVE    SPACE           TO  SPA-PRT-FLG
      *
           .
       4101-SKYPRT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       41011-PRINT-PG-SEC              SECTION.
      *
      *
      *    請求書
           MOVE    SPACE               TO  WRK-SEIKYU-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
           ELSE
              MOVE    "ORCHCN03"          TO  WRK-SEIKYU-PG
           END-IF
      *
           .
      *
       41011-PRINT-PG-EXT.
           EXIT.
      *************************************************************
      *    印刷処理
      *************************************************************
       411-PRINT-SEC                   SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    ZERO            TO  WRK-SRYPRT-DENPEDANUM
           IF    ( SPA-GMN-O04-SELNUM  >=  1 )
            AND  ( SPA-GMN-O04-SELNUM  <=  SPA-GMN-O04-SMEILIST-MAX )
               MOVE    SPA-GMN-O04-TDENPEDANUM(SPA-GMN-O04-SELNUM)
                                   TO  WRK-SRYPRT-DENPEDANUM
           END-IF
      *
           PERFORM 800-ORCSPRTCTRL-SEC
      *
           PERFORM 4111-SRYPRT-PRINT-SEC
      *
           PERFORM 800-ORCHCDUMMY-SEC
      *
           .
       411-PRINT-EXT.
           EXIT.
      *************************************************************
      *    明細書兼領収書印刷処理
      *************************************************************
       4111-SRYPRT-PRINT-SEC           SECTION.
      *
           MOVE    SPACE           TO  SPA-PRT-FLG
           MOVE    ZERO            TO  SPA-CLIENT-TEMP-FLG
      *    IF    ( SPA-CLIENT-PRT-FLG  =   1 )
               MOVE    "02"        TO  SPA-PRT-FLG
               MOVE    2           TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
           MOVE    SPA-NAI-O04-SRYYMD  TO  WRK-SPA-SRYYMD
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  WRK-SPA-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  WRK-SPA-PTID
           MOVE    SPA-GMN-O04-PTNUM   TO  WRK-SPA-PTNUM
           MOVE    SPA-GMN-O04-KANANAME TO WRK-SPA-KANANAME
           MOVE    SPA-GMN-O04-NAME    TO  WRK-SPA-NAME
      *
           PERFORM 41111-PRINT-PG-SEC
      *
      *
           INITIALIZE      ORCHCN04AREA
      *
           MOVE    WRK-SPA-PTID    TO  ORCHCN04-PTID
           MOVE    WRK-SPA-PTNUM   TO  ORCHCN04-PTNUM
           MOVE    WRK-SPA-NAME    TO  ORCHCN04-NAME
           MOVE    SPA-NAI-O04-DENPNUM
                                   TO  ORCHCN04-DENPNUM (1)
           MOVE    WRK-SRYPRT-DENPEDANUM
                                   TO  ORCHCN04-DENPEDANUM (1)
      *
           CALL    WRK-MEISAI-PG   USING   WRK-SPA-AREA
                                           ORCHCN04AREA
           MOVE    SPACE           TO  SPA-PRT-FLG
      *
           .
       4111-SRYPRT-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       41111-PRINT-PG-SEC              SECTION.
      *
      *    請求書
           MOVE    SPACE               TO  WRK-MEISAI-PG
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           WRK-SPA-AREA
                                           SYS-1035-REC
           IF    ( ORCSPRTNM-RC        =   ZERO )
              MOVE    ORCSPRTNM-PRTPG     TO  WRK-MEISAI-PG
           ELSE
              MOVE    "ORCHCN04"          TO  WRK-MEISAI-PG
           END-IF
      *
           .
      *
       41111-PRINT-PG-EXT.
           EXIT.
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       410-P97-SENI-SEC                SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-O01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "O04"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-PTNUM
           MOVE    ZERO                TO  SPA-PTID
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410-P97-SENI-EXT.
           EXIT.
      **************************************************************
      *    明細表示区分変更処理
      **************************************************************
       450-MEIDSPKBN-SEC                SECTION.
      *
           IF    ( SPA-NAI-O04-MEIDSPCTRL  =   "1" )
             IF    ( SPA-NAI-O04-MEIDSPKBN   =   " " )
               MOVE    "1"         TO  SPA-NAI-O04-MEIDSPKBN
             ELSE
               MOVE    " "         TO  SPA-NAI-O04-MEIDSPKBN
             END-IF
      *
             PERFORM 3101-SYUMEI-SET-SEC
             PERFORM 4200-INPUT-AREA-CLEAR-SEC
           END-IF
      *
           .
       450-MEIDSPKBN-EXT.
           EXIT.
      *************************************************************
      *    登録処理
      *************************************************************
       410-TOROKU-SEC                  SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4901-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    ZERO        TO  WRK-NYUKIN
                                       WRK-SKYMONEY
               IF    ( SPA-GMN-O04-SYORIKBN
                                   =   CONST-PROC-RRKSYU )
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-O04-SKYMONEY
                       -   SPA-NAI-O04-SKYMONEY-MOTO
                       +   SPA-GMN-O04-SKYMONEYIN
      *
                   COMPUTE WRK-NYUKIN
                       =   SPA-GMN-O04-NYUKIN-TTL
                       -   SPA-NAI-O04-NYUKIN-MOTO
                       +   SPA-GMN-O04-NYUKIN
               ELSE
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-O04-SKYMONEY
                       +   SPA-GMN-O04-SKYMONEYIN
      *
                   COMPUTE WRK-NYUKIN
                       =   SPA-GMN-O04-NYUKIN-TTL
                       +   SPA-GMN-O04-NYUKIN
               END-IF
      *
               IF    ( SPA-NAI-O04-PROC-FLG    =   4 )
                       PERFORM 4101-TOROKU-SEC
               ELSE
                   IF    ( WRK-SKYMONEY    <   WRK-NYUKIN )
                       MOVE    "2001"  TO  SPA-OIDCD
                   ELSE
                       PERFORM 4101-TOROKU-SEC
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       410-TOROKU-EXT.
           EXIT.
      *
      *************************************************************
      *    登録処理
      *************************************************************
       4101-TOROKU-SEC                 SECTION.
      *
           COMPUTE WRK-SAGAKU
               =   SPA-GMN-O04-SKYMONEYIN
               -   SPA-NAI-O04-SKYMONEY-MOTO
      *
           EVALUATE SPA-GMN-O04-SYORIKBN
           WHEN    CONST-PROC-CHOSEI
               PERFORM 4902-CHOSEI-KOUSIN-SEC
           WHEN    CONST-PROC-NYUKIN
               PERFORM 4902-NYUKIN-KOUSIN-SEC
           WHEN    CONST-PROC-RRKSYU
               PERFORM 4902-RRKSYU-KOUSIN-SEC
           END-EVALUATE
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               PERFORM 800-ORCSPRTCTRL-SEC
      *
               IF    ( SPA-GMN-O04-SKYPRT  =   "1" )
                   MOVE    SPA-NAI-O04-DENPEDANUM
                                   TO  WRK-SKYPRT-DENPEDANUM
                   PERFORM 4101-SKYPRT-PRINT-SEC
               END-IF
      *
               IF    ( SPA-GMN-O04-SRYPRT  =   "1" )
                   MOVE    SPA-NAI-O04-DENPEDANUM
                                   TO  WRK-SRYPRT-DENPEDANUM
                   PERFORM 4111-SRYPRT-PRINT-SEC
               END-IF
      *
               PERFORM 800-ORCHCDUMMY-SEC
      *
               PERFORM 3101-SYUNOU-SET-SEC
      *
               EVALUATE    SPA-NAI-O04-SAKKBN
               WHEN    "1"
                   COMPUTE SPA-GMN-O04-SKY1ST
                       =   SPA-GMN-O04-SKY1ST
                       -   WRK-SAGAKU
               WHEN    "2"
                   COMPUTE SPA-GMN-O04-SKY2ND
                       =   SPA-GMN-O04-SKY2ND
                       -   WRK-SAGAKU
               WHEN    OTHER
                   COMPUTE SPA-GMN-O04-SKY3RD
                       =   SPA-GMN-O04-SKY3RD
                       -   WRK-SAGAKU
               END-EVALUATE
      *
               PERFORM 3101-KBT-SAGAKU-HEN-SEC
      *
               PERFORM 3101-SYUMEI-SET-SEC
      *
               PERFORM 4200-INPUT-AREA-CLEAR-SEC
      *
               EVALUATE    SPA-GMN-O04-SYORIKBN
               WHEN    CONST-PROC-NYUKIN
                   MOVE    2       TO  FLG-SMEILIST1-ROW
                   MOVE    4       TO  SPA-GMN-O04-CUR
               WHEN    CONST-PROC-RRKSYU
                   MOVE    8       TO  SPA-GMN-O04-CUR
               END-EVALUATE
           END-IF
      *
           .
       4101-TOROKU-EXT.
           EXIT.
      *************************************************************
      *    関連チェック
      *************************************************************
       4901-KANRENCHK-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-O04-SYORIKBN
           WHEN    CONST-PROC-CHOSEI
               PERFORM 4901-CHOSEI-CHK-SEC
           WHEN    CONST-PROC-NYUKIN
               PERFORM 4901-NYUKIN-CHK-SEC
           WHEN    CONST-PROC-RRKSYU
               PERFORM 4901-RRKSYU-CHK-SEC
           END-EVALUATE
      *
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *************************************************************
      *    調整金処理時関連チェック処理
      *************************************************************
       4901-CHOSEI-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-O04-CHOSEI1IN   =   ZERO )
            AND  ( SPA-GMN-O04-CHOSEI2IN   =   ZERO )
               MOVE    3               TO  SPA-GMN-O04-CUR
               MOVE    "0016"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-O04-PROC-FLG    =   1  OR  2 )
      *
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-O04-SKYMONEY
                       +   SPA-GMN-O04-SKYMONEYIN
      *
                   IF    ( WRK-SKYMONEY    <   ZERO )
                       MOVE    "0015"  TO  SPA-ERRCD
                       IF    ( SPA-GMN-O04-CHOSEI2IN
                                   NOT =   ZERO )
                           MOVE    11  TO  SPA-GMN-O04-CUR
                       ELSE
                           MOVE    3   TO  SPA-GMN-O04-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-O04-NYUKINYMD   =   SPACE )
                   MOVE    5           TO  SPA-GMN-O04-CUR
                   MOVE    "0012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-O04-NYUKIN  >   ZERO )
                   IF    ( SPA-GMN-O04-NYKN-HOHO-G =   SPACE )
                       MOVE    6       TO  SPA-GMN-O04-CUR
                       MOVE    "0004"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4901-CHOSEI-CHK-EXT.
           EXIT.
      *************************************************************
      *    入金処理時関連チェック処理
      *************************************************************
       4901-NYUKIN-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-O04-NYUKIN      =   ZERO )
               MOVE    4               TO  SPA-GMN-O04-CUR
               MOVE    "0007"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-O04-NYUKINYMD   =   SPACE )
                   MOVE    5           TO  SPA-GMN-O04-CUR
                   MOVE    "0012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-O04-NYUKIN  >   ZERO )
                   IF    ( SPA-GMN-O04-NYKN-HOHO-G =   SPACE )
                       MOVE    6       TO  SPA-GMN-O04-CUR
                       MOVE    "0004"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4901-NYUKIN-CHK-EXT.
           EXIT.
      *************************************************************
      *    履歴修正処理時関連チェック処理
      *************************************************************
       4901-RRKSYU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-GMN-CUR
           EVALUATE    TRUE
           WHEN  ( SPA-GMN-O04-CHOSEI1IN   NOT =   ZERO )
               MOVE    3               TO  WRK-GMN-CUR
           WHEN  ( SPA-GMN-O04-CHOSEI2IN   NOT =   ZERO )
               MOVE    11              TO  WRK-GMN-CUR
           END-EVALUATE
      *
           IF    ( SPA-GMN-O04-SELNUM      =   ZERO )
               MOVE    8               TO  SPA-GMN-O04-CUR
               MOVE    "0006"          TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-O04-PROC-FLG    =   1  OR  2 )
      *
                   COMPUTE WRK-SKYMONEY
                       =   SPA-GMN-O04-SKYMONEY
                       -   SPA-NAI-O04-SKYMONEY-MOTO
                       +   SPA-GMN-O04-SKYMONEYIN
      *
                   IF    ( WRK-SKYMONEY    <   ZERO )
                       MOVE    "0015"  TO  SPA-ERRCD
                       MOVE    WRK-GMN-CUR TO  SPA-GMN-O04-CUR
                   END-IF
                   IF    ( SPA-ERRCD       =   SPACE )
                       IF    ( WRK-SKYMONEY    >   CONST-KINGAKU-MAX )
                           MOVE    "0024"  TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-NAI-O04-NYUKINYMD   =   SPACE )
                   MOVE    5           TO  SPA-GMN-O04-CUR
                   MOVE    "0012"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( SPA-GMN-O04-NYUKIN  >   ZERO )
                   IF    ( SPA-GMN-O04-NYKN-HOHO-G =   SPACE )
                       MOVE    6       TO  SPA-GMN-O04-CUR
                       MOVE    "0004"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4901-RRKSYU-CHK-EXT.
           EXIT.
      *************************************************************
      *    調整金処理
      *************************************************************
       4902-CHOSEI-KOUSIN-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-INS-SEC
           END-IF
      *
           .
       4902-CHOSEI-KOUSIN-EXT.
           EXIT.
      *************************************************************
      *    入金更新処理
      *************************************************************
       4902-NYUKIN-KOUSIN-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-INS-SEC
           END-IF
      *
           .
       4902-NYUKIN-KOUSIN-EXT.
           EXIT.
      *************************************************************
      *    履歴修正更新処理
      *************************************************************
       4902-RRKSYU-KOUSIN-SEC          SECTION.
      *
           PERFORM 49021-SYUNOU-UPD-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-SYUMEI-UPD-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 49021-PTTEIKIRRK-UPD-SEC
           END-IF
      *
           .
       4902-NYUKIN-RRKSYU-EXT.
           EXIT.
      *************************************************************
      *    収納マスタ更新処理
      *************************************************************
       49021-SYUNOU-UPD-SEC            SECTION.
      *
           MOVE    SPA-NAI-O04-NYUGAIKBN   TO  WRK-SYU-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID        TO  WRK-SYU-PTID
           MOVE    SPA-NAI-O04-DENPNUM     TO  WRK-SYU-DENPNUM
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
           IF    ( FLG-SYUNOU      NOT =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               EVALUATE    SPA-GMN-O04-SYORIKBN
               WHEN    CONST-PROC-CHOSEI
                   COMPUTE SYU-SKYMONEY
                       =   SYU-SKYMONEY
                       +   SPA-GMN-O04-SKYMONEYIN
                   COMPUTE SYU-CHOSEI1
                       =   SYU-CHOSEI1
                       +   SPA-GMN-O04-CHOSEI1IN
                   COMPUTE SYU-CHOSEI2
                       =   SYU-CHOSEI2
                       +   SPA-GMN-O04-CHOSEI2IN
                       COMPUTE SYU-CHOSEI
                           =   SYU-CHOSEI1
                           +   SYU-CHOSEI2
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       +   SPA-GMN-O04-NYUKIN
               WHEN    CONST-PROC-NYUKIN
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       +   SPA-GMN-O04-NYUKIN
               WHEN    CONST-PROC-RRKSYU
      *
                   IF    ( SPA-NAI-O04-PROC-FLG    =   1  OR  2 )
                       COMPUTE SYU-SKYMONEY
                           =   SYU-SKYMONEY
                           -   SPA-NAI-O04-SKYMONEY-MOTO
                           +   SPA-GMN-O04-SKYMONEYIN
                       COMPUTE SYU-CHOSEI1
                           =   SYU-CHOSEI1
                           -   SPA-NAI-O04-CHOSEI1-MOTO
                           +   SPA-GMN-O04-CHOSEI1IN
                       COMPUTE SYU-CHOSEI2
                           =   SYU-CHOSEI2
                           -   SPA-NAI-O04-CHOSEI2-MOTO
                           +   SPA-GMN-O04-CHOSEI2IN
                       COMPUTE SYU-CHOSEI
                           =   SYU-CHOSEI1
                           +   SYU-CHOSEI2
                   END-IF
      *
                   COMPUTE SYU-NYUKIN-TOTAL
                       =   SYU-NYUKIN-TOTAL
                       -   SPA-NAI-O04-NYUKIN-MOTO
                       +   SPA-GMN-O04-NYUKIN
      *
               END-EVALUATE
      *
               COMPUTE SYU-NYUKIN-KAISU    =   SYU-NYUKIN-KAISU    +   1
      *
               IF    ( SYU-SKYMONEY    =   ZERO )
                   MOVE    SPACE           TO  SYU-DENPJTIKBN
               ELSE
                   IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                       MOVE    "2"         TO  SYU-DENPJTIKBN
                   ELSE
                       MOVE    "1"         TO  SYU-DENPJTIKBN
                   END-IF
               END-IF
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               INITIALIZE                  SS002-AREA
               MOVE    "2"                 TO  SS002-UPDKBN
               MOVE    SPA-NAI-O04-SYURRKNUM
                                           TO  SS002-IN-SYURRKNUM
               MOVE    SPA-NAI-O04-SYUEDANUM
                                           TO  SS002-IN-SYUEDANUM
               MOVE    SPA-NAI-O04-SYURRKUPDKBN
                                           TO  SS002-IN-SYURRKUPDKBN
               CALL    "ORCSS002"  USING   SS002-AREA
                                           SYUNOU-REC
                                           SPA-AREA
               IF    ( SS002-RC    NOT =   ZERO )
                   GO  TO  49021-SYUNOU-UPD-EXT
               END-IF
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC              NOT =   ZERO )
                   MOVE    "1002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-SYUNOU-UPD-EXT.
           EXIT.
      *************************************************************
      *    収納明細作成処理
      *************************************************************
       49021-SYUMEI-INS-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  SMEI-PTID
           MOVE    SPA-NAI-O04-DENPNUM TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      NOT =   ZERO )
            OR   ( SMEI-DENPEDANUM    >=   CONST-SMEILIST-MAX )
               MOVE    "1003"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               COMPUTE WRK-DENPEDANUM  =   SMEI-DENPEDANUM +   1
      *
               INITIALIZE                  SYUMEI-REC
      *
               MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
               MOVE    SPA-NAI-O04-NYUGAIKBN
                                           TO  SMEI-NYUGAIKBN
               MOVE    SPA-NAI-O04-PTID    TO  SMEI-PTID
      *
      *        伝票番号
               MOVE    SPA-NAI-O04-DENPNUM TO  SMEI-DENPNUM
      *
      *        伝票番号枝番
               MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *
      *        入金連番
               MOVE    ZERO                TO  SMEI-NYUKINRENNUM
      *
      *        診療科
               MOVE    SPA-NAI-O04-SRYKA       TO  SMEI-SRYKA
      *
      *        伝票状態区分
               MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *
      *        請求発行日（発行日の西暦）
               MOVE    SPACE               TO  SMEI-SKYPRINTYMD
      *
      *        請求再発行日
               MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *
      *        領収書発行日
               MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *
      *        領収書再発行日
               MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *
      *        請求金額
               IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-CHOSEI )
                   MOVE    SPA-GMN-O04-SKYMONEYIN
                                           TO  SMEI-SKYMONEY
               ELSE
                   MOVE    ZERO            TO  SMEI-SKYMONEY
               END-IF
      *
      *        入返金区分
               IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-NYUKIN )
                   IF    ( SPA-GMN-O04-NYUKIN  >=  ZERO )
                       MOVE    "1"         TO  SMEI-NYUHEN-KBN
                   ELSE
                       MOVE    "2"         TO  SMEI-NYUHEN-KBN
                   END-IF
               ELSE
                       MOVE    SPACE       TO  SMEI-NYUHEN-KBN
               END-IF
      *
      *        入返金額
               MOVE    SPA-GMN-O04-NYUKIN  TO  SMEI-NYUHEN-MONEY
      *
      *        入返金日
               IF    ( SPA-NAI-O04-NYUKINYMD   =   SPACE )
                   MOVE    SPA-SYSYMD      TO  SMEI-NYUHEN-YMD
               ELSE
                   MOVE    SPA-NAI-O04-NYUKINYMD
                                           TO  SMEI-NYUHEN-YMD
               END-IF
      *
      *        入金方法
               IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-NYUKIN )
                   MOVE    SPA-GMN-O04-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
               ELSE
                   MOVE   "00"             TO  SMEI-NYUKIN-HOHO
               END-IF
      *
      *        状態区分
               IF    ( SPA-GMN-O04-SYORIKBN    =   CONST-PROC-NYUKIN )
                   IF    ( SPA-GMN-O04-NYUKIN  >=  ZERO )
                       MOVE    "2"         TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "4"         TO  SMEI-JOUTAIKBN
                   END-IF
               ELSE
                   MOVE    "8"             TO  SMEI-JOUTAIKBN
               END-IF
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
                                               SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
                                               SMEI-UPHMS
      *
               MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
               MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
               MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
               PERFORM 800-DAILYKEY-SEC
               MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    "1004"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-SYUMEI-INS-EXT.
           EXIT.
      *************************************************************
      *    収納明細更新処理
      *************************************************************
       49021-SYUMEI-UPD-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM          TO  SMEI-HOSPNUM
           MOVE    SPA-NAI-O04-NYUGAIKBN
                                       TO  SMEI-NYUGAIKBN
           MOVE    SPA-NAI-O04-PTID    TO  SMEI-PTID
           MOVE    SPA-NAI-O04-DENPNUM TO  SMEI-DENPNUM
           MOVE    SPA-NAI-O04-DENPEDANUM  TO  SMEI-DENPEDANUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =    ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               MOVE    1               TO  FLG-SYUMEI
               INITIALIZE                  SYUMEI-REC
           END-IF
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-SYUMEI      NOT =   ZERO )
               MOVE    "1005"          TO  SPA-ERRCD
               PERFORM 910-DBABORT-SEC
           ELSE
      *
               MOVE    SMEI-NYUHEN-YMD     TO  WRK-OLD-NYUHEN-YMD
      *
      *        請求金額
               IF    ( SPA-NAI-O04-PROC-FLG    = 1 OR 2 )
                   MOVE    SPA-GMN-O04-SKYMONEYIN
                                           TO  SMEI-SKYMONEY
               ELSE
                   MOVE    ZERO            TO  SMEI-SKYMONEY
               END-IF
      *
      *        入返金額
               MOVE    SPA-GMN-O04-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *
      *        入返金日
               IF    ( SPA-NAI-O04-NYUKINYMD   =   ZERO )
                   MOVE     SPA-SYSYMD     TO  SMEI-NYUHEN-YMD
               ELSE
                   MOVE     SPA-NAI-O04-NYUKINYMD
                                           TO  SMEI-NYUHEN-YMD
               END-IF
      *
      *        入金方法
               IF    ( SPA-GMN-O04-NYUKIN      >   ZERO )
                   MOVE  SPA-GMN-O04-NYKN-HOHO TO  SMEI-NYUKIN-HOHO
               ELSE
                   IF    ( SMEI-JOUTAIKBN  =   "6" )
                    AND  ( SPA-GMN-O04-NYUKIN  <   ZERO )
                       IF    ( SPA-GMN-O04-NYKN-HOHO   =   SPACE )
                           MOVE    "00"    TO  SMEI-NYUKIN-HOHO
                       ELSE
                           MOVE    SPA-GMN-O04-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
                       END-IF
                   ELSE
                           MOVE    "00"    TO  SMEI-NYUKIN-HOHO
                   END-IF
               END-IF
      *
               IF    ( SMEI-DENPEDANUM     =   1 )
                   MOVE  SPA-GMN-O04-NYKN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
               END-IF
      *
      *        伝票状態区分
               EVALUATE    SPA-NAI-O04-PROC-FLG
               WHEN    1
                   IF    ( SPA-GMN-O04-SKYMONEYIN   <=   ZERO )
                       MOVE    SPACE   TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "9"     TO  SMEI-JOUTAIKBN
                   END-IF
               WHEN    2
               WHEN    4
                   CONTINUE
               WHEN    OTHER
                   IF    ( SPA-GMN-O04-NYUKIN     >=   ZERO )
                       MOVE    "2"     TO  SMEI-JOUTAIKBN
                   ELSE
                       MOVE    "4"     TO  SMEI-JOUTAIKBN
                   END-IF
               END-EVALUATE
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
               IF    ( SMEI-NYUHEN-YMD     NOT =   WRK-OLD-NYUHEN-YMD )
                   MOVE    SMEI-UPYMD          TO  SDAILYKEY-CREYMD
                   MOVE    SMEI-UPHMS          TO  SDAILYKEY-CREHMS
                   MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
                   PERFORM 800-DAILYKEY-SEC
                   MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
               END-IF
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1006"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-SYUMEI-UPD-EXT.
           EXIT.
      *************************************************************
      *    定期請求履歴更新処理
      *************************************************************
       49021-PTTEIKIRRK-UPD-SEC        SECTION.
      *
           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
      *
           IF    ( PTTEIKIRRK-DENPEDANUM
                                       =   SPA-NAI-O04-DENPEDANUM )
               MOVE    SPA-GMN-O04-SKYMONEYIN
                                           TO  PTTEIKIRRK-SKYMONEY
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD        TO  PTTEIKIRRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTTEIKIRRK-UPHMS
               MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   MOVE    "1007"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49021-PTTEIKIRRK-UPD-EXT.
           EXIT.
      **************************************************************
      *    処理区分変更処理
      **************************************************************
       430-LAST-PTNUM-SET-SEC          SECTION.
      *
           IF    ( SPA-LAST-PTNUM  NOT =   SPACE )
               MOVE    SPA-LAST-PTNUM  TO  O04-PTNUM
               PERFORM 4100-PTNUM-CHK-SEC
           END-IF
      *
           .
       430-LAST-PTNUM-SET-EXT.
           EXIT.
      **************************************************************
      *    処理区分変更処理
      **************************************************************
       430-SYORIKBN-SEC                SECTION.
      *
      *    IF    ( SPA-NAI-O04-HEAD-DENPJTIKBN NOT =   "3" )
      *
      *        EVALUATE SPA-GMN-O04-SYORIKBN
      *
      *        WHEN    CONST-PROC-NYUKIN
      *            MOVE    CONST-PROC-RRKSYU   TO  SPA-GMN-O04-SYORIKBN
      *
      *        WHEN    CONST-PROC-RRKSYU
      *            MOVE    CONST-PROC-CHOSEI   TO  SPA-GMN-O04-SYORIKBN
      *
      *        WHEN    CONST-PROC-CHOSEI
      *            MOVE    CONST-PROC-NYUKIN   TO  SPA-GMN-O04-SYORIKBN
      *
      *        WHEN    OTHER
      *            MOVE    CONST-PROC-CHOSEI   TO  SPA-GMN-O04-SYORIKBN
      *        END-EVALUATE
      *
      *        PERFORM 4200-INPUT-AREA-CLEAR-SEC
      *
      *    END-IF
      *
           .
       430-SYORIKBN-EXT.
           EXIT.
      **************************************************************
      *    画面初期化処理
      **************************************************************
       420-CLEAR-SEC                   SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入力領域初期化処理
      *****************************************************************
       4200-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-O04-SMEILIST-SEL
                                       SPA-GMN-O04-SELNUM
      *
           MOVE    SPA-GMN-O04-SYORIKBN    TO  WRK-CMB-CD
           PERFORM 4200-SYORIKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM             TO  SPA-GMN-O04-SYORIKBN-G
      *
           INITIALIZE              SPA-GMN-O04-INPUT-AREA
                                   SPA-NAI-O04-INPUT-AREA
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-O04-NYUKINYMD
      *
           MOVE    SPA-GMN-O04-NYKN-HOHOLST (1)
                                   TO  SPA-GMN-O04-NYKN-HOHO-G
      *
           EVALUATE   SPA-GMN-O04-SYORIKBN
           WHEN   CONST-PROC-CHOSEI
           WHEN   CONST-PROC-NYUKIN
               MOVE    3               TO  SPA-GMN-O04-CUR
           WHEN   OTHER
               MOVE    8               TO  SPA-GMN-O04-CUR
           END-EVALUATE
      *
           MOVE    1               TO  FLG-SMEILIST1-ROW
      *
           .
       4200-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    最新請求データチェック処理
      *****************************************************************
       4200-SAISIN-SKY-CHK-SEC         SECTION.
      *
           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0        >   SPA-GMN-O04-SMEILIST-MAX )
                    OR   ( SPA-GMN-O04-SMEILIST-SEL
                                   NOT =   ZERO )
      *
               IF    ( PTTEIKIRRK-DENPEDANUM
                                   =   SPA-GMN-O04-TDENPEDANUM(IDX0))
                   MOVE    IDX0        TO  SPA-GMN-O04-SMEILIST-SEL
                                           SPA-GMN-O04-SELNUM
                   PERFORM 41001-INPUT-AREA-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SAISIN-SKY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理区分コンボチェック処理
      *****************************************************************
       4200-SYORIKBNLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O04-SYORIKBN-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O04-SYORIKBNL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O04-SYORIKBN-MAX )
               MOVE    SPA-GMN-O04-SYORIKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SYORIKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金コンボチェック処理
      *****************************************************************
       4200-NYKN-HOHOLST-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O04-NYKN-HOHO-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O04-NYKN-HOHOL (IDX-CMB))
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O04-NYKN-HOHO-MAX )
               MOVE    SPA-GMN-O04-NYKN-HOHOLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               MOVE    SPA-NAI-O04-NYKN-HOHOMEIL (IDX-CMB)
                                       TO  WRK-CMB-ITM2
               MOVE    SPA-NAI-O04-NYKN-BUNRUIL (IDX-CMB)
                                       TO  WRK-CMB-ITM3
           END-IF
      *
           .
       4200-NYKN-HOHOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書発行区分コンボチェック処理
      *****************************************************************
       4200-SKYPRTLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O04-SKYPRT-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O04-SKYPRTL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O04-SKYPRT-MAX )
               MOVE    SPA-GMN-O04-SKYPRTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SKYPRTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    明細書発行区分コンボチェック処理
      *****************************************************************
       4200-SRYPRTLST-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
               UNTIL ( IDX-CMB     >   SPA-GMN-O04-SRYPRT-MAX )
                OR   ( WRK-CMB-CD  =   SPA-GMN-O04-SRYPRTL (IDX-CMB) )
      *
               CONTINUE
      *
           END-PERFORM
      *
           IF    ( IDX-CMB     <=  SPA-GMN-O04-SRYPRT-MAX )
               MOVE    SPA-GMN-O04-SRYPRTLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SRYPRTLST-CHK-EXT.
           EXIT.
      *************************************************************
      *    エラーメッセージ表示理
      *************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "患者番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "患者番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "入金合計額が請求額を超えています"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "入金方法を入力して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "選択番号が未入力です"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "入金額を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "入金合計額が０円を下回ります"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "請求または入金データを選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "入金日入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "入金方法入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "入金日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "請求期間を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "請求期間選択番号入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "請求金額が０円を下回ります"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               MOVE    "調整金額を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0017"
               MOVE    "マイナスの金額を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "0018"
               MOVE    "入金額が０円を下回ります"
                                       TO  SPA-ERRMSG
           WHEN    "0019"
               MOVE    "調整金１入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "調整金２入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0021"
               MOVE    "入金額入力エラー"
                                       TO  SPA-ERRMSG
           WHEN    "0023"
                MOVE    "今回請求金額が上限を超えます"
                                        TO  SPA-ERRMSG
           WHEN    "0024"
                MOVE    "請求金額が上限を超えます"
                                        TO  SPA-ERRMSG
           WHEN    "0025"
                MOVE    "入金額が上限を超えます"
                                        TO  SPA-ERRMSG
           WHEN    "1001"
           WHEN    "1002"
               MOVE    "収納の更新処理でエラーになりました"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
           WHEN    "1004"
           WHEN    "1005"
           WHEN    "1006"
               MOVE    "収納明細の更新処理でエラーになりました"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "定期請求履歴の更新処理でエラーになりました"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "負担金の差額計算に失敗しました"
               
                                       TO  SPA-ERRMSG
           WHEN    "9999"
               MOVE    "他端末で使用中です。"
                                       TO  SPA-ERRMSG
               MOVE    SLOCK-MSG       TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  OERR
           MOVE    SPA-ERRCD           TO  OERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  OERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "O04"               TO  SPA-MOTOPG
           MOVE    "OERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "OERR"              TO  MCP-WINDOW
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
      *    エラーメッセージ表示理
      *****************************************************************
       520-OIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  WRK-OIDMSG
      *
           EVALUATE    SPA-OIDCD
           WHEN    "2001"
               MOVE    "過入金状態になりますがよろしいですか"
                                        TO  WRK-OIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  OID1
           INITIALIZE                       OID1
           MOVE    SPA-OIDCD            TO  OID1-ID1CODE
           MOVE    WRK-OIDMSG           TO  OID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "O04"                TO  SPA-MOTOPG
           MOVE    "OID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "OID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-OIDSET-EXT.
           EXIT.
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HIZUKE-ERR
           MOVE    SPACE               TO  WRK-HENYMDG
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
           IF  ( SGDAY-RC              =   ZERO )
               MOVE    SGDAY-OTDAY     TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY      TO  WRK-SYMD
           ELSE
               MOVE    1               TO  FLG-HIZUKE-ERR
           END-IF
      *
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
      *
       800-DAILYKEY-EXT.
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
      *    金額数値変換処理
      *****************************************************************
       800-KINGAKU-HEN-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-ORCSNUMERR
      *
           INITIALIZE                  WRK-KINGAKU9
                                       WRK-KINGAKUZ
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-KINGAKUX    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-SEISUU         >   7     )
            OR   ( SNUM-SYOKBN         =   "1"   )
               MOVE    1               TO  FLG-ORCSNUMERR
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKU9
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKUZ
           END-IF
      *
           .
       800-KINGAKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷制御情報取得処理
      *****************************************************************
       800-ORCSPRTCTRL-SEC             SECTION.
      *
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               INITIALIZE                  ORCSPRTCTRLAREA
               CALL    "ORCSPRTCTRL"       USING
                                           SPA-AREA
                                           ORCSPRTCTRLAREA
                                           MCPAREA
           END-IF
      *
           .
       800-ORCSPRTCTRL-EXT.
           EXIT.
      *****************************************************************
      *    ダミー印刷処理
      *****************************************************************
       800-ORCHCDUMMY-SEC              SECTION.
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                    TO  SPA-CLIENT-TEMP-FLG
      *
           .
       800-ORCHCDUMMY-EXT.
           EXIT.
      *************************************************************
      *    患者定期請求履歴検索処理
      *************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-O04-PTID    TO  PTTEIKIRRK-PTID
           MOVE    SPA-NAI-O04-DENPNUM TO  PTTEIKIRRK-DENPNUM
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *************************************************************
      *    患者定期請求履歴検索処理
      *************************************************************
       900-PTTEIKIRRK-KEY8-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SPA-NAI-O04-PTID    TO  PTTEIKIRRK-PTID
           MOVE    "tbl_ptteikirrk"        TO  MCP-TABLE
           MOVE    "key8"                  TO  MCP-PATHNAME
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY8-SEL-EXT.
           EXIT.
      *************************************************************
      *    システム管理検索処理(KEY10)
      *************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *************************************************************
      *    患者定期請求履歴検索処理
      *************************************************************
       900-PTTEIKIRRK-KEY8-FET-SEC     SECTION.
      *
           MOVE    "tbl_ptteikirrk"        TO  MCP-TABLE
           MOVE    "key8"                  TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
               MOVE    ZERO            TO  FLG-PTTEIKIRRK
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-FETCH-SEC          SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    ZERO        TO  FLG-SYSKANRI
           ELSE
               INITIALIZE              MCPDATA-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-FETCH-EXT.
           EXIT.
      *****************************************************************
      *    収納マスタ読込
      *****************************************************************
       900-SYUNOU-FETCH-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              MCPDATA-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-FETCH-EXT.
           EXIT.
      *
      *************************************************************
      *    収納マスター検索処理
      *************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
      *
           MOVE    WRK-SYU-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    WRK-SYU-PTID        TO  SYU-PTID
           MOVE    WRK-SYU-DENPNUM     TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SMEI-SYUEDANUM      TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴検索処理
      *************************************************************
       900-SYURRK-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           .
       900-SYURRK-KEY3-SEL-EXT.
           EXIT.
      *************************************************************
      *    収納履歴FETCH処理
      *************************************************************
       900-SYURRK-KEY3-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           .
       900-SYURRK-KEY3-FET-EXT.
           EXIT.
      *************************************************************
      *    トランザクションキャンセル処理
      *************************************************************
       910-DBABORT-SEC               SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "abort"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
      *
           .
       910-DBABORT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-NAI-O04-PTID    TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF    ( SLOCK-RETURN    NOT =   ZERO )
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *************************************************************
      *    ＰＵＴ　処理
      *************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
