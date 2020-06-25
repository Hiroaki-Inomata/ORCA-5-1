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
       PROGRAM-ID.                               ORCGW21.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 病室管理
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/08/20    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 00.00.01     NACL−太田   02/11/29  有効開始日を基準にシス管の
      *                                     情報を取得するよう修正
      * 00.00.02     NACL−太田   03/01/29  有効開始日のチェック追加
      *                                     入院中患者チェック追加
      * 00.00.03     NACL-太田    03/06/10  重症者等療養環境特別加算、
      *                                     無菌治療室管理加算を追加
      * 01.00.01     NACL-太田    03.11.17  更新日付編集対応
      * 03.05.00     NACL-太田    07.05.18  グループ診療対応
      * 04.01.00     NACL-太田    07/11/20  室料差額６桁対応
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
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *
      *    コンスタント値
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "W20SCR-SPA".
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-NYUKHN                          PIC 9(01).
           03  FLG-ENTRYCHK                        PIC 9(01).
           03  FLG-BRM-NUM                         PIC 9(01).
           03  FLG-KIKAN-DABURI                    PIC 9(01).
           03  FLG-UPD-KBN                         PIC 9(01).
           03  FLG-TENSU                           PIC 9(01).
           03  FLG-PTNYUINRRK                      PIC 9(01).
           03  FLG-SYS5002                         PIC 9(01).
           03  FLG-TARGET                          PIC 9(01).
           03  FLG-INPUT-AREA                      PIC 9(01).
           03  FLG-SSTKIJUNERR                     PIC 9(01).
           03  FLG-KSNERR                          PIC 9(01).
           03  FLG-HIT                             PIC 9(01).
           03  FLG-NYUKSNCHK                       PIC 9(01).
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
           03  IDX7                                PIC 9(05).
           03  IDXA                                PIC 9(05).
           03  IDXB                                PIC 9(05).
           03  IDXL                                PIC 9(05).
           03  IDXS                                PIC 9(05).
           03  IDX-CMB                             PIC 9(05).
           03  IDX-BRMLST                          PIC 9(05).
           03  IDX-KSN                             PIC 9(05).
           03  IDX-GKSN                            PIC 9(05).
           03  IDX-SST                             PIC 9(05).
           03  IDX-MUL                             PIC 9(05).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-SST                             PIC 9(05).
           03  CNT-SEL                             PIC 9(05).
           03  CNT-KSN                             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET                      PIC X(64). 
           03  WRK-WIDMSG                          PIC X(70). 
           03  WRK-MSGPLUS                         PIC X(20). 
           03  WRK-MAX                             PIC 9(05).
           03  WRK-YMD                             PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY                         PIC 9(04).
               05  WRK-SMM                         PIC 9(02).
               05  WRK-SDD                         PIC 9(02).
           03  WRK-HENYMDG                         PIC X(09).
           03  WRK-KANACHK-INPUT               PIC X(400).
           03  WRK-SYS5002-KBNCD.
               05  WRK-SYS5002-BTUNUM             PIC X(02).
               05  WRK-SYS5002-BRMNUM             PIC X(06).
           03  WRK-ZZZ9                            PIC ZZZ9.
           03  WRK-999999                          PIC 9(06).
           03  WRK-ZZZZZ9                          PIC Z(5)9.
           03  WRK-ZZZZZZZ9                        PIC Z(7)9.
           03  WRK-CMB-CD                          PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM                     PIC X(200).
               05  WRK-CMB-ITM2                    PIC X(200).
               05  WRK-CMB-ITM3                    PIC X(200).
               05  WRK-CMB-ITM4                    PIC X(200).
               05  WRK-CMB-ITM5                    PIC X(200).
           03  WRK-KJNYMD                          PIC X(08).
           03  WRK-STR1                            PIC X(20).
           03  WRK-STR2                            PIC X(20).
           03  WRK-STR-TMP                         PIC X(100).
           03  WRK-IDX1                            PIC 9(05).
           03  WRK-IDX2                            PIC 9(05).
           03  WRK-IDX3                            PIC 9(05).
           03  WRK-BRMNUM                          PIC X(06).
           03  WRK-BRMNUM-KEY                      PIC X(06).
           03  WRK-BRMNUM-IN                       PIC X(06).
           03  WRK-BRMNUM-OT                       PIC X(06).
           03  WRK-TOKNYUIN                        PIC X(02).
           03  WRK-KHNSRYCD                        PIC X(09).
           03  WRK-KJN-YUKSTYMD                    PIC X(08).
           03  WRK-BTU-YUKSTYMD                    PIC X(08).
           03  WRK-YUKOSTYMD                       PIC X(08).
           03  WRK-YUKOEDYMD                       PIC X(08).
           03  WRK-TNSYMD                          PIC X(08).
           03  WRK-TNSSRYCD                        PIC X(09).
           03  WRK-HEN-SAGAKU.
               05  WRK-HEN-SAGAKU-Z9               PIC ZZZZZ9.
               05  WRK-HEN-SAGAKU-EN               PIC X(02).
           03  WRK-KSNCD                           PIC X(09).
           03  WRK-KSNIDX                          PIC 9(03).
           03  WRK-KSNKBN                          PIC 9(03).
           03  WRK-NYUINKBN                        PIC 9(03).
           03  WRK-SSTKJNCD-G.
               05  WRK-SSTKJNCD-OCC                OCCURS 10.
                   07  WRK-SSTKJNCDSEP             PIC X(01).
                   07  WRK-SSTKJNCD                PIC X(04).
                   07  WRK-SSTKJNCD-R              REDEFINES
                       WRK-SSTKJNCD.
                       09  WRK-SSTKJNCD9           PIC 9(04).
           03  WRK-KSNMEI                          PIC X(100).
           03  WRK-PNT                             PIC 9(03).
           03  WRK-BTUNUM                          PIC X(02).
           03  WRK-SELNUM-G.
               05  WRK-SELNUM-MUL                  PIC X(50).
               05  WRK-INPUT-MODE                  PIC 9(01).
               05  WRK-SELNUM-X                    PIC X(50).
               05  WRK-SELNUM                      PIC 9(03).
               05  WRK-SELNUM-FLG                  PIC 9(01)
                                                   OCCURS  200.
           03  WRK-MULTI-AREA.
               05  WRK-MUL-NUM             PIC X(10)
                                           OCCURS 20.
               05  WRK-MUL-TENSU-X         PIC X(10).
               05  WRK-MUL-STNUM-X         PIC X(10).
               05  WRK-MUL-EDNUM-X         PIC X(10).
               05  WRK-MUL-TENSU           PIC 9(05).
               05  WRK-MUL-STNUM           PIC 9(05).
               05  WRK-MUL-EDNUM           PIC 9(05).
           03  WRK-ZZZ-G.
               05  WRK-ZZZ                 PIC ZZZ.
           03  WRK-STIDX                   PIC 9(05).
           03  WRK-YUKOSTYMD-MOT           PIC X(08).
           03  WRK-NUM02X.
               05  WRK-NUM02                       PIC 9(02).
           03  WRK-FROM                            PIC 9(05).
           03  WRK-KBN1-G.
               05  WRK-KBN1                        PIC 9(01).
           03  WRK-KBN2-G.
               05  WRK-KBN2                        PIC 9(02).
           03  WRK-KBN4-G.
               05  WRK-KBN4                        PIC 9(04).
           03  WRK-TOKCHK1-G.
               05  WRK-TOKCHK1-KBNCD               PIC X(02)
                                                   OCCURS 100.
           03  WRK-TOKCHK2-G.
               05  WRK-TOKCHK2-KBNCD               PIC X(02)
                                                   OCCURS 100.
           03  WRK-EDTYMD1                         PIC X(10).
           03  WRK-EDTYMD3                         PIC X(22).
      *
       01  TKSN-AREA.
           03  TKSN-MAX                            PIC 9(03).
           03  TKSN-OCC                            OCCURS  7.
               05  TKSN-LBL                        PIC X(36).
               05  TKSN-IDX                        PIC 9(03).
               05  TKSN-SRYCD                      PIC X(09).
      *
       01  SSTKJN-AREA.
           03  SSTKJN-VAL.
               05  SSTKJN-VAL-OCC                  PIC X(500)
                                                   OCCURS  20.
           03  SSTKJN-R    REDEFINES       SSTKJN-VAL.
               05  SSTKJN-CD                       PIC X(01)
                                                   OCCURS  10000.
      *
       01  TSPLIT-CONF-DATE-AREA.
           03  TSPLIT-CONF-DATE-OCC                OCCURS  4.
               05  TSPLIT-CONF-DATE-BEFORE         PIC 9(01).
               05  TSPLIT-CONF-DATE-AFTER          PIC 9(01).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-SPLIT-CONF-DATE-VAL.
               05  CONST-H200401            PIC X(08) VALUE "20080401".
               05  CONST-H220401            PIC X(08) VALUE "20100401".
               05  CONST-H240401            PIC X(08) VALUE "20120401".
               05  CONST-R020401            PIC X(08) VALUE "20200401".
           03  CONST-SPLIT-CONF-DATE-R REDEFINES
                                            CONST-SPLIT-CONF-DATE-VAL.
               05  CONST-SPLIT-CONF-DATE    PIC X(08) OCCURS 4.
           03  CONST-SPLIT-CONF-DATE-MAX    PIC 9(03) VALUE  4.
           03  CONST-H180401                PIC X(08) VALUE "20060401".
           03  CONST-H180701                PIC X(08) VALUE "20060701".
           03  CONST-H250401                PIC X(08) VALUE "20130401".
           03  CONST-H241001                PIC X(08) VALUE "20121001".
           03  CONST-KSN-MAX                PIC 9(03) VALUE 11.
           03  CONST-GKSN-MAX               PIC 9(03) VALUE 14.
           03  CONST-5002-KSN-MAX           PIC 9(03) VALUE 12.
           03  CONST-TOKNYUIN-MAX           PIC 9(03) VALUE 50.
           03  CONST-R02-AREA.
               05   CONST-R02-A301C5        PIC X(09) VALUE "190219270".
           03  CONST-H24-AREA.
               05   CONST-H2404-BTURYO1     PIC X(09) VALUE "190120210".
               05   CONST-H2404-BTURYO2     PIC X(09) VALUE "190106070".
               05   CONST-H2404-BTURYO3     PIC X(09) VALUE "190106170".
               05   CONST-H2404-BTURYO4     PIC X(09) VALUE "190106270".
               05   CONST-H2404-SNRRYO1     PIC X(09) VALUE "190106370".
               05   CONST-H2404-SNRRYO2     PIC X(09) VALUE "190106470".
               05   CONST-H2404-BTURYOKAI1  PIC X(09) VALUE "190146210".
               05   CONST-H2404-BTURYOKAI2  PIC X(09) VALUE "190146310".
               05   CONST-H2404-SNRRYOKAI   PIC X(09) VALUE "190146410".
               05   CONST-H2404-MUKIN1      PIC X(09) VALUE "190106570".
               05   CONST-H2404-MUKIN1K     PIC X(09) VALUE "190713410".
               05   CONST-H2404-MUKIN2      PIC X(09) VALUE "190146510".
           03  CONST-SINSEIJI          PIC X(09) VALUE "190109770".
           03  CONST-SNR-RYOYO-VAL.
               05                      PIC X(09) VALUE "190106370".
               05                      PIC X(09) VALUE "190106470".
           03  CONST-SNR-RYOYO-R REDEFINES   CONST-SNR-RYOYO-VAL.
               05  CONST-SNR-RYOYO
                                       PIC X(09) OCCURS 2.
           03  CONST-HOSYASEN          PIC X(09) VALUE "190106670".
           03  CONST-RYOYO             PIC X(09) VALUE "190105570".
           03  CONST-BTU-RYOYO-VAL.
               05                      PIC X(09) VALUE "190106070".
               05                      PIC X(09) VALUE "190106170".
               05                      PIC X(09) VALUE "190106270".
           03  CONST-BTU-RYOYO-R   REDEFINES   CONST-BTU-RYOYO-VAL.
               05  CONST-BTU-RYOYO     PIC X(09) OCCURS 3.
           03  CONST-BTU-RYOYO-H18     PIC X(09) VALUE "190120210".
           03  CONST-JYUTOK1           PIC X(09) VALUE "190105870".
           03  CONST-JYUTOK2           PIC X(09) VALUE "190105970".
           03  CONST-MUKIN             PIC X(09) VALUE "190106570".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    基本情報
           COPY  "CPSK1001.INC".
      *    施設基準
           COPY  "CPSK1006.INC".
      *    病棟管理
           COPY  "CPSK5001.INC".
      *    病室管理
           COPY  "CPSK5002.INC".
      *    診療科コード
           COPY  "CPSK1005.INC".
      *    病室種別
           COPY  "CPSK5108.INC".
      *    特定入院料（病棟）
           COPY  "CPSK5110.INC".
      *    特定入院料（病室）
           COPY  "CPSK5111.INC".
      *    室料差額
           COPY  "CPSK5113.INC".
      *    性別特定
           COPY  "CPSK5109.INC".
      *    点数マスタ
           COPY  "CPTENSU.INC".
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY  "CPPTNYUINRRK.INC".
      *
      *    入院料加算チェック
       01  NYUKSNCHK-REC.
           COPY    "CPNYUKSNCHK.INC".
      *
           COPY    "CPSK5002.INC"  REPLACING  //SYS-//
                                   BY         //TMP-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    入院料加算チェック
      *    COPY    "CPORCSNYUKSNCHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    病室名編集
           COPY    "CPORCSBRMNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    部分一致文字列編集
           COPY    "CPORCSADDSIGN.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W20FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W20FREE     TO  SPA-FREE
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
      *    エラー画面より
           IF    ( SPA-MOTOPG      =   "WERR" )
               PERFORM 5001-MAPCUR-SEC
           ELSE
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
           MOVE    "W21    "           TO  MCP-WINDOW.
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
           WHEN    "W20"
               IF    ( SPA-W20-W21-MOTOPG  =   "W21" )
      *
                   INITIALIZE  SPA-W20-W21-AREA
      *
      *            病棟情報取得処理
                   PERFORM 3101-SYS-5001-GET-SEC
                   MOVE    SPA-GMN-W21-BTUNUM
                                       TO  WRK-CMB-CD
                   PERFORM 4200-BTUNUMLST-CHK-SEC
                   IF    ( WRK-CMB-ITM =   SPACE )
                       GO  TO  300-SCREEN-EXT
                   END-IF
      *
                   MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-BTUNUM-G
                   MOVE    WRK-CMB-ITM2    TO  WRK-TOKNYUIN
                   MOVE    WRK-CMB-ITM3    TO  WRK-KHNSRYCD
                   MOVE    WRK-CMB-ITM4    TO  WRK-BTU-YUKSTYMD
                   MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W21-BTUSBT
      *
      *            入院基本料編集処理
                   PERFORM 3101-BTU-KHNRYO-EDIT-SEC
      *
                   PERFORM 4100-SYSKANRI-RESET-SEC
      *
                   GO  TO  300-SCREEN-EXT
               ELSE
                   PERFORM 310-SPASET-SEC
      *            病棟情報設定処理
                   PERFORM 310-BTU-SET-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           WHEN    "WID1"
               PERFORM 330-WID1-SET-SEC
               GO  TO      300-SCREEN-EXT
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
           INITIALIZE              SPA-W21-AREA
      *
           MOVE    SPA-SYSYMD      TO  SPA-NAI-W21-KJNYMD
           MOVE    SPA-SYSYMDWH    TO  SPA-GMN-W21-KJNYMD
      *
           PERFORM 311-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       311-SPASET-SEC              SECTION.
      *
      *    病棟情報取得処理
           PERFORM 3101-SYS-5001-GET-SEC
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    1               TO  SPA-GMN-W21-CUR
      *
           .
       311-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称設定処理（病棟画面より遷移時）
      *****************************************************************
       310-BTU-SET-SEC                 SECTION.
      *
      *    クリア処理
           PERFORM 420-CLEAR-SEC
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-W20-W21-BTUNUM
                                   TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               GO  TO  310-BTU-SET-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-BTUNUM-G
           MOVE    WRK-CMB-ITM2    TO  WRK-TOKNYUIN
           MOVE    WRK-CMB-ITM3    TO  WRK-KHNSRYCD
           MOVE    WRK-CMB-ITM4    TO  WRK-BTU-YUKSTYMD
           MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W21-BTUSBT
      *
      *    入院基本料編集処理
           PERFORM 3101-BTU-KHNRYO-EDIT-SEC
      *
      *    病室管理情報検索（病室リストに表示）
           PERFORM 3101-SYS-5002-GET-SEC
      *
           IF    ( SPA-GMN-W21-BRMLST-MAX  >   ZERO )
               MOVE    2           TO  SPA-GMN-W21-CUR
           ELSE
               MOVE    3           TO  SPA-GMN-W21-CUR
           END-IF
      *
           .
       310-BTU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院基本料編集処理
      *****************************************************************
       3101-BTU-KHNRYO-EDIT-SEC    SECTION.
      *
           INITIALIZE              SPA-GMN-W21-BTUKHNRYO
                                   SPA-NAI-W21-BTUKHNCD
                                   SPA-NAI-W21-BTUTOKCD
      *
           IF    ( WRK-KHNSRYCD    NOT =   SPACE )
               MOVE    WRK-KHNSRYCD        TO  WRK-TNSSRYCD
               MOVE    WRK-BTU-YUKSTYMD    TO  WRK-TNSYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-NAME        TO  SPA-GMN-W21-BTUKHNRYO
               MOVE    WRK-KHNSRYCD    TO  SPA-NAI-W21-BTUKHNCD
           END-IF
      *
           IF    ( WRK-TOKNYUIN    NOT =   SPACE )
               INITIALIZE                  SYSKANRI-REC
               MOVE    "5110"          TO  SYS-KANRICD
               MOVE    WRK-TOKNYUIN    TO  SYS-KBNCD
               MOVE    WRK-BTU-YUKSTYMD
                                       TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC     TO  SYS-5110-REC
               MOVE    SYS-5110-BTU-TOKUTEINYUIN-NM
                                        TO  SPA-GMN-W21-BTUKHNRYO
               MOVE    SYS-5110-SRYCD   TO  SPA-NAI-W21-BTUTOKCD
           END-IF
      *
           .
       3101-BTU-KHNRYO-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期処理（病棟コンボを除く）
      *****************************************************************
       3101-CMB-SYOKI-SEC          SECTION.
      *
      *    診療科コード取得処理
           PERFORM 3101-SYS-1005-GET-SEC
      *
      *    病室種別取得処理
           PERFORM 3101-SYS-5108-GET-SEC
      *
      *    特定入院料（病室）取得処理
           PERFORM 3101-SYS-5111-GET-SEC
      *
      *    室料差額取得処理
           PERFORM 3101-SYS-5113-GET-SEC
      *
      *    性別特定取得処理
           PERFORM 3101-SYS-5109-GET-SEC
      *
           IF    ( WRK-KJN-YUKSTYMD       >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-BTUSBT      =   "04" OR "09" )
      *        入院基本料コンボ設定処理
               PERFORM 3101-KHNRYO-GET-SEC
           END-IF
      *
      *    病棟機能
           PERFORM 3101-TBTUTYPE-GET-SEC
      *
      *    加算共通コンボボックス初期処理
           PERFORM 3101-CMB-KSNCOMMON-SET-SEC
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H240401 )
               PERFORM 31012-CMB-KSN-H24R02-SEC
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H220401 )
               PERFORM 31012-CMB-KSN-H22-SEC
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H180401 )
               PERFORM 31012-CMB-KSN-SYOKI-SEC
           WHEN    OTHER
               PERFORM 31011-CMB-KSN-SYOKI-SEC
           END-EVALUATE
      *
           .
       3101-CMB-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟情報取得処理
      *****************************************************************
       3101-SYS-5001-GET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-BTUNUMLST-G
                       SPA-NAI-W21-BTUINFLST-G
      *
      *    システム管理マスタ検索処理
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W21-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI        NOT =   ZERO )
                    OR   ( SPA-GMN-W21-BTUNUM-MAX >=   50 )
      *
               COMPUTE SPA-GMN-W21-BTUNUM-MAX
                   =   SPA-GMN-W21-BTUNUM-MAX  +   1
      *
               MOVE    SPA-GMN-W21-BTUNUM-MAX
                                   TO  IDX1
      *
               MOVE    SYS-5001-KBNCD
                                   TO  SPA-GMN-W21-BTUNUML (IDX1)
               MOVE    SYS-5001-BTU-NAME
                                   TO  SPA-GMN-W21-BTUNUMMEIL    (IDX1)
               MOVE    SYS-5001-BTU-TOKTEI-NYUIN
                                   TO  SPA-NAI-W21-BTUINF-TOKNYUIN
                                                                 (IDX1)
               MOVE    SYS-5001-BTU-KHNSRYCD
                                   TO  SPA-NAI-W21-BTUINF-KHNSRYCD
                                                                 (IDX1)
               MOVE    SYS-5001-STYUKYMD
                                   TO  SPA-NAI-W21-BTUINF-STYUKYMD
                                                                 (IDX1)
               MOVE    SYS-5001-BTU-SBT
                                   TO  SPA-NAI-W21-BTUINF-SBT    (IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYS-5001-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療科コード取得処理
      *****************************************************************
       3101-SYS-1005-GET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-SRYKALST-G
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SRYKA-MAX   >=  99 )
      *
               COMPUTE SPA-GMN-W21-SRYKA-MAX
                   =   SPA-GMN-W21-SRYKA-MAX   +   1
               MOVE    SPA-GMN-W21-SRYKA-MAX
                                       TO  IDX1
               MOVE    SYS-1005-KBNCD
                                   TO  SPA-GMN-W21-SRYKAL (IDX1)
               MOVE    SYS-1005-SRYKANAME
                                   TO  SPA-GMN-W21-SRYKAMEIL (IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC        TO  SYS-1005-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYS-1005-GET-EXT.
           EXIT.
      *****************************************************************
      *    病室種別取得処理
      *****************************************************************
       3101-SYS-5108-GET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-SBTLST-G
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    "5108"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5108-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SBT-MAX >=  15 )
      *
               COMPUTE SPA-GMN-W21-SBT-MAX
                   =   SPA-GMN-W21-SBT-MAX +   1
               MOVE    SPA-GMN-W21-SBT-MAX
                                   TO  IDX1
      *
               MOVE    SYS-5108-KBNCD
                                   TO  SPA-GMN-W21-SBTL   (IDX1)
               MOVE    SYS-5108-BRM-SBT-NM
                                   TO  SPA-GMN-W21-SBTMEIL(IDX1)
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5108-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYS-5108-GET-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料取得処理
      *****************************************************************
       3101-SYS-5111-GET-SEC       SECTION.
      *
           INITIALIZE              SPA-GMN-W21-TOKNYUINLST-G
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-TOKNYUIN-MAX
                                          >=  CONST-TOKNYUIN-MAX )
      *
               IF    ( SYS-5111-BRM-NYUINRYOKBN    =  "01"  )
                AND  ( SYS-5111-SEIKATU-RYOKBN     =  SPACE )
                   COMPUTE SPA-GMN-W21-TOKNYUIN-MAX
                       =   SPA-GMN-W21-TOKNYUIN-MAX    +   1
                   MOVE    SPA-GMN-W21-TOKNYUIN-MAX
                                       TO  IDX1
      *
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                   TO  SPA-GMN-W21-TOKNYUINMEIL
                                                            (IDX1)
      *
                   MOVE    SYS-5111-KBNCD
                                   TO  SPA-GMN-W21-TOKNYUINL-KBNCD
                                                            (IDX1)
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM2
                                   TO  SPA-GMN-W21-TOKNYUINL-TANMEI
                                                            (IDX1)
                   MOVE    SYS-5111-SRYCD
                                   TO  SPA-GMN-W21-TOKNYUINL-SRYCD
                                                            (IDX1)
                   MOVE    SYS-5111-SRYCD          TO  WRK-TNSSRYCD
                   MOVE    WRK-KJN-YUKSTYMD        TO  WRK-TNSYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   MOVE    TNS-KOUHYOJYUNNUM
                                       TO  SPA-GMN-W21-TOKNYUINL-JUNNUM
                                                         (IDX1)
               END-IF
      *
      *        システム管理マスタ読込処理
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           COMPUTE IDXS    =   CONST-TOKNYUIN-MAX  +   1
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   SPA-GMN-W21-TOKNYUIN-MAX )
      *
               COMPUTE WRK-FROM    =   IDXA    +   1
               PERFORM VARYING IDXB    FROM    WRK-FROM   BY  1
                       UNTIL ( IDXB    >   SPA-GMN-W21-TOKNYUIN-MAX )
                   IF    ( SPA-GMN-W21-TOKNYUINL-JUNNUM (IDXA)
                               >   SPA-GMN-W21-TOKNYUINL-JUNNUM (IDXB))
                       MOVE    SPA-GMN-W21-TOKNYUINLST-OCC (IDXA)
                               TO  SPA-GMN-W21-TOKNYUINLST-OCC (IDXS)
                       MOVE    SPA-GMN-W21-TOKNYUINLST-OCC (IDXB)
                               TO  SPA-GMN-W21-TOKNYUINLST-OCC (IDXA)
                       MOVE    SPA-GMN-W21-TOKNYUINLST-OCC (IDXS)
                               TO  SPA-GMN-W21-TOKNYUINLST-OCC (IDXB)
                   END-IF
               END-PERFORM
      *
               COMPUTE WRK-NUM02   =   IDXA
               MOVE    WRK-NUM02X  TO  SPA-GMN-W21-TOKNYUINL (IDXA)
      *
           END-PERFORM
      *
           .
       3101-SYS-5111-GET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額取得処理
      *****************************************************************
       3101-SYS-5113-GET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-SAGAKULST-G
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    "5113"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SAGAKU-MAX  >=  99 )
      *
               COMPUTE SPA-GMN-W21-SAGAKU-MAX
                   =   SPA-GMN-W21-SAGAKU-MAX  +   1
               MOVE    SPA-GMN-W21-SAGAKU-MAX
                                   TO  IDX1
      *
               MOVE    SYS-5113-KBNCD
                                   TO  SPA-GMN-W21-SAGAKUL (IDX1)
               INITIALIZE              WRK-HEN-SAGAKU
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                   TO  WRK-HEN-SAGAKU-Z9
               MOVE    "円"        TO  WRK-HEN-SAGAKU-EN
               MOVE    WRK-HEN-SAGAKU
                                   TO  SPA-GMN-W21-SAGAKUMEIL (IDX1)
      *
      *        システム管理マスタ読込処理
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYS-5113-GET-EXT.
           EXIT.
      *****************************************************************
      *    性別特定取得処理
      *****************************************************************
       3101-SYS-5109-GET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-SEXLST-G
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    "5109"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5109-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-SEX-MAX >=  10 )
      *
               COMPUTE SPA-GMN-W21-SEX-MAX
                   =   SPA-GMN-W21-SEX-MAX +   1
               MOVE    SPA-GMN-W21-SEX-MAX
                                   TO  IDX1
      *
               MOVE    SYS-5109-KBNCD
                                   TO  SPA-GMN-W21-SEXL (IDX1)
               MOVE    SYS-5109-BRM-SEX-NM
                                   TO  SPA-GMN-W21-SEXMEIL (IDX1)
      *
      *        システム管理マスタ読込処理
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5109-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-SYS-5109-GET-EXT.
           EXIT.
      *****************************************************************
      *    入院料取得処理
      *****************************************************************
       3101-KHNRYO-GET-SEC           SECTION.
      *
           INITIALIZE                SPA-GMN-W21-KHNRYOLST-G
                                     SPA-NAI-W21-KHNRYOLST-G
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-GMN-W21-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    WRK-KJN-YUKSTYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5111"          TO  SYS-KANRICD
           MOVE    WRK-KJN-YUKSTYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
                    OR   ( SPA-GMN-W21-KHNRYO-MAX  >= 20 )
      *
               MOVE    ZERO        TO  FLG-TARGET
      *
               IF    ( SPA-NAI-W21-BTUSBT          =   "04" )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "05" )
                AND  ( SYS-5111-SEIKATU-RYOKBN     =   SPACE )
                   MOVE    1       TO  FLG-TARGET
               END-IF
      *
               IF    ( SPA-NAI-W21-BTUSBT          =   "09" )
                AND  ( SYS-5111-BRM-NYUINRYOKBN    =   "06" )
                AND  ( SYS-5111-SEIKATU-RYOKBN     =   SPACE )
                   MOVE    1       TO  FLG-TARGET
               END-IF
      *
               IF    ( WRK-KJN-YUKSTYMD
                                 >=  CONST-H220401 )
                   IF    ( SYS-5111-SRYCD
                                       =   "099999953"
                                        OR "099999954" )
                           MOVE  ZERO  TO  FLG-TARGET
                   END-IF
                   IF    ( SYS-5001-KHNSRYCD
                                       =   SPACE
                                  OR       "099999953"
                                  OR       "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190132010"
                                  OR       "190132210" )
                       IF    ( SYS-5111-SRYCD
                                       =   "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010"
                                  OR       "190132510"
                                  OR       "190132710"
                                  OR       "190132910"
                                  OR       "190133110"
                                  OR       "190133310"
                                  OR       "190133510"
                                  OR       "190133710"
                                  OR       "190133910"
                                  OR       "190134110" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
                   IF    ( SYS-5001-KHNSRYCD
                                       =   "099999954"
                                  OR       "190132410"
                                  OR       "190132610"
                                  OR       "190132810"
                                  OR       "190133010"
                                  OR       "190133210"
                                  OR       "190133410"
                                  OR       "190133610"
                                  OR       "190133810"
                                  OR       "190134010" )
                       IF    ( SYS-5111-SRYCD
                                       =   "190121310"
                                  OR       "190121410"
                                  OR       "190121510"
                                  OR       "190121610"
                                  OR       "190121710"
                                  OR       "190131610"
                                  OR       "190131810"
                                  OR       "190132010"
                                  OR       "190132210"
                                  OR       "190123710"
                                  OR       "190123810"
                                  OR       "190123910"
                                  OR       "190124010"
                                  OR       "190124110"
                                  OR       "190131710"
                                  OR       "190131910"
                                  OR       "190132110"
                                  OR       "190132310" )
                           MOVE  ZERO  TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET          =   1 )
                   COMPUTE SPA-GMN-W21-KHNRYO-MAX
                       =   SPA-GMN-W21-KHNRYO-MAX    +   1
                   MOVE    SPA-GMN-W21-KHNRYO-MAX
                                       TO  IDX1
      *
                   MOVE    SYS-5111-KBNCD
                                       TO  SPA-GMN-W21-KHNRYOL(IDX1)
                   MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM
                                       TO  SPA-GMN-W21-KHNRYOMEIL
                                                                (IDX1)
                   MOVE    SYS-5111-SRYCD
                                       TO  SPA-NAI-W21-KHNRYOL(IDX1)
               END-IF
      *
      *        システム管理マスタ読込処理
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               MOVE    SYSKANRI-REC    TO  SYS-5111-REC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       3101-KHNRYO-GET-EXT.
           EXIT.
      *****************************************************************
      *    病棟機能情報取得処理
      *****************************************************************
       3101-TBTUTYPE-GET-SEC        SECTION.
      *
           INITIALIZE  SPA-GMN-W21-TBTUTYPELST-G
      *
           MOVE    "1"             TO  SPA-GMN-W21-TBTUTYPEL (1)
           MOVE    "高度急性期"    TO  SPA-GMN-W21-TBTUTYPEMEIL (1)
      *
           MOVE    "2"             TO  SPA-GMN-W21-TBTUTYPEL (2)
           MOVE    "急性期"        TO  SPA-GMN-W21-TBTUTYPEMEIL (2)
      *
           MOVE    "3"             TO  SPA-GMN-W21-TBTUTYPEL (3)
           MOVE    "回復期"        TO  SPA-GMN-W21-TBTUTYPEMEIL (3)
      *
           MOVE    "4"             TO  SPA-GMN-W21-TBTUTYPEL (4)
           MOVE    "慢性期"        TO  SPA-GMN-W21-TBTUTYPEMEIL (4)
      *
           MOVE    4               TO  SPA-GMN-W21-TBTUTYPE-MAX
      *
           .
       3101-TBTUTYPE-GET-EXT.
           EXIT.
      *****************************************************************
      *    加算共通コンボボックス初期処理
      *****************************************************************
       3101-CMB-KSNCOMMON-SET-SEC          SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W21-KSNCOMMONL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W21-KSNCOMMONMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W21-KSNCOMMONL(2)
           MOVE    "算定する"          TO  SPA-GMN-W21-KSNCOMMONMEIL(2)
           MOVE    2                   TO  SPA-GMN-W21-KSNCOMMON-MAX
      *
           .
       3101-CMB-KSNCOMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H14.4〜H18.3)
      *****************************************************************
       31011-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    新生児入院医療管理加算
           PERFORM 3101-CMB-SINSEIJI-SET-SEC
      *
      *    診療所療養病床療養環境加算
           PERFORM 3101-CMB-SNRRYO-SET-SEC
      *
      *    放射線治療病室管理加算
           PERFORM 3101-CMB-HOSYASEN-SET-SEC
      *
      *    療養環境加算
           PERFORM 3101-CMB-RYO-SET-SEC
      *
      *    療養病棟療養環境加算
           PERFORM 3101-CMB-BTURYO-SET-SEC
      *
      *    重症者等療養環境特別加算
           PERFORM 3101-CMB-JYUTOK-SET-SEC
      *
      *    無菌治療室管理加算
           PERFORM 3101-CMB-MUKIN-SET-SEC
      *
           .
       31011-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H18.4〜)
      *****************************************************************
       31012-CMB-KSN-SYOKI-SEC         SECTION.
      *
      *    新生児入院医療管理加算
           PERFORM 3101-CMB-SINSEIJI-SET-SEC
      *
      *    診療所療養病床療養環境加算
           PERFORM 3101-CMB-SNRRYO-SET-SEC
      *
      *    放射線治療病室管理加算
           PERFORM 3101-CMB-HOSYASEN-SET-SEC
      *
      *    療養環境加算
           PERFORM 3101-CMB-RYO-SET-SEC
      *
      *    療養病棟療養環境加算
           PERFORM 3101-CMB-BTURYO-SET-SEC
      *
      *    重症者等療養環境特別加算
           PERFORM 3101-CMB-JYUTOK-SET-SEC
      *
      *    無菌治療室管理加算
           PERFORM 3101-CMB-MUKIN-SET-SEC
      *
           .
       31012-CMB-KSN-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H22.4〜)
      *****************************************************************
       31012-CMB-KSN-H22-SEC           SECTION.
      *
      *    診療所療養病床療養環境加算
           PERFORM 3101-CMB-SNRRYO-SET-SEC
      *
      *    放射線治療病室管理加算
           PERFORM 3101-CMB-HOSYASEN-SET-SEC
      *
      *    療養環境加算
           PERFORM 3101-CMB-RYO-SET-SEC
      *
      *    療養病棟療養環境加算
           PERFORM 3101-CMB-BTURYO-SET-SEC
      *
      *    重症者等療養環境特別加算
           PERFORM 3101-CMB-JYUTOK-SET-SEC
      *
      *    無菌治療室管理加算
           PERFORM 3101-CMB-MUKIN-SET-SEC
      *
           .
       31012-CMB-KSN-H22-EXT.
           EXIT.
      *****************************************************************
      *    入院加算コンボ初期処理(H24.4〜)
      *****************************************************************
       31012-CMB-KSN-H24R02-SEC        SECTION.
      *
      *    診療所療養病床療養環境加算
           PERFORM 3101-CMB-SNRRYO-SET-SEC
      *
      *    診療所療養病床療養環境改善加算
           PERFORM 3101-CMB-SNRRYOKAI-SET-SEC
      *
      *    放射線治療病室管理加算
           PERFORM 3101-CMB-HOSYASEN-SET-SEC
      *
      *    療養環境加算
           PERFORM 3101-CMB-RYO-SET-SEC
      *
      *    療養病棟療養環境加算
           PERFORM 3101-CMB-BTURYO-SET-SEC
      *
      *    療養病棟療養環境改善加算
           PERFORM 3101-CMB-BTURYOKAI-SET-SEC
      *
      *    重症者等療養環境特別加算
           PERFORM 3101-CMB-JYUTOK-SET-SEC
      *
      *    無菌治療室管理加算
           PERFORM 3101-CMB-MUKIN-SET-SEC
      *
           IF    ( WRK-KJN-YUKSTYMD    >=   CONST-R020401 )
      *        早期栄養介入管理加算
               PERFORM 3101-CMB-SOKIEIYO-SET-SEC
           END-IF
      *
           .
       31012-CMB-KSN-H24R02-EXT.
           EXIT.
      *****************************************************************
      *    新生児入院医療管理加算
      *****************************************************************
       3101-CMB-SINSEIJI-SET-SEC       SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W21-SINSEIJIL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W21-SINSEIJIMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W21-SINSEIJIL(2)
           MOVE    "算定する"          TO  SPA-GMN-W21-SINSEIJIMEIL(2)
           MOVE    2                   TO  SPA-GMN-W21-SINSEIJI-MAX
      *
           .
       3101-CMB-SINSEIJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療所療養病床療養環境加算
      *****************************************************************
       3101-CMB-SNRRYO-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=   CONST-H241001 )
               MOVE    SPA-GMN-W21-KSNCOMMONLST-G
                                       TO  SPA-GMN-W21-SNRRYOLST-G
           WHEN    OTHER
               MOVE    "1"             TO  SPA-GMN-W21-SNRRYOL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W21-SNRRYOMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W21-SNRRYOL(2)
               MOVE    "加算１"        TO  SPA-GMN-W21-SNRRYOMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W21-SNRRYOL(3)
               MOVE    "加算２"        TO  SPA-GMN-W21-SNRRYOMEIL(3)
               MOVE    3               TO  SPA-GMN-W21-SNRRYO-MAX
           END-EVALUATE
      *
           .
       3101-CMB-SNRRYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療所療養病床療養環境改善加算
      *****************************************************************
       3101-CMB-SNRRYOKAI-SET-SEC      SECTION.
      *
           MOVE    SPA-GMN-W21-KSNCOMMONLST-G
                                       TO  SPA-GMN-W21-SNRRYOKAILST-G
      *
           .
       3101-CMB-SNRRYOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    放射線治療病室管理加算加算
      *****************************************************************
       3101-CMB-HOSYASEN-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W21-KSNCOMMONLST-G
                                       TO  SPA-GMN-W21-HOSYASENLST-G
      *
           .
       3101-CMB-HOSYASEN-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養環境加算
      *****************************************************************
       3101-CMB-RYO-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-W21-KSNCOMMONLST-G
                                       TO  SPA-GMN-W21-RYOLST-G
      *
           .
       3101-CMB-RYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟療養環境加算
      *****************************************************************
       3101-CMB-BTURYO-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H241001 )
      *
               MOVE    "1"                 TO  SPA-GMN-W21-BTURYOL(1)
               MOVE    "算定しない"        TO  SPA-GMN-W21-BTURYOMEIL(1)
      *
               MOVE    "2"                 TO  SPA-GMN-W21-BTURYOL(2)
               MOVE    "療養病棟療養環境加算１"
                                           TO  SPA-GMN-W21-BTURYOMEIL(2)
      *
               MOVE    "3"                 TO  SPA-GMN-W21-BTURYOL(3)
               MOVE    "療養病棟療養環境加算２"
                                           TO  SPA-GMN-W21-BTURYOMEIL(3)
      *
               MOVE    3                   TO  SPA-GMN-W21-BTURYO-MAX
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H180401 )
      *
               MOVE    "1"                 TO  SPA-GMN-W21-BTURYOL(1)
               MOVE    "算定しない"        TO  SPA-GMN-W21-BTURYOMEIL(1)
      *
               MOVE    "2"                 TO  SPA-GMN-W21-BTURYOL(2)
               MOVE    "療養病棟療養環境加算１"
                                           TO  SPA-GMN-W21-BTURYOMEIL(2)
      *
               MOVE    "3"                 TO  SPA-GMN-W21-BTURYOL(3)
               MOVE    "療養病棟療養環境加算２"
                                           TO  SPA-GMN-W21-BTURYOMEIL(3)
      *
               MOVE    "4"                 TO  SPA-GMN-W21-BTURYOL(4)
               MOVE    "療養病棟療養環境加算３"
                                           TO  SPA-GMN-W21-BTURYOMEIL(4)
      *
               MOVE    "5"                 TO  SPA-GMN-W21-BTURYOL(5)
               MOVE    "療養病棟療養環境加算４"
                                           TO  SPA-GMN-W21-BTURYOMEIL(5)
      *
               MOVE    5                   TO  SPA-GMN-W21-BTURYO-MAX
           WHEN    OTHER
      *
               MOVE    "1"                 TO  SPA-GMN-W21-BTURYOL(1)
               MOVE    "算定しない"        TO  SPA-GMN-W21-BTURYOMEIL(1)
      *
               MOVE    "2"                 TO  SPA-GMN-W21-BTURYOL(2)
               MOVE    "療養病棟療養環境加算１"
                                           TO  SPA-GMN-W21-BTURYOMEIL(2)
      *
               MOVE    "3"                 TO  SPA-GMN-W21-BTURYOL(3)
               MOVE    "療養病棟療養環境加算２"
                                           TO  SPA-GMN-W21-BTURYOMEIL(3)
      *
               MOVE    "4"                 TO  SPA-GMN-W21-BTURYOL(4)
               MOVE    "療養病棟療養環境加算３"
                                           TO  SPA-GMN-W21-BTURYOMEIL(4)
      *
      *
               MOVE    4                   TO  SPA-GMN-W21-BTURYO-MAX
           END-EVALUATE
      *
           .
       3101-CMB-BTURYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟療養環境改善加算
      *****************************************************************
       3101-CMB-BTURYOKAI-SET-SEC         SECTION.
      *
           MOVE    "1"             TO  SPA-GMN-W21-BTURYOKAIL(1)
           MOVE    "算定しない"    TO  SPA-GMN-W21-BTURYOKAIMEIL(1)
      *
           MOVE    "2"             TO  SPA-GMN-W21-BTURYOKAIL(2)
           MOVE    "療養病棟療養環境改善加算１"
                                   TO  SPA-GMN-W21-BTURYOKAIMEIL(2)
      *
           MOVE    "3"             TO  SPA-GMN-W21-BTURYOKAIL(3)
           MOVE    "療養病棟療養環境改善加算２"
                                   TO  SPA-GMN-W21-BTURYOKAIMEIL(3)
      *
           MOVE    3               TO  SPA-GMN-W21-BTURYOKAI-MAX
      *
           .
       3101-CMB-BTURYOKAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    重症者等療養環境特別加算
      *****************************************************************
       3101-CMB-JYUTOK-SET-SEC         SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W21-JYUTOKL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W21-JYUTOKMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W21-JYUTOKL(2)
           MOVE    "個室"              TO  SPA-GMN-W21-JYUTOKMEIL(2)
           MOVE    "3"                 TO  SPA-GMN-W21-JYUTOKL(3)
           MOVE    "２人部屋"          TO  SPA-GMN-W21-JYUTOKMEIL(3)
           MOVE    3                   TO  SPA-GMN-W21-JYUTOK-MAX
      *
           .
       3101-CMB-JYUTOK-SET-EXT.
           EXIT.
      *****************************************************************
      *    無菌治療室管理加算
      *****************************************************************
       3101-CMB-MUKIN-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H250401 )
               MOVE    "1"             TO  SPA-GMN-W21-MUKINL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W21-MUKINMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W21-MUKINL(2)
               MOVE    "無菌治療室管理加算１"
                                       TO  SPA-GMN-W21-MUKINMEIL(2)
               MOVE    "4"             TO  SPA-GMN-W21-MUKINL(3)
               MOVE    "無菌治療室管理加算２"
                                       TO  SPA-GMN-W21-MUKINMEIL(3)
               MOVE    3               TO  SPA-GMN-W21-MUKIN-MAX
           WHEN  ( WRK-KJN-YUKSTYMD    >=  CONST-H240401 )
               MOVE    "1"             TO  SPA-GMN-W21-MUKINL(1)
               MOVE    "算定しない"    TO  SPA-GMN-W21-MUKINMEIL(1)
               MOVE    "2"             TO  SPA-GMN-W21-MUKINL(2)
               MOVE    "無菌治療室管理加算１"
                                       TO  SPA-GMN-W21-MUKINMEIL(2)
               MOVE    "3"             TO  SPA-GMN-W21-MUKINL(3)
               MOVE    "無菌治療室管理加算１（経過措置）"
                                       TO  SPA-GMN-W21-MUKINMEIL(3)
               MOVE    "4"             TO  SPA-GMN-W21-MUKINL(4)
               MOVE    "無菌治療室管理加算２"
                                       TO  SPA-GMN-W21-MUKINMEIL(4)
               MOVE    4               TO  SPA-GMN-W21-MUKIN-MAX
           WHEN    OTHER
               MOVE    SPA-GMN-W21-KSNCOMMONLST-G
                                       TO  SPA-GMN-W21-MUKINLST-G
           END-EVALUATE
      *
           .
       3101-CMB-MUKIN-SET-EXT.
           EXIT.
      *****************************************************************
      *    早期栄養介入管理加算
      *****************************************************************
       3101-CMB-SOKIEIYO-SET-SEC           SECTION.
      *
           MOVE    "1"                 TO  SPA-GMN-W21-SOKIEIYOL(1)
           MOVE    "算定しない"        TO  SPA-GMN-W21-SOKIEIYOMEIL(1)
           MOVE    "2"                 TO  SPA-GMN-W21-SOKIEIYOL(2)
           MOVE    "算定する"          TO  SPA-GMN-W21-SOKIEIYOMEIL(2)
           MOVE    2                   TO  SPA-GMN-W21-SOKIEIYO-MAX
      *
           .
       3101-CMB-SOKIEIYO-SET-EXT.
           EXIT.
      *****************************************************************
      *    病室管理情報検索処理
      *****************************************************************
       3101-SYS-5002-GET-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-BRMLST
                       SPA-NAI-W21-BRMLST
      *
           INITIALIZE  SYSKANRI-REC
           MOVE    "5002"          TO  SYS-KANRICD
           MOVE    SPA-GMN-W21-BTUNUM
                                   TO  SYS-KBNCD
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-GMN-W21-BTUNUM
                                   TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING   ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR    TO  SYS-KBNCD
           MOVE    SPA-NAI-W21-KJNYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY9-SEL-SEC
           MOVE    MCPDATA-REC     TO  SYS-5002-REC
           MOVE    FLG-SYSKANRI    TO  FLG-SYS5002
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( FLG-SYS5002 NOT =   ZERO )
                 OR      ( IDX1    >=  200 )
      *
               COMPUTE SPA-GMN-W21-BRMLST-MAX
                   =   SPA-GMN-W21-BRMLST-MAX  +   1
               MOVE    SPA-GMN-W21-BRMLST-MAX
                                   TO  IDX1
      *
      *        病室リスト編集
               PERFORM 3102-BRMLST-EDIT-SEC
      *
               PERFORM 900-SYSKANRI-KEY9-FET-SEC
               MOVE    MCPDATA-REC     TO  SYS-5002-REC
               MOVE    FLG-SYSKANRI    TO  FLG-SYS5002
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *
           .
       3101-SYS-5002-GET-EXT.
           EXIT.
      *****************************************************************
      *    病室リスト編集処理
      *****************************************************************
       3102-BRMLST-EDIT-SEC            SECTION.
      *
      *    選択番号
           MOVE    IDX1    TO  SPA-GMN-W21-TSELNUM (IDX1)
      *    病室番号
           IF    ( SYS-5002-KBNCD ( 3 : )
                           IS  NUMERIC )
               MOVE    SYS-5002-KBNCD ( 3 : )
                           TO   WRK-999999
               MOVE    WRK-999999
                           TO   WRK-ZZZZZ9
               MOVE    WRK-ZZZZZ9
                           TO  SPA-GMN-W21-TBRMNUM (IDX1)
           ELSE
               MOVE    SYS-5002-BRM-NUM
                           TO  SPA-GMN-W21-TBRMNUM (IDX1)
           END-IF
      *
           MOVE    SYS-5002-BRM-NUM
                           TO  SPA-GMN-W21-TBRMNUM2 (IDX1)
      *
           MOVE    SYS-5002-KBNCD
                           TO  SPA-NAI-W21-TBRMNUM  (IDX1)
      *
      *    病室種別
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5108"              TO  SYS-KANRICD
           MOVE    SYS-5002-BRM-SBT    TO  SYS-KBNCD
           MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5108-REC
           MOVE    SYS-5108-BRM-SBT-NM TO  SPA-GMN-W21-TSBT (IDX1)
      *
      *    特定入院料
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5111"              TO  SYS-KANRICD
           MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                       TO  SYS-KBNCD
           MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5111-REC
           MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM2
                                       TO  SPA-GMN-W21-TTOKNYUIN (IDX1)
           MOVE    SYS-5111-SRYCD      TO  SPA-NAI-W21-TTOKNYUIN (IDX1)
      *
      *    収容人数
           MOVE    SYS-5002-BRM-NINZU
                           TO  SPA-GMN-W21-TNINZU (IDX1)
      *
      *    室料差額
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5113"              TO  SYS-KANRICD
           MOVE    SYS-5002-BRM-SAGAKU TO  SYS-KBNCD
           MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5113-REC
               MOVE    SYS-5113-BRM-SAGAKU-NM
                                       TO  SPA-GMN-W21-TSAGAKU (IDX1)
           END-IF
      *
      *    性別特定
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5109"              TO  SYS-KANRICD
           MOVE    SYS-5002-BRM-SEX    TO  SYS-KBNCD
           MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5109-REC
           MOVE    SYS-5109-BRM-SEX-NM
                                       TO  SPA-GMN-W21-TSEX (IDX1)
      *
      *    内線番号
           MOVE    SYS-5002-BRM-NAITEL
                           TO  SPA-GMN-W21-TNAITEL (IDX1)
      *
      *    診療科
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1005"              TO  SYS-KANRICD
           MOVE    SYS-5002-BRM-KA     TO  SYS-KBNCD
           MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-1005-REC
           MOVE    SYS-1005-SRYKANAME  TO  SPA-GMN-W21-TSRYKA (IDX1)
      *
      *    入院基本料
           INITIALIZE                      SYSKANRI-REC
           MOVE    "5111"              TO  SYS-KANRICD
           MOVE    SYS-5002-KHNKBNCD (2:2)
                                       TO  SYS-KBNCD
           MOVE    SYS-5002-STYUKYMD   TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5111-REC
           MOVE    SYS-5111-BRM-TOKUTEINYUIN-NM2
                                       TO  SPA-GMN-W21-TKHNRYO(IDX1)
      *
      *
      *    有効開始日
           MOVE    SYS-5002-STYUKYMD
                           TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG
                           TO  SPA-GMN-W21-TYUKOSTYMD (IDX1)
           MOVE    SYS-5002-STYUKYMD
                           TO  SPA-NAI-W21-TYUKOSTYMD (IDX1)
      *
      *    有効終了日
           MOVE    SYS-5002-EDYUKYMD
                           TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           MOVE    WRK-HENYMDG
                           TO  SPA-GMN-W21-TYUKOEDYMD (IDX1)
           MOVE    SYS-5002-EDYUKYMD
                           TO  SPA-NAI-W21-TYUKOEDYMD (IDX1)
      *
           .
       3102-BRMLST-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       4100-INPUT-AREA-EDIT-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-INPUT-AREA
      *
      *    病室情報検索処理
           PERFORM 4200-BRM-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
      *
      *        有効開始日
               MOVE    SYS-5002-STYUKYMD
                                   TO  SPA-NAI-W21-YUKOSTYMD
                                       SPA-NAI-W21-YUKOSTYMD-OLD
                                       WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W21-YUKOSTYMD
      *
      *        有効終了日
               MOVE    SYS-5002-EDYUKYMD
                                   TO  SPA-NAI-W21-YUKOEDYMD
                                       SPA-NAI-W21-YUKOEDYMD-OLD
                                       WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W21-YUKOEDYMD
      *
               PERFORM 41001-INPUT-AREA-EDIT-SEC
      *
           ELSE
               MOVE    1           TO  FLG-INPUT-AREA
           END-IF
      *
           .
       4100-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    入力項目領域編集処理
      *****************************************************************
       41001-INPUT-AREA-EDIT-SEC       SECTION.
      *
      *    システム管理情報再設定処理
           PERFORM 4100-SYSKANRI-RESET-SEC
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
      *    区分コード
           MOVE    SYS-5002-KBNCD
                                   TO  SPA-NAI-W21-5002-KBNCD
      *
      *    選択番号１
           IF    ( SPA-NAI-W21-INPUT-MODE    =   1 )
               MOVE    WRK-SELNUM  TO  WRK-ZZZ
      *
               IF    ( WRK-ZZZ-G (1:1)     =   SPACE )
      *
                   UNSTRING    WRK-ZZZ-G   DELIMITED   BY  ALL SPACE
                   INTO        WRK-STR-TMP
                               SPA-GMN-W21-SELNUM1
                   END-UNSTRING
      *
               ELSE
                   MOVE    WRK-ZZZ-G   TO  SPA-GMN-W21-SELNUM1
               END-IF
           END-IF
      *
      *    病室番号
           MOVE    SYS-5002-BRM-NUM
                                   TO  SPA-GMN-W21-BRMNUM
                                       SPA-NAI-W21-BRMNUM-OLD
      *
      *    病室種別
           MOVE    SYS-5002-BRM-SBT
                                   TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-SBT-G
      *
      *    特定入院料
           MOVE    SYS-5002-BRM-TOKTEI-NYUIN
                                   TO  WRK-CMB-CD
           PERFORM 4201-TOKNYUINLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-TOKNYUIN-G
           MOVE    WRK-CMB-ITM4    TO  SPA-NAI-W21-TOKNYUIN
      *
      *    収容人数
           MOVE    SYS-5002-BRM-NINZU
                                   TO  SPA-GMN-W21-NINZU
      *
      *    室料差額
           MOVE    SYS-5002-BRM-SAGAKU
                                   TO  WRK-CMB-CD
           PERFORM 4200-SAGAKULST-CHK-SEC
           MOVE    WRK-CMB-ITM   TO  SPA-GMN-W21-SAGAKU-G
      *
      *    性別特定
           MOVE    SYS-5002-BRM-SEX
                                   TO  WRK-CMB-CD
           PERFORM 4200-SEXLST-CHK-SEC
           MOVE    WRK-CMB-ITM   TO  SPA-GMN-W21-SEX-G
      *
      *    内線番号
           MOVE    SYS-5002-BRM-NAITEL
                                   TO  SPA-GMN-W21-NAITEL
      *
      *    診療科
           MOVE    SYS-5002-BRM-KA TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-SRYKA-G
      *
      *    入院基本料
           MOVE    SYS-5002-KHNKBNCD (2:2)
                                   TO  WRK-CMB-CD
           PERFORM 4200-KHNRYOLST-CHK-SEC
           IF    ( WRK-CMB-ITM     NOT =   SPACE )
               MOVE    WRK-CMB-ITM             TO  SPA-GMN-W21-KHNRYO-G
               MOVE    WRK-CMB-ITM2            TO  SPA-NAI-W21-KHNSRYCD
               MOVE    SPA-NAI-W21-KHNSRYCD    TO  WRK-TNSSRYCD
               MOVE    SPA-NAI-W21-YUKOSTYMD   TO  WRK-TNSYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-TEN                 TO  SPA-GMN-W21-KHNTEN
           END-IF
      *
      *
      *    病棟コード
           IF    ( SYS-5002-TSRYCD      NOT =   SPACE )
               MOVE    SYS-5002-TSRYCD(5:1)
                                       TO  WRK-CMB-CD
               PERFORM 4200-TBTUTYPELST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-TBTUTYPE-G
               MOVE    SYS-5002-TSRYCD(6:4)
                                       TO  WRK-KBN4-G
               MOVE    WRK-KBN4        TO  SPA-GMN-W21-TBTUCD
           END-IF
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W21-YUKOSTYMD   >=   CONST-H240401 )
               PERFORM 41004-H24R02-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W21-YUKOSTYMD   >=   CONST-H220401 )
               PERFORM 41003-KSN-EDIT-SEC
           WHEN  ( SPA-NAI-W21-YUKOSTYMD   >=   CONST-H180401 )
               PERFORM 41002-KSN-EDIT-SEC
           WHEN    OTHER
               PERFORM 41001-KSN-EDIT-SEC
           END-EVALUATE
      *
           .
       41001-INPUT-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H14.4〜H18.3)
      *****************************************************************
       41001-KSN-EDIT-SEC               SECTION.
      *
      *    新生児入院医療管理加算
           MOVE    SYS-5002-SINSEIJI-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SINSEIJILST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-SINSEIJI-G
      *
      *    診療所療養病床療養環境加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-SNR-RYOYO-KBN (1)  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-SNR-RYOYO-KBN (2)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-SNRRYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-SNRRYO-G
      *
      *    放射線治療病室管理加算
           MOVE    SYS-5002-HOSYASEN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOSYASENLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-HOSYASEN-G
      *
      *    療養環境加算
           MOVE    SYS-5002-RYOYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-RYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-RYO-G
      *
      *    療養病棟療養環境加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (1)  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (2)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (3)  =   "2" )
               MOVE    "4"             TO  WRK-CMB-CD
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-BTURYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-BTURYO-G
      *
      *    重症者等療養環境特別加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-JYUTOK1-KBN  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-JYUTOK2-KBN  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN    OTHER
               MOVE    SPACE           TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-JYUTOKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-JYUTOK-G
      *
      *    無菌治療室管理加算
           MOVE    SYS-5002-MUKIN-KBN  TO  WRK-CMB-CD
           PERFORM 4200-MUKINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-MUKIN-G
      *
           .
       41001-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H18.4〜)
      *****************************************************************
       41002-KSN-EDIT-SEC               SECTION.
      *
      *
      *    新生児入院医療管理加算
           MOVE    SYS-5002-SINSEIJI-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-SINSEIJILST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-SINSEIJI-G
      *
      *    診療所療養病床療養環境加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-SNR-RYOYO-KBN (1)  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-SNR-RYOYO-KBN (2)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-SNRRYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-SNRRYO-G
      *
      *    放射線治療病室管理加算
           MOVE    SYS-5002-HOSYASEN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOSYASENLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-HOSYASEN-G
      *
      *    療養環境加算
           MOVE    SYS-5002-RYOYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-RYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-RYO-G
      *
      *    療養病棟療養環境加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-BTU-RYOYO-H18-KBN  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (1)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (2)  =   "2" )
               MOVE    "4"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (3)  =   "2" )
               MOVE    "5"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-BTURYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-BTURYO-G
      *
      *    重症者等療養環境特別加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-JYUTOK1-KBN  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-JYUTOK2-KBN  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-JYUTOKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-JYUTOK-G
      *
      *    無菌治療室管理加算
           MOVE    SYS-5002-MUKIN-KBN  TO  WRK-CMB-CD
           PERFORM 4200-MUKINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-MUKIN-G
      *
           .
       41002-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H22.4〜)
      *****************************************************************
       41003-KSN-EDIT-SEC               SECTION.
      *
      *    診療所療養病床療養環境加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-SNR-RYOYO-KBN (1)  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-SNR-RYOYO-KBN (2)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-SNRRYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-SNRRYO-G
      *
      *    放射線治療病室管理加算
           MOVE    SYS-5002-HOSYASEN-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-HOSYASENLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-HOSYASEN-G
      *
      *    療養環境加算
           MOVE    SYS-5002-RYOYO-KBN
                                       TO  WRK-CMB-CD
           PERFORM 4200-RYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-RYO-G
      *
      *    療養病棟療養環境加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-BTU-RYOYO-H18-KBN  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (1)  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (2)  =   "2" )
               MOVE    "4"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-BTU-RYOYO-KBN (3)  =   "2" )
               MOVE    "5"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-BTURYOLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-BTURYO-G
      *
      *    重症者等療養環境特別加算
           MOVE    "1"                 TO  WRK-CMB-CD
           EVALUATE    TRUE
           WHEN  ( SYS-5002-JYUTOK1-KBN  =   "2" )
               MOVE    "2"             TO  WRK-CMB-CD
           WHEN  ( SYS-5002-JYUTOK2-KBN  =   "2" )
               MOVE    "3"             TO  WRK-CMB-CD
           END-EVALUATE
           PERFORM 4200-JYUTOKLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-JYUTOK-G
      *
      *    無菌治療室管理加算
           MOVE    SYS-5002-MUKIN-KBN  TO  WRK-CMB-CD
           PERFORM 4200-MUKINLST-CHK-SEC
           MOVE    WRK-CMB-ITM         TO  SPA-GMN-W21-MUKIN-G
      *
           .
       41003-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟別設定加算設定処理(H24.4〜)
      *****************************************************************
       41004-H24R02-KSN-EDIT-SEC           SECTION.
      *
           PERFORM VARYING IDX-KSN     FROM    1   BY  1
                   UNTIL ( IDX-KSN     >   CONST-5002-KSN-MAX )
      *
               IF    ( SYS-5002-KSNKBN (IDX-KSN)   =   "2" )
      *
                   IF    ( SPA-NAI-W21-YUKOSTYMD   >=   CONST-R020401 )
                       EVALUATE    SYS-5002-KSNCD (IDX-KSN)
                       WHEN    CONST-R02-A301C5
                           MOVE    "2"     TO  SPA-GMN-W21-SOKIEIYO
                       END-EVALUATE
                   END-IF
      *
                   EVALUATE    SYS-5002-KSNCD (IDX-KSN)
                   WHEN    CONST-H2404-SNRRYO1
                       MOVE    "2"     TO  SPA-GMN-W21-SNRRYO
                   WHEN    CONST-H2404-SNRRYO2
                       MOVE    "3"     TO  SPA-GMN-W21-SNRRYO
                   WHEN    CONST-H2404-SNRRYOKAI
                       MOVE    "2"     TO  SPA-GMN-W21-SNRRYOKAI
                   WHEN    CONST-HOSYASEN
                       MOVE    "2"     TO  SPA-GMN-W21-HOSYASEN
                   WHEN    CONST-RYOYO
                       MOVE    "2"     TO  SPA-GMN-W21-RYO
                   WHEN    CONST-H2404-BTURYO1
                       MOVE    "2"     TO  SPA-GMN-W21-BTURYO
                   WHEN    CONST-H2404-BTURYO2
                       MOVE    "3"     TO  SPA-GMN-W21-BTURYO
                   WHEN    CONST-H2404-BTURYO3
                       MOVE    "4"     TO  SPA-GMN-W21-BTURYO
                   WHEN    CONST-H2404-BTURYO4
                       MOVE    "5"     TO  SPA-GMN-W21-BTURYO
                   WHEN    CONST-H2404-BTURYOKAI1
                       MOVE    "2"     TO  SPA-GMN-W21-BTURYOKAI
                   WHEN    CONST-H2404-BTURYOKAI2
                       MOVE    "3"     TO  SPA-GMN-W21-BTURYOKAI
                   WHEN    CONST-JYUTOK1
                       MOVE    "2"     TO  SPA-GMN-W21-JYUTOK
                   WHEN    CONST-JYUTOK2
                       MOVE    "3"     TO  SPA-GMN-W21-JYUTOK
                   WHEN    CONST-H2404-MUKIN1
                       MOVE    "2"     TO  SPA-GMN-W21-MUKIN
                   WHEN    CONST-H2404-MUKIN1K
                       MOVE    "3"     TO  SPA-GMN-W21-MUKIN
                   WHEN    CONST-H2404-MUKIN2
                       MOVE    "4"     TO  SPA-GMN-W21-MUKIN
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
           .
       41004-H24R02-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室情報検索処理
      *****************************************************************
       4200-BRM-SEL-SEC                SECTION.
      *
           INITIALIZE              SYS-5002-REC
           MOVE    "5002"          TO  SYS-5002-KANRICD
           MOVE    SPA-GMN-W21-BTUNUM
                                   TO  WRK-SYS5002-BTUNUM
           PERFORM 4100-BRMNUM-KEY-EDIT-SEC
           MOVE    WRK-BRMNUM-KEY  TO  WRK-SYS5002-BRMNUM
           MOVE    WRK-SYS5002-KBNCD
                                   TO  SYS-5002-KBNCD
           MOVE    WRK-KJNYMD      TO  SYS-5002-STYUKYMD
                                       SYS-5002-EDYUKYMD
           MOVE    SYS-5002-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
           .
       4200-BRM-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴領域編集処理
      *****************************************************************
       4100-RIREKI-AREA-EDIT-SEC       SECTION.
      *
           INITIALIZE      SPA-GMN-W21-RIREKILST
                           SPA-NAI-W21-RIREKILST
      *
      *    病室情報検索
           PERFORM 4100-BRM-SEL-SEC
           IF   (  FLG-SYSKANRI    =   ZERO )
               CONTINUE
           ELSE
               GO  TO  4100-RIREKI-AREA-EDIT-EXT
           END-IF
      *
           MOVE    SPA-GMN-W21-BRMLST-SEL
                                       TO  SPA-NAI-W21-RIREKILST-SELNUM
      *
           MOVE    SPA-GMN-W21-BRMNUM
                                       TO  SPA-NAI-W21-RIREKILST-BRMNUM
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( FLG-SYSKANRI    NOT =   ZERO )
                 OR      ( IDX1        >   20 )
      *
               MOVE    IDX1            TO  SPA-GMN-W21-RIREKILST-MAX
      *
      *        選択番号
               MOVE    IDX1            TO  SPA-GMN-W21-TRSELNUM (IDX1) 
      *
      *        有効開始日
               MOVE    SYS-5002-STYUKYMD
                                   TO  SPA-NAI-W21-TRYUKOSTYMD (IDX1)
                                           WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W21-TRYUKOSTYMD (IDX1)
      *
      *        有効終了日
               MOVE    SYS-5002-EDYUKYMD
                                   TO  SPA-NAI-W21-TRYUKOEDYMD (IDX1)
                                       WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG TO  SPA-GMN-W21-TRYUKOEDYMD (IDX1)
      *
               IF    ( SPA-NAI-W21-YUKOSTYMD-OLD
                                   =   SPA-NAI-W21-TRYUKOSTYMD (IDX1))
                AND  ( SPA-NAI-W21-YUKOEDYMD-OLD
                                   =   SPA-NAI-W21-TRYUKOEDYMD (IDX1))
                   MOVE    IDX1    TO  SPA-GMN-W21-SELNUM2
                   MOVE    IDX1    TO  SPA-GMN-W21-RIREKILST-SEL
               END-IF
      *
      *        病棟データ読込処理
               PERFORM 4100-BRM-FET-SEC
      *
           END-PERFORM
      *
      *    病棟データクローズ処理
           PERFORM 4100-BRM-CLOSECURSOR-SEC
           .
      *
      *
       4100-RIREKI-AREA-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           IF    ( SPA-WID1-FLG    =   "OK" )
               EVALUATE    SPA-WIDCD
               WHEN "2001"
                       PERFORM 421-SAKUJO-SEC
               WHEN "2002"
                       PERFORM 42021-HENKOU-SEC
               WHEN "2003"
                       PERFORM 4201-EDYUKYMD-UPDATE-SEC
                       IF    ( SPA-ERRCD   =   SPACE )
                           PERFORM 4201-TUIKA-SEC
                       END-IF
               END-EVALUATE
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               PERFORM  210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 420-CLEAR-SEC
      *    追加
           WHEN    "CLICKED"       ALSO    "B06"
               PERFORM 420-TUIKA-SEC
      *    変更
           WHEN    "CLICKED"       ALSO    "B07"
               PERFORM 420-HENKOU-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B08"
               PERFORM 420-SAKUJO-SEC
      *    基準日
           WHEN    "CLICKED"       ALSO    "B04"
      *
               PERFORM 4101-GMN-SPA-SET-SEC
               MOVE    101         TO      SPA-GMN-W21-CUR
      *
      *    病棟管理情報画面へ
           WHEN    "CLICKED"       ALSO    "B09"
               IF  ( SPA-W20-W21-MOTOPG    =       SPACE )
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM 420-BYOTO-KANRI-SENI-SEC
               END-IF
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    病棟名称
           WHEN    "ACTIVATE"      ALSO    "BTU_NAME"
               PERFORM 4100-BTUNUM-CHK-SEC
      *    基準日
           WHEN    ANY             ALSO    "KJNYMD"
               PERFORM 4100-KJNYMD-CHK-SEC
      *    選択番号１
           WHEN    "ACTIVATE"      ALSO    "SELNUM1"
               PERFORM 4100-SELNUM1-CHK-SEC
      *    選択番号
           WHEN    "ACTIVATE"      ALSO    "SELNUM2"
               PERFORM 4100-SELNUM2-CHK-SEC
      *    病室リスト
           WHEN    ANY             ALSO    "BRMLST"
               PERFORM 4100-BRMLST-CHK-SEC
      *    履歴リスト
           WHEN    ANY             ALSO    "RIREKILST"
               PERFORM 4100-RIREKILST-CHK-SEC
           WHEN    OTHER
      *
      *        入力個所のセット
               PERFORM 400-GMN-INPUT-SEC
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    ZERO        TO      SPA-GMN-W21-CUR
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
           IF    ( SPA-ERRCD   NOT =   SPACE )
             OR  ( FLG-END     NOT =   ZERO  )
               GO  TO  410-INPUT-CHK-EXT
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
       4101-GMN-SPA-SET-SEC            SECTION.
      *
      *    病室番号
           MOVE    W21-BRM-NUM     TO  SPA-GMN-W21-BRMNUM
      *
      *    病室種別
           MOVE    W21-BRM-SBT     TO  SPA-GMN-W21-SBT-G
      *
      *    特定入院料
           MOVE    W21-BRM-TOKUTEINYUIN
                                   TO  SPA-GMN-W21-TOKNYUIN-G
      *
      *    収容人数
           MOVE    W21-BRM-NINZU   TO  SPA-GMN-W21-NINZU
      *
      *    室料差額
           MOVE    W21-BRM-SAGAKU
                                   TO  SPA-GMN-W21-SAGAKU-G
      *
      *    性別特定
           MOVE    W21-BRM-SEX     TO  SPA-GMN-W21-SEX-G
      *
      *    内線番号
           MOVE    W21-BRM-NAITEL  TO  SPA-GMN-W21-NAITEL
      *
      *    診療科
           MOVE    W21-BRM-KA      TO  SPA-GMN-W21-SRYKA-G
      *
      *    入院基本料
           MOVE    W21-KHNRYO      TO  SPA-GMN-W21-KHNRYO-G
      *
           IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
               CONTINUE
           ELSE
      *        有効期間開始日
               MOVE    W21-YUKOSTYMD   TO  SPA-GMN-W21-YUKOSTYMD
      *
      *        有効期間終了日
               MOVE    W21-YUKOEDYMD   TO  SPA-GMN-W21-YUKOEDYMD
           END-IF
      *
      *    病棟コード（病棟機能）（特定入院料）
           MOVE    W21-TBTUTYPE    TO  SPA-GMN-W21-TBTUTYPE-G
      *
      *    病棟コード（連番）（特定入院料）
           MOVE    W21-TBTUCD      TO  SPA-GMN-W21-TBTUCD
      *
      *    入院料加算
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               MOVE    SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                   TO  IDX-GKSN
               IF    ( IDX-GKSN     >   ZERO )
                   MOVE    W21-KSN (IDX-GKSN)
                                   TO  SPA-GMN-W21-KSNCD-G
                                                      (IDX-KSN)
               END-IF
           END-PERFORM
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
      *    病棟名称チェック処理
           PERFORM 4101-BTUNUM-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 420-CLEAR-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        選択番号１チェック
               PERFORM 4101-SELNUM1-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間開始日チェック処理
               PERFORM 4100-YUKOSTYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間終了日チェック処理
               PERFORM 4100-YUKOEDYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病室番号チェック処理
               PERFORM 4100-BRMNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病室種別チェック処理
               PERFORM 4100-SBT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        特定入院料チェック処理
               PERFORM 4100-TOKNYUIN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        収容人数チェック処理
               PERFORM 4100-NINZU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        室料差額チェック処理
               PERFORM 4100-SAGAKU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        性別特定チェック処理
               PERFORM 4100-SEX-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        内線番号チェック処理
               PERFORM 4100-NAITEL-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        診療科チェック処理
               PERFORM 4100-SRYKA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        入院基本料チェック処理
               PERFORM 4100-KHNRYO-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟コード（病棟機能）（特定入院料）チェック処理
               PERFORM 4101-TBTUTYPE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病棟コード（連番）（特定入院料）チェック処理
               PERFORM 4101-TBTUCD-CHK-SEC
           END-IF
      *
      *    入院料加算チェック
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               PERFORM 41021-KSN-CHK-SEC
      *
           END-PERFORM
      *
      *    IF    ( SPA-ERRCD   =   SPACE )
      *        入院加算情報チェック処理-施設基準チェック処理
      *        PERFORM 4901-SSTKIJUN-CHK-SEC
      *    END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算チェック処理
      *****************************************************************
       41021-KSN-CHK-SEC           SECTION.
      *
           IF    ( SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
      *
               MOVE    SPA-GMN-W21-KSNCD (IDX-KSN)
                                   TO  WRK-CMB-CD
      *
               MOVE    SPACE       TO  WRK-CMB-ITM-G
               MOVE    SPA-GMN-W21-KSNLST-MAX (IDX-KSN)
                                   TO  WRK-MAX
               PERFORM VARYING IDXL    FROM    1   BY  1
                       UNTIL ( IDXL    >   WRK-MAX )
                        OR   ( WRK-CMB-ITM-G   NOT =   SPACE )
      *
                   IF    ( SPA-GMN-W21-KSNCDL (IDX-KSN IDXL)
                                   =   SPA-GMN-W21-KSNCD (IDX-KSN))
                       MOVE    SPA-GMN-W21-KSNLST (IDX-KSN IDXL)
                                   TO  WRK-CMB-ITM
                   END-IF
      *
               END-PERFORM
      *
               IF    ( WRK-CMB-ITM =   SPACE )
                   MOVE    SPA-GMN-W21-KSNLST (IDX-KSN 1)
                                   TO  SPA-GMN-W21-KSNCD-G (IDX-KSN)
               ELSE
                   MOVE    WRK-CMB-ITM
                                   TO  SPA-GMN-W21-KSNCD-G (IDX-KSN)
               END-IF
      *
           END-IF
      *
           .
       41021-KSN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF    ( SPA-W20-W21-MOTOPG  =   SPACE )
               MOVE    "W01"               TO  SPA-SAKIPG
           ELSE
               MOVE    SPA-W20-W21-MOTOPG  TO  SPA-SAKIPG
           END-IF
           MOVE    "W21"               TO  SPA-MOTOPG
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
      *    条件クリア処理
      *****************************************************************
       420-CLEAR-SEC               SECTION.
      *
           INITIALIZE  SPA-GMN-W21-RIREKILST
                       SPA-NAI-W21-RIREKILST
                       SPA-GMN-W21-BRMLST
                       SPA-NAI-W21-BRMLST
      *
           PERFORM 4201-HEAD-AREA-CLEAR-SEC
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
           MOVE    1               TO  SPA-GMN-W21-CUR
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       420-TUIKA-SEC               SECTION.
      *
      *    入力項目入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  420-TUIKA-EXT
           END-IF
      *
      *    共通関連チェック
           PERFORM 4900-KANRENCHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  420-TUIKA-EXT
           END-IF
      *
      *    既存の履歴データをチェックし、期間の重複したデータが
      *    存在しないこと
           MOVE    1                   TO  FLG-UPD-KBN
           PERFORM 4100-RIREKI-CHK-SEC
           EVALUATE    FLG-KIKAN-DABURI
           WHEN    ZERO
               PERFORM 4201-TUIKA-SEC
           WHEN    1
               MOVE    "2003"      TO  SPA-WIDCD
           WHEN    OTHER
               MOVE    "1104"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-W21-CUR
           END-EVALUATE
      *
           .
       420-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    追加処理
      *****************************************************************
       4201-TUIKA-SEC                  SECTION.
      *
      *    病室情報追加編集処理
           PERFORM 4201-BRM-INSERT-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               MOVE    SPA-GMN-W21-BTUNUM
                                   TO  W21-BTU-NAME
               PERFORM 4100-BTUNUM-CHK-SEC
      *
      *        病棟リスト最大件数チェック
               IF    ( SPA-GMN-W21-BRMLST-MAX  >=  200 )
                   MOVE    "1001"  TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4201-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    有効期間変更処理
      *****************************************************************
       4201-EDYUKYMD-UPDATE-SEC        SECTION.
      *
           PERFORM 900-SYSKANRI-KEY16-SEL-SEC
           IF    ( FLG-SYSKANRI    =   ZERO )
      *
               MOVE    SYS-5002-REC    TO  SYSKANRI-REC
               MOVE    SYS-KEY         TO  SYSUPD-KEY
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-W21-YUKOSTYMD
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  SYS-EDYUKYMD
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "upd1"          TO  MCP-PATHNAME
               PERFORM 910-DBUPDATE-SEC
               IF   (  MCP-RC      NOT =   ZERO )
                   MOVE    "1205"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4201-EDYUKYMD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    変更処理
      *****************************************************************
       420-HENKOU-SEC                  SECTION.
      *
      *    入力項目入力時チェック
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  420-HENKOU-EXT
           END-IF
      *
      *    選択番号が入力されていること
           IF    ( SPA-GMN-W21-SELNUM1 =   SPACE )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-W21-CUR
               GO  TO  420-HENKOU-EXT
           END-IF
      *
           IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
               PERFORM 4202-HENKOU-SEC
           ELSE
               PERFORM 4201-HENKOU-SEC
           END-IF
      *
           .
       420-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(通常の変更処理)
      *****************************************************************
       4201-HENKOU-SEC                 SECTION.
      *
      *    病室番号が変更されていないこと
           IF    ( SPA-GMN-W21-BRMNUM     =   SPA-NAI-W21-BRMNUM-OLD )
               CONTINUE
           ELSE
               MOVE    "1106"      TO  SPA-ERRCD
               MOVE    3           TO  SPA-GMN-W21-CUR
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        共通関連チェック
               PERFORM 4900-KANRENCHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        有効期間の開始日と終了日が同時に変更されていないこと
               IF    ( SPA-NAI-W21-YUKOSTYMD
                               NOT =   SPA-NAI-W21-YUKOSTYMD-OLD )
                AND
                     ( SPA-NAI-W21-YUKOEDYMD
                               NOT =   SPA-NAI-W21-YUKOEDYMD-OLD )
                   MOVE    "1107"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-W21-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        既存の履歴データをチェックし、期間の重複したデータが
      *        存在しないこと
               MOVE    2                   TO  FLG-UPD-KBN
               PERFORM 4100-RIREKI-CHK-SEC
               IF   ( FLG-KIKAN-DABURI NOT =   ZERO )
                   MOVE    "1104"          TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-W21-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        病室リスト更新処理
               PERFORM 4201-BRM-UPDATE-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               MOVE    SPA-GMN-W21-BTUNUM
                                   TO  W21-BTU-NAME
               PERFORM 4100-BTUNUM-CHK-SEC
           END-IF
      *
           .
       4201-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(加算一括変更)
      *****************************************************************
       4202-HENKOU-SEC                 SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W21-BRMLST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( SPA-NAI-W21-SELNUM-FLG (IDX0)   =   1 )
                   COMPUTE WRK-STIDX   =   IDX0    +   1
                   PERFORM VARYING IDX1    FROM    WRK-STIDX   BY  1
                           UNTIL ( IDX1    >   SPA-GMN-W21-BRMLST-MAX )
                            OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                       IF    ( SPA-NAI-W21-SELNUM-FLG (IDX1)   =   1 )
                           IF    ( SPA-NAI-W21-TTOKNYUIN (IDX0)
                                   NOT =   SPA-NAI-W21-TTOKNYUIN (IDX1))
                               MOVE    "1012"  TO  SPA-ERRCD
                               MOVE    2       TO  SPA-GMN-W21-CUR
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF    ( SPA-ERRCD   =   SPACE )
               MOVE    "2002"           TO  SPA-WIDCD
           END-IF
      *
           .
       4202-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    変更処理(加算一括変更)
      *****************************************************************
       42021-HENKOU-SEC                SECTION.
      *
           PERFORM VARYING IDX7    FROM    1   BY  1
                   UNTIL ( IDX7    >   SPA-GMN-W21-BRMLST-MAX )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF  ( SPA-NAI-W21-SELNUM-FLG (IDX7) =   1 )
      *
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    "5002"          TO  SYS-KANRICD
                   MOVE    SPA-NAI-W21-TBRMNUM (IDX7)
                                           TO  SYS-KBNCD
                   MOVE    SPA-NAI-W21-TYUKOSTYMD (IDX7)
                                           TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
      *
                       MOVE    SYSKANRI-REC    TO  SYS-5002-REC
      *
      *                病室加算情報編集処理
                       PERFORM 4200-BRM-KSN-EDIT-SEC
      *
                       MOVE    SYS-5002-REC    TO  SYSKANRI-REC
                       PERFORM 800-MCNDATE-SEC
                       MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
                       MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
                       MOVE    SYSKANRI-REC    TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"  TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBUPDATE-SEC
                       IF   (  MCP-RC          NOT =   ZERO )
                           MOVE    "1204"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-ERRCD       =    SPACE )
      *
               MOVE    SPA-GMN-W21-BTUNUM
                                   TO  W21-BTU-NAME
               PERFORM 4100-BTUNUM-CHK-SEC
           END-IF
      *
      *
           .
       42021-HENKOU-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       420-SAKUJO-SEC                  SECTION.
      *
           PERFORM 4101-SELNUM1-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               MOVE    2           TO  SPA-GMN-W21-CUR
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
      *    選択番号が入力されていること
           IF    ( SPA-GMN-W21-SELNUM1 =   SPACE )
               MOVE    "1105"      TO  SPA-ERRCD
               MOVE    2           TO  SPA-GMN-W21-CUR
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
      *    入院中の患者が存在しないかチェック
           PERFORM 900-PTNYUINRRK-KEY15-SEL-SEC
           IF    ( FLG-PTNYUINRRK  =   ZERO )
               MOVE    "1108"      TO  SPA-ERRCD
               GO  TO  420-SAKUJO-EXT
           END-IF
      *
           MOVE    "2001"          TO  SPA-WIDCD
      *
           .
       420-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       421-SAKUJO-SEC                  SECTION.
      *
      *    病室情報削除処理
           PERFORM  4201-BRM-DELETE-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  421-SAKUJO-EXT
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
               MOVE    SPA-GMN-W21-BTUNUM
                                   TO  W21-BTU-NAME
               PERFORM 4100-BTUNUM-CHK-SEC
           END-IF
      *
           .
       421-SAKUJO-EXT.
           EXIT.
      *****************************************************************
      *    病棟管理遷移処理
      *****************************************************************
       420-BYOTO-KANRI-SENI-SEC        SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           INITIALIZE  SPA-W20-W21-AREA
      *
           MOVE    SPA-GMN-W21-BTUNUM  TO  SPA-W20-W21-BTUNUM
      *
           MOVE    "W20"               TO  SPA-SAKIPG
           MOVE    "W21"               TO  SPA-MOTOPG
                                           SPA-W20-W21-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       420-BYOTO-KANRI-SENI-EXT.
           EXIT.
      *****************************************************************
      *    履歴チェック処理
      *****************************************************************
       4100-RIREKI-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-KIKAN-DABURI
      *
      *    病棟情報検索処理
           PERFORM 4100-BRM-SEL-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( FLG-SYSKANRI    NOT =   ZERO )
      *
      *        有効期間が重複するデータがないかチェック
      *        （変更処理の場合、元データは対象外とする）
               IF    ( FLG-UPD-KBN           =  2 )
                AND  ( SPA-NAI-W21-BRMNUM-OLD
                                             =   SYS-5002-BRM-NUM  )
                AND  ( SPA-NAI-W21-YUKOSTYMD-OLD
                                             =   SYS-5002-STYUKYMD )
                AND  ( SPA-NAI-W21-YUKOEDYMD-OLD
                                             =   SYS-5002-EDYUKYMD )
                   CONTINUE
               ELSE
                   IF    ( SPA-NAI-W21-YUKOEDYMD
                                             <   SYS-5002-STYUKYMD )
                     OR  ( SPA-NAI-W21-YUKOSTYMD
                                             >   SYS-5002-EDYUKYMD )
                       CONTINUE
                   ELSE
                       IF    ( SPA-NAI-W21-YUKOSTYMD
                                           >   SYS-5002-STYUKYMD )
                        AND  ( SPA-NAI-W21-YUKOSTYMD
                                          <=   SYS-5002-EDYUKYMD )
                           MOVE    1       TO  FLG-KIKAN-DABURI
                       ELSE
                           MOVE    2       TO  FLG-KIKAN-DABURI
                       END-IF
                   END-IF
               END-IF
      *
      *        病棟データ読込処理
               PERFORM 4100-BRM-FET-SEC
      *
           END-PERFORM
      *
      *    病棟データクローズ処理
           PERFORM 4100-BRM-CLOSECURSOR-SEC
           .
      *
       4100-RIREKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室情報検索処理
      *****************************************************************
       4100-BRM-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
      *
           MOVE    SPA-GMN-W21-BTUNUM  TO  WRK-SYS5002-BTUNUM
           MOVE    SPA-GMN-W21-BRMNUM  TO  WRK-BRMNUM
           PERFORM 4100-BRMNUM-KEY-EDIT-SEC
           MOVE    WRK-BRMNUM-KEY      TO  WRK-SYS5002-BRMNUM
      *
           MOVE    WRK-SYS5002-KBNCD   TO  SYS-5002-KBNCD
           MOVE    SYS-5002-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY8-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5002-REC
      *
           .
       4100-BRM-SEL-EXT.
           EXIT.
      *****************************************************************
      *    病室情報読込処理
      *****************************************************************
       4100-BRM-FET-SEC               SECTION.
      *
           PERFORM 900-SYSKANRI-KEY8-FET-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5002-REC
      *
           .
       4100-BRM-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    病室情報クローズ処理
      *****************************************************************
       4100-BRM-CLOSECURSOR-SEC              SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4100-BRM-CLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    病室番号左詰処理
      *****************************************************************
       4100-BRM-NUM-LEFT-EDIT-SEC      SECTION.
      *
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    "1"             TO  SBRMNUM-IN-PROCKBN
           MOVE    WRK-BRMNUM-IN   TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           MOVE    SBRMNUM-OT-BRMNUM
                                   TO  WRK-BRMNUM-OT
      *
           .
       4100-BRM-NUM-LEFT-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室番号キー作成処理
      *****************************************************************
       4100-BRMNUM-KEY-EDIT-SEC       SECTION.
      *
           INITIALIZE                  ORCSBRMNUMAREA
           MOVE    "2"             TO  SBRMNUM-IN-PROCKBN
           MOVE    WRK-BRMNUM      TO  SBRMNUM-IN-BRMNUM
           CALL    "ORCSBRMNUM"    USING   ORCSBRMNUMAREA
           MOVE    SBRMNUM-OT-BRMNUM
                                   TO  WRK-BRMNUM-KEY
      *
           .
       4100-BRMNUM-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室情報更新処理
      *****************************************************************
       4201-BRM-UPDATE-SEC             SECTION.
      *
      *    アップデート対象のデータを読み直す
           MOVE    SPA-GMN-W21-BRMNUM
                                   TO  WRK-BRMNUM
           MOVE    SPA-NAI-W21-YUKOSTYMD-OLD
                                   TO  WRK-KJNYMD
           PERFORM 4200-BRM-SEL-SEC
           IF   ( FLG-SYSKANRI     NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
               GO  TO  4201-BRM-UPDATE-EXT
           END-IF
      *
      *    病室情報編集処理
           PERFORM 4201-BRM-EDIT-SEC
      *
      *    病室加算情報編集処理
           PERFORM 4200-BRM-KSN-EDIT-SEC
      *
           MOVE    SYS-5002-REC    TO  SYSKANRI-REC
           MOVE    SYS-5002-HOSPNUM TO SYSUP-HOSPNUM
           MOVE    SYS-5002-KANRICD
                                   TO  SYSUP-KANRICD
           MOVE    SPA-NAI-W21-5002-KBNCD
                                   TO  SYSUP-KBNCD
           MOVE    SPA-NAI-W21-YUKOSTYMD-OLD
                                   TO  SYSUP-STYUKYMD
           MOVE    SPA-NAI-W21-YUKOEDYMD-OLD
                                   TO  SYSUP-EDYUKYMD
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-UPHMS
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "upd1"          TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
           END-IF
           .
      *
       4201-BRM-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    病室情報追加処理
      *****************************************************************
       4201-BRM-INSERT-SEC             SECTION.
      *
      *    更新対象のデータが既に存在しないかチェック
           MOVE    SPA-GMN-W21-BRMNUM
                                   TO  WRK-BRMNUM
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  WRK-KJNYMD
           PERFORM 4200-BRM-SEL-SEC
           IF   ( FLG-SYSKANRI     =   ZERO )
               MOVE    "1201"      TO  SPA-ERRCD
               GO  TO  4201-BRM-INSERT-EXT
           END-IF
      *
           MOVE    SPACE       TO  SYS-5002-REC
           INITIALIZE              SYS-5002-REC
      *
      *    病室情報編集処理
           PERFORM 4201-BRM-EDIT-SEC
      *
      *    病室加算情報編集処理
           PERFORM 4200-BRM-KSN-EDIT-SEC
      *
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  SYS-5002-CREYMD
                                       SYS-5002-UPYMD
           MOVE    SMCNDATE-HMS    TO  SYS-5002-UPHMS
      *
           MOVE    SYS-5002-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "1202"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4201-BRM-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    病室情報削除処理
      *****************************************************************
       4201-BRM-DELETE-SEC             SECTION.
      *
           INITIALIZE              SYS-5002-REC
           MOVE    SPA-HOSPNUM     TO  SYS-5002-HOSPNUM
           MOVE    "5002"          TO  SYS-5002-KANRICD
           MOVE    SPA-NAI-W21-5002-KBNCD
                                   TO  SYS-5002-KBNCD
           MOVE    SPA-NAI-W21-YUKOSTYMD-OLD
                                   TO  SYS-5002-STYUKYMD
           MOVE    SPA-NAI-W21-YUKOEDYMD-OLD
                                   TO  SYS-5002-EDYUKYMD
           MOVE    SYS-5002-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF   (  MCP-RC          NOT =   ZERO )
               MOVE    "1203"      TO  SPA-ERRCD
           END-IF
      *
           .
       4201-BRM-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    病室情報編集処理
      *****************************************************************
       4201-BRM-EDIT-SEC               SECTION.
      *
      *    MOVE    SPACE       TO  SYS-5002-REC
      *    INITIALIZE              SYS-5002-REC
      *
           MOVE    SPA-HOSPNUM TO  SYS-5002-HOSPNUM
      *
      *    管理コード
           MOVE    "5002"      TO  SYS-5002-KANRICD
      *
      *    区分コード
           MOVE    SPA-GMN-W21-BTUNUM
                               TO  WRK-SYS5002-BTUNUM
      *
           MOVE    SPA-GMN-W21-BRMNUM
                               TO  WRK-BRMNUM
           PERFORM 4100-BRMNUM-KEY-EDIT-SEC
           MOVE    WRK-BRMNUM-KEY
                               TO  WRK-SYS5002-BRMNUM
      *
           MOVE    WRK-SYS5002-KBNCD
                               TO  SYS-5002-KBNCD
      *
      *    有効開始年月日
           MOVE    SPA-NAI-W21-YUKOSTYMD
                               TO  SYS-5002-STYUKYMD
      *
      *    有効終了年月日
           MOVE    SPA-NAI-W21-YUKOEDYMD
                               TO  SYS-5002-EDYUKYMD
      *
      *    病室番号
           MOVE    SPA-GMN-W21-BRMNUM
                               TO  SYS-5002-BRM-NUM
      *
      *    病室種別
           MOVE    SPA-GMN-W21-SBT
                               TO  SYS-5002-BRM-SBT
      *
      *    特定入院料
           MOVE    SPA-NAI-W21-TOKNYUIN
                               TO  SYS-5002-BRM-TOKTEI-NYUIN
      *
      *    収容人数
           MOVE    SPA-GMN-W21-NINZU
                               TO  SYS-5002-BRM-NINZU
      *
      *    室料差額
           MOVE    SPA-GMN-W21-SAGAKU
                               TO  SYS-5002-BRM-SAGAKU
      *
      *    性別特定
           MOVE    SPA-GMN-W21-SEX
                               TO  SYS-5002-BRM-SEX
      *
      *    内線番号
           MOVE    SPA-GMN-W21-NAITEL
                               TO  SYS-5002-BRM-NAITEL
      *
      *    診療科
           MOVE    SPA-GMN-W21-SRYKA
                               TO  SYS-5002-BRM-KA
      *
      *    入院基本料
           MOVE    SPA-NAI-W21-KHNSRYCD
                               TO  SYS-5002-KHNSRYCD
           MOVE    "3"         TO  SYS-5002-KHNKBNCD (1:1)
           MOVE    SPA-GMN-W21-KHNRYO
                               TO  SYS-5002-KHNKBNCD (2:2)
      *
      *    病棟コード
           IF    ( SPA-GMN-W21-TBTUTYPE NOT =   SPACE )
               MOVE    SPACE           TO  SYS-5002-TSRYCD
               MOVE    "1906"          TO  SYS-5002-TSRYCD
               MOVE    SPA-GMN-W21-TBTUTYPE
                                       TO  SYS-5002-TSRYCD(5:1)
               MOVE    SPA-GMN-W21-TBTUCD
                                       TO  WRK-KBN4
               MOVE    WRK-KBN4-G      TO  SYS-5002-TSRYCD(6:)
           ELSE
               MOVE    SPACE           TO  SYS-5002-TSRYCD
           END-IF
      *
           .
       4201-BRM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室情報追加処理
      *****************************************************************
       4200-BRM-KSN-EDIT-SEC           SECTION.
      *
           INITIALIZE                  TKSN-AREA
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W21-YUKOSTYMD
                                  >=   CONST-H240401 )
               PERFORM 4202-H24R02-BRM-KSN-EDIT-SEC
           WHEN  OTHER
               PERFORM 4201-BRM-KSN-EDIT-SEC
           END-EVALUATE
      *
           .
       4200-BRM-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室情報編集処理
      *****************************************************************
       4202-H24R02-BRM-KSN-EDIT-SEC        SECTION.
      *
           MOVE    ZERO                TO  CNT-KSN
      *
           INITIALIZE  SYS-5002-KSNAREA
      *
      *    療養環境加算
           IF    ( SPA-GMN-W21-RYO         =   "2" )
               MOVE    CONST-RYOYO             TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-RYO-IDX     TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-IF
      *
      *    重症者等療養環境特別加算
           EVALUATE    SPA-GMN-W21-JYUTOK
           WHEN    "2"
               MOVE    CONST-JYUTOK1           TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-JYUTOK-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-JYUTOK2           TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-JYUTOK-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    療養病棟療養環境加算
           EVALUATE    SPA-GMN-W21-BTURYO
           WHEN    "2"
               MOVE    CONST-H2404-BTURYO1     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-BTURYO-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-BTURYO2     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-BTURYO-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "4"
               MOVE    CONST-H2404-BTURYO3     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-BTURYO-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "5"
               MOVE    CONST-H2404-BTURYO4     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-BTURYO-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    療養病棟療養環境改善加算
           EVALUATE    SPA-GMN-W21-BTURYOKAI
           WHEN    "2"
               MOVE    CONST-H2404-BTURYOKAI1  TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-BTURYOKAI-IDX
                                               TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-BTURYOKAI2  TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-BTURYOKAI-IDX
                                               TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    診療所療養病床療養環境加算
           EVALUATE    SPA-GMN-W21-SNRRYO
           WHEN    "2"
               MOVE    CONST-H2404-SNRRYO1     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-SNRRYO-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-SNRRYO2     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-SNRRYO-IDX  TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    診療所療養病床療養環境改善加算
           IF    ( SPA-GMN-W21-SNRRYOKAI   =   "2" )
               MOVE    CONST-H2404-SNRRYOKAI   TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-SNRRYOKAI-IDX
                                               TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-IF
      *
      *    無菌治療室管理加算
           EVALUATE    SPA-GMN-W21-MUKIN
           WHEN    "2"
               MOVE    CONST-H2404-MUKIN1      TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-MUKIN-IDX   TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "3"
               MOVE    CONST-H2404-MUKIN1K     TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-MUKIN-IDX   TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           WHEN    "4"
               MOVE    CONST-H2404-MUKIN2      TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-MUKIN-IDX   TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-EVALUATE
      *
      *    放射線治療病室管理加算
           IF    ( SPA-GMN-W21-HOSYASEN    =   "2" )
               MOVE    CONST-HOSYASEN          TO  WRK-KSNCD
               MOVE    SPA-NAI-W21-HOSYASEN-IDX
                                               TO  WRK-KSNIDX
               PERFORM 42021-BRM-KSN-EDIT-SEC
           END-IF
      *
           IF  ( SPA-NAI-W21-YUKOSTYMD    >=   CONST-R020401 )
               IF    ( SPA-GMN-W21-SOKIEIYO    =   "2" )
                   MOVE    CONST-R02-A301C5    TO  WRK-KSNCD
                   MOVE    SPA-NAI-W21-SOKIEIYO-IDX
                                               TO  WRK-KSNIDX
                   PERFORM 42021-BRM-KSN-EDIT-SEC
               END-IF
           END-IF
      *
           .
       4202-H24R02-BRM-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟情報編集処理(H24.04〜)
      *****************************************************************
       42021-BRM-KSN-EDIT-SEC          SECTION.
      *
           IF    ( CONST-5002-KSN-MAX   >  CNT-KSN )
               COMPUTE CNT-KSN =   CNT-KSN +   1
      *
               MOVE    WRK-KSNCD   TO SYS-5002-KSNCD (CNT-KSN)
               MOVE    "2"         TO SYS-5002-KSNKBN (CNT-KSN)
           END-IF
      *
           IF    ( CONST-KSN-MAX       >   TKSN-MAX )
               COMPUTE TKSN-MAX    =   TKSN-MAX    +   1
               MOVE    WRK-KSNCD   TO  TKSN-SRYCD (TKSN-MAX)
               MOVE    WRK-KSNIDX  TO  TKSN-IDX   (TKSN-MAX)
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   CONST-KSN-MAX )
                        OR   ( TKSN-LBL (TKSN-MAX) NOT =   SPACE )
      *
                   IF    ( SPA-NAI-W21-KSNINF-IDX (IDX0)
                                       =   WRK-KSNIDX )
                       MOVE    SPA-GMN-W21-KSN-LBL (IDX0)
                                   TO  TKSN-LBL (TKSN-MAX)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       42021-BRM-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病室情報編集処理
      *****************************************************************
       4201-BRM-KSN-EDIT-SEC           SECTION.
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD   >=  CONST-H220401 )
               CONTINUE
           ELSE
      *        新生児入院医療管理加算
               MOVE    CONST-SINSEIJI  TO  SYS-5002-SINSEIJI-CD
               IF    ( SPA-GMN-W21-SINSEIJI    = "2" )
                   MOVE    "2"         TO  SYS-5002-SINSEIJI-KBN
               ELSE
                   MOVE    "1"         TO  SYS-5002-SINSEIJI-KBN
               END-IF
           END-IF
      *
      *    診療所療養病床療養環境加算
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
               MOVE    CONST-SNR-RYOYO (IDX1)
                                   TO  SYS-5002-SNR-RYOYO-CD  (IDX1)
               MOVE    "1"         TO  SYS-5002-SNR-RYOYO-KBN (IDX1)
           END-PERFORM
           EVALUATE    SPA-GMN-W21-SNRRYO
           WHEN    "2"
               MOVE    "2"         TO  SYS-5002-SNR-RYOYO-KBN (1)
           WHEN    "3"
               MOVE    "2"         TO  SYS-5002-SNR-RYOYO-KBN (2)
           END-EVALUATE
      *
      *    放射線治療病室管理加算
           MOVE    CONST-HOSYASEN  TO  SYS-5002-HOSYASEN-CD
           IF    ( SPA-GMN-W21-HOSYASEN    =   "2" )
               MOVE    "2"         TO  SYS-5002-HOSYASEN-KBN
           ELSE
               MOVE    "1"         TO  SYS-5002-HOSYASEN-KBN
           END-IF
      *
      *    療養環境加算
           MOVE    CONST-RYOYO     TO  SYS-5002-RYOYO-CD
           IF    ( SPA-GMN-W21-RYO     =   "2" )
               MOVE    "2"             TO  SYS-5002-RYOYO-KBN
           ELSE
               MOVE    "1"             TO  SYS-5002-RYOYO-KBN
           END-IF
      *
      *    療養病棟療養環境加算
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   3 )
               MOVE    CONST-BTU-RYOYO (IDX1)
                                   TO  SYS-5002-BTU-RYOYO-CD  (IDX1)
               MOVE    "1"         TO  SYS-5002-BTU-RYOYO-KBN (IDX1)
           END-PERFORM
           MOVE    CONST-BTU-RYOYO-H18
                                   TO  SYS-5002-BTU-RYOYO-H18-CD
           MOVE    "1"             TO  SYS-5002-BTU-RYOYO-H18-KBN
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-W21-YUKOSTYMD   >=  CONST-H180401 )
               EVALUATE    SPA-GMN-W21-BTURYO
               WHEN    "2"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-H18-KBN
               WHEN    "3"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (1)
               WHEN    "4"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (2)
               WHEN    "5"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (3)
               END-EVALUATE
           WHEN    OTHER
               EVALUATE    SPA-GMN-W21-BTURYO
               WHEN    "2"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (1)
               WHEN    "3"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (2)
               WHEN    "4"
                   MOVE    "2"     TO  SYS-5002-BTU-RYOYO-KBN (3)
               END-EVALUATE
           END-EVALUATE
      *
      *    重症者等療養環境特別加算（個室）
           MOVE    CONST-JYUTOK1   TO  SYS-5002-JYUTOK1-CD
           MOVE    "1"             TO  SYS-5002-JYUTOK1-KBN
      *
      *    重症者等療養環境特別加算（２人部屋）
           MOVE    CONST-JYUTOK2   TO  SYS-5002-JYUTOK2-CD
           MOVE    "1"             TO  SYS-5002-JYUTOK2-KBN
           EVALUATE    SPA-GMN-W21-JYUTOK
           WHEN    "2"
               MOVE    "2"         TO  SYS-5002-JYUTOK1-KBN
           WHEN    "3"
               MOVE    "2"         TO  SYS-5002-JYUTOK2-KBN
           END-EVALUATE
      *
      *    無菌治療室管理加算
           MOVE    CONST-MUKIN     TO  SYS-5002-MUKIN-CD
           IF    ( SPA-GMN-W21-MUKIN   =   "2" )
               MOVE    "2"         TO  SYS-5002-MUKIN-KBN
           ELSE
               MOVE    "1"         TO  SYS-5002-MUKIN-KBN
           END-IF
      *
           .
       4201-BRM-KSN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称
      *****************************************************************
       4100-BTUNUM-CHK-SEC           SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-W21-CUR
      *
      *    クリア処理
           PERFORM 420-CLEAR-SEC
      *
           MOVE    SPACE       TO  SPA-GMN-W21-BTUKHNRYO
      *
           MOVE    W21-BTU-NAME    TO  SPA-GMN-W21-BTUNUM-G
      *
           PERFORM 4101-BTUNUM-CHK-SEC
           IF   (  SPA-ERRCD   NOT =   SPACE )
               GO  TO  4100-BTUNUM-CHK-EXT
           END-IF
      *
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
      *    入院基本料編集処理
           PERFORM 3101-BTU-KHNRYO-EDIT-SEC
      *
      *    病室管理情報検索（病室リストに表示）
           PERFORM 3101-SYS-5002-GET-SEC
      *
           MOVE    ZERO        TO  SPA-GMN-W21-CUR
      *
           .
      *
       4100-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟名称
      *****************************************************************
       4101-BTUNUM-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W21-BTUNUM-G
                                   =   SPACE )
               MOVE    "1006"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-W21-CUR
               GO  TO  4101-BTUNUM-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-BTUNUM
                                   TO  WRK-CMB-CD
           PERFORM 4200-BTUNUMLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    "1007"      TO  SPA-ERRCD
               MOVE    1           TO  SPA-GMN-W21-CUR
               GO  TO  4101-BTUNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-W21-BTUNUM-G
           MOVE    WRK-CMB-ITM2    TO  WRK-TOKNYUIN
           MOVE    WRK-CMB-ITM3    TO  WRK-KHNSRYCD
           MOVE    WRK-CMB-ITM4    TO  WRK-BTU-YUKSTYMD
           MOVE    WRK-CMB-ITM5    TO  SPA-NAI-W21-BTUSBT
           .
      *
       4101-BTUNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    基準日
      *****************************************************************
       4100-KJNYMD-CHK-SEC             SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-W21-CUR
           IF    ( W21-KJNYMD  =   SPACE )
               MOVE    SPA-NAI-W21-KJNYMD
                               TO  W21-KJNYMD
           END-IF
           MOVE    W21-KJNYMD  TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF  ( SPA-ERRCD         =   SPACE )
      *
               MOVE    SPA-GMN-W21-BTUNUM
                                   TO   WRK-BTUNUM
      *
               INITIALIZE              SPA-W21-AREA
      *
               MOVE    WRK-HENYMDG TO  SPA-GMN-W21-KJNYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W21-KJNYMD
      *
               PERFORM 311-SPASET-SEC
      *
               MOVE    WRK-BTUNUM  TO  W21-BTU-NAME
               PERFORM 4100-BTUNUM-CHK-SEC
               MOVE    SPACE       TO  SPA-ERRCD
      *
           ELSE
               MOVE    "0014"      TO  SPA-ERRCD
               MOVE    101         TO  SPA-GMN-W21-CUR
           END-IF
      *
           .
      *
       4100-KJNYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１
      *****************************************************************
       4100-SELNUM1-CHK-SEC            SECTION.
      *
           MOVE    ZERO        TO  SPA-GMN-W21-CUR
      *
           INITIALIZE                  WRK-SELNUM-G
           MOVE    ZERO            TO  SPA-NAI-W21-INPUT-MODE
      *
           MOVE    W21-SELNUM1 TO  SPA-GMN-W21-SELNUM1
      *
           IF    ( SPA-GMN-W21-SELNUM1 =  SPACE )
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4201-RIREKI-AREA-CLEAR-SEC
               MOVE    3           TO  SPA-GMN-W21-CUR
               GO  TO  4100-SELNUM1-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W21-SELNUM1 TO  WRK-KANACHK-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-W21-SELNUM1
      *
           PERFORM 4101-SELNUM1-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4201-RIREKI-AREA-CLEAR-SEC
               MOVE    W21-SELNUM1     TO  SPA-GMN-W21-SELNUM1
               GO  TO  4100-SELNUM1-CHK-EXT
           END-IF
      *
      *    入力項目領域編集処理
           MOVE    SPA-GMN-W21-TBRMNUM2 (WRK-SELNUM)
                                       TO  WRK-BRMNUM
           MOVE    SPA-NAI-W21-KJNYMD  TO  WRK-KJNYMD
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
      *    履歴領域初期化
           PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
           IF    ( WRK-INPUT-MODE      =   2 )
               MOVE    W21-SELNUM1 TO  SPA-GMN-W21-SELNUM1
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SPA-GMN-W21-BRMLST-MAX )
                   MOVE    WRK-SELNUM-FLG (IDX0)
                                   TO  SPA-NAI-W21-SELNUM-FLG (IDX0)
               END-PERFORM
           END-IF
      *
      *    入力データ編集処理
           MOVE    WRK-INPUT-MODE  TO  SPA-NAI-W21-INPUT-MODE
           PERFORM 4100-INPUT-AREA-EDIT-SEC
           IF    ( FLG-INPUT-AREA  =   ZERO )
      *
               MOVE    WRK-SELNUM  TO  SPA-GMN-W21-BRMLST-SEL
      *
               IF    ( SPA-NAI-W21-INPUT-MODE  =   1 )
      *            履歴データ編集処理
                   PERFORM 4100-RIREKI-AREA-EDIT-SEC
               END-IF
           ELSE
               PERFORM 4201-INPUT-AREA-CLEAR-SEC
               PERFORM 4201-RIREKI-AREA-CLEAR-SEC
               MOVE    W21-SELNUM1 TO  SPA-GMN-W21-SELNUM1
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-W21-CUR
      *
           .
      *
       4100-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       4101-SELNUM1-CHK-SEC            SECTION.
      *
           INITIALIZE                  TSPLIT-CONF-DATE-AREA
           MOVE    SPACE           TO  WRK-MSGPLUS
      *
           IF    ( SPA-GMN-W21-SELNUM1 =  SPACE )
               CONTINUE
           ELSE
               IF    ( SPA-GMN-W21-SELNUM1 (1:1)    =   "." )
                   MOVE    SPA-GMN-W21-SELNUM1 (2:)
                                           TO  WRK-SELNUM-MUL
               ELSE
                   MOVE    SPA-GMN-W21-SELNUM1 TO  WRK-SELNUM-MUL
               END-IF
      *
               PERFORM 41011-MULTI-SEL-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( WRK-INPUT-MODE  =   2 )
                   PERFORM 41011-SELNUM1-CHK-SEC
               END-IF
           END-IF
      *
           .
      *
       4101-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       41011-SELNUM1-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-W21-BRMLST-MAX )
      *
               IF    ( WRK-SELNUM-FLG (IDX1)   =   1 )
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0
                               >   CONST-SPLIT-CONF-DATE-MAX )
                       IF    ( SPA-NAI-W21-TYUKOSTYMD (IDX1)
                               >= CONST-SPLIT-CONF-DATE (IDX0))
                           MOVE  1 TO  TSPLIT-CONF-DATE-AFTER (IDX0)
                       ELSE
                           MOVE  1 TO  TSPLIT-CONF-DATE-BEFORE(IDX0)
                       END-IF
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0  > CONST-SPLIT-CONF-DATE-MAX )
      *
               IF    ( TSPLIT-CONF-DATE-AFTER (IDX0)   =   1 )
                AND  ( TSPLIT-CONF-DATE-BEFORE(IDX0)   =   1 )
                   MOVE    "0016"  TO  SPA-ERRCD
                   MOVE    CONST-SPLIT-CONF-DATE (IDX0)
                                   TO  WRK-YMD
                   PERFORM 800-HIZUKE-SEC
                   MOVE    WRK-EDTYMD3(1:16)
                                   TO  WRK-MSGPLUS
                   MOVE    2       TO  SPA-GMN-W21-CUR
               END-IF
      *
           END-PERFORM
      *
           .
      *
       41011-SELNUM1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    複数行選択チェック処理
      *****************************************************************
       41011-MULTI-SEL-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-MULTI-AREA
      *
           MOVE    ZERO            TO  WRK-INPUT-MODE
      *
           UNSTRING    WRK-SELNUM-MUL  DELIMITED   BY  ","
                                                   OR  "."
           INTO        WRK-MUL-NUM (1)
                       WRK-MUL-NUM (2)
                       WRK-MUL-NUM (3)
                       WRK-MUL-NUM (4)
                       WRK-MUL-NUM (5)
                       WRK-MUL-NUM (6)
                       WRK-MUL-NUM (7)
                       WRK-MUL-NUM (8)
                       WRK-MUL-NUM (9)
                       WRK-MUL-NUM (10)
                       WRK-MUL-NUM (11)
                       WRK-MUL-NUM (12)
                       WRK-MUL-NUM (13)
                       WRK-MUL-NUM (14)
                       WRK-MUL-NUM (15)
                       WRK-MUL-NUM (16)
                       WRK-MUL-NUM (17)
                       WRK-MUL-NUM (18)
                       WRK-MUL-NUM (19)
                       WRK-MUL-NUM (20)
           ON  OVERFLOW
               MOVE    2           TO  SPA-GMN-W21-CUR
               MOVE    "0015"      TO  SPA-ERRCD  
               GO  TO  41011-MULTI-SEL-CHK-EXT
           END-UNSTRING
      *
           MOVE    ZERO            TO  CNT-SEL
                                       WRK-SELNUM
           PERFORM VARYING IDX-MUL    FROM    1   BY  1
                   UNTIL ( IDX-MUL    >       20 )
      *
               IF    ( WRK-MUL-NUM (IDX-MUL)  =   SPACE )
                   CONTINUE
               ELSE
      *
                   INITIALIZE  WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                               WRK-MUL-STNUM
                               WRK-MUL-EDNUM
      *
                   UNSTRING    WRK-MUL-NUM (IDX-MUL) DELIMITED   BY  "-"
                   INTO        WRK-MUL-STNUM-X
                               WRK-MUL-EDNUM-X
                   END-UNSTRING
      *
                   IF    ( WRK-MUL-STNUM-X     =   SPACE )
                    AND  ( WRK-MUL-EDNUM-X     =   SPACE )
                       MOVE    1           TO  SPA-GMN-W21-CUR
                       MOVE    "0001"      TO  SPA-ERRCD
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   ELSE
                       EVALUATE    TRUE
                       WHEN  ( WRK-MUL-EDNUM-X     =   SPACE )
                           MOVE    WRK-MUL-STNUM-X
                                           TO  WRK-MUL-EDNUM-X
                       WHEN  ( WRK-MUL-STNUM-X     =   SPACE )
                           MOVE    WRK-MUL-EDNUM-X
                                           TO  WRK-MUL-STNUM-X
                       END-EVALUATE
                   END-IF
      *
                   MOVE    WRK-MUL-STNUM-X     TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-STNUM
      *
                   MOVE    WRK-MUL-EDNUM-X TO  WRK-SELNUM-X
                   PERFORM 4101-SELNUM-LIMIT-CHK-SEC
                   IF    ( SPA-ERRCD   NOT =   SPACE )
                       GO  TO  41011-MULTI-SEL-CHK-EXT
                   END-IF
                   MOVE    WRK-SELNUM  TO  WRK-MUL-EDNUM
      *
      *
                   PERFORM VARYING IDX1    FROM    WRK-MUL-STNUM   BY  1
                           UNTIL ( IDX1    >       WRK-MUL-EDNUM )
                       IF    ( IDX1        >=  1 )
                        AND  ( IDX1        <=  SPA-GMN-W21-BRMLST-MAX )
                           COMPUTE CNT-SEL  =   CNT-SEL  +   1
                           IF    ( CNT-SEL  =   1 )
                               MOVE    IDX1    TO  WRK-SELNUM
                           END-IF
                           MOVE    1       TO  WRK-SELNUM-FLG (IDX1)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           MOVE    ZERO            TO  CNT-SEL
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       SPA-GMN-W21-BRMLST-MAX )
      *
               IF    ( WRK-SELNUM-FLG (IDX1)   =   1 )
                   COMPUTE CNT-SEL =   CNT-SEL +   1
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SEL         >   1 )
               MOVE    2           TO  WRK-INPUT-MODE
           ELSE
               MOVE    1           TO  WRK-INPUT-MODE
           END-IF
      *
           .
       41011-MULTI-SEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号範囲チェック処理
      *****************************************************************
       4101-SELNUM-LIMIT-CHK-SEC       SECTION.
      *
           INITIALIZE                  ORCSNUMAREA
           MOVE    WRK-SELNUM-X    TO  SNUM-INX
           CALL    "ORCSNUM"       USING   ORCSNUMAREA
           IF    ( SNUM-RC         =   ZERO )
            AND  ( SNUM-MINKBN     =   SPACE )
            AND  ( SNUM-SYOKBN     =   SPACE ) 
               MOVE    SNUM-OUTNUM TO  WRK-SELNUM
               IF    ( WRK-SELNUM  >=  1 )
                AND  ( WRK-SELNUM  <=  SPA-GMN-W21-BRMLST-MAX )
                   CONTINUE
               ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    2       TO  SPA-GMN-W21-CUR
               END-IF
      *
           ELSE
                   MOVE    "0001"  TO  SPA-ERRCD
                   MOVE    2       TO  SPA-GMN-W21-CUR
           END-IF
      *
           .
       4101-SELNUM-LIMIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室番号
      *****************************************************************
       4100-BRMNUM-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-W21-BRMNUM =   SPACE   )
               GO  TO  4100-BRMNUM-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-W21-BRMNUM  TO  WRK-KANACHK-INPUT
      *
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-W21-BRMNUM
           IF  ( FLG-ENTRYCHK      NOT =   1 )
               MOVE    3               TO  SPA-GMN-W21-CUR
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO  4100-BRMNUM-CHK-EXT
           END-IF
      *
      *    左詰編集を行う
           MOVE    SPA-GMN-W21-BRMNUM TO  WRK-BRMNUM-IN
           PERFORM 4100-BRM-NUM-LEFT-EDIT-SEC
           MOVE    WRK-BRMNUM-OT      TO  SPA-GMN-W21-BRMNUM
      *
      *    間に空白文字が混入していないかチェック
           MOVE    SPACE           TO  WRK-STR1
                                       WRK-STR2
           UNSTRING    SPA-GMN-W21-BRMNUM
           DELIMITED   BY  ALL SPACE
           INTO        WRK-STR1
                       WRK-STR2
           END-UNSTRING
      *
           IF    ( WRK-STR2        NOT =   SPACE )
               MOVE    3               TO  SPA-GMN-W21-CUR
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4100-BRMNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室種別
      *****************************************************************
       4100-SBT-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-W21-SBT-G   =   SPACE )
               GO  TO  4100-SBT-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-SBT TO  WRK-CMB-CD
           PERFORM 4200-SBTLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    4           TO  SPA-GMN-W21-CUR
               MOVE    "0003"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W21-SBT-G
           END-IF
      *
      *
           .
      *
       4100-SBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料
      *****************************************************************
       4100-TOKNYUIN-CHK-SEC   SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-W21-TOKNYUIN
      *
           IF    ( SPA-GMN-W21-TOKNYUIN-G  =   SPACE )
               GO  TO  4100-TOKNYUIN-CHK-EXT
           END-IF
      *
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-TOKNYUIN
                                   TO  WRK-CMB-CD
           PERFORM 4200-TOKNYUINLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    5           TO  SPA-GMN-W21-CUR
               MOVE    "0004"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W21-TOKNYUIN-G
               MOVE    WRK-CMB-ITM4
                                   TO  SPA-NAI-W21-TOKNYUIN
           END-IF
      *
      *
           .
      *
       4100-TOKNYUIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    収容人数
      *****************************************************************
       4100-NINZU-CHK-SEC          SECTION.
      *
      *
           .
      *
       4100-NINZU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額
      *****************************************************************
       4100-SAGAKU-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W21-SAGAKU-G
                                    =   SPACE )
               GO  TO  4100-SAGAKU-CHK-EXT
           END-IF
      *
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-SAGAKU
                                   TO  WRK-CMB-CD
           PERFORM 4200-SAGAKULST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    7           TO  SPA-GMN-W21-CUR
               MOVE    "0006"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W21-SAGAKU-G
           END-IF
      *
      *
           .
      *
       4100-SAGAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    性別特定
      *****************************************************************
       4100-SEX-CHK-SEC            SECTION.
      *
           IF    ( SPA-GMN-W21-SEX-G   =   SPACE )
               GO  TO  4100-SEX-CHK-EXT
           END-IF
      *
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-SEX TO  WRK-CMB-CD
           PERFORM 4200-SEXLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    8           TO  SPA-GMN-W21-CUR
               MOVE    "0007"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W21-SEX-G
           END-IF
      *
      *
           .
      *
       4100-SEX-CHK-EXT.
           EXIT.
      *****************************************************************
      *    内線番号
      *****************************************************************
       4100-NAITEL-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-W21-NAITEL  =   SPACE   )
               GO  TO  4100-NAITEL-CHK-EXT
           END-IF
      *
      *    全角半角混在チェック
           MOVE    SPA-GMN-W21-NAITEL  TO  WRK-KANACHK-INPUT
           PERFORM 800-ENRTRY-CHK-SEC
           MOVE    WRK-KANACHK-INPUT   TO  SPA-GMN-W21-NAITEL
           IF  ( FLG-ENTRYCHK      NOT =   1 )
               MOVE    9               TO  SPA-GMN-W21-CUR
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4100-NAITEL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科
      *****************************************************************
       4100-SRYKA-CHK-SEC   SECTION.
      *
           IF    ( SPA-GMN-W21-SRYKA-G     =   SPACE )
               GO  TO  4100-SRYKA-CHK-EXT
           END-IF
      *
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-SRYKA
                                   TO  WRK-CMB-CD
           PERFORM 4200-SRYKALST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    10          TO  SPA-GMN-W21-CUR
               MOVE    "0009"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM TO  SPA-GMN-W21-SRYKA-G
           END-IF
      *
      *
           .
      *
       4100-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料チェック処理
      *****************************************************************
       4100-KHNRYO-CHK-SEC             SECTION.
      *
           INITIALIZE                  SPA-NAI-W21-KHNSRYCD
                                       SPA-GMN-W21-KHNTEN
      *
           IF    ( SPA-GMN-W21-KHNRYO-G    =   SPACE )
               GO  TO  4100-KHNRYO-CHK-EXT
           END-IF
      *
      *    該当コードのデータが存在するかチェック
           MOVE    SPA-GMN-W21-KHNRYO
                                   TO  WRK-CMB-CD
           PERFORM 4200-KHNRYOLST-CHK-SEC
           IF    ( WRK-CMB-ITM   =   SPACE )
               MOVE    13          TO  SPA-GMN-W21-CUR
               MOVE    "0010"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM             TO  SPA-GMN-W21-KHNRYO-G
               MOVE    WRK-CMB-ITM2            TO  SPA-NAI-W21-KHNSRYCD
               MOVE    SPA-NAI-W21-KHNSRYCD    TO  WRK-TNSSRYCD
               MOVE    SPA-NAI-W21-YUKOSTYMD   TO  WRK-TNSYMD
               PERFORM 900-TENSU-KEY-SEL-SEC
               MOVE    TNS-TEN                 TO  SPA-GMN-W21-KHNTEN
           END-IF
      *
      *
           .
      *
       4100-KHNRYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟コード（病棟機能）チェック処理
      *****************************************************************
       4101-TBTUTYPE-CHK-SEC           SECTION.
      *
           IF    ( SPA-GMN-W21-TBTUTYPE-G   =   SPACE )
               MOVE    ZERO        TO  SPA-GMN-W21-TBTUCD
           ELSE
      *
      *        該当コードのデータが存在するかチェック
               MOVE    SPA-GMN-W21-TBTUTYPE
                                   TO  WRK-CMB-CD
               PERFORM 4200-TBTUTYPELST-CHK-SEC
               IF    ( WRK-CMB-ITM     =   SPACE )
                   MOVE    102         TO  SPA-GMN-W21-CUR
                   MOVE    "0019"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-CMB-ITM TO  SPA-GMN-W21-TBTUTYPE-G
               END-IF
      *
           END-IF
      *
           .
      *
       4101-TBTUTYPE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟コード（連番）チェック処理
      *****************************************************************
       4101-TBTUCD-CHK-SEC             SECTION.
      *
           IF    ( SPA-GMN-W21-TBTUCD  =   ZERO )
               CONTINUE
           ELSE
      *
               IF    ( SPA-GMN-W21-TBTUCD   >=  1 )
                AND  ( SPA-GMN-W21-TBTUCD   <=  50 )
                   CONTINUE
               ELSE
                   MOVE    103         TO  SPA-GMN-W21-CUR
                   MOVE    "0020"      TO  SPA-ERRCD
               END-IF
      *
           END-IF
      *
           .
      *
       4101-TBTUCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間開始日
      *****************************************************************
       4100-YUKOSTYMD-CHK-SEC          SECTION.
      *
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  WRK-YUKOSTYMD
      *
           INITIALIZE                  WRK-TOKCHK1-G
                                       WRK-TOKCHK2-G
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W21-TOKNYUIN-MAX )
               MOVE    SPA-GMN-W21-TOKNYUINL-KBNCD (IDX0)
                                   TO  WRK-TOKCHK1-KBNCD (IDX0)
           END-PERFORM
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD-MOT   =   SPACE )
               MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  SPA-NAI-W21-YUKOSTYMD-MOT
           END-IF
      *
           MOVE    SPACE           TO  SPA-NAI-W21-YUKOSTYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-W21-YUKOSTYMD   =   SPACE )
               MOVE    SPA-GMN-W21-KJNYMD  (1 : 6)
                                       TO  SPA-GMN-W21-YUKOSTYMD (1 : 6)
               MOVE    ". 1"           TO  SPA-GMN-W21-YUKOSTYMD (7 : 3)
               MOVE    SPA-NAI-W21-KJNYMD  (1 : 6)
                                       TO  SPA-NAI-W21-YUKOSTYMD (1 : 6)
               MOVE    "01"            TO  SPA-NAI-W21-YUKOSTYMD (7 : 2)
      *
      *        システム管理情報再設定
               PERFORM 4100-SYSKANRI-RESET-SEC
           ELSE
      *
               MOVE    SPA-GMN-W21-YUKOSTYMD
                                   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               IF  ( SPA-ERRCD         =   SPACE )
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-W21-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-W21-YUKOSTYMD
               ELSE
                   MOVE    WRK-YUKOSTYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-CHK-SEC
                   MOVE    WRK-HENYMDG
                                       TO  SPA-GMN-W21-YUKOSTYMD
                   MOVE    WRK-SYMD    TO  SPA-NAI-W21-YUKOSTYMD
                   MOVE    "0011"      TO  SPA-ERRCD
                   MOVE    11          TO  SPA-GMN-W21-CUR
               END-IF
      *
               IF    ( SPA-NAI-W21-YUKOSTYMD   <   "20020401" )
                   MOVE    "0100"      TO  SPA-ERRCD
                   MOVE    "H14. 4. 1" TO  SPA-GMN-W21-YUKOSTYMD
                   MOVE    "20020401"  TO  SPA-NAI-W21-YUKOSTYMD
                   MOVE    11          TO  SPA-GMN-W21-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD
                               NOT =   SPA-NAI-W21-YUKOSTYMD-MOT )
      *        システム管理情報再設定
               PERFORM 4100-SYSKANRI-RESET-SEC
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   SPA-GMN-W21-TOKNYUIN-MAX )
               MOVE    SPA-GMN-W21-TOKNYUINL-KBNCD (IDX0)
                                   TO  WRK-TOKCHK2-KBNCD (IDX0)
           END-PERFORM
      *
           EVALUATE    TRUE
           WHEN   ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-H180401 )
            AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-H180401 )
           WHEN  ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-H220401 )
            AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-H220401 )
           WHEN  ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-H240401 )
            AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-H240401 )
           WHEN  ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-R020401 )
            AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-R020401 )
                   INITIALIZE                  SPA-NAI-W21-KHNSRYCD
                                               SPA-GMN-W21-KHNTEN
                                               SPA-GMN-W21-KHNRYO-G
      *
                   PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           END-EVALUATE
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-H180701 )
               IF    ( SPA-NAI-W21-BTUSBT  =   "04" OR "09" )
                       INITIALIZE              SPA-NAI-W21-KHNSRYCD
                                               SPA-GMN-W21-KHNTEN
                                               SPA-GMN-W21-KHNRYO-G
      *
                       PERFORM 4201-KSN-AREA-CLEAR-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD  >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-BTUSBT      =   "04" OR "09" )
                   CONTINUE
            ELSE
                       INITIALIZE              SPA-NAI-W21-KHNSRYCD
                                               SPA-GMN-W21-KHNTEN
                                               SPA-GMN-W21-KHNRYO-G
           END-IF
      *
           IF    ( WRK-TOKCHK1-G            NOT=   WRK-TOKCHK2-G )
               MOVE    SPA-NAI-W21-TOKNYUIN TO WRK-CMB-CD
               PERFORM 4201-TOKNYUINLST-CHK-SEC
               MOVE    WRK-CMB-ITM          TO  SPA-GMN-W21-TOKNYUIN-G

           END-IF
      *
           PERFORM 4200-BRM-SEL-SEC
           IF    ( FLG-INPUT-AREA  =    ZERO )
               EVALUATE    TRUE
               WHEN  ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-H220401 )
                AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-H220401 )
               WHEN  ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-H240401 )
                AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-H240401 )
               WHEN  ( SPA-NAI-W21-YUKOSTYMD      >=   CONST-R020401 )
                AND  ( SPA-NAI-W21-YUKOSTYMD-MOT   <   CONST-R020401 )
                   PERFORM 900-SYSKANRI-KEY16-SEL-SEC
                   PERFORM 41001-INPUT-AREA-EDIT-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  SPA-NAI-W21-YUKOSTYMD-MOT
      *
           .
      *
       4100-YUKOSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    有効期間終了日
      *****************************************************************
       4100-YUKOEDYMD-CHK-SEC           SECTION.
      *
           MOVE    SPA-NAI-W21-YUKOEDYMD
                                   TO  WRK-YUKOEDYMD
           MOVE    SPACE           TO  SPA-NAI-W21-YUKOEDYMD
      *
      *    日付妥当性チェック＆編集処理
           IF    ( SPA-GMN-W21-YUKOEDYMD   =   SPACE )
               MOVE    "99999999"  TO  SPA-GMN-W21-YUKOEDYMD
                                       SPA-NAI-W21-YUKOEDYMD
               GO  TO  4100-YUKOEDYMD-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W21-YUKOEDYMD
                                   TO  WRK-YMD
           PERFORM 800-HIZUKE-CHK-SEC
           IF  ( SPA-ERRCD         =   SPACE )
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-W21-YUKOEDYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W21-YUKOEDYMD
           ELSE
               MOVE    WRK-YUKOEDYMD
                                   TO  WRK-YMD
               PERFORM 800-HIZUKE-CHK-SEC
               MOVE    WRK-HENYMDG
                                   TO  SPA-GMN-W21-YUKOEDYMD
               MOVE    WRK-SYMD    TO  SPA-NAI-W21-YUKOEDYMD
               MOVE    12          TO  SPA-GMN-W21-CUR
               MOVE    "0012"      TO  SPA-ERRCD
           END-IF
      *
           .
      *
       4100-YUKOEDYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択番号２
      *****************************************************************
       4100-SELNUM2-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-W21-CUR
      *
           MOVE    W21-SELNUM2     TO  SPA-GMN-W21-SELNUM2
      *
           IF  (   SPA-GMN-W21-SELNUM2 =   ZERO )
               MOVE    3           TO  SPA-GMN-W21-CUR
               GO  TO  4100-SELNUM2-CHK-EXT
           END-IF
      *
      *    範囲内の番号が選択されていること
           IF    ( SPA-GMN-W21-SELNUM2 >   SPA-GMN-W21-RIREKILST-MAX )
               MOVE    100         TO  SPA-GMN-W21-CUR
               MOVE    "1008"      TO  SPA-ERRCD
               GO  TO  4100-SELNUM2-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-W21-SELNUM2 TO  SPA-GMN-W21-RIREKILST-SEL
      *
      *    入力項目領域編集処理
           MOVE    SPA-NAI-W21-RIREKILST-BRMNUM
                                   TO  WRK-BRMNUM
           MOVE    SPA-NAI-W21-TRYUKOSTYMD (SPA-GMN-W21-SELNUM2)
                                   TO  WRK-KJNYMD
      *
      *    入力領域初期化
           PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
      *
           MOVE    1       TO  SPA-NAI-W21-INPUT-MODE
           MOVE    SPA-NAI-W21-RIREKILST-SELNUM
                                   TO  WRK-SELNUM
           PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
           MOVE    W21-SELNUM2     TO  SPA-GMN-W21-SELNUM2
      *
           MOVE    ZERO            TO  SPA-GMN-W21-CUR
      *
           .
      *
       4100-SELNUM2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室リスト
      *****************************************************************
       4100-BRMLST-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   200 )
      *
               IF    ( W21-BRMLST-SEL  (IDX2)  =   "T" )
      *
      *            入力項目領域編集処理
                   MOVE    SPA-GMN-W21-TBRMNUM2 (IDX2)
                                           TO  WRK-BRMNUM
                   MOVE    SPA-NAI-W21-KJNYMD
                                           TO  WRK-KJNYMD
      *
      *            入力領域初期化
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
      *            履歴領域初期化
                   PERFORM 4201-RIREKI-AREA-CLEAR-SEC
      *
      *
      *            入力データ編集処理
                   MOVE    1               TO  SPA-NAI-W21-INPUT-MODE
                   MOVE    IDX2            TO  WRK-SELNUM
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
                   MOVE    IDX2    TO  SPA-GMN-W21-BRMLST-SEL
      *
      *            履歴データ編集処理
                   PERFORM 4100-RIREKI-AREA-EDIT-SEC
      *
                   MOVE    200     TO  IDX2
               END-IF
           END-PERFORM
      *
           MOVE    3               TO  SPA-GMN-W21-CUR
      *
           .
       4100-BRMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    履歴リスト
      *****************************************************************
       4100-RIREKILST-CHK-SEC      SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL     ( IDX2    >   20 )
               IF    ( W21-RIREKILST-SEL (IDX2)
                                       =   "T" )
      *
      *            入力項目領域編集処理
                   MOVE    SPA-NAI-W21-RIREKILST-BRMNUM
                                       TO  WRK-BRMNUM
                   MOVE    SPA-NAI-W21-TRYUKOSTYMD (IDX2)
                                       TO  WRK-KJNYMD
      *
      *            入力領域初期化
                   PERFORM 4201-INPUT-AREA-CLEAR-SEC
      *
      *
                   MOVE    1       TO  SPA-NAI-W21-INPUT-MODE
                   MOVE    SPA-NAI-W21-RIREKILST-SELNUM
                                       TO  WRK-SELNUM
                   PERFORM 4100-INPUT-AREA-EDIT-SEC
      *
                   MOVE    IDX2        TO  SPA-GMN-W21-RIREKILST-SEL
                                           SPA-GMN-W21-SELNUM2
      *
                   MOVE    20          TO  IDX2
               END-IF
           END-PERFORM
      *
           MOVE    3                   TO  SPA-GMN-W21-CUR
      *
           .
       4100-RIREKILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ヘッダ情報初期化処理
      *****************************************************************
       4201-HEAD-AREA-CLEAR-SEC        SECTION.
      *
           INITIALIZE  SPA-GMN-W21-HEAD-AREA
                       SPA-NAI-W21-HEAD-AREA
      *
           .
      *
       4201-HEAD-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入力情報領域初期化処理
      *****************************************************************
       4201-INPUT-AREA-CLEAR-SEC       SECTION.
      *
           INITIALIZE  SPA-GMN-W21-INPUT-AREA
                       SPA-NAI-W21-INPUT-AREA
      *
      *    有効期間の開始日と終了日を設定
           MOVE    SPA-GMN-W21-KJNYMD  (1 : 6)
                                   TO  SPA-GMN-W21-YUKOSTYMD (1 : 6)
           MOVE    ". 1"           TO  SPA-GMN-W21-YUKOSTYMD (7 : 3)
           MOVE    SPA-NAI-W21-KJNYMD  (1 : 6)
                                   TO  SPA-NAI-W21-YUKOSTYMD (1 : 6)
           MOVE    "01"            TO  SPA-NAI-W21-YUKOSTYMD (7 : 2)
      *
           MOVE    "99999999"      TO  SPA-GMN-W21-YUKOEDYMD
                                       SPA-NAI-W21-YUKOEDYMD
      *
      *    システム管理情報再設定処理
           PERFORM 4100-SYSKANRI-RESET-SEC
      *
           PERFORM 4201-KSN-AREA-CLEAR-SEC
      *
           MOVE    2               TO  SPA-GMN-W21-CUR
      *
           .
      *
       4201-INPUT-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    入院料加算領域初期化処理
      *****************************************************************
       4201-KSN-AREA-CLEAR-SEC         SECTION.
      *
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                      >    ZERO )
                   MOVE   SPA-GMN-W21-KSNLST (IDX-KSN 1)
                                       TO  SPA-GMN-W21-KSNCD-G(IDX-KSN)
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4201-KSN-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *   履歴情報領域初期化処理
      *****************************************************************
       4201-RIREKI-AREA-CLEAR-SEC      SECTION.
      *
           INITIALIZE  SPA-GMN-W21-RIREKILST
                       SPA-NAI-W21-RIREKILST
           MOVE    ZERO            TO  SPA-GMN-W21-BRMLST-SEL
      *
           .
      *
       4201-RIREKI-AREA-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    システム管理情報再設定処理
      *****************************************************************
       4100-SYSKANRI-RESET-SEC         SECTION.
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           MOVE    SYS-1001-HOSPSBT
                                   TO  SPA-NAI-W21-HOSPSBT
      *
      *
      *    コンボ初期処理
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  WRK-KJN-YUKSTYMD
           PERFORM 3101-CMB-SYOKI-SEC
      *
           INITIALIZE  SPA-NAI-W21-KSNINF-G
      *
           PERFORM VARYING IDX-KSN    FROM 1  BY  1
                   UNTIL ( IDX-KSN    >    CONST-KSN-MAX )
      *
               MOVE    SPACE               TO  SPA-GMN-W21-KSN-LBL
                                                           (IDX-KSN)
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD
                                       >=  CONST-R020401 )
               MOVE    "早期栄養介入管理加算（特定集中）"
                                       TO  SPA-GMN-W21-SOKIEIYO-LBL
               MOVE    CONST-R02-A301C5
                                       TO  SPA-NAI-W21-SOKIEIYO-CD
           END-IF
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD
                                       >=  CONST-H240401 )
               MOVE    "診療所療養病床療養環境改善加算"
                                       TO  SPA-GMN-W21-SNRRYOKAI-LBL
               MOVE    CONST-H2404-SNRRYOKAI
                                       TO  SPA-NAI-W21-SNRRYOKAI-CD
               MOVE    "療養病棟療養環境改善加算"
                                       TO  SPA-GMN-W21-BTURYOKAI-LBL
               MOVE    CONST-H2404-BTURYOKAI1
                                       TO  SPA-NAI-W21-BTURYOKAI-CD
           END-IF
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD
                                       >=  CONST-H220401 )
               CONTINUE
           ELSE
               MOVE    "新生児入院医療管理加算"
                                       TO  SPA-GMN-W21-SINSEIJI-LBL
               MOVE    CONST-SINSEIJI  TO  SPA-NAI-W21-SINSEIJI-CD
           END-IF
      *
               MOVE    "診療所療養病床療養環境加算"
                                       TO  SPA-GMN-W21-SNRRYO-LBL
               MOVE    CONST-SNR-RYOYO(1)
                                       TO  SPA-NAI-W21-SNRRYO-CD
      *
               MOVE    "放射線治療病室管理加算"
                                       TO  SPA-GMN-W21-HOSYASEN-LBL
               MOVE    CONST-HOSYASEN  TO  SPA-NAI-W21-HOSYASEN-CD
      *
               MOVE    "療養環境加算"  TO  SPA-GMN-W21-RYO-LBL
               MOVE    CONST-RYOYO     TO  SPA-NAI-W21-RYO-CD
      *
               MOVE    "療養病棟療養環境加算"
                                       TO  SPA-GMN-W21-BTURYO-LBL
               MOVE    CONST-BTU-RYOYO (1)
                                       TO  SPA-NAI-W21-BTURYO-CD
      *
               MOVE    "重症者等療養環境特別加算"
                                       TO  SPA-GMN-W21-JYUTOK-LBL
               MOVE    CONST-JYUTOK1   TO  SPA-NAI-W21-JYUTOK-CD
      *
               MOVE    "無菌治療室管理加算"
                                       TO  SPA-GMN-W21-MUKIN-LBL
               MOVE    CONST-MUKIN     TO  SPA-NAI-W21-MUKIN-CD
      *
           MOVE    ZERO                    TO  IDX-GKSN
           PERFORM VARYING IDX-KSN     FROM 1  BY  1
                   UNTIL ( IDX-KSN     >   CONST-KSN-MAX )
      *
               IF    ( SPA-NAI-W21-KSNINF-CD (IDX-KSN)
                                       NOT =   SPACE )
                   MOVE    SPA-NAI-W21-KSNINF-CD (IDX-KSN)
                                       TO  WRK-TNSSRYCD
                   MOVE    SPA-NAI-W21-YUKOSTYMD
                                       TO  WRK-TNSYMD
                   PERFORM 900-TENSU-KEY-SEL-SEC
                   IF    ( FLG-TENSU           =   ZERO )
                       IF    ( TNS-HOSPSRYKBN  =   ZERO )
                        OR   ( TNS-HOSPSRYKBN  =   SPA-NAI-W21-HOSPSBT )
                           COMPUTE    IDX-GKSN =   IDX-GKSN +   1
                           MOVE   IDX-GKSN  TO  SPA-NAI-W21-KSNINF-IDX
                                                           (IDX-KSN)
                       END-IF
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-SYSKANRI-RESET-EXT.
           EXIT.
      *****************************************************************
      *    病室番号入力チェック
      *****************************************************************
       4900-BRMNUM-INPUT-CHK-SEC      SECTION.
      *
      *    病室番号が入力されていること
           IF    ( SPA-GMN-W21-BRMNUM     NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1101"          TO  SPA-ERRCD
               MOVE   3                TO  SPA-GMN-W21-CUR
           END-IF
      *
           .
       4900-BRMNUM-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    共通関連チェック
      *****************************************************************
       4900-KANRENCHK-SEC              SECTION.
      *
      *    病室番号が入力されていること
           PERFORM 4900-BRMNUM-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効期間開始日が入力されていること
           IF    ( SPA-NAI-W21-YUKOSTYMD NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1102"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-W21-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効期間終了日が入力されていること
           IF    ( SPA-NAI-W21-YUKOEDYMD NOT =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1103"      TO  SPA-ERRCD
               MOVE    12          TO  SPA-GMN-W21-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    有効期間開始日≦有効期間終了日となっていること
           IF    ( SPA-NAI-W21-YUKOSTYMD <=  SPA-NAI-W21-YUKOEDYMD )
               CONTINUE
           ELSE
               MOVE    "1004"      TO  SPA-ERRCD
               MOVE    11          TO  SPA-GMN-W21-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
      *    収容人数が１以上であること
           IF    ( SPA-GMN-W21-NINZU   >   0 )
               CONTINUE
           ELSE
               MOVE    "1009"      TO  SPA-ERRCD
               MOVE    6           TO  SPA-GMN-W21-CUR
               GO  TO  4900-KANRENCHK-EXT
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-W21-TBTUTYPE    NOT =   SPACE )
                OR   ( SPA-GMN-W21-TBTUCD      NOT =   ZERO )
                   IF    ( SPA-GMN-W21-TBTUTYPE    =   SPACE )
                       MOVE    "1109"  TO  SPA-ERRCD
                       MOVE    102     TO  SPA-GMN-W21-CUR
                   END-IF
                   IF    ( SPA-GMN-W21-TBTUCD      =   ZERO )
                       MOVE    "1109"  TO  SPA-ERRCD
                       MOVE    103     TO  SPA-GMN-W21-CUR
                   END-IF
               END-IF
           END-IF
      *
           .
      *
       4900-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       4901-SSTKIJUN-CHK-SEC           SECTION.
      *
           MOVE    SPACE           TO  WRK-KSNMEI
      *
      *    施設基準検索処理
           PERFORM 4100-SYS-1006-SEL-SEC
      *
           IF    ( SPA-ERRCD               =   SPACE )
               IF    ( SPA-NAI-W21-YUKOSTYMD >=  CONST-H240401 )
                   PERFORM 49012-SSTKIJUN-CHK-SEC
               ELSE
                   PERFORM 49011-SSTKIJUN-CHK-SEC
               END-IF
           END-IF
      *
           .
       4901-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理 (〜H24.3)
      *****************************************************************
       49011-SSTKIJUN-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               IF    ( SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                   >   ZERO )
      *
                   MOVE    SPACE   TO  WRK-KSNCD
      *
                   EVALUATE    SPA-NAI-W21-KSNINF-CD (IDX-KSN)
                   WHEN    CONST-SINSEIJI
                       IF    ( SPA-GMN-W21-SINSEIJI    =   "2" )
                           MOVE    CONST-SINSEIJI  TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-SNR-RYOYO(1)
                       EVALUATE    SPA-GMN-W21-SNRRYO
                       WHEN   "2"
                           MOVE    CONST-SNR-RYOYO(1)  TO  WRK-KSNCD
                       WHEN   "3"
                           MOVE    CONST-SNR-RYOYO(2)  TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-HOSYASEN
                       IF    ( SPA-GMN-W21-HOSYASEN  =   "2" )
                           MOVE    CONST-HOSYASEN      TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-RYOYO
                       IF    ( SPA-GMN-W21-RYO  =   "2" )
                           MOVE    CONST-RYOYO         TO  WRK-KSNCD
                       END-IF
                   WHEN    CONST-BTU-RYOYO (1)
                       EVALUATE    TRUE
                       WHEN  ( SPA-NAI-W21-YUKOSTYMD
                                               >=  CONST-H180401 )
                           EVALUATE    SPA-GMN-W21-BTURYO
                           WHEN    "2"
                               MOVE    CONST-BTU-RYOYO-H18
                                                       TO  WRK-KSNCD
                           WHEN    "3"
                               MOVE    CONST-BTU-RYOYO (1)
                                                       TO  WRK-KSNCD
                           WHEN    "4"
                               MOVE    CONST-BTU-RYOYO (2)
                                                       TO  WRK-KSNCD
                           WHEN    "5"
                               MOVE    CONST-BTU-RYOYO (3)
                                                       TO  WRK-KSNCD
                           END-EVALUATE
                       WHEN    OTHER
                           EVALUATE    SPA-GMN-W21-BTURYO
                           WHEN    "2"
                               MOVE    CONST-BTU-RYOYO (1)
                                                       TO  WRK-KSNCD
                           WHEN    "3"
                               MOVE    CONST-BTU-RYOYO (2)
                                                       TO  WRK-KSNCD
                           WHEN    "4"
                               MOVE    CONST-BTU-RYOYO (3)
                                                       TO  WRK-KSNCD
                           END-EVALUATE
                       END-EVALUATE
                   WHEN    CONST-JYUTOK1
                       EVALUATE    SPA-GMN-W21-JYUTOK
                       WHEN    "2"
                           MOVE    CONST-JYUTOK1       TO  WRK-KSNCD
                       WHEN    "3"
                           MOVE    CONST-JYUTOK2       TO  WRK-KSNCD
                       END-EVALUATE
                   WHEN    CONST-MUKIN
                       IF    ( SPA-GMN-W21-MUKIN       =   "2" )
                           MOVE    CONST-MUKIN         TO  WRK-KSNCD
                       END-IF
                   END-EVALUATE
      *
                   IF    ( WRK-KSNCD       NOT =   SPACE )
                       PERFORM 41011-SSTKIJUN-CHK-SEC
      *
                       IF    ( SPA-ERRCD           =   SPACE )
                           IF    ( FLG-KSNERR      =   1 )
                               MOVE    "1011"  TO  SPA-ERRCD
                               COMPUTE SPA-GMN-W21-CUR
                                   =   SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                   +   13
                               MOVE    SPA-GMN-W21-KSN-LBL (IDX-KSN)
                                               TO  WRK-KSNMEI
                           END-IF
                       END-IF
      *
                       IF    ( SPA-ERRCD           =   SPACE )
                           IF    ( FLG-SSTKIJUNERR     =   1 )
                               MOVE    "1010"          TO  SPA-ERRCD
                               COMPUTE SPA-GMN-W21-CUR
                                   =   SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                   +   13
                               MOVE    SPA-GMN-W21-KSN-LBL (IDX-KSN)
                                               TO  WRK-KSNMEI
                           END-IF
                       END-IF
                   END-IF
      *
               END-IF
      *
           END-PERFORM
      *
           .
       49011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理 (H24.4〜)
      *****************************************************************
       49012-SSTKIJUN-CHK-SEC           SECTION.
      *
           PERFORM 4200-BRM-KSN-EDIT-SEC
      *
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   TKSN-MAX )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               MOVE    TKSN-SRYCD (IDX-KSN)    TO  WRK-KSNCD
      *
               PERFORM 41011-SSTKIJUN-CHK-SEC
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-KSNERR      =   1 )
                       MOVE    "1011"  TO  SPA-ERRCD
                   COMPUTE SPA-GMN-W21-CUR
                       =   TKSN-IDX (IDX-KSN)
                       +   13
                   MOVE    TKSN-LBL (IDX-KSN)
                                           TO  WRK-KSNMEI
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( FLG-SSTKIJUNERR     =   1 )
                       MOVE    "1010"          TO  SPA-ERRCD
                       COMPUTE SPA-GMN-W21-CUR
                           =   TKSN-IDX (IDX-KSN)
                           +   13
                       MOVE    TKSN-LBL (IDX-KSN)
                                           TO  WRK-KSNMEI
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       49012-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準チェック処理
      *****************************************************************
       41011-SSTKIJUN-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-SSTKJNCD-G
      *
           MOVE    ZERO            TO  FLG-SSTKIJUNERR
                                       FLG-KSNERR
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    "5001"          TO  SYS-KANRICD
           MOVE    SPA-GMN-W21-BTUNUM
                                   TO  SYS-KBNCD
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-5001-REC
      *
      *    INITIALIZE  SNYUKSNCHK-LNK
      *    MOVE    SYS-5001-BTU-SBT
      *                            TO  SNYUKSNCHK-BTUSBT
      *    MOVE    SYS-5001-BTU-KHNSRYCD
      *                            TO  SNYUKSNCHK-KHN-SRYCD
      *    MOVE    SYS-5001-BTU-TOKTEI-NYUIN
      *                            TO  SNYUKSNCHK-BTU-TOKKBN
      *
      *    IF    ( SPA-GMN-W21-TOKNYUIN    NOT =   SPACE )
      *        MOVE    SPA-NAI-W21-TOKNYUIN
      *                            TO  SNYUKSNCHK-BRM-TOKKBN
      *    END-IF
      *    IF    ( SYS-5001-BTU-SBT    =   "08" OR "09" )
      *        MOVE    SYS-5001-CRYOYOKBN
      *                            TO  SNYUKSNCHK-RYOYOKBN
      *    ELSE
      *        MOVE    SYS-5001-HRYOYOKBN
      *                            TO  SNYUKSNCHK-RYOYOKBN
      *    END-IF
      *    MOVE    SPA-NAI-W21-YUKOSTYMD
      *                            TO  SNYUKSNCHK-SRYYMD
      *    MOVE    WRK-KSNCD       TO  SNYUKSNCHK-KSN-SRYCD
      *    CALL    "ORCSNYUKSNCHK"         USING
      *                                    SNYUKSNCHK-LNK
      *                                    SPA-AREA
      *    IF    ( SNYUKSNCHK-ERR      NOT =   ZERO )
      *            MOVE    1   TO      FLG-KSNERR
      *    END-IF
      *
           MOVE    WRK-KSNCD                   TO  WRK-TNSSRYCD
           MOVE    SPA-NAI-W21-YUKOSTYMD       TO  WRK-TNSYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           MOVE    ZERO            TO  FLG-HIT
                                       CNT-SST
           PERFORM VARYING IDX5    FROM    1   BY  1
                   UNTIL ( IDX5    >   10 )
                    OR   ( FLG-HIT     =   1 )
      *
               IF    ( TNS-SSTKIJUNCD (IDX5)   >   ZERO )
                   COMPUTE CNT-SST =   CNT-SST +   1
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  WRK-SSTKJNCD9(CNT-SST)
                   MOVE    TNS-SSTKIJUNCD (IDX5)
                                   TO  IDX-SST
                   IF    ( SSTKJN-CD (IDX-SST) =   "1" )
                       MOVE    1   TO  FLG-HIT
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( CNT-SST     >   ZERO )
               IF    ( FLG-HIT =   ZERO )
                   MOVE        1   TO  FLG-SSTKIJUNERR
               END-IF
           END-IF
      *
           .
       41011-SSTKIJUN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    施設基準検索処理
      *****************************************************************
       4100-SYS-1006-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  SSTKJN-AREA
      *
      *    施設基準情報検索
           INITIALIZE  SYSKANRI-REC
           MOVE    "1006"          TO  SYS-KANRICD
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                   TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYSKANRI    =   1 )
      *
               MOVE    SYSKANRI-REC    TO  SYS-1006-REC
      *
               EVALUATE    TRUE
               WHEN  ( SYS-1006-KBNCD (2:)         =   SPACE )
                AND  ( SYS-1006-KBNCD  >=  "1" )
                AND  ( SYS-1006-KBNCD  <=  "9" )
                   MOVE    SYS-1006-KBNCD  TO  WRK-KBN1-G
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL-OCC
                                                     (WRK-KBN1)
               WHEN  ( SYS-1006-KBNCD (2:1)    NOT =   SPACE )
                AND  ( SYS-1006-KBNCD  >=  "10" )
                AND  ( SYS-1006-KBNCD  <=  "19" )
                   MOVE    SYS-1006-KBNCD  TO  WRK-KBN2-G
                   MOVE    SYS-1006-TBL    TO  SSTKJN-VAL-OCC
                                                     (WRK-KBN2)
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       4100-SYS-1006-SEL-EXT.
           EXIT.
      *****************************************************************
      *       病室番号検索処理
      *****************************************************************
       4100-BRM-NUM-LSTSEL-SEC         SECTION.
      *
           MOVE    1               TO  FLG-BRM-NUM
           MOVE    ZERO            TO  IDX-BRMLST
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( FLG-BRM-NUM =   ZERO )
                OR       ( IDX1    >   SPA-GMN-W21-BRMLST-MAX )
      *
               IF    ( SPA-GMN-W21-BRMNUM
                                   =   SPA-GMN-W21-TBRMNUM2 (IDX1) )
                   MOVE    ZERO    TO  FLG-BRM-NUM
                   MOVE    IDX1    TO  IDX-BRMLST
               END-IF
      *
           END-PERFORM
      *
           .
      *
       4100-BRM-NUM-LSTSEL-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN                  SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-WIDCD       NOT =   SPACE )
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W21    "           TO  MCP-WINDOW.
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
           INITIALIZE                  W21
      *
           IF    ( SPA-W20-W21-MOTOPG  =   SPACE )
               MOVE    "病棟"      TO  W21-B09
           END-IF
      *
      *    病棟名称コンボ
           MOVE    SPA-GMN-W21-BTUNUM-G    TO  W21-BTU-NAME
           MOVE    SPA-GMN-W21-BTUNUM-MAX  TO  W21-BTU-NAME-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BTU-NAME-CNT )
                MOVE   SPA-GMN-W21-BTUNUMLST (IDX1)
                                   TO  W21-BTU-NAME-ITM (IDX1)
           END-PERFORM
      *
      *    病棟入院基本料
           MOVE    SPA-GMN-W21-BTUKHNRYO   TO  W21-BTU-KHNRYO
      *
      *    基準日
           MOVE    SPA-GMN-W21-KJNYMD      TO  W21-KJNYMD
      *
      *    選択番号１
           MOVE    SPA-GMN-W21-SELNUM1     TO  W21-SELNUM1
      *
           IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
              CONTINUE
           ELSE
              PERFORM 5001-GMNHEN-SEC
           END-IF
      *
      *    入院料加算
           PERFORM VARYING IDX-KSN FROM    1   BY  1
                   UNTIL ( IDX-KSN >   CONST-KSN-MAX )
      *
               MOVE    SPA-NAI-W21-KSNINF-IDX (IDX-KSN)
                                   TO  IDX-GKSN
      *
               IF    ( IDX-GKSN    >   ZERO )
                   MOVE    SPA-GMN-W21-KSN-LBL    (IDX-KSN)
                                   TO  W21-LBLKSN (IDX-GKSN)
                   MOVE    SPA-GMN-W21-KSNCD-G    (IDX-KSN)
                                   TO  W21-KSN (IDX-GKSN)
                   MOVE    SPA-GMN-W21-KSNLST-MAX (IDX-KSN)
                                   TO  W21-KSN-CNT(IDX-GKSN)
                                       WRK-MAX
                   PERFORM VARYING IDX1    FROM    1   BY  1
                           UNTIL ( IDX1    >   WRK-MAX )
                       MOVE   SPA-GMN-W21-KSNLST (IDX-KSN IDX1)
                                   TO  W21-KSN-ITM (IDX-GKSN IDX1)
                   END-PERFORM
               END-IF
      *
           END-PERFORM
      *
      *    病室リスト
           MOVE    SPACE                   TO  W21-BRMLST-TBL
      *
           MOVE    SPA-GMN-W21-BRMLST-MAX  TO  W21-BRMLST-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BRMLST-CNT )
      *
               MOVE    SPA-GMN-W21-TSELNUM  (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9
                                       TO  W21-TSELNUM  (IDX1)
               IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
                   IF    ( SPA-NAI-W21-SELNUM-FLG (IDX1)
                                       =   1 )
                       MOVE    "*"     TO  W21-TSELNUM  (IDX1)(1:1)
                   END-IF
               END-IF
      *
               STRING  "  "
                       SPA-GMN-W21-TBRMNUM (IDX1)
                       DELIMITED   BY  SIZE
               INTO    W21-TBRM-NUM (IDX1)
               END-STRING
      *
               MOVE    SPA-GMN-W21-TSBT (IDX1)
                                       TO  W21-TBRM-SBT (IDX1)
               MOVE    SPA-GMN-W21-TTOKNYUIN (IDX1)
                                       TO  W21-TBRM-TOKUTEINYUIN (IDX1)
               MOVE    SPA-GMN-W21-TNINZU (IDX1)
                                       TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9
                                       TO  W21-TBRM-NINZU (IDX1)
               MOVE    SPA-GMN-W21-TSAGAKU (IDX1)
                                       TO  WRK-ZZZZZZZ9
               MOVE    WRK-ZZZZZZZ9
                                       TO  W21-TBRM-SAGAKU (IDX1)
               MOVE    SPA-GMN-W21-TSEX (IDX1)
                                       TO  W21-TBRM-SEX (IDX1)
               MOVE    SPA-GMN-W21-TNAITEL (IDX1)
                                       TO  W21-TBRM-NAITEL (IDX1)
               MOVE    SPA-GMN-W21-TSRYKA  (IDX1)
                                       TO  W21-TBRM-KA (IDX1)
               MOVE    SPA-GMN-W21-TKHNRYO  (IDX1)
                                       TO  W21-TKHNRYO (IDX1)
               MOVE    SPA-GMN-W21-TYUKOSTYMD (IDX1)
                                       TO  W21-TYUKOSTYMD (IDX1)
               MOVE    SPA-GMN-W21-TYUKOEDYMD (IDX1)
                                       TO  W21-TYUKOEDYMD (IDX1)
      *
               IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
                   CONTINUE
               ELSE
                   IF    ( SPA-GMN-W21-BRMLST-SEL  =   IDX1 )
                       MOVE    "T"     TO  W21-BRMLST-SEL (IDX1)
                   ELSE
                       MOVE    "F"     TO  W21-BRMLST-SEL (IDX1)
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    履歴リスト
           INITIALIZE                      W21-RIREKILST-TBL
           MOVE    SPA-GMN-W21-RIREKILST-MAX
                                       TO  W21-RIREKILST-CNT
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   SPA-GMN-W21-RIREKILST-MAX )
      *
               MOVE    SPA-GMN-W21-TRSELNUM (IDX1)
                                       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  W21-TRSELNUM (IDX1)
      *
               STRING  SPA-GMN-W21-TRYUKOSTYMD (IDX1)
                       " 〜 "
                       SPA-GMN-W21-TRYUKOEDYMD (IDX1)
                       DELIMITED   BY  SIZE
               INTO    W21-TRIREKI (IDX1)
               END-STRING
      *
               IF    ( IDX1    =   SPA-GMN-W21-RIREKILST-SEL )
                   MOVE    "T"         TO  W21-RIREKILST-SEL (IDX1)
               ELSE
                   MOVE    "F"         TO  W21-RIREKILST-SEL (IDX1)
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD  >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-BTUSBT      =   "04" OR "09" )
               MOVE    "入院基本料"    TO  W21-LBLKHNRYO1
               MOVE    "点"            TO  W21-LBLKHNRYO2
           END-IF
      *
           IF    ( SPA-NAI-W21-KJNYMD     >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-BTUSBT      =   "04" OR "09" )
               MOVE    "入院基本料"    TO  W21-TKHNRYO-TITLE
           END-IF
      *
           MOVE    "black"                 TO  W21-LBLINFO-STYLE
           IF    ( SPA-NAI-W21-RYO-IDX >   ZERO )
               IF    ( SPA-NAI-W21-YUKOSTYMD   <   CONST-H180401 )
                AND  ( SPA-NAI-W21-YUKOEDYMD   >=  CONST-H180401 )
                   MOVE    "red"               TO  W21-LBLINFO-STYLE
                   STRING "※療養病棟環境加算１、２、３は"
                                               DELIMITED   BY  SIZE
                          "平成１８年４月以降"
                                               DELIMITED   BY  SIZE
                          "それぞれ２、３、４とみなします。"
                                               DELIMITED   BY  SIZE
                   INTO   W21-LBLINFO
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    "black"                 TO  W21-LBLMODE-STYLE
           IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
               MOVE    "blue"              TO  W21-LBLMODE-STYLE
               MOVE    "<入院料加算一括変更＞"
                                           TO  W21-LBLMODE
           END-IF
      *
      *    項目状態編集
           PERFORM 500-GMNHEN-STATE-EDIT-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       5001-GMNHEN-SEC                 SECTION.
      *
      *    病室番号
           MOVE    SPA-GMN-W21-BRMNUM      TO  W21-BRM-NUM
      *
      *    病室種別コンボ
           MOVE    SPA-GMN-W21-SBT-G       TO  W21-BRM-SBT
           MOVE    SPA-GMN-W21-SBT-MAX     TO  W21-BRM-SBT-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BRM-SBT-CNT )
                MOVE   SPA-GMN-W21-SBTLST (IDX1)
                                   TO  W21-BRM-SBT-ITM (IDX1)
           END-PERFORM
      *
      *    特定入院料コンボ
           MOVE    SPA-GMN-W21-TOKNYUIN-G
                                       TO  W21-BRM-TOKUTEINYUIN
           MOVE    SPA-GMN-W21-TOKNYUIN-MAX
                                       TO  W21-BRM-TOKUTEINYUIN-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BRM-TOKUTEINYUIN-CNT )
                MOVE   SPA-GMN-W21-TOKNYUINLST (IDX1)
                                   TO  W21-BRM-TOKUTEINYUIN-ITM (IDX1)
           END-PERFORM
      *
      *    収容人数
           MOVE    SPA-GMN-W21-NINZU       TO  W21-BRM-NINZU
      *
      *    室料差額コンボ
           MOVE    SPA-GMN-W21-SAGAKU-G    TO  W21-BRM-SAGAKU
           MOVE    SPA-GMN-W21-SAGAKU-MAX  TO  W21-BRM-SAGAKU-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BRM-SAGAKU-CNT )
                MOVE   SPA-GMN-W21-SAGAKULST (IDX1)
                                   TO  W21-BRM-SAGAKU-ITM (IDX1)
           END-PERFORM
      *
      *    性別特定コンボ
           MOVE    SPA-GMN-W21-SEX-G       TO  W21-BRM-SEX
           MOVE    SPA-GMN-W21-SEX-MAX     TO  W21-BRM-SEX-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BRM-SEX-CNT )
                MOVE   SPA-GMN-W21-SEXLST (IDX1)
                                   TO  W21-BRM-SEX-ITM (IDX1)
           END-PERFORM
      *
      *    内線番号
           MOVE    SPA-GMN-W21-NAITEL      TO  W21-BRM-NAITEL
      *
      *    病室科名コンボ
           MOVE    SPA-GMN-W21-SRYKA-G     TO  W21-BRM-KA
           MOVE    SPA-GMN-W21-SRYKA-MAX   TO  W21-BRM-KA-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-BRM-KA-CNT )
                MOVE   SPA-GMN-W21-SRYKALST (IDX1)
                                   TO  W21-BRM-KA-ITM (IDX1)
           END-PERFORM
      *
      *    入院基本料コンボ
           MOVE    SPA-GMN-W21-KHNRYO-G     TO  W21-KHNRYO
           MOVE    SPA-GMN-W21-KHNRYO-MAX   TO  W21-KHNRYO-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-KHNRYO-CNT )
                MOVE   SPA-GMN-W21-KHNRYOLST (IDX1)
                                   TO  W21-KHNRYO-ITM (IDX1)
           END-PERFORM
      *
           MOVE    SPA-GMN-W21-KHNTEN      TO  W21-KHNTEN
      *
      *    病棟機能（特定入院料）
           MOVE    SPA-GMN-W21-TBTUTYPE-G  TO  W21-TBTUTYPE
           MOVE    SPA-GMN-W21-TBTUTYPE-MAX
                                           TO  W21-TBTUTYPE-CNT
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   W21-TBTUTYPE-CNT )
                MOVE   SPA-GMN-W21-TBTUTYPELST (IDX1)
                                   TO  W21-TBTUTYPE-ITM (IDX1)
           END-PERFORM
      *
      *    病棟コード（特定入院料）
           MOVE    SPA-GMN-W21-TBTUCD      TO  W21-TBTUCD
      *
      *    有効期間開始日
           MOVE    SPA-GMN-W21-YUKOSTYMD   TO  W21-YUKOSTYMD
      *
      *    有効期間終了日
           MOVE    SPA-GMN-W21-YUKOEDYMD   TO  W21-YUKOEDYMD
      *
      *    選択番号２
           MOVE    SPA-GMN-W21-SELNUM2     TO  W21-SELNUM2
      *
           .
       5001-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    項目状態編集処理
      *****************************************************************
       500-GMNHEN-STATE-EDIT-SEC       SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  W21-B02-STATE
                                           W21-B05-STATE
                                           W21-B06-STATE
                                           W21-B07-STATE
                                           W21-B08-STATE
                                           W21-B09-STATE
                                           W21-B12-STATE
                                           W21-BRM-NUM-STT
                                           W21-BRM-SBT-STT
                                           W21-BRM-SBT-ITM-STT
                                           W21-BRM-TOKUTEINYUIN-STT
                                           W21-BRM-TOKUTEINYUIN-ITM-STT
                                           W21-BRM-NINZU-STT
                                           W21-BRM-SAGAKU-STT
                                           W21-BRM-SAGAKU-ITM-STT
                                           W21-BRM-SEX-STT
                                           W21-BRM-SEX-ITM-STT
                                           W21-BRM-NAITEL-STT
                                           W21-BRM-KA-STT
                                           W21-BRM-KA-ITM-STT
                                           W21-YUKOSTYMD-STT
                                           W21-YUKOEDYMD-STT
                                           W21-KHNRYO-STT
                                           W21-KHNRYO-ITM-STT
                                           W21-KHNTEN-STT
                                           W21-TBTUTYPE-STATE
                                           W21-TBTUTYPE-ITM-STATE
                                           W21-TBTUCD-STATE
                                           W21-SELNUM2-STT
      *
      *
           IF    ( SPA-GMN-W21-BRMLST-MAX  >=  200 )
               MOVE    WIDGET-INSENSITIVE  TO  W21-B06-STATE
           END-IF
      *
           IF    ( SPA-NAI-W21-INPUT-MODE   =  2 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W21-B06-STATE
                                           W21-B08-STATE
                                           W21-B09-STATE
                                           W21-BRM-NUM-STT
                                           W21-BRM-SBT-STT
                                           W21-BRM-SBT-ITM-STT
                                           W21-BRM-TOKUTEINYUIN-STT
                                           W21-BRM-TOKUTEINYUIN-ITM-STT
                                           W21-BRM-NINZU-STT
                                           W21-BRM-SAGAKU-STT
                                           W21-BRM-SAGAKU-ITM-STT
                                           W21-BRM-SEX-STT
                                           W21-BRM-SEX-ITM-STT
                                           W21-BRM-NAITEL-STT
                                           W21-BRM-KA-STT
                                           W21-BRM-KA-ITM-STT
                                           W21-YUKOSTYMD-STT
                                           W21-YUKOEDYMD-STT
                                           W21-KHNRYO-STT
                                           W21-KHNRYO-ITM-STT
                                           W21-KHNTEN-STT
                                           W21-TBTUTYPE-STATE
                                           W21-TBTUTYPE-ITM-STATE
                                           W21-TBTUCD-STATE
                                           W21-SELNUM2-STT
           END-IF
      *
           PERFORM VARYING IDX-GKSN FROM    1   BY  1
                   UNTIL ( IDX-GKSN >  CONST-GKSN-MAX )
               IF    ( W21-KSN-CNT (IDX-GKSN)   >   ZERO )
                   MOVE    WIDGET-NORMAL   
                                           TO  W21-KSN-STATE (IDX-GKSN)
                                               W21-KSN-ITM-STATE
                                                             (IDX-GKSN)
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                                           TO  W21-KSN-STATE (IDX-GKSN)
                                               W21-KSN-ITM-STATE
                                                             (IDX-GKSN)
               END-IF
           END-PERFORM
      *
           IF    ( SPA-NAI-W21-YUKOSTYMD  >=   CONST-H180701 )
            AND  ( SPA-NAI-W21-BTUSBT      =   "04" OR "09" )
                   CONTINUE
            ELSE
               MOVE    WIDGET-INSENSITIVE
                                       TO  W21-KHNRYO-STT
                                           W21-KHNRYO-ITM-STT
                                           W21-KHNTEN-STT
           END-IF
      *
      *
           IF    ( SPA-NAI-W21-HOSPSBT =   2 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W21-TBTUTYPE-STATE
                                           W21-TBTUTYPE-ITM-STATE
                                           W21-TBTUCD-STATE
           END-IF
      *
           IF    ( W21-TBTUTYPE        =   SPACE )
               MOVE    WIDGET-INSENSITIVE
                                       TO  W21-TBTUCD-STATE
           END-IF
      *
           .
       500-GMNHEN-STATE-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-W21-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-W21-CUR
           WHEN    001
               MOVE    "BTU_NAME"          TO  MCP-WIDGET
           WHEN    002
               MOVE    "SELNUM1"           TO  MCP-WIDGET
           WHEN    003
               MOVE    "BRM_NUM"           TO  MCP-WIDGET
           WHEN    004
               MOVE    "BRM_SBT"           TO  MCP-WIDGET
           WHEN    005
               MOVE    "BRM_TOKUTEINYUIN"  TO  MCP-WIDGET
           WHEN    006
               MOVE    "BRM_NINZU"         TO  MCP-WIDGET
           WHEN    007
               MOVE    "BRM_SAGAKU"        TO  MCP-WIDGET
           WHEN    008
               MOVE    "BRM_SEX"           TO  MCP-WIDGET
           WHEN    009
               MOVE    "BRM_NAITEL"        TO  MCP-WIDGET
           WHEN    010
               MOVE    "BRM_KA"            TO  MCP-WIDGET
           WHEN    011
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           WHEN    012
               MOVE    "YUKOEDYMD"         TO  MCP-WIDGET
           WHEN    013
               MOVE    "KHNRYO"            TO  MCP-WIDGET
           WHEN    014
               MOVE    "KSN01"             TO  MCP-WIDGET
           WHEN    015
               MOVE    "KSN02"             TO  MCP-WIDGET
           WHEN    016
               MOVE    "KSN03"             TO  MCP-WIDGET
           WHEN    017
               MOVE    "KSN04"             TO  MCP-WIDGET
           WHEN    018
               MOVE    "KSN05"             TO  MCP-WIDGET
           WHEN    019
               MOVE    "KSN06"             TO  MCP-WIDGET
           WHEN    020
               MOVE    "KSN07"             TO  MCP-WIDGET
           WHEN    021
               MOVE    "KSN08"             TO  MCP-WIDGET
           WHEN    022
               MOVE    "KSN09"             TO  MCP-WIDGET
           WHEN    023
               MOVE    "KSN10"             TO  MCP-WIDGET
           WHEN    024
               MOVE    "KSN11"             TO  MCP-WIDGET
           WHEN    025
               MOVE    "KSN12"             TO  MCP-WIDGET
           WHEN    026
               MOVE    "KSN13"             TO  MCP-WIDGET
           WHEN    027
               MOVE    "KSN14"             TO  MCP-WIDGET
           WHEN    100
               MOVE    "SELNUM2"           TO  MCP-WIDGET
           WHEN    101
               MOVE    "KJNYMD"            TO  MCP-WIDGET
           WHEN    102
               MOVE    "TBTUTYPE"          TO  MCP-WIDGET
           WHEN    103
               MOVE    "TBTUCD"            TO  MCP-WIDGET
           WHEN    206
               MOVE    "B06"               TO  MCP-WIDGET
           WHEN    207
               MOVE    "B07"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "BTU_NAME"
               MOVE    002             TO  SPA-GMN-W21-CUR
           WHEN    "SELNUM1"
               MOVE    003             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_NUM"
               MOVE    004             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_SBT"
               MOVE    005             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_TOKUTEINYUIN"
               MOVE    006             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_NINZU"
               MOVE    007             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_SAGAKU"
               MOVE    008             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_SEX"
               MOVE    009             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_NAITEL"
               MOVE    010             TO  SPA-GMN-W21-CUR
           WHEN    "BRM_KA"
               MOVE    013             TO  SPA-GMN-W21-CUR
           WHEN    "KHNRYO"
               MOVE    102             TO  SPA-GMN-W21-CUR
           WHEN    "TBTUTYPE"
               MOVE    103             TO  SPA-GMN-W21-CUR
           WHEN    "TBTUCD"
               MOVE    014             TO  SPA-GMN-W21-CUR
           WHEN    "KSN01"
               MOVE    015             TO  SPA-GMN-W21-CUR
           WHEN    "KSN02"
               MOVE    016             TO  SPA-GMN-W21-CUR
           WHEN    "KSN03"
               MOVE    017             TO  SPA-GMN-W21-CUR
           WHEN    "KSN04"
               MOVE    018             TO  SPA-GMN-W21-CUR
           WHEN    "KSN05"
               MOVE    019             TO  SPA-GMN-W21-CUR
           WHEN    "KSN06"
               MOVE    020             TO  SPA-GMN-W21-CUR
           WHEN    "KSN07"
               MOVE    021             TO  SPA-GMN-W21-CUR
           WHEN    "KSN08"
               MOVE    022             TO  SPA-GMN-W21-CUR
           WHEN    "KSN09"
               MOVE    023             TO  SPA-GMN-W21-CUR
           WHEN    "KSN10"
               MOVE    024             TO  SPA-GMN-W21-CUR
           WHEN    "KSN11"
               MOVE    025             TO  SPA-GMN-W21-CUR
           WHEN    "KSN12"
               MOVE    026             TO  SPA-GMN-W21-CUR
           WHEN    "KSN13"
               MOVE    027             TO  SPA-GMN-W21-CUR
           WHEN    "KSN14"
               MOVE    011             TO  SPA-GMN-W21-CUR
           WHEN    "YUKOSTYMD"
               MOVE    012             TO  SPA-GMN-W21-CUR
           WHEN    "YUKOEDYMD"
               MOVE    206             TO  SPA-GMN-W21-CUR
           WHEN    "SELNUM2"
               MOVE    003             TO  SPA-GMN-W21-CUR
           WHEN    "KJNYMD"
               MOVE    002             TO  SPA-GMN-W21-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    画面カーソルスキップ処理
      *****************************************************************
       5012-SKIP-CUR-SEC           SECTION.
      *
           IF    ( SPA-GMN-W21-CUR =   13 )
               IF    ( W21-KHNRYO-STT
                                       =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-W21-CUR =   102
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W21-CUR =   102 )
               IF    ( W21-TBTUTYPE-STATE
                                       =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-W21-CUR =   103
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-W21-CUR =   103 )
               IF    ( W21-TBTUCD-STATE
                                       =   WIDGET-INSENSITIVE )
                   COMPUTE SPA-GMN-W21-CUR =   14
               END-IF
           END-IF
      *
           PERFORM VARYING IDX-GKSN    FROM    1   BY  1
                   UNTIL ( IDX-GKSN    >   CONST-GKSN-MAX )
      *
               IF    ( SPA-GMN-W21-CUR =   IDX-GKSN + 13 )
      *
                   IF    ( W21-KSN-STATE (IDX-GKSN)
                                       =   WIDGET-INSENSITIVE )
                       COMPUTE SPA-GMN-W21-CUR
                           =   IDX-GKSN
                           +   14
      *
      *
                   IF    ( IDX-GKSN    =   CONST-GKSN-MAX )
                       COMPUTE SPA-GMN-W21-CUR
                           =   11
                   END-IF
      *
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF    ( SPA-GMN-W21-CUR     =   206 )
               IF    ( W21-B06-STATE   =   WIDGET-INSENSITIVE )
                   MOVE    207         TO  SPA-GMN-W21-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-NAI-W21-INPUT-MODE  =   2 )
               EVALUATE    SPA-GMN-W21-CUR
               WHEN    1
               WHEN    14  THRU    27
               WHEN    207
                   CONTINUE
               WHEN    11
                   COMPUTE SPA-GMN-W21-CUR =   207
               WHEN    OTHER
                   COMPUTE SPA-GMN-W21-CUR =   14
               END-EVALUATE
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "選択番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "病室番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "病室種別の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "特定入院料の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "収容人数の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0006"
               MOVE    "室料差額の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "性別特定の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "内線番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0009"
               MOVE    "診療科の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "入院基本料の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0011"
               MOVE    "有効期間開始日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0012"
               MOVE    "有効期間終了日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0013"
               MOVE    "室歴番号の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0014"
               MOVE    "基準日の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0015"
               MOVE    "選択された番号が多すぎます"
                                       TO  SPA-ERRMSG
           WHEN    "0016"
               STRING  "有効開始日が"      DELIMITED BY SIZE
                       WRK-MSGPLUS         DELIMITED BY SPACE
                       "以降の病室と"      DELIMITED BY SIZE
                       "これより前の病室は同時に選択できません"
                                           DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0019"
               MOVE    "病棟機能の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "連番は１から５０の範囲で設定してください"
                                       TO  SPA-ERRMSG
           WHEN    "0100"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "有効開始日は平成１４年４月１日以降にして"
                       "ください"
                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "0101"
               MOVE    "日付の入力に誤りがあります"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "追加可能最大件数に達しました"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
               MOVE    "変更対象の病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
               MOVE    "削除対象の病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "1004"
               MOVE    "有効期間開始日＞有効期間終了日となっています"
                                       TO  SPA-ERRMSG
           WHEN    "1006"
               MOVE    "病棟名称を選択して下さい"
                                       TO  SPA-ERRMSG
           WHEN    "1007"
               MOVE    "入力番号に該当する病棟は存在しません"
                                       TO  SPA-ERRMSG
           WHEN    "1008"
               MOVE    "範囲内の選択番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1009"
               MOVE    "収容人数に１以上を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1010"
               MOVE    1               TO  WRK-PNT
               IF    ( WRK-KSNMEI      =   SPACE )
                   STRING  "加算に必要な施設基準が設定されていません"
                           DELIMITED   BY  SPACE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               ELSE
                   STRING  "必要な施設基準が設定されていません／"
                           DELIMITED   BY  SPACE
                           WRK-KSNMEI
                           DELIMITED   BY  SPACE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               END-IF
      *
               PERFORM VARYING IDX-SST FROM    1   BY  1
                       UNTIL ( IDX-SST >   CNT-SST )
      *
                   STRING  WRK-SSTKJNCD-OCC  (IDX-SST)
                           DELIMITED   BY  SIZE
                   INTO    SPA-ERRMSG
                   WITH    POINTER         WRK-PNT
                   END-STRING
               END-PERFORM
           WHEN    "1011"
               STRING  "入院料に対して"
                       WRK-KSNMEI      DELIMITED   BY  SPACE
                       "は算定できません"
                                       DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1012"
               MOVE    "選択中の病室に複数の入院料が存在する場合"
                                       TO  SPA-ERRMSG (1 : 40)
               MOVE    "は変更できません"
                                       TO  SPA-ERRMSG (41 : )
           WHEN    "1101"
               MOVE     "病室番号を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1102"
               MOVE     "有効期間開始日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1103"
               MOVE     "有効期間終了日を入力してください"
                                       TO  SPA-ERRMSG
           WHEN    "1104"
               MOVE    "有効期間の重複するデータが存在します"
                                       TO  SPA-ERRMSG
           WHEN    "1105"
               MOVE    "病室を選択してください"
                                       TO  SPA-ERRMSG
           WHEN    "1106"
               MOVE    "病室番号の変更はできません"
                                       TO  SPA-ERRMSG
           WHEN    "1107"
               MOVE    "有効期間の開始日と終了日は同時に変更できません"
                                       TO  SPA-ERRMSG
           WHEN    "1108"
               MOVE    "入院中の患者が存在するため削除できません"
                                       TO  SPA-ERRMSG
           WHEN    "1109"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "レセ電病棟情報を設定する場合、病棟機能と連番"
                                           DELIMITED   BY  SIZE
                       "の両方を設定してください"
                                           DELIMITED   BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    "1201"
           WHEN    "1202"
           WHEN    "1203"
           WHEN    "1204"
           WHEN    "1205"
               MOVE    "更新処理に失敗しました"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  WERR
           MOVE    SPA-ERRCD           TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  WERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "W21"               TO  SPA-MOTOPG
           MOVE    "WERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "WERR"              TO  MCP-WINDOW
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
       520-WIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-WIDCD
           WHEN    "2001"
               MOVE    "該当の病室を削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "2001"
               MOVE    "該当の病室を削除します。よろしいですか"
                                        TO  WRK-WIDMSG
           WHEN    "2002"
               MOVE    "選択中の病室について入院料加算を変更しま"
                                        TO  WRK-WIDMSG (1 : 40)
               MOVE    "す"
                                        TO  WRK-WIDMSG (41 : )
           WHEN    "2003"
               MOVE    SPACE            TO  WRK-WIDMSG
               STRING  "有効期間の重複するデータが存在します。"
                                            DELIMITED   BY  SIZE
                       "前の期間を区切りますか"
                                            DELIMITED   BY  SIZE
               INTO    WRK-WIDMSG
               END-STRING
           END-EVALUATE
      *
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "W21"                TO  SPA-MOTOPG
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
      *    病棟コンボボックスチェック処理
      *****************************************************************
       4200-BTUNUMLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-BTUNUM-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-BTUNUML (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-BTUNUMLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-NAI-W21-BTUINF-TOKNYUIN (IDX-CMB)
                                   TO  WRK-CMB-ITM2
      *
                   MOVE    SPA-NAI-W21-BTUINF-KHNSRYCD (IDX-CMB)
                                   TO  WRK-CMB-ITM3
      *
                   MOVE    SPA-NAI-W21-BTUINF-STYUKYMD (IDX-CMB)
                                   TO  WRK-CMB-ITM4
                   MOVE    SPA-NAI-W21-BTUINF-SBT (IDX-CMB)
                                   TO  WRK-CMB-ITM5
               END-IF
      *
           END-PERFORM
      *
           .
       4200-BTUNUMLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科コンボボックスチェック処理
      *****************************************************************
       4200-SRYKALST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-SRYKA-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-SRYKAL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-SRYKALST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SRYKALST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院基本料コンボボックスチェック処理
      *****************************************************************
       4200-KHNRYOLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-KHNRYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-KHNRYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-KHNRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
                   MOVE    SPA-NAI-W21-KHNRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM2
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KHNRYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病室種別コンボボックスチェック処理
      *****************************************************************
       4200-SBTLST-CHK-SEC                 SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-SBT-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-SBTL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-SBTLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SBTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボボックスチェック処理
      *****************************************************************
       4200-TOKNYUINLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-TOKNYUINL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-TOKNYUINLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-GMN-W21-TOKNYUINL-TANMEI(IDX-CMB)
                                   TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-W21-TOKNYUINL-SRYCD (IDX-CMB)
                                   TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-W21-TOKNYUINL-KBNCD (IDX-CMB)
                                   TO  WRK-CMB-ITM4
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TOKNYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    特定入院料コンボボックスチェック処理
      *****************************************************************
       4201-TOKNYUINLST-CHK-SEC    SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-TOKNYUIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-TOKNYUINL-KBNCD (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-TOKNYUINLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
      *
                   MOVE    SPA-GMN-W21-TOKNYUINL-TANMEI(IDX-CMB)
                                   TO  WRK-CMB-ITM2
                   MOVE    SPA-GMN-W21-TOKNYUINL-SRYCD (IDX-CMB)
                                   TO  WRK-CMB-ITM3
                   MOVE    SPA-GMN-W21-TOKNYUINL-KBNCD (IDX-CMB)
                                   TO  WRK-CMB-ITM4
               END-IF
      *
           END-PERFORM
      *
           .
       4201-TOKNYUINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額コンボボックスチェック処理
      *****************************************************************
       4200-SAGAKULST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-SAGAKU-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-SAGAKUL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-SAGAKULST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SAGAKULST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    性別コンボボックスチェック処理
      *****************************************************************
       4200-SEXLST-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-SEX-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-SEXL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-SEXLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SEXLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    新生児入院医療管理加算コンボボックスチェック処理
      *****************************************************************
       4200-SINSEIJILST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-SINSEIJI-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-SINSEIJIL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-SINSEIJILST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W21-SINSEIJILST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SINSEIJILST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療所療養病床療養環境加算コンボボックスチェック処理
      *****************************************************************
       4200-SNRRYOLST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-SNRRYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-SNRRYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-SNRRYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    SPA-GMN-W21-SNRRYOLST (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-SNRRYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    放射線治療病室管理加算コンボボックスチェック処理
      *****************************************************************
       4200-HOSYASENLST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-HOSYASEN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-HOSYASENL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-HOSYASENLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W21-HOSYASENLST   (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-HOSYASENLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養環境加算コンボボックスチェック処理
      *****************************************************************
       4200-RYOLST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-RYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-RYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-RYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W21-RYOLST   (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-RYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    療養病棟療養環境加算コンボボックスチェック処理
      *****************************************************************
       4200-BTURYOLST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-BTURYO-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-BTURYOL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-BTURYOLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W21-BTURYOLST   (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-BTURYOLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    重症者等療養環境特別加算コンボボックスチェック処理
      *****************************************************************
       4200-JYUTOKLST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-JYUTOK-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-JYUTOKL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-JYUTOKLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W21-JYUTOKLST   (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-JYUTOKLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    無菌治療室管理加算コンボボックスチェック処理
      *****************************************************************
       4200-MUKINLST-CHK-SEC               SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-MUKIN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-MUKINL (IDX-CMB)
                                       =   WRK-CMB-CD )
      *
                   MOVE    SPA-GMN-W21-MUKINLST   (IDX-CMB)
                                   TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           IF    ( WRK-CMB-ITM         =   SPACE )
                   MOVE    SPA-GMN-W21-MUKINLST   (1)
                                       TO  WRK-CMB-ITM
           END-IF
      *
           .
       4200-MUKINLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    病棟状態コンボチェック処理
      *****************************************************************
       4200-TBTUTYPELST-CHK-SEC    SECTION.
      *
           INITIALIZE              WRK-CMB-ITM-G
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-W21-TBTUTYPE-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-W21-TBTUTYPEL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-W21-TBTUTYPELST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-TBTUTYPELST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    全角半角混在チェック
      *****************************************************************
       800-ENRTRY-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-INPUT
                                   =   SPACE )
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT
                                   TO  WRK-KANACHK-INPUT
           IF      ( KANACHK-RC    NOT =   ZERO )
               MOVE    9           TO  FLG-ENTRYCHK
               GO  TO  800-ENRTRY-CHK-EXT
           END-IF
      *
           MOVE    KANACHK-SYUBETU TO  FLG-ENTRYCHK
      *
           .
       800-ENRTRY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       800-HIZUKE-CHK-SEC             SECTION.
      *
           IF    ( WRK-YMD         =   "00000000" 
                                   OR  "99999999" )
               MOVE    WRK-YMD     TO  WRK-HENYMDG
                                       WRK-SYMD
           ELSE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    WRK-YMD         TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
               IF  ( SGDAY-RC          =   ZERO )
                   MOVE    SGDAY-OTDAY TO  WRK-HENYMDG
                   MOVE    SGDAY-SDAY  TO  WRK-SYMD
               ELSE
                   MOVE    "0101"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                  SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD         TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-EDTYMD3
           INSPECT WRK-EDTYMD3     REPLACING  ALL "  "  BY  "　"
      *
           .
       800-HIZUKE-EXT.
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
      *    入院料加算チェックテーブル検索処理(KEY)
      *****************************************************************
       900-NYUKSNCHK-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUKSNCHK
      *
           INITIALIZE                      NYUKSNCHK-REC
           MOVE    WRK-NYUINKBN        TO  NYUKSNCHK-NYUINKBN
           MOVE    WRK-KSNKBN          TO  NYUKSNCHK-KSNKBN
           MOVE    WRK-YUKOSTYMD       TO  NYUKSNCHK-YUKOSTYMD
                                           NYUKSNCHK-YUKOEDYMD
           MOVE    NYUKSNCHK-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUKSNCHK-REC
           ELSE
               INITIALIZE                  NYUKSNCHK-REC
               MOVE    1               TO  FLG-NYUKSNCHK
           END-IF
      *
           MOVE    "tbl_nyuksnchk"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUKSNCHK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-TNSSRYCD        TO  TNS-SRYCD
           MOVE    WRK-TNSYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
           ELSE
               MOVE    1               TO  FLG-TENSU
               INITIALIZE                  TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
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
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY8)
      *****************************************************************
       900-SYSKANRI-KEY8-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY9)
      *****************************************************************
       900-SYSKANRI-KEY9-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ検索処理(KEY9)
      *****************************************************************
       900-SYSKANRI-KEY9-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               INITIALIZE                  SYSKANRI-REC
           END-IF
      *
           .
       900-SYSKANRI-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY16-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYS-5002-REC
           MOVE    "5002"              TO  SYS-5002-KANRICD
           MOVE    SPA-GMN-W21-BTUNUM  TO  WRK-SYS5002-BTUNUM
           MOVE    SPA-GMN-W21-BRMNUM  TO  WRK-BRMNUM
           PERFORM 4100-BRMNUM-KEY-EDIT-SEC
           MOVE    WRK-BRMNUM-KEY      TO  WRK-SYS5002-BRMNUM
           MOVE    WRK-SYS5002-KBNCD   TO  SYS-5002-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5002-HOSPNUM
           MOVE    SPA-NAI-W21-YUKOSTYMD
                                       TO  SYS-5002-STYUKYMD
           MOVE    SYS-5002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYS-5002-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYS-5002-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY16-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理(KEY15)
      *****************************************************************
       900-PTNYUINRRK-KEY15-SEL-SEC    SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM      TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-NAI-W21-5002-KBNCD
                                   TO  PTNYUINRRK-BRMNUM
           MOVE    SPA-NAI-W21-YUKOEDYMD-OLD
                                   TO  PTNYUINRRK-NYUINYMD
           MOVE    SPA-NAI-W21-YUKOSTYMD-OLD
                                   TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key15"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY15-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING       MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"      USING      MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
