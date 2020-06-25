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
       PROGRAM-ID.                               ORCGZ07.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 自院コード期限切れ変更（Ｚ０７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/06/17    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/12/12  MONFUNC対応 他
      *  04.04.00    NACL-太田    09/01/22  チェックマスタ
      *                                     薬情マスタ更新処理追加
      *  04.05.00    NACL-太田    09/11/24  経過措置医薬品置換えマスタ一覧
      *                                     印刷機能追加
      *  05.00.00    NACL-森脇    17/12/18  クライアント印刷対応
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
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア(検索画面用）
           COPY    "Z01SCR-SPA".
      *
      *    画面ＳＰＡエリア
      *
       01  SPA-Z07.
           03  SPA-GMN-Z07-CUR             PIC 9(03).
           03  SPA-GMN-MAX                 PIC 9(04).
           03  SPA-GMN-PAGE                PIC 9(02).
      *
           03  SPA-GMN-AREA.
               05  SPA-GMN-YUKOYMD          PIC X(09).
               05  SPA-GMN-TEN-LIST.
                   07  SPA-GMN-TEN-LST-OCC         OCCURS   200.
                       09  SPA-GMN-TNUM            PIC 9(03).
                       09  SPA-GMN-TINPUTCD        PIC X(20).
                       09  SPA-GMN-TSRYCD          PIC X(09).
                       09  SPA-GMN-TNAME           PIC X(100).
                       09  SPA-GMN-TYUKOSTYMD      PIC X(09).
                       09  SPA-GMN-TYUKOEDYMD      PIC X(09).
      *
                       09  SPA-NAI-TYKZKBN         PIC X(01).
                       09  SPA-NAI-TNAME           PIC X(100).
      *
                   07  SPA-TENSU-MAX               PIC 9(04).
      *
               05  SPA-GMN-IN-AREA.
                   07  SPA-GMN-SELNUM              PIC 9(003).
                   07  SPA-GMN-OLD-SRYCD           PIC X(09).
                   07  SPA-GMN-OLD-NAME            PIC X(100).
                   07  SPA-NAI-OLD-NAME            PIC X(100).
                   07  SPA-NAI-OLD-YKZKBN          PIC X(01).
      *
                   07  SPA-GMN-SRYCD-G.
                       09  SPA-GMN-SRYCD           PIC X(09).
                       09  SPA-GMN-FIL             PIC X(11).
                   07  SPA-GMN-NAME            PIC X(100).
                   07  SPA-GMN-CHKCHG-G.
                       09  SPA-GMN-CHKCHG      PIC X(01).
                       09  SPA-GMN-CHKCHGFIL   PIC X(01).
                       09  SPA-GMN-CHKCHGMEI   PIC X(18).
                   07  SPA-GMN-YKJCHG-G.
                       09  SPA-GMN-YKJCHG      PIC X(01).
                       09  SPA-GMN-YKJCHGFIL   PIC X(01).
                       09  SPA-GMN-YKJCHGMEI   PIC X(18).
                   07  SPA-GMN-PTKINKICHG-G.
                       09  SPA-GMN-PTKINKICHG      PIC X(01).
                       09  SPA-GMN-PTKINKICHGFIL   PIC X(01).
                       09  SPA-GMN-PTKINKICHGMEI   PIC X(18).
      *
               05  SPA-GMN-INPUTCD-G.
                   07  SPA-GMN-INPUTCD-OCC     OCCURS  10.
                       09  SPA-GMN-INPUTCD     PIC X(20).
                       09  SPA-GMN-CDSYU       PIC X(01).
      *
               05  SPA-GMN-NEXT                PIC 9(01).
      *
             03  SPA-GMN-COMBO-AREA.
               05  SPA-GMN-CHKCHGLST-G.
                   07  SPA-GMN-CHKCHG-MAX      PIC 9(03).
                   07  SPA-GMN-CHKCHGLST       OCCURS  3.
                       09  SPA-GMN-CHKCHGL     PIC X(01).
                       09  SPA-GMN-CHKCHGFLL   PIC X(01).
                       09  SPA-GMN-CHKCHGMEIL  PIC X(18).
               05  SPA-GMN-YKJCHGLST-G.
                   07  SPA-GMN-YKJCHG-MAX      PIC 9(03).
                   07  SPA-GMN-YKJCHGLST       OCCURS  3.
                       09  SPA-GMN-YKJCHGL     PIC X(01).
                       09  SPA-GMN-YKJCHGFLL   PIC X(01).
                       09  SPA-GMN-YKJCHGMEIL  PIC X(18).
               05  SPA-GMN-PTKINKICHGLST-G.
                   07  SPA-GMN-PTKINKICHG-MAX      PIC 9(03).
                   07  SPA-GMN-PTKINKICHGLST       OCCURS  3.
                       09  SPA-GMN-PTKINKICHGL     PIC X(01).
                       09  SPA-GMN-PTKINKICHGFLL   PIC X(01).
                       09  SPA-GMN-PTKINKICHGMEIL  PIC X(18).
             03  SPA-NAI-AREA.
                 05  SPA-NAI-YUKOYMD       PIC X(08).
      *
                 05  SPA-NEXT-YUKOYMD      PIC X(08).
      *
                 05  SPA-NAI-CURIDX        PIC 9(02).
                 05  SPA-NAI-TBL-GROUP     PIC X(14).
                 03  SPA-NAI-JOB-JOBID     PIC 9(07).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-INPUTCD                     PIC 9(01).
           03  FLG-SRYCDCHG                    PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
      *
           03  FLG-KANJI                       PIC 9(01).
           03  FLG-KENSAKU                     PIC 9(01).
           03  FLG-ERR                         PIC 9(01).
           03  FLG-OK                          PIC 9(01).
           03  FLG-CHKMST                      PIC 9(01).
           03  FLG-CHKSND                      PIC 9(01).
           03  FLG-CHKTRD                      PIC 9(01).
           03  FLG-CHK005                      PIC 9(01).
           03  FLG-YAKUJYO                     PIC 9(01).
           03  FLG-PTKINKI                     PIC 9(01).
           03  FLG-TCHKMST                     PIC 9(01).
           03  FLG-TCHKSND                     PIC 9(01).
           03  FLG-TCHKTRD                     PIC 9(01).
           03  FLG-TCHK005                     PIC 9(01).
           03  FLG-TYAKUJYO                    PIC 9(01).
           03  FLG-TPTKINKI                    PIC 9(01).
           03  FLG-LOOPEND                     PIC 9(01).
           03  FLG-ROW                         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
           03  IDZ                               PIC 9(04).
           03  IDX-C                             PIC 9(04).
           03  IDX-CMB                           PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
      *
           03  WRK-ZZZ                 PIC ZZZ.
      *
           03  WRK-IDX2                PIC 9(02).
      *
           03  WRK-STR                 PIC 9(04).
      *
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-INPUTCD             PIC X(20).
      *    パス名
           03  WRK-PATH                PIC X(64).
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-MAE-SRYCD           PIC X(09).
      *
           03  WRK-TNS-YUKOSTYMD       PIC X(08).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-NEXTDAY             PIC X(08).
           03  WRK-SHORIERR            PIC X(200).
           03  WRK-ZIDMSG              PIC X(70).
           03  WRK-STPAGE              PIC 9(05).
           03  WRK-EDPAGE              PIC 9(05).
           03  WRK-JOBID               PIC 9(07).
           03  WRK-PAGE                PIC ZZZZZZZZZ9.
           03  WRK-PRT-PAGE            PIC 9(10).
           03  WRK-ROW                 PIC S9(09).
           03  WRK-CHKKBN              PIC X(01).
           03  WRK-SEL-SRYCD           PIC X(09).
           03  WRK-NEW-SRYCD           PIC X(09).
           03  WRK-OLD-SRYCD           PIC X(09).
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
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  CONST-AREA.
           03  CONST-3003-KBNCD            PIC X(08)   VALUE   "102B".
           03  CONST-TBL-KEY               PIC X(08)   VALUE   "TENSU2".
           03  CONST-SHELLID               PIC X(08)   VALUE   "TENSU2".
      *
       01  TCHK-REC.
           COPY    "CPCHK.INC"     REPLACING   //CHK-//
                                   BY          //TCHK-//.
      *
       01  TCHKSND-REC.
           COPY    "CPCHKSND.INC"  REPLACING   //CHKSND-//
                                   BY          //TCHKSND-//.
      *
       01  TCHKTRD-REC.
           COPY    "CPCHKTRD.INC"  REPLACING   //CHKTRD-//
                                   BY          //TCHKTRD-//.
      *
       01  TCHK005-REC.
           COPY    "CPCHK005.INC"  REPLACING   //CHK005-//
                                   BY          //TCHK005-//.
      *
           COPY    "CPYAKUJYO.INC" REPLACING   //YAKUJYO-//
                                   BY          //TYAKUJYO-//.
      *
       01  TPTKINKI-REC.
           COPY    "CPPTKINKI.INC" REPLACING   //PTKINKI-//
                                   BY          //TPTKINKI-//.
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    印刷用呼出SHELL
           COPY    "CPHCMLSHELL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1031.INC".
           COPY    "CPSK3003.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
            COPY    "CPINPUTCD.INC".
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
        01  CHK-REC.
            COPY    "CPCHK.INC".
      *
        01  CHKSND-REC.
            COPY    "CPCHKSND.INC".
      *
        01  CHKTRD-REC.
            COPY    "CPCHKTRD.INC".
      *
        01  CHK005-REC.
            COPY    "CPCHK005.INC".
      *
            COPY    "CPYAKUJYO.INC".
      *
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *----(05.00.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(05.00.00)--ADD-END-----
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
           INITIALIZE                            IDX-AREA
           INITIALIZE                            WRK-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z07
      *
           MOVE    SPACE                         TO  SPA-ERRCD
           MOVE    ZERO                          TO  FLG-END
      *
           EVALUATE    MCP-STATUS       ALSO     MCP-EVENT
               WHEN    "LINK"           ALSO     ANY
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
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z07             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
      *
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z07    "           TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           MOVE    1                   TO  SPA-GMN-Z07-CUR
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               IF    (  SPA-MOTOPG     =   "XA01" OR "XB01" OR "XC01" )
                   MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
                   MOVE    LNK-SCRSPA      TO  SPA-Z01FREE
                   MOVE    SPA-WORK        TO  SPA-Z01KYOUTU
                   MOVE    SPA-Z01SONOTA   TO  SPA-Z07
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "ZID1"
               PERFORM 320-ZID1-SET-SEC
           WHEN    "ZID3"
               PERFORM 320-ZID3-SET-SEC
           WHEN    "Z97"
               PERFORM 340-Z97-SET-SEC
           WHEN    "Z98"
      *        一覧検索
               PERFORM 320-Z98-SPASET-SEC
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
           INITIALIZE                      SPA-Z07
      *    システム日付の末日を有効日とする
           MOVE    SPA-SYSYMD          TO  SPA-NAI-YUKOYMD
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SYSYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY7-KOYOMI     TO  SPA-NAI-YUKOYMD
           END-IF
           MOVE    SPA-NAI-YUKOYMD     TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-YUKOYMD
      *
      *    次回有効日を決定する
      *    有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOYMD     TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  SPA-NEXT-YUKOYMD
           ELSE
               MOVE    SPA-NAI-YUKOYMD     TO  SPA-NEXT-YUKOYMD
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-TENSU-HENSYU-SEC
      *
           IF      SPA-TENSU-MAX       >   ZERO
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-Z07-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-Z07-CUR
           END-IF
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コンボボックス初期編集処理
      *****************************************************************
       3101-CMBSET-SEC                 SECTION.
      *
           INITIALIZE                          SPA-GMN-COMBO-AREA
      *
           IF    ( SPA-GMN-OLD-SRYCD (1:1) =   "6" OR SPACE )
               MOVE    "0"                     TO  SPA-GMN-CHKCHGL (1)
               MOVE    "複写しない"            TO  SPA-GMN-CHKCHGMEIL(1)
               MOVE    "1"                     TO  SPA-GMN-CHKCHGL (2)
               MOVE    "複写する"              TO  SPA-GMN-CHKCHGMEIL(2)
               MOVE    2                       TO  SPA-GMN-CHKCHG-MAX
      *
               MOVE    "0"                     TO  SPA-GMN-YKJCHGL (1)
               MOVE    "複写しない"            TO  SPA-GMN-YKJCHGMEIL(1)
               MOVE    "1"                     TO  SPA-GMN-YKJCHGL (2)
               MOVE    "複写する"              TO  SPA-GMN-YKJCHGMEIL(2)
               MOVE    2                       TO  SPA-GMN-YKJCHG-MAX
      *
               MOVE    "0"                 TO  SPA-GMN-PTKINKICHGL (1)
               MOVE    "複写しない"        TO  SPA-GMN-PTKINKICHGMEIL(1)
               MOVE    "1"                 TO  SPA-GMN-PTKINKICHGL (2)
               MOVE    "複写する"          TO  SPA-GMN-PTKINKICHGMEIL(2)
               MOVE    2                   TO  SPA-GMN-PTKINKICHG-MAX
      *
               MOVE    SPA-GMN-CHKCHGLST(2)    TO  SPA-GMN-CHKCHG-G
               MOVE    SPA-GMN-YKJCHGLST(2)    TO  SPA-GMN-YKJCHG-G
               MOVE    SPA-GMN-PTKINKICHGLST(2) TO SPA-GMN-PTKINKICHG-G
           ELSE
               MOVE    "0"                     TO  SPA-GMN-CHKCHGL (1)
               MOVE    "複写しない"            TO  SPA-GMN-CHKCHGMEIL(1)
               MOVE    1                       TO  SPA-GMN-CHKCHG-MAX
      *
               MOVE    "0"                     TO  SPA-GMN-YKJCHGL (1)
               MOVE    "複写しない"            TO  SPA-GMN-YKJCHGMEIL(1)
               MOVE    1                       TO  SPA-GMN-YKJCHG-MAX
      *
               MOVE    "0"                 TO  SPA-GMN-PTKINKICHGL (1)
               MOVE    "複写しない"        TO  SPA-GMN-PTKINKICHGMEIL(1)
               MOVE    1                   TO  SPA-GMN-PTKINKICHG-MAX
      *
               MOVE    SPA-GMN-CHKCHGLST(1)    TO  SPA-GMN-CHKCHG-G
               MOVE    SPA-GMN-YKJCHGLST(1)    TO  SPA-GMN-YKJCHG-G
               MOVE    SPA-GMN-PTKINKICHGLST(1) TO SPA-GMN-PTKINKICHG-G
           END-IF
      *
           .
       3101-CMBSET-EXT.
           EXIT.
      *****************************************************************
      *    期限切れ点数マスタ一覧表編集処理
      *****************************************************************
       3101-TENSU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-TEN-LIST
      *
           IF      SPA-GMN-PAGE        =   1   OR  ZERO
               MOVE    1                   TO  WRK-STR
           ELSE
               COMPUTE WRK-STR             =  (SPA-GMN-PAGE
                                           -  1   )
                                           *  200
                                           +  1
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    ALL "0"             TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOYMD     TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    SPACE               TO  WRK-MAE-SRYCD
           PERFORM UNTIL    (FLG-TENSU     =   1  )  OR
                            (IDX           >=  200)
               MOVE    1               TO  FLG-OK
               IF      FLG-OK              =   1
      *
      *        テーブル編集
                   ADD     1                   TO  IDY
                   IF      IDY                 >=  WRK-STR
                       ADD     1               TO  IDX
      *
                       MOVE    IDX             TO  SPA-GMN-TNUM (IDX)
                       MOVE    TNS-SRYCD       TO  SPA-GMN-TSRYCD
                                                                (IDX)
                       MOVE    TNS-NAME        TO  SPA-NAI-TNAME
                                                                (IDX)
                       MOVE    TNS-NAME        TO  SPA-GMN-TNAME
                                                                (IDX)
      *                老人
                       IF      TNS-ROUTEKKBN   =   2
                           MOVE    SPACE           TO  SPA-GMN-TNAME
                                                                (IDX)
                           STRING  "［老］"
                               TNS-NAME            DELIMITED   BY  SIZE
                               INTO                SPA-GMN-TNAME(IDX)
                           END-STRING
                       END-IF
      *
                       MOVE    TNS-YUKOSTYMD   TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TYUKOSTYMD
                                                                (IDX)
                       MOVE    TNS-YUKOEDYMD   TO  WRK-SYMD
                       PERFORM 520-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TYUKOEDYMD
                                                                (IDX)
      *                薬剤区分
                       MOVE    TNS-YKZKBN      TO  SPA-NAI-TYKZKBN
                                                                (IDX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    最大件数編集
           MOVE    IDX                 TO  SPA-TENSU-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-TENSU-MAX
      *        入力コード検索
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-GMN-TSRYCD(IDX) TO  ICD-SRYCD
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key4"               TO  MCP-PATHNAME
                   PERFORM 920-INPUTCD-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-INPUTCD         =   ZERO
                   MOVE    ICD-INPUTCD     TO  SPA-GMN-TINPUTCD (IDX)
               END-IF
           END-PERFORM
      *
      *
           IF     (IDX                 >=   200 )  AND
                  (FLG-TENSU           =   ZERO)
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-NEXT
           ELSE
               MOVE    ZERO            TO  SPA-GMN-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
      *
           PERFORM 3101-CMBSET-SEC
      *
           .
       3101-TENSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       320-ZID1-SET-SEC            SECTION.
      *
           IF    ( SPA-ZID1-FLG    =   "OK" )
               EVALUATE    SPA-ZIDCD
               WHEN    "6666"
                   MOVE    "JOIN"  TO  MCP-PUTTYPE
                   PERFORM 2101-BACK-SEC

               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       320-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＺＩＤ３）ＯＫ処理
      *****************************************************************
       320-ZID3-SET-SEC            SECTION.
      *
           IF    ( SPA-ZID1-FLG    NOT =   SPACE )
               PERFORM 800-JOBKANRI-CHK-SEC
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 4111-PRINT-SEC
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-ZID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-Z97-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       320-ZID3-SET-EXT.
           EXIT.
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-Z97-SET-SEC             SECTION.
      *
           IF    ( SPA-Z97-FLG     =   "OK" )
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    ZERO            TO  SPA-GMN-Z07-CUR
      *
           MOVE    SPACE           TO  SPA-Z97-FLG
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
      *
           .
       340-Z97-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98-SPASET-SEC              SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
               MOVE    SPACE               TO  SPA-GMN-SRYCD-G
               MOVE    SPA-Z98-SRYCD       TO  SPA-GMN-SRYCD
               PERFORM 40041-TENSU-CHK-SEC
           ELSE
               MOVE    3                   TO  SPA-GMN-Z07-CUR
           END-IF
           .
       320-Z98-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *        次ページ
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-NEXT-PAGE-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-KENSAKU-SEC
      *        再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-REPRINT-SEC
      *        情報削除
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *        印刷
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM  411-PRINT-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *        処理確認
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 490-KAKUNIN-SEC
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
      *        有効年月日
               WHEN    "ACTIVATE"       ALSO    "YUKOYMD"
                   PERFORM 4001-YUKOYMD-CHK-SEC
      *        選択番号
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM 4002-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "TENSU_LIST"
                   PERFORM 4003-TENSU-LIST-SEC
      *        診療コード
               WHEN    "ACTIVATE"       ALSO    "SRYCD"
                   PERFORM 4004-SRYCD-CHK-SEC
      *        その他一括
               WHEN    OTHER
                   PERFORM 4000-ENTRY-SYORI-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効年月日チェック処理
      *****************************************************************
       4001-YUKOYMD-CHK-SEC                        SECTION.
      *
           MOVE    Z07-YUKOYMD         TO  SPA-GMN-YUKOYMD
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-YUKOYMD     TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-YUKOYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-YUKOYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-Z07-CUR
               GO      TO      4001-YUKOYMD-CHK-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 3101-TENSU-HENSYU-SEC
      *
           IF      SPA-TENSU-MAX       >   ZERO
               MOVE    2                   TO  SPA-GMN-Z07-CUR
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-Z07-CUR
           END-IF
      *
           .
      *
       4001-YUKOYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効年月日チェック処理
      *****************************************************************
       4002-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    Z07-SELNUM          TO  SPA-GMN-SELNUM
           IF      SPA-GMN-SELNUM      <   1   OR
                                       >   SPA-TENSU-MAX
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-Z07-CUR
               GO      TO      4002-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-TSRYCD (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-OLD-SRYCD
           MOVE    SPA-GMN-TNAME  (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-OLD-NAME
           MOVE    SPA-NAI-TNAME  (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-OLD-NAME
           MOVE    SPA-NAI-TYKZKBN(SPA-GMN-SELNUM)
                                       TO  SPA-NAI-OLD-YKZKBN
      *    入力コード編集
           INITIALIZE                      SPA-GMN-INPUTCD-G
           MOVE    SPACE               TO  SPA-GMN-SRYCD-G
           MOVE    SPACE               TO  SPA-GMN-NAME
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL   (FLG-INPUTCD    =   1    )  OR
                       (IDX            >=  10   )
               ADD     1                   TO  IDX
               MOVE    ICD-INPUTCD         TO  SPA-GMN-INPUTCD (IDX)
               MOVE    ICD-CDSYU           TO  SPA-GMN-CDSYU   (IDX)
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
      *
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    経過措置コード判定
      *    薬剤のみ
           IF      SPA-GMN-OLD-SRYCD(1:1)  =   "6"
               PERFORM 40021-SRYCDCHG-CHK-SEC
           END-IF
      *
           PERFORM 3101-CMBSET-SEC
      *
           .
       4002-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   経過措置コード判定処理
      *****************************************************************
       40021-SRYCDCHG-CHK-SEC                    SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-GMN-OLD-SRYCD   TO  CHG-IPNSRYCD
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
      *        点数マスタ編集
               MOVE    CHG-RJNSRYCD        TO   SPA-GMN-SRYCD
               PERFORM 40041-TENSU-CHK-SEC
               MOVE    4                   TO  SPA-GMN-Z07-CUR
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SRYCD
               MOVE    3                   TO  SPA-GMN-Z07-CUR
           END-IF
      *
           .
       40021-SRYCDCHG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   行選択処理
      *****************************************************************
       4003-TENSU-LIST-SEC                    SECTION.
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-TENSU-MAX
              IF      Z07-TENSU-SELECT (IDX)  =  "T"
                  IF      IDX                 =   SPA-GMN-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX             TO  SPA-GMN-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    SPA-GMN-SELNUM      TO  Z07-SELNUM
               PERFORM 4002-SELNUM-CHK-SEC
           END-IF
      *
           .
       4003-TENSU-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コードチェック処理
      *****************************************************************
       4004-SRYCD-CHK-SEC           SECTION.
      *
           MOVE    Z07-SRYCD           TO  SPA-GMN-SRYCD-G
      *
           MOVE    ZERO                TO  FLG-KENSAKU
      *
           MOVE    ZERO                TO  WRK-CD-MCNT
           MOVE    ZERO                TO  FLG-KANJI
      *    コード内容チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SRYCD-G     TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-SRYCD-G
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-Z07-CUR
               GO      TO    4004-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9    )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (SPA-GMN-SRYCD       NUMERIC  )
               PERFORM 40041-TENSU-CHK-SEC
               GO      TO    4004-SRYCD-CHK-EXT
           END-IF
      *
      *    検索画面へ
           MOVE    1                   TO  FLG-KENSAKU
           MOVE    SPA-GMN-SRYCD-G     TO  WRK-INPUTCD
           PERFORM 4091-Z98-KENSAKU-SEC
      *
           .
       4004-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       40041-TENSU-CHK-SEC                 SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-GMN-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TENSU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-TENSU           =   1
               MOVE    "0006"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-Z07-CUR
               GO      TO      40041-TENSU-CHK-EXT
           END-IF
      *
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
      *
      *    入力コードチェック
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-SRYCD       TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"               TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-INPUTCD         =   ZERO
               MOVE    "0007"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-Z07-CUR
               GO      TO      40041-TENSU-CHK-EXT
           END-IF
           MOVE    4                   TO  SPA-GMN-Z07-CUR
           MOVE    1                   TO  SPA-NAI-CURIDX
      *
           .
       40041-TENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       4000-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-Z07-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       4000-ENTRY-SYORI-EXT.
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
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-CHKCHG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-YKJCHG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4102-PTKINKICHG-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        関連処理
               PERFORM 4103-KANREI-SEC
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
           MOVE    Z07-INPUTCD01       TO  SPA-GMN-INPUTCD (01)
           MOVE    Z07-INPUTCD02       TO  SPA-GMN-INPUTCD (02)
           MOVE    Z07-INPUTCD03       TO  SPA-GMN-INPUTCD (03)
           MOVE    Z07-INPUTCD04       TO  SPA-GMN-INPUTCD (04)
           MOVE    Z07-INPUTCD05       TO  SPA-GMN-INPUTCD (05)
           MOVE    Z07-INPUTCD06       TO  SPA-GMN-INPUTCD (06)
           MOVE    Z07-INPUTCD07       TO  SPA-GMN-INPUTCD (07)
           MOVE    Z07-INPUTCD08       TO  SPA-GMN-INPUTCD (08)
           MOVE    Z07-INPUTCD09       TO  SPA-GMN-INPUTCD (09)
           MOVE    Z07-INPUTCD10       TO  SPA-GMN-INPUTCD (10)
           MOVE    Z07-CHKCHG          TO  SPA-GMN-CHKCHG-G
           MOVE    Z07-YKJCHG          TO  SPA-GMN-YKJCHG-G
           MOVE    Z07-PTKINKICHG      TO  SPA-GMN-PTKINKICHG-G
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
      *    同一連番がある時、１つとする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   10
                       IF     (IDX         NOT =   IDY )  AND
                              (SPA-GMN-INPUTCD (IDY)
                                           NOT =   SPACE )  AND
                              (SPA-GMN-INPUTCD (IDX)
                                               =   SPA-GMN-INPUTCD(IDY))
                           MOVE    SPACE       TO  SPA-GMN-INPUTCD(IDY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    空白ずめ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=  10
               IF      SPA-GMN-INPUTCD (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD-OCC (IDX + 1 )
                                               TO  SPA-GMN-INPUTCD-OCC
                                                                 (IDX)
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD-OCC
                                                            (IDX + 1)
               END-IF
           END-PERFORM
      *
      *    他の点数マスタに存在しないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   10  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-NAI-CURIDX
                   PERFORM 41021-INPUTCD-CHK-SEC
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-Z07-CUR
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    存在チェック処理
      *****************************************************************
       41021-INPUTCD-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-CD-MCNT
           MOVE    ZERO                TO  FLG-KANJI
      *    コード内容チェック
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-INPUTCD (IDX)
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-INPUTCD (IDX)
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *
      *        間に空白がある時、エラー
               IF      KANACHK-RC2     NOT =   ZERO
                   MOVE    "0023"          TO  SPA-ERRCD
                   GO      TO    41021-INPUTCD-CHK-EXT
               END-IF
      *
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   IF      KANACHK-MAX         >   10
                       MOVE    "0008"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        半角
               IF      KANACHK-SYUBETU     =   1
                   IF      SPA-GMN-INPUTCD(IDX)(1:KANACHK-MAX)
                                                NUMERIC
      *                数字桁数チェック
                       IF      KANACHK-MAX         <   4   OR  >  6
                           MOVE    "0009"              TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                S,Pチェック
                       IF      SPA-GMN-INPUTCD(IDX)(1:1)
                                                   =   "S"   OR  "P"
                           MOVE    "0010"          TO  SPA-ERRCD
                       END-IF
      *                #,$チェック
                       IF      SPA-GMN-INPUTCD(IDX)(1:1)
                                                   =   "#"   OR  "$"
                           MOVE    "0011"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    41021-INPUTCD-CHK-EXT
           END-IF
      *    記号は入力不可とする
           IF      FLG-KANJI           =   1
               MOVE    2                   TO  IDX-C
           ELSE
               MOVE    1                   TO  IDX-C
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  IDX-C
                   UNTIL      (IDY         >   20  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDX-C               =   1
                   IF       SPA-GMN-INPUTCD(IDX)(IDY:1)
                                               =   "+"   OR  "-"
                                               OR  "/"   OR  "!"
                                               OR  "."   OR  "*"
                       MOVE    "0011"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF     SPA-GMN-INPUTCD(IDX) (IDY:2)
                                               =   "＋"   OR  "−"
                                               OR  "／"   OR  "！"
                                               OR  "．"   OR  "＊"
                       MOVE    "0011"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO    41021-INPUTCD-CHK-EXT
           END-IF
      *
      *    セットコード存在チェック
           PERFORM 410112-DB-INPUTCD-CHK-SEC
      *
           .
       41021-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコード存在チェック
      *****************************************************************
       410112-DB-INPUTCD-CHK-SEC        SECTION.
      *
      *    入力コード使用チェック
           INITIALIZE                  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-INPUTCD(IDX)
                                       TO  ICD-INPUTCD
           IF      FLG-KANJI           =   1
               MOVE    "J"                 TO  ICD-CDSYU
           ELSE
      *        全桁数字は、コード種別を桁数で決定
               IF      SPA-GMN-INPUTCD(IDX)(1:KANACHK-MAX)
                                           NUMERIC
                   EVALUATE    KANACHK-MAX
                       WHEN    4
                           MOVE    "4"                 TO  ICD-CDSYU
                       WHEN    5
                           MOVE    "5"                 TO  ICD-CDSYU
                       WHEN    6
                           MOVE    "6"                 TO  ICD-CDSYU
                       WHEN    OTHER
                           MOVE    "A"                 TO  ICD-CDSYU
                   END-EVALUATE
               ELSE
                   MOVE    "A"                 TO  ICD-CDSYU
               END-IF
           END-IF
      *
           MOVE    ICD-CDSYU           TO  SPA-GMN-CDSYU (IDX)
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (SPA-GMN-INPUTCD(IDX)
                                       =   ICD-INPUTCD )
               IF      ICD-SRYCD       NOT =   SPA-GMN-OLD-SRYCD
                   MOVE    "0013"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       410112-DB-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ置換チェック処理
      *****************************************************************
       4102-CHKCHG-CHK-SEC          SECTION.
      *
           MOVE    ZERO        TO  IDX-CMB
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL ( IDZ     >   SPA-GMN-CHKCHG-MAX )
                    OR   ( IDX-CMB >   ZERO )
               IF    ( SPA-GMN-CHKCHGL (IDZ)
                                   =   SPA-GMN-CHKCHG )
                   MOVE    IDZ     TO  IDX-CMB
               END-IF
           END-PERFORM
      *
           IF    ( IDX-CMB     >   ZERO )
               MOVE    SPA-GMN-CHKCHGLST (IDX-CMB)
                                   TO  SPA-GMN-CHKCHG-G
           ELSE
               MOVE    "0015"      TO  SPA-ERRCD
           END-IF
      *
           .
       4102-CHKCHG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ置換チェック処理
      *****************************************************************
       4102-YKJCHG-CHK-SEC          SECTION.
      *
           MOVE    ZERO        TO  IDX-CMB
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL ( IDZ     >   SPA-GMN-YKJCHG-MAX )
                    OR   ( IDX-CMB >   ZERO )
               IF    ( SPA-GMN-YKJCHGL (IDZ)
                                   =   SPA-GMN-YKJCHG )
                   MOVE    IDZ     TO  IDX-CMB
               END-IF
           END-PERFORM
      *
           IF    ( IDX-CMB     >   ZERO )
               MOVE    SPA-GMN-YKJCHGLST (IDX-CMB)
                                   TO  SPA-GMN-YKJCHG-G
           ELSE
               MOVE    "0016"      TO  SPA-ERRCD
           END-IF
      *
           .
       4102-YKJCHG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤置換チェック処理
      *****************************************************************
       4102-PTKINKICHG-CHK-SEC          SECTION.
      *
           MOVE    ZERO        TO  IDX-CMB
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL ( IDZ     >   SPA-GMN-PTKINKICHG-MAX )
                    OR   ( IDX-CMB >   ZERO )
               IF    ( SPA-GMN-PTKINKICHGL (IDZ)
                                   =   SPA-GMN-PTKINKICHG )
                   MOVE    IDZ     TO  IDX-CMB
               END-IF
           END-PERFORM
      *
           IF    ( IDX-CMB     >   ZERO )
               MOVE    SPA-GMN-PTKINKICHGLST (IDX-CMB)
                                   TO  SPA-GMN-PTKINKICHG-G
           ELSE
               MOVE    "0017"      TO  SPA-ERRCD
           END-IF
      *
           .
       4102-PTKINKICHG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連処理
      *****************************************************************
       4103-KANREI-SEC              SECTION.
      *
      *    表示キーチェック
           IF     (SPA-GMN-INPUTCD(1)   NOT =   SPACE )  AND
                  (SPA-GMN-INPUTCD(1)(3:)   =   SPACE )  AND
                  (SPA-GMN-CDSYU  (1)       =   "J"   )
      *        表示のキーが全角１文字数字はエラー
               IF      SPA-GMN-INPUTCD(1)(1:2)     >=  "０"  AND
                                                   <=  "９"
                   MOVE    "0012"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-Z07-CUR
                   MOVE    01              TO  SPA-NAI-CURIDX
               END-IF
           END-IF
      *
      *H31.3
      *    画面表示コードが１０文字以上をエラーとする
           IF     (SPA-GMN-INPUTCD(1)  NOT =   SPACE )  AND
                  (SPA-GMN-INPUTCD(1)(11:)
                                       NOT =   SPACE )
               MOVE    "0022"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-Z07-CUR
               MOVE    01              TO  SPA-NAI-CURIDX
           END-IF
      *
           .
       4103-KANREI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 800-JOBKANRI-CHK-SEC
           IF    ( SPA-ERRCD           =   "7777" )
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    "6666"          TO  SPA-ZIDCD
           ELSE
               MOVE    "CHANGE"        TO  MCP-PUTTYPE
               PERFORM 2101-BACK-SEC
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       2101-BACK-SEC                   SECTION.
      *
           MOVE    "Z01"           TO  SPA-SAKIPG
           MOVE    "Z07"           TO  SPA-MOTOPG
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       2101-BACK-EXT.
           EXIT.
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC                   SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
      *        初期画面へ
               PERFORM 310-SPASET-SEC
           ELSE
               INITIALIZE                      SPA-GMN-IN-AREA
               INITIALIZE                      SPA-GMN-INPUTCD-G
               PERFORM 3101-CMBSET-SEC
               MOVE    2                   TO  SPA-GMN-Z07-CUR
           END-IF
      *
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    次ページ検索処理
      *****************************************************************
       407-NEXT-PAGE-SEC          SECTION.
      *
           IF      SPA-GMN-NEXT        =   ZERO
               GO      TO      407-NEXT-PAGE-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           PERFORM 3101-TENSU-HENSYU-SEC
      *
           .
       407-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面へ処理
      *****************************************************************
       409-KENSAKU-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-INPUTCD
      *
           IF     (SPA-GMN-OLD-SRYCD   NOT =   SPACE )  AND
                  (SPA-GMN-SRYCD           =   SPACE )
               MOVE    SPA-NAI-OLD-NAME(1:18)  TO  WRK-INPUTCD
               MOVE    1                       TO  FLG-KANJI
           END-IF
           PERFORM 4091-Z98-KENSAKU-SEC
      *
           .
       409-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面へ処理
      *****************************************************************
       4091-Z98-KENSAKU-SEC          SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           INITIALIZE                  SPA-Z98GMN-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           EVALUATE    SPA-GMN-OLD-SRYCD(1:1)
               WHEN    "1"
      *        診療行為
                   MOVE    "8"             TO  SPA-Z98-GMN-SYORI
               WHEN    "6"
      *        薬剤
                   EVALUATE    SPA-NAI-OLD-YKZKBN
                       WHEN    "4"
      *                    注射
                           MOVE    "5"         TO  SPA-Z98-GMN-SYORI
                       WHEN    "6"
      *                    外用
                           MOVE    "4"         TO  SPA-Z98-GMN-SYORI
                       WHEN    OTHER
      *                    内服
                           MOVE    "3"         TO  SPA-Z98-GMN-SYORI
                   END-EVALUATE
               WHEN    "7"
      *            器材
                   MOVE    "7"             TO  SPA-Z98-GMN-SYORI
           END-EVALUATE
      *
           IF      WRK-INPUTCD         NOT =   SPACE
      *********MOVE    "3"                 TO  SPA-Z98-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Z98-INPUTCD
               MOVE    "2"                 TO  SPA-Z98-TYPE
      *        すべてを対象とする
               MOVE    "1"                 TO  SPA-Z98-GMN-ALL
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Z98-CDSYUBETU
               ELSE
                   MOVE    "1"                 TO  SPA-Z98-TYPE
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
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z07"               TO  SPA-Z98-MOTOPG
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
       4091-Z98-KENSAKU-EXT.
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
           MOVE    SPA-Z07             TO  SPA-Z01SONOTA
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
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z07
      *
           .
       41011-Z98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-SHELLID       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       450-REPRINT-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-TBL-KEY       TO  SPA-PRTKANRI-TBL-KEY
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷処理
      *****************************************************************
       411-PRINT-SEC               SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 800-JOBKANRI-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "1001"      TO  SPA-ZIDCD
           END-IF
      *
           .
       411-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       4111-PRINT-SEC              SECTION.
      *
           INITIALIZE                      SPA-Z97-AREA
           MOVE    ZERO                TO  SPA-Z97-FILESV-CHK-FLG
           MOVE    ZERO                TO  WRK-JOBID
           INITIALIZE                      SPA-NAI-TBL-GROUP
                                           SPA-NAI-JOB-JOBID
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3003"              TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-KBNCD
           MOVE    "00000000"          TO  SYS-STYUKYMD
           MOVE    "99999999"          TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC              =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1005"          TO  SPA-ERRCD
               GO  TO  4111-PRINT-EXT
           END-IF
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "1006"      TO  SPA-ERRCD
               GO  TO  4111-PRINT-EXT
           END-IF
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    "OR102ER2"          TO  ERRFLPARA-FILE-ID
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF 
      *
      *    印刷用スクリプト処理
      *
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    1               TO  LNK-PRTKANRI-RENNUM
           MOVE    CONST-TBL-KEY   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    SMCNDATE-YMD    TO  LNK-PRTKANRI-TBL-GROUP(1:8)
           MOVE    SMCNDATE-HMS    TO  LNK-PRTKANRI-TBL-GROUP(9:)
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "execoprg.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPA-NAI-TBL-GROUP
      *
           PERFORM 800-JOBKANRI-JBS-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
      *    ファイルデイレクトリ位置指定
           INITIALIZE              ORCSMKPASSAREA
           MOVE    "scripts/daily/execoprg.sh"
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    LNK-PRTKANRI-RENNUM
                                  TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                  TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                  TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                  TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                  TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                  TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                  TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                  TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                  TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                  TO  SHELLTBL-ARG11
      *
           MOVE    WRK-JOBID      TO  SHELLTBL-ARG12
           MOVE    CONST-SHELLID  TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM    TO  SHELLTBL-ARG14
           MOVE    ERRFLPARA      TO  SHELLTBL-ARG15
           MOVE    "ORCBZ01"      TO  SHELLTBL-ARG16
           MOVE    SPA-NAI-YUKOYMD
                                  TO  SHELLTBL-ARG17
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "shell"         TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"           USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    印刷処理
           IF    ( SPA-ZID1-FLG        =   "OK"  OR "PV" )
      *
               PERFORM 800-JOBKANRI-JBS-SEC
      *
               MOVE    WRK-JOBID       TO  SPA-NAI-JOB-JOBID
      *
               MOVE    0               TO  WRK-STPAGE
               MOVE    99999           TO  WRK-EDPAGE
      *
               MOVE    SPACE           TO  SHELLTBL
      *        ファイルデイレクトリ位置指定
               INITIALIZE              ORCSMKPASSAREA
               MOVE    "scripts/allways/print_prv.sh"
                                       TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"    USING
                                       ORCSMKPASSAREA
               MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
               MOVE    SPACE           TO  LNK-PRTKANRI-AREA
               INITIALIZE                  LNK-PRTKANRI-AREA
               MOVE    CONST-TBL-KEY   TO  LNK-PRTKANRI-TBL-KEY 
               MOVE    SMCNDATE-YMD    TO  LNK-PRTKANRI-TBL-GROUP(1:8)
               MOVE    SMCNDATE-HMS    TO  LNK-PRTKANRI-TBL-GROUP(9:)
               MOVE    ZERO            TO  LNK-PRTKANRI-RENNUM
               MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
               MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
               MOVE    "print_prv.sh"  TO  LNK-PRTKANRI-SHELLID
               MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
               MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
               MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
               MOVE    LNK-PRTKANRI-RENNUM
                                       TO  SHELLTBL-ARG1
               MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
               MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
               MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
               MOVE    LNK-PRTKANRI-SRYYM
                                       TO  SHELLTBL-ARG5
               MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SHELLTBL-ARG6
               MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
               MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
               MOVE    LNK-PRTKANRI-TERMID
                                       TO  SHELLTBL-ARG9
               MOVE    LNK-PRTKANRI-OPID
                                       TO  SHELLTBL-ARG10
               MOVE    LNK-PRTKANRI-PRTNM
                                       TO  SHELLTBL-ARG11
      *
               MOVE    ERRFLPARA       TO  SHELLTBL-ARG12
               MOVE    WRK-JOBID       TO  SHELLTBL-ARG13
               MOVE    CONST-SHELLID   TO  SHELLTBL-ARG14
               MOVE    WRK-STPAGE      TO  SHELLTBL-ARG15
               MOVE    WRK-EDPAGE      TO  SHELLTBL-ARG16
               EVALUATE    SPA-ZID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
      *----(05.00.00)--ADD-START---
               WHEN    "OK"
                   IF      SPA-CLIENT-PRT-FLG  =  2
                       MOVE    "5"         TO  SHELLTBL-ARG17
                   ELSE
                       MOVE    "2"         TO  SHELLTBL-ARG17
                   END-IF
               END-EVALUATE
               MOVE    SPA-CLIENT-SHOWDIALOG
                                           TO  SHELLTBL-ARG22
      *----(05.00.00)--ADD-END-----
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
               MOVE    SPACE               TO  SHELLTBL-ARG19
               MOVE    SPACE               TO  SHELLTBL-ARG20
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "shell"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 800-JOBID-UPD-SEC
      *     
           PERFORM 490-KAKUNIN-SEC
      *
           .
       4111-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           INITIALIZE                      SPA-Z97-JOB-AREA
           MOVE    CONST-3003-KBNCD    TO  SPA-Z97-JOB-KBNCD
           MOVE    CONST-SHELLID       TO  SPA-Z97-JOB-SHELLID
           MOVE    SPA-NAI-JOB-JOBID   TO  SPA-Z97-JOB-JOBID
      *
           MOVE    SPACE               TO  Z97
           INITIALIZE                      Z97
           MOVE    ZERO                TO  Z97-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  Z97-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *
           MOVE    5                   TO  Z97-DURATION
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "3003"              TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC
           ELSE
               INITIALIZE                  SYS-3003-REC
           END-IF
      *
           PERFORM VARYING WRK-JOBID   FROM    1   BY  1
                   UNTIL ( WRK-JOBID   >   SYS-3003-JOBID )
      *
      *        ジョブ管理チェック処理
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-JOBID       TO  JOB-JOBID
               MOVE    CONST-SHELLID   TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA  
                                       JOBKANRI-REC 
                                       SPA-AREA
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   MOVE    JOB-JOBID       TO  Z97-RENNUM   (WRK-JOBID)
                   MOVE    JOB-SHELLMSG    TO  Z97-SHORINM  (WRK-JOBID)
                   MOVE    JOB-STARTTIME(1:2)
                                           TO  WRK-HH
                   MOVE    JOB-STARTTIME(3:2)
                                           TO  WRK-MM
                   MOVE    JOB-STARTTIME(5:2)
                                           TO  WRK-SS
                   MOVE    WRK-HENTIME     TO  Z97-STARTTIME(WRK-JOBID)
                   MOVE    JOB-ENDTIME(1:2)
                                           TO  WRK-HH
                   MOVE    JOB-ENDTIME(3:2)
                                           TO  WRK-MM
                   MOVE    JOB-ENDTIME(5:2)
                                           TO  WRK-SS
                   MOVE    WRK-HENTIME     TO  Z97-ENDTIME  (WRK-JOBID)
                   MOVE    JOB-UPDCNT      TO  WRK-PAGE
                   MOVE    WRK-PAGE        TO  Z97-PAGE     (WRK-JOBID)
                   MOVE    JOB-YOBI        TO  Z97-ERRMSG   (WRK-JOBID)
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "処理中です"    TO  Z97-MSG
                   ELSE
                       IF      JOB-ERRCD       =   SPACE
                           CONTINUE
                       ELSE
                           MOVE    SPACE           TO  Z97-MSG
                           STRING  JOB-YOBI        DELIMITED  BY  SPACE
                                   "【"            DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE
                           INTO  Z97-MSG
                           END-STRING
                       END-IF
                   END-IF
                   MOVE    WRK-JOBID       TO  Z97-COUNT
                                               SPA-Z91-JOB-JOBID
               END-IF
           END-PERFORM
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( Z97-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( Z97-COUNT       >   ZERO )
                   MOVE    "処理は正常に終了しました"
                                               TO  Z97-MSG
               ELSE
                   MOVE    "該当する処理はありません"
                                               TO  Z97-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-ZID1-FLG    =   "PV" )
               MOVE    "F12:プレビュー"    TO  Z97-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                   MOVE    WIDGET-NORMAL   TO  Z97-B12-STATE
                   MOVE    "B12"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "Z07"                TO  SPA-MOTOPG
                                            SPA-Z97-MOTOPG
           MOVE    "Z97"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "Z97"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC           SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "Z07"           TO  SPA-MOTOPG
           MOVE    "XC01"          TO  SPA-SAKIPG
      *
           MOVE    0               TO  WRK-STPAGE
           MOVE    0               TO  WRK-EDPAGE
      *
           INITIALIZE                  LNK-XC01-AREA
      *
           MOVE    CONST-TBL-KEY       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    ZERO                TO  LNK-XC01-RENNUM
           MOVE    SPA-NAI-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    ZERO                TO  LNK-XC01-SHORI-RENNUM
           MOVE    WRK-STPAGE          TO  LNK-XC01-ST-PAGE
           MOVE    WRK-EDPAGE          TO  LNK-XC01-ED-PAGE
           MOVE    "Z07"               TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-Z01FREE         TO  LNK-SCRSPA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        診療コード変更
               IF      SPA-GMN-SRYCD       =   SPACE
                   MOVE    "0014"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-Z07-CUR
               ELSE
      *        点数マスタ存在チェック
                   PERFORM 40041-TENSU-CHK-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4121-KOUSIN-SYORI-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-CHKCHG      =   "1" )
                   PERFORM 4122-CHKMST-KOUSIN-SYORI-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-YKJCHG      =   "1" )
                   PERFORM 4123-YKJMST-KOUSIN-SYORI-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               IF    ( SPA-GMN-PTKINKICHG  =   "1" )
                   PERFORM 4123-PTKINKI-KOUSIN-SYORI-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        再表示へ
               PERFORM 3101-TENSU-HENSYU-SEC
               MOVE    SPACE               TO  SPA-ERRCD
      *
               INITIALIZE                      SPA-GMN-IN-AREA
               INITIALIZE                      SPA-GMN-INPUTCD-G
               MOVE    2                   TO  SPA-GMN-Z07-CUR
           END-IF
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード登録処理
      *****************************************************************
       4121-KOUSIN-SYORI-SEC          SECTION.
      *
      *    更新日取得
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        入力コード検索
               MOVE    SPACE               TO  INPUTCD-REC
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-GMN-INPUTCD (IDX)
                                           TO  ICD-INPUTCD
               MOVE    SPA-GMN-CDSYU   (IDX)
                                           TO  ICD-CDSYU
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 920-INPUTCD-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        入力コード更新
               IF      (FLG-INPUTCD        =   ZERO )  AND
                       (SPA-GMN-OLD-SRYCD  =   ICD-SRYCD )
      *            診療コード変更
                   MOVE    SPA-GMN-SRYCD       TO  ICD-SRYCD
                   MOVE    IDX                 TO  ICD-DSPSEQ
      *
                   MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
                   MOVE    INPUTCD-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0100"          TO  SPA-ERRCD
                       DISPLAY "Z07 INPUTCD UPD ERR:" MCP-RC
                               ",KEY:" ICD-KEY
                   END-IF
               ELSE
                   IF      FLG-INPUTCD     =   ZERO
                       MOVE    "0101"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        入力コード追加
               IF       FLG-INPUTCD        =   1
                   MOVE    SPACE               TO  INPUTCD-REC
                   MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
                   MOVE    SPA-GMN-INPUTCD (IDX)
                                               TO  ICD-INPUTCD
                   MOVE    SPA-GMN-CDSYU   (IDX)
                                               TO  ICD-CDSYU
                   MOVE    SPA-GMN-SRYCD       TO  ICD-SRYCD
                   MOVE    IDX                 TO  ICD-DSPSEQ
      *
                   MOVE    SMCNDATE-YMD        TO  ICD-CREYMD
                   MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
                   MOVE    INPUTCD-REC         TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_inputcd"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0100"          TO  SPA-ERRCD
                       DISPLAY "Z07 INPUTCD INS ERR:" MCP-RC
                               ",KEY:" ICD-KEY
                   END-IF
               END-IF
      *
           END-PERFORM
      *
      *    前の診療コードの連番付け直し
           PERFORM 41211-OLD-INPUTCD-SEC
      *
           .
       4121-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックマスタ更新処理
      *****************************************************************
       4122-CHKMST-KOUSIN-SYORI-SEC    SECTION.
      *
           MOVE    "1"             TO  WRK-ZOGENPTN
           MOVE    1               TO  WRK-ZOGEN
           MOVE    SPA-NAI-YUKOYMD TO  WRK-SYMD
           PERFORM 800-CALENDAR-SEC
           MOVE    WRK-SYMD        TO  WRK-NEXTDAY
      *
           PERFORM 41221-CHKMST-SRYCD-KOUSIN-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41221-CHKMST-KOUSIN-SYORI-SEC
               PERFORM 41222-CHKMST-TEKCD-KOUSIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41223-CHKSND-SRYCD-KOUSIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41221-CHKMST-KOUSIN-SYORI-SEC
               PERFORM 41224-CHKSND-TEKCD-KOUSIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41225-CHKTRD-SRYCD-KOUSIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41221-CHKMST-KOUSIN-SYORI-SEC
               PERFORM 41226-CHKTRD-TEKCD-KOUSIN-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 41227-CHK005-TEKCD-KOUSIN-SEC
           END-IF
      *
           .
       4122-CHKMST-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ更新処理
      *****************************************************************
       41221-CHKMST-KOUSIN-SYORI-SEC   SECTION.
      *
           MOVE    SPACE       TO  WRK-CHKKBN
           MOVE    SPACE       TO  WRK-SEL-SRYCD
           MOVE    SPA-GMN-OLD-SRYCD
                               TO  WRK-OLD-SRYCD
           MOVE    SPA-GMN-SRYCD
                               TO  WRK-NEW-SRYCD
      *
           .
       41221-CHKMST-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ更新処理（診療行為コード）
      *****************************************************************
       41221-CHKMST-SRYCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHKMST-KEY18-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 900-CHKMST-KEY6-SEL-SEC
               IF    ( FLG-TCHKMST     =   ZERO )
                   CONTINUE
               ELSE
      *
                   MOVE    SMCNDATE-YMD    TO  CHK-CREYMD
                                               CHK-UPYMD
                   MOVE    SMCNDATE-HMS    TO  CHK-UPHMS
                   MOVE    CHK-SRYCD       TO  CHK-UPSRYCD
                   MOVE    CHK-YUKOSTYMD   TO  CHK-UPYUKOSTYMD
                   MOVE    CHK-YUKOEDYMD   TO  CHK-UPYUKOEDYMD
                   MOVE    SPA-GMN-SRYCD   TO  CHK-SRYCD
                   MOVE    "00000000"      TO  CHK-YUKOSTYMD
                   MOVE    "99999999"      TO  CHK-YUKOEDYMD
                   MOVE    CHK-REC         TO  MCPDATA-REC
                   MOVE    "tbl_chk"       TO  MCP-TABLE
                   MOVE    "ins1"          TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0200"      TO  SPA-ERRCD
                   END-IF
      *
                   PERFORM 900-CHKMST-KEY2-SEL-SEC
      *
                   PERFORM UNTIL ( FLG-TCHKMST NOT =   ZERO )
                            OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                       MOVE    CHK-CD      TO  WRK-SRYCD
                       PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SRYCDCHG    =   ZERO )
                           MOVE    CHK-CHKKBN
                                           TO  WRK-CHKKBN
                           MOVE    SPA-GMN-SRYCD
                                           TO  WRK-SEL-SRYCD
                           MOVE    CHG-IPNSRYCD
                                           TO  WRK-OLD-SRYCD
                           MOVE    CHG-RJNSRYCD
                                           TO  WRK-NEW-SRYCD
                           PERFORM 41222-CHKMST-TEKCD-KOUSIN-SEC
                       END-IF
      *
                       PERFORM 900-CHKMST-KEY2-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_chk"       TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
               END-IF
      *
               PERFORM 900-CHKMST-KEY18-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk"       TO  MCP-TABLE
           MOVE    "key18"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41221-CHKMST-SRYCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ更新処理（適用コード）
      *****************************************************************
       41222-CHKMST-TEKCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHKMST-KEY22-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    CHK-SRYCD       TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
      *
                   PERFORM 900-CHKMST-KEY-SEL-SEC
                   IF    ( FLG-TCHKMST     =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM 900-CHKMST-KEY20-SEL-SEC
                       MOVE    SMCNDATE-YMD    TO  CHK-CREYMD
                                                   CHK-UPYMD
                       MOVE    SMCNDATE-HMS    TO  CHK-UPHMS
                       MOVE    WRK-NEW-SRYCD   TO  CHK-CD
                       COMPUTE CHK-RENNUM      =   TCHK-RENNUM +   1
                       MOVE    CHK-REC         TO  MCPDATA-REC
                       MOVE    "tbl_chk"       TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC          NOT =   ZERO )
                           MOVE    "0201"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-CHKMST-KEY22-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk"       TO  MCP-TABLE
           MOVE    "key22"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41222-CHKMST-TEKCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２更新処理（診療行為コード）
      *****************************************************************
       41223-CHKSND-SRYCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHKSND-KEY19-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 900-CHKSND-KEY30-SEL-SEC
               IF    ( FLG-TCHKSND     =   ZERO )
                   CONTINUE
               ELSE
      *
                   MOVE    SMCNDATE-YMD        TO  CHKSND-CREYMD
                                                   CHKSND-UPYMD
                   MOVE    SMCNDATE-HMS        TO  CHKSND-UPHMS
                   MOVE    CHKSND-CHKKBN       TO  CHKSND-UPCHKSNDKBN
                   MOVE    CHKSND-SRYCD        TO  CHKSND-UPSRYCD
                   MOVE    CHKSND-YUKOSTYMD    TO  CHKSND-UPYUKOSTYMD
                   MOVE    CHKSND-YUKOEDYMD    TO  CHKSND-UPYUKOEDYMD
                   MOVE    SPA-GMN-SRYCD       TO  CHKSND-SRYCD
                   MOVE    "00000000"          TO  CHKSND-YUKOSTYMD
                   MOVE    "99999999"          TO  CHKSND-YUKOEDYMD
                   MOVE    CHKSND-REC          TO  MCPDATA-REC
                   MOVE    "tbl_chksnd"        TO  MCP-TABLE
                   MOVE    "ins1"              TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0202"          TO  SPA-ERRCD
                   END-IF
      *
                   PERFORM 900-CHKSND-KEY3-SEL-SEC
      *
                   PERFORM UNTIL ( FLG-TCHKSND NOT =   ZERO )
                            OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                       MOVE    CHKSND-CD   TO  WRK-SRYCD
                       PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SRYCDCHG    =   ZERO )
                           MOVE    CHKSND-CHKKBN
                                           TO  WRK-CHKKBN
                           MOVE    SPA-GMN-SRYCD
                                           TO  WRK-SEL-SRYCD
                           MOVE    CHG-IPNSRYCD
                                           TO  WRK-OLD-SRYCD
                           MOVE    CHG-RJNSRYCD
                                           TO  WRK-NEW-SRYCD
                           PERFORM 41224-CHKSND-TEKCD-KOUSIN-SEC
                       END-IF
      *
                       PERFORM 900-CHKSND-KEY3-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_chksnd"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
               END-IF
      *
               PERFORM 900-CHKSND-KEY19-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key19"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41223-CHKSND-SRYCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２更新処理（適用コード）
      *****************************************************************
       41224-CHKSND-TEKCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHKSND-KEY32-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    CHKSND-SRYCD    TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
      *
                   PERFORM 900-CHKSND-KEY22-SEL-SEC
                   IF    ( FLG-TCHKSND     =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM 900-CHKSND-KEY21-SEL-SEC
                       MOVE    SMCNDATE-YMD    TO  CHKSND-CREYMD
                                                   CHKSND-UPYMD
                       MOVE    SMCNDATE-HMS    TO  CHKSND-UPHMS
                       MOVE    WRK-NEW-SRYCD   TO  CHKSND-CD
                       COMPUTE CHKSND-RENNUM   =   TCHKSND-RENNUM +   1
                       MOVE    CHKSND-REC      TO  MCPDATA-REC
                       MOVE    "tbl_chksnd"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC          NOT =   ZERO )
                           MOVE    "0203"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-CHKSND-KEY32-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key32"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41224-CHKSND-TEKCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（診療行為コード）
      *****************************************************************
       41225-CHKTRD-SRYCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHKTRD-KEY19-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 900-CHKTRD-KEY6-SEL-SEC
               IF    ( FLG-TCHKTRD     =   ZERO )
                   CONTINUE
               ELSE
      *
                   MOVE    SMCNDATE-YMD        TO  CHKTRD-CREYMD
                                                   CHKTRD-UPYMD
                   MOVE    SMCNDATE-HMS        TO  CHKTRD-UPHMS
                   MOVE    CHKTRD-CHKKBN       TO  CHKTRD-UPCHKTRDKBN
                   MOVE    CHKTRD-SRYCD        TO  CHKTRD-UPSRYCD
                   MOVE    CHKTRD-YUKOSTYMD    TO  CHKTRD-UPYUKOSTYMD
                   MOVE    CHKTRD-YUKOEDYMD    TO  CHKTRD-UPYUKOEDYMD
                   MOVE    SPA-GMN-SRYCD       TO  CHKTRD-SRYCD
                   MOVE    "00000000"          TO  CHKTRD-YUKOSTYMD
                   MOVE    "99999999"          TO  CHKTRD-YUKOEDYMD
                   MOVE    CHKTRD-REC          TO  MCPDATA-REC
                   MOVE    "tbl_chktrd"        TO  MCP-TABLE
                   MOVE    "ins1"              TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0204"          TO  SPA-ERRCD
                   END-IF
      *
                   PERFORM 900-CHKTRD-KEY3-SEL-SEC
      *
                   PERFORM UNTIL ( FLG-TCHKTRD NOT =   ZERO )
                            OR   ( SPA-ERRCD   NOT =   SPACE )
      *
                       MOVE    CHKTRD-CD       TO  WRK-SRYCD
                       PERFORM 900-SRYCDCHG-KEY2-SEL-SEC
                       IF    ( FLG-SRYCDCHG    =   ZERO )
                           MOVE    CHKTRD-CHKKBN
                                           TO  WRK-CHKKBN
                           MOVE    SPA-GMN-SRYCD
                                           TO  WRK-SEL-SRYCD
                           MOVE    CHG-IPNSRYCD
                                           TO  WRK-OLD-SRYCD
                           MOVE    CHG-RJNSRYCD
                                           TO  WRK-NEW-SRYCD
                           PERFORM 41226-CHKTRD-TEKCD-KOUSIN-SEC
                       END-IF
      *
                       PERFORM 900-CHKTRD-KEY3-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_chktrd"    TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
               END-IF
      *
               PERFORM 900-CHKTRD-KEY19-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chktrd"    TO  MCP-TABLE
           MOVE    "key19"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41225-CHKTRD-SRYCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３更新処理（適用コード）
      *****************************************************************
       41226-CHKTRD-TEKCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHKTRD-KEY23-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    WRK-NEXTDAY     TO  WRK-SYMD
               MOVE    CHKTRD-SRYCD    TO  WRK-SRYCD
               PERFORM 901-TENSU-KEY-SEL-SEC
               IF    ( FLG-TENSU       =   ZERO )
      *
                   PERFORM 900-CHKTRD-KEY22-SEL-SEC
                   IF    ( FLG-TCHKTRD     =   ZERO )
                       CONTINUE
                   ELSE
                       PERFORM 900-CHKTRD-KEY21-SEL-SEC
                       MOVE    SMCNDATE-YMD    TO  CHKTRD-CREYMD
                                                   CHKTRD-UPYMD
                       MOVE    SMCNDATE-HMS    TO  CHKTRD-UPHMS
                       MOVE    WRK-NEW-SRYCD   TO  CHKTRD-CD
                       COMPUTE CHKTRD-RENNUM   =   TCHKTRD-RENNUM +   1
                       MOVE    CHKTRD-REC      TO  MCPDATA-REC
                       MOVE    "tbl_chktrd"    TO  MCP-TABLE
                       MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM 910-DBINSERT-SEC
                       IF    ( MCP-RC          NOT =   ZERO )
                           MOVE    "0205"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-CHKTRD-KEY23-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chktrd"    TO  MCP-TABLE
           MOVE    "key23"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41226-CHKTRD-TEKCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５更新処理（適用コード）
      *****************************************************************
       41227-CHK005-TEKCD-KOUSIN-SEC   SECTION.
      *
           PERFORM 900-CHK005-KEY8-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               PERFORM 900-CHK005-KEY9-SEL-SEC
               IF    ( FLG-TCHK005     =   ZERO )
                   CONTINUE
               ELSE
                   PERFORM 900-CHK005-KEY10-SEL-SEC
                   MOVE    SMCNDATE-YMD    TO  CHK005-CREYMD
                                               CHK005-UPYMD
                   MOVE    SMCNDATE-HMS    TO  CHK005-UPHMS
                   MOVE    SPA-GMN-SRYCD   TO  CHK005-SRYCD
                   COMPUTE CHK005-RENNUM   =   TCHK005-RENNUM +   1
                   MOVE    CHK005-REC      TO  MCPDATA-REC
                   MOVE    "tbl_chk005"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0206"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               PERFORM 900-CHK005-KEY8-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chk005"    TO  MCP-TABLE
           MOVE    "key8"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41227-CHK005-TEKCD-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ更新処理
      *****************************************************************
       4123-YKJMST-KOUSIN-SYORI-SEC    SECTION.
      *
           PERFORM 900-YAKUJYO-KEY-SEL-SEC
           IF    ( FLG-YAKUJYO     =   ZERO )
               PERFORM 901-YAKUJYO-KEY-SEL-SEC
               IF    ( FLG-TYAKUJYO    =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-SRYCD   TO  YAKUJYO-SRYCD
                   MOVE    SPACE           TO  YAKUJYO-NAME
                                               YAKUJYO-TERMID
                                               YAKUJYO-OPID
                   MOVE    SMCNDATE-YMD    TO  YAKUJYO-CREYMD
                                               YAKUJYO-UPYMD
                   MOVE    SMCNDATE-HMS    TO  YAKUJYO-UPHMS
                   MOVE    YAKUJYO-REC     TO  MCPDATA-REC
                   MOVE    "tbl_yakujyo"   TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0300"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4123-YKJMST-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤更新処理
      *****************************************************************
       4123-PTKINKI-KOUSIN-SYORI-SEC    SECTION.
      *
           PERFORM 900-PTKINKI-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTKINKI NOT =   ZERO )
      *
               PERFORM 901-PTKINKI-KEY-SEL-SEC
               IF    ( FLG-TPTKINKI    =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-SRYCD   TO  PTKINKI-SRYCD
                   MOVE    SPACE           TO  PTKINKI-TERMID
                   MOVE    SPA-OPID        TO  PTKINKI-OPID
                   MOVE    SMCNDATE-YMD    TO  PTKINKI-CREYMD
                                               PTKINKI-UPYMD
                   MOVE    SMCNDATE-HMS    TO  PTKINKI-UPHMS
                   MOVE    PTKINKI-REC     TO  MCPDATA-REC
                   MOVE    "tbl_ptkinki"   TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-DBINSERT-SEC
                   IF    ( MCP-RC          NOT =   ZERO )
                       MOVE    "0400"      TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               PERFORM 900-PTKINKI-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4123-PTKINKI-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    前の診療コードの連番付け直し処理
      *****************************************************************
       41211-OLD-INPUTCD-SEC           SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL    FLG-INPUTCD    =   1
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  ICD-DSPSEQ
      *
               MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
               MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "0100"          TO  SPA-ERRCD
                   DISPLAY "Z07 INPUTCD UPD ERR:" MCP-RC
                           ",KEY:" ICD-KEY
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-READ-SEC
      *
           END-PERFORM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41211-OLD-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               EVALUATE    SPA-ZIDCD
               WHEN    "1001"
                   PERFORM 540-ZID3SET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               WHEN    OTHER
                   PERFORM 520-ZIDSET-SEC
                   GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z07    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    Z07-TENSU-ROW       TO  WRK-ROW
      *
           MOVE    SPACE               TO  Z07
           INITIALIZE                      Z07
      *
           IF    ( FLG-ROW             =   1 )
              MOVE    ZERO             TO  Z07-TENSU-ROW
           ELSE
              MOVE    WRK-ROW          TO  Z07-TENSU-ROW
           END-IF
      *
           MOVE    SPA-GMN-YUKOYMD     TO  Z07-YUKOYMD
           MOVE    SPA-GMN-SELNUM      TO  Z07-SELNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  Z07-OLDSRYCD
           MOVE    SPA-GMN-OLD-NAME    TO  Z07-OLDNAME
           MOVE    SPA-GMN-SRYCD       TO  Z07-SRYCD
           MOVE    SPA-GMN-NAME        TO  Z07-NAME
      *
           MOVE    SPA-GMN-INPUTCD(01) TO  Z07-INPUTCD01
           MOVE    SPA-GMN-INPUTCD(02) TO  Z07-INPUTCD02
           MOVE    SPA-GMN-INPUTCD(03) TO  Z07-INPUTCD03
           MOVE    SPA-GMN-INPUTCD(04) TO  Z07-INPUTCD04
           MOVE    SPA-GMN-INPUTCD(05) TO  Z07-INPUTCD05
           MOVE    SPA-GMN-INPUTCD(06) TO  Z07-INPUTCD06
           MOVE    SPA-GMN-INPUTCD(07) TO  Z07-INPUTCD07
           MOVE    SPA-GMN-INPUTCD(08) TO  Z07-INPUTCD08
           MOVE    SPA-GMN-INPUTCD(09) TO  Z07-INPUTCD09
           MOVE    SPA-GMN-INPUTCD(10) TO  Z07-INPUTCD10
      *
           MOVE    SPA-GMN-CHKCHG-G    TO  Z07-CHKCHG
           MOVE    SPA-GMN-CHKCHG-MAX  TO  Z07-CHKCHG-COUNT
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-GMN-CHKCHG-MAX
      *
               MOVE    SPA-GMN-CHKCHGLST (IDX)
                                       TO  Z07-CHKCHG-ITEM (IDX)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-YKJCHG-G    TO  Z07-YKJCHG
           MOVE    SPA-GMN-YKJCHG-MAX  TO  Z07-YKJCHG-COUNT
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-GMN-YKJCHG-MAX
      *
               MOVE    SPA-GMN-YKJCHGLST (IDX)
                                       TO  Z07-YKJCHG-ITEM (IDX)
      *
           END-PERFORM
      *
           MOVE    SPA-GMN-PTKINKICHG-G    TO  Z07-PTKINKICHG
           MOVE    SPA-GMN-PTKINKICHG-MAX  TO  Z07-PTKINKICHG-COUNT
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-GMN-PTKINKICHG-MAX
      *
               MOVE    SPA-GMN-PTKINKICHGLST (IDX)
                                       TO  Z07-PTKINKICHG-ITEM (IDX)
      *
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY   1
                   UNTIL       IDX     >   SPA-TENSU-MAX
               MOVE    SPA-GMN-TNUM (IDX)      TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  Z07-TNUM  (IDX)
               MOVE    SPA-GMN-TINPUTCD(IDX)   TO  Z07-TINPUTCD (IDX)
               MOVE    SPA-GMN-TSRYCD  (IDX)   TO  Z07-TSRYCD  (IDX)
               MOVE    SPA-GMN-TNAME   (IDX)   TO  Z07-TNAME  (IDX)
               MOVE    SPA-GMN-TYUKOSTYMD (IDX)
                                               TO  Z07-TYUKOSTYMD(IDX)
               MOVE    SPA-GMN-TYUKOEDYMD (IDX)
                                               TO  Z07-TYUKOEDYMD(IDX)
               IF      SPA-GMN-SELNUM          =   IDX
                   MOVE    "T"             TO  Z07-TENSU-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  Z07-TENSU-SELECT (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-TENSU-MAX       TO  Z07-TENSU-COUNT
      *
      *    次頁あり
           IF      SPA-GMN-NEXT        =   1
               MOVE    "次ページ有"        TO  Z07-NEXT-MSG-VALUE
               MOVE    "red"               TO  Z07-NEXT-MSG-STYLE
           ELSE
               MOVE    SPACE               TO  Z07-NEXT-MSG-VALUE
               MOVE    SPACE               TO  Z07-NEXT-MSG-STYLE
           END-IF
      *
      *    カーソルセット
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-Z07-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-Z07-CUR
               WHEN    01
                   MOVE    "YUKOYMD"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYCD"         TO  MCP-WIDGET
               WHEN    04
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    SPA-NAI-CURIDX  TO  MCP-WIDGET(8:2)
               WHEN    05
                   MOVE    "CHKCHG"        TO  MCP-WIDGET
               WHEN    06
                   MOVE    "YKJCHG"        TO  MCP-WIDGET
               WHEN    07
                   MOVE    "PTKINKICHG"    TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       50011-CUR-SET-SEC              SECTION. 
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOYMD"
                   MOVE    2               TO  SPA-GMN-Z07-CUR
               WHEN    "SELNUM"
                   MOVE    3               TO  SPA-GMN-Z07-CUR
               WHEN    "SRYCD"
                   MOVE    4               TO  SPA-GMN-Z07-CUR
               WHEN    "CHKCHG"
                   MOVE    6               TO  SPA-GMN-Z07-CUR
               WHEN    "YKJCHG"
                   MOVE    7               TO  SPA-GMN-Z07-CUR
               WHEN    "PTKINKICHG"
                   MOVE    12              TO  SPA-GMN-Z07-CUR
               WHEN    OTHER
                   IF      WRK-MCP-WIDGET(1:7)    =   "INPUTCD"
                       MOVE    WRK-MCP-WIDGET(8:2)
                                           TO  SPA-NAI-CURIDX
                       IF      SPA-NAI-CURIDX         =   10
                           MOVE    5       TO  SPA-GMN-Z07-CUR
                       ELSE
                           ADD     1       TO  SPA-NAI-CURIDX
                           MOVE    4       TO  SPA-GMN-Z07-CUR
                       END-IF
                   ELSE
                       MOVE    1           TO  SPA-GMN-Z07-CUR
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効日付入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "対象のデータがありません"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象が２００件以上あります"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "選択番号がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "全角・半角の混在はできません"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "点数マスタが存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "既に自院コードが登録されています。"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "漢字コードは５文字までです"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "入力コードは４桁から６桁で入力してください"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "Ｐ，Ｓは使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE
                   "入力コードには使用できない記号があります。"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE
                   "表示入力コードに全角１文字の数字は使用できません"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE
                   "この入力コードは既に別の診療コードで使われています"
                                                 TO  SPA-ERRMSG
               WHEN    "0100"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "別の診療コードで使われています。"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "新診療行為コードを入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "チェックマスタ置換指示入力エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "薬剤情報マスタ置換指示入力エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "患者禁忌薬剤置換指示入力エラー"
                                                 TO  SPA-ERRMSG
      *H31.3
               WHEN    "0022"
                   STRING  "表示入力コードは半角１０文字、"
                           "全角５文字までにして下さい。"
                                         DELIMITED BY SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0023"
                   MOVE    "文字の間に空白は入力できません。"
                                                 TO  SPA-ERRMSG
      *
               WHEN    "0200"  THRU  "0206"
                   MOVE    "チェックマスタ更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0300"
                   MOVE    "薬剤情報マスタ更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0400"
                   MOVE    "患者禁忌薬剤更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "1005"
               WHEN    "1006"
                   MOVE    "印刷前処理に失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-SHORIERR               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "ZERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
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
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
           WHEN    "6666"
               MOVE    "印刷処理中です"
                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z07"                TO  SPA-MOTOPG
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
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       540-ZID3SET-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
           WHEN    "1001"
               MOVE    "経過措置医薬品置換マスタ一覧を印刷します"
                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  ZID3
           INITIALIZE                      ZID3
           MOVE    SPA-ZIDCD           TO  ZID3-ID1CODE
           MOVE    WRK-ZIDMSG          TO  ZID3-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "Z07"               TO  SPA-MOTOPG
           MOVE    "ZID3"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZID3"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       540-ZID3SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
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
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC               SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       800-JOBKANRI-CHK-SEC            SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    "3003"              TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC
           ELSE
               INITIALIZE                  SYS-3003-REC
           END-IF
     *
           PERFORM VARYING WRK-JOBID   FROM    1   BY  1
                   UNTIL ( WRK-JOBID   >   SYS-3003-JOBID )
      *
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-JOBID       TO  JOB-JOBID
               MOVE    CONST-SHELLID   TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
               IF    ( SJOBKANRI-RETURN    =   ZERO )
                   IF    ( JOB-ENDYMD      =   SPACE )
                       MOVE    "7777"          TO  SPA-ERRCD
                       MOVE    SPACE           TO  WRK-SHORIERR
                       STRING  "処理中です【"  DELIMITED  BY  SIZE
                               JOB-SHELLMSG    DELIMITED  BY  SPACE
                               "】"            DELIMITED  BY  SIZE
                       INTO  WRK-SHORIERR
                       END-STRING
                   END-IF
               END-IF
      *
           END-PERFORM
      *
         .
       800-JOBKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理ジョブスタート処理
      *****************************************************************
       800-JOBKANRI-JBS-SEC        SECTION.
      *
           COMPUTE WRK-JOBID   =   WRK-JOBID   +   1
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-JOBID       TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-JOBID     TO  JOB-JOBID
           MOVE    CONST-SHELLID   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
         .
       800-JOBKANRI-JBS-EXT.
           EXIT.
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       800-JOBID-UPD-SEC               SECTION.
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "3003"              TO  SYS-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-KBNCD
           MOVE    "00000000"          TO  SYS-STYUKYMD
           MOVE    "99999999"          TO  SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           MOVE    SYSKANRI-REC        TO  SYS-3003-REC
           MOVE    CONST-TBL-KEY       TO  SYS-3003-TBL-KEY
           MOVE    WRK-JOBID           TO  SYS-3003-JOBID
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
           END-IF
      *
         .
       800-JOBID-UPD-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得処理
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
         .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       910-TENSU-NEXT-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
      *********INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       920-INPUTCD-READ-SEC     SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       990-SRYCDCHG-READ-EXT.
           EXIT.
      *****************************************************************
      *    老人一般コード検索処理
      *****************************************************************
       900-SRYCDCHG-KEY2-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYCDCHG
      *
           INITIALIZE                      SRYCDCHG-REC
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    WRK-SRYCD           TO  CHG-IPNSRYCD
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_srycdchg"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SRYCDCHG-REC
               ELSE
                   MOVE    1           TO  FLG-SRYCDCHG
                   INITIALIZE              SRYCDCHG-REC
               END-IF
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
      *
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SRYCDCHG-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY2-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    WRK-NEXTDAY         TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKMST-KEY2-FET-SEC
           ELSE
               MOVE    1               TO  FLG-TCHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY2-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-TCHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY18-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  CHK-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKMST-KEY18-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY18-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY18-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key18"             TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHK-REC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY18-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    CHK-REC             TO  TCHK-REC
           MOVE    SPA-GMN-SRYCD       TO  TCHK-SRYCD
      *
           MOVE    TCHK-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chk"       TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHK-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKMST
                   INITIALIZE              TCHK-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKMST
           END-IF
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKMST-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY22-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
      *
           INITIALIZE                      CHK-REC
           MOVE    SPA-HOSPNUM         TO  CHK-HOSPNUM
           MOVE    WRK-CHKKBN          TO  CHK-CHKKBN
           MOVE    WRK-SEL-SRYCD       TO  CHK-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHK-YUKOSTYMD
                                           CHK-YUKOEDYMD
           MOVE    WRK-OLD-SRYCD       TO  CHK-CD
           MOVE    CHK-REC             TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKMST-KEY22-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKMST
               INITIALIZE                  CHK-REC
           END-IF
      *
           .
       900-CHKMST-KEY22-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY22-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKMST
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKMST  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chk"           TO  MCP-TABLE
               MOVE    "key22"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHK-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHK-CDKBN       =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHKMST
                   INITIALIZE                  CHK-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHKMST-KEY22-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    CHK-REC             TO  TCHK-REC
           MOVE    WRK-NEW-SRYCD       TO  TCHK-CD
      *
           MOVE    TCHK-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chk"       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHK-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKMST
                   INITIALIZE              TCHK-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKMST
           END-IF
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKMST-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ検索処理
      *****************************************************************
       900-CHKMST-KEY20-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKMST
      *
           MOVE    CHK-REC             TO  TCHK-REC
      *
           MOVE    TCHK-REC            TO  MCPDATA-REC
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chk"       TO  MCP-TABLE
               MOVE    "key20"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHK-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKMST
                   INITIALIZE              TCHK-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKMST
           END-IF
      *
           MOVE    "tbl_chk"           TO  MCP-TABLE
           MOVE    "key20"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKMST-KEY20-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    WRK-NEXTDAY         TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    "3"                 TO  CHKSND-CDKBN
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKSND-KEY3-FET-SEC
           ELSE
               MOVE    1               TO  FLG-TCHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY3-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-TCHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY19-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  CHKSND-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKSND-KEY19-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY19-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY19-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY30-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    CHKSND-REC          TO  TCHKSND-REC
           MOVE    "0"                 TO  TCHKSND-CDKBN
           MOVE    SPA-GMN-SRYCD       TO  TCHKSND-SRYCD
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key30"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHKSND-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKSND
                   INITIALIZE              TCHKSND-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKSND-KEY30-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY32-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-CHKKBN          TO  CHKSND-CHKKBN
           MOVE    WRK-SEL-SRYCD       TO  CHKSND-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    "3"                 TO  CHKSND-CDKBN
           MOVE    WRK-OLD-SRYCD       TO  CHKSND-CD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key32"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKSND-KEY32-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY32-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY32-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chksnd"        TO  MCP-TABLE
               MOVE    "key32"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHKSND-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHKSND-CDKBN    =   "3" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHKSND
                   INITIALIZE                  CHKSND-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHKSND-KEY32-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY22-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    CHKSND-REC          TO  TCHKSND-REC
           MOVE    WRK-NEW-SRYCD       TO  TCHKSND-CD
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key22"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHKSND-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKSND
                   INITIALIZE              TCHKSND-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKSND-KEY22-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKSND-KEY21-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKSND
      *
           MOVE    CHKSND-REC          TO  TCHKSND-REC
      *
           MOVE    TCHKSND-REC         TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chksnd"    TO  MCP-TABLE
               MOVE    "key21"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHKSND-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKSND
                   INITIALIZE              TCHKSND-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKSND
           END-IF
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKSND-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKTRD-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    WRK-NEXTDAY         TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    "2"                 TO  CHKTRD-CDKBN
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKTRD-KEY3-FET-SEC
           ELSE
               MOVE    1               TO  FLG-TCHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ２検索処理
      *****************************************************************
       900-CHKTRD-KEY3-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-TCHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY19-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  CHKTRD-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKTRD-KEY19-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY19-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY19-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKTRD-REC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY19-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    CHKTRD-REC          TO  TCHKTRD-REC
           MOVE    SPA-GMN-SRYCD       TO  TCHKTRD-SRYCD
      *
           MOVE    TCHKTRD-REC         TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chktrd"       TO  MCP-TABLE
               MOVE    "key6"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHKTRD-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKTRD
                   INITIALIZE              TCHKTRD-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKTRD
           END-IF
      *
           MOVE    "tbl_chktrd"           TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKTRD-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY23-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
      *
           INITIALIZE                      CHKTRD-REC
           MOVE    SPA-HOSPNUM         TO  CHKTRD-HOSPNUM
           MOVE    WRK-CHKKBN          TO  CHKTRD-CHKKBN
           MOVE    WRK-SEL-SRYCD       TO  CHKTRD-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHKTRD-YUKOSTYMD
                                           CHKTRD-YUKOEDYMD
           MOVE    WRK-OLD-SRYCD       TO  CHKTRD-CD
           MOVE    CHKTRD-REC          TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHKTRD-KEY23-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHKTRD
               INITIALIZE                  CHKTRD-REC
           END-IF
      *
           .
       900-CHKTRD-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY23-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKTRD
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHKTRD  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chktrd"        TO  MCP-TABLE
               MOVE    "key23"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHKTRD-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHKTRD-CDKBN    =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHKTRD
                   INITIALIZE                  CHKTRD-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHKTRD-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY22-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    CHKTRD-REC          TO  TCHKTRD-REC
           MOVE    WRK-NEW-SRYCD       TO  TCHKTRD-CD
      *
           MOVE    TCHKTRD-REC         TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chktrd"    TO  MCP-TABLE
               MOVE    "key22"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHKTRD-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKTRD
                   INITIALIZE              TCHKTRD-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKTRD
           END-IF
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKTRD-KEY22-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ３検索処理
      *****************************************************************
       900-CHKTRD-KEY21-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHKTRD
      *
           MOVE    CHKTRD-REC          TO  TCHKTRD-REC
      *
           MOVE    TCHKTRD-REC         TO  MCPDATA-REC
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chktrd"    TO  MCP-TABLE
               MOVE    "key21"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHKTRD-REC
               ELSE
                   MOVE    1           TO  FLG-TCHKTRD
                   INITIALIZE              TCHKTRD-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHKTRD
           END-IF
      *
           MOVE    "tbl_chktrd"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHKTRD-KEY21-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY8-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
      *
           INITIALIZE                      CHK005-REC
           MOVE    SPA-HOSPNUM         TO  CHK005-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  CHK005-SRYCD
           MOVE    SPA-NAI-YUKOYMD     TO  CHK005-YUKOSTYMD
                                           CHK005-YUKOEDYMD
           MOVE    CHK005-REC          TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key8"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-CHK005-KEY8-FET-SEC
           ELSE
               MOVE    1               TO  FLG-CHK005
               INITIALIZE                  CHK005-REC
           END-IF
      *
           .
       900-CHK005-KEY8-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY8-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK005
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-CHK005  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_chk005"        TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  CHK005-REC
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( CHK005-CDKBN    =   "2" )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-CHK005
                   INITIALIZE                  CHK005-REC
               END-IF
           END-PERFORM
      *
           .
       900-CHK005-KEY8-FET-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY9-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHK005
      *
           MOVE    CHK005-REC          TO  TCHK005-REC
           MOVE    SPA-GMN-SRYCD       TO  TCHK005-SRYCD
      *
           MOVE    TCHK005-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chk005"    TO  MCP-TABLE
               MOVE    "key9"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHK005-REC
               ELSE
                   MOVE    1           TO  FLG-TCHK005
                   INITIALIZE              TCHK005-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHK005
           END-IF
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHK005-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタ５検索処理
      *****************************************************************
       900-CHK005-KEY10-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-TCHK005
      *
           MOVE    CHK005-REC          TO  TCHK005-REC
      *
           MOVE    TCHK005-REC         TO  MCPDATA-REC
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_chk005"    TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TCHK005-REC
               ELSE
                   MOVE    1           TO  FLG-TCHK005
                   INITIALIZE              TCHK005-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TCHK005
           END-IF
      *
           MOVE    "tbl_chk005"        TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-CHK005-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       900-YAKUJYO-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUJYO
      *
           INITIALIZE                      YAKUJYO-REC
           MOVE    SPA-HOSPNUM         TO  YAKUJYO-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  YAKUJYO-SRYCD
           MOVE    1                   TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC         TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  YAKUJYO-REC
               ELSE
                   MOVE    1           TO  FLG-YAKUJYO
                   INITIALIZE              YAKUJYO-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-YAKUJYO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    薬剤情報マスタ検索処理
      *****************************************************************
       901-YAKUJYO-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TYAKUJYO
      *
           MOVE    YAKUJYO-REC         TO  TYAKUJYO-REC
           MOVE    SPA-GMN-SRYCD       TO  TYAKUJYO-SRYCD
           MOVE    TYAKUJYO-REC        TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TYAKUJYO-REC
               ELSE
                   MOVE    1           TO  FLG-TYAKUJYO
                   INITIALIZE              TYAKUJYO-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TYAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       901-YAKUJYO-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤検索処理
      *****************************************************************
       900-PTKINKI-KEY4-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKINKI
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-GMN-OLD-SRYCD   TO  PTKINKI-SRYCD
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptkinki"   TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  PTKINKI-REC
               ELSE
                   MOVE    1           TO  FLG-PTKINKI
                   INITIALIZE              PTKINKI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKINKI
           END-IF
      *
           .
       900-PTKINKI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤検索処理
      *****************************************************************
       900-PTKINKI-KEY4-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTKINKI
      *
           MOVE    "tbl_ptkinki"   TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  PTKINKI-REC
           ELSE
               MOVE    1           TO  FLG-PTKINKI
               INITIALIZE              PTKINKI-REC
           END-IF
      *
           .
       900-PTKINKI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者禁忌薬剤検索処理
      *****************************************************************
       901-PTKINKI-KEY-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TPTKINKI
      *
           MOVE    PTKINKI-REC         TO  TPTKINKI-REC
           MOVE    SPA-GMN-SRYCD       TO  TPTKINKI-SRYCD
           MOVE    TPTKINKI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptkinki"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  TPTKINKI-REC
               ELSE
                   MOVE    1           TO  FLG-TPTKINKI
                   INITIALIZE              TPTKINKI-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-TPTKINKI
           END-IF
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       901-PTKINKI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(存在チェックのみで値を返却しない）
      *****************************************************************
       901-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-NEXTDAY         TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   CONTINUE
               ELSE
                   MOVE    1           TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       901-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF    ( MCP-RC      =   ZERO )
                   MOVE    MCPDATA-REC
                                   TO  SYSKANRI-REC
               ELSE
                   INITIALIZE          SYSKANRI-REC
                   MOVE    1       TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルINSERT処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    テーブルDELETE処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
