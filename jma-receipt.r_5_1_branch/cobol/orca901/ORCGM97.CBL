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
       PROGRAM-ID.                 ORCGM97.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : プログラム更新（Ｍ９７）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/01/16    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    NACL-伊藤    04/12/02  画面表示明細数を超えた場合の処置
      *  01.01.02    NACL-伊藤    06/02/22  MONFUNC 対応
      *  03.04.01    NACL-森脇    07/01/24  タイマーイベント追加
      *  03.05.00    NACL-多々納  07/05/28  グループ診療対応
      *  04.06.00    NACL-伊藤    11/03/15  再機動の確認対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    プログラム更新　管理ファイル
           SELECT  PGUP-FILE       ASSIGN  FILE-PGUP
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PGUP.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  PGUP-FILE.
       01  PGUP-REC.
           03  PGUP-TIMES              PIC 9(04).
           03  FILLER                  PIC X(01).
           03  PGUP-UPDATEFLG          PIC X(01).
           03  FILLER                  PIC X(01).
           03  PGUP-RELEASEYMD         PIC X(08).
           03  FILLER                  PIC X(01).
           03  PGUP-COMMENT            PIC X(100).
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPMKPASS.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M901COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-M97.
           03  SPA-M97-AREA.
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-ONKBN   PIC X(01).
                   07  SPA-GMN-OFFKBN  PIC X(01).
      *
                   07  SPA-GMN-OFFNAIYO.
                       09  SPA-GMN-CD         PIC X(01).
                       09  SPA-GMN-F1         PIC X(01).
                       09  SPA-GMN-MEI        PIC X(12).
      *
                   07  SPA-GMN-NAIYOLIST      OCCURS  99.
                       09  SPA-GMN-TEIKYOYMD  PIC X(09).
                       09  SPA-GMN-SYORIFLG   PIC X(08).
                       09  SPA-GMN-NAIYO      PIC X(100).
                   07  SPA-GMN-NAIYOLIST-MAX  PIC 9(02).
      *
                   07  SPA-GMN-MESSEGE.
                       09  SPA-GMN-MSG-STYLE  PIC X(20).
                       09  SPA-GMN-MSG        PIC X(122).
      *
      *----(03.04.01)--ADD-START---
                   07  SPA-GMN-DURATION       PIC 9(01).
      *----(03.04.01)--ADD-END-----
      *
               05  SPA-GMN-OFFNAIYO-AREA.
                   07  SPA-GMN-OFFLIST        OCCURS  2.
                       09  SPA-GMN-CDL        PIC X(01).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-MEIL       PIC X(12).
                   07  SPA-GMN-OFFLIST-MAX    PIC 9(02).
      *
           03  SPA-NAI-KADO-LEVEL      PIC  9(01).
           03  SPA-NAI-CONTRACT-COND   PIC  9(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PGKANRI         PIC 9(01).
           03  FLG-SYSBASE         PIC 9(01).
           03  FLG-ACCESSKEY       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PGKANRI         PIC 9(06).
           03  CNT-SKIP-MAX        PIC 9(06).
           03  CNT-TERMID          PIC 9(03).
      *
       01  STS-AREA.
           03  STS-PGUP            PIC X(02).
      *
       01  FILE-AREA.
           03  FILE-PGUP           PIC X(48)  VALUE
               "/usr/lib/jma-receipt/patch-lib/patch-program.prf".
      *
      *    処理区分領域
       01  SKBN-AREA.
           03  SKBN-RUN            PIC X(03).
           03  MST-RUN             PIC X(03).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH          PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-MIDMSG          PIC X(80).
           03  WRK-SECOND.
               05  WRK-SEC         PIC Z9.
           03  WRK-ENV-OFFLINE     PIC X(03).
      *
           03  WRK-GMN-TBL.
               05  WRK-GMN-NAIYOLIST      OCCURS  99.
                   07  WRK-GMN-TEIKYOYMD  PIC X(09).
                   07  WRK-GMN-SYORIFLG   PIC X(08).
                   07  WRK-GMN-NAIYO      PIC X(100).
               05  WRK-GMN-NAIYOLIST-MAX  PIC 9(02).
      *
       01  SHELL-AREA.
           03  SHELL-NAME1         PIC X(41)  VALUE
               "program_upgrade_online.sh".
           03  SHELL-NAME2         PIC X(42)  VALUE
               "program_upgrade_restart.sh".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システムユーザ
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    ＰＧ管理
      * 01  PGKANRI-REC.
      *     COPY    "CPPGKANRI.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *    ユーザー情報取得
           COPY    "CPORCSLDUSER.INC".
      *
           COPY    "CPACCESSKEY.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M80.INC".
           COPY    "M96.INC".
           COPY    "M97.INC".
           COPY    "MMERR.INC".
           COPY    "MMID1.INC".
           COPY    "MJOB.INC".
           COPY    "MGUIDE1.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M901KYOUTU
           MOVE    SPA-FREE        TO  SPA-M97
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M901KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M97         TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG      =   "MMERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD   NOT     =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "M97"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"   ALSO    "B01"
                   PERFORM 210-BACK
      *        情報削除
               WHEN    "CLICKED"   ALSO    "B02"
                   PERFORM 410-JOBDEL-SEC
      *        プラグイン
               WHEN    "CLICKED"   ALSO    "B04"
                   PERFORM 410-PLUGIN-SEC
      *        一斉通知
               WHEN    "CLICKED"   ALSO    "B05"
                   PERFORM 410-SENDKAKUNIN-SEC
      *        チェック
               WHEN    "CLICKED"   ALSO    "B09"
                   PERFORM 400-CHECK-KAKUNIN-SEC
      *----(03.04.01)---ADD-START---
               WHEN    "CLICKED"   ALSO    "pandatimer1"
      *----(03.04.01)---ADD-END-----
      *        状況
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 400-JYOKYO-SET-SEC
      *        更新
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 410-TERMIDCNT-SEC
      *        オンライン・オフライン
               WHEN    "CLICKED"   ALSO    "ONKBN"
                   MOVE    M97-ONKBN       TO  SPA-GMN-ONKBN
      *             MOVE    2               TO  SPA-GMN-CUR
      *         WHEN    "CLICKED"   ALSO    "OFFKBN"
      *             MOVE    M97-OFFKBN-NAIYO    TO  SPA-GMN-OFFKBN
      *             IF      SPA-GMN-OFFKBN      =   "T"
      *                 MOVE  3                 TO  SPA-GMN-CUR
      *             ELSE
                       MOVE  99                TO  SPA-GMN-CUR
      *             END-IF
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"  ALSO    "OFFNAIYO"
                   PERFORM  400-OFFNAIYO-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 820-JOBKANRI-READ2-SEC
      *
      *    IF      SKBN-RUN   =   "RUN"
      *        MOVE    "0301"          TO  SPA-MMID1CD
      *        PERFORM 520-MIDSET-SEC
      *    ELSE
               PERFORM 830-GAMENN-SET6-SEC
      *    END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
      *    確認画面ＯＫ処理
           EVALUATE    SPA-MOTOPG
               WHEN    "MMID1"
               WHEN    "MGUIDE1"
                   PERFORM 320-MID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    SPACE           TO  SPA-MMID1-FLG
           MOVE    SPACE           TO  SPA-MMID1CD
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
           INITIALIZE              SPA-M97-AREA
      *
           MOVE    "T"             TO  SPA-GMN-ONKBN
           MOVE    "F"             TO  SPA-GMN-OFFKBN
      *
           MOVE    "1"             TO  SPA-GMN-CDL(1)
           MOVE    "フロッピィ"    TO  SPA-GMN-MEIL(1)
           MOVE    "2"             TO  SPA-GMN-CDL(2)
           MOVE    "ＣＤ−ＲＯＭ"  TO  SPA-GMN-MEIL(2)
           MOVE    2               TO  SPA-GMN-OFFLIST-MAX
      *
           MOVE    ZERO            TO  SPA-GMN-NAIYOLIST-MAX
      *
           MOVE    ZERO                TO  SPA-NAI-KADO-LEVEL
           MOVE    ZERO                TO  SPA-NAI-CONTRACT-COND
           IF      SPA-MW              =   SPA-PANDA
               PERFORM 900-ACCESSKEY-READ-SEC
               IF      FLG-ACCESSKEY       =   ZERO
                   MOVE    ACCESS-KEY-KADO-LEVEL
                                           TO  SPA-NAI-KADO-LEVEL
                   MOVE    ACCESS-KEY-CONTRACT-COND
                                           TO  SPA-NAI-CONTRACT-COND
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           PERFORM 400-JYOKYO-SET-SEC
      *
      *----(03.04.01)--UPD-START---
      *     IF      SPA-GMN-NAIYOLIST-MAX   =  ZERO
      *         MOVE    SPACE           TO  SPA-GMN-MSG
      *         MOVE    SPACE           TO  SPA-GMN-MSG-STYLE
      *     END-IF
      *----(03.04.01)--UPD-END-----
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
     *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       320-MID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-MMID1CD
               WHEN    "0200"
                   IF      SPA-MMID1-FLG   =   "OK"
      *            確認処理
                       PERFORM 420-SENDALL-SEC
                   END-IF
               WHEN    "0201"
                   IF      SPA-MMID1-FLG   =   "OK"
      *            確認処理
                       PERFORM 410-KAKUNIN-SEC
                       GO  TO  320-MID1-SET-EXT
                   END-IF
               WHEN    "0202"
                   IF      SPA-MMID1-FLG   =   "OK"
                       PERFORM 820-JOBKANRI-READ3-SEC
                       IF      MST-RUN         =   "RUN"
                           MOVE    "0501"          TO  SPA-ERRCD
                       ELSE
      *                    更新処理
                           PERFORM 420-KOUSHIN-SEC
                       END-IF
                   END-IF
               WHEN    "0204"
               WHEN    "0206"
                   IF      SPA-MMID1-FLG   =   "OK"
      *                削除処理
                       PERFORM 820-JOBKANRI-DEL-SEC
                       PERFORM 400-JYOKYO-SET-SEC
                   END-IF
               WHEN    "0205"
                   IF      SPA-MMID1-FLG   =   "OK"
      *            チェック処理
                       PERFORM 400-CHECK-SEC
                   END-IF
               WHEN    "0301"
                   IF      SPA-MMID1-FLG   =   "OK"
      *                戻る処理
                       PERFORM 830-GAMENN-SET6-SEC
                   END-IF
      ***      WHEN    "0401"
      ***          IF      SPA-MMID1-FLG   =   "OK"
      *            更新処理
      ***              PERFORM 420-RESTART-SEC
      ***          END-IF
               WHEN    "GUD1"
                   MOVE    "0202"              TO  SPA-MMID1CD
                   PERFORM 520-MIDSET-SEC
                   GO  TO  320-MID1-SET-EXT
           END-EVALUATE
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-MMID1-FLG
           MOVE    SPACE           TO  SPA-MMID1CD
           .
      *
       320-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       400-CHECK-KAKUNIN-SEC       SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  400-CHECK-KAKUNIN-EXT
           END-IF
      *
           PERFORM 820-JOBKANRI-READ2-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "0203"          TO  SPA-ERRCD
               PERFORM 510-ERRSET-SEC
           ELSE
               MOVE    "0205"          TO  SPA-MMID1CD
               PERFORM 520-MIDSET-SEC
           END-IF
           .
      *
       400-CHECK-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック
      *****************************************************************
       400-CHECK-SEC               SECTION.
      *
      *    ジョブ管理削除
           PERFORM 820-JOBKANRI-DEL-SEC
      *
      *    ジョブ管理作成
           PERFORM 820-JOBKANRI-JBS-SEC
      *
      *    状況更新
           PERFORM 400-JYOKYO-SET-SEC
      *
      *    チェック処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
      *
      *    オンライン
      *     IF      SPA-GMN-ONKBN   =   "T"
      *         MOVE    SHELL-NAME3     TO  MKPASS-IN-01
      *     END-IF
      *
      *     CALL    "ORCSMKPASS"        USING
      *                                 ORCSMKPASSAREA
      *     MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SHELL-NAME1     TO  SHELLTBL-NAME
           MOVE    "check"         TO  SHELLTBL-ARG1
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    SPA-SUPERVISOR  TO  SHELLTBL-ARG3
           MOVE    SHELLTBL        TO  MCPDATA-REC
      *
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       400-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    状況更新
      *****************************************************************
       400-JYOKYO-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  CNT-PGKANRI
           MOVE    ZERO                TO  FLG-PGKANRI
      *
      *    レコード件数を求める
           OPEN    INPUT   PGUP-FILE
           IF      STS-PGUP        NOT =   ZERO
               MOVE    1                   TO  FLG-PGKANRI
           ELSE
               PERFORM UNTIL   FLG-PGKANRI =   1
                   PERFORM 4001-PROOF-READ-SEC
               END-PERFORM
               MOVE    ZERO                TO  FLG-PGKANRI
           END-IF
           CLOSE   PGUP-FILE
      *
           IF      CNT-PGKANRI         >   99
               COMPUTE CNT-SKIP-MAX        =   CNT-PGKANRI  -  99
           ELSE
               MOVE    ZERO                TO  CNT-SKIP-MAX
           END-IF
      *
      *     プログラム更新
      *     MOVE    SPACE               TO  STS-RESTARTMSG
      *
           MOVE    ZERO                TO  CNT-PGKANRI
           MOVE    ZERO                TO  FLG-PGKANRI
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-GMN-NAIYOLIST-MAX
      *
           OPEN    INPUT   PGUP-FILE
           IF      STS-PGUP        NOT =   ZERO
               MOVE    1                   TO  FLG-PGKANRI
           ELSE
               PERFORM 4001-PROOF-READ-SEC
           END-IF
      *
           PERFORM UNTIL   FLG-PGKANRI =   1
               IF      CNT-PGKANRI         >   CNT-SKIP-MAX
                   COMPUTE IDX                 =   IDX +   1
      *            提供日
                   MOVE    PGUP-RELEASEYMD     TO  WRK-SYMD
                   PERFORM 700-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO
                                               WRK-GMN-TEIKYOYMD(IDX)
      *            更新フラグ
                   EVALUATE    PGUP-UPDATEFLG
                       WHEN    "T"
                           MOVE    "　　済　"      TO
                                                   WRK-GMN-SYORIFLG(IDX)
                       WHEN    "F"
                           MOVE    "　未処理"      TO
                                                   WRK-GMN-SYORIFLG(IDX)
                       WHEN    "E"
                           MOVE    "　エラー"      TO
                                                   WRK-GMN-SYORIFLG(IDX)
                       WHEN    OTHER
                           MOVE    "　　　　"      TO
                                                   WRK-GMN-SYORIFLG(IDX)
                   END-EVALUATE
      *            内容
                   MOVE    PGUP-COMMENT        TO  WRK-GMN-NAIYO(IDX)
               END-IF
               PERFORM 4001-PROOF-READ-SEC
           END-PERFORM
      *
           MOVE    IDX                 TO  WRK-GMN-NAIYOLIST-MAX
           CLOSE   PGUP-FILE
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING IDY     FROM    WRK-GMN-NAIYOLIST-MAX
                                   BY  -1
                   UNTIL   IDY     =   ZERO
               COMPUTE IDX                 =   IDX +   1
               MOVE    WRK-GMN-NAIYOLIST(IDY)  TO
                                           SPA-GMN-NAIYOLIST(IDX)
           END-PERFORM
      *
           MOVE    WRK-GMN-NAIYOLIST-MAX   TO  SPA-GMN-NAIYOLIST-MAX
      *
      *    メッセージ編集
           PERFORM 820-JOBKANRI-READ1-SEC
      *
           IF      SJOBKANRI-RETURN    =   ZERO
               EVALUATE    JOB-ERRCD(1:2)
                   WHEN    "99"
                       MOVE    SPACE           TO  SKBN-RUN
                       MOVE    "プログラム更新は異常終了しました。"
                                               TO  SPA-GMN-MSG
                       MOVE    "red"           TO  SPA-GMN-MSG-STYLE
                   WHEN    "98"
                       MOVE    SPACE           TO  SKBN-RUN
                       MOVE
                           "プログラムチェックは異常終了しました。"
                                               TO  SPA-GMN-MSG
                       MOVE    "red"           TO  SPA-GMN-MSG-STYLE
                   WHEN    "97"
                       MOVE    SPACE           TO  SKBN-RUN
                       STRING  "異常終了 / "   DELIMITED BY SIZE
                               JOB-YOBI(9:)    DELIMITED BY SIZE
                                             INTO  SPA-GMN-MSG
                       END-STRING
                       MOVE    "red"           TO  SPA-GMN-MSG-STYLE
                   WHEN    "01"
                       MOVE    SPACE           TO  SKBN-RUN
                       MOVE    "更新データはありませんでした。"
                                               TO  SPA-GMN-MSG
                       MOVE    "blue"          TO  SPA-GMN-MSG-STYLE
                   WHEN    "51"
                       MOVE    SPACE           TO  SKBN-RUN
                       MOVE    JOB-ERRCD(3:2)  TO  WRK-SEC
                       STRING  WRK-SECOND      DELIMITED BY SIZE
                               " 秒後に日レセを再起動しますので"
                                               DELIMITED BY SIZE
                               "端末を終了してください。"
                                               DELIMITED BY SIZE
                                             INTO  SPA-GMN-MSG
                       END-STRING
                       IF      JOB-ERRCD(3:2)  =   "00"
                           MOVE    "日レセを再起動します。"
                                                   TO  SPA-GMN-MSG
                       END-IF
                       MOVE    "red"           TO  SPA-GMN-MSG-STYLE
                   WHEN    OTHER
                       IF     (JOB-ENDTIME NOT =   SPACE)  AND
                              (JOB-ENDYMD  NOT =   SPACE)
                           MOVE    SPACE           TO  SKBN-RUN
                           MOVE    "処理は正常に終了しました。"
                                                   TO  SPA-GMN-MSG
                           MOVE    "blue"          TO  SPA-GMN-MSG-STYLE
                       ELSE
                           MOVE    "処理中です。"
                                                   TO  SPA-GMN-MSG
                           MOVE    "blue"          TO  SPA-GMN-MSG-STYLE
                       END-IF
               END-EVALUATE
           ELSE
               MOVE    SPACE           TO  SPA-GMN-MSG
               MOVE    SPACE           TO  SPA-GMN-MSG-STYLE
           END-IF
      *
      *----(03.04.01)--ADD-START---
           MOVE    ZERO                TO  SPA-GMN-DURATION
      *
           IF    ( SJOBKANRI-RETURN    =   ZERO )  AND
                 ( JOB-ERRCD(1:2)  NOT =   "99" )  AND
                 ( JOB-ERRCD(1:2)  NOT =   "01" )
               IF    ( JOB-ENDTIME     =   SPACE)  OR
                     ( JOB-ENDYMD      =   SPACE)
                   MOVE    5                   TO  SPA-GMN-DURATION
               END-IF
           END-IF
           IF    ( SJOBKANRI-RETURN    =   ZERO )  AND
                 ( JOB-ERRCD(1:2)      =   "51" )
               MOVE    4                   TO  SPA-GMN-DURATION
           END-IF
      *----(03.04.01)--ADD-END-----
      ***  IF      SJOBKANRI-RETURN    =   ZERO
      ***    IF    JOB-ERRCD(1:2)      =   "55"
      *           MOVE    "プログラム更新は異常終了しました。"
      *                                   TO  SPA-GMN-MSG
      *           MOVE    "red"           TO  SPA-GMN-MSG-STYLE
      ***         MOVE    "0401"          TO  SPA-MMID1CD
      ***         PERFORM 520-MIDSET-SEC
      ***    END-IF
      ***  END-IF
           .
       400-JYOKYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プルーフ読み込み処理
      *****************************************************************
       4001-PROOF-READ-SEC         SECTION.
           READ    PGUP-FILE
               AT END
                   MOVE    1                   TO  FLG-PGKANRI
               NOT AT END
                   COMPUTE CNT-PGKANRI         =   CNT-PGKANRI  +  1
           END-READ
           .
       4001-PROOF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オフライン処理コンボ
      *****************************************************************
       400-OFFNAIYO-CHK-SEC        SECTION.
      *
      *    変化なし
           IF     (M97-OFF-NAIYO   NOT =   SPACE) AND
                  (M97-OFF-NAIYO       =   SPA-GMN-OFFNAIYO)
      *
               IF      SPA-GMN-OFFLIST-MAX    >  ZERO
                   MOVE    1           TO  SPA-GMN-CUR
               END-IF
               GO  TO  400-OFFNAIYO-CHK-EXT
           END-IF
      *
           MOVE    M97-OFF-NAIYO   TO  SPA-GMN-OFFNAIYO
      *
           IF      SPA-GMN-CD      =   "1"     OR    "2"
               MOVE    99              TO  SPA-GMN-CUR
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
               PERFORM 510-ERRSET-SEC
           END-IF
           .
      *
       400-OFFNAIYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *       削除処理
      *****************************************************************
       410-JOBDEL-SEC              SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-JOBDEL-EXT
           END-IF
      *
           PERFORM 820-JOBKANRI-READ2-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "0206"          TO  SPA-MMID1CD
           ELSE
               MOVE    "0204"          TO  SPA-MMID1CD
           END-IF
           PERFORM 520-MIDSET-SEC
           .
      *
       410-JOBDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    プラグイン
      *****************************************************************
       410-PLUGIN-SEC              SECTION.
      *
           MOVE    "M80"               TO  SPA-SAKIPG
           MOVE    "M97"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    "M80"               TO  MCP-WINDOW
           MOVE    "JOIN"              TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       410-PLUGIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認処理
      *****************************************************************
       410-SENDKAKUNIN-SEC         SECTION.
      *
           MOVE    "0200"          TO  SPA-MMID1CD
           PERFORM 520-MIDSET-SEC
           .
      *
       410-SENDKAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    TermID一覧取得処理
      *****************************************************************
       410-TERMIDCNT-SEC           SECTION.
      *
           MOVE    ZERO            TO  CNT-TERMID
           INITIALIZE                  SLDUSER-AREA
           MOVE    1               TO  SLDUSER-KBN
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           IF    ( SLDUSER-OTHERS  =   ZERO )
               PERFORM 410-KAKUNIN-SEC
           ELSE
               MOVE    SLDUSER-MAX     TO  CNT-TERMID
               MOVE    "0201"          TO  SPA-MMID1CD
               PERFORM 520-MIDSET-SEC
           END-IF
      *
           .
       410-TERMIDCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       410-KAKUNIN-SEC             SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-KAKUNIN-EXT
           END-IF
      *
           PERFORM 820-JOBKANRI-READ2-SEC
      *
           IF      SKBN-RUN        =   "RUN"
               MOVE    "0203"          TO  SPA-ERRCD
               PERFORM 510-ERRSET-SEC
           ELSE
               PERFORM 820-JOBKANRI-READ3-SEC
               IF      MST-RUN         =   "RUN"
                   MOVE    "0501"          TO  SPA-ERRCD
                   PERFORM 510-ERRSET-SEC
               ELSE
                   ACCEPT  WRK-ENV-OFFLINE FROM ENVIRONMENT
                                               "ENV_OFFLINE"
                   IF    ( SPA-MW              =   SPA-PANDA ) AND
                         ( SPA-NAI-CONTRACT-COND
                                           NOT =   1   AND 2 ) AND
                         ( WRK-ENV-OFFLINE NOT =   "yes" )
                       MOVE    "GUD1"              TO  SPA-MMID1CD
                       PERFORM 830-GAMENN-SET7-SEC
                       GO  TO  410-KAKUNIN-EXT
                   ELSE
                       MOVE    "0202"              TO  SPA-MMID1CD
                   END-IF
                   PERFORM 520-MIDSET-SEC
               END-IF
           END-IF
           .
      *
       410-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       420-KOUSHIN-SEC             SECTION.
      *
      *    ジョブ管理削除
           PERFORM 820-JOBKANRI-DEL-SEC
      *
      *    ジョブ管理作成
           PERFORM 820-JOBKANRI-JBS-SEC
      *
      *    状況更新
           PERFORM 400-JYOKYO-SET-SEC
      *
      *    プログラム更新処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
      *
      *    オンライン
      *    DISPLAY "SPA-GMN-ONKBN=" SPA-GMN-ONKBN
      *     IF      SPA-GMN-ONKBN       =   "T"
      *         MOVE    SHELL-NAME1     TO  MKPASS-IN-01
      *         MOVE    "2"             TO  SHELLTBL-ARG1
      *     END-IF
      *
      *    オフライン
      *     IF      SPA-GMN-OFFKBN      =   "T"
      *         MOVE    SHELL-NAME2     TO  MKPASS-IN-01
      *         EVALUATE    SPA-GMN-CD
      *             WHEN    "1"
      *                 MOVE    "2"         TO  SHELLTBL-ARG1
      *                 MOVE    "1"         TO  SHELLTBL-ARG2
      *             WHEN    "2"
      *                 MOVE    "2"         TO  SHELLTBL-ARG1
      *                 MOVE    "2"         TO  SHELLTBL-ARG2
      *             WHEN    OTHER
      *                 GO  TO  420-KOUSHIN-EXT
      *         END-EVALUATE
      *     END-IF
      *
      *     CALL    "ORCSMKPASS"        USING
      *                                 ORCSMKPASSAREA
      *     MOVE    MKPASS-OT-01    TO  SHELLTBL-NAME
           MOVE    SHELL-NAME1     TO  SHELLTBL-NAME
           MOVE    "update"        TO  SHELLTBL-ARG1
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    SPA-SUPERVISOR  TO  SHELLTBL-ARG3
           MOVE    SHELLTBL        TO  MCPDATA-REC
      *
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       420-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一斉メッセージ配信処理
      *****************************************************************
       420-SENDALL-SEC             SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    5               TO  SLDUSER-KBN
           STRING  "プログラム更新処理を行います。"
                                       DELIMITED BY SIZE
                   "入力作業を完了し速やかにログアウトしてください。"
                                       DELIMITED BY SIZE
                                 INTO  SLDUSER-MESSAGE
           END-STRING
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       420-SENDALL-EXT.
           EXIT.
      *
      *****************************************************************
      *       再起動処理
      *****************************************************************
       420-RESTART-SEC             SECTION.
      *
      *    日レセ再起動処理
           MOVE    SPACE           TO  SHELLTBL
           INITIALIZE                  ORCSMKPASSAREA
      *
           MOVE    SHELL-NAME2     TO  SHELLTBL-NAME
           MOVE    "update"        TO  SHELLTBL-ARG1
grpsys     MOVE    SPA-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    SPA-SUPERVISOR  TO  SHELLTBL-ARG3
           MOVE    SHELLTBL        TO  MCPDATA-REC
      *
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       420-RESTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD        =   ALL "9"  OR   SPACE
               MOVE    SPACE           TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
      *
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-MMID1CD     NOT =   SPACE
               PERFORM 520-MIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           PERFORM 500-GMNHEN-SEC
           MOVE    SPACE           TO  LINKAREA
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "M97"           TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  M97
      *
      *    オンライン・オフライン
           MOVE    SPA-GMN-ONKBN   TO  M97-ONKBN
           MOVE    SPA-GMN-OFFKBN  TO  M97-OFFKBN-NAIYO
      *
      *    オフラインコンボ
      *     IF      SPA-GMN-OFFKBN  =   "T"
      *         MOVE    SPA-GMN-OFFNAIYO    TO  M97-OFF-NAIYO
      *         PERFORM VARYING IDX FROM    1       BY  1
      *                 UNTIL   IDX >       SPA-GMN-OFFLIST-MAX
      *             MOVE    SPA-GMN-OFFLIST(IDX)
      *                                     TO  M97-OFF-LIST(IDX)
      *         END-PERFORM
      *         MOVE    SPA-GMN-OFFLIST-MAX TO  M97-OFF-COUNT
      *         MOVE    WIDGET-NORMAL       TO  M97-OFF-STATE
      *     ELSE
               MOVE    WIDGET-INSENSITIVE  TO  M97-OFF-STATE
               MOVE    WIDGET-INSENSITIVE  TO  M97-OFFKBN-STATE
      *     END-IF
      *
      *    コラムリスト
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX >       SPA-GMN-NAIYOLIST-MAX
               MOVE    SPA-GMN-TEIKYOYMD(IDX)
                                       TO  M97-NAIYO-0(IDX)
               MOVE    SPA-GMN-SYORIFLG(IDX)
                                       TO  M97-NAIYO-1(IDX)
               MOVE    SPA-GMN-NAIYO(IDX)
                                       TO  M97-NAIYO-2(IDX)
           END-PERFORM
           MOVE    SPA-GMN-NAIYOLIST-MAX   TO  M97-NAIYO-COUNT
      *
      *    メッセージ
           MOVE    SPA-GMN-MSG         TO  M97-MSG-NAIYO
           MOVE    SPA-GMN-MSG-STYLE   TO  M97-MSG-STYLE
      *
      *----(03.04.01)--ADD-START---
           MOVE   SPA-GMN-DURATION     TO  M97-DURATION
      *----(03.04.01)--ADD-END-----
           IF      SPA-MW              =   SPA-PANDA
               IF      SPA-NAI-CONTRACT-COND       =   1   OR  2
                   IF      SPA-NAI-KADO-LEVEL      =   5
                       MOVE    "教育版"            TO  M97-CONTRACTMSG
                   ELSE
                       MOVE    "商用版"            TO  M97-CONTRACTMSG
                   END-IF
                   MOVE    "red"               TO
                                               M97-CONTRACTMSG-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "ONKBN"           TO  MCP-WIDGET
               WHEN    2
                   MOVE  "OFFKBN"          TO  MCP-WIDGET
               WHEN    3
                   MOVE  "OFFNAIYO"        TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                            TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "更新処理が実行中の為、削除できません"
                                            TO  SPA-ERRMSG
               WHEN    "0203"
                   MOVE    "処理中です。"
                                            TO  SPA-ERRMSG
               WHEN    "0501"
                   MOVE    "マスタ更新が実行中です。"
                                            TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPACE            TO  SPA-ERRMSG
           END-EVALUATE
      *
      *----(03.04.01)--ADD-START---
           MOVE    ZERO            TO  M97-DURATION
      *----(03.04.01)--ADD-END-----
      *
           MOVE    SPACE           TO  MMERR
           MOVE    SPA-ERRCD       TO  MMERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MMERR-ERRMSG
      *
           PERFORM 830-GAMENN-SET4-SEC
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-MMID1CD
               WHEN    "0200"
               MOVE
              "プログラム更新処理の一斉通知を行います。よろしいですか？"
                                           TO  WRK-MIDMSG
               WHEN    "0201"
               MOVE    SPACE       TO  WRK-MIDMSG
               STRING  "接続中の端末があります。端末数は" DELIMITED SIZE
                       CNT-TERMID                         DELIMITED SIZE
                       "です。"                           DELIMITED SIZE
                                         INTO  WRK-MIDMSG
               END-STRING
               WHEN    "0202"
               MOVE    "プログラム更新を実行します。よろしいですか？"
                                           TO  WRK-MIDMSG
               WHEN    "0204"
                   MOVE    "ジョブ管理を削除します。よろしいですか？"
                                           TO  WRK-MIDMSG
               WHEN    "0205"
	       MOVE    SPACE               TO  WRK-MIDMSG
               STRING "プログラムの適用状態をチェ"   DELIMITED BY SIZE
                      "ックします。よろしいですか？" DELIMITED BY SIZE
                                         INTO  WRK-MIDMSG
               END-STRING
               WHEN    "0206"
                   MOVE    "処理中ですがジョブ管理を削除しますか？"
                                           TO  WRK-MIDMSG
               WHEN    "0301"
                   MOVE    "処理中ですがよろしいですか？"
                                           TO  WRK-MIDMSG
      ***      WHEN    "0401"
      ***      MOVE "再起動しますので端末を切断します。よろしいですか？"
      ***                                  TO  WRK-MIDMSG
               WHEN    OTHER
                   MOVE    SPA-MMID1CD     TO  WRK-MIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  MMID1
           INITIALIZE                      MMID1
           MOVE    SPA-MMID1CD         TO  MMID1-ID1CODE
           MOVE    WRK-MIDMSG          TO  MMID1-ID1MSG
      *
           PERFORM 830-GAMENN-SET5-SEC
      *
           .
       520-MIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ1-SEC      SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
      *
       820-JOBKANRI-READ1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ2-SEC      SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           IF      SJOBKANRI-RETURN    =   ZERO
               IF      JOB-ERRCD(1:2)      =   "99"
                   MOVE    SPACE           TO  SKBN-RUN
               ELSE
      ***        IF      JOB-ERRCD(1:2)      =   "55"
      ***            MOVE    "RST"           TO  SKBN-RUN
      ***        ELSE
                   IF      JOB-ERRCD(1:2)      =   "01"
                       MOVE    SPACE           TO  SKBN-RUN
                   ELSE
                       IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                              (JOB-ENDYMD      NOT =   SPACE)
                           MOVE    SPACE           TO  SKBN-RUN
                       ELSE
                           MOVE    "RUN"           TO  SKBN-RUN
                       END-IF
                   END-IF
      ***        END-IF
               END-IF
           ELSE
               MOVE    SPACE            TO  SKBN-RUN
           END-IF
           .
      *
       820-JOBKANRI-READ2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ3-SEC      SECTION.
      *
           MOVE    SPACE           TO  MST-RUN
      *
           MOVE    ZERO                TO  FLG-SYSBASE
           INITIALIZE                      SYSBASE-REC
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           PERFORM UNTIL   FLG-SYSBASE =   1
      *
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    1               TO  JOB-JOBID
               MOVE    "MSTMNT"        TO  JOB-SHELLID
               MOVE    SYSBASE-HOSPNUM TO  JOB-HOSPNUM
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
               IF      SJOBKANRI-RETURN    =   ZERO
                   IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                          (JOB-ENDYMD      NOT =   SPACE)
                       CONTINUE
                   ELSE
                       MOVE    "RUN"       TO  MST-RUN
                   END-IF
               END-IF
      *
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           END-PERFORM
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
      *
           IF      MST-RUN         =   "RUN"
               GO  TO  820-JOBKANRI-READ3-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYSBASE
           INITIALIZE                      SYSBASE-REC
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           PERFORM UNTIL   FLG-SYSBASE =   1
      *
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    1               TO  JOB-JOBID
               MOVE    "MSTMLI"        TO  JOB-SHELLID
               MOVE    SYSBASE-HOSPNUM TO  JOB-HOSPNUM
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
               IF      SJOBKANRI-RETURN    =   ZERO
                   IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                          (JOB-ENDYMD      NOT =   SPACE)
                       CONTINUE
                   ELSE
                       MOVE    "RUN"       TO  MST-RUN
                   END-IF
               END-IF
      *
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           END-PERFORM
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
      *
           .
      *
       820-JOBKANRI-READ3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理削除
      *****************************************************************
       820-JOBKANRI-DEL-SEC        SECTION.
      *
      *     PERFORM 820-JOBKANRI-READ1-SEC
      *
      *     IF      SJOBKANRI-RETURN    =   ZERO
      *         IF     (JOB-ENDTIME     NOT =   SPACE)  AND
      *                (JOB-ENDYMD      NOT =   SPACE)
      *            ジョブ管理削除
                   MOVE    "DEL"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-JOBID
                   MOVE    "PRGMNT"        TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"           USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
      *         ELSE
      *             MOVE    "0004"          TO  SPA-ERRCD
      *         END-IF
      *     END-IF
           .
      *
       820-JOBKANRI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理作成
      *****************************************************************
       820-JOBKANRI-JBS-SEC        SECTION.
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           .
      *
       820-JOBKANRI-JBS-EXT.
           EXIT.
      *****************************************************************
      *    画面ERR
      *****************************************************************
       830-GAMENN-SET4-SEC         SECTION.
      *
           MOVE    "B01"           TO  MCP-WIDGET
           MOVE    "M97"           TO  SPA-MOTOPG
           MOVE    "MMERR"         TO  SPA-SAKIPG
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MMERR"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       830-GAMENN-SET4-EXT.
           EXIT.
      *****************************************************************
      *    画面警告
      *****************************************************************
       830-GAMENN-SET5-SEC         SECTION.
      *
           MOVE    "B12"           TO  MCP-WIDGET
           MOVE    "M97"           TO  SPA-MOTOPG
           MOVE    "MMID1"         TO  SPA-SAKIPG
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MMID1"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       830-GAMENN-SET5-EXT.
           EXIT.
      *****************************************************************
      *    戻る
      *****************************************************************
       830-GAMENN-SET6-SEC         SECTION.
      *
           MOVE    "M00"           TO  SPA-SAKIPG
           MOVE    "M97"           TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU      TO  LNK-KYOUTU
      *
           MOVE    "M00"           TO  MCP-WINDOW
           MOVE    "JOIN"          TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
      *
       830-GAMENN-SET6-EXT.
           EXIT.
      *****************************************************************
      *    案内１画面
      *****************************************************************
       830-GAMENN-SET7-SEC         SECTION.
      *
           MOVE    "B12"               TO  MCP-WIDGET
           MOVE    "M97"               TO  SPA-MOTOPG
           MOVE    "MGUIDE1"           TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MGUIDE1"           TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END 
           .
       830-GAMENN-SET7-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSBASE-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           .
       900-SYSBASE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    アクセスキー読込処理
      *****************************************************************
       900-ACCESSKEY-READ-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ACCESSKEY
           INITIALIZE  ACCESS-KEY-REC
           MOVE    SPA-HOSPNUM         TO  ACCESS-KEY-HOSPNUM
           MOVE    ACCESS-KEY-REC      TO  MCPDATA-REC
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_access_key"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  ACCESS-KEY-REC
               ELSE
                   MOVE    1               TO  FLG-ACCESSKEY
                   INITIALIZE  ACCESS-KEY-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-ACCESSKEY
               INITIALIZE  ACCESS-KEY-REC
           END-IF
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           .
       900-ACCESSKEY-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルFETCH処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
