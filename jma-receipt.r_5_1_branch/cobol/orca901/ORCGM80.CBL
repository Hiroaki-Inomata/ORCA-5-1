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
       PROGRAM-ID.                 ORCGM80.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : プラグイン
      *  コンポーネント名  : プラグイン画面（Ｍ８０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/02/01    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  11/12/06  upgrade非活性対応
      *  04.07.00    NACL-多々納  12/07/11  ベンダー毎対応
      *  04.07.01    MACL-森脇    13/03/06  組込・取込、削除・取外まとめ
      *  04.08.00    NACL-森脇    15/09/07  GINBEE対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    プラグインデータ
      *----(04.08.00)--UPD-START---
      *     SELECT  PLUGINLIST-FILE     ASSIGN  FILE-NAME1
           SELECT  PLUGINLIST-FILE     ASSIGN  ASGNPARA
                                       ORGANIZATION   IS LINE SEQUENTIAL
                                       FILE    STATUS  IS  STS-DATA1.
      *----(04.08.00)--UPD-END-----
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    プラグインデータ
       FD  PLUGINLIST-FILE.
       01  PLUGINLIST-REC.
           03  PLUGIN-ID               PIC X(15).
           03  PLUGIN-KBN              PIC X(01).
           03  PLUGIN-DATA             PIC X(1024).
      *
       WORKING-STORAGE             SECTION.
      *
      *----(04.08.00)--UPD-START---
      *    プラグインデータ
       01  FILE-NAME1.
      *     03  FILLER                  PIC X(09)   VALUE   "/var/tmp/".
           03  FILLER                  PIC X(14)   VALUE   "pluginlist".
      *----(04.08.00)--UPD-END-----
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-DATA1               PIC X(02).
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
           COPY    "M901COMMON-SPA".
      *----(04.08.00)--ADD-START---
           COPY    "CPASGNPARA.INC".
      *----(04.08.00)--ADD-END-----
      *
      *    画面スパ領域
       01  SPA-M80.
           03  SPA-M80-AREA.
               05  SPA-GMN-CUR         PIC 9(03).
               05  SPA-GMN-PAGE        PIC 9(03).
               05  SPA-GMN-NEXT        PIC 9(01).
      *
      *        一覧表
               05  SPA-GMN-LIST-AREA.
                   07  SPA-GMN-TBLREC          OCCURS  100.
                       09  SPA-GMN-TNUM        PIC 9(04).
                       09  SPA-GMN-TPGMEISYO   PIC X(100).
                       09  SPA-GMN-TPGNAME     PIC X(50).
                       09  SPA-GMN-TVERSION    PIC X(20).
                       09  SPA-GMN-TVERSION2   PIC X(20).
                       09  SPA-GMN-TVENDOR     PIC X(20).
                       09  SPA-GMN-TLINK       PIC X(10).
                       09  SPA-GMN-TDATE       PIC X(10).
                       09  SPA-NAI-TLINK       PIC X(10).
                   07  SPA-TBL-MAX             PIC 9(04).
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-IDX         PIC 9(04).
                   07  SPA-NAI-LINK        PIC 9(01).
                   07  SPA-NAI-PGNAME      PIC X(50).
                   07  SPA-NAI-VERSION     PIC X(20).
                   07  SPA-NAI-VERSION2    PIC X(20).
      *
               05  SPA-GMN-SELNUM      PIC 9(04).
               05  SPA-GMN-DURATION    PIC 9(01).
      *
      *        処理中
               05  SPA-GMN-SYORIMSG    PIC X(80).
               05  SPA-NAI-SYORIERR    PIC 9(01).
      *
      *        ベンダー
               05  SPA-NAI-VENDFLG     PIC 9(01).
               05  SPA-NAI-VENDOR      PIC X(20).
               05  SPA-NAI-VENDORALL   PIC X(01).
      *
      *        並び替え
               05  SPA-NAI-SORTFLG     PIC 9(01).
      *
      *        SHELL区分
               05  SPA-SHELLFLG        PIC 9(01).
               05  SPA-SHELL-RC        PIC 9(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-DATAEND             PIC 9(01).
      *
           03  FLG-NO                  PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-SYORI-END           PIC 9(01).
           03  FLG-HENOK               PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-STR                 PIC 9(04).
           03  IDX-V                   PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-COMMAND             PIC X(100).
           03  WRK-PACKAGE             PIC X(100).
           03  WRK-VERSION             PIC X(100).
      *
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-Z9                  PIC ZZZ9.
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
      *    コラムリスト位置
           03  WRK-M80-LIST-ROW        PIC S9(09).
           03  FLG-GMN-INIT            PIC 9(01).
      *
           03  WRK-VENDOR              PIC X(20).
      *
      *H24.7
      *    保存
       01  WRK-GMN-HZN-AREA.
           03  WRK-GMN-TBLREC          OCCURS  100.
               05  WRK-GMN-TNUM        PIC 9(04).
               05  WRK-GMN-TPGMEISYO   PIC X(100).
               05  WRK-GMN-TPGNAME     PIC X(50).
               05  WRK-GMN-TVERSION    PIC X(20).
               05  WRK-GMN-TVERSION2   PIC X(20).
               05  WRK-GMN-TVENDOR     PIC X(20).
               05  WRK-GMN-TLINK       PIC X(10).
               05  WRK-GMN-TDATE       PIC X(10).
               05  WRK-NAI-TLINK       PIC X(10).
           03  WRK-TBL-MAX             PIC 9(04).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG             PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO             PIC X(01).
               05  WRK-HYY             PIC Z9.
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HMM             PIC Z9.
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HDD             PIC Z9.
      *
      *   明細最大数
       01  MAX-LINE                    PIC 9(04)   VALUE   100.
      *
      *    シェル
       01  WRK-SHELL-NAME.
           03  FILLER                  PIC X(09)  VALUE  "plugin.sh".
      *
       01  CONST-AREA.
           03  CONST-MNTJOBID          PIC 9(07)  VALUE    1.
           03  CONST-MNTSHELLID        PIC X(08)  VALUE    "PLUGIN".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *----(04.08.00)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.00)--ADD-END-----
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
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
           MOVE    SPA-WORK        TO  SPA-M901KYOUTU
           MOVE    SPA-FREE        TO  SPA-M80
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
      *        初期
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *        画面遷移
               WHEN    "PUTG"      ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *        入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-M901KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M80         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "MMERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
      *
               MOVE   SPACE            TO  MCP-PUTTYPE
               MOVE   "M80"            TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1               TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-AREA
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MMID1"
      *            確認画面より
                   PERFORM 3002-MMID1-SET-SEC
               WHEN    OTHER
                   PERFORM 3001-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-SET-SEC            SECTION.
      *
           INITIALIZE                  SPA-M80
           INITIALIZE                  SPA-M901KYOUTU
      *    一覧更新
           PERFORM 404-LIST-INIT-SEC
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3001-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＸＤＩＤ１）ＯＫ処理
      *****************************************************************
       3002-MMID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-MMID1CD
      *        インストール
               WHEN    "0101"
                   IF      SPA-MMID1-FLG       =   "OK"
                       IF      SPA-MW              NOT =   SPA-PANDA
                           MOVE    "enable"            TO  WRK-COMMAND
                       ELSE
                           MOVE    "install+enable"    TO  WRK-COMMAND
                       END-IF
                       PERFORM 4010-SHELL-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
      *        アンインストール
               WHEN    "0102"
                   IF      SPA-MMID1-FLG       =   "OK"
                       IF      SPA-MW              NOT =   SPA-PANDA
                           MOVE    "disable"           TO  WRK-COMMAND
                       ELSE
                           MOVE    "uninstall"         TO  WRK-COMMAND
                       END-IF
                       PERFORM 4010-SHELL-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
      *        アップグレード
               WHEN    "0105"
                   IF      SPA-MMID1-FLG       =   "OK"
                       IF      SPA-NAI-LINK        =   2
                           MOVE    "upgrade2"      TO  WRK-COMMAND
                       ELSE
                           MOVE    "upgrade"       TO  WRK-COMMAND
                       END-IF
                       PERFORM 4010-SHELL-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
      *        強制インストール
               WHEN    "0111"
                   IF      SPA-MMID1-FLG       =   "OK"
                       MOVE    "reinstall"         TO  WRK-COMMAND
                       PERFORM 4010-SHELL-SYORI-SEC
                   ELSE
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-MMID1CD
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       3002-MMID1-SET-EXT.
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
      *        クリア
               WHEN    "CLICKED"   ALSO    "B02"
                    PERFORM 402-CLREA-SEC
      *        並び替え
               WHEN    "CLICKED"   ALSO    "B03"
                    PERFORM 403-SORT-INIT-SEC
      *        一覧更新
               WHEN    "CLICKED"   ALSO    "B04"
                    PERFORM 404-LIST-INIT-SEC
      *        ベンダー
               WHEN    "CLICKED"   ALSO    "B05"
                    PERFORM 405-VENDOR-INIT-SEC
      *        前頁
               WHEN    "CLICKED"   ALSO    "B06"
                   PERFORM 406-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"   ALSO    "B07"
                   PERFORM 407-NEXT-PAGE-SEC
      *        インストール
               WHEN    "CLICKED"   ALSO    "B09"
                    PERFORM 409-INSTALL-SEC
      *        アップグレード
               WHEN    "CLICKED"   ALSO    "B10"
                    PERFORM 410-UPGRADE-SEC
      *        状況
               WHEN    "CLICKED"   ALSO    "pandatimer1"
                   PERFORM 400-JYOKYO-SET-SEC
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
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
           MOVE    ZERO            TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"  ALSO    "SELNUM"
                   PERFORM 4001-SELNUM-CHK-SEC
      *        選択
               WHEN    ANY         ALSO    "LIST"
                   PERFORM 4003-LIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-MW          NOT =   SPA-PANDA
               MOVE    "M00"               TO  SPA-SAKIPG
           ELSE
               MOVE    "M97"               TO  SPA-SAKIPG
           END-IF
           MOVE    "M80"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    状況更新
      *****************************************************************
       400-JYOKYO-SET-SEC          SECTION.
      *
      *    メッセージ編集
           PERFORM 820-JOBKANRI-READ1-SEC
      *
           MOVE    ZERO                TO  FLG-SYORI-END
           MOVE    SPACE               TO  SPA-GMN-SYORIMSG
           MOVE    ZERO                TO  SPA-NAI-SYORIERR
           IF      SJOBKANRI-RETURN    =   ZERO
               EVALUATE    JOB-ERRCD(1:2)
                   WHEN    "99"
                   WHEN    "98"
                       MOVE    "異常終了しました。"
                                               TO  SPA-GMN-SYORIMSG
                       MOVE    1               TO  SPA-NAI-SYORIERR
                       MOVE    1               TO  FLG-SYORI-END
                   WHEN    "01"
                       MOVE    "対象データがありません。"
                                               TO  SPA-GMN-SYORIMSG
                       MOVE    2               TO  SPA-NAI-SYORIERR
                       MOVE    1               TO  FLG-SYORI-END
                   WHEN    OTHER
                       IF     (JOB-ENDTIME     NOT =   SPACE)  AND
                              (JOB-ENDYMD      NOT =   SPACE)
                           IF      SPA-SHELLFLG        =   2
                               MOVE    "処理は正常に終了しました。"
                                                   TO  SPA-GMN-SYORIMSG
                           END-IF
                           MOVE    1               TO  FLG-SYORI-END
                       ELSE
                           MOVE    "処理中です。"  TO  SPA-GMN-SYORIMSG
                           MOVE    2               TO  SPA-NAI-SYORIERR
                       END-IF
               END-EVALUATE
           ELSE
               MOVE    1               TO  FLG-SYORI-END
           END-IF
      *
           MOVE    0           TO  SPA-GMN-DURATION
      *
           IF    ( SJOBKANRI-RETURN    =   ZERO )  AND
                 ( JOB-ERRCD(1:2)  NOT =   "99" )  AND
                 ( JOB-ERRCD(1:2)  NOT =   "01" )
               IF    ( JOB-ENDTIME     =   SPACE)  OR
                     ( JOB-ENDYMD      =   SPACE)
                   MOVE    3           TO  SPA-GMN-DURATION
               END-IF
           END-IF
      *
      *    処理終了
           IF     (FLG-SYORI-END       =   1   )  AND
                  (SPA-NAI-SYORIERR    =   ZERO )
      *        一覧表表示
               PERFORM 4041-LIST-HENSYU-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       400-JYOKYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLREA-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           INITIALIZE                      SPA-NAI-AREA
           MOVE    SPACE               TO  SPA-GMN-SYORIMSG
           MOVE    ZERO                TO  SPA-NAI-SYORIERR
           MOVE    ZERO                TO  SPA-NAI-VENDFLG
           MOVE    SPACE               TO  SPA-NAI-VENDOR
           MOVE    SPACE               TO  SPA-NAI-VENDORALL
           MOVE    ZERO                TO  SPA-NAI-SORTFLG
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       402-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    並び替え
      *****************************************************************
       403-SORT-INIT-SEC         SECTION.
      *
      *    並び替え
           MOVE    1                   TO  SPA-NAI-SORTFLG
           PERFORM 404-LIST-INIT-SEC
           .
       403-SORT-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    リスト更新処理
      *****************************************************************
       404-LIST-INIT-SEC           SECTION.
      *
           MOVE    1               TO  SPA-SHELLFLG
           MOVE    ZERO            TO  SPA-SHELL-RC
      *
           MOVE    "list"          TO  WRK-COMMAND
           MOVE    SPACE           TO  WRK-PACKAGE
           MOVE    SPACE           TO  WRK-VERSION
           PERFORM 4041-SHELL-CALL-SEC
           .
       404-LIST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＨＥＬＬ起動処理
      *****************************************************************
       4041-SHELL-CALL-SEC         SECTION.
      *
      *    ジョブ管理削除
           PERFORM 820-JOBKANRI-DEL-SEC
      *
      *    ジョブ管理作成
           PERFORM 820-JOBKANRI-JBS-SEC
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    WRK-SHELL-NAME      TO  SHELLTBL-NAME
      *
      *    パラメタセット
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    CONST-MNTJOBID      TO  SHELLTBL-ARG2
           MOVE    CONST-MNTSHELLID    TO  SHELLTBL-ARG3
           MOVE    WRK-COMMAND         TO  SHELLTBL-ARG4
           MOVE    WRK-PACKAGE         TO  SHELLTBL-ARG5
           MOVE    WRK-VERSION         TO  SHELLTBL-ARG6
      * 
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *    状況更新
           PERFORM 400-JYOKYO-SET-SEC
           .
       4041-SHELL-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表編集処理
      *****************************************************************
       4041-LIST-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO            TO  SPA-GMN-SELNUM
           INITIALIZE                  SPA-NAI-AREA
           MOVE    SPACE           TO  SPA-GMN-SYORIMSG
           MOVE    ZERO            TO  SPA-NAI-SYORIERR
      *
           MOVE    1               TO  FLG-GMN-INIT
           INITIALIZE                  SPA-GMN-LIST-AREA
           MOVE    ZERO            TO  SPA-GMN-NEXT
      *H24.7
           INITIALIZE                  WRK-GMN-HZN-AREA
      *
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  IDX
      *
           COMPUTE IDX-STR             =  (SPA-GMN-PAGE  -   1  )
                                       *   MAX-LINE
                                       +   1
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    FILE-NAME1          TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING     SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGNPARA
           MOVE    SGETTEMP-ST         TO  ASGNPARA-ST
      *----(04.08.01)--ADD-END-----
      *
           OPEN    INPUT               PLUGINLIST-FILE
      *
           IF      STS-DATA1           =   ZERO
               PERFORM 900-PLUGNDATA-READ-SEC
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-DATAEND
           END-IF
           PERFORM UNTIL   FLG-DATAEND     =   1
      *
      *        並び替え
               IF      SPA-NAI-SORTFLG =   1
                   PERFORM 40424-LIST-HEN04-SEC
               ELSE
      *            一覧表編集
                   EVALUATE    SPA-NAI-VENDFLG
                       WHEN    ZERO
      *                    全体
                           PERFORM 4042-LIST-HEN-SEC
                       WHEN    1
      *                    べンダー一覧
                           PERFORM 40422-LIST-HEN02-SEC
                       WHEN    2
      *                    ベンダー毎
                           IF      SPA-NAI-VENDORALL   =   "1"
      *                        全体
                               PERFORM 4042-LIST-HEN-SEC
                           ELSE
      *                        ベンダー毎
                               PERFORM 40423-LIST-HEN03-SEC
                           END-IF
                   END-EVALUATE
               END-IF
      *
           END-PERFORM
      *
           CLOSE                       PLUGINLIST-FILE
           .
       4041-LIST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＨＥＬＬ起動処理
      *****************************************************************
       4042-LIST-HEN-SEC           SECTION.
      *
           IF      PLUGIN-ID           =   "name  "
               ADD     1               TO  IDX
               IF      IDX                 <   IDX-STR
      *            読み飛ばし
                   MOVE    1               TO  FLG-NO
               ELSE
                   MOVE    ZERO            TO  FLG-NO
                   ADD     1               TO  SPA-TBL-MAX
                   ADD     1               TO  IDY
                   IF      SPA-TBL-MAX     >   MAX-LINE
                       MOVE    MAX-LINE        TO  SPA-TBL-MAX
                       MOVE    1               TO  SPA-GMN-NEXT
                       MOVE    1               TO  FLG-DATAEND
                   END-IF
               END-IF
           END-IF
      *
           IF     (FLG-NO              =   ZERO )
             AND  (FLG-DATAEND         =   ZERO )
               EVALUATE    PLUGIN-ID
               WHEN    "name "
      *            番号
                   MOVE    IDX             TO  SPA-GMN-TNUM    (IDY)
      *            プログラムＩＤ
                   MOVE    PLUGIN-DATA     TO  SPA-GMN-TPGNAME   (IDY)
               WHEN    "current "
      *            バージョン
                   MOVE    PLUGIN-DATA     TO  SPA-GMN-TVERSION(IDY)
               WHEN    "available "
      *            バージョン（最新）
                   MOVE    PLUGIN-DATA     TO  SPA-GMN-TVERSION2(IDY)
               WHEN    "description "
      *            プログラム名称
                   MOVE    PLUGIN-DATA     TO  SPA-GMN-TPGMEISYO(IDY)
               WHEN    "vendor "
      *            ベンダー
                   MOVE    PLUGIN-DATA     TO  SPA-GMN-TVENDOR (IDY)
               WHEN    "date "
      *            日付
                   MOVE    PLUGIN-DATA     TO  SPA-GMN-TDATE (IDY)
               WHEN    "link "
      *            LINK
                   MOVE    PLUGIN-DATA     TO  SPA-NAI-TLINK (IDY)
                   IF      SPA-NAI-TLINK(IDY)  =   "ERROR"
                       IF      SPA-MW          =   SPA-GINBEE
                           MOVE    "-"             TO
                                               SPA-GMN-TLINK(IDY)
                       ELSE
                           MOVE    "NOT FOUND"     TO
                                               SPA-GMN-TLINK(IDY)
                       END-IF
                   ELSE
                       IF      SPA-NAI-TLINK (IDY) =   "FALSE"
                           MOVE    "-"         TO  SPA-GMN-TLINK (IDY)
                           IF      SPA-MW                  =   SPA-PANDA
                               IF      SPA-GMN-TVERSION (IDY)
                                                       NOT =  "-"
                                   MOVE    "ERROR"         TO
                                                   SPA-GMN-TLINK (IDY)
                               END-IF
                           END-IF
                       ELSE
                           MOVE    "○"        TO  SPA-GMN-TLINK (IDY)
                       END-IF
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF      FLG-DATAEND          =   ZERO
               PERFORM 900-PLUGNDATA-READ-SEC
           END-IF
           .
       4042-LIST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ベンダー一覧編集処理
      *****************************************************************
       40422-LIST-HEN02-SEC        SECTION.
      *
           IF      PLUGIN-ID           =   "vendor "
               MOVE    ZERO                TO  FLG-HENOK
               IF      IDX                 =   ZERO
                   MOVE    1                   TO  FLG-HENOK
               ELSE
                   PERFORM 404221-LIST02-INIT-SEC
               END-IF
               IF     (FLG-HENOK           =   1     )
                   ADD     1                   TO  IDX
                   IF      IDX                 <   IDX-STR
      *                読み飛ばし
                       MOVE    1               TO  FLG-NO
                   ELSE
                       MOVE    ZERO            TO  FLG-NO
                       ADD     1               TO  SPA-TBL-MAX
                       ADD     1               TO  IDY
                       IF      SPA-TBL-MAX     >   MAX-LINE
                           MOVE    MAX-LINE        TO  SPA-TBL-MAX
                           MOVE    1               TO  SPA-GMN-NEXT
                           MOVE    1               TO  FLG-DATAEND
                       END-IF
                   END-IF
                   IF     (FLG-NO              =   ZERO )
                     AND  (FLG-DATAEND         =   ZERO )
      *                テーブル編集
                       MOVE    IDX             TO  SPA-GMN-TNUM   (IDY)
      *                ベンダー
                       MOVE    PLUGIN-DATA     TO  SPA-GMN-TVENDOR(IDY)
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-DATAEND          =   ZERO
               PERFORM 900-PLUGNDATA-READ-SEC
           END-IF
      *
      *    最終行
           IF      FLG-DATAEND          =   1
               IF     (SPA-GMN-NEXT     =   ZERO )
               AND    (SPA-TBL-MAX      <   MAX-LINE )
                   ADD     1                TO  IDY
                   ADD     1                TO  SPA-TBL-MAX
                   ADD     1                TO  IDX
      *            テーブル編集
                   MOVE    IDX             TO  SPA-GMN-TNUM    (IDY)
      *            ベンダー
                   MOVE    "全部"          TO  SPA-GMN-TVENDOR(IDY)
               END-IF
           END-IF
           .
       40422-LIST-HEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ベンダー判定処理
      *****************************************************************
       404221-LIST02-INIT-SEC      SECTION.
      *
           MOVE    1                   TO  FLG-HENOK
           MOVE    PLUGIN-DATA         TO  WRK-VENDOR
           PERFORM VARYING     IDX-V   FROM    1   BY  1
                   UNTIL       IDX-V   >   SPA-TBL-MAX
               IF      WRK-VENDOR      =   SPA-GMN-TVENDOR (IDX-V)
                   MOVE    ZERO                TO  FLG-HENOK
                   MOVE    SPA-TBL-MAX         TO  IDX-V
               END-IF
           END-PERFORM
           .
       404221-LIST02-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ベンダー毎編集処理
      *****************************************************************
       40423-LIST-HEN03-SEC        SECTION.
      *
           IF      PLUGIN-ID           =   "name  "
               PERFORM 40423-LIST03-INIT-SEC
           END-IF
      *
           EVALUATE    PLUGIN-ID
               WHEN    "name "
      *            番号
                   MOVE    1               TO  WRK-GMN-TNUM(1)
      *            プログラムＩＤ
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TPGNAME(1)
               WHEN    "current "
      *            バージョン
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TVERSION(1)
               WHEN    "available "
      *            バージョン
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TVERSION2(1)
               WHEN    "description "
      *            プログラム名称
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TPGMEISYO(1)
               WHEN    "vendor "
      *            ベンダー
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TVENDOR(1)
               WHEN    "date "
      *            日付
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TDATE(1)
               WHEN    "link "
      *            LINK
                   MOVE    PLUGIN-DATA     TO  WRK-NAI-TLINK(1)
                   IF      WRK-NAI-TLINK(1)    =   "ERROR"
                       IF      SPA-MW          =   SPA-GINBEE
                           MOVE    "-"             TO
                                               WRK-GMN-TLINK(1)
                       ELSE
                           MOVE    "NOT FOUND"     TO
                                               WRK-GMN-TLINK(1)
                       END-IF
                   ELSE
                       IF      WRK-NAI-TLINK(1)    =   "FALSE"
                           MOVE    "-"             TO  WRK-GMN-TLINK(1)
                       ELSE
                           MOVE    "○"            TO  WRK-GMN-TLINK(1)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      FLG-DATAEND          =   ZERO
               PERFORM 900-PLUGNDATA-READ-SEC
           END-IF
      *
           IF     (FLG-DATAEND      NOT =   ZERO )
           AND    (SPA-GMN-NEXT         =   ZERO )
               PERFORM 40423-LIST03-INIT-SEC
           END-IF
           .
       40423-LIST-HEN03-EXT.
           EXIT.
      *
      *****************************************************************
      *    ベンダー一覧
      *****************************************************************
       40423-LIST03-INIT-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-HENOK
           IF      WRK-GMN-TNUM(1)     NOT =   ZERO
               IF     (SPA-NAI-VENDOR      =   WRK-GMN-TVENDOR(1))
      *            対象
                   MOVE    1               TO  FLG-HENOK
               END-IF
           END-IF
      *
           IF      FLG-HENOK           =   1
      *        編集対象
               ADD     1                   TO  IDX
               IF      IDX                 <   IDX-STR
      *            読み飛ばし
                   MOVE    1               TO  FLG-NO
               ELSE
                   MOVE    ZERO            TO  FLG-NO
                   ADD     1               TO  SPA-TBL-MAX
                   ADD     1               TO  IDY
                   IF      SPA-TBL-MAX     >   MAX-LINE
                       MOVE    MAX-LINE        TO  SPA-TBL-MAX
                       MOVE    1               TO  SPA-GMN-NEXT
                       MOVE    1               TO  FLG-DATAEND
                   END-IF
               END-IF
               IF     (FLG-NO              =   ZERO )
                 AND  (SPA-GMN-NEXT        =   ZERO )
                   MOVE    IDX                 TO  WRK-GMN-TNUM(1)
                   MOVE    WRK-GMN-TBLREC(1)   TO  SPA-GMN-TBLREC(IDY)
               END-IF
           END-IF
      *    クリア
           INITIALIZE                  WRK-GMN-HZN-AREA
           .
       40423-LIST03-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    並び替え処理
      *****************************************************************
       40424-LIST-HEN04-SEC        SECTION.
      *
           EVALUATE    PLUGIN-ID
               WHEN    "name "
      *            番号
                   ADD     1               TO  IDX
                   ADD     1               TO  WRK-TBL-MAX
                   MOVE    IDX             TO  WRK-GMN-TNUM(IDX)
      *            プログラムＩＤ
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TPGNAME(IDX)
               WHEN    "current "
      *            バージョン
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TVERSION(IDX)
               WHEN    "available "
      *            バージョン
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TVERSION2(IDX)
               WHEN    "description "
      *            プログラム名称
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TPGMEISYO(IDX)
               WHEN    "vendor "
      *            ベンダー
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TVENDOR(IDX)
               WHEN    "date "
      *            日付
                   MOVE    PLUGIN-DATA     TO  WRK-GMN-TDATE(IDX)
               WHEN    "link "
      *            LINK
                   MOVE    PLUGIN-DATA     TO  WRK-NAI-TLINK(IDX)
                   IF      WRK-NAI-TLINK(IDX)  =   "ERROR"
                       IF      SPA-MW          =   SPA-GINBEE
                           MOVE    "-"             TO
                                               WRK-GMN-TLINK(IDX)
                       ELSE
                           MOVE    "NOT FOUND"     TO
                                               WRK-GMN-TLINK(IDX)
                       END-IF
                   ELSE
                       IF      WRK-NAI-TLINK(IDX)  =   "FALSE"
                           MOVE    "-"         TO  WRK-GMN-TLINK(IDX)
                       ELSE
                           MOVE    "○"        TO  WRK-GMN-TLINK(IDX)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      FLG-DATAEND          =   ZERO
               PERFORM 900-PLUGNDATA-READ-SEC
           END-IF
      *
           IF     (FLG-DATAEND      NOT =   ZERO )
           AND    (SPA-GMN-NEXT         =   ZERO )
               PERFORM 40424-LIST04-INIT-SEC
           END-IF
           .
       40424-LIST-HEN04-EXT.
           EXIT.
      *
      *****************************************************************
      *    並び替え一覧
      *****************************************************************
       40424-LIST04-INIT-SEC       SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   WRK-TBL-MAX
               IF      WRK-NAI-TLINK(IDX)  =   "TRUE"
                   PERFORM 40424-LIST042-INIT-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   WRK-TBL-MAX
               IF      WRK-NAI-TLINK(IDX)  =   "ERROR"
                   PERFORM 40424-LIST042-INIT-SEC
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   WRK-TBL-MAX
               IF      WRK-NAI-TLINK(IDX)  =   "FALSE"
                   PERFORM 40424-LIST042-INIT-SEC
               END-IF
           END-PERFORM
      *
      *    クリア
           INITIALIZE                  WRK-GMN-HZN-AREA
           MOVE    ZERO            TO  SPA-NAI-SORTFLG
           .
       40424-LIST04-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    並び替え内容
      *****************************************************************
       40424-LIST042-INIT-SEC       SECTION.
      *
           MOVE    ZERO        TO  FLG-HENOK
      *
           IF     (SPA-NAI-VENDFLG     =   ZERO )
               MOVE    1           TO  FLG-HENOK
           ELSE
               IF     (SPA-NAI-VENDORALL   =   "1" )
                   MOVE    1           TO  FLG-HENOK
               ELSE
                   IF      (SPA-NAI-VENDOR =   WRK-GMN-TVENDOR(IDX))
                       MOVE    1           TO  FLG-HENOK
                   END-IF
               END-IF
           END-IF
      *
           IF     (FLG-HENOK       =   1 )
               ADD     1                   TO  IDY
               ADD     1                   TO  SPA-TBL-MAX
               MOVE    WRK-GMN-TBLREC(IDX) TO  SPA-GMN-TBLREC(IDY)
               MOVE    IDY                 TO  SPA-GMN-TNUM(IDY)
           END-IF
      *
           .
       40424-LIST042-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ベンダー一覧
      *****************************************************************
       405-VENDOR-INIT-SEC         SECTION.
      *
      *    ベンダー一覧
           MOVE    1                   TO  SPA-NAI-VENDFLG
           MOVE    SPACE               TO  SPA-NAI-VENDOR
           MOVE    SPACE               TO  SPA-NAI-VENDORALL
           MOVE    ZERO                TO  SPA-NAI-SORTFLG
           PERFORM 404-LIST-INIT-SEC
           .
       405-VENDOR-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4001-SELNUM-CHK-SEC         SECTION.
      *
           MOVE    M80-SELNUM         TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TBL-MAX ) OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-SELNUM      =   SPA-GMN-TNUM  (IDX)
                   MOVE    IDX             TO  SPA-NAI-IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               INITIALIZE                  SPA-NAI-AREA
               IF      SPA-GMN-SELNUM      >   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           ELSE
      *H24.7
      *        ベンダー選択一覧
               IF      SPA-NAI-VENDFLG         =   1
      *            ベンダー選択
                   PERFORM 40012-SEL-VENDER-SYORI-SEC
               ELSE
      *            内容編集
                   PERFORM 40011-SEL-SYORI-SEC
               END-IF
           END-IF
      *
           .
       4001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       40011-SEL-SYORI-SEC         SECTION.
      *
      *    リンク判定
           IF      SPA-NAI-TLINK (SPA-NAI-IDX) =   "FALSE"
               MOVE    1                   TO  SPA-NAI-LINK
           ELSE
               MOVE    2                   TO  SPA-NAI-LINK
           END-IF
      *
           MOVE    SPA-GMN-TPGNAME (SPA-NAI-IDX)
                                       TO  SPA-NAI-PGNAME
           MOVE    SPA-GMN-TVERSION(SPA-NAI-IDX)
                                       TO  SPA-NAI-VERSION
           MOVE    SPA-GMN-TVERSION2(SPA-NAI-IDX)
                                       TO  SPA-NAI-VERSION2
      *
           MOVE    11                  TO  SPA-GMN-CUR
           .
       40011-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ベンダー選択処理
      *****************************************************************
       40012-SEL-VENDER-SYORI-SEC  SECTION.
      *
           MOVE    2                   TO  SPA-NAI-VENDFLG
           MOVE    ZERO                TO  SPA-NAI-SORTFLG
           IF      SPA-GMN-TVENDOR (SPA-NAI-IDX)   =   "全部"
               MOVE    SPACE               TO  SPA-NAI-VENDOR
               MOVE    "1"                 TO  SPA-NAI-VENDORALL
           ELSE
               MOVE    SPA-GMN-TVENDOR (SPA-NAI-IDX)
                                           TO  SPA-NAI-VENDOR
               MOVE    SPACE               TO  SPA-NAI-VENDORALL
           END-IF
           PERFORM 404-LIST-INIT-SEC
      *
           .
       40012-SEL-VENDER-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細リスト選択処理
      *****************************************************************
       4003-LIST-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               IF      M80-LIST-SELECT (IDX)   =   "T"
                   MOVE    SPA-GMN-TNUM (IDX)      TO  WRK-SELNUM
                   MOVE    SPA-TBL-MAX             TO  IDX
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO )
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
               MOVE    SPA-GMN-SELNUM      TO  M80-SELNUM
               PERFORM 4001-SELNUM-CHK-SEC
           END-IF
      *
           .
       4003-LIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-MAE-PAGE-SEC            SECTION.
      *
           IF      SPA-GMN-PAGE        >   1
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       -   1
               PERFORM 404-LIST-INIT-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       406-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       407-NEXT-PAGE-SEC           SECTION.
      *
           IF      SPA-GMN-NEXT        =   1
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       +   1
               PERFORM 404-LIST-INIT-SEC
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       407-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    インストール処理
      *****************************************************************
       409-INSTALL-SEC             SECTION.
      *
           IF     (SPA-GMN-SELNUM      =   ZERO )  OR
                  (SPA-NAI-IDX         =   ZERO )
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-NAI-LINK
                   WHEN    1
                       IF      SPA-MW              NOT =   SPA-PANDA
                           MOVE    "0101"              TO  SPA-MMID1CD
                       ELSE
                           IF     (SPA-NAI-VERSION     NOT =   SPACE)
                             AND  (SPA-NAI-VERSION     =
                                                       SPA-NAI-VERSION2)
                               MOVE    "0111"          TO  SPA-MMID1CD
                           ELSE
                               MOVE    "0101"          TO  SPA-MMID1CD
                           END-IF
                       END-IF
                   WHEN    2
                       MOVE    "0102"              TO  SPA-MMID1CD
               END-EVALUATE
           END-IF
           .
       409-INSTALL-EXT.
           EXIT.
      *****************************************************************
      *    ｕｐｇｒａｄｅ処理
      *****************************************************************
       410-UPGRADE-SEC             SECTION.
      *
           IF      SPA-NAI-TLINK (SPA-NAI-IDX) =   "ERROR"
               MOVE    "0111"              TO  SPA-MMID1CD
           ELSE
               MOVE    "0105"              TO  SPA-MMID1CD
           END-IF
           .
       410-UPGRADE-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC           SECTION.
      *
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    SHELL　ＣＡＬＬ処理
      *****************************************************************
       4010-SHELL-SYORI-SEC        SECTION.
      *
           MOVE    2                   TO  SPA-SHELLFLG
      *
           MOVE    SPA-NAI-PGNAME      TO  WRK-PACKAGE
           EVALUATE    WRK-COMMAND
               WHEN    "install"
               WHEN    "install+enable"
               WHEN    "upgrade"
               WHEN    "upgrade2"
                   MOVE    SPA-NAI-VERSION2    TO  WRK-VERSION
               WHEN    "disable"
               WHEN    "uninstall"
                   MOVE    SPA-NAI-VERSION     TO  WRK-VERSION
                   IF      SPA-NAI-VERSION     =   "-"
                       MOVE    SPA-NAI-VERSION2    TO  WRK-VERSION
                   END-IF
               WHEN    OTHER
                   MOVE    SPA-NAI-VERSION2    TO  WRK-VERSION
           END-EVALUATE
           PERFORM 4041-SHELL-CALL-SEC
           .
       4010-SHELL-SYORI-EXT.
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
           IF      SPA-MMID1CD     NOT =   SPACE
               PERFORM 520-MMIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "M80"           TO  MCP-WINDOW.
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
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO            TO  WRK-M80-LIST-ROW
           ELSE
               MOVE    M80-LIST-ROW    TO  WRK-M80-LIST-ROW
           END-IF
      *
           MOVE    SPACE           TO  M80
      *
      *    コラムリストの位置指定
           INITIALIZE                  M80
           MOVE    ZERO            TO  M80-LIST-ROW
           IF      WRK-M80-LIST-ROW    NOT =   ZERO
               MOVE    WRK-M80-LIST-ROW    TO  M80-LIST-ROW
           END-IF
      *
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL     IDX   >   SPA-TBL-MAX
               MOVE    SPA-GMN-TNUM(IDX)       TO  WRK-Z9
               MOVE    WRK-Z9                  TO  M80-TNUM(IDX)
               MOVE    SPA-GMN-TPGMEISYO(IDX)  TO  M80-TPGMEISYO(IDX)
               MOVE    SPA-GMN-TPGNAME(IDX)    TO  M80-TPGNAME(IDX)
               MOVE    SPA-GMN-TVERSION(IDX)   TO  M80-TVERSION(IDX)
               MOVE    SPA-GMN-TVERSION2(IDX)  TO  M80-TVERSION2(IDX)
               MOVE    SPA-GMN-TVENDOR(IDX)    TO  M80-TVENDOR(IDX)
               MOVE    SPA-GMN-TLINK(IDX)      TO  M80-TLINK(IDX)
               IF      SPA-GMN-TNUM(IDX)       =   SPA-GMN-SELNUM
                   MOVE    "T"                 TO  M80-LIST-SELECT(IDX)
               ELSE
                   MOVE    "F"                 TO  M80-LIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-TBL-MAX         TO  M80-LIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  M80-SELNUM
      *
      *    次頁あり
           IF      SPA-GMN-NEXT        =   1
               IF      SPA-GMN-PAGE        =   1
                   MOVE    "次頁有り"          TO  M80-MSGPAGE-VALUE
               ELSE
                   MOVE    "前頁・次頁有り"    TO  M80-MSGPAGE-VALUE
               END-IF
               MOVE    "red"               TO  M80-MSGPAGE-STYLE
           ELSE
               IF      SPA-GMN-PAGE        >   1
                   MOVE    "前頁有り"          TO  M80-MSGPAGE-VALUE
                   MOVE    "red"               TO  M80-MSGPAGE-STYLE
               ELSE
                   MOVE    SPACE               TO  M80-MSGPAGE-VALUE
                   MOVE    "black"             TO  M80-MSGPAGE-STYLE
               END-IF
           END-IF
      *
           EVALUATE    SPA-NAI-LINK
               WHEN    1
                   MOVE    "組込"              TO  M80-B09-LABEL
                   MOVE    WIDGET-NORMAL       TO  M80-B09-STATE
               WHEN    2
                   MOVE    "削除"              TO  M80-B09-LABEL
                   MOVE    WIDGET-NORMAL       TO  M80-B09-STATE
               WHEN    OTHER
                   MOVE    SPACE               TO  M80-B09-LABEL
                   MOVE    WIDGET-INSENSITIVE  TO  M80-B09-STATE
           END-EVALUATE
      *
           MOVE   SPA-GMN-SYORIMSG     TO  M80-SYORIMSG-VALUE
           EVALUATE    SPA-NAI-SYORIERR
               WHEN    1
                   MOVE    "red"           TO  M80-SYORIMSG-STYLE
               WHEN    2
                   MOVE    "blue"          TO  M80-SYORIMSG-STYLE
               WHEN    OTHER
                   MOVE    "black"         TO  M80-SYORIMSG-STYLE
           END-EVALUATE
      *
           MOVE   SPA-GMN-DURATION     TO  M80-DURATION
      *ver.4.7
           MOVE    SPACE               TO  M80-B03-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  M80-B03-STATE
           MOVE    SPACE               TO  M80-B10-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  M80-B10-STATE
      *
      *    明細があり
           IF     (SPA-TBL-MAX         >   ZERO )
               IF   (SPA-NAI-VENDFLG   NOT =   1 )
                   MOVE    "並び替え"          TO  M80-B03-LABEL
                   MOVE    WIDGET-NORMAL       TO  M80-B03-STATE
               END-IF
      *----(04.08.00)--UPD-START---
      *        未選択の時
      *        ginbee対応
               IF      SPA-MW              NOT =   SPA-GINBEE
               IF   (SPA-GMN-SELNUM        =   ZERO )
                   MOVE    "一括更新"          TO  M80-B10-LABEL
                   MOVE    WIDGET-NORMAL       TO  M80-B10-STATE
               ELSE
                   IF      SPA-NAI-LINK        =   2
                       MOVE    "個別更新"          TO  M80-B10-LABEL
                       MOVE    WIDGET-NORMAL       TO  M80-B10-STATE
                   END-IF
               END-IF
               END-IF
      *----(04.08.00)--UPD-END-----
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
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
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    09
                   MOVE    "B03"           TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B04"           TO  MCP-WIDGET
               WHEN    11
                   MOVE    "B09"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    11              TO  SPA-GMN-CUR
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
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                           TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "一覧表の内容がありません"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "選択番号を入力して下さい"
                                           TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "スクリプトの起動に失敗しました。"
                                           TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  MMERR
           MOVE    SPA-ERRCD       TO  MMERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  MMERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "M80"           TO  SPA-MOTOPG
           MOVE    "MMERR"         TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MMERR"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MMIDSET-SEC             SECTION.
      *
           MOVE    SPACE           TO  MMID1
      *
           INITIALIZE                  MMID1
           EVALUATE    SPA-MMID1CD
               WHEN    "0101"
               WHEN    "0111"
                   STRING  "選択したパッケージを組み込みます。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    MMID1-ID1MSG
                   END-STRING
               WHEN    "0102"
                   STRING  "選択したパッケージを削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    MMID1-ID1MSG
                   END-STRING
               WHEN    "0105"
                   STRING  "パッケージを更新します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO    MMID1-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-MMID1CD     TO  MMID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-MMID1CD     TO  MMID1-ID1CODE
           MOVE    SPACE           TO  SPA-MMID1-FLG
      *
           MOVE    "B12"           TO  MCP-WIDGET
      *
           MOVE    "M80"           TO  SPA-MOTOPG
           MOVE    "MMID1"         TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "MMID1"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           MOVE    1               TO  FLG-END
      *
           .
       520-MMIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理削除
      *****************************************************************
       820-JOBKANRI-DEL-SEC        SECTION.
      *
      *    ジョブ管理削除
           MOVE    "DEL"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           .
       820-JOBKANRI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理作成
      *****************************************************************
       820-JOBKANRI-JBS-SEC        SECTION.
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           .
       820-JOBKANRI-JBS-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ1-SEC      SECTION.
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
           .
       820-JOBKANRI-READ1-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC           SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE           TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)   TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)   TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)   TO  WRK-HDD
               MOVE    WRK-HENYMD      TO  WRK-HENYMDG
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
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ読込処理
      *****************************************************************
       900-PLUGNDATA-READ-SEC      SECTION.
      *
           READ    PLUGINLIST-FILE
               AT  END
                   MOVE    1               TO  FLG-DATAEND
               NOT AT  END
                   MOVE    ZERO            TO  FLG-DATAEND
           END-READ
           .
       900-PLUGNDATA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
