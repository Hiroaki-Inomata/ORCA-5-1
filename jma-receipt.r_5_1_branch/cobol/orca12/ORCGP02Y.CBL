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
       PROGRAM-ID.             ORCGP02Y.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者禁忌薬剤登録画面（Ｐ０２Ｙ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/12/03    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/12/09  患者禁忌薬剤期間対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PTKINKI         PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-INPUT           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-TENSU-OK        PIC 9(01).
           03  FLG-CHNGARI         PIC 9(01).
           03  FLG-P02Y1           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDX-TBL             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PIDMSG              PIC X(100).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-INPUTCD             PIC X(22).
           03  WRK-CD-MCNT             PIC 9(02).
           03  WRK-ICD-CDSYU           PIC X(01).
           03  WRK-CHNGCD              PIC X(09).
           03  WRK-KINKSTYMD           PIC X(08).
           03  WRK-TOUYOYMD            PIC X(08).
      *
           03  WRK-DATA                PIC X(100).
           03  WRK-MAX-CNT             PIC 9(03).
      *
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-TIME.
               05  WRK-THH             PIC 9(02).
               05  WRK-TMM             PIC 9(02).
               05  WRK-TSS             PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-P02Y-ROW        PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH-H       PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM-H       PIC Z9.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者禁忌薬剤マスタ
       01  PTKINKI-REC.
           COPY    "CPPTKINKI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ−
      *01  TNSWK-TENSU-REC.
      **** COPY    "CPTENSUWK.INC".
           COPY    "CPTENSU.INC"      REPLACING
                                     //TNS-// BY //TNSWK-//.
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *
      *    同一患者識別テーブル
       01  PTSAME-REC.
           COPY    "CPPTSAME.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    検索文字追加
           COPY    "CPORCSADDSIGN.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA12SCRAREA.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
           END-EVALUATE
      *
      *    画面編集後、表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02Y"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *        検索画面より
               WHEN    "P02Y1"
                   PERFORM 3002-P02Y1-INIT-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-SCREEN-INIT-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索一覧画面より
      *****************************************************************
       3002-P02Y1-INIT-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-P02Y1-SRYCD (IDX)   NOT =   SPACE
                   MOVE    SPA-P02Y1-SRYCD (IDX)   TO  WRK-SRYCD
                   MOVE    SPACE                   TO  WRK-KINKSTYMD
                   MOVE    SPACE                   TO  WRK-TOUYOYMD
                   MOVE    1                       TO  FLG-P02Y1
      *            患者禁忌薬剤登録処理
                   PERFORM 4121-PTKINKI-INS-SEC
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-ERRCD
           PERFORM 3000-SCREEN-INIT-SEC
      *
           .
       3002-P02Y1-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *
           INITIALIZE                      SPA-P02Y-AREA
      *
           MOVE    SPA-HOSPNUM         TO  SPA-P02Y-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SPA-P02Y-PTID
      *
      *    禁忌薬剤編集
           MOVE    1                   TO  SPA-P02Y-PAGE
           PERFORM 3001-PTKINKI-HENSYU-SEC
           IF      SPA-P02Y-MAX        =   ZERO
               MOVE    2                   TO  SPA-P02Y-CUR
           ELSE
               MOVE    1                   TO  SPA-P02Y-CUR
           END-IF
      *
      *    同一患者識別テーブル
           IF     (SPA-NAI-GRPHOSPNUM  =   ZERO )
           AND    (SPA-NAI-GRPPTID     =   ZERO )
               CONTINUE
           ELSE
               PERFORM 30001-PTSAME-HEN-SEC
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤編集処理
      *****************************************************************
       3001-PTKINKI-HENSYU-SEC              SECTION.
      *
           INITIALIZE                      SPA-P02Y-INPUT-G
           MOVE    ZERO                TO  SPA-P02Y-SELNUM
      *
           MOVE    1                   TO  FLG-GMN-INIT
           MOVE    ZERO                TO  SPA-P02Y-MAX
           MOVE    ZERO                TO  SPA-P02Y-NEXT
           INITIALIZE                      SPA-P02Y-KINKILIST
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-P02Y-HOSPNUM    TO  PTKINKI-HOSPNUM
           MOVE    SPA-P02Y-PTID       TO  PTKINKI-PTID
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE                      PTKINKI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           COMPUTE IDS                 =  (SPA-P02Y-PAGE   -  1  )
                                       *   100
                                       +   1
           PERFORM UNTIL  ( FLG-PTKINKI    =   1  )
                   OR     ( IDX            >=  100)
               ADD    1                    TO  IDY
               IF     IDY                  >=  IDS
                   PERFORM 30011-PTKINKI-HEN-SEC
               END-IF
      *
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           END-PERFORM
           IF     (IDX                 >=  100 )  AND
                  (FLG-PTKINKI         =   ZERO)
               MOVE    1                   TO  SPA-P02Y-NEXT
           END-IF
           MOVE    IDX                 TO  SPA-P02Y-MAX
      *
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-PTKINKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表編集処理
      *****************************************************************
       30011-PTKINKI-HEN-SEC              SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    IDX                 TO  SPA-P02Y-TNUM (IDX)
           MOVE    PTKINKI-SRYCD       TO  SPA-P02Y-TSRYCD(IDX)
      *    点数マスタ検索
           MOVE    PTKINKI-SRYCD       TO  WRK-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU-OK        =   1
               MOVE    TNS-NAME            TO  SPA-P02Y-TNAME (IDX)
               MOVE    TNS-KANANAME        TO  SPA-P02Y-TKANANAME (IDX)
               MOVE    TNS-YUKOEDYMD       TO  SPA-P02Y-TYUKOEDYMDS(IDX)
               MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P02Y-TYUKOEDYMD(IDX)
      *        入力ＣＤ
               MOVE    PTKINKI-SRYCD       TO  WRK-SRYCD
               PERFORM 400211-INPUTCD-HEN-SEC
               MOVE    ICD-INPUTCD         TO  SPA-P02Y-TINPUTCD(IDX)
      *        禁忌開始日
               MOVE    PTKINKI-KINKSTYMD   TO  SPA-P02Y-TKINKSTYMD(IDX)
               MOVE    PTKINKI-KINKSTYMD   TO  WRK-SYMD
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P02Y-TKINKSTYMDG(IDX)
      *        投与開始日
               MOVE    PTKINKI-TOUYOYMD    TO  SPA-P02Y-TTOUYOYMD(IDX)
               MOVE    PTKINKI-TOUYOYMD    TO  WRK-SYMD
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-P02Y-TTOUYOYMDG(IDX)
      *        経過措置コード
               PERFORM 30011-SRYCDCHG-HEN-SEC
               MOVE    WRK-CHNGCD          TO  SPA-P02Y-TCHNGECD (IDX)
               MOVE    FLG-CHNGARI         TO  SPA-P02Y-TCHNGFLG (IDX)
           END-IF
           .
       30011-PTKINKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    経過措置コード処理
      *****************************************************************
       30011-SRYCDCHG-HEN-SEC          SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    WRK-SRYCD           TO  CHG-IPNSRYCD
           MOVE    SPA-P02Y-HOSPNUM    TO  CHG-HOSPNUM
      *
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
           MOVE    SPACE               TO  WRK-CHNGCD
           MOVE    ZERO                TO  FLG-CHNGARI
           IF      FLG-SRYCDCHG        =   ZERO
               MOVE    CHG-RJNSRYCD        TO  WRK-CHNGCD
      *        登録の有無判定
               INITIALIZE                      PTKINKI-REC
               MOVE    SPA-P02Y-HOSPNUM    TO  PTKINKI-HOSPNUM
               MOVE    SPA-P02Y-PTID       TO  PTKINKI-PTID
               MOVE    CHG-RJNSRYCD        TO  PTKINKI-SRYCD
               MOVE    PTKINKI-REC         TO  MCPDATA-REC
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkinki"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 900-PTKINKI-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-PTKINKI
                   INITIALIZE                      PTKINKI-REC
               END-IF
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-PTKINKI         =   ZERO
                   MOVE    1                   TO  FLG-CHNGARI
               ELSE
                   MOVE    ZERO                TO  FLG-CHNGARI
               END-IF
           END-IF
           .
       30011-SRYCDCHG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入力ＣＤ処理
      *****************************************************************
       400211-INPUTCD-HEN-SEC          SECTION.
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-P02Y-HOSPNUM    TO  ICD-HOSPNUM
           MOVE    WRK-SRYCD           TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 930-INPUTCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
               INITIALIZE                  INPUTCD-REC
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       400211-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一患者識別編集処理
      *****************************************************************
       30001-PTSAME-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      PTSAME-REC
           MOVE    SPA-NAI-GRPHOSPNUM  TO  PTSAME-HOSPNUM
           MOVE    SPA-NAI-GRPPTID     TO  PTSAME-PTID
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTSAME-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTSAME
           END-IF
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  IDX
           IF      FLG-PTSAME          =   ZERO
      *        同一患者情報
               MOVE    PTSAME-REC          TO  MCPDATA-REC
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTSAME
               END-IF
               PERFORM UNTIL    FLG-PTSAME     =   1
                   IF     (PTSAME-HOSPNUM      =   SPA-HOSPNUM   )
                     AND  (PTSAME-PTID         =   SPA-GMN-PTID  )
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX
                       MOVE    PTSAME-HOSPNUM  TO  SPA-P02Y-LGRPHOSPNUM
                                                                (IDX)
                       MOVE    PTSAME-PTID     TO  SPA-P02Y-GRPPTID
                                                                (IDX)
                   END-IF
      *
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               END-PERFORM
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           ELSE
               ADD     1                   TO  IDX
               MOVE    SPA-NAI-GRPHOSPNUM  TO  SPA-P02Y-LGRPHOSPNUM
                                                             (IDX)
               MOVE    SPA-NAI-GRPPTID     TO  SPA-P02Y-GRPPTID
                                                             (IDX)
           END-IF
           MOVE    IDX                 TO  SPA-P02Y-GRPMAX
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >    SPA-P02Y-GRPMAX
      *        医療機関名称
               PERFORM VARYING     IDY     FROM    1  BY  1
                       UNTIL      (IDY     >    9  )
                               OR (SPA-GRPHOSPNUM (IDY)   =   SPACE )
                   IF      SPA-GRPHOSPNUM (IDY)
                                           =   SPA-P02Y-LGRPHOSPNUM(IDX)
                       MOVE    SPA-GRPHOSPNAME(IDY)
                                           TO  SPA-P02Y-LGRPHOSPNAME
                                                                (IDX)
                       MOVE    9           TO  IDY
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       30001-PTSAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
      *            削除
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4031-DELETE-SEC
                   END-IF
               WHEN    "0102"
      *            他院複写
                   IF      SPA-PID1-FLG        =   "OK"
                       PERFORM 4101-GRPCOPY-SEC
                   END-IF
               WHEN    "0103"
      *            経過措置複写
                   PERFORM 411-CHGCOPY-SYORI-SEC
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-P02Y-MAX        =   ZERO )
               MOVE    ZERO                TO  WRK-P02Y-ROW
           ELSE
               MOVE    P02Y-ROW            TO  WRK-P02Y-ROW
           END-IF
      *
           MOVE    SPACE               TO  P02Y
           INITIALIZE                      P02Y
      *    コラムリストの位置指定
           IF      WRK-P02Y-ROW    NOT =   ZERO
               MOVE    WRK-P02Y-ROW        TO  P02Y-ROW
           END-IF
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P02Y-MAX
               MOVE    SPA-P02Y-TNUM   (IDX)   TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  P02Y-TNUM  (IDX)
               MOVE    SPA-P02Y-TSRYCD (IDX)   TO  P02Y-TSRYCD (IDX)
               MOVE    SPA-P02Y-TINPUTCD(IDX)  TO  P02Y-TINPCD (IDX)
               MOVE    SPA-P02Y-TNAME  (IDX)
                                               TO  P02Y-TNAME (IDX)
               MOVE    SPA-P02Y-TYUKOEDYMD (IDX)
                                               TO  P02Y-TYUKOEDYMD (IDX)
               MOVE    SPA-P02Y-TKINKSTYMDG(IDX)
                                               TO  P02Y-TKINKSTYMD (IDX)
               MOVE    SPA-P02Y-TTOUYOYMDG (IDX)
                                               TO  P02Y-TTOUYOYMD (IDX)
               IF      SPA-P02Y-TCHNGFLG (IDX) =   "1"
                   CONTINUE
               ELSE
                   MOVE    SPA-P02Y-TCHNGECD (IDX)
                                               TO  P02Y-TCHNGCD (IDX)
               END-IF
               IF      SPA-P02Y-TNUM (IDX)     =   SPA-P02Y-SELNUM
                   MOVE    "T"                 TO  P02Y-SELECT (IDX)
                   MOVE    IDX                 TO  P02Y-ROW
               ELSE
                   MOVE    "F"                 TO  P02Y-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-P02Y-MAX        TO  P02Y-COUNT
      *
           MOVE    SPA-P02Y-SELNUM     TO  P02Y-SELNUM
           MOVE    SPA-P02Y-SRYCD      TO  P02Y-SRYCD
           MOVE    SPA-P02Y-SRYCDMEI   TO  P02Y-SRYMEI
           MOVE    SPA-P02Y-KIKAN      TO  P02Y-YUKOKIKAN
           MOVE    SPA-P02Y-KEIKAMSG   TO  P02Y-KEIKAMSG
           MOVE    SPA-P02Y-INPUTCD    TO  P02Y-INPUTCDMSG
           MOVE    SPA-P02Y-KINKSTYMDG TO  P02Y-KINKSTYMD
           MOVE    SPA-P02Y-TOUYOYMDG  TO  P02Y-TOUYOYMD
      *
           MOVE    SPA-P02Y-GRPHOSPNUM-G   TO  P02Y-GRPHOSPNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >    SPA-P02Y-GRPMAX
               MOVE    SPA-P02Y-GRPHOSPNUM-LST (IDX)
                                       TO  P02Y-GRPHOSPNUM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-P02Y-GRPMAX     TO  P02Y-GRPHOSPNUM-COUNT
      *
           IF      SPA-P02Y-GRPMAX     =   ZERO
               MOVE    SPACE               TO  P02Y-B09-LABEL
               MOVE    SPACE               TO  P02Y-B10-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  P02Y-B09-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P02Y-B10-STATE
           ELSE
               MOVE    "F9 他院患者"       TO  P02Y-B09-LABEL
               MOVE    WIDGET-NORMAL       TO  P02Y-B09-STATE
               MOVE    WIDGET-NORMAL       TO  P02Y-B10-STATE
               IF      SPA-P02Y-GRPHOSPNUM     =   SPACE
                   MOVE    SPACE               TO  P02Y-B10-LABEL
                   MOVE    WIDGET-INSENSITIVE  TO  P02Y-B10-STATE
               ELSE
                   MOVE    "F10 他院複写"      TO  P02Y-B10-LABEL
               END-IF
           END-IF
      *
           IF      SPA-P02Y-GRPHOSPNUM     =   SPACE
               MOVE    WIDGET-NORMAL       TO  P02Y-B03-STATE
               MOVE    WIDGET-NORMAL       TO  P02Y-B08-STATE
               MOVE    WIDGET-NORMAL       TO  P02Y-B12-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P02Y-B03-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P02Y-B08-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P02Y-B12-STATE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-P02Y-CUR        =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "SELNUM"
      ****             MOVE    93           TO  SPA-P02Y-CUR
                       MOVE    05           TO  SPA-P02Y-CUR
                   WHEN    "SRYCD"
                       MOVE    05           TO  SPA-P02Y-CUR
                   WHEN    "TOUYOYMD"
                       MOVE    06           TO  SPA-P02Y-CUR
                   WHEN    "KINKSTYMD"
                       MOVE    99           TO  SPA-P02Y-CUR
                   WHEN    OTHER
                       MOVE    1            TO  SPA-P02Y-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-P02Y-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SRYCD"         TO  MCP-WIDGET
               WHEN    03
                   MOVE    "GRPHOSPNUM"    TO  MCP-WIDGET
               WHEN    05
                   MOVE    "TOUYOYMD"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "KINKSTYMD"     TO  MCP-WIDGET
               WHEN    93
                   MOVE    "B03"           TO  MCP-WIDGET
               WHEN    98
                   MOVE    "B10"           TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 403-DELETE-SYORI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 405-ZENPAGE-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 406-NEXTPAGE-SEC
      *    検索
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 408-P02Y1-SYORI-SEC
      *    紐付患者
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-GRPHOSPNUM-SEC
      *    紐付複写
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 410-GRPCOPY-SYORI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-TOUROKU-SYORI-SEC
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
           MOVE    ZERO                TO  SPA-P02Y-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4001-SELNUM-CHK-SEC
      *        医療機関番号
               WHEN    "ACTIVATE"      ALSO    "GRPHOSPNUM"
                   PERFORM 4001-GRPHOSPNUM-CHK-SEC
      *        リスト選択
               WHEN    ANY             ALSO    "PTKINKILST"
                   PERFORM 4001-KINKLIST-SEL-SEC
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表選択処理
      *****************************************************************
       4001-KINKLIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P02Y-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02Y-MAX
               IF      P02Y-SELECT (IDX)   =   "T"
                   MOVE    SPA-P02Y-TNUM (IDX)
                                           TO  SPA-P02Y-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-P02Y-SELNUM    NOT =   ZERO
               MOVE    SPA-P02Y-SELNUM        TO  P02Y-SELNUM
               PERFORM 4001-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-KINKLIST-SEL-EXT.
           EXIT.
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       4001-SELNUM-CHK-SEC                 SECTION.
      *
           INITIALIZE                  SPA-P02Y-INPUT-G
           MOVE    P02Y-SELNUM         TO  SPA-P02Y-SELNUM
      *
           MOVE    ZERO                TO  IDX-TBL
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02Y-MAX
               IF      SPA-P02Y-TNUM (IDX) =   SPA-P02Y-SELNUM
                   MOVE    IDX             TO  IDX-TBL
                   MOVE    SPA-P02Y-MAX    TO  IDX
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  SPA-P02Y-IDX
           IF      SPA-P02Y-SELNUM     =   ZERO
               MOVE    2                   TO  SPA-P02Y-CUR
           ELSE
               IF      IDX-TBL             =   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-P02Y-CUR
               ELSE
                   PERFORM 40011-SRYCD-HENSYU-SEC
               END-IF
           END-IF
           .
       4001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧選択編集処理
      *****************************************************************
       40011-SRYCD-HENSYU-SEC          SECTION.
      *
           MOVE    SPA-P02Y-TSRYCD (IDX-TBL)
                                       TO  SPA-P02Y-SRYCD
           MOVE    SPA-P02Y-SRYCD      TO  P02Y-SRYCD
           MOVE    IDX-TBL             TO  SPA-P02Y-IDX
      *    薬剤コード決定編集
           PERFORM 40021-SRYCD-HEN-SEC
           PERFORM 4002-KINKSTYMD-CHK-SEC
           PERFORM 4002-TOUYOYMD-CHK-SEC
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    SPA-P02Y-SRYCD      TO  SPA-P02Y-MAE-SRYCD
               MOVE    5                   TO  SPA-P02Y-CUR
           END-IF
           .
       40011-SRYCD-HENSYU-EXT.
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
           PERFORM 4002-SRYCD-CHK-SEC
      *
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               PERFORM 4002-TOUYOYMD-CHK-SEC
           END-IF
      *
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               PERFORM 4002-KINKSTYMD-CHK-SEC
           END-IF
           .
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02Y-SRYCD          TO  SPA-P02Y-SRYCD
           MOVE    P02Y-KINKSTYMD      TO  SPA-P02Y-KINKSTYMDG
           MOVE    P02Y-TOUYOYMD       TO  SPA-P02Y-TOUYOYMDG
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤コード処理
      *****************************************************************
       4002-SRYCD-CHK-SEC                 SECTION.
      *
           IF     (SPA-P02Y-SRYCD      =   SPA-P02Y-MAE-SRYCD)
             OR   (SPA-P02Y-SRYCD      =   SPACE             )
               MOVE    SPA-P02Y-SRYCD      TO  SPA-P02Y-MAE-SRYCD
               IF     (SPA-P02Y-SRYCD          =   SPACE    )
                   INITIALIZE                  SPA-P02Y-INPUT-G
                   MOVE    2                   TO  SPA-P02Y-CUR
               END-IF
               IF     (SPA-P02Y-GRPHOSPNUM NOT =   SPACE    )
                   MOVE    98                  TO  SPA-P02Y-CUR
               END-IF
               GO      TO      4002-SRYCD-CHK-EXT
           END-IF
      *
           MOVE    2                   TO  SPA-P02Y-CUR
           INITIALIZE                  SPA-P02Y-INPUT-G
      *    全角入力の時、４文字まで
           MOVE    P02Y-SRYCD          TO  WRK-DATA
           MOVE    9                   TO  WRK-MAX-CNT
           PERFORM 810-ORCSKANACHK-SEC
           MOVE    WRK-DATA            TO  SPA-P02Y-SRYCD
      *
           IF     (SPA-P02Y-SRYCD      =   SPACE             )
               MOVE    SPA-P02Y-SRYCD      TO  SPA-P02Y-MAE-SRYCD
               GO      TO      4002-SRYCD-CHK-EXT
           END-IF
           IF     (SPA-P02Y-GRPHOSPNUM NOT =   SPACE    )
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    SPACE               TO  SPA-P02Y-SRYCD
               MOVE    SPACE               TO  SPA-P02Y-MAE-SRYCD
               MOVE    SPACE               TO  SPA-P02Y-KINKSTYMD
               MOVE    SPACE               TO  SPA-P02Y-TOUYOYMD
               MOVE    SPACE               TO  SPA-P02Y-KINKSTYMDG
               MOVE    SPACE               TO  SPA-P02Y-TOUYOYMDG
               MOVE    ZERO                TO  SPA-P02Y-SELNUM
               MOVE    98                  TO  SPA-P02Y-CUR
               GO      TO      4002-SRYCD-CHK-EXT
           END-IF
      *
      *    "//" 入力の時、検索画面へ
           IF      SPA-P02Y-SRYCD(1:2)  =  "//"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 408-P02Y1-SYORI-SEC
           ELSE
      *        入力コードチェック
               MOVE    SPA-P02Y-SRYCD      TO  WRK-INPUTCD
               PERFORM 40021-SRYCD-INCHK-SEC
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               IF      SPA-P02Y-SRYCD(1:1)     =   "6"
                   CONTINUE
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        薬剤コード決定編集
               PERFORM 40021-SRYCD-HEN-SEC
               PERFORM 40023-LIST-CHK-SEC
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    SPA-P02Y-SRYCD      TO  SPA-P02Y-MAE-SRYCD
               MOVE    5                   TO  SPA-P02Y-CUR
      ***      IF      SPA-P02Y-SELNUM     =   ZERO
      *            MOVE    99                  TO  SPA-P02Y-CUR
      *        ELSE
      *            MOVE    93                  TO  SPA-P02Y-CUR
      ****     END-IF
           END-IF
           .
       4002-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　入力コードチェック処理
      *****************************************************************
       40023-LIST-CHK-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02Y-MAX
               IF      SPA-P02Y-TSRYCD (IDX)   =   SPA-P02Y-SRYCD
                   MOVE    SPA-P02Y-TNUM (IDX) TO  SPA-P02Y-SELNUM
                   MOVE    IDX                 TO  SPA-P02Y-IDX
                   MOVE    SPA-P02Y-MAX        TO  IDX
               END-IF
           END-PERFORM
           .
       40023-LIST-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　入力コードチェック処理
      *****************************************************************
       40021-SRYCD-INCHK-SEC            SECTION.
      *
           MOVE    ZERO               TO  WRK-CD-MCNT
           MOVE    ZERO               TO  FLG-KANJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-INPUTCD         TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-INPUTCD
           IF      KANACHK-RC      NOT =   ZERO
      *        全角半角混在エラー
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *    半角で９文字の時、点数マスタチェックへ
               IF     (WRK-CD-MCNT         =   9    )  AND
                      (FLG-KANJI           =   ZERO )  AND
                      (WRK-INPUTCD         NUMERIC  )
                   CONTINUE
               ELSE
                   PERFORM 400211-INPUTCD-CHK-SEC
                   IF     (FLG-INPUT       =   ZERO ) AND
                          (SPA-ERRCD       =   SPACE )
      *                入力コード検索
                       PERFORM 408-P02Y1-SYORI-SEC
                   END-IF
               END-IF
           END-IF
           .
       40021-SRYCD-INCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力ＣＤ処理
      *****************************************************************
       400211-INPUTCD-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUT
           MOVE    SPACE               TO  WRK-ICD-CDSYU
           IF      FLG-KANJI           =   1
      *        全角のコードチェック
               MOVE    "J"             TO  WRK-ICD-CDSYU
           ELSE
      *        診療コードチェック
               IF      WRK-INPUTCD(1:1)        =   "P"  OR  "S"
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   IF     (WRK-CD-MCNT     >   1   AND  <=  9 )  AND
                          (WRK-INPUTCD(1:WRK-CD-MCNT)   NUMERIC    )
                       EVALUATE    WRK-CD-MCNT
                           WHEN    9
      *                    診療行為コード入力
                               MOVE    1           TO  FLG-INPUT
      *                    短縮３桁
                           WHEN    3
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *                    ４桁
                           WHEN    4
                               MOVE    "4"         TO  WRK-ICD-CDSYU
      *                    ５桁
                           WHEN    5
                               MOVE    "5"         TO  WRK-ICD-CDSYU
      *                    ６桁
                           WHEN    6
                               MOVE    "6"         TO  WRK-ICD-CDSYU
      *
                           WHEN    OTHER
                               MOVE    "A"         TO  WRK-ICD-CDSYU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"         TO  WRK-ICD-CDSYU
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-ICD-CDSYU   NOT =   SPACE
               PERFORM 4002110-INPUTCD-HEN-SEC
           END-IF
           .
       400211-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力コード処理
      *****************************************************************
       4002110-INPUTCD-HEN-SEC                 SECTION.
      *
           INITIALIZE                  INPUTCD-REC
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    WRK-ICD-CDSYU       TO  ICD-CDSYU
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
           COMPUTE IDK                 =   WRK-CD-MCNT  +  1
           MOVE    "%"                 TO  ICD-INPUTCD(IDK:1)
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 930-INPUTCD-READ-SEC
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (WRK-INPUTCD         =   ICD-INPUTCD )
      *        点数コード決定
               MOVE    ICD-SRYCD           TO  SPA-P02Y-SRYCD
               MOVE    1                   TO  FLG-INPUT
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4002110-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    薬剤コード処理
      *****************************************************************
       40021-SRYCD-HEN-SEC                 SECTION.
      *
      *    点数マスタ検索
           MOVE    SPA-P02Y-SRYCD      TO  WRK-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU-OK        =   1
               MOVE    TNS-NAME            TO  SPA-P02Y-SRYCDMEI
               MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
               PERFORM 800-SE-WAFORMAT-YMDEDIT-SEC
               STRING  "有効終了日："
                       WRK-HENYMD      DELIMITED  BY  SIZE
                                       INTO    SPA-P02Y-KIKAN
               END-STRING
      *        経過措置コード
               PERFORM 30011-SRYCDCHG-HEN-SEC
               IF     (WRK-CHNGCD      NOT =   SPACE ) AND
                      (FLG-CHNGARI         =   ZERO  )
                   MOVE    WRK-CHNGCD          TO  SPA-P02Y-CHNGCD
                   MOVE    WRK-CHNGCD          TO  WRK-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   MOVE    SPACE               TO  WRK-DATA
                   STRING  "品目移行コード："
                            WRK-CHNGCD     DELIMITED  BY  SPACE
                            "　"           DELIMITED  BY  SIZE
                            TNS-NAME       DELIMITED  BY  SPACE
                                           INTO    WRK-DATA
                   END-STRING
                   MOVE    56                  TO  WRK-MAX-CNT
                   PERFORM 810-ORCSKANACHK-SEC
                   MOVE    WRK-DATA            TO  SPA-P02Y-KEIKAMSG
               END-IF
      *        入力コード
               MOVE    SPA-P02Y-SRYCD      TO  WRK-SRYCD
               PERFORM 400211-INPUTCD-HEN-SEC
               MOVE    SPACE               TO  WRK-DATA
               IF      ICD-INPUTCD     NOT =   SPACE
                   STRING  "入力ＣＤ："
                           ICD-INPUTCD         DELIMITED  BY  SPACE
                                               INTO  WRK-DATA
                   END-STRING
                   MOVE    21                  TO  WRK-MAX-CNT
                   PERFORM 810-ORCSKANACHK-SEC
               END-IF
               MOVE    WRK-DATA            TO  SPA-P02Y-INPUTCD
           ELSE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-P02Y-CUR
           END-IF
      *
      *    患者禁忌薬剤検索処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 900-PTKINKI-SELECT-SEC
               IF      FLG-PTKINKI         =   ZERO
                   MOVE    PTKINKI-KINKSTYMD   TO  SPA-P02Y-KINKSTYMDG
                   MOVE    PTKINKI-TOUYOYMD    TO  SPA-P02Y-TOUYOYMDG
               ELSE
                   MOVE    SPACE               TO  SPA-P02Y-KINKSTYMDG
                   MOVE    SPACE               TO  SPA-P02Y-TOUYOYMDG
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-P02Y-SRYCD      TO  SPA-P02Y-MAE-SRYCD
           END-IF
           .
       40021-SRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌開始日チェック処理
      *****************************************************************
       4002-KINKSTYMD-CHK-SEC                 SECTION.
      *
           IF      SPA-P02Y-KINKSTYMDG =   SPACE
                                       OR  ZERO
               MOVE    SPACE               TO  SPA-P02Y-KINKSTYMD
               MOVE    SPACE               TO  SPA-P02Y-KINKSTYMDG
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-P02Y-KINKSTYMDG TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-P02Y-KINKSTYMDG
                   MOVE    SGDAY-SDAY          TO  SPA-P02Y-KINKSTYMD
               ELSE
                   MOVE    "0005"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-P02Y-CUR
               END-IF
           END-IF
           .
       4002-KINKSTYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    投与日チェック処理
      *****************************************************************
       4002-TOUYOYMD-CHK-SEC                 SECTION.
      *
           IF      SPA-P02Y-TOUYOYMDG  =   SPACE
                                       OR  ZERO
               MOVE    SPACE               TO  SPA-P02Y-TOUYOYMD
               MOVE    SPACE               TO  SPA-P02Y-TOUYOYMDG
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-P02Y-TOUYOYMDG  TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-P02Y-TOUYOYMDG
                   MOVE    SGDAY-SDAY          TO  SPA-P02Y-TOUYOYMD
               ELSE
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-P02Y-CUR
               END-IF
           END-IF
           .
       4002-TOUYOYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-P02Y-SELNUM
           MOVE    ZERO                TO  SPA-P02Y-IDX
           INITIALIZE                      SPA-P02Y-INPUT-G
           MOVE    1                   TO  SPA-P02Y-CUR
      *
           MOVE    SPA-HOSPNUM         TO  SPA-P02Y-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  SPA-P02Y-PTID
           MOVE    SPACE               TO  SPA-P02Y-GRPHOSPNUM-G
      *
           .
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       403-DELETE-SYORI-SEC             SECTION.
      *
           IF     (SPA-P02Y-SELNUM     =   ZERO )  OR
                  (SPA-P02Y-SELNUM     >   SPA-P02Y-MAX)  OR
                  (SPA-P02Y-IDX        =   ZERO  )
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF     (SPA-P02Y-SRYCD      =   SPA-P02Y-TSRYCD
                                                     (SPA-P02Y-IDX))
                   MOVE    "0101"              TO  SPA-PIDCD
               ELSE
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       403-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DELETE-SEC             SECTION.
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKINKI-PTID
           MOVE    SPA-P02Y-SRYCD      TO  PTKINKI-SRYCD
      *
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P02Y PTKINKI KEY DEL ERR:"  MCP-RC
                       ",KEY:" PTKINKI-KEY
           END-IF
      *
           IF     SPA-ERRCD            =   SPACE
               PERFORM 410-INIT-SYORI-SEC
           END-IF
           .
       4031-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新後初期編集処理
      *****************************************************************
       410-INIT-SYORI-SEC             SECTION.
      *
           PERFORM  402-CLEAR-SEC
      *
      *    禁忌薬剤編集
           MOVE    1                   TO  SPA-P02Y-PAGE
           PERFORM 3001-PTKINKI-HENSYU-SEC
           IF      SPA-P02Y-MAX        =   ZERO
               MOVE    2                   TO  SPA-P02Y-CUR
           ELSE
               MOVE    1                   TO  SPA-P02Y-CUR
           END-IF
           .
       410-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
        405-ZENPAGE-SEC             SECTION.
      *
           IF      SPA-P02Y-PAGE       >   1
               PERFORM  402-CLEAR-SEC
               COMPUTE SPA-P02Y-PAGE   =   SPA-P02Y-PAGE   -   1
               PERFORM 3001-PTKINKI-HENSYU-SEC
               MOVE    1                   TO  SPA-P02Y-CUR
           ELSE
               MOVE    "0010"              TO  SPA-ERRCD
           END-IF
           .
        405-ZENPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
        406-NEXTPAGE-SEC             SECTION.
      *
           IF      SPA-P02Y-NEXT       =   1
               PERFORM 402-CLEAR-SEC
               COMPUTE SPA-P02Y-PAGE   =   SPA-P02Y-PAGE   +   1
               PERFORM 3001-PTKINKI-HENSYU-SEC
               MOVE    1                   TO  SPA-P02Y-CUR
           ELSE
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
           .
        406-NEXTPAGE-EXT.
           EXIT.
      *****************************************************************
      *    検索処理
      *****************************************************************
       408-P02Y1-SYORI-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-P02Y1-AREA
           INITIALIZE                  SPA-P02Y1GMN-AREA 
           MOVE    "2"                 TO  SPA-P02Y1-TYPE
           MOVE    "P02Y"              TO  SPA-MOTOPG
      *
           IF      WRK-INPUTCD         NOT =   SPACE
      *        内服
               MOVE    "3"                 TO  SPA-P02Y1-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-P02Y1-INPUTCD
               MOVE    "1"                 TO  SPA-P02Y1-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-P02Y1-CDSYUBETU
               ELSE
                   IF      WRK-INPUTCD(1:1)    NUMERIC
                       EVALUATE    WRK-CD-MCNT
                           WHEN    3
                           WHEN    6
                               MOVE    "6"     TO  SPA-P02Y1-CDSYUBETU
                           WHEN    4
                               MOVE    "4"     TO  SPA-P02Y1-CDSYUBETU
                           WHEN    5
                               MOVE    "5"     TO  SPA-P02Y1-CDSYUBETU
                       END-EVALUATE
                   ELSE
                       MOVE    "A"             TO  SPA-P02Y1-CDSYUBETU
                   END-IF
               END-IF
      *        画面編集
               PERFORM 41011-P02Y1-SET-SEC
           ELSE
      *        画面編集
               PERFORM 41011-P02Y1-SET-SEC
           END-IF
      *
      *
           MOVE    "P02Y"              TO  SPA-MOTOPG
           MOVE    "P02Y1"             TO  SPA-SAKIPG
      *
           IF      SPA-P02Y1-GMN-MAX   =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "P02Y1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       408-P02Y1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面編集処理
      *****************************************************************
       41011-P02Y1-SET-SEC             SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02Y1"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
           .
       41011-P02Y1-SET-EXT.
           EXIT.
      *****************************************************************
      *    紐付患者処理
      *****************************************************************
       409-GRPHOSPNUM-SEC            SECTION.
      *
           IF      SPA-P02Y-GRPHOSPNUM     =   SPACE
               MOVE    SPA-P02Y-GRPHOSPNUM-LST (1)
                                           TO  P02Y-GRPHOSPNUM
           ELSE
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-P02Y-GRPMAX
                   IF      SPA-P02Y-GRPHOSPNUM
                                       =   SPA-P02Y-LGRPHOSPNUM (IDX)
                       IF      IDX         =   SPA-P02Y-GRPMAX
                           MOVE    SPACE           TO  P02Y-GRPHOSPNUM
                       ELSE
                           MOVE    SPA-P02Y-GRPHOSPNUM-LST (IDX + 1)
                                                   TO  P02Y-GRPHOSPNUM
                       END-IF
                       MOVE    SPA-P02Y-GRPMAX     TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           PERFORM 4001-GRPHOSPNUM-CHK-SEC
           .
       409-GRPHOSPNUM-SEC-EXT.
           EXIT.
      *****************************************************************
      *    紐付患者処理
      *****************************************************************
       4001-GRPHOSPNUM-CHK-SEC            SECTION.
      *
           MOVE    P02Y-GRPHOSPNUM     TO  SPA-P02Y-GRPHOSPNUM-G
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02Y-GRPMAX
               IF      SPA-P02Y-GRPHOSPNUM
                                       =   SPA-P02Y-LGRPHOSPNUM (IDX)
                   MOVE    SPA-P02Y-GRPHOSPNUM-LST (IDX)
                                           TO  SPA-P02Y-GRPHOSPNUM-G
                   MOVE    SPA-P02Y-LGRPHOSPNUM (IDX)
                                           TO  SPA-P02Y-HOSPNUM
                   MOVE    SPA-P02Y-GRPPTID (IDX)
                                           TO  SPA-P02Y-PTID
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF     SPA-P02Y-GRPHOSPNUM  =   SPACE
               MOVE    1               TO  FLG-OK
           END-IF
           IF      FLG-OK              =   ZERO
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-P02Y-CUR
           ELSE
               IF     SPA-P02Y-GRPHOSPNUM  =   SPACE
                   MOVE    SPA-HOSPNUM         TO  SPA-P02Y-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  SPA-P02Y-PTID
               END-IF
               MOVE    1                   TO  SPA-P02Y-PAGE
               PERFORM 3001-PTKINKI-HENSYU-SEC
               IF     SPA-P02Y-GRPHOSPNUM  =   SPACE
                   MOVE    1                   TO  SPA-P02Y-CUR
               ELSE
                   IF      SPA-P02Y-MAX        =   ZERO
                       MOVE    3                   TO  SPA-P02Y-CUR
                   ELSE
                       MOVE    98                  TO  SPA-P02Y-CUR
                   END-IF
               END-IF
           END-IF
           .
       4001-GRPHOSPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    紐付患者処理
      *****************************************************************
       410-GRPCOPY-SYORI-SEC            SECTION.
      *
           IF     (SPA-P02Y-GRPHOSPNUM  =   SPACE ) OR
                  (SPA-P02Y-MAX         =   ZERO  )
               MOVE    "0015"               TO  SPA-ERRCD
               MOVE    3                    TO  SPA-P02Y-CUR
           ELSE
               MOVE    "0102"               TO  SPA-PIDCD
           END-IF
           .
       410-GRPCOPY-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    他院複写処理
      *****************************************************************
       4101-GRPCOPY-SEC            SECTION.
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-P02Y-HOSPNUM    TO  PTKINKI-HOSPNUM
           MOVE    SPA-P02Y-PTID       TO  PTKINKI-PTID
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE                      PTKINKI-REC
           END-IF
           PERFORM UNTIL  ( FLG-PTKINKI    =   1  )
                      OR  ( SPA-ERRCD  NOT =   SPACE )
      *
               MOVE    PTKINKI-SRYCD       TO  WRK-SRYCD
               MOVE    PTKINKI-KINKSTYMD   TO  WRK-KINKSTYMD
               MOVE    PTKINKI-TOUYOYMD    TO  WRK-TOUYOYMD
               MOVE    1                   TO  FLG-P02Y1
               PERFORM 4121-PTKINKI-INS-SEC
      *        登録済みのコードは登録しない
               IF      SPA-ERRCD           =   "1002" 
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
               MOVE    ZERO                TO  FLG-PTKINKI
      *
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 410-INIT-SYORI-SEC
           END-IF
      *
           .
       4101-GRPCOPY-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SYORI-SEC            SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4120-KANREN-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
      *        経過措置薬剤があれば、複写
               IF      SPA-P02Y-CHNGCD     NOT =   SPACE
                   MOVE    "0103"          TO  SPA-PIDCD
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
      *        患者禁忌薬剤登録処理
               MOVE    SPA-P02Y-SRYCD      TO  WRK-SRYCD
               MOVE    SPA-P02Y-KINKSTYMD  TO  WRK-KINKSTYMD
               MOVE    SPA-P02Y-TOUYOYMD   TO  WRK-TOUYOYMD
               MOVE    ZERO                TO  FLG-P02Y1
               PERFORM 4121-PTKINKI-INS-SEC
           END-IF
      *
           IF    ((SPA-ERRCD           =   SPACE ) OR
                  (SPA-ERRCD           =   "1002"))  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 410-INIT-SYORI-SEC
           END-IF
           .
       412-TOUROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録前関連チェック処理
      *****************************************************************
       4120-KANREN-CHK-SEC           SECTION.
      *
           IF      SPA-P02Y-SRYCD      =   SPACE
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-P02Y-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-P02Y-KINKSTYMD  NOT =   SPACE )
                 AND  (SPA-P02Y-TOUYOYMD   NOT =   SPACE )
                 AND  (SPA-P02Y-KINKSTYMD      <   SPA-P02Y-TOUYOYMD)
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-P02Y-CUR
               END-IF
           END-IF
           .
       4120-KANREN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者禁忌薬剤登録処理
      *****************************************************************
       4121-PTKINKI-INS-SEC           SECTION.
      *
      *    患者禁忌薬剤検索処理
           PERFORM 900-PTKINKI-SELECT-SEC
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      FLG-PTKINKI         =   1
      *        追加
               INITIALIZE                      PTKINKI-REC
               MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
               MOVE    SPA-GMN-PTID        TO  PTKINKI-PTID
               MOVE    WRK-SRYCD           TO  PTKINKI-SRYCD
               MOVE    WRK-KINKSTYMD       TO  PTKINKI-KINKSTYMD
               MOVE    WRK-TOUYOYMD        TO  PTKINKI-TOUYOYMD
      *
               MOVE    SMCNDATE-YMD        TO  PTKINKI-CREYMD
               MOVE    SMCNDATE-YMD        TO  PTKINKI-UPYMD
               MOVE    SMCNDATE-HMS        TO  PTKINKI-UPHMS
               MOVE    SPA-OPID            TO  PTKINKI-OPID
      *
               MOVE    PTKINKI-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "P02Y PTKINKI KEY INS ERR:"  MCP-RC
                       ",KEY:" PTKINKI-KEY
               END-IF
           ELSE
               IF      FLG-P02Y1           =   ZERO
      *            登録時のみ更新
                   PERFORM 4121-PTKINKI-UPD-SEC
               ELSE
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4121-PTKINKI-INS-EXT.
           EXIT.
      *****************************************************************
      *    移行品目複写処理
      *****************************************************************
       411-CHGCOPY-SYORI-SEC            SECTION.
      *
      *    患者禁忌薬剤登録処理
           MOVE    SPA-P02Y-SRYCD      TO  WRK-SRYCD
           MOVE    SPA-P02Y-KINKSTYMD  TO  WRK-KINKSTYMD
           MOVE    SPA-P02Y-TOUYOYMD   TO  WRK-TOUYOYMD
           MOVE    ZERO                TO  FLG-P02Y1
           PERFORM 4121-PTKINKI-INS-SEC
      *
           IF     (SPA-PID1-FLG        =   "OK"  )
            AND   (SPA-ERRCD           =   SPACE )
      *        経過措置品目登録
               MOVE    SPA-P02Y-CHNGCD     TO  WRK-SRYCD
               MOVE    SPA-P02Y-KINKSTYMD  TO  WRK-KINKSTYMD
               MOVE    SPA-P02Y-TOUYOYMD   TO  WRK-TOUYOYMD
               MOVE    ZERO                TO  FLG-P02Y1
               PERFORM 4121-PTKINKI-INS-SEC
           END-IF
      *
           IF    ((SPA-ERRCD           =   SPACE ) OR
                  (SPA-ERRCD           =   "1002"))  AND
                  (FLG-END             =   ZERO  )
               PERFORM 410-INIT-SYORI-SEC
           END-IF
           .
       411-CHGCOPY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者禁忌薬剤更新処理
      *****************************************************************
       4121-PTKINKI-UPD-SEC             SECTION.
      *
           MOVE    WRK-KINKSTYMD       TO  PTKINKI-KINKSTYMD
           MOVE    WRK-TOUYOYMD        TO  PTKINKI-TOUYOYMD
      *
           MOVE    SMCNDATE-YMD        TO  PTKINKI-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTKINKI-UPHMS
           MOVE    SPA-OPID            TO  PTKINKI-OPID
      *
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "P02Y PTKINKI KEY UPD ERR:"  MCP-RC
                       ",KEY:" PTKINKI-KEY
           END-IF
           .
       4121-PTKINKI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02Y"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    ZERO                TO  SPA-FLG-END
      *****MOVE    "P02"               TO  SPA-P02-GAMEN
      *
           .
       210-BACK-EXT.
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
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P02Y"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "選択後に薬剤コードが変更されています。"
                           "再度、選択して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "薬剤コード入力エラー。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "薬剤コードは点数マスタに存在しません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "禁忌開始日暦日エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING  "投与開始日暦日エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0007"
                   STRING  "薬剤コードではありません。"
                           "登録できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0008"
                   STRING  "薬剤コードを入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0009"
                   STRING  "禁忌開始日＞投与開始日で入力して下さい"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING  "前頁はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "次頁はありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "他院の表示中です。入力はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0014"
                   STRING  "医療機関選択エラー"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0015"
                   STRING  "複写情報がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1001"
                   MOVE    "患者禁忌薬剤データ更新ができませんでした。"
                                                   TO  SPA-ERRMSG
               WHEN    "1002"
                   STRING  "既に患者禁忌薬剤に登録されています。"
                           "登録しませんでした。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02Y"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-PIDMSG
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "選択した薬剤コードを削除します。"
                           "よろしいですか？"  DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0102"
                   STRING  "他医院の禁忌薬剤をすべて複写します。"
                           "よろしいですか？"  DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "経過措置品目移行コードがあります。"
                           "ＯＫで品目移行コードも登録します。"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           EVALUATE    SPA-PIDCD
               WHEN    "0103"
                   MOVE    "ＮＯ"              TO  PID1-B01
           END-EVALUATE
           MOVE    "ＯＫ"              TO  PID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P02Y"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       800-SE-WAFORMAT-YMDEDIT-SEC  SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    "00000000"
               WHEN    "99999999"
                   MOVE    WRK-SYMD        TO  WRK-HENYMD
               WHEN    OTHER
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1             =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       800-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角チェック処理
      *****************************************************************
       810-ORCSKANACHK-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
           MOVE    WRK-MAX-CNT         TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-DATA
           .
       810-ORCSKANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者禁忌薬剤検索処理
      *****************************************************************
       900-PTKINKI-SELECT-SEC         SECTION.
      *
           INITIALIZE                      PTKINKI-REC
           MOVE    SPA-HOSPNUM         TO  PTKINKI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKINKI-PTID
           MOVE    WRK-SRYCD           TO  PTKINKI-SRYCD
           MOVE    PTKINKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkinki"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTKINKI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTKINKI
               INITIALIZE                      PTKINKI-REC
           END-IF
           MOVE    "tbl_ptkinki"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-PTKINKI-SELECT-EXT.
           EXIT.
      *
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
      *    患者禁忌薬剤マスタ読込処理
      *****************************************************************
       900-PTKINKI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKINKI-REC
               MOVE    ZERO            TO  FLG-PTKINKI
           ELSE
               INITIALIZE                  PTKINKI-REC
               MOVE    1               TO  FLG-PTKINKI
           END-IF
           .
       900-PTKINKI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPACE               TO  TNSWK-TENSU-REC
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
      *H3.10   開始日の降順に検索
      ***  MOVE    "key4"               TO  MCP-PATHNAME
           MOVE    "key24"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 9110-TENSU-READ-SEC
           END-IF
           MOVE    ZERO                TO  FLG-TENSU-OK
           PERFORM UNTIL  (FLG-TENSU       =   1 ) OR
                          (FLG-TENSU-OK    =   1 )
               IF     (TNS-YUKOSTYMD       <=  SPA-SYSYMD ) AND
                      (TNS-YUKOEDYMD       >=  SPA-SYSYMD )
                   MOVE    1                   TO  FLG-TENSU-OK
                   MOVE    1                   TO  FLG-TENSU
               ELSE
                   MOVE    2                   TO  FLG-TENSU-OK
      *H30.10
                   IF      TNSWK-TENSU-REC     =   SPACE
      *                最終履歴を編集 
                       MOVE    TNS-TENSU-REC       TO  TNSWK-TENSU-REC
                   END-IF
               END-IF
               IF      FLG-TENSU           =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 9110-TENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *H30.10
      *    当日有効な履歴が最終履歴でない時、終了日を変更
           IF     (FLG-TENSU-OK        =   1     )
              AND (TNSWK-TENSU-REC NOT =   SPACE )
               MOVE    TNSWK-YUKOEDYMD     TO  TNS-YUKOEDYMD
           END-IF
      *
      *    当日期間外
           IF      FLG-TENSU-OK        =   2
               MOVE    TNSWK-TENSU-REC     TO  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU-OK
           END-IF
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       9110-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       9110-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別マスター読込処理
      *****************************************************************
       900-PTSAME-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTSAME
               MOVE    MCPDATA-REC         TO  PTSAME-REC
           ELSE
               MOVE    1                   TO  FLG-PTSAME
           END-IF
      *
           .
       900-PTSAME-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"         TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
