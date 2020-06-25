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
       PROGRAM-ID.             ORCGP02G2.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : グループ医療機関患者紐付画面（Ｐ０２Ｇ２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/01/27    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PIDMSG          PIC X(100).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-RENNUMZ         PIC ZZ.
      *
           03  WRK-PTID            PIC 9(10).
           03  WRK-PTNUM           PIC X(20).
           03  WRK-HOSPNUM         PIC 9(02).
           03  WRK-HOSPNAME        PIC X(20).
           03  WRK-PTNAME          PIC X(50).
           03  WRK-BIRTHDAY        PIC X(08).
           03  WRK-HEN-BIRTHDAY    PIC X(09).
           03  WRK-SEX             PIC X(01).
           03  WRK-HEN-SEX         PIC X(02).
           03  WRK-ADRS            PIC X(100).
           03  WRK-BANTI           PIC X(100).
      *
           03  WRK-MCP-PATHNAME      PIC X(64).
           03  WRK-MCP-WIDGET        PIC X(64).
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
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者情報
       01  PTINF-REC.
           COPY "CPPTINF.INC".
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
      *
      *    患者複写パラメタ
           COPY    "CPORCSP04.INC".
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
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
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
           END-EVALUATE.
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
               IF      FLG-END             =   ZERO
      *
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02G2"             TO  MCP-WINDOW
      *
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
      *    初期編集処理
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *    ＳＰＡ編集
           INITIALIZE                      SPA-P02G2-AREA
           MOVE    SPA-NAI-GRPHOSPNUM  TO  SPA-P02G2-NAI-HOSPNUM
           MOVE    SPA-NAI-GRPPTID     TO  SPA-P02G2-NAI-PTID
      *    グループ医療機関情報
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )   OR
                              (SPA-GRPHOSPNUM (IDX)    =   ZERO )
               MOVE    SPA-GRPHOSPNUM (IDX)
                                      TO  SPA-P02G2-GRPHOSPNUM-LST
                                                     (IDX)(1:2)
               MOVE    SPA-GRPHOSPNAME (IDX)
                                      TO  SPA-P02G2-GRPHOSPNUM-LST
                                                     (IDX)(4:)
               MOVE    IDX            TO  SPA-P02G2-GRPMAX
           END-PERFORM
           MOVE    SPA-P02G2-GRPHOSPNUM-LST(1)
                                       TO  SPA-P02G2-GRPHOSPNUM-G
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
      *    同一患者識別編集処理
      *****************************************************************
       30001-PTSAME-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX
           INITIALIZE                      SPA-P02G2-LIST-AREA
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
                       MOVE    IDX             TO  SPA-P02G2-LNUM (IDX)
                       MOVE    PTSAME-HOSPNUM  TO  SPA-P02G2-LHOSPNUM
                                                                (IDX)
                       MOVE    PTSAME-PTID     TO  SPA-P02G2-LPTID
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
               MOVE    IDX                 TO  SPA-P02G2-MAX
           ELSE
      *
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-P02G2-LNUM (IDX)
               MOVE    SPA-NAI-GRPHOSPNUM  TO  SPA-P02G2-LHOSPNUM
                                                             (IDX)
               MOVE    SPA-NAI-GRPPTID     TO  SPA-P02G2-LPTID
                                                             (IDX)
               MOVE    IDX                 TO  SPA-P02G2-MAX
           END-IF
      *
           PERFORM 300011-TBLHENSYU-SEC
           .
       30001-PTSAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       300011-TBLHENSYU-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >    SPA-P02G2-MAX
      *        医療機関名称
               PERFORM VARYING     IDY     FROM    1  BY  1
                       UNTIL      (IDY     >    9  )
                               OR (SPA-GRPHOSPNUM (IDY)   =   SPACE )
                   IF      SPA-GRPHOSPNUM (IDY)
                                           =   SPA-P02G2-LHOSPNUM (IDX)
                       MOVE    SPA-GRPHOSPNAME(IDY)
                                           TO  SPA-P02G2-LHOSPNAME(IDX)
                       MOVE    9           TO  IDY
                   END-IF
               END-PERFORM
      *        患者番号・氏名
               MOVE    SPA-P02G2-LHOSPNUM (IDX)    TO  WRK-HOSPNUM
               MOVE    SPA-P02G2-LPTID    (IDX)    TO  WRK-PTID
               MOVE    SPACE                   TO  WRK-PTNUM
               PERFORM 30011-PTNUM-HEN-SEC
               MOVE    WRK-PTNUM           TO  SPA-P02G2-LPTNUM(IDX)
               MOVE    WRK-PTNAME          TO  SPA-P02G2-LNAME (IDX)
               MOVE    SPACE               TO  SPA-ERRCD
           END-PERFORM
           .
       300011-TBLHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       30011-PTNUM-HEN-SEC              SECTION.
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    WRK-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-PTID            TO  SPTID-PTID
           MOVE    WRK-PTNUM           TO  SPTID-PTNUM
           MOVE    1                   TO  SPTID-SYS1009-KBN
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
               MOVE    ZERO            TO  WRK-PTID
               MOVE    "0003"          TO  SPA-ERRCD
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
               MOVE    SPTID-PTID      TO  WRK-PTID
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      30011-PTNUM-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    WRK-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WRK-PTID            TO  PTINF-PTID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTINF       NOT =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    PTINF-NAME          TO  WRK-PTNAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           MOVE    PTINF-SEX           TO  WRK-SEX
           MOVE    PTINF-HOME-ADRS     TO  WRK-ADRS
           MOVE    PTINF-HOME-BANTI    TO  WRK-BANTI
      *
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"            TO  WRK-HEN-SEX
               WHEN    "2"
                   MOVE    "女"            TO  WRK-HEN-SEX
           END-EVALUATE
      *    生年月日
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    PTINF-BIRTHDAY      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HEN-BIRTHDAY
           ELSE
               MOVE    PTINF-BIRTHDAY      TO  WRK-HEN-BIRTHDAY
           END-IF
      *
           .
       30011-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
      *    EVALUATE    SPA-PIDCD
      *        WHEN    "0101"
      *    END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
           MOVE    SPACE               TO  SPA-PID1-FLG
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  P02G2
           INITIALIZE                      P02G2
      *
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-P02G2-MAX
               MOVE    SPA-P02G2-LNUM   (IDX) TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ            TO  P02G2-LNUM   (IDX)
               MOVE    SPA-P02G2-LHOSPNAME (IDX)
                                              TO  P02G2-LHOSPNAME (IDX)
               MOVE    SPA-P02G2-LPTNUM (IDX) TO  P02G2-LPTNUM (IDX)
               MOVE    SPA-P02G2-LNAME  (IDX) TO  P02G2-LPTNAME (IDX)
           END-PERFORM
           MOVE    SPA-P02G2-MAX       TO  P02G2-GRPLIST-COUNT
      *
           MOVE    SPA-P02G2-GRPHOSPNUM-G  TO  P02G2-GRPHOSPNUM
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-P02G2-GRPMAX
               MOVE    SPA-P02G2-GRPHOSPNUM-LST (IDX)
                                       TO  P02G2-GRPHOSPNUM-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-P02G2-GRPMAX    TO  P02G2-GRPHOSPNUM-COUNT
      *
           MOVE    SPA-P02G2-PTNUM     TO  P02G2-PTNUM
      *
           MOVE    SPA-P02G2-PTNAME    TO  P02G2-NAME
           MOVE    SPA-P02G2-ADRS1     TO  P02G2-ADRS1
           MOVE    SPA-P02G2-ADRS2     TO  P02G2-ADRS2
           MOVE    SPA-P02G2-BIRTHDAY  TO  P02G2-BIRTHDAY
           MOVE    SPA-P02G2-SEX       TO  P02G2-SEX
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
           EVALUATE    SPA-P02G2-CUR
               WHEN    01
                   MOVE    "GRPHOSPNUM"    TO  MCP-WIDGET
               WHEN    02
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    12
                   MOVE    "B12"           TO  MCP-WIDGET
               WHEN    10
                   MOVE    "B01"           TO  MCP-WIDGET
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
                   PERFORM 402-CLREA-SEC
      *    解除
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 409-KAIJYO-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4012-TOROKU-SEC
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
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-P02G2-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        リスト選択
               WHEN    ANY             ALSO    "GRPLIST"
                   PERFORM 4001-GRPLIST-SEL-SEC
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
      *    一覧選択処理
      *****************************************************************
       4001-GRPLIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  SPA-P02G2-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-P02G2-MAX
               IF      P02G2-GRPLIST-SELECT (IDX)   =   "T"
                   MOVE    IDX             TO  SPA-P02G2-SELNUM
               END-IF
           END-PERFORM
      *
           .
       4001-GRPLIST-SEL-EXT.
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
           .
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    P02G2-GRPHOSPNUM    TO  SPA-P02G2-GRPHOSPNUM-G
           MOVE    P02G2-PTNUM         TO  SPA-P02G2-PTNUM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    医療機関番号
           PERFORM 41021-GRPHOSPNUM-CHK-SEC
      *    患者番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41022-PTNUM-CHK-SEC
           END-IF
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    医療機関番号処理
      *****************************************************************
       41021-GRPHOSPNUM-CHK-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-P02G2-GRPMAX
               IF      SPA-P02G2-GRPHOSPNUM-LST (IDX)(1:2)
                                       =   SPA-P02G2-GRPHOSPNUM
                   MOVE    SPA-P02G2-GRPHOSPNUM-LST (IDX)
                                           TO  SPA-P02G2-GRPHOSPNUM-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-P02G2-CUR
           END-IF
           .
       41021-GRPHOSPNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号処理
      *****************************************************************
       41022-PTNUM-CHK-SEC                 SECTION.
      *
           IF    ((SPA-P02G2-GRPHOSPNUM =   ZERO  )  OR
                  (SPA-P02G2-PTNUM      =   SPACE ))
           OR    ((SPA-P02G2-GRPHOSPNUM =   SPA-P02G2-MAEHOSPNUM) AND
                  (SPA-P02G2-PTNUM      =   SPA-P02G2-MAEPTNUM ))
               IF      SPA-P02G2-PTNUM      =   SPACE
                   INITIALIZE               SPA-P02G2-OTAREA
               END-IF
           ELSE
      *        患者番号・氏名
               MOVE    SPA-P02G2-GRPHOSPNUM    TO  WRK-HOSPNUM
               MOVE    SPA-P02G2-PTNUM         TO  WRK-PTNUM
               MOVE    ZERO                    TO  WRK-PTID
               PERFORM 30011-PTNUM-HEN-SEC
      *        患者紐付け判定
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 410221-PTSAME-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-PTID            TO  SPA-P02G2-PTID
                   MOVE    WRK-PTNUM           TO  SPA-P02G2-PTNUM
                   MOVE    WRK-PTNAME          TO  SPA-P02G2-PTNAME
                   MOVE    WRK-HEN-BIRTHDAY    TO  SPA-P02G2-BIRTHDAY 
                   MOVE    WRK-HEN-SEX         TO  SPA-P02G2-SEX 
                   MOVE    WRK-ADRS            TO  SPA-P02G2-ADRS1 
                   MOVE    WRK-BANTI           TO  SPA-P02G2-ADRS2 
                   MOVE    SPA-P02G2-GRPHOSPNUM
                                               TO  SPA-P02G2-MAEHOSPNUM
                   MOVE    SPA-P02G2-PTNUM     TO  SPA-P02G2-MAEPTNUM
                   MOVE    12                  TO  SPA-P02G2-CUR
               ELSE
                   MOVE    2                   TO  SPA-P02G2-CUR
                   INITIALIZE                  SPA-P02G2-OTAREA
               END-IF
           END-IF
           .
       41022-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者紐付け判定処理
      *****************************************************************
       410221-PTSAME-CHK-SEC           SECTION.
      *
           INITIALIZE                      PTSAME-REC
           MOVE    PTINF-HOSPNUM       TO  PTSAME-HOSPNUM
           MOVE    PTINF-PTID          TO  PTSAME-PTID
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
      *    データがある時、エラー
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
               PERFORM UNTIL   (FLG-PTSAME     =   1 )
                          OR   (SPA-ERRCD  NOT =   SPACE )
                   IF     (PTSAME-HOSPNUM      =   SPA-HOSPNUM   )
                       MOVE    "0006"              TO  SPA-ERRCD
                   END-IF
      *
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               END-PERFORM
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       410221-PTSAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLREA-SEC          SECTION.
      *
           IF      SPA-P02G2-PTNUM     =   SPACE
      *        初期画面表示
               PERFORM 3000-SCREEN-INIT-SEC
           ELSE
               INITIALIZE                   SPA-P02G2-INAREA
               MOVE    SPA-P02G2-GRPHOSPNUM-LST(1)
                                       TO  SPA-P02G2-GRPHOSPNUM-G
           END-IF
           MOVE    2                   TO  SPA-P02G2-CUR
           .
       402-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    解除処理
      *****************************************************************
       409-KAIJYO-SEC          SECTION.
      *
           INITIALIZE                  SPA-P02G2-LIST-AREA
           INITIALIZE                  SPA-P02G2-NAI-HOSPNUM
           INITIALIZE                  SPA-P02G2-NAI-PTID
      *
           INITIALIZE                  SPA-P02G2-INAREA
           MOVE    SPA-P02G2-GRPHOSPNUM-LST(1)
                                       TO  SPA-P02G2-GRPHOSPNUM-G
           MOVE    2                   TO  SPA-P02G2-CUR
           .
       409-KAIJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       4012-TOROKU-SEC          SECTION.
      *
           IF      SPA-P02G2-MAX       >   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *    入力チェック処理
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 410-INPUT-CHK-SEC
           END-IF
           IF     (SPA-ERRCD           =   SPACE )
               IF      (SPA-P02G2-GRPHOSPNUM   =   SPACE )
                   OR  (SPA-P02G2-PTNUM        =   SPACE )
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-P02G2-GRPHOSPNUM
                                               TO  SPA-P02G2-NAI-HOSPNUM
                   MOVE    SPA-P02G2-PTID      TO  SPA-P02G2-NAI-PTID
      *
                   INITIALIZE                  SPA-P02G2-INAREA
      *
                   ADD     1                   TO  SPA-P02G2-MAX
                   MOVE    SPA-P02G2-MAX       TO  IDX
                   MOVE    IDX                 TO  SPA-P02G2-LNUM (IDX)
                   MOVE    SPA-P02G2-NAI-HOSPNUM
                                               TO  SPA-P02G2-LHOSPNUM
                                                             (IDX)
                   MOVE    SPA-P02G2-NAI-PTID  TO  SPA-P02G2-LPTID
                                                             (IDX)
                   PERFORM 300011-TBLHENSYU-SEC
      *
                   MOVE    SPA-P02G2-GRPHOSPNUM-LST(1)
                                           TO  SPA-P02G2-GRPHOSPNUM-G
      *
                   MOVE    10                  TO  SPA-P02G2-CUR
               END-IF
           END-IF
           .
       4012-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-P02G2-NAI-HOSPNUM   TO  SPA-NAI-GRPHOSPNUM
           MOVE    SPA-P02G2-NAI-PTID      TO  SPA-NAI-GRPPTID
      *
           MOVE    "P02G2"             TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           MOVE    ZERO                TO  SPA-FLG-END
           MOVE    "P02"               TO  SPA-P02-GAMEN
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
           MOVE    "P02G2"             TO  MCP-WINDOW.
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
                   MOVE    "医療機関番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "患者情報が登録されてません"
                                                   TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "患者番号は登録されていません。" 
                                                   TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "医療機関番号と患者番号を入力して下さい"
                                                   TO  SPA-ERRMSG
               WHEN    "0005"
                   STRING  "既に紐付されています。"
                           "紐付解除後、再度、登録をして下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO   SPA-ERRMSG
                   END-STRING
               WHEN    "0006"
                   STRING    "既に紐付けされている患者です。"
                             "紐付けできません。"
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02G2"             TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   STRING  "患者情報を複写します。"
                           "よろしいですか？"  DELIMITED  BY  SIZE
                                               INTO  WRK-PIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-PIDCD           TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "戻る"              TO  PID1-B01
           MOVE    "ＯＫ"              TO  PID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P02G2"             TO  SPA-MOTOPG
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
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
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
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
