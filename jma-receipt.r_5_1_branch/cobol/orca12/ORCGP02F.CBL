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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGP02F.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者複写元一覧（Ｐ０２Ｆ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/08    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    患者登録用共通パラメタ
           COPY    "P02COMMON-SPA".
      *
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SEYMD.
               05  WRK-SEYY        PIC 9(04).
               05  WRK-SEMM        PIC 9(02).
               05  WRK-SEDD        PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-Z9              PIC ZZ9.
           03  WRK-MOJISU          PIC 9(04).
           03  WRK-NAME            PIC X(15).
      *
           03  WRK-SAKIPG          PIC X(08).
      *
           03  WRK-DBPATH            PIC X(64).
      *
           03  WRK-ERRMSG          PIC X(40).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           MOVE    SPA-COMMON          TO  SPA-AREA.
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
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
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           PERFORM 5001-MAPCUR-SEC
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "P02F"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           IF      SPA-GMN2E-SYORI     =   ZERO
               MOVE    "＜患者複写＞"          TO  SPA-GMN2F-MIDASI
           ELSE
               MOVE    "＜患者紐付け複写＞"    TO  SPA-GMN2F-MIDASI
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN2F-SELNUM
           MOVE    ZERO                TO  SPA-GMN2F-MAX
           IF      SPA-GMN2F-KNJKEY    =   SPACE
               MOVE    1               TO  SPA-GMN2F-CUR
               GO      TO      300-SCREEN-EXT
           END-IF
      *
           PERFORM 310-SPA-SET-SEC
      *    １件のみの時、対象とする
           IF      SPA-GMN2F-MAX       =   1
               MOVE    1                   TO  IDX
               MOVE    SPA-GMN2F-PTNUM(IDX)
                                           TO  SPA-GMN2E-PTNUM
               MOVE    SPA-GMN2F-PTID (IDX)
                                           TO  SPA-GMN2E-PTID
           END-IF
      *
           IF      SPA-GMN2F-MAX       =   ZERO
               MOVE    1               TO  SPA-GMN2F-CUR
           ELSE
               MOVE    2               TO  SPA-GMN2F-CUR
           END-IF
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
      *    一覧読込み
           INITIALIZE                  SPA-GMN2F-KNJLIST-AREA
      *
           MOVE    ZERO                TO  SPA-GMN2F-MAX
      *
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    20  BY  -1
                   UNTIL       IDX     <       1
               IF      SPA-GMN2F-KNJKEY(IDX:1)  NOT =   SPACE
                   MOVE    IDX                     TO  WRK-MOJISU
                   MOVE    1                       TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDX
           COMPUTE IDY                 =   WRK-MOJISU  +  1
      *
      *    漢字検索
           INITIALIZE                  PTINF-REC
           IF      SPA-GMN2E-GRPHOSPNUM    =   SPACE
               MOVE    SPA-HOSPNUM             TO  PTINF-HOSPNUM
           ELSE
               MOVE    SPA-GMN2E-GRPHOSPNUM    TO  PTINF-HOSPNUM
           END-IF
           MOVE    SPACE               TO  PTINF-NAME
           MOVE    SPA-GMN2F-KNJKEY(1:WRK-MOJISU)
                                       TO  PTINF-NAME(1:WRK-MOJISU)
           MOVE    SPA-GMN2F-KNJKEY(1:WRK-MOJISU)
                                       TO  PTINF-KANANAME(1:WRK-MOJISU)
           IF      IDY                 <=  20
               MOVE    "%"             TO  PTINF-NAME(IDY:1)
               MOVE    "%"             TO  PTINF-KANANAME(IDY:1)
           END-IF
      *
      *    患者マスターを漢字氏名で検索
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 810-PTINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF 
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (IDX             >=  100       ) OR
                              (FLG-PTINF       =   1          )
               MOVE    ZERO            TO  FLG-OK
      *        患者紐付け判定
               IF      SPA-GMN2E-GRPHOSPNUM    =   SPACE
                   MOVE    1                   TO  FLG-OK
               ELSE
                   PERFORM 410111-PTSAME-CHK-SEC
               END-IF
      *
               IF      FLG-OK              =   1
                   ADD     1                   TO  IDX
                   IF      IDX                 <=  100
                       PERFORM 2012-PTINF-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 810-PTINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (IDX                 >=  100  )  AND
                  (FLG-PTINF           =   ZERO )
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       2012-PTINF-HENSYU-SEC          SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN2F-SEQNUM (IDX)
      *
      *    患者番号変換より、患者IDより患者NOを取得する
           MOVE    PTINF-PTID          TO  SPA-GMN2F-PTID  (IDX)
      *
           INITIALIZE                      ORCSPTIDAREA
           IF      SPA-GMN2E-GRPHOSPNUM    =   SPACE
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           ELSE
               MOVE    SPA-GMN2E-GRPHOSPNUM
                                           TO  SPTID-HOSPNUM
               MOVE    1                   TO  SPTID-SYS1009-KBN
           END-IF
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  SPA-GMN2F-PTNUM  (IDX)
           ELSE
               MOVE    SPTID-PTNUM     TO  SPA-GMN2F-PTNUM  (IDX)
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN2F-NAME   (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN2F-BIRTHDAY  (IDX)
      *
           MOVE    IDX                 TO  SPA-GMN2F-MAX
           .
       2012-PTINF-HENSYU-EXT.
           EXIT.
           .
      *
      *****************************************************************
      *    患者紐付け判定処理
      *****************************************************************
       410111-PTSAME-CHK-SEC           SECTION.
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
      *
           IF      FLG-PTSAME          =   1
               MOVE    1                   TO  FLG-OK
           ELSE
      *        同一医療機関で登録済み
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
               MOVE    1                   TO  FLG-OK
               PERFORM UNTIL   (FLG-PTSAME     =   1 )
                   IF     (PTSAME-HOSPNUM      =   SPA-HOSPNUM   )
                       MOVE    ZERO                TO  FLG-OK
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
       410111-PTSAME-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
      *****        MOVE    SPACE           TO  SPA-GMN2E-PTNUM
      *****        MOVE    ZERO            TO  SPA-GMN2E-PTID
                   PERFORM 210-BACK
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 220-NEXT-HEN-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        検索項目入力
               WHEN    "ACTIVATE"      ALSO    "KNJKEY"
                   PERFORM 41012-KNJKEY-SEC
      *        キー入力
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-SELNUM-CHK-SEC
      *        行選択
               WHEN    ANY             ALSO    "KNJLIST"
                   PERFORM 41012-LISTSEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *   検索項目入力　処理
      *****************************************************************
       41012-KNJKEY-SEC                SECTION.
      *
           MOVE    P02F-KNJKEY         TO  SPA-GMN2F-KNJKEY
      *
           PERFORM 310-SPA-SET-SEC
           IF      SPA-GMN2F-MAX       =   ZERO
               MOVE    1               TO  SPA-GMN2F-CUR
           ELSE
               MOVE    2               TO  SPA-GMN2F-CUR
           END-IF
      *
           .
       41012-KNJKEY-EXT.
           EXIT.
      *****************************************************************
      *    番号選択　処理
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    2                   TO  SPA-GMN2F-CUR
      *
           MOVE    P02F-SELNUM         TO  SPA-GMN2F-SELNUM
      *
      *    １件のみとき、１とする
           IF     (SPA-GMN2F-MAX       =   1  )  AND
                  (SPA-GMN2F-SELNUM    =   ZERO)
               MOVE    1               TO   SPA-GMN2F-SELNUM
           END-IF
      *
           IF       SPA-GMN2F-SELNUM   <   1   OR
                                       >   SPA-GMN2F-MAX
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41011-SELNUM-CHK-EXT
           ELSE
               MOVE    SPA-GMN2F-SELNUM    TO  IDX
               MOVE    SPA-GMN2F-PTNUM(IDX)
                                           TO  SPA-GMN2E-PTNUM
               MOVE    SPA-GMN2F-PTID (IDX)
                                           TO  SPA-GMN2E-PTID
      *
               PERFORM 210-BACK
           END-IF
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *    行選択
           MOVE    ZERO                TO  SPA-GMN2F-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   200
               IF      P02F-SELECT (IDX)    =   "T"
                   MOVE    SPA-GMN2F-SEQNUM (IDX)
                                            TO  SPA-GMN2F-SELNUM
                   MOVE    200              TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN2F-SELNUM    >   ZERO
               MOVE    SPA-GMN2F-SELNUM   TO  P02F-SELNUM
               PERFORM 41011-SELNUM-CHK-SEC
           END-IF
           .
       41012-LISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       220-NEXT-HEN-SEC       SECTION.
      *
           .
       220-NEXT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "P02E"              TO  SPA-SAKIPG
           MOVE    "P02F"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "P02E"              TO  MCP-WINDOW
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
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "P02F"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  P02F
           INITIALIZE                      P02F
           MOVE    SPA-GMN2F-KNJKEY    TO  P02F-KNJKEY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN2F-MAX
               MOVE    SPA-GMN2F-SEQNUM(IDX)   TO  WRK-Z9
               MOVE    WRK-Z9                  TO  P02F-SEQNUM (IDX)
               MOVE    SPA-GMN2F-PTNUM (IDX)   TO  P02F-PTNUM  (IDX)
               MOVE    SPA-GMN2F-NAME  (IDX)   TO  P02F-NAME   (IDX)
               MOVE    SPA-GMN2F-BIRTHDAY (IDX)
                                               TO  P02F-BIRTHDAY(IDX)
           END-PERFORM
           MOVE    SPA-GMN2F-MAX       TO  P02F-COUNT
      *
           MOVE    SPA-GMN2F-SELNUM    TO  P02F-SELNUM
      *
           MOVE    SPA-GMN2F-MIDASI    TO  P02F-MIDASI-VALUE
           IF      SPA-GMN2E-SYORI     =   ZERO
               MOVE    "black"             TO  P02F-MIDASI-STYLE
           ELSE
               MOVE    "red"               TO  P02F-MIDASI-STYLE
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
           EVALUATE    SPA-GMN2F-CUR
               WHEN    01
                   MOVE    "KNJKEY"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM"        TO  MCP-WIDGET
           END-EVALUATE
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "次ページはありません"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが１００件以上あります"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P02F"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "PERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
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
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       700-SEIWA-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       810-PTINF-READ-SEC         SECTION.
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
       810-PTINF-READ-EXT.
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
