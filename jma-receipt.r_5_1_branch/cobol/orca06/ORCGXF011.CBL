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
       PROGRAM-ID.                 ORCGXF011.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : メモ登録入力
      *  コンポーネント名  : 雛形一覧（ＸＦ０１１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/06/15    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "XF01SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-EXAMPLE         PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SELNUM2         PIC 9(03).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
           03  WRK-XF01IDMSG       PIC X(80).
      *
           03  WRK-COMMENT         PIC X(1000).
      *    コラムリスト位置
           03  WRK-XF011-BUNREI-ROW    PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
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
      *    雛形
       01  EXAMPLE-REC.
           COPY    "CPEXAMPLE.INC".
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
      *
      *    COPY    "CPORCXKANACONV.INC".
      *
           COPY    "CPORCXKANACONV2.INC".
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
           COPY    "XF01.INC".
           COPY    "XF011.INC".
           COPY    "XFERR.INC".
           COPY    "XFID1.INC".
           COPY    "XF01X.INC".
           COPY    "XFID2.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
      *****MOVE    SPA-WORK            TO  SPA-XF01KYOUTU
           MOVE    SPA-FREE            TO  SPA-XF01
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
      **** MOVE    SPA-XF01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA             TO  SPA-COMMON
           MOVE    SPA-XF01             TO  SPA-FREE
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
           INITIALIZE                  STS-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "XFERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE               TO  LNK-KYOUTU
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "XF011"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
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
           EVALUATE    SPA-MOTOPG
      *            文例画面より
               WHEN    "XF011"
                   IF      SPA-XF011-SELNUM    NOT =   ZERO
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPACE               TO  SPA-XFIDCD
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *            確認画面より
               WHEN    "XFID1"
                   PERFORM 3003-XFID1-SET-SEC
               WHEN    OTHER
                   PERFORM                     310-SPA-SET-SEC
                   MOVE    1                   TO  SPA-XF011-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       3003-XFID1-SET-SEC                  SECTION.
      *
           .
       3003-XFID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
            INITIALIZE                  SPA-XF011-AREA
      *
            MOVE    1                  TO  SPA-XF011-PAGE
      *     文例編集
            PERFORM 3101-BUNREI-HEN-SEC
      *
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    文例編集処理
      *****************************************************************
       3101-BUNREI-HEN-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                  SPA-XF011-BUNNREI-G
           MOVE    ZERO                TO  SPA-XF011-BUNREIMAX
      *
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-XF011-NEXT
           IF      SPA-XF011-PAGE      =   ZERO
               MOVE    1                   TO  SPA-XF011-PAGE
           END-IF
           COMPUTE IDX-S               =  (SPA-XF011-PAGE  -  1 )
                                       *   100
                                       +   1
           PERFORM
                   UNTIL      (FLG-EXAMPLE        =   1 )
                           OR (SPA-XF011-NEXT      =   1 )
               ADD     1                   TO  IDX2
               IF      IDX2                >=  IDX-S
                   ADD     1                   TO  IDX
                   IF      IDX                 >   100
                       MOVE    1               TO  SPA-XF011-NEXT
                   ELSE
                       MOVE    IDX             TO  SPA-XF011-BUNREIMAX
                       MOVE    EXAMPLE-RENNUM  TO  SPA-XF011-TRENNUM
                                                                  (IDX)
      **              MOVE    EXAMPLE-COMMENT TO  SPA-XF011-TCOMMENT
      **                                                          (IDX)
                       PERFORM 31011-COMMENT-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           END-PERFORM
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  SPA-XF011-SELNUM
      *
           .
       3101-BUNREI-HEN-EXT.
           EXIT.
      *H30.11
      *****************************************************************
      *    コメントチェック処理
      *****************************************************************
       31011-COMMENT-HEN-SEC            SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    400                 TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    0                   TO  KANACONV-CHAR-TYPE
           MOVE    EXAMPLE-COMMENT     TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF     (KANACONV-RETURN     =   ZERO )
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-XF011-TCOMMENT(IDX)
           ELSE
               MOVE    KANACONV-OUTDATA
                                       TO  SPA-XF011-TCOMMENT(IDX)
           END-IF
           .
      *
       31011-COMMENT-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-XF011-BUNREI-ROW
           ELSE
               MOVE    XF011-BUNREI-ROW    TO  WRK-XF011-BUNREI-ROW
           END-IF
      *
           MOVE    SPACE               TO  XF011
           INITIALIZE                      XF011
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  XF011-BUNREI-ROW
           MOVE    ZERO                TO  XF011-BUNREI-ROWATTR
           IF      WRK-XF011-BUNREI-ROW NOT =   ZERO
               MOVE    WRK-XF011-BUNREI-ROW TO  XF011-BUNREI-ROW
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-XF011-BUNREIMAX
               MOVE    SPA-XF011-TRENNUM (IDX) TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  XF011-TRENNUM (IDX)
               MOVE    SPA-XF011-TCOMMENT(IDX) TO  XF011-TCOMMENT(IDX)
      *
               IF      SPA-XF011-SELNUM    =   SPA-XF011-TRENNUM (IDX)
                   MOVE    "T"             TO  XF011-BUNREI-SELECT (IDX)
                   MOVE    IDX             TO  XF011-BUNREI-ROW
                   MOVE    2               TO  XF011-BUNREI-ROWATTR
               ELSE
                   MOVE    "F"             TO  XF011-BUNREI-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-XF011-BUNREIMAX  TO  XF011-BUNREI-COUNT
      *
           MOVE    SPA-XF011-SELNUM     TO  XF011-SELNUM
      *    次頁あり
           IF      SPA-XF011-NEXT       =   1
               IF      SPA-XF011-PAGE       =   1
                   MOVE    "［次頁有り］"       TO  XF011-NEXT-VALUE
               ELSE
                   MOVE    "［前頁・次頁有り］" TO  XF011-NEXT-VALUE
               END-IF
               MOVE    "red"               TO  XF011-NEXT-STYLE
           ELSE
               IF      SPA-XF011-PAGE      >   1
                   MOVE    "［前頁有り］"      TO  XF011-NEXT-VALUE
                   MOVE    "red"               TO  XF011-NEXT-STYLE
               ELSE
                   MOVE    SPACE               TO  XF011-NEXT-VALUE
                   MOVE    "black"             TO  XF011-NEXT-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *
           EVALUATE    SPA-XF011-CUR
               WHEN    1
                   MOVE    "SELNUM"            TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        確定
               WHEN    "CLICKED"       ALSO    "B11"
                   MOVE    1               TO  FLG-OK
                   PERFORM 212-TOUROKU-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    2               TO  FLG-OK
                   PERFORM 212-TOUROKU-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 290-BACK-PAGE-SEC
      *        次月頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 291-NEXT-PAGE-SEC
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
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 420-SELNUM-SEC
      *        明細選択
               WHEN    ANY             ALSO    "BUNREILIST"
                   PERFORM 440-BUNREILIST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号処理
      *****************************************************************
       420-SELNUM-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-XF011-CUR
      *
           MOVE    XF011-SELNUM        TO  SPA-XF011-SELNUM
           MOVE    ZERO                TO  SPA-XF011-TBL
      *
           IF      SPA-XF011-SELNUM     =   ZERO
               GO      TO      420-SELNUM-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-XF011-BUNREIMAX
              IF      SPA-XF011-TRENNUM  (IDX)  =   SPA-XF011-SELNUM
                  MOVE    IDX                      TO  SPA-XF011-TBL
              END-IF
           END-PERFORM
           IF      SPA-XF011-TBL       =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    2                   TO  SPA-XF011-CUR
           END-IF
      *
           .
       420-SELNUM-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    明細選択処理
      *****************************************************************
       440-BUNREILIST-SEC                 SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-XF011-BUNREIMAX
              IF      XF011-BUNREI-SELECT (IDX)   =  "T"
                  IF      SPA-XF011-TRENNUM  (IDX)  =   SPA-XF011-SELNUM
                      MOVE    SPA-XF011-TRENNUM  (IDX)  TO  WRK-SELNUM
                  ELSE
                      MOVE    SPA-XF011-TRENNUM  (IDX)  TO  WRK-SELNUM2
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM2     NOT =   ZERO
               MOVE    WRK-SELNUM2         TO  SPA-XF011-SELNUM
           ELSE
               MOVE    WRK-SELNUM          TO  SPA-XF011-SELNUM
           END-IF
      *
           MOVE    SPA-XF011-SELNUM     TO  XF011-SELNUM
           PERFORM 420-SELNUM-SEC
      *
           .
       440-BUNREILIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録　処理
      *****************************************************************
       212-TOUROKU-SEC                   SECTION.
      *
           IF      SPA-XF011-SELNUM     =   ZERO
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *
           MOVE    SPA-XF011-SELNUM     TO  XF011-SELNUM
           PERFORM 420-SELNUM-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      212-TOUROKU-EXT
           END-IF
      *
      *    雛形有無
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-XF011-TRENNUM (SPA-XF011-TBL)
                                       TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_example"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-EXAMPLE-READ-SEC
           ELSE
               MOVE    1               TO  FLG-EXAMPLE
               INITIALIZE                  EXAMPLE-REC
           END-IF
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-EXAMPLE         =   ZERO
               IF      FLG-OK              =   1
      *            コメント追加
                   MOVE    SPACE               TO  WRK-COMMENT
                   STRING  SPA-GMN-MEMO02      DELIMITED   BY  "  "
                           EXAMPLE-COMMENT     DELIMITED   BY  SPACE
                           INTO                WRK-COMMENT
                   END-STRING
      *
                  INITIALIZE                  ORCXKANACONVAREA
                  MOVE    1000                TO  KANACONV-LEN
                  MOVE    5000                TO  KANACONV-STLEN
                  MOVE    0                   TO  KANACONV-CONV-FLAG
                  MOVE    0                   TO  KANACONV-CHAR-TYPE
                  MOVE    WRK-COMMENT         TO  KANACONV-INDATA
                  CALL    "kanaconv"          USING ORCXKANACONVAREA
                  MOVE    KANACONV-OUTDATA
                                              TO  SPA-GMN-MEMO02
               ELSE
      *            コメント置換え
                   MOVE    EXAMPLE-COMMENT     TO  SPA-GMN-MEMO02
               END-IF
               PERFORM 210-BACK
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
           END-IF
      *
           .
       212-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除　処理
      *****************************************************************
       240-COMMENT-DEL-SEC                   SECTION.
      *
           IF      SPA-XF011-TBL        =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    "0104"              TO  SPA-XFIDCD
           END-IF
      *
           .
       240-COMMENT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント削除処理
      *****************************************************************
       2401-COMMENT-DELSYORI-SEC       SECTION.
      *
           MOVE    SPACE               TO  EXAMPLE-REC
           INITIALIZE                  EXAMPLE-REC
           MOVE    SPA-HOSPNUM         TO  EXAMPLE-HOSPNUM
           MOVE    "1"                 TO  EXAMPLE-KUBUN
           MOVE    SPA-XF011-TRENNUM (SPA-XF011-TBL)
                                       TO  EXAMPLE-RENNUM
      *
           MOVE    EXAMPLE-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_example"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "XF011 EXAMPLE DEL ERR:" MCP-RC "."
           END-IF
      *
      *     文例編集
            PERFORM 3101-BUNREI-HEN-SEC
      *
            MOVE    1                  TO  SPA-XF011-CUR
           .
       2401-COMMENT-DELSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       290-BACK-PAGE-SEC               SECTION.
      *
           IF      SPA-XF011-PAGE      >   1
               COMPUTE SPA-XF011-PAGE      =   SPA-XF011-PAGE
                                           -   1
      *        文例編集
               PERFORM 3101-BUNREI-HEN-SEC
           ELSE
               MOVE    "0010"              TO  SPA-ERRCD
           END-IF
           .
      *
       290-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       291-NEXT-PAGE-SEC               SECTION.
      *
           IF      SPA-XF011-NEXT      =   1
               COMPUTE SPA-XF011-PAGE      =   SPA-XF011-PAGE
                                           +   1
      *        文例編集
               PERFORM 3101-BUNREI-HEN-SEC
           ELSE
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
           .
      *
       291-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "XF01"              TO  SPA-SAKIPG
           MOVE    "XF011"             TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "XF01"              TO  MCP-WINDOW
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
           IF      SPA-XFIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *****MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XF011"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
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
               WHEN    "0002"
                   MOVE    "保険組合せ選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "選択番号を選択して下さい"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "次頁はありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "前頁はありません。"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XFERR
           MOVE    SPA-ERRCD           TO  XFERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XFERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "XF011"             TO  SPA-MOTOPG
           MOVE    "XFERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XFERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-XFIDCD
               WHEN    "XXXX"
                   MOVE    "XXXX"          TO  WRK-XF01IDMSG
               WHEN    OTHER
                   MOVE    SPA-XFIDCD      TO  WRK-XF01IDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-XFID1-FLG
      *
           MOVE    SPACE               TO  XFID1
           INITIALIZE                      XFID1
           MOVE    SPA-XFIDCD          TO  XFID1-ID1CODE
           MOVE    WRK-XF01IDMSG       TO  XFID1-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "XF011"             TO  SPA-MOTOPG
           MOVE    "XFID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XFID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    雛形読込
      *****************************************************************
       930-EXAMPLE-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  EXAMPLE-REC
               MOVE    ZERO                TO  FLG-EXAMPLE
           ELSE
               INITIALIZE                      EXAMPLE-REC
               MOVE    1                   TO  FLG-EXAMPLE
           END-IF
      *
           .
       930-EXAMPLE-READ-EXT.
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
      *    ＤＢクロース処理
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
      *
