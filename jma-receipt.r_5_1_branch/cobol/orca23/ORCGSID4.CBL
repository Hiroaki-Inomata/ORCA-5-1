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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGSID4.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 確認画面４（ＳＩＤ４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  10/05/10    太田    新規作成
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
       DATA                                      DIVISION.
      *FILE                                      SECTION.
      *
      *
       WORKING-STORAGE                           SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "S01COMMON-SPA".
      *
      *    画面活性非活性
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
       01  SPA-SID4.
           03  SPA-GMN-CUR                 PIC 9(03).
           03  SPA-GMN-EDANUM-MAX          PIC 9(03).
           03  SPA-GMN-LBLID1SKYPRT        PIC X(70).
           03  SPA-GMN-AREA.
               05  SPA-GMN-LBLSRYYMD       PIC X(08).
               05  SPA-GMN-SRYYMD          PIC X(20).
               05  SPA-GMN-SRYKA           PIC X(50).
               05  SPA-GMN-HOKEN           PIC X(20).
               05  SPA-GMN-KONSEIKYU       PIC X(10).
               05  SPA-GMN-TAX-SAI         PIC X(10).
               05  SPA-GMN-ZENSEIKYU       PIC X(10).
               05  SPA-GMN-NYUKIN          PIC X(10).
           03  SPA-NAI-AREA.
               05  SPA-NAI-PREV            PIC 9(01).
               05  SPA-NAI-NEXT            PIC 9(01).
               05  SPA-NAI-EDANUM          PIC  9(3).
               05  SPA-NAI-KONSEIKYU       PIC S9(7).
               05  SPA-NAI-TAX-SAI         PIC S9(7).
               05  SPA-NAI-ZENSEIKYU       PIC S9(7).
               05  SPA-NAI-NYUKIN          PIC S9(7).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-PARA                    PIC 9(01).
           03  FLG-ORCSNUMERR              PIC 9(01).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
      *
      *    作業領域
       01  WRK-AREA.
           03  WRK-SKYPRTMSG               PIC X(50).
           03  WRK-MCP-WIDGET              PIC X(64).
           03  WRK-FILEMEI.
               05  WRK-FILEMEI-YMD         PIC X(08).
               05  WRK-FILEMEI-HMS         PIC X(06).
               05  WRK-FILEMEI-CD          PIC X(06).
           03  WRK-KINGAKUX                PIC X(10).
           03  WRK-KINGAKU9                PIC S9(10).
           03  WRK-KINGAKUZ                PIC ----------.
           03  WRK-EDANUM                  PIC 9(03).
           03  WRK-LBLKENSU.
               05  WRK-LBLKENSU1           PIC ZZ9.
               05  WRK-LBLKENSU-FL         PIC X(01).
               05  WRK-LBLKENSU2           PIC ZZ9.
           03  WRK-YMD                     PIC X(10).
           03  WRK-EDTYMD1                 PIC X(10).
           03  WRK-EDTSTYMD                PIC X(09).
           03  WRK-EDTEDYMD                PIC X(09).
      *
      *    パラメタテーブル退避領域
       01  TPARA-REC.
           COPY    "CPPARA.INC"            REPLACING  //PARA-//
                                           BY         //TPARA-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
           COPY    "CPSYUPARA01.INC".
           COPY    "CPSYUPARA03.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
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
           COPY    "S02.INC".
           COPY    "S03.INC".
           COPY    "S04.INC".
           COPY    "S06.INC".
           COPY    "S07.INC".
           COPY    "S08.INC".
           COPY    "S09.INC".
           COPY    "S10.INC".
           COPY    "S99.INC".
           COPY    "SERR.INC".
           COPY    "SID1.INC".
           COPY    "SID2.INC".
           COPY    "SID3.INC".
           COPY    "SID4.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
           MOVE    SPA-FREE2       TO  SPA-SID4
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *        画面遷移
           WHEN     "PUTG"         ALSO   "CLICKED"
               PERFORM 200-GMNSENI
      *        入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-SID4        TO  SPA-FREE2
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "SERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
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
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    SPACE           TO  MCP-PUTTYPE.
           MOVE    "SID4"          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                               SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"           ALSO    "B01"
               MOVE    SPACE           TO  SPA-SID1-FLG
               PERFORM 210-BACK
      *    前
           WHEN    "CLICKED"           ALSO    "B06"
               IF    ( SPA-NAI-PREV    =   1 )
                   PERFORM 460-SKYPRT-PREV-SEC
               END-IF
      *    次
           WHEN    "CLICKED"           ALSO    "B07"
               IF    ( SPA-NAI-NEXT    =   1 )
                   PERFORM 470-SKYPRT-NEXT-SEC
               END-IF
      *    請求書
           WHEN    "CLICKED"           ALSO    "B10"
               MOVE    "O1"            TO  SPA-SID1-FLG
               PERFORM 210-BACK
      *    明細書
           WHEN    "CLICKED"           ALSO    "B11"
               MOVE    "O2"            TO  SPA-SID1-FLG
               PERFORM 210-BACK
      *    請求書・明細書
           WHEN    "CLICKED"           ALSO    "B12"
               MOVE    "OK"            TO  SPA-SID1-FLG
               PERFORM 210-BACK
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                                  SECTION.
      *
           IF    ( SPA-SID1-FLG        NOT =   SPACE )
               PERFORM 410-INPUT-CHK-SEC
               IF    ( SPA-ERRCD           =   SPACE )
                   PERFORM 420-SYUPARA-UPD-SEC
               END-IF
               IF    ( SPA-ERRCD           =   SPACE )
                   PERFORM 421-SYUPARA-UPD-SEC
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "S02"           TO  SPA-SAKIPG
               MOVE    "SID4"          TO  SPA-MOTOPG
      *
               MOVE    "JOIN"          TO  MCP-PUTTYPE
               MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1               TO  FLG-END
      *
           END-IF
           .
      *
       210-BACK-EXT.
           EXIT.
      *
      *************************************************************
      *    初期画面処理
      *************************************************************
       300-SCREEN-SEC              SECTION.
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-SID4
           INITIALIZE                      SPA-SID4
      *
           MOVE    SPA-S01-FILEMEI     TO  WRK-FILEMEI
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    1                   TO  WRK-EDANUM
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
           MOVE    SYUPARA01-CNT       TO  SPA-GMN-EDANUM-MAX
      *
           MOVE    SID4-LBLID1SKYPRT   TO  SPA-GMN-LBLID1SKYPRT
      *
           MOVE    SPA-S01-FILEMEI     TO  WRK-FILEMEI
           MOVE    "00102"             TO  WRK-FILEMEI-CD
           PERFORM 900-PARA-KEY5-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
      *
           MOVE    ZERO                TO  WRK-EDANUM
           IF    ( SPA-S01-TUKI-SRYYM  NOT =   SPACE )
               MOVE    PARA-EDANUM     TO  WRK-EDANUM
           END-IF
           PERFORM UNTIL ( FLG-PARA    NOT =   ZERO )
                    OR   ( WRK-EDANUM  NOT =   ZERO )
      *
               PERFORM VARYING IDX0    FROM    1   BY  1
                       UNTIL ( IDX0    >   SYUPARA03-GRP-DENPNUM-MAX )
                        OR   ( WRK-EDANUM  NOT =   ZERO )
                   IF    ( SYUPARA03-GRP-DENPNUM (IDX0)
                                       =   SPA-S01-DENPNUM )
                       MOVE    PARA-EDANUM
                                       TO  WRK-EDANUM
                   END-IF
               END-PERFORM
      *
               IF    ( WRK-EDANUM      =   ZERO )
                   PERFORM 900-PARA-KEY5-FET-SEC
                   MOVE    PARA-DATA-REC   TO  SYUPARA03-AREA
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 3101-SPASET-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *************************************************************
      *    画面編集処理
      *************************************************************
       3101-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-GMN-AREA
                                           SPA-NAI-AREA
      *
           MOVE    PARA-EDANUM         TO  SPA-NAI-EDANUM
           MOVE    SYUPARA03-SYU-DATA  TO  SYUNOU-REC
      *
           IF    ( SPA-S01-TUKI-SRYYM  NOT =   SPACE )
            AND  ( SYUPARA03-KIKANKBN      =   SPACE )
                   MOVE    "診療年月"  TO  SPA-GMN-LBLSRYYMD
                   MOVE    SYU-SRYYMD  TO  WRK-YMD
                   PERFORM 800-HIZUKE-EDIT-SEC
                   MOVE    WRK-EDTYMD1(1:6)
                                       TO  SPA-GMN-SRYYMD
           ELSE
               MOVE    "診療日"        TO  SPA-GMN-LBLSRYYMD
               IF    ( SYUPARA03-STSRYYMD
                                   NOT =   SYUPARA03-EDSRYYMD )
      *
                   MOVE    SYUPARA03-STSRYYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-EDIT-SEC
                   MOVE    WRK-EDTYMD1 TO  WRK-EDTSTYMD
      *
                   MOVE    SYUPARA03-EDSRYYMD
                                       TO  WRK-YMD
                   PERFORM 800-HIZUKE-EDIT-SEC
                   MOVE    WRK-EDTYMD1 TO  WRK-EDTEDYMD
      *
                   STRING  WRK-EDTSTYMD    DELIMITED BY SIZE
                           "-"             DELIMITED BY SIZE
                           WRK-EDTEDYMD(5:) 
                                           DELIMITED BY SIZE
                   INTO    SPA-GMN-SRYYMD
                   END-STRING
               ELSE
                   MOVE    SYU-SRYYMD  TO  WRK-YMD
                   PERFORM 800-HIZUKE-EDIT-SEC
                   MOVE    WRK-EDTYMD1 TO  SPA-GMN-SRYYMD
               END-IF
           END-IF
      *
           MOVE    SYUPARA03-SRYKAMEI  TO  SPA-GMN-SRYKA
           MOVE    SYUPARA03-HKNCOMBIMEI
                                       TO  SPA-GMN-HOKEN
      *
           MOVE    SYUPARA03-SEIKYU    TO  SPA-NAI-KONSEIKYU
                                           WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-KONSEIKYU
      *
           MOVE    SYUPARA03-SEIKYU-TAX-SAI
                                       TO  SPA-NAI-TAX-SAI
                                           WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-TAX-SAI
      *
           MOVE    SYUPARA03-MISYU     TO  SPA-NAI-ZENSEIKYU
                                           WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-ZENSEIKYU
      *
           MOVE    SYUPARA03-NYUKIN    TO  SPA-NAI-NYUKIN
                                           WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SPA-GMN-NYUKIN
      *
           MOVE    PARA-REC            TO  TPARA-REC
           COMPUTE PARA-EDANUM = PARA-EDANUM + 1
           PERFORM 900-PARA-KEY6-SEL-SEC
           IF    ( FLG-PARA            =   ZERO )
               MOVE    1               TO  SPA-NAI-NEXT
           END-IF
      *
           MOVE    TPARA-REC           TO  PARA-REC
           COMPUTE PARA-EDANUM = PARA-EDANUM - 1
           PERFORM 900-PARA-KEY7-SEL-SEC
           IF    ( FLG-PARA            =   ZERO )
               MOVE    1               TO  SPA-NAI-PREV
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       3101-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
      *    EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    WHEN    "ACTIVATE"      ALSO    "XXXX"
      *    WHEN    OTHER
      *
               MOVE    ZERO        TO  SPA-GMN-CUR
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
      *    END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        基本チェック処理
               PERFORM 4102-KIHON-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    SID4-KINGAKU (1)    TO  SPA-GMN-KONSEIKYU
           MOVE    SID4-KINGAKU (2)    TO  SPA-GMN-TAX-SAI
           MOVE    SID4-KINGAKU (3)    TO  SPA-GMN-ZENSEIKYU
           MOVE    SID4-KINGAKU (5)    TO  SPA-GMN-NYUKIN
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-KONSEIKYU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-TAX-SAI-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-ZENSEIKYU-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 4101-NYUKIN-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    今回請求額チェック処理
      *****************************************************************
       4101-KONSEIKYU-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-KONSEIKYU
           MOVE    SPA-GMN-KONSEIKYU
                                   TO  WRK-KINGAKUX
           PERFORM 800-KINGAKU-EDIT-SEC
           IF    ( FLG-ORCSNUMERR  =   1 )
               MOVE    1           TO  SPA-GMN-CUR
               MOVE    "0001"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-KINGAKU9    TO  SPA-NAI-KONSEIKYU
               MOVE    WRK-KINGAKUZ    TO  SPA-GMN-KONSEIKYU
           END-IF
      *
           .
       4101-KONSEIKYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    消費税再掲チェック処理
      *****************************************************************
       4101-TAX-SAI-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-TAX-SAI
           MOVE    SPA-GMN-TAX-SAI TO  WRK-KINGAKUX
           PERFORM 800-KINGAKU-EDIT-SEC
           IF    ( FLG-ORCSNUMERR  =   1 )
               MOVE    2           TO  SPA-GMN-CUR
               MOVE    "0002"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-KINGAKU9    TO  SPA-NAI-TAX-SAI
               MOVE    WRK-KINGAKUZ    TO  SPA-GMN-TAX-SAI
           END-IF
      *
           .
       4101-TAX-SAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前回請求額チェック処理
      *****************************************************************
       4101-ZENSEIKYU-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-ZENSEIKYU
           MOVE    SPA-GMN-ZENSEIKYU
                                   TO  WRK-KINGAKUX
           PERFORM 800-KINGAKU-EDIT-SEC
           IF    ( FLG-ORCSNUMERR  =   1 )
               MOVE    3           TO  SPA-GMN-CUR
               MOVE    "0003"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-KINGAKU9    TO  SPA-NAI-ZENSEIKYU
               MOVE    WRK-KINGAKUZ    TO  SPA-GMN-ZENSEIKYU
           END-IF
      *
           COMPUTE WRK-KINGAKU9
               =   SPA-NAI-KONSEIKYU
               +   SPA-NAI-ZENSEIKYU
      *
           IF    ( WRK-KINGAKU9    >=  10000000 )
            OR   ( WRK-KINGAKU9    <= -1000000 )
               IF    ( WRK-MCP-WIDGET  =   "KINGAKU03" )
                   MOVE    3       TO  SPA-GMN-CUR
               ELSE
                   MOVE    1       TO  SPA-GMN-CUR
               END-IF
               MOVE    "0005"      TO  SPA-ERRCD
           END-IF
      *
           .
       4101-ZENSEIKYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金額チェック処理
      *****************************************************************
       4101-NYUKIN-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-NAI-NYUKIN
           MOVE    SPA-GMN-NYUKIN  TO  WRK-KINGAKUX
           PERFORM 800-KINGAKU-EDIT-SEC
           IF    ( FLG-ORCSNUMERR  =   1 )
               MOVE    4           TO  SPA-GMN-CUR
               MOVE    "0004"      TO  SPA-ERRCD
           ELSE
               MOVE    WRK-KINGAKU9    TO  SPA-NAI-NYUKIN
               MOVE    WRK-KINGAKUZ    TO  SPA-GMN-NYUKIN
           END-IF
      *
           .
       4101-NYUKIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求書パラメタ更新処理
      *****************************************************************
       420-SYUPARA-UPD-SEC             SECTION.
      *
           MOVE    SPA-S01-FILEMEI     TO  WRK-FILEMEI
           MOVE    "00102"             TO  WRK-FILEMEI-CD
           MOVE    SPA-NAI-EDANUM      TO  WRK-EDANUM
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA03-AREA
           MOVE    SPA-NAI-KONSEIKYU   TO  SYUPARA03-SEIKYU
           MOVE    SPA-NAI-TAX-SAI     TO  SYUPARA03-SEIKYU-TAX-SAI
           MOVE    SPA-NAI-ZENSEIKYU   TO  SYUPARA03-MISYU
           IF    ( SYUPARA03-MISYU     >=  ZERO )
               MOVE    SYUPARA03-MISYU TO  SYUPARA03-MISYU-P
               MOVE    ZERO            TO  SYUPARA03-MISYU-M
           ELSE
               MOVE    ZERO            TO  SYUPARA03-MISYU-P
               MOVE    SYUPARA03-MISYU TO  SYUPARA03-MISYU-M
           END-IF
           MOVE    SPA-NAI-NYUKIN      TO  SYUPARA03-NYUKIN
           MOVE    SYUPARA03-AREA      TO  PARA-DATA-REC
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       420-SYUPARA-UPD-EXT.
           EXIT.
      *****************************************************************
      *    請求書パラメタ更新処理
      *****************************************************************
       421-SYUPARA-UPD-SEC             SECTION.
      *
           MOVE    SPA-S01-FILEMEI     TO  WRK-FILEMEI
           MOVE    "00101"             TO  WRK-FILEMEI-CD
           MOVE    1                   TO  WRK-EDANUM
           PERFORM 900-PARA-KEY4-SEL-SEC
           MOVE    PARA-DATA-REC       TO  SYUPARA01-AREA
           MOVE    1                   TO  SYUPARA01-UPDFLG
           MOVE    SYUPARA01-AREA      TO  PARA-DATA-REC
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4002"          TO  SPA-ERRCD
           END-IF
      *
           .
       421-SYUPARA-UPD-EXT.
           EXIT.
      *****************************************************************
      *    請求書表示切替処理（前）
      *****************************************************************
       460-SKYPRT-PREV-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 420-SYUPARA-UPD-SEC
           END-IF
      *
           MOVE    SPA-S01-FILEMEI     TO  WRK-FILEMEI
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
      *
           COMPUTE PARA-EDANUM = SPA-NAI-EDANUM - 1
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           PERFORM 900-PARA-KEY7-SEL-SEC
           MOVE    PARA-DATA-REC   TO  SYUPARA03-AREA
           IF    ( FLG-PARA        =   ZERO )
               PERFORM 3101-SPASET-SEC
           END-IF
      *
           .
       460-SKYPRT-PREV-EXT.
           EXIT.
      *****************************************************************
      *    請求書表示切替処理（次）
      *****************************************************************
       470-SKYPRT-NEXT-SEC             SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 420-SYUPARA-UPD-SEC
           END-IF
      *
           MOVE    SPA-S01-FILEMEI     TO  WRK-FILEMEI
           MOVE    "00102"             TO  WRK-FILEMEI-CD
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
      *
           COMPUTE PARA-EDANUM = SPA-NAI-EDANUM + 1
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           PERFORM 900-PARA-KEY6-SEL-SEC
           MOVE    PARA-DATA-REC   TO  SYUPARA03-AREA
           IF    ( FLG-PARA        =   ZERO )
               PERFORM 3101-SPASET-SEC
           END-IF
      *
           .
       470-SKYPRT-NEXT-EXT.
           EXIT.
      *************************************************************
      *    自画面編集処理
      *************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"      TO  MCP-PUTTYPE
           MOVE    "SID4"         TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *************************************************************
      *    画面編集処理
      *************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  SID4
           INITIALIZE                      SID4
      *
           MOVE    SPA-GMN-LBLID1SKYPRT TO SID4-LBLID1SKYPRT
      *
           IF    ( SPA-GMN-EDANUM-MAX  >   1 )
               INITIALIZE                  WRK-LBLKENSU
               MOVE    SPA-NAI-EDANUM      TO  WRK-LBLKENSU1
               MOVE    "/"                 TO  WRK-LBLKENSU-FL
               MOVE    SPA-GMN-EDANUM-MAX  TO  WRK-LBLKENSU2
               MOVE    WRK-LBLKENSU        TO  SID4-LBLKENSU
           END-IF
      *
           MOVE    SPA-GMN-LBLSRYYMD   TO  SID4-LBLSRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  SID4-SRYYMD
           MOVE    SPA-GMN-HOKEN       TO  SID4-HOKEN
           MOVE    SPA-GMN-SRYKA       TO  SID4-SRYKA
           MOVE    SPA-GMN-KONSEIKYU   TO  SID4-KINGAKU (1)
           MOVE    SPA-GMN-TAX-SAI     TO  SID4-KINGAKU (2)
           MOVE    SPA-GMN-ZENSEIKYU   TO  SID4-KINGAKU (3)
      *
           COMPUTE WRK-KINGAKU9
               =   SPA-NAI-KONSEIKYU
               +   SPA-NAI-ZENSEIKYU
           MOVE    WRK-KINGAKU9        TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  SID4-KINGAKU (4)
      *
           MOVE    SPA-GMN-NYUKIN      TO  SID4-KINGAKU (5)
      *
           MOVE    WIDGET-NORMAL       TO  SID4-B06-STATE
                                           SID4-B07-STATE
           MOVE    "T"                 TO  SID4-B06-VISIBLE
                                           SID4-B07-VISIBLE
           IF    ( SPA-NAI-PREV        =   ZERO )
            AND  ( SPA-NAI-NEXT        =   ZERO )
               MOVE    "F"             TO  SID4-B06-VISIBLE
                                           SID4-B07-VISIBLE
               IF    ( SPA-NAI-PREV    =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  SID4-B06-STATE
               END-IF
               IF    ( SPA-NAI-NEXT    =   ZERO )
                   MOVE    WIDGET-INSENSITIVE  TO  SID4-B07-STATE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *************************************************************
      *    画面カーソルセット処理
      *************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF        ( SPA-ERRCD       =   SPACE )
               AND   ( SPA-GMN-CUR     =   ZERO )
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN   1
               MOVE    "KINGAKU01"     TO  MCP-WIDGET
           WHEN   2
               MOVE    "KINGAKU02"     TO  MCP-WIDGET
           WHEN   3
               MOVE    "KINGAKU03"     TO  MCP-WIDGET
           WHEN   4
               MOVE    "KINGAKU04"     TO  MCP-WIDGET
           WHEN   5
               MOVE    "KINGAKU05"     TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理（入力位置より）
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "KINGAKU01"
               MOVE    2           TO  SPA-GMN-CUR
           WHEN    "KINGAKU02"
               MOVE    3           TO  SPA-GMN-CUR
           WHEN    "KINGAKU03"
               MOVE    5           TO  SPA-GMN-CUR
           WHEN    "KINGAKU04"
               MOVE    5           TO  SPA-GMN-CUR
           WHEN    "KINGAKU05"
               MOVE    1           TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    エラーメッセージ表示理
      **************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "今回請求額入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE    "消費税（再掲）入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "前回請求額入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "入金額入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "合計請求額が大きすぎます"
                                           TO  SPA-ERRMSG
           WHEN    "4001" THRU "4002"
               MOVE    "パラメタテーブルの更新に失敗しました"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  SERR
           MOVE    SPA-ERRCD       TO  SERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  SERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "SID4"          TO  SPA-MOTOPG
           MOVE    "SERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "SERR"          TO  MCP-WINDOW
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
      *    金額数値変換処理
      *****************************************************************
       800-KINGAKU-EDIT-SEC         SECTION.
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
               MOVE    1           TO  FLG-ORCSNUMERR
           END-IF
           IF    ( SNUM-MINKBN     =   SPACE  )
            AND  ( SNUM-SEISUU     >   7     )
               MOVE    1           TO  FLG-ORCSNUMERR
           END-IF
           IF    ( SNUM-MINKBN     =   "1"  )
            AND  ( SNUM-SEISUU     >   6     )
               MOVE    1           TO  FLG-ORCSNUMERR
           END-IF
           IF    ( SNUM-SYOKBN         =   "1"   )
               MOVE    1           TO  FLG-ORCSNUMERR
           END-IF
      *
           IF    ( FLG-ORCSNUMERR      =   ZERO )
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKU9
               MOVE    SNUM-OUTNUM     TO  WRK-KINGAKUZ
           END-IF
      *
           .
       800-KINGAKU-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-EDIT-SEC          SECTION.
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"        TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD     TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1
                               TO  WRK-EDTYMD1
      *
           .
       800-HIZUKE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索処理
      *****************************************************************
       900-PARA-KEY4-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    WRK-EDANUM          TO  PARA-EDANUM
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PARA-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索処理
      *****************************************************************
       900-PARA-KEY5-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-S01-GYOUMUID    TO  PARA-GYOUMUID
           MOVE    SPA-S01-UID         TO  PARA-TERMID
           MOVE    WRK-FILEMEI         TO  PARA-FILEMEI
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブルFETCH処理
      *****************************************************************
       900-PARA-KEY5-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           .
       900-PARA-KEY5-FET-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索処理
      *****************************************************************
       900-PARA-KEY6-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PARA-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    パラメタテーブル検索処理
      *****************************************************************
       900-PARA-KEY7-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PARA
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PARA-REC
           ELSE
               MOVE    1               TO  FLG-PARA
               INITIALIZE                  PARA-REC
           END-IF
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PARA-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
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
       910-DB-SELECT-EXT.
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
           .
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
           .
      *****************************************************************
      *    ＤＢＵＰＤＡＴＥ処理
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
