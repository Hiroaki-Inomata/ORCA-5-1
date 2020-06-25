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
       PROGRAM-ID.                 ORCGKA021.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院処方せん・薬剤情報印刷
      *  コンポーネント名  : 選択内容一覧（ＫＡ０２１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/09/08    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    選択内容データ
      *    SELECT  SPAPRT-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPAPRT.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  SPAPRT-FILE.
      *01  SPAPRT-REC              PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "KA02PRT".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "KA02SPA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "KA01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "KA02SCR-SPA".
      *
      *    画面スパ領域２
           COPY    "KA02SCR-SPA2".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-SPAPRT          PIC X(02).
      *    03  STS-SPASCR          PIC X(02).
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SPAPRT          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *R02.2
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-HEN-END         PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDG-K               PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDX-J2              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDX-B               PIC 9(04).
           03  IDX-G               PIC 9(04).
           03  IDX-W               PIC 9(04).
           03  IDX-W2              PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDX1                PIC 9(04).
      *
           03  IDX-KOUI             PIC 9(04).
           03  IDX-KAI              PIC 9(04).
      *
           03  IDX11               PIC 9(01).
           03  IDX2                PIC 9(02).
           03  IDX-S2              PIC 9(02).
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-SELNUM2             PIC 9(03).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
          03  WRK-ZAITEN           PIC 9(07).
          03  WRK-KAISU-X.
               05  WRK-KAISU       PIC Z(08).
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(08).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
           03  WRK-MEISYO          PIC X(40).
      *    数量表示用
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-SURYO-J         PIC X(22).
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(08).
           03  WRK-KAISU-J         PIC X(16).
           03  WRK-IDG             PIC 9(04).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(01).
           03  IDJ2                PIC 9(04).
      *
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z92             PIC --,---,---.
           03  WRK-ZZ              PIC ZZZ.
      *
           03  WRK-SRYKAISUKEI         PIC 9(03).
           03  WRK-TBANGO              PIC 9(03).
      *
           03  WRK-MCP-WIDGET          PIC X(68).
      *    コラムリスト位置
           03  WRK-KA02-MEILIST-ROW    PIC S9(09).
           03  FLG-GMN-INIT1           PIC  9(01).
      *    換算値数量表示用
           03  WRK-TSURYO              PIC X(30).
           03  WRK-G-SURYO             PIC X(30).
           03  IDM2                    PIC 9(04).
           03  IDM3                    PIC 9(04).
           03  WRK-KANSURYO            PIC X(12).
           03  WRK-SHO                 PIC 9(04).
           03  WRK-AMARI               PIC 9(04).
           03  WRK-KANZANTANINAME      PIC X(08).
      *
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *R02.2
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *TEST
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
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
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           COPY    "KA01.INC".
           COPY    "KA02.INC".
           COPY    "KA021.INC".
           COPY    "KAERR.INC".
           COPY    "KAID1.INC".
           COPY    "KAID2.INC".
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
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-KA01KYOUTU
           MOVE    SPA-FREE            TO  SPA-KA02
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-KA02SPA-SET-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-KA02SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-KA01KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KA02            TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-KA02SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-KA02-DATA
           INITIALIZE                      SPA-KA02-DATA
      *
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPANAI"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL  (FLG-SPATMP          =   1   )
                 OR   (IDX                 >=  100 )
               ADD    1                    TO  IDX
               MOVE   SPATMP-DATA-REC      TO  SPA-PARA-ZAI-REC
                                                       (IDX)
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           END-PERFORM
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
      *
           MOVE    IDX                 TO  SPA-PARA-MAX
      *
           .
       1002-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-KA02SPA-OUT-SEC             SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPANAI"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPANAI"            TO  SPATMP-FILEMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   100  )  OR
                              (SPA-PARA-ZAINUM (IDX)  =   ZERO)
      *
               MOVE    SPA-PARA-ZAI-REC (IDX)  TO  SPATMP-DATA-REC
               MOVE    IDX                     TO  SPATMP-EDANUM
      *
               MOVE    SPATMP-REC          TO  MCPDATA2-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "KT02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               END-IF
           END-PERFORM
      *
           .
       1003-KA02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KAERR"
               MOVE    SPACE               TO  SPA-MOTOPG
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
               MOVE    SPACE               TO  LNK-KYOUTU
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "KA021"              TO  MCP-WINDOW
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
       300-SCREEN-SEC             SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KAID1"
      *            確認画面より
                   PERFORM 3002-KAID1-SET-SEC
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
       3001-SPA-SET-SEC             SECTION.
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           INITIALIZE                  SPA-KA021
           MOVE    1                   TO  SPA-KA021-PAGE
           MOVE    1                   TO  IDX-STR
           PERFORM 30011-GMNHENSYU-SEC
      *
           .
       3001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       30011-GMNHENSYU-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT1
      *
           INITIALIZE                  SPA-KA021-MEISAI-AREA
           MOVE    ZERO                TO  SPA-KA021-SELNUM
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  FLG-HEN-END
           MOVE    ZERO                TO  SPA-KA021-NEXT
           MOVE    ZERO                TO  SPA-KA021-ENDIDX
           MOVE    IDX-STR             TO  SPA-KA021-MAEIDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PARA-MAX )
                           OR (FLG-HEN-END =   1        )
               IF      IDX             >=  IDX-STR
                   PERFORM 3101-SPAPARA-HEN-SEC
               END-IF
           END-PERFORM
      *    最大件数
           IF      IDG                 >   200
               MOVE    200                 TO  SPA-KA021MEISAI-MAX
           ELSE
               MOVE    IDG                 TO  SPA-KA021MEISAI-MAX
           END-IF
      *    すべての行を選択可能とする
           MOVE    ZERO                TO  WRK-TBANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KA021MEISAI-MAX
               IF      SPA-KA021-TMEISYO(IDX)(1:3)   NOT =   "---"
                   IF      SPA-KA021-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-KA021-TBANGO (IDX)    TO  WRK-TBANGO
                   END-IF
                   MOVE    WRK-TBANGO      TO  SPA-KA021NAI-TBANGO2(IDX)
               END-IF
           END-PERFORM
      *
           IF      SPA-KA021MEISAI-MAX     =   ZERO
               MOVE    99                  TO  SPA-KA021-CUR
           ELSE
               MOVE    1                   TO  SPA-KA021-CUR
           END-IF
      *
           .
       30011-GMNHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       3101-SPAPARA-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
      *
      *    剤内１行目
           ADD     1               TO  IDG
           PERFORM 31011-GYO1-HEN-SEC
      *
      *    剤明細編集
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   50  )  OR
                              (IDG         >=  200 )  OR
                              (SPA-PARA-SRYCD (IDX IDZ) =  SPACE)
               ADD     1                   TO  IDX-W
               PERFORM 31012-ZAIMEI-HEN-SEC
           END-PERFORM
      *    自ページ有無判定
           IF      IDG                 >=  200
               IF      IDX             <   SPA-PARA-MAX
                   MOVE    1               TO  SPA-KA021-NEXT
                   MOVE    IDX             TO  SPA-KA021-ENDIDX
                   IF     (SPA-PARA-SRYCD (IDX IDZ) NOT =  SPACE )
                       COMPUTE SPA-KA021-ENDIDX    =   IDX
                                                   -   1
                   END-IF
               END-IF
               IF     (IDX             =   SPA-PARA-MAX   )  AND
                      (SPA-PARA-SRYCD (IDX IDZ) NOT =  SPACE )
                   MOVE    1               TO  SPA-KA021-NEXT
                   COMPUTE SPA-KA021-ENDIDX    =   IDX
                                               -   1
               END-IF
               MOVE    1               TO  FLG-HEN-END
           END-IF
      *
      *    ＊日数　編集 （最終行に追加）
           PERFORM 31013-KAISU-HEN-SEC
           MOVE    WRK-ZAIKEIJ         TO  SPA-KA021-TZAIKEI (IDG)
           MOVE    1                   TO  SPA-KA021NAI-KAIFLG  (IDG)
      *
           INSPECT SPA-KA021-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
           MOVE    IDX                 TO  SPA-KA021NAI-TBANGO    (IDG)
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 <=  200
               MOVE    ALL "-"             TO  SPA-KA021-TMEISYO (IDG)
           END-IF
      *
           .
       3101-SPAPARA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *    診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    SPA-PARA-SRYSYUKBN(IDX) =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH
      *
      *    診療種別がない時、診療行為区分より
           IF      SPA-PARA-SRYSYUKBN (IDX)   =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    SPA-PARA-SRYKBN(IDX)
                                           =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
           IF      SPA-PARA-SRYSYUKBN (IDX)   =   SPACE
               MOVE    SPACE           TO  SPA-KA021-TMEISYO (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-KA021-TMEISYO (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-KA021-TMEISYO (IDG)(1:1)
               MOVE    SPA-PARA-SRYSYUKBN (IDX)
                                       TO  SPA-KA021-TMEISYO (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-KA021-TMEISYO (IDG)(7:)
           END-IF
      *
           INSPECT SPA-KA021-TMEISYO  (IDG)   REPLACING
                                            ALL "  "  BY  "　"
           INSPECT SPA-KA021-TYOBI0   (IDG)   REPLACING
                                            ALL "  "  BY  "　"
      *
      *    番号
           MOVE    IDX                  TO  SPA-KA021-TBANGO     (IDG)
           MOVE    IDX                  TO  SPA-KA021NAI-TBANGO  (IDG)
      *
           .
       31011-GYO1-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDG
           MOVE    IDX                 TO  SPA-KA021NAI-TBANGO    (IDG)
           MOVE    SPA-PARA-SRYCD (IDX IDZ)
                                       TO  SPA-KA021NAI-TSRYCD (IDG)
           MOVE    IDX-W               TO  SPA-KA021NAI-TWNUM (IDG)
      *
      *    診療コードより診療行為名称を取得
           MOVE    SPA-PARA-SRYCD (IDX IDZ)
                                       TO  TNS-SRYCD
           MOVE    SPA-NAI-KOUFUYMD    TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-KOUFUYMD    TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    SPA-PARA-SRYCD (IDX IDZ)    TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-KA021-TMEISYO2(IDG)
      *    薬剤服用コメントの時、再編集
           IF      SPA-PARA-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO     TO  SPA-KA021-TMEISYO2(IDG)
           END-IF
      *    後発医薬品変更再編集
           IF      SPA-PARA-SRYCD (IDX IDZ)    =   "099209903"
                                               OR  "099209905"
                                               OR  "099209906"
                PERFORM 3100122-KOUHATU-MEISYO-SEC
                MOVE    WRK-MEISYO     TO  SPA-KA021-TMEISYO2(IDG)
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((SPA-PARA-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (SPA-PARA-SRYCD   (IDX IDZ)(1:3)  =   "008" ))  AND
                  (SPA-PARA-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    SPA-PARA-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-KA021-TMEISYO2(IDG)
           END-IF
      *H30.4
           IF     (SPA-PARA-SRYCD   (IDX IDZ)(1:7)  =   "0992081")
             AND  (SPA-PARA-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    SPA-PARA-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-KA021-TMEISYO2(IDG)
           END-IF
      *
      *    用量割合再編集
           IF     (SPA-PARA-SRYCD(IDX IDZ)(1:7)  =   "0992000")  AND
                  (SPA-PARA-SRYSURYO(IDX IDZ) NOT =   ZERO     )
                PERFORM 3005-INPUT-MEISYO3-SEC
                MOVE    WRK-MEISYO         TO  SPA-KA021-TMEISYO2(IDG)
           END-IF
      *    器材商品名の器材コード
           IF     (SPA-PARA-SRYCD(IDX IDZ)(1:1)   =   "7"  )  AND
                  (SPA-PARA-AUTOKBN (IDX IDZ)     =   "T"  )
                PERFORM 3005-INPUT-MEISYO4-SEC
                MOVE    WRK-MEISYO        TO  SPA-KA021-TMEISYO2(IDG)
      *ver.4.7
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
           END-IF
      *    器材商品名コード
           IF     (SPA-PARA-SRYCD(IDX IDZ)(1:3)    =   "058"  )
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
           END-IF
      *ver.4.7
      *
      *    数量（薬剤、器材のとき）
           IF     (SPA-PARA-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7" )
               OR (SPA-PARA-SRYCD (IDX IDZ)(1:3)   =   "059"  )
               MOVE    SPA-PARA-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-J         TO  SPA-KA021-TSURYOU (IDG)
               MOVE    TNS-TANINAME        TO  SPA-KA021-TTANI1  (IDG)
           END-IF
      *
      *H28.5
           IF     (SPA-PARA-SRYCD (IDX IDZ)(1:1)   =   "6")
             AND  (SPA-PARA-INPUTCOMENT (IDX IDZ)
                                               NOT =   SPACE)
      *        一般名表示
               MOVE    SPA-PARA-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-KA021-TMEISYO2(IDG)
           END-IF
      *
      *    交付日期間外の点数マスタ判定
           IF     (FLG-TENSU           =   ZERO  )
               CONTINUE
           ELSE
               MOVE    1               TO  SPA-KA021NAI-ERRFLG  (IDG)
           END-IF
      *
           INSPECT SPA-KA021-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-KA021-TSURYOU (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-KA021-TTANMEI (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-KA021-TTANI1  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-KA021-TYOBI1  (IDG) REPLACING  ALL "  "  BY  "　"
      *ver.4.5
      *    換算値数量設定あり
           IF      SPA-PARA-KANZANCHI (IDX IDZ)    >  ZERO
               PERFORM 310129-KANSURYO-HEN-SEC
           END-IF
      *
      *    コメント以外の最終行セット
           IF      SPA-PARA-SRYCD (IDX IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *ver.4.5
      *****************************************************************
      *   換算値数量編集処理
      *****************************************************************
       310129-KANSURYO-HEN-SEC                  SECTION.
      *
      *    次行へ編集
           ADD     1                   TO  IDG
           MOVE    IDX                 TO  SPA-KA021NAI-TBANGO    (IDG)
      *
           MOVE    SPA-PARA-KANSURYO (IDX IDZ)  TO  WRK-SURYO
           MOVE    SPA-PARA-KANZANTANINAME (IDX IDZ)
                                       TO  WRK-KANZANTANINAME
           PERFORM 3101291-KANSURYO-HENKAN-SEC
      *
           INSPECT SPA-KA021-TMEISYO(IDG) REPLACING
                                               ALL "  "  BY  "　"
      *
           .
       310129-KANSURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   換算値数量編集処理
      *****************************************************************
       3101291-KANSURYO-HENKAN-SEC                  SECTION.
      *
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    1                   TO  IDM
           MOVE    1                   TO  IDM2
           MOVE    SPACE               TO  WRK-TSURYO
           PERFORM
                   UNTIL       IDM     >   22
               IF      WRK-SURYO-J(IDM:1)    =   SPACE
                   ADD     1               TO  IDM
               ELSE
                   IF      WRK-SURYO-J(IDM:2)    =   "　"
                       ADD     2               TO  IDM
                   ELSE
                       MOVE    WRK-SURYO-J(IDM:2)
                                               TO  WRK-TSURYO
                                                        (IDM2:2)
                       ADD     2               TO  IDM2
                       ADD     2               TO  IDM
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-SURYO           =   ZERO
               MOVE    SPACE               TO  WRK-TSURYO
           END-IF
      *    数量換算単位名称
           MOVE    WRK-KANZANTANINAME  TO  WRK-TSURYO  (IDM2:)
           MOVE    SPACE               TO  WRK-G-SURYO
           STRING  "（"
                   WRK-TSURYO
                   "）"                DELIMITED  BY  SPACE
                                       INTO    WRK-G-SURYO
           END-STRING
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1   BY  1
                   UNTIL      (IDM     >   30  )
                         OR   (WRK-G-SURYO (IDM:1) =   SPACE )
               ADD     1               TO  IDM2
           END-PERFORM
           COMPUTE IDM3                =   30
                                       -   IDM2
                                       +   1
           MOVE    SPACE                   TO  SPA-KA021-TKANSURYO
                                                    (IDG)
           MOVE    WRK-G-SURYO(1:IDM2)     TO  SPA-KA021-TKANSURYO
                                                    (IDG)(IDM3:IDM2)
      *
           .
       3101291-KANSURYO-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3100121-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
      *    埋め込み有
           IF     (SPA-PARA-INPUTCOMENT(IDX IDZ)   NOT =  SPACE )
               MOVE    SPACE               TO  WRK-MEISYO
               STRING     "【"             DELIMITED   BY  SIZE
                          SPA-PARA-INPUTCOMENT(IDX IDZ)
                                           DELIMITED   BY  SPACE
                          "】"             DELIMITED   BY  SIZE
                          INTO             WRK-MEISYO
               END-STRING
           END-IF
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     後発医薬品変更再編集処理
      *****************************************************************
       3100122-KOUHATU-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3100122-KOUHATU-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     用量割合再編集処理
      *****************************************************************
       3005-INPUT-MEISYO3-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "1"                 TO  ORCSMEIHEN-KBN
           MOVE    SPA-PARA-SRYSURYO(IDX IDZ)
                                       TO  ORCSMEIHEN-SURYO
           MOVE    SPA-PARA-SRYCD(IDX IDZ)
                                       TO  ORCSMEIHEN-SRYCD
           MOVE    TNS-NAME            TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-MEISYO
           .
       3005-INPUT-MEISYO3-EXT.
           EXIT.
      *****************************************************************
      *    器材商品名コードの器材編集
      *****************************************************************
       3005-INPUT-MEISYO4-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "〔"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "〕"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3005-INPUT-MEISYO4-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-X         TO  KANACHK-MAE-INPUT
           MOVE    22                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-MAX         >   ZERO
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-SURYO-J
           ELSE
               MOVE    SPACE               TO  WRK-SURYO-J
           END-IF
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   回数画面再編集処理
      *****************************************************************
       31013-KAISU-HEN-SEC                  SECTION.
      *
           MOVE    SPA-PARA-ZAIKAIKEI (IDX)    TO  WRK-SRYKAISUKEI
      *    回数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KAISU-J 
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-KAISU-H         TO  KANACHK-MAE-INPUT
           MOVE    40                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KAISU-J
      *
           MOVE    SPACE               TO  WRK-ZAIKEIJ
           STRING  "×"                    DELIMITED   BY  SIZE
                   WRK-KAISU-J             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEIJ
           END-STRING
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＸＤＩＤ１）ＯＫ処理
      *****************************************************************
       3002-KAID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KAID1CD
      *        削除処理
               WHEN    "0101"
                   IF      SPA-KAID1-FLG        =   "OK"
                       PERFORM 4091-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-KA021-CUR
                   END-IF
      *        一括削除処理
               WHEN    "0102"
                   IF      SPA-KAID1-FLG        =   "OK"
                       PERFORM 4111-ALLDELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-KA021-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KAID1CD
           .
       3002-KAID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＡＩＤ２）ＯＫ処理
      *****************************************************************
       3003-KAID2-SET-SEC          SECTION.
      *
           .
       3003-KAID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                    PERFORM 402-CLREA-SEC
      *        削除処理
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 409-DELETE-SEC
      *        一括削除処理
               WHEN    "CLICKED"       ALSO    "B11"
                    PERFORM 411-ALLDELETE-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-NEXT-PAGE-SEC
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
           MOVE    ZERO                TO  SPA-KA021-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4001-SELNUM-CHK-SEC
      *        選択
               WHEN    ANY             ALSO    "MEILIST"
                   PERFORM 4005-MEILIST-SEL-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       4001-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    KA021-SELNUM        TO  SPA-KA021-SELNUM
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-KA021MEISAI-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-KA021-SELNUM      =   SPA-KA021-TBANGO  (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-KA021-SELNUM    =   ZERO
               MOVE    1                   TO  SPA-KA021-CUR
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-KA021-CUR
               ELSE
                   MOVE    9                   TO  SPA-KA021-CUR
               END-IF
           END-IF
      *
           .
       4001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細リスト選択処理
      *****************************************************************
       4005-MEILIST-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KA021MEISAI-MAX
              IF     (KA021-MEILIST-SELECT (IDX)   =   "T" )
                 AND (KA021-MNUM  (IDX)(1:1)  NOT  =   "-" )
                   IF      SPA-KA021NAI-TBANGO (IDX)
                                               NOT =   SPA-KA021-SELNUM
                       MOVE    SPA-KA021NAI-TBANGO2(IDX)
                                                   TO  WRK-SELNUM
                   ELSE
                       MOVE    SPA-KA021NAI-TBANGO2(IDX)
                                                   TO  WRK-SELNUM2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO ) AND
                  (WRK-SELNUM      NOT =   SPA-KA021-SELNUM)
               MOVE    WRK-SELNUM          TO  SPA-KA021-SELNUM
               MOVE    SPA-KA021-SELNUM    TO  KA021-SELNUM
               PERFORM 4001-SELNUM-CHK-SEC
               MOVE    9                   TO  SPA-KA021-CUR
           ELSE
               MOVE    2                   TO  SPA-KA021-CUR
           END-IF
      *
           .
       4005-MEILIST-SEL-EXT.
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
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "KA02"              TO  SPA-SAKIPG
           MOVE    "KA021"             TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "KA02"              TO  MCP-WINDOW
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
      *    クリア処理
      *****************************************************************
       402-CLREA-SEC       SECTION.
      *
           MOVE    ZERO                TO  SPA-KA021-SELNUM
           MOVE    1                   TO  SPA-KA021-CUR
      *
           .
       402-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-MAE-PAGE-SEC       SECTION.
      *
           IF      SPA-KA021-PAGE      >   1
               COMPUTE SPA-KA021-PAGE  =   SPA-KA021-PAGE
                                       -   1
               IF      SPA-KA021-PAGE      =   1
                   MOVE    1                   TO  IDX-STR
               ELSE
                   MOVE    SPA-KA021-MAEIDX    TO  IDX-STR
               END-IF
               PERFORM 30011-GMNHENSYU-SEC
           END-IF
           .
       406-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       407-NEXT-PAGE-SEC       SECTION.
      *
           IF      SPA-KA021-NEXT      =   1
               COMPUTE SPA-KA021-PAGE  =   SPA-KA021-PAGE
                                       +   1
               MOVE    SPA-KA021-ENDIDX    TO  IDX-STR
               COMPUTE IDX-STR             =   IDX-STR   +   1
               PERFORM 30011-GMNHENSYU-SEC
           END-IF
           .
       407-NEXT-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       409-DELETE-SEC       SECTION.
      *
           IF      SPA-KA021-SELNUM    >   ZERO
               MOVE    "0101"          TO  SPA-KAID1CD
           ELSE
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-KA021-CUR
           END-IF
      *
           .
       409-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       4091-DELETE-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX-W
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-KA021MEISAI-MAX )  OR
                              (IDX-W   >   ZERO      )
               IF      SPA-KA021-SELNUM    =   SPA-KA021-TBANGO (IDX)
                   MOVE    IDX                     TO  IDG
                   MOVE    SPA-KA021NAI-TBANGO (IDX)   TO  IDX-W
               END-IF
           END-PERFORM
      *
           IF      IDX-W           NOT =   ZERO
               MOVE    SPACE               TO  SPA-PARA-ZAI-REC (IDX-W)
               INITIALIZE                      SPA-PARA-ZAI-REC (IDX-W)
      *
      *        削除
               COMPUTE SPA-PARA-MAX        =   SPA-PARA-MAX   -   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >=  100
                   IF      SPA-PARA-ZAINUM (IDX)   =   ZERO
                       MOVE    SPA-PARA-ZAI-REC (IDX + 1 )
                                           TO  SPA-PARA-ZAI-REC (IDX)
                       MOVE    SPACE       TO  SPA-PARA-ZAI-REC
                                                            (IDX + 1)
                       INITIALIZE             SPA-PARA-ZAI-REC
                                                            (IDX + 1)
                   END-IF
               END-PERFORM
           END-IF
           IF      SPA-PARA-MAX        =   ZERO
               INITIALIZE                  SPA-KA02-DATA
               INITIALIZE                  SPA-PARA-WKSRY-TBL
           END-IF
      *
      *    初期表示
           PERFORM 3001-SPA-SET-SEC
           .
       4091-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    一括削除処理
      *****************************************************************
       411-ALLDELETE-SEC       SECTION.
      *
           MOVE    "0102"          TO  SPA-KAID1CD
      *
           .
       411-ALLDELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括削除処理
      *****************************************************************
       4111-ALLDELETE-SYORI-SEC             SECTION.
      *
           INITIALIZE                  SPA-KA02-DATA
           INITIALIZE                  SPA-PARA-WKSRY-TBL
      *    初期表示
           PERFORM 3001-SPA-SET-SEC
      *
           .
       4111-ALLDELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
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
       800-SEIWA-HEN-EXT.
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
           IF      SPA-KAID1CD       NOT =   SPACE
               PERFORM 520-KA02IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "KA021"             TO  MCP-WINDOW.
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
           IF      FLG-GMN-INIT1       =   1
               MOVE    ZERO                TO  WRK-KA02-MEILIST-ROW
           ELSE
               MOVE    KA021-MEILIST-ROW   TO  WRK-KA02-MEILIST-ROW
           END-IF
      *
           MOVE    SPACE               TO  KA021
      *    コラムリストの位置指定
           INITIALIZE                      KA021
           MOVE    ZERO                TO  KA021-MEILIST-ROW
           IF      WRK-KA02-MEILIST-ROW NOT =   ZERO
               MOVE    WRK-KA02-MEILIST-ROW TO  KA021-MEILIST-ROW
           END-IF
      *    交付日
           STRING  "【交付日："
                   SPA-GMN-KOUFUYMD
                   " 】"               DELIMITED  BY  SIZE
                                       INTO  KA021-PRTYMD
           END-STRING
      *    病棟・病室
           STRING  "【病棟："
                   SPA-NAI-BTUNAME     DELIMITED  BY  SPACE
                   "　"                DELIMITED  BY  SIZE
                   SPA-NAI-ROOMNO      DELIMITED  BY  SIZE
                   " 】"               DELIMITED  BY  SIZE
                                       INTO  KA021-BTUNAME
           END-STRING
      *
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX     >   SPA-KA021MEISAI-MAX
               IF      SPA-KA021-TMEISYO(IDX)(1:1)   =   "-"
                   MOVE    ALL "-"             TO  KA021-MNUM  (IDX)
                   MOVE    ALL "-"             TO  KA021-MKBN  (IDX)
                   MOVE    ALL "−"            TO  KA021-MEISAI(IDX)
               ELSE
                   IF      SPA-KA021-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-KA021-TBANGO (IDX)    TO  WRK-ZZ
                       MOVE    WRK-ZZ              TO  KA021-MNUM (IDX)
                   ELSE
                       MOVE    SPACE               TO  KA021-MNUM (IDX)
                   END-IF
      *            期間外
                   IF     (SPA-KA021NAI-ERRFLG(IDX) =   1     )
                       MOVE    "外"            TO  KA021-MKBN (IDX)
                   END-IF
                   MOVE    SPA-KA021-TMEISYO(IDX)
                                           TO  KA021-MEISAI (IDX)
               END-IF
               IF     (SPA-KA021-TMEISYO (IDX)   NOT =   ALL "-" )  AND
                      (SPA-KA021-TBANGO (IDX)    >   ZERO  )  AND
                      (SPA-KA021-TBANGO (IDX)    =   SPA-KA021-SELNUM)
                   MOVE    "T"             TO  KA021-MEILIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  KA021-MEILIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-KA021MEISAI-MAX   TO  KA021-MEILIST-COUNT
      *
           MOVE    SPA-KA021-SELNUM    TO  KA021-SELNUM
      *
           IF      SPA-KA021-NEXT      =   1
               IF      SPA-KA021-PAGE      >   1
                   MOVE    "前・次頁あり"      TO  KA021-NEXT-VALUE
                   MOVE    "red"               TO  KA021-NEXT-STYLE
               ELSE
                   MOVE    "次頁あり"          TO  KA021-NEXT-VALUE
                   MOVE    "red"               TO  KA021-NEXT-STYLE
               END-IF
           ELSE
               IF      SPA-KA021-PAGE      >   1
                   MOVE    "前頁あり"      TO  KA021-NEXT-VALUE
                   MOVE    "red"           TO  KA021-NEXT-STYLE
               END-IF
           END-IF
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
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
           IF      SPA-KA021-CUR     =   ZERO
               MOVE    1             TO  SPA-KA021-CUR
           END-IF
      *
           EVALUATE    SPA-KA021-CUR
               WHEN    01
                   MOVE    "SELNUM"         TO  MCP-WIDGET
               WHEN    09
                   MOVE    "B04"             TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
                   MOVE    "削除番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "削除番号を選択して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KAERR
           MOVE    SPA-ERRCD           TO  KAERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KAERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KA021"             TO  SPA-MOTOPG
           MOVE    "KAERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KAERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KA02IDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KAID1
           INITIALIZE                      KAID1
           EVALUATE    SPA-KAID1CD
               WHEN    "0101"
                   STRING  "選択した剤を削除します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID1-ID1MSG
                   END-STRING
               WHEN    "0102"
                   STRING  "選択内容をすべて削除します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID1-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KAID1CD
                                       TO  KAID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-KAID1CD         TO  KAID1-ID1CODE
      *
           MOVE    SPACE               TO  SPA-KAID1-FLG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "KA021"             TO  SPA-MOTOPG
           MOVE    "KAID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KAID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KA02IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
               PERFORM 980-TENSUPLUS-READ-SEC
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-NAI-KOUFUYMD    TO  TNSP-YUKOSTYMD
           MOVE    SPA-NAI-KOUFUYMD    TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ユーザー単位設定
           IF     (FLG-TENSUPLUS       =   ZERO  )
              AND (TNSP-USERTANICD NOT =   SPACE )
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
      *
           .
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *TEST
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       910-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       990-SPATMP-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *TEST
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
       990-DBCLOSE-SEC           SECTION.
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
