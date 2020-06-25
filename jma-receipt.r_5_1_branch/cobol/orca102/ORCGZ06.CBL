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
       PROGRAM-ID.                               ORCGZ06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ用法設置画面（Ｚ０６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/18    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/12/09  MONFUNC対応 他
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.01.00    NACL-森脇    07/11/28  起床時追加
      *  04.06.00    NACL-多々納  11/02/14  コメント対応
      *  04.07.00    NACL-多々納  12/08/22  ユーザ登録複写機能追加
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
       01  SPA-Z06.
      *    第二画面ＳＰＡエリア
           03  SPA-GMN-CUR                     PIC 9(02).
           03  SPA-GMN-AREA.
               05  SPA-GMN-SRYCDH              PIC X(09).
               05  SPA-GMN-NAMEH               PIC X(100).
               05  SPA-GMN-YUKOSTYMDH          PIC X(09).
               05  SPA-GMN-YUKOEDYMDH          PIC X(09).
               05  SPA-GMN-YUKOSTYMD           PIC X(10).
               05  SPA-GMN-YUKOEDYMD           PIC X(10).
               05  SPA-GMN-KANANAME            PIC X(100).
               05  SPA-GMN-NAME                PIC X(100).
               05  SPA-GMN-FORMALNAME          PIC X(200).
               05  SPA-GMN-JITEN1-G.
                   07  SPA-GMN-JITEN1-X.
                       09   SPA-GMN-JITEN1          PIC 9(01).
                   07  SPA-GMN-H1              PIC X(01).
                   07  SPA-GMN-JITEN1MEI       PIC X(10).
               05  SPA-GMN-JITEN1-LST          OCCURS  2.
                   07  SPA-GMN-JITEN1L         PIC X(01).
                   07  SPA-GMN-H1L             PIC X(01).
                   07  SPA-GMN-JITEN1MEIL      PIC X(10).
      *
               05  SPA-GMN-JITEN2-G.
                   07  SPA-GMN-JITEN2          PIC 9(01).
                   07  SPA-GMN-JITEN2-X        REDEFINES
                       SPA-GMN-JITEN2          PIC X(01).
                   07  SPA-GMN-H2              PIC X(01).
                   07  SPA-GMN-JITEN2MEI       PIC X(10).
               05  SPA-GMN-JITEN2-LST          OCCURS  2.
                   07  SPA-GMN-JITEN2L         PIC X(01).
                   07  SPA-GMN-H2L             PIC X(01).
                   07  SPA-GMN-JITEN2MEIL      PIC X(10).
      *
               05  SPA-GMN-JITEN3-G.
                   07  SPA-GMN-JITEN3          PIC 9(01).
                   07  SPA-GMN-JITEN3-X        REDEFINES
                       SPA-GMN-JITEN3          PIC X(01).
                   07  SPA-GMN-H3              PIC X(01).
                   07  SPA-GMN-JITEN3MEI       PIC X(10).
               05  SPA-GMN-JITEN3-LST          OCCURS  2.
                   07  SPA-GMN-JITEN3L         PIC X(01).
                   07  SPA-GMN-H3L             PIC X(01).
                   07  SPA-GMN-JITEN3MEIL      PIC X(10).
      *
               05  SPA-GMN-JITEN4-G.
                   07  SPA-GMN-JITEN4          PIC 9(01).
                   07  SPA-GMN-JITEN4-X        REDEFINES
                       SPA-GMN-JITEN4          PIC X(01).
                   07  SPA-GMN-H4              PIC X(01).
                   07  SPA-GMN-JITEN4MEI       PIC X(10).
               05  SPA-GMN-JITEN4-LST          OCCURS  2.
                   07  SPA-GMN-JITEN4L         PIC X(01).
                   07  SPA-GMN-H4L             PIC X(01).
                   07  SPA-GMN-JITEN4MEIL      PIC X(10).
      *
      *----(04.01.00)--ADD-START-------------------------------------
               05  SPA-GMN-JITEN5-G.
                   07  SPA-GMN-JITEN5          PIC 9(01).
                   07  SPA-GMN-JITEN5-X        REDEFINES
                       SPA-GMN-JITEN5          PIC X(01).
                   07  SPA-GMN-H5              PIC X(01).
                   07  SPA-GMN-JITEN5MEI       PIC X(10).
               05  SPA-GMN-JITEN5-LST          OCCURS  2.
                   07  SPA-GMN-JITEN5L         PIC X(01).
                   07  SPA-GMN-H5L             PIC X(01).
                   07  SPA-GMN-JITEN5MEIL      PIC X(10).
      *----(04.01.00)--ADD-END---------------------------------------
      *
      *        カラム位置
               05  SPA-GMN-DIGIT-G.
                   07  SPA-GMN-DIGIT           PIC 9(03)
                                               OCCURS   5.
      *        コメント区分
               05  SPA-GMN-COMMENTKBN-G.
                   07  SPA-GMN-COMMENTKBN      PIC X(01).
                   07  SPA-GMN-COMMENTKBN-F    PIC X(01).
                   07  SPA-GMN-COMMENTKBN-MEI  PIC X(28).
               05  SPA-GMN-COMMENTKBN-LSTG.
                   07  SPA-GMN-COMMENTKBN-LST  PIC X(30)
                                               OCCURS   5.
               05  SPA-GMN-COMKBN-MAX          PIC 9(02).
      *
      *        複写元コード
               05  SPA-GMN-COPYCD              PIC X(09).
      *
      *    第二画面内容
           03  SPA-NAI-AREA.
               05  SPA-NAI-YUKOSTYMD           PIC X(08).
               05  SPA-NAI-YUKOEDYMD           PIC X(08).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
      *
           03  FLG-END                           PIC 9(01).
           03  FLG-TOUROKU                       PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDZ                               PIC 9(04).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-ZIDMSG                        PIC X(70).
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 200.
      *
           03  WRK-YUKOEDYMD                     PIC X(08).
      *
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *v47
           03  WRK-INPUTCD             PIC X(09).
           03  WRK-CD-MCNT             PIC 9(02).
           03  FLG-KANJI               PIC 9(01).
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
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           INITIALIZE                      FLG-AREA
           INITIALIZE                      WRK-AREA
           INITIALIZE                      CNT-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z06
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF
      *
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z06                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
           .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z06"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                             TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *    更新処理
               WHEN    "Z99"
                   MOVE    10              TO  SPA-GMN-CUR
      *
      *    検索画面より
               WHEN    "Z98"
                   PERFORM 320-Z98SPASET-SEC
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
       310-SPASET-SEC                  SECTION.
      *
      *    点数マスタ該当データ読み出し
           INITIALIZE                      SPA-Z06
      *
           MOVE    SPA-Z01-SRYCD       TO  SPA-GMN-SRYCDH
      *
           MOVE    SPA-Z01-YUKOSTYMD   TO  SPA-NAI-YUKOSTYMD
           IF      SPA-Z01-SYORIKBN    =   2
               MOVE    SPA-Z01-YUKOSTYMDH  TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE    SPA-Z01-YUKOSTYMDH  TO  SPA-GMN-YUKOSTYMD
      *
           MOVE    SPA-Z01-YUKOEDYMD   TO  SPA-NAI-YUKOEDYMD
           IF      SPA-Z01-SYORIKBN    =   2
                MOVE   SPA-Z01-YUKOEDYMDH  TO  SPA-GMN-YUKOEDYMDH
           END-IF
           MOVE    SPA-Z01-YUKOEDYMDH  TO  SPA-GMN-YUKOEDYMD
      *
      *    服用時点リスト
           MOVE    "0"                 TO  SPA-GMN-JITEN1L    (1)
           MOVE    "服用しない"        TO  SPA-GMN-JITEN1MEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JITEN1L    (2)
           MOVE    "服用する"          TO  SPA-GMN-JITEN1MEIL (2)
      *
           MOVE    "0"                 TO  SPA-GMN-JITEN2L    (1)
           MOVE    "服用しない"        TO  SPA-GMN-JITEN2MEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JITEN2L    (2)
           MOVE    "服用する"          TO  SPA-GMN-JITEN2MEIL (2)
      *
           MOVE    "0"                 TO  SPA-GMN-JITEN3L    (1)
           MOVE    "服用しない"        TO  SPA-GMN-JITEN3MEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JITEN3L    (2)
           MOVE    "服用する"          TO  SPA-GMN-JITEN3MEIL (2)
      *
           MOVE    "0"                 TO  SPA-GMN-JITEN4L    (1)
           MOVE    "服用しない"        TO  SPA-GMN-JITEN4MEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JITEN4L    (2)
           MOVE    "服用する"          TO  SPA-GMN-JITEN4MEIL (2)
      *
      *----(04.01.00)--ADD-START-------------------------------------
           MOVE    "0"                 TO  SPA-GMN-JITEN5L    (1)
           MOVE    "服用しない"        TO  SPA-GMN-JITEN5MEIL (1)
           MOVE    "1"                 TO  SPA-GMN-JITEN5L    (2)
           MOVE    "服用する"          TO  SPA-GMN-JITEN5MEIL (2)
      *----(04.01.00)--ADD-END---------------------------------------
      *
      *    コメント区分
           MOVE    "0 服用方法"        TO  SPA-GMN-COMMENTKBN-LST (1)
           MOVE    "1 服用方法コメント有"
                                       TO  SPA-GMN-COMMENTKBN-LST (2)
           MOVE    "2 コメント"        TO  SPA-GMN-COMMENTKBN-LST (3)
           MOVE    3                   TO  SPA-GMN-COMKBN-MAX
      *
      *    点数マスタ検索
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                  TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           IF      SPA-Z01-SELNUM      =   ZERO
               MOVE    ALL "9"             TO  TNS-YUKOSTYMD
               MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           ELSE
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *
               MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
      *
               MOVE    TNS-NAME            TO  SPA-GMN-NAME
      *
               MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
               MOVE    TNS-SSTKIJUNCD(1)   TO  SPA-GMN-JITEN1
               MOVE    TNS-SSTKIJUNCD(2)   TO  SPA-GMN-JITEN2
               MOVE    TNS-SSTKIJUNCD(3)   TO  SPA-GMN-JITEN3
               MOVE    TNS-SSTKIJUNCD(4)   TO  SPA-GMN-JITEN4
               MOVE    TNS-SSTKIJUNCD(5)   TO  SPA-GMN-JITEN5
      *        カラム位置
               MOVE    TNS-SSTKIJUNCD(6)   TO  SPA-GMN-DIGIT (1)
               MOVE    TNS-SSTKIJUNCD(7)   TO  SPA-GMN-DIGIT (2)
               MOVE    TNS-SSTKIJUNCD(8)   TO  SPA-GMN-DIGIT (3)
               MOVE    TNS-SSTKIJUNCD(9)   TO  SPA-GMN-DIGIT (4)
               MOVE    TNS-SSTKIJUNCD(10)  TO  SPA-GMN-DIGIT (5)
      *        コメント区分
               MOVE    TNS-KENTAICOMMENT   TO  SPA-GMN-COMMENTKBN
           END-IF
      *
      *    各リストボックス編集
           PERFORM 3101-COB-HEN-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    各リストボックス編集処理
      *****************************************************************
       3101-COB-HEN-SEC          SECTION.
      *
      *    朝
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JITEN1-X    =   SPA-GMN-JITEN1L(IDX)
                   MOVE    SPA-GMN-JITEN1-LST (IDX)
                                           TO  SPA-GMN-JITEN1-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *    昼
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JITEN2-X    =   SPA-GMN-JITEN2L(IDX)
                   MOVE    SPA-GMN-JITEN2-LST (IDX)
                                           TO  SPA-GMN-JITEN2-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *    夜
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JITEN3-X    =   SPA-GMN-JITEN3L(IDX)
                   MOVE    SPA-GMN-JITEN3-LST (IDX)
                                           TO  SPA-GMN-JITEN3-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *    寝前
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JITEN4-X    =   SPA-GMN-JITEN4L(IDX)
                   MOVE    SPA-GMN-JITEN4-LST (IDX)
                                           TO  SPA-GMN-JITEN4-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *    起床
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-JITEN5-X    =   SPA-GMN-JITEN5L(IDX)
                   MOVE    SPA-GMN-JITEN5-LST (IDX)
                                           TO  SPA-GMN-JITEN5-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *
      *    コメント区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-COMKBN-MAX
               IF      SPA-GMN-COMMENTKBN  =   SPA-GMN-COMMENTKBN-LST
                                                      (IDX)(1:1)
                  MOVE    SPA-GMN-COMMENTKBN-LST (IDX)
                                           TO  SPA-GMN-COMMENTKBN-G
                   MOVE    SPA-GMN-COMKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
           .
       3101-COB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為コードセット処理
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
      *        複写
               MOVE    SPA-Z98-SRYCD       TO  SPA-GMN-COPYCD
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-GMN-COPYCD
           END-IF 
           .
       320-Z98SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
      *        削除
               WHEN    "0001"
                   MOVE    SPACE           TO  SPA-ZIDCD
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
      *        登録
               WHEN    "0002"
                   MOVE    SPACE           TO  SPA-ZIDCD
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4121-TOUROKU-SEC
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                 SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE  "CHANGE"      TO  MCP-PUTTYPE
                   PERFORM  210-BACK-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        入力コード設定
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-INPUTCD-SEC
      *v47
      *        前回複写
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM  406-ZENCOPY-SEC
      *        検索
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE       TO  WRK-INPUTCD
                   PERFORM 409-KENSAKU-SEC
      *
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        複写先コード
               WHEN    "ACTIVATE"      ALSO    "COPYCD"
                   PERFORM 502-COPYCD-CHK-SEC
      *        一括入力分
               WHEN    OTHER
                   PERFORM 2001-ENTRY-SYORI-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他一括処理
      *****************************************************************
       2001-ENTRY-SYORI-SEC         SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       2001-ENTRY-SYORI-EXT.
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
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
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
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    Z06-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z06-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
           MOVE    Z06-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z06-NAME            TO  SPA-GMN-NAME
           MOVE    Z06-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
           MOVE    Z06-JITEN1          TO  SPA-GMN-JITEN1-G
           MOVE    Z06-JITEN2          TO  SPA-GMN-JITEN2-G
           MOVE    Z06-JITEN3          TO  SPA-GMN-JITEN3-G
           MOVE    Z06-JITEN4          TO  SPA-GMN-JITEN4-G
           MOVE    Z06-JITEN5          TO  SPA-GMN-JITEN5-G
      *    カラム位置
           MOVE    Z06-DIGIT1          TO  SPA-GMN-DIGIT (1)
           MOVE    Z06-DIGIT2          TO  SPA-GMN-DIGIT (2)
           MOVE    Z06-DIGIT3          TO  SPA-GMN-DIGIT (3)
           MOVE    Z06-DIGIT4          TO  SPA-GMN-DIGIT (4)
           MOVE    Z06-DIGIT5          TO  SPA-GMN-DIGIT (5)
           MOVE    Z06-COMMENTKBN      TO  SPA-GMN-COMMENTKBN-G
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
      *    有効開始年月日チェック
           PERFORM 501-YUKOSTYMDCHK-SEC
      *    有効開始年月日チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 502-YUKOEDYMDCHK-SEC
           END-IF
      *    カナ名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 503-KANANAME-CHK-SEC
           END-IF
      *    漢字名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 504-NAME-CHK-SEC
           END-IF
      *    通称名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 505-FORMALNAME-CHK-SEC
           END-IF
      *    朝チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-JITEN1-CHK-SEC
           END-IF
      *    昼チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-JITEN2-CHK-SEC
           END-IF
      *    夜チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-JITEN3-CHK-SEC
           END-IF
      *    寝前チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-JITEN4-CHK-SEC
           END-IF
      *    起床チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-JITEN5-CHK-SEC
           END-IF
      *    カラム位置チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-DIGIT-CHK-SEC
           END-IF
      *    コメント区分チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 506-COMMENTKBN-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC            SECTION.
      *
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       501-YUKOSTYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       502-YUKOEDYMDCHK-SEC                      SECTION.
      *
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "9999999"           TO  SPA-GMN-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ名称チェック処理
      *****************************************************************
       503-KANANAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    漢字名称チェック処理
      *****************************************************************
       504-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       504-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通称名称チェック処理
      *****************************************************************
       505-FORMALNAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-FORMALNAME  TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-FORMALNAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       505-FORMALNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    服用時点朝チェック処理
      *****************************************************************
       506-JITEN1-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   2   )  OR
                              (FLG-OK      =   1  )
               IF      SPA-GMN-JITEN1-X    =   SPA-GMN-JITEN1L (IDX)
                   MOVE    SPA-GMN-JITEN1-LST (IDX)
                                           TO  SPA-GMN-JITEN1-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-JITEN1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    服用時点昼チェック処理
      *****************************************************************
       506-JITEN2-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   2   )  OR
                              (FLG-OK      =   1  )
               IF      SPA-GMN-JITEN2-X    =   SPA-GMN-JITEN2L (IDX)
                   MOVE    SPA-GMN-JITEN2-LST (IDX)
                                           TO  SPA-GMN-JITEN2-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-JITEN2-CHK-EXT.
           EXIT.
      *****************************************************************
      *    服用時点夕チェック処理
      *****************************************************************
       506-JITEN3-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   2   )  OR
                              (FLG-OK      =   1  )
               IF      SPA-GMN-JITEN3-X    =   SPA-GMN-JITEN3L (IDX)
                   MOVE    SPA-GMN-JITEN3-LST (IDX)
                                           TO  SPA-GMN-JITEN3-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-JITEN3-CHK-EXT.
           EXIT.
      *****************************************************************
      *    服用時点朝チェック処理
      *****************************************************************
       506-JITEN4-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   2   )  OR
                              (FLG-OK      =   1  )
               IF      SPA-GMN-JITEN4-X    =   SPA-GMN-JITEN4L (IDX)
                   MOVE    SPA-GMN-JITEN4-LST (IDX)
                                           TO  SPA-GMN-JITEN4-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-JITEN4-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    服用時点起床チェック処理
      *****************************************************************
       506-JITEN5-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   2   )  OR
                              (FLG-OK      =   1  )
               IF      SPA-GMN-JITEN5-X    =   SPA-GMN-JITEN5L (IDX)
                   MOVE    SPA-GMN-JITEN5-LST (IDX)
                                           TO  SPA-GMN-JITEN5-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0016"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       506-JITEN5-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメント位置チェック処理
      *****************************************************************
       506-DIGIT-CHK-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL      (IDX         >   5  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-GMN-DIGIT (IDX) NOT =   ZERO )
      *            前ゼロはエラー
                   PERFORM VARYING     IDZ     FROM    IDX   BY  -1
                           UNTIL      (IDZ         <   1  ) 
                                   OR (SPA-ERRCD   NOT =   SPACE )
                       IF      SPA-GMN-DIGIT (IDZ)     =   ZERO
                           MOVE    "0018"              TO  SPA-ERRCD
                           MOVE    1                   TO  IDZ
                       END-IF
                   END-PERFORM
                   IF      SPA-ERRCD       NOT =   SPACE
                       EVALUATE    IDX
                       WHEN    2
                           MOVE    13          TO  SPA-GMN-CUR
                       WHEN    3
                           MOVE    14          TO  SPA-GMN-CUR
                       WHEN    4
                           MOVE    15          TO  SPA-GMN-CUR
                       WHEN    5
                           MOVE    16          TO  SPA-GMN-CUR
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       506-DIGIT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    コメント区分チェック処理
      *****************************************************************
       506-COMMENTKBN-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-GMN-COMKBN-MAX )  OR
                              (FLG-OK      =   1  )
               IF      SPA-GMN-COMMENTKBN  =   SPA-GMN-COMMENTKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-COMMENTKBN-LST (IDX)
                                               TO  SPA-GMN-COMMENTKBN-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
           .
       506-COMMENTKBN-CHK-EXT.
           EXIT.
      *
      *v47
      ****************************************************************
      *    複写先コード処理
      ****************************************************************
       502-COPYCD-CHK-SEC      SECTION.
      *
           MOVE    Z06-COPYCD          TO  SPA-GMN-COPYCD
      *
           IF      SPA-GMN-COPYCD      =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      502-COPYCD-CHK-EXT
           END-IF
      *
           MOVE    18                  TO  SPA-GMN-CUR
      *    全角入力の時、４文字まで
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    Z06-COPYCD          TO  KANACHK-MAE-INPUT
           MOVE    9                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-COPYCD
      *
      *    "//" 入力の時、検索画面へ
           IF     SPA-GMN-COPYCD(1:2)  =  "//"
               MOVE    SPACE               TO  WRK-INPUTCD
               PERFORM 409-KENSAKU-SEC
           ELSE
      *        入力コードチェック
               IF      SPA-GMN-COPYCD      NOT =   SPACE
                   MOVE    SPA-GMN-COPYCD      TO  WRK-INPUTCD
                   PERFORM 4010-SRYCD-CHK-SEC
               END-IF
           END-IF
      *
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
           END-IF
           IF     (FLG-END             =   ZERO  )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-COPYCD  NOT =   SPACE )
      *        複写処理
               PERFORM 5021-COPYCD-SYORI-SEC
           END-IF
           .
       502-COPYCD-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    複写チェック処理
      ****************************************************************
       5022-COPYCD-CDCHK-SEC      SECTION.
      *
      *    用法コード
           IF      (SPA-GMN-COPYCD(1:3)    =   "001" )
               CONTINUE
           ELSE
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
           .
       5022-COPYCD-CDCHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    複写処理処理
      ****************************************************************
       5021-COPYCD-SYORI-SEC      SECTION.
      *
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-GMN-COPYCD      TO  TNS-SRYCD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 50211-COPYTENSU-SET-SEC
               MOVE    SPACE               TO  SPA-GMN-COPYCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    "0021"              TO  SPA-ERRCD
           END-IF
           .
       5021-COPYCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡに複写
      *****************************************************************
       50211-COPYTENSU-SET-SEC         SECTION.
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *
           MOVE    TNS-SSTKIJUNCD(1)   TO  SPA-GMN-JITEN1
           MOVE    TNS-SSTKIJUNCD(2)   TO  SPA-GMN-JITEN2
           MOVE    TNS-SSTKIJUNCD(3)   TO  SPA-GMN-JITEN3
           MOVE    TNS-SSTKIJUNCD(4)   TO  SPA-GMN-JITEN4
           MOVE    TNS-SSTKIJUNCD(5)   TO  SPA-GMN-JITEN5
      *    カラム位置
           MOVE    TNS-SSTKIJUNCD(6)   TO  SPA-GMN-DIGIT (1)
           MOVE    TNS-SSTKIJUNCD(7)   TO  SPA-GMN-DIGIT (2)
           MOVE    TNS-SSTKIJUNCD(8)   TO  SPA-GMN-DIGIT (3)
           MOVE    TNS-SSTKIJUNCD(9)   TO  SPA-GMN-DIGIT (4)
           MOVE    TNS-SSTKIJUNCD(10)  TO  SPA-GMN-DIGIT (5)
      *    コメント区分
           MOVE    TNS-KENTAICOMMENT   TO  SPA-GMN-COMMENTKBN
      *
      *    各リストボックス編集
           PERFORM 3101-COB-HEN-SEC
      *
           .
       50211-COPYTENSU-SET-EXT.
           EXIT.
      *****************************************************************
      *  　入力コードチェック処理
      *****************************************************************
       4010-SRYCD-CHK-SEC                        SECTION.
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
               GO      TO    4010-SRYCD-CHK-EXT
           ELSE
               MOVE    KANACHK-MAX         TO  WRK-CD-MCNT
      *        全角
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
               ELSE
                   MOVE    ZERO            TO  FLG-KANJI
               END-IF
           END-IF
      *    半角で９文字の時、点数マスタチェックへ
           IF     (WRK-CD-MCNT         =   9    )  AND
                  (FLG-KANJI           =   ZERO )  AND
                  (WRK-INPUTCD         NUMERIC  )
               CONTINUE
           ELSE
      *        入力コード検索
               PERFORM 409-KENSAKU-SEC
           END-IF
      *
           .
       4010-SRYCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索処理
      *****************************************************************
       409-KENSAKU-SEC             SECTION.
      *
      *    共通ＳＰＡ編集
           INITIALIZE                  SPA-Z98-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           INITIALIZE                  SPA-Z98GMN-AREA
           IF      WRK-INPUTCD         NOT =   SPACE
      *        器材
               MOVE    "7"                 TO  SPA-Z98-GMN-SYORI
               MOVE    WRK-INPUTCD         TO  SPA-Z98-INPUTCD
               MOVE    "1"                 TO  SPA-Z98-TYPE
               IF      FLG-KANJI           =   1
                   MOVE    "J"             TO  SPA-Z98-CDSYUBETU
               ELSE
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
           MOVE    "Z06"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z06"               TO  SPA-Z98-MOTOPG
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
       409-KENSAKU-EXT.
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
           MOVE    SPA-Z06             TO  SPA-Z01SONOTA
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
           MOVE    SPA-COMMON                    TO  SPA-AREA
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z06
      *
           .
       41011-Z98-SET-EXT.
           EXIT.
      *****************************************************************
      *    前回コード複写処理
      *****************************************************************
       406-ZENCOPY-SEC          SECTION.
      *
           IF      SPA-Z08MAE-SRYCD        =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SPA-Z08MAE-SRYCD    TO  SPA-GMN-COPYCD
      *        複写チェック
               PERFORM 5022-COPYCD-CDCHK-SEC
               IF     (FLG-END             =   ZERO  )  AND
                      (SPA-ERRCD           =   SPACE )
      *            複写処理
                   PERFORM 5021-COPYCD-SYORI-SEC
               END-IF 
           END-IF 
           .
       406-ZENCOPY-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >=  SPA-NAI-YUKOEDYMD
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    有効期限範囲チェック
           IF      SPA-Z01-SYORIKBN    =   2
      *        前チェック
               IF      SPA-Z01-SELNUM      >   1
                   MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM - 1)
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
      *******:     IF      SPA-NAI-YUKOSTYMD   NOT =   WRK-OTYMD
                   IF      SPA-NAI-YUKOSTYMD       <   WRK-OTYMD
                       MOVE    "0011"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *        後チェック
               IF      SPA-Z01-SELNUM      <   SPA-Z01-MAX
                   MOVE    SPA-NAI-YUKOEDYMD
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
                   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
                                                   <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *    改正チェック
           IF      SPA-Z01-SYORIKBN    =   3
               IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           >=  SPA-NAI-YUKOSTYMD
                   MOVE    "1012"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
               IF  SPA-NAI-YUKOEDYMD    NOT    =  ALL "9"
                   MOVE    "1013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    "1014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SEC                    SECTION.
      *
           MOVE    "Z01"               TO  SPA-SAKIPG
      *
           MOVE    "Z06"               TO  SPA-MOTOPG
      *
      ***  MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                     SECTION.
      *
      *    新規・改正は削除処理なし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               GO      TO   403-DEL-EXT
           END-IF
      *
           MOVE    "0001"              TO  SPA-ZIDCD
      *
           .
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
      *    点数マスタ削除
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z03 TENSU DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      *    入力コード削除
           IF     (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               MOVE     SPA-HOSPNUM        TO  ICD-HOSPNUM
               MOVE     SPA-Z01-SRYCD      TO  ICD-SRYCD
               MOVE     INPUTCD-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-Z08MAE-SRYCD
      *
               MOVE    "JOIN"              TO  MCP-PUTTYPE
               PERFORM  210-BACK-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                              SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z06             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z06
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z06"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z99GMN-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       405-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
      *    登録確認メッセージ
           MOVE    "0002"              TO  SPA-ZIDCD
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-TOUROKU-SEC                          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *
      *    前画面に戻る
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-Z01-SRYCD       TO  SPA-Z08MAE-SRYCD
      *
               MOVE    "JOIN"              TO  MCP-PUTTYPE
               PERFORM 210-BACK-SEC
           END-IF
           .
       4121-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正時処理
      *****************************************************************
       41211-KAISEI-SYORI-SEC          SECTION.
      *
      *    有効終了日を決定する
      *    今回の有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOSTYMD   TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-YUKOEDYMD
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41211-KAISEI-SYORI-EXT
           END-IF
      *
      *    有効年月日を更新する
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      ***  MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新
               MOVE    WRK-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z02 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41211-KAISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41212-UPDATE-SYORI-SEC                       SECTION.
      *
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新項目編集
               MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
               PERFORM 4121-KOUMOKUISO-SEC
      *
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "tbl_tensu_user"    TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z02 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41212-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41213-INSERT-SYORI-SEC          SECTION.
      *
      *    データＩＮＳＥＲＴ
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
           PERFORM 4121-KOUMOKUISO-SEC
      *
           MOVE    SPA-OPID            TO  TNS-OPID
           MOVE    SMCNDATE-YMD        TO  TNS-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
     ****  MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z02 TENSU INS ERR:" MCP-RC
                       ",KEY:" TNS-KEY
           END-IF
      *
           .
       41213-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-KOUMOKUISO-SEC                       SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  TNS-KANANAME
           MOVE    SPA-GMN-KANANAME    TO  WRK-STRINGTBL
           MOVE    100                 TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE    CNT-STRING          TO  TNS-KANAYUKOKETA
      *
           MOVE    SPA-GMN-NAME        TO  TNS-NAME
                                           WRK-STRINGTBL
           MOVE    100                 TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE    CNT-STRING          TO  TNS-YUKOKETA
      *
           MOVE    SPA-GMN-FORMALNAME  TO  TNS-FORMALNAME
                                           WRK-STRINGTBL
           MOVE    200                 TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE    CNT-STRING          TO  TNS-FORMALYUKOKETA
      *    服用時点を施設基準へ
           MOVE    SPA-GMN-JITEN1      TO  TNS-SSTKIJUNCD(1)
           MOVE    SPA-GMN-JITEN2      TO  TNS-SSTKIJUNCD(2)
           MOVE    SPA-GMN-JITEN3      TO  TNS-SSTKIJUNCD(3)
           MOVE    SPA-GMN-JITEN4      TO  TNS-SSTKIJUNCD(4)
           MOVE    SPA-GMN-JITEN5      TO  TNS-SSTKIJUNCD(5)
      *    カラム位置
           MOVE    SPA-GMN-DIGIT (1)   TO  TNS-SSTKIJUNCD(6)
           MOVE    SPA-GMN-DIGIT (2)   TO  TNS-SSTKIJUNCD(7)
           MOVE    SPA-GMN-DIGIT (3)   TO  TNS-SSTKIJUNCD(8)
           MOVE    SPA-GMN-DIGIT (4)   TO  TNS-SSTKIJUNCD(9)
           MOVE    SPA-GMN-DIGIT (5)   TO  TNS-SSTKIJUNCD(10)
      *    コメント区分
           MOVE    SPA-GMN-COMMENTKBN  TO  TNS-KENTAICOMMENT
      *
           .
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       400-ZENCHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
           .
      *
       400-ZENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC             SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "Z06    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *
            MOVE    SPACE                  TO  Z06
            INITIALIZE                     Z06
      *
            MOVE   SPA-GMN-SRYCDH          TO  Z06-SRYCDH
            MOVE   SPA-GMN-NAMEH           TO  Z06-NAMEH
            MOVE   SPA-GMN-YUKOSTYMDH      TO  Z06-YUKOSTYMDH
            MOVE   SPA-GMN-YUKOEDYMDH      TO  Z06-YUKOEDYMDH
      *
            MOVE   SPA-GMN-YUKOSTYMD       TO  Z06-YUKOSTYMD
            MOVE   SPA-GMN-YUKOEDYMD       TO  Z06-YUKOEDYMD
      *
            MOVE   SPA-GMN-KANANAME        TO  Z06-KANANAME
            MOVE   SPA-GMN-NAME            TO  Z06-NAME
            MOVE   SPA-GMN-FORMALNAME      TO  Z06-FORMALNAME
      *
            MOVE   SPA-GMN-JITEN1-G        TO  Z06-JITEN1
            PERFORM VARYING    IDX     FROM    1   BY  1
                    UNTIL      IDX     >   2
               MOVE    SPA-GMN-JITEN1-LST(IDX) TO  Z06-JITEN1-ITEM(IDX)
               MOVE    IDX                     TO  Z06-JITEN1-COUNT
            END-PERFORM
      *
            MOVE   SPA-GMN-JITEN2-G        TO  Z06-JITEN2
            PERFORM VARYING    IDX     FROM    1   BY  1
                    UNTIL      IDX     >   2
               MOVE    SPA-GMN-JITEN2-LST(IDX) TO  Z06-JITEN2-ITEM(IDX)
               MOVE    IDX                     TO  Z06-JITEN2-COUNT
            END-PERFORM
      *
            MOVE   SPA-GMN-JITEN3-G        TO  Z06-JITEN3
            PERFORM VARYING    IDX     FROM    1   BY  1
                    UNTIL      IDX     >   2
               MOVE    SPA-GMN-JITEN3-LST(IDX) TO  Z06-JITEN3-ITEM(IDX)
               MOVE    IDX                     TO  Z06-JITEN3-COUNT
            END-PERFORM
      *
            MOVE   SPA-GMN-JITEN4-G        TO  Z06-JITEN4
            PERFORM VARYING    IDX     FROM    1   BY  1
                    UNTIL      IDX     >   2
               MOVE    SPA-GMN-JITEN4-LST(IDX) TO  Z06-JITEN4-ITEM(IDX)
               MOVE    IDX                     TO  Z06-JITEN4-COUNT
            END-PERFORM
      *
      *----(04.01.00)--ADD-START-------------------------------------
            MOVE   SPA-GMN-JITEN5-G        TO  Z06-JITEN5
            PERFORM VARYING    IDX     FROM    1   BY  1
                    UNTIL      IDX     >   2
               MOVE    SPA-GMN-JITEN5-LST(IDX) TO  Z06-JITEN5-ITEM(IDX)
               MOVE    IDX                     TO  Z06-JITEN5-COUNT
            END-PERFORM
      *----(04.01.00)--ADD-END---------------------------------------
      *
      *    カラム位置
           MOVE    SPA-GMN-DIGIT (1)   TO  Z06-DIGIT1
           MOVE    SPA-GMN-DIGIT (2)   TO  Z06-DIGIT2
           MOVE    SPA-GMN-DIGIT (3)   TO  Z06-DIGIT3
           MOVE    SPA-GMN-DIGIT (4)   TO  Z06-DIGIT4
           MOVE    SPA-GMN-DIGIT (5)   TO  Z06-DIGIT5
      *
           MOVE    SPA-GMN-COMMENTKBN-G    TO  Z06-COMMENTKBN
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-COMKBN-MAX
               MOVE    SPA-GMN-COMMENTKBN-LST(IDX)
                                          TO  Z06-COMMENTKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-COMKBN-MAX TO  Z06-COMMENTKBN-COUNT
      *v47
      *    複写元コード
           MOVE    SPA-GMN-COPYCD      TO  Z06-COPYCD
      *
           MOVE    "前回複写"          TO  Z06-B06-LABEL
           MOVE    "検索"              TO  Z06-B09-LABEL
      *H29.1
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"  TO  Z06-MIDASHI-DATA
               MOVE    "red"               TO  Z06-MIDASHI-STYLE
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  Z06-COPYCD-STATE
           MOVE    WIDGET-NORMAL       TO  Z06-B06-STATE
           MOVE    WIDGET-NORMAL       TO  Z06-B09-STATE
      *H30.9
      *    新規・改正処理は削除なし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               MOVE    WIDGET-INSENSITIVE  TO  Z06-B03-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  Z06-B03-STATE
           END-IF
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
           IF      SPA-GMN-CUR    =   ZERO
               EVALUATE    WRK-MCP-WIDGET
                   WHEN    "YUKOSTYMD"
                       MOVE    2                 TO  SPA-GMN-CUR
                   WHEN    "YUKOEDYMD"
                       MOVE    3                 TO  SPA-GMN-CUR
                   WHEN    "KANANAME"
                       MOVE    4                 TO  SPA-GMN-CUR
                   WHEN    "NAME"
                       MOVE    5                 TO  SPA-GMN-CUR
                   WHEN    "FORMALNAME"
                       MOVE    11                TO  SPA-GMN-CUR
                   WHEN    "JITEN1"
                       MOVE    7                 TO  SPA-GMN-CUR
                   WHEN    "JITEN2"
                       MOVE    8                 TO  SPA-GMN-CUR
                   WHEN    "JITEN3"
                       MOVE    9                 TO  SPA-GMN-CUR
                   WHEN    "JITEN4"
      ***********      MOVE    10                TO  SPA-GMN-CUR
                       MOVE    12                TO  SPA-GMN-CUR
                   WHEN    "JITEN5"
                       MOVE    6                 TO  SPA-GMN-CUR
      *
                   WHEN    "DIGIT1"
                       MOVE    13                TO  SPA-GMN-CUR
                   WHEN    "DIGIT2"
                       MOVE    14                TO  SPA-GMN-CUR
                   WHEN    "DIGIT3"
                       MOVE    15                TO  SPA-GMN-CUR
                   WHEN    "DIGIT4"
                       MOVE    16                TO  SPA-GMN-CUR
                   WHEN    "DIGIT5"
                       MOVE    17                TO  SPA-GMN-CUR
                   WHEN    "COMMENTKBN"
                       MOVE    99                TO  SPA-GMN-CUR
      *
                   WHEN    OTHER
                       MOVE    1                 TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
           EVALUATE    SPA-GMN-CUR
              WHEN     1
                  MOVE  "YUKOSTYMD"        TO  MCP-WIDGET
              WHEN     2
                  MOVE  "YUKOEDYMD"        TO  MCP-WIDGET
              WHEN     3
                  MOVE  "KANANAME"         TO  MCP-WIDGET
              WHEN     4
                  MOVE  "NAME"             TO  MCP-WIDGET
              WHEN     5
                  MOVE  "FORMALNAME"       TO  MCP-WIDGET
              WHEN     6
                  MOVE  "JITEN1"           TO  MCP-WIDGET
              WHEN     7
                  MOVE  "JITEN2"           TO  MCP-WIDGET
              WHEN     8
                  MOVE  "JITEN3"           TO  MCP-WIDGET
              WHEN     9
                  MOVE  "JITEN4"           TO  MCP-WIDGET
              WHEN     11
                  MOVE  "JITEN5"           TO  MCP-WIDGET
              WHEN     12
                  MOVE  "DIGIT1"            TO  MCP-WIDGET
              WHEN     13
                  MOVE  "DIGIT2"            TO  MCP-WIDGET
              WHEN     14
                  MOVE  "DIGIT3"            TO  MCP-WIDGET
              WHEN     15
                  MOVE  "DIGIT4"            TO  MCP-WIDGET
              WHEN     16
                  MOVE  "DIGIT5"            TO  MCP-WIDGET
              WHEN     17
                  MOVE  "COMMENTKBN"        TO  MCP-WIDGET
              WHEN     18
                  MOVE  "COPYCD"            TO  MCP-WIDGET
      *
              WHEN     99
                  MOVE     "B12"            TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効開始年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効終了年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "カナ名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "漢字名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "正式名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "朝のコードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "昼のコードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "夕のコードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "寝前コードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "起床コードがありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "コメント区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "カラム位置は順番に設定して下さい"
                                                 TO  SPA-ERRMSG
      *v47
               WHEN    "0020"
                   STRING  "複写できるコードではありません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0021"
                   STRING  "複写元コードが登録されていません。"
                                                   DELIMITED BY  SIZE
                                                   INTO    SPA-ERRMSG
                   END-STRING
      *
      *        以下共通
               WHEN    "1010"
                   MOVE
                   "有効開始年月日≧有効終了年月日で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "有効年月日の範囲が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "有効年月日が重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "改正の終了年月日はＡＬＬ９のみ入力できます"
                                                 TO  SPA-ERRMSG
               WHEN    "1014"
                   MOVE    "漢字名称を入力してください"
                                                 TO  SPA-ERRMSG
              WHEN    "1001"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "追加処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "点数マスタ付加情報が更新できません"
                                                 TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD             TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                    TO  ZERR-ERRMSG
      *
           MOVE    "Z06"                         TO  SPA-MOTOPG
           MOVE    "ZERR"                        TO  SPA-SAKIPG
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE
           MOVE    "ZERR"                        TO  MCP-WINDOW
           MOVE    "B01"                         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z06"                TO  SPA-MOTOPG
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
      *
      *****************************************************************
      *    ＤＢ　処理
      *****************************************************************
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .    
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
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
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付変換　処理
      *****************************************************************
      *****************************************************************
      *     対象日＋１日計算処理
      *****************************************************************
       800-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-YMD
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-OTYMD
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-OTYMD
               WHEN    OTHER
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
           END-EVALUATE
           .                                       
      *
           .
       800-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:7)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "9999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
      *
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                       MOVE    ZERO                TO  FLG-ERR
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
           END-EVALUATE
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→和暦（機能２１）
      *****************************************************************
       832-YMDCONV21-SEC                          SECTION.
      *
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO 
               WHEN  "99999999"
                   MOVE    WRK-LNKYMD            TO  WRK-HENYMDG
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY2-AREA
                  MOVE  "21"                     TO  LNK-DAY2-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY2-YMD
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
           END-EVALUATE
           .                                       
      *
       832-YMDCONV21-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→日数１日マイナス（機能６１）
      *****************************************************************
       833-YMDCONV61-SEC                          SECTION.
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY6-AREA
                  MOVE  "61"                     TO  LNK-DAY6-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY6-KIJUN
                  MOVE  "1"                      TO  LNK-DAY6-ZOGENPTN
                  MOVE  -1                       TO  LNK-DAY6-ZOGEN
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN        TO  WRK-SYMD
           END-EVALUATE
           .                                       
      *
       833-YMDCONV61-EXT.
           EXIT.
      *
      *****************************************************************
      *   文字数カウント処理（全角のみ）
      *****************************************************************
       891-STRCOUNT-SEC                         SECTION.
      *
           PERFORM  UNTIL  WRK-STRING(CNT-STRING)  NOT  =  SPACE
                    OR  CNT-STRING  =  1
               COMPUTE  CNT-STRING  =  CNT-STRING  -  1
           END-PERFORM
      *
           IF  CNT-STRING                        =  1
               MOVE  ZERO                        TO  CNT-STRING
           ELSE
               COMPUTE  CNT-STRING               =  CNT-STRING  /  2
           END-IF
           .                                       
       891-STRCOUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
      *
