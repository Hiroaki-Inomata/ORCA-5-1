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
       PROGRAM-ID.                 ORCGW62.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : システム管理情報設定
      *  コンポーネント名  : 帳票テスト印刷画面
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/04/01    NACL−森脇    新規作成
      *****************************************************************
      *  プログラム修正履歴
      *  Maj/Min/Rev 修正者        日付      内容
      *  00.00.01    NACL-森脇     06.08.02  労災、自賠のプリンタ追加
      *  03.05.00    NACL-多々納  07/05/02  グループ診療対応
      *  04.08.01    NACL-森脇     16/09/06  GINBEE対応
      *  05.00.00    NACL-太田     17/11/29  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "W01COMMON-SPA".
      *    画面用ＳＰＡ
           COPY    "W11SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WIDGET          PIC X(64).
           03  WRK-WIDMSG          PIC X(70).
      *
           03  WRK-PRTNUM          PIC X(17).
           03  WRK-PRTNAME-G.
               05  WRK-PRTNAME     PIC X(16)   OCCURS  16.
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-FILENAME   PIC X(20)   VALUE
                                   "data/test.pdf".
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "101".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PREVIEW-TBL-KEY
                                   PIC X(08)   VALUE   "TSTPRINT".
           03  WRK-CONS-PREVIEW-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "TSTPRINT".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY  "CPSYSKANRI.INC".
      *    外来プリンタ情報
           COPY  "CPSK1031.INC".
      *    入院プリンタ情報
           COPY  "CPSK1034.INC".
      *    総括、レセプリンタ情報
           COPY  "CPSK2005.INC".
      *    ジョブ管理情報
          COPY    "CPSK3003.INC".
      *
      *01  PRVKANRI-REC.
           COPY    "CPPRVKANRI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSPRTLNK.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
           COPY    "CPSHELLTBL.INC".
      *
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    COPY    "ORCA-DBPATH".
           COPY    "ORCA-BLOB".
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
           COPY    "ORCA101SCRAREA.INC".
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
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W11
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
           MOVE    SPA-W11         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
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
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE           TO  SPA-MOTOPG
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
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
               MOVE   "NEW"            TO  MCP-PUTTYPE
               MOVE   "W62"            TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-W62-AREA
      *
      *    ファイル位置決定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-CONS-FILENAME
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
      *
      *    ファイル存在チェック
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    MKPASS-OT-01    TO  FILECHK-NAME
           CALL    "ORCSFILECHK"       USING
                                       ORCSFILECHKAREA
           IF          FILECHK-RC      =   0
               MOVE    "BLOBIMPORT"    TO  MCP-FUNC
               MOVE    "blob"          TO  MCP-TABLE
               MOVE    "primary"       TO  MCP-PATHNAME
               MOVE    MKPASS-OT-01    TO  ORCA-BLOB-FILE
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   ORCA-BLOB
                                                   SPA-AREA
               MOVE    ORCA-BLOB-OBJECT    TO  SPA-W62-OBJECT
           END-IF
      *
           MOVE    ZERO            TO  SPA-W62-PRTNUM-MAX
      *
      *    外来プリンタセット
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1031"              TO  SYS-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-SELECT-SEC
           PERFORM UNTIL   FLG-SYSKANRI    NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1031-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       20
                   MOVE    ZERO            TO  FLG-OK
                   IF      SYS-1031-PRTNM(IDX)     =   SPACE
                       MOVE    1           TO  FLG-OK
                   END-IF
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       20
                           OR      FLG-OK  =   1
                       IF      SPA-W62-PRTNUML(IDY)
                                             =   SYS-1031-PRTNM(IDX)
                           MOVE    1           TO  FLG-OK
                       ELSE
                           IF      SPA-W62-PRTNUML(IDY)    =   SPACE
                               MOVE     IDY    TO  SPA-W62-PRTKBNL(IDY)
                               MOVE     SYS-1031-PRTNM(IDX)
                                               TO  SPA-W62-PRTNUML(IDY)
                               ADD     1       TO  SPA-W62-PRTNUM-MAX
                               MOVE    1       TO  FLG-OK
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
      *    総括、レセプリンタセット
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2005"              TO  SYS-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-SELECT-SEC
           PERFORM UNTIL   FLG-SYSKANRI    NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2005-REC
               MOVE    SPACE           TO  WRK-PRTNAME-G
               MOVE    SYS-2005-SYAPRTNM           TO  WRK-PRTNAME(1)
               MOVE    SYS-2005-KKHPRTNM           TO  WRK-PRTNAME(2)
               MOVE    SYS-2005-SYAPRTNM-NEXT      TO  WRK-PRTNAME(3)
               MOVE    SYS-2005-KKHPRTNM-NEXT      TO  WRK-PRTNAME(4)
               MOVE    SYS-2005-SYAPRTNM-N         TO  WRK-PRTNAME(5)
               MOVE    SYS-2005-KKHPRTNM-N         TO  WRK-PRTNAME(6)
               MOVE    SYS-2005-SYAPRTNM-NEXT-N    TO  WRK-PRTNAME(7)
               MOVE    SYS-2005-KKHPRTNM-NEXT-N    TO  WRK-PRTNAME(8)
               MOVE    SYS-2005-ROUPRTNM           TO  WRK-PRTNAME(9)
               MOVE    SYS-2005-ROUPRTNM-NEXT      TO  WRK-PRTNAME(10)
               MOVE    SYS-2005-ROUPRTNM-N         TO  WRK-PRTNAME(11)
               MOVE    SYS-2005-ROUPRTNM-NEXT-N    TO  WRK-PRTNAME(12)
               MOVE    SYS-2005-JBAIPRTNM          TO  WRK-PRTNAME(13)
               MOVE    SYS-2005-JBAIPRTNM-NEXT     TO  WRK-PRTNAME(14)
               MOVE    SYS-2005-JBAIPRTNM-N        TO  WRK-PRTNAME(15)
               MOVE    SYS-2005-JBAIPRTNM-NEXT-N   TO  WRK-PRTNAME(16)
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       16
                   MOVE    ZERO            TO  FLG-OK
                   IF      WRK-PRTNAME(IDX)    =   SPACE
                       MOVE    1           TO  FLG-OK
                   END-IF
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       20
                           OR      FLG-OK  =   1
                       IF      SPA-W62-PRTNUML(IDY)
                                             =   WRK-PRTNAME(IDX)
                           MOVE    1           TO  FLG-OK
                       ELSE
                           IF      SPA-W62-PRTNUML(IDY)    =   SPACE
                               MOVE     IDY    TO  SPA-W62-PRTKBNL(IDY)
                               MOVE     WRK-PRTNAME(IDX)
                                               TO  SPA-W62-PRTNUML(IDY)
                               ADD     1       TO  SPA-W62-PRTNUM-MAX
                               MOVE    1       TO  FLG-OK
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
           IF    ( SPA-BEDSU           >   ZERO )
               CONTINUE
           ELSE
               GO  TO  310-SPASET-EXT
           END-IF
      *
      *    入院プリンタセット
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1034"              TO  SYS-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-SELECT-SEC
           PERFORM UNTIL FLG-SYSKANRI  NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1034-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       20
                   MOVE    ZERO            TO  FLG-OK
                   IF      SYS-1034-PRTNM(IDX)     =   SPACE
                       MOVE    1           TO  FLG-OK
                   END-IF
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       20
                           OR      FLG-OK  =   1
                       IF      SPA-W62-PRTNUML(IDY)
                                               =   SYS-1034-PRTNM(IDX)
                           MOVE    1           TO  FLG-OK
                       ELSE
                           IF      SPA-W62-PRTNUML(IDY)    =   SPACE
                               MOVE     IDY    TO  SPA-W62-PRTKBNL(IDY)
                               MOVE     SYS-1034-PRTNM(IDX)
                                               TO  SPA-W62-PRTNUML(IDY)
                               ADD     1       TO  SPA-W62-PRTNUM-MAX
                               MOVE    1       TO  FLG-OK
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
           MOVE    1               TO  SPA-W62-CUR
      *
          .
       310-SPASET-EXT.
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
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "W62    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W62
           INITIALIZE                  W62
      *
      *----(04.08.01)--ADD-START---
      *    ファイル位置決定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-CONS-FILENAME
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
      *
      *    ファイル存在チェック
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    MKPASS-OT-01    TO  FILECHK-NAME
           CALL    "ORCSFILECHK"       USING
                                       ORCSFILECHKAREA
           IF          FILECHK-RC      =   0
               MOVE    "BLOBIMPORT"    TO  MCP-FUNC
               MOVE    "blob"          TO  MCP-TABLE
               MOVE    "primary"       TO  MCP-PATHNAME
               MOVE    MKPASS-OT-01    TO  ORCA-BLOB-FILE
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   ORCA-BLOB
                                                   SPA-AREA
               MOVE    ORCA-BLOB-OBJECT    TO  SPA-W62-OBJECT
           END-IF
      *----(04.08.01)--ADD-END-----
      *
           MOVE    SPA-W62-OBJECT  TO  W62-PSAREA
           MOVE    SPA-W62-PRTNUM-G    TO  W62-PRTNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-W62-PRTNUM-MAX
               MOVE    SPA-W62-PRTNUMLST(IDX)
                                       TO  W62-PRTNUMLST(IDX)
           END-PERFORM
           MOVE    SPA-W62-PRTNUM-MAX  TO  W62-PRTNUM-COUNT
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
           IF      SPA-W62-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-W62-CUR
               WHEN    1
                   MOVE    "PRTNUM"    TO   MCP-WIDGET
               WHEN    99
                   MOVE    "B12"       TO   MCP-WIDGET
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
           EVALUATE    WRK-WIDGET
               WHEN    "PRTNUM"
                   MOVE    99          TO  SPA-W62-CUR
               WHEN    "B12"
                   MOVE    1           TO  SPA-W62-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
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
      *        印刷
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 220-KAKUNIN-SEC
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
      *    プリンタセット
           PERFORM 2001-PRTNUM-SYORI-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    プリンタ名入力処理
      *****************************************************************
       2001-PRTNUM-SYORI-SEC       SECTION.
      *
           MOVE    W62-PRTNUM      TO  SPA-W62-PRTNUM-G
      *
           MOVE    ZERO            TO  FLG-OK
           MOVE    SPACE           TO  WRK-PRTNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-W62-PRTNUM-MAX
               IF      SPA-W62-PRTNUM  =   SPA-W62-PRTNUML(IDX)
                   MOVE    SPA-W62-PRTNUMLST(IDX)
                                           TO  SPA-W62-PRTNUM-G
                   MOVE    SPA-W62-PRTNUM-MAX  TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    SPA-W62-PRTNUM-G    TO  WRK-PRTNUM
           ELSE
               MOVE    SPA-W62-PRTNUM      TO  WRK-PRTNUM
           END-IF
      *
           IF      WRK-PRTNUM          =   SPACE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-W62-CUR
           ELSE
               MOVE    99              TO  SPA-W62-CUR
           END-IF
      *
           .
       2001-PRTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W11"           TO  SPA-SAKIPG
           MOVE    "W62"           TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    "W11"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認　処理
      *****************************************************************
       220-KAKUNIN-SEC             SECTION.
      *
           PERFORM 2001-PRTNUM-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  220-KAKUNIN-EXT
           END-IF
      *
           PERFORM 230-PRINT-SEC
      *
           .
       220-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷　処理
      *****************************************************************
       230-PRINT-SEC               SECTION.
      *
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                                       SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               MOVE    SYS-3003-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "1005"          TO  SPA-ERRCD
                   GO  TO  230-PRINT-EXT
               END-IF
           END-IF
      *
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"          TO  SPA-ERRCD
               GO  TO  230-PRINT-EXT
           END-IF
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM          TO  WRK-SYSMM
           MOVE    SYS-DD          TO  WRK-SYSDD
           ACCEPT  SYS-TIME        FROM    TIME
      *
           MOVE    ZERO            TO  WRK-CONS-JOB-JOBID
           MOVE    SPACE           TO  WRK-CONS-PREVIEW-TBL-GROUP
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
      *
           MOVE    "preprint.sh"   TO  SHELLTBL-NAME
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    WRK-CONS-PREVIEW-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    WRK-CONS-PREVIEW-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO            TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "preprint.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    WRK-PRTNUM      TO  LNK-PRTKANRI-PRTNM
      *
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
           MOVE    SPACE           TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG14
      *
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-CONS-FILENAME
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-ARG15
           MOVE    WRK-PRTNUM      TO  SHELLTBL-ARG16
      *
           IF    SPA-CLIENT-PRT-FLG  =  2
               MOVE    "1"         TO  SHELLTBL-ARG17
           END-IF
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    ＪＯＢＩＤ更新処理
           PERFORM 45012-JOBID-UPD-SEC
      *
           .
       230-PRINT-EXT.
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
                   MOVE    "プリンタ名を入力して下さい。"
                                           TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                           TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W62"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "WERR"          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC   SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC         SECTION.
      *
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                                       SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    ZERO            TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"         TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PREVIEW-TBL-KEY
                                   TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PREVIEW-TBL-GROUP
                                   TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SYS-3003-JOBID
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ検索処理
      *****************************************************************
       900-SYSKANRI-SELECT-SEC     SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          =   ZERO
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタクローズ処理
      *****************************************************************
       900-SYSKANRI-CLOSE-SEC      SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
