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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGO05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 請求削除-（Ｏ０５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/01/16    NACL-太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.

       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "O01COMMON-SPA".
      *
      *   
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-O05.
           03  SPA-O05-AREA.
               05  SPA-GMN-MAX                     PIC 9(04).
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(03).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-KBT                 PIC X(22).
                   07  SPA-GMN-KIKAN               PIC X(20).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SRYYM               PIC X(06).
                   07  SPA-NAI-KBT                 PIC X(1).
                   07  SPA-NAI-SKYSTYMD            PIC X(08).
                   07  SPA-NAI-SKYEDYMD            PIC X(08).
                   07  SPA-NAI-GETUMATU            PIC X(1).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-TEIKIRRK                        PIC 9(01).
           03  FLG-HAITA                           PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR        PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDX-CMB             PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-TEIKIERR            PIC X(100).
           03  WRK-MOTOPG              PIC X(64).
           03  WRK-YMD                 PIC X(10).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1         PIC X(10).
               05  WRK-EDTYMD3         PIC X(22).
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM         PIC X(200).
               05  WRK-CMB-ITM2        PIC X(200).
           03  WRK-STDAY               PIC 9(02).
           03  WRK-OIDMSG              PIC X(70).
           03  WRK-NUM-G.
               05  WRK-NUMX            PIC X(01).
               05  WRK-NUMX-R          REDEFINES   WRK-NUMX.
                   07  WRK-NUM9        PIC 9(01).
      *
           03  WRK-HEN-AREA.
               05  WRK-HENYMDG         PIC X(09).
               05  WRK-HENTIME.
                   07  WRK-HH          PIC X(02).
                   07  WRK-DOT1        PIC X(01)   VALUE   ".".
                   07  WRK-MM          PIC X(02).
                   07  WRK-DOT2        PIC X(01)   VALUE   ".".
                   07  WRK-SS          PIC X(02).
      *
       01  KKN-G.
           03  KKN-YMD-G.
               05  KKN-STYMD-W         PIC  X(09).
               05  KKN-EDYMD-W         PIC  X(09).
           03  KKN-OCC           OCCURS   3.
               05  KKN-GETUMATU        PIC X(01).
               05  KKN-SKYSTYMD        PIC X(08).
               05  KKN-SKYEDYMD        PIC X(08).
               05  KKN-KIKAN           PIC X(20).
      *
       01  WRK-SHELL-NAME.
           05  FILLER             PIC X(16) VALUE
                                             "scripts/monthly/".
           05  WRK-SHELL-SHELL    PIC X(20).         
      *
       01  CONST-AREA.
      *    ジョブ管理ＤＢのキー値  
           03  CONST-SHELLID       PIC X(08)   VALUE   "TEIKI3".
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
      *
           03  CONST-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  CONST-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
           03  CONST-KBT-MAX    PIC 9(02)   VALUE   4.
           03  CONST-KBT-VAL.
               05                  PIC X(01)   VALUE   "1".
               05                  PIC X(01)   VALUE   " ".
               05                  PIC X(14)   VALUE   "１期".
               05                  PIC X(01)   VALUE   "2".
               05                  PIC X(01)   VALUE   " ".
               05                  PIC X(14)   VALUE   "２期".
               05                  PIC X(01)   VALUE   "3".
               05                  PIC X(01)   VALUE   " ".
               05                  PIC X(14)   VALUE   "３期".
           03  CONST-KBT-R         REDEFINES   CONST-KBT-VAL.
               05  CONST-KBTLST OCCURS  3.
                   07  CONST-KBTL      PIC  X(01).
                   07  CONST-KBTF1L    PIC  X(01).
                   07  CONST-KBTMEIL   PIC  X(14).
      *
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5010.INC".
      *
      *    患者入院履歴
       01  TEIKIRRK-REC.
           COPY    "CPTEIKIRRK.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    定期請求ジョブチェックサブ
           COPY    "CPORCSTEIKIJOB.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
           COPY    "O01.INC".
           COPY    "O02.INC".
           COPY    "O03.INC".
           COPY    "O04.INC".
           COPY    "O05.INC".
           COPY    "O99.INC".
           COPY    "OERR.INC".
           COPY    "OID1.INC".
           COPY    "OID2.INC".
           COPY    "OID3.INC".
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
           MOVE    SPA-FREE        TO  SPA-O05
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *    画面遷移
           WHEN     "PUTG"         ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN      OTHER
               PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF    ( FLG-END         =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-O01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-O05         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF    ( SPA-MOTOPG          =   "OERR" )
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF    ( SPA-ERRCD   NOT =   SPACE )
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "O05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU  TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        IF    ( WRK-MOTOPG(1:3)     =  "???" )
      *            MOVE    LNK-COMMON      TO  SPA-KYOUTU
      *            MOVE    LNK-SCRSPA      TO  SPA-O05
      *            MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      *            PERFORM 3101-U01SPASET-SEC
      *            GO      TO      300-SCREEN-EXT
      *        END-IF
      *
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
           WHEN    "OID1"
               PERFORM 330-OID1-SET-SEC
           WHEN    "O99"
               MOVE    SPACE           TO  SPA-OID1-FLG
                                           SPA-O99-FLG
                                           SPA-OIDCD
                                           SPA-OIDCD2
           WHEN    OTHER
               PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
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
           INITIALIZE                  SPA-O05-AREA
      *
           PERFORM 900-TEIKIRRK-KEY2-SEL-SEC
           IF    ( FLG-TEIKIRRK    =   ZERO )
               MOVE    TEIKIRRK-SRYYM  TO  WRK-YMD
               MOVE    "01"            TO  WRK-YMD (7:)
               PERFORM 800-WAREKI-HEN-SEC
               MOVE    WRK-EDTYMD3(1:16)   TO  SPA-GMN-KBT (1:16)
               MOVE    "　"                TO  SPA-GMN-KBT (17:2)
               MOVE    TEIKIRRK-KBT    TO  WRK-CMB-CD
               PERFORM 4200-KBTLST-CHK-SEC
               MOVE    WRK-CMB-ITM         TO  SPA-GMN-KBT (19:)
      *
               MOVE    TEIKIRRK-SRYYM  TO  SPA-NAI-SRYYM
               MOVE    TEIKIRRK-KBT    TO  SPA-NAI-KBT
      *
               PERFORM 3101-TEIKI-KIKAN-HEN-SEC
           END-IF
      *
           MOVE    212                 TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求期間編集処理
      *****************************************************************
       3101-TEIKI-KIKAN-HEN-SEC        SECTION.
      *
           INITIALIZE                      SYS-5010-REC
           MOVE    "5010"              TO  SYS-5010-KANRICD
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    SPA-NAI-SRYYM       TO  SYS-5010-STYUKYMD
           MOVE    "01"                TO  SYS-5010-STYUKYMD (7 : 2)
           MOVE    SYS-5010-STYUKYMD   TO  SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5010-REC
           ELSE
               INITIALIZE                  SYS-5010-REC
           END-IF
      *
           MOVE    1                   TO  WRK-STDAY
      *
           INITIALIZE                      KKN-G
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   3 )
                    OR   ( SYS-5010-SEIKYU-DAY (IDX1)  =   ZERO )
      *
               INITIALIZE                  KKN-YMD-G
      *
               MOVE    SPA-NAI-SRYYM   TO  WRK-YMD
               MOVE    WRK-STDAY       TO  WRK-YMD (7 : 2)
               PERFORM 800-WAREKI-HEN-SEC
               MOVE    WRK-EDTYMD1     TO  KKN-STYMD-W
               MOVE    WRK-YMD         TO  KKN-SKYSTYMD (IDX1)
      *
               IF    ( SYS-5010-SEIKYU-DAY (IDX1)  =   99 )
                   INITIALIZE              STS-AREA-DAY
                   INITIALIZE              LNK-DAY1-AREA
                   MOVE    "71"            TO   LNK-DAY7-IRAI
                   MOVE    SPA-NAI-SRYYM   TO   LNK-DAY7-YM
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY7-AREA
                   MOVE    LNK-DAY7-KOYOMI TO  WRK-YMD
                   PERFORM 800-WAREKI-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  KKN-EDYMD-W
                   MOVE    WRK-YMD         TO  KKN-SKYEDYMD (IDX1)
      *
                   MOVE    "1"             TO  KKN-GETUMATU (IDX1)
      *
               ELSE
                   MOVE    SPA-NAI-SRYYM   TO  WRK-YMD
                   MOVE    SYS-5010-SEIKYU-DAY (IDX1)
                                           TO  WRK-YMD (7 : 2)
                   PERFORM 800-WAREKI-HEN-SEC
                   MOVE    WRK-EDTYMD1     TO  KKN-EDYMD-W
                   MOVE    WRK-YMD         TO  KKN-SKYEDYMD (IDX1)
      *
                   COMPUTE WRK-STDAY   =   SYS-5010-SEIKYU-DAY (IDX1)
                                       +   1
      *
               END-IF
      *
               STRING  KKN-STYMD-W DELIMITED   BY  SIZE
                       "〜"        DELIMITED   BY  SIZE
                       KKN-EDYMD-W DELIMITED   BY  SIZE
               INTO    KKN-KIKAN (IDX1)
               END-STRING
      *
           END-PERFORM
      *
           IF    ( SPA-NAI-KBT     =   "1" OR "2" OR "3" )
               MOVE    SPA-NAI-KBT         TO  WRK-NUMX
               MOVE    KKN-KIKAN    (WRK-NUM9)
                                           TO  SPA-GMN-KIKAN
               MOVE    KKN-SKYSTYMD (WRK-NUM9)
                                           TO  SPA-NAI-SKYSTYMD
               MOVE    KKN-SKYEDYMD (WRK-NUM9)
                                           TO  SPA-NAI-SKYEDYMD
               MOVE    KKN-GETUMATU (WRK-NUM9)
                                           TO  SPA-NAI-GETUMATU
           END-IF
      *
           .
       3101-TEIKI-KIKAN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-OID1-SET-SEC            SECTION.
      *
           IF    ( SPA-OID1-FLG    =   "OK" )
               EVALUATE    SPA-OIDCD
               WHEN    "6666"
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               WHEN    "1001"
                   PERFORM 4501-JOBKANRICHK-SEC
                   IF    ( SPA-ERRCD       =   SPACE )
                       PERFORM 450-KOUSHIN-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  SPA-OID1-FLG
                                           SPA-O99-FLG
                                           SPA-OIDCD
                                           SPA-OIDCD2
           .
       330-OID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO  MCP-PUTTYPE
               PERFORM 210-BACK
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 490-JYOUHOU-DEL-SEC
      *    登録
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-DELETE-SEC
      *    処理確認
           WHEN    "CLICKED"       ALSO    "B13"
               PERFORM 490-KAKUNIN-SEC
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
      *
      *    EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
      *    WHEN    "ACTIVATE"      ALSO    "XXXXXX"
      *    WHEN        OTHER
      *
               MOVE    ZERO        TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
      *    END-EVALUATE
      *
           .
       200-ENTRY-EXT.
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
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    期別リストチェック処理
      *****************************************************************
       4200-KBTLST-CHK-SEC             SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM-G
           PERFORM VARYING IDX-CMB     FROM    1   BY  1
                   UNTIL ( IDX-CMB         >   3 )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( CONST-KBTL (IDX-CMB)    =   WRK-CMB-CD )
                   MOVE    CONST-KBTMEIL (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4200-KBTLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           PERFORM 800-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDYMD      =   SPACE )
                   MOVE    "6666"          TO  SPA-OIDCD
                   GO  TO  210-BACK-EXT
               END-IF
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    "O01"               TO  SPA-SAKIPG
           MOVE    "O05"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       450-DELETE-SEC                  SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-DELETE-EXT
           END-IF
      *
      *    ジョブ管理チェック
           PERFORM 4501-JOBKANRICHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-DELETE-EXT
           END-IF
      *
           MOVE    "1001"             TO  SPA-OIDCD
      *
           .
       450-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       450-KOUSHIN-SEC                 SECTION.
      *
      *    ジョブ開始セット
           MOVE    "DEL"               TO  SJOBKANRI-MODE
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           PERFORM 800-ORCSJOB-SEC
      *
           MOVE    SPA-HOSPNUM         TO  TEIKIERR-HOSPNUM
           MOVE    "OR033ERR"          TO  TEIKIERR-FILE-ID
           MOVE    SPA-TERMID          TO  TEIKIERR-TERMID
           MOVE    5                   TO  TEIKIERR-SYOKBN1
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "teiki3.sh"         TO  SHELLTBL-NAME
      *
           MOVE    TEIKIERR            TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG2
           MOVE    SPA-TERMID          TO  SHELLTBL-ARG3
           MOVE    SPA-OPID            TO  SHELLTBL-ARG4
           MOVE    SPA-SYSYMD          TO  SHELLTBL-ARG5
           MOVE    CONST-JOBID         TO  SHELLTBL-ARG6
           MOVE    CONST-SHELLID       TO  SHELLTBL-ARG7
           MOVE    SPA-NAI-SRYYM       TO  SHELLTBL-ARG8
           MOVE    SPA-NAI-KBT         TO  SHELLTBL-ARG9
           MOVE    SPA-NAI-SKYSTYMD    TO  SHELLTBL-ARG10
           MOVE    SPA-NAI-SKYEDYMD    TO  SHELLTBL-ARG11
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "monthly"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
      *    自動的に情報確認画面を表示
           PERFORM 490-KAKUNIN-SEC
           .
       450-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRICHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-TEIKIERR
           PERFORM 45011-TEIKI-CHK-SEC
      *
           IF    ( WRK-TEIKIERR    =   SPACE )
               PERFORM 45011-RECEPT1-CHK-SEC
           END-IF
      *
           .
       4501-JOBKANRICHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求ジョブチェック処理
      *****************************************************************
       45011-TEIKI-CHK-SEC             SECTION.
      *
           INITIALIZE                  STEIKIJOB-AREA
           CALL    "ORCSTEIKIJOB"      USING   STEIKIJOB-AREA
                                               SPA-AREA
           IF    ( STEIKIJOB-ERRCD NOT =   SPACE )
               MOVE    STEIKIJOB-ERRCD     TO  SPA-ERRCD
               MOVE    STEIKIJOB-ERRMSG    TO  WRK-TEIKIERR
           END-IF
      *
           .
       45011-TEIKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院レセプトジョブチェック処理
      *****************************************************************
       45011-RECEPT1-CHK-SEC           SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM             TO  JOB-HOSPNUM
           MOVE    CONST-JOBID-RECEPT1     TO  JOB-JOBID
           MOVE    CONST-SHELLID-RECEPT1   TO  JOB-SHELLID
           MOVE    SPA-TERMID          TO  JOB-TERMID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC 
                                       SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF   (  JOB-ENDYMD      =   SPACE )
      *
                   MOVE    1           TO  FLG-HAITA
      *
                   IF    ( JOB-NYUGAIKBN   =   "2" )
                       MOVE    ZERO    TO  FLG-HAITA
                   ELSE
                       IF    ( JOB-SYOKBN      =   "1" )
                        AND  ( JOB-SRYYM   NOT =   SPA-NAI-SRYYM )
                           MOVE    ZERO    TO  FLG-HAITA
                       END-IF
                   END-IF
      *
                   IF    ( FLG-HAITA   =   1 )
                       MOVE    "7777"      TO  SPA-ERRCD
                       MOVE    SPACE       TO  WRK-TEIKIERR
                       STRING  "入院レセプト処理中です【"
                                           DELIMITED  BY  SIZE
                              JOB-SHELLMSG DELIMITED  BY  SPACE
                              "】"         DELIMITED  BY  SIZE
                       INTO  WRK-TEIKIERR
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
           .
       45011-RECEPT1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    "O05"           TO  SPA-MOTOPG
           MOVE    SPA-O01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           MOVE    "LINK"          TO  MCP-STATUS
           MOVE    SPACE           TO  MCP-EVENT
      *
           CALL    "ORCGO99"       USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      *
      *
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "O05"           TO  SPA-MOTOPG
           MOVE    "O99"           TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "O99"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-O05             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "O05"               TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-SHELLID       TO  SPA-JOB-SHELLID
      *          
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF    ( SPA-ERRCD       NOT =   SPACE )
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF    ( SPA-OIDCD       NOT =   SPACE )
               PERFORM 520-OIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "O05    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  O05
      *
           INITIALIZE                      O05
           MOVE    SPA-GMN-KBT         TO  O05-KBT
           MOVE    SPA-GMN-KIKAN       TO  O05-KIKAN
      *
           MOVE    WIDGET-NORMAL       TO  O05-B12-STATE
           MOVE    "black"             TO  O05-LBLMSG-STYLE
      *
           EVALUATE    TRUE
           WHEN  ( SPA-NAI-SRYYM   =   SPACE )
               MOVE    "red"           TO  O05-LBLMSG-STYLE
               MOVE    "削除対象の期間はありません"
                                       TO  O05-LBLMSG
               MOVE    WIDGET-INSENSITIVE
                                       TO  O05-B12-STATE
           WHEN  ( SPA-NAI-GETUMATU    =   "1" )
               MOVE    "red"           TO  O05-LBLMSG-STYLE
               STRING  "月末を含む場合、"  DELIMITED   BY  SIZE
                       "月末一括請求分も削除されます"
                                           DELIMITED   BY  SIZE
               INTO    O05-LBLMSG
               END-STRING
           END-EVALUATE
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
           IF    ( SPA-ERRCD       =   SPACE)
            AND  ( SPA-GMN-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    212
               MOVE    "B12"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC          SECTION.
      *
      *    EVALUATE    WRK-MCP-WIDGET
      *    WHEN    "XXXX"
      *        MOVE    XXX         TO  SPA-GMN-CUR
      *    END-EVALUATE
      *
           MOVE    212             TO  SPA-GMN-CUR
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
           WHEN    "7777"
               MOVE    WRK-TEIKIERR     TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  OERR
           MOVE    SPA-ERRCD            TO  OERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  OERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "O05"                TO  SPA-MOTOPG
           MOVE    "OERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "OERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-OIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-OIDMSG
      *
           EVALUATE    SPA-OIDCD
           WHEN    "1001"
               STRING  SPA-GMN-KBT     DELIMITED   BY  SIZE
                       "の請求データを削除します"
                                       DELIMITED   BY  SIZE
               INTO    WRK-OIDMSG
               END-STRING
           WHEN    "6666"
               MOVE    "定期請求作成処理中です"
                                        TO  WRK-OIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  OID1
           INITIALIZE                       OID1
           MOVE    SPA-OIDCD            TO  OID1-ID1CODE
           MOVE    WRK-OIDMSG           TO  OID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "O05"                TO  SPA-MOTOPG
           MOVE    "OID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "OID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-OIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理サブ
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CONST-JOBID         TO  JOB-JOBID
           MOVE    CONST-SHELLID       TO  JOB-SHELLID
           MOVE    SPA-TERMID          TO  JOB-TERMID
           IF    ( SJOBKANRI-MODE      =   "JBS" )
               MOVE    "1"             TO  JOB-SYOKBN
               MOVE    SPA-NAI-SRYYM   TO  JOB-SRYYM
               MOVE    "1"             TO  JOB-NYUGAIKBN
           END-IF
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA 
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    和暦変換処理
      *****************************************************************
       800-WAREKI-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-EDTYMD-G.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
      *
      *
           .
       800-WAREKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求履歴検索（KEY2）
      *****************************************************************
       900-TEIKIRRK-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TEIKIRRK
           INITIALIZE                      TEIKIRRK-REC
      *
           MOVE    SPA-HOSPNUM         TO  TEIKIRRK-HOSPNUM
           MOVE    TEIKIRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  TEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-TEIKIRRK
               MOVE    MCPDATA-REC     TO  TEIKIRRK-REC
           END-IF
      *
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-TEIKIRRK-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理（KEY10）
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル読込処理
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ追加処理
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ削除処理
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBDELETE-EXT.
           EXIT.
      *
