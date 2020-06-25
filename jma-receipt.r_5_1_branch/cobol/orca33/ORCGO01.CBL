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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGO01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院業務
      *  コンポーネント名  : 定期請求（Ｏ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/08/21    ひさなが　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    02/12/05  処理結果確認画面への
      *                                     遷移追加等
      *  01.00.02    NACL-太田    02/02/17  期毎の指定を可能にする
      *  01.01.03    NACL-太田    03/11/12  更新日付編集対応
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "O01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
       01  SPA-O01.
           03  SPA-O01-AREA.
               05  SPA-GMN-MAX                     PIC 9(04).
               05  SPA-GMN-PAGE                    PIC 9(02).
               05  SPA-GMN-CUR                     PIC 9(02).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-PRTKBN-G.
                       09  SPA-GMN-PRTKBN          PIC X(01).
                       09  SPA-GMN-PRTKBNFL        PIC X(01).
                       09  SPA-GMN-PRTKBNMEI       PIC X(18).
                   07  SPA-GMN-SRYYM               PIC X(07).
                   07  SPA-GMN-DENPPRTYMD          PIC X(10).
                   07  SPA-GMN-IKKATU              PIC X(01).
                   07  SPA-GMN-KOBETU              PIC X(01).
                   07  SPA-GMN-CHK-KIKAN-VAL.
                       09  SPA-GMN-CHK-KIKAN01     PIC X(01).
                       09  SPA-GMN-CHK-KIKAN02     PIC X(01).
                       09  SPA-GMN-CHK-KIKAN03     PIC X(01).
                   07  SPA-GMN-CHK-KIKAN-R REDEFINES
                       SPA-GMN-CHK-KIKAN-VAL.
                       09  SPA-GMN-CHK-KIKAN-OCC   OCCURS  3.
                           11  SPA-GMN-CHK-KIKAN   PIC X(01).
                   07  SPA-GMN-KIKAN-VAL.
                       09  SPA-GMN-KIKAN01         PIC X(20).
                       09  SPA-GMN-KIKAN02         PIC X(20).
                       09  SPA-GMN-KIKAN03         PIC X(20).
                   07  SPA-GMN-KIKAN-R     REDEFINES
                       SPA-GMN-KIKAN-VAL.
                       09  SPA-GMN-KIKAN-OCC       OCCURS  3.
                           11  SPA-GMN-KIKAN       PIC X(20).
                   07  SPA-GMN-LBLINFO01           PIC X(80).
      *
               05  SPA-GMN-COMBO-AREA.
                   07  SPA-GMN-PRTKBNLST-G.
                       09  SPA-GMN-PRTKBN-MAX       PIC 9(05).
                       09  SPA-GMN-PRTKBNLST        OCCURS   5.
                           11  SPA-GMN-PRTKBNL      PIC X(01).
                           11  SPA-GMN-PRTKBNFLL    PIC X(01).
                           11  SPA-GMN-PRTKBNMEIL   PIC X(18).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-DELKBN              PIC X(01).
               05  SPA-NAI-O01-AREA.
                   07  SPA-NAI-SRYYM               PIC 9(06).
                   07  SPA-NAI-DENPPRTYMD          PIC X(08).
                   07  SPA-NAI-KIKAN-VAL.
                       09  SPA-NAI-KIKAN01ST       PIC X(08).
                       09  SPA-NAI-KIKAN01ED       PIC X(08).
                       09  SPA-NAI-KIKAN02ST       PIC X(08).
                       09  SPA-NAI-KIKAN02ED       PIC X(08).
                       09  SPA-NAI-KIKAN03ST       PIC X(08).
                       09  SPA-NAI-KIKAN03ED       PIC X(08).
                   07  SPA-NAI-KIKAN-R   REDEFINES
                       SPA-NAI-KIKAN-VAL.
                       09  SPA-NAI-KIKAN-OCC       OCCURS 3.
                           11  SPA-NAI-KIKANST     PIC X(08).
                           11  SPA-NAI-KIKANED     PIC X(08).
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
       01  STS-AREA.
           03  STS-TEIKIERR            PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-QTEIKI              PIC 9(01).
           03  FLG-TEIKIRRK            PIC 9(01).
           03  FLG-HAITA               PIC 9(01).
           03  FLG-STOP                PIC 9(01).
           03  FLG-RUN                 PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                 PIC 9(06).
           03  CNT-UPDCNT              PIC 9(06).
           03  CNT-SELKBT              PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
           03  IDX4                    PIC 9(05).
           03  IDX5                    PIC 9(05).
           03  IDX-CHK                 PIC 9(05).
           03  IDX-CMB                 PIC 9(05).
           03  IDX-SELKBT              PIC 9(05).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KBT-G.
               05  WRK-KBT9            PIC 9(01).
               05  WRK-KBTR   REDEFINES    WRK-KBT9.
                   07  WRK-KBTX        PIC X(01).
           03  WRK-JOBID               PIC 9(07).
           03  WRK-TEIKIERR            PIC X(200).
           03  WRK-SRYYM               PIC X(06).
           03  WRK-SRYYM-MOT           PIC X(06).
           03  WRK-DENPPRTYMD-MOT      PIC X(10).
           03  WRK-CREYMD              PIC X(08).
           03  WRK-OIDMSG              PIC X(70).
           03  WRK-YM                  PIC X(07). 
           03  WRK-SYMD                PIC X(10).
           03  WRK-SHELL-ARG1.
             05  WRK-SHELL-SRYYM       PIC  X(06).
             05  WRK-SHELL-TERMID      PIC  X(64).
             05  WRK-SHELL-SYSYMD      PIC  X(08).
           03  WRK-SHELL-KIKAN-TBL.
             05  WRK-SHELL-KIKAN-OCC   OCCURS  3.
               07  WRK-SHELL-KIKAN-ST  PIC  X(08).
               07  WRK-SHELL-KIKAN-ED  PIC  X(08).
               07  WRK-SHELL-KIKAN-KBN PIC  X(01).
           03  WRK-KIKAN-YMD.
               05  WRK-KIKAN-STYMD-S   PIC  X(08).
               05  WRK-KIKAN-EDYMD-S   PIC  X(08).
               05  WRK-KIKAN-STYMD-W   PIC  X(09).
               05  WRK-KIKAN-EDYMD-W   PIC  X(09).
           03  WRK-STDAY               PIC  9(02).
           03  WRK-MCP-WIDGET          PIC X(64).
           03  WRK-SRYYMH              PIC X(16).
           03  WRK-HEN-AREA.
               05  WRK-HENYMDG         PIC X(09).
               05  WRK-HENYMDG3        PIC X(22).
               05  WRK-HENTIME.
                   07  WRK-HH          PIC X(02).
                   07  WRK-DOT1        PIC X(01)   VALUE   ".".
                   07  WRK-MM          PIC X(02).
                   07  WRK-DOT2        PIC X(01)   VALUE   ".".
                   07  WRK-SS          PIC X(02).
           03  WRK-JOBYMD              PIC X(08).
           03  WRK-JOBHMS              PIC X(06).
           03  WRK-SELKBT-G.
               05  WRK-SELKBT          PIC X(01)
                                       OCCURS  3.
           03  WRK-CMB-CD              PIC X(10).
           03  WRK-CMB-ITM-G.
               05  WRK-CMB-ITM         PIC X(200).
           03  WRK-PNT                 PIC 9(03).
           03  WRK-LSTKBT              PIC X(06).
           03  WRK-MOTOPG              PIC X(08).
      *
       01  WRK-SHELL-NAME.
           05  FILLER                  PIC X(16) VALUE
                                             "scripts/monthly/".
           05  WRK-SHELL-SHELL         PIC X(20).         
      *              
      *
       01  CONST-AREA.
           03  CONST-KBT-G.
               05  CONST-KBT-VAL.
                   07                      PIC X(01)   VALUE   "1".
                   07                      PIC X(04)   VALUE   "１期".
                   07                      PIC X(01)   VALUE   "2".
                   07                      PIC X(04)   VALUE   "２期".
                   07                      PIC X(01)   VALUE   "3".
                   07                      PIC X(04)   VALUE   "３期".
               05  CONST-KBT-R         REDEFINES   CONST-KBT-VAL.
                   07    CONST-KBT-OCC     OCCURS  3.
                       09    CONST-KBT     PIC X(01).
                       09    CONST-KBTMSG  PIC X(04).
           03  CONST-SHELLID           PIC X(08)   VALUE   "TEIKI1".
      *
           03  CONST-SHELLID-RECEPT1
                                       PIC X(08)   VALUE   "RECEPT1".
           03  CONST-JOBID-RECEPT1     PIC 9(07)   VALUE   1.
           03  CONST-PRTKANRI-TBL-KEY  PIC X(08)   VALUE   "TEIKI2".
           03  CONST-3003-KBNCD        PIC X(08)   VALUE   "033".

      *
           COPY    "CPTEIKI1BTPARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK3003.INC".
      *
      *    請求管理マスタ
           COPY    "CPSK5010.INC".
      *
      *    定期請求作成指示コード
           COPY    "CPSK5011.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    定期請求履歴
       01  TEIKIRRK-REC.
           COPY    "CPTEIKIRRK.INC".
      *
      *    定期請求クエリ
       01  QTEIKI-REC.
           COPY    "CPQTEIKI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
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
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *     
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
           MOVE    SPA-FREE        TO  SPA-O01
           MOVE    SPA-WORK        TO  SPA-O01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
           WHEN    "LINK"          ALSO    ANY
               PERFORM 100-INIT-SEC
      *        画面遷移
           WHEN    "PUTG"          ALSO    "CLICKED"
               PERFORM 200-GMNSENI
      *    入力
           WHEN    OTHER
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
           MOVE    SPA-O01         TO  SPA-FREE
      *
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
           IF    ( SPA-MOTOPG          =   "OERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF    ( FLG-END         =   1 )
                   GO  TO  100-INIT-EXT
               END-IF
      *
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
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "O01   "            TO  MCP-WINDOW.
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
       300-SCREEN-SEC                  SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    他のＬＤより
           IF    ( LINKAREA        NOT =   SPACE )
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      SPA-MOTOPG      =   "XB01" OR "XA01"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-O01
                   INITIALIZE              SPA-O01KYOUTU
                   GO  TO  300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               MOVE    WRK-MOTOPG          TO  SPA-MOTOPG
      *
               INITIALIZE              SPA-O01KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
           WHEN    "OID1"
               PERFORM 330-OID1-SET-SEC
           WHEN    "O02"
           WHEN    "O03"
           WHEN    "O04"
               PERFORM 350-O01SPASET-SEC
           WHEN    "O99"
               PERFORM 340-OID2-SET-SEC
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
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-O01
                                       SPA-O01KYOUTU
      *
           PERFORM 3101-PRTKBNLST-SEC
      *
           MOVE    "T"             TO  SPA-GMN-IKKATU
           MOVE    "F"             TO  SPA-GMN-KOBETU
      *
           MOVE    ZERO            TO  SPA-O01-DENPPRTYMD
           PERFORM 3101-DENPPRTYMD-EDIT-SEC
      *
           PERFORM 490-CLEAR-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷区分リスト編集処理
      *****************************************************************
       3101-PRTKBNLST-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-PRTKBNLST-G
      *
           MOVE    "1"                     TO  SPA-GMN-PRTKBNL    (1)
           MOVE    "新規作成"              TO  SPA-GMN-PRTKBNMEIL (1)
           MOVE    "2"                     TO  SPA-GMN-PRTKBNL    (2)
           MOVE    "再印刷（前回作成）"    TO  SPA-GMN-PRTKBNMEIL (2)
           MOVE    "3"                     TO  SPA-GMN-PRTKBNL    (3)
           MOVE    "再印刷（指定年月）"    TO  SPA-GMN-PRTKBNMEIL (3)
      *
           MOVE    3                       TO  SPA-GMN-PRTKBN-MAX
      *
           .
       3101-PRTKBNLST-EXT.
           EXIT.
      *****************************************************************
      *    定期請求期間取得処理
      *****************************************************************
       3101-TEIKI-KIKAN-GET-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KIKAN-VAL
                                       SPA-NAI-KIKAN-VAL
                                       SPA-NAI-DELKBN
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
               MOVE    SYSKANRI-REC    TO  SYS-5010-REC
           ELSE
               INITIALIZE                  SYS-5010-REC
           END-IF
      *
           MOVE    1                   TO  WRK-STDAY
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1        >   3 )
                    OR   ( SYS-5010-SEIKYU-DAY (IDX1)  =   ZERO )
      *
               INITIALIZE                  WRK-KIKAN-YMD
      *
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    WRK-STDAY       TO  WRK-SYMD (7 : 2)
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  WRK-KIKAN-STYMD-W
               MOVE    WRK-SYMD        TO  WRK-KIKAN-STYMD-S
      *
               IF    ( SYS-5010-SEIKYU-DAY (IDX1)  =   99 )
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY1-AREA
                   MOVE    "71"                TO   LNK-DAY7-IRAI
                   MOVE    SPA-NAI-SRYYM       TO   LNK-DAY7-YM
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY7-AREA
                   MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD
                                                   WRK-KIKAN-EDYMD-S
      *
                   PERFORM 800-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  WRK-KIKAN-EDYMD-W
               ELSE
                   MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
                   MOVE    SYS-5010-SEIKYU-DAY (IDX1)
                                           TO  WRK-SYMD (7 : 2)
                   PERFORM 800-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  WRK-KIKAN-EDYMD-W
                   MOVE    WRK-SYMD        TO  WRK-KIKAN-EDYMD-S
      *
                   COMPUTE WRK-STDAY   =   SYS-5010-SEIKYU-DAY (IDX1)
                                       +   1
      *
               END-IF
      *
               MOVE    SPACE               TO  SPA-GMN-KIKAN (IDX1)
      *
               STRING  WRK-KIKAN-STYMD-W
                       "〜"
                       WRK-KIKAN-EDYMD-W
                       DELIMITED   BY  SIZE
               INTO    SPA-GMN-KIKAN (IDX1)
               END-STRING
      *
               MOVE    WRK-KIKAN-STYMD-S   TO  SPA-NAI-KIKANST (IDX1)
               MOVE    WRK-KIKAN-EDYMD-S   TO  SPA-NAI-KIKANED (IDX1)
      *
           END-PERFORM
      *
      *
           PERFORM VARYING IDX1    FROM    1   BY  1   UNTIL 
                         ( IDX1    >   3 )
               IF    ( SPA-GMN-KIKAN (IDX1)    =   SPACE )
                   MOVE    "指定なし"  TO  SPA-GMN-KIKAN (IDX1)
               END-IF
           END-PERFORM
      *
           MOVE    "F"             TO  SPA-GMN-CHK-KIKAN01
                                       SPA-GMN-CHK-KIKAN02
                                       SPA-GMN-CHK-KIKAN03
      *
           IF    ( SYS-5010-DELKBN =   SPACE )
               MOVE    "1"             TO  SPA-NAI-DELKBN
           ELSE
               MOVE    SYS-5010-DELKBN TO  SPA-NAI-DELKBN
           END-IF
      *
           .
       3101-TEIKI-KIKAN-GET-EXT.
           EXIT.
      *****************************************************************
      *    診療年月編集処理
      *****************************************************************
       3101-SRYYM-EDIT-SEC             SECTION.
      *
           IF    ( SPA-O01-SRYYM       =   ZERO )
               MOVE    SPA-SYSYMD (1:6)    TO  SPA-O01-SRYYM
           END-IF
      *
           MOVE    SPA-O01-SRYYM       TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 800-HIZUKE-HEN-SEC
           MOVE    SPA-O01-SRYYM       TO  SPA-NAI-SRYYM
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-GMN-SRYYM
      *
           .
       3101-SRYYM-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    伝票発行日編集処理
      *****************************************************************
       3101-DENPPRTYMD-EDIT-SEC        SECTION.
      *
           IF    ( SPA-O01-DENPPRTYMD  =   ZERO )
               MOVE    SPA-SYSYMD      TO  SPA-O01-DENPPRTYMD
           END-IF
      *
           MOVE    SPA-O01-DENPPRTYMD  TO  WRK-SYMD
           PERFORM 800-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-DENPPRTYMD
      *
           MOVE    SPA-O01-DENPPRTYMD  TO  SPA-NAI-DENPPRTYMD
      *
           .
       3101-DENPPRTYMD-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    最終一括処理実施日編集処理
      *****************************************************************
       3101-LASTRUNDATE-EDIT-SEC   SECTION.
      *
           PERFORM 900-TEIKIRRK-KEY2-SEL-SEC
      *
           IF    ( FLG-TEIKIRRK    =   ZERO )
               IF    ( TEIKIRRK-UPYMD  NOT =   SPACE )
      *
                   MOVE    1               TO  WRK-PNT
                   MOVE    SPACE           TO  SPA-GMN-LBLINFO01
                   MOVE    TEIKIRRK-SRYYM  TO  WRK-SYMD
                   MOVE    "01"            TO  WRK-SYMD(7:2)
                   PERFORM 800-HIZUKE-HEN-SEC
                   STRING  "前回実施期間："
                           WRK-HENYMDG (1:6)   DELIMITED BY  SIZE
                           " - "               DELIMITED BY  SIZE
                           TEIKIRRK-KBT        DELIMITED BY  SIZE
                           "期"                DELIMITED BY  SIZE
                   INTO    SPA-GMN-LBLINFO01
                   WITH    POINTER WRK-PNT
                   END-STRING
      *
                   MOVE    TEIKIRRK-UPYMD  TO  WRK-SYMD
                   PERFORM 800-HIZUKE-HEN-SEC
                   STRING  "　実施日："    DELIMITED BY  SIZE
                           WRK-HENYMDG     DELIMITED BY  SIZE
                   INTO    SPA-GMN-LBLINFO01
                   WITH    POINTER WRK-PNT
                   END-STRING
      *
               END-IF
      *
           END-IF
      *
           .
       3101-LASTRUNDATE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    確認画面（ＯＩＤ１）ＯＫ処理
      *****************************************************************
       330-OID1-SET-SEC            SECTION.
      *
           IF    ( SPA-OID1-FLG    =   "OK" )
               EVALUATE    SPA-OIDCD
               WHEN    "1004"
               WHEN    "1005"
                   PERFORM 4501-O03-SENI-SEC
               WHEN    "6666"
                   MOVE    "JOIN"          TO  MCP-PUTTYPE
                   PERFORM 210-BACK-SYORI-SEC
               WHEN    OTHER
                   PERFORM 4501-JOBKANRICHK-SEC
                   IF    ( SPA-ERRCD   =   SPACE )
                       PERFORM 450-KOUSHIN-SEC
                   END-IF
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-OID1-FLG
                                       SPA-O99-FLG
           MOVE    SPACE           TO  SPA-OIDCD
           MOVE    SPACE           TO  SPA-OIDCD2
           .
       330-OID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＯＩＤ２）ＯＫ処理
      *****************************************************************
       340-OID2-SET-SEC            SECTION.
      *
           IF      SPA-O99-FLG     =   "OK"
               PERFORM 4501-NEXT-GMN-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-OID1-FLG
                                       SPA-O99-FLG
           MOVE    SPACE           TO  SPA-OIDCD
           MOVE    SPACE           TO  SPA-OIDCD2
           .
       340-OID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻り画面処理
      *****************************************************************
       350-O01SPASET-SEC           SECTION.
      *
           IF    ( SPA-O01KYOUTU       =   SPACE )
               GO  TO  350-O01SPASET-EXT
           END-IF
      *
           INITIALIZE                          SPA-O01
      *
           PERFORM 3101-PRTKBNLST-SEC
      *
           IF  (   SPA-O01-FILEKBN     =   "1" )
               MOVE    "T"             TO  SPA-GMN-IKKATU
               MOVE    "F"             TO  SPA-GMN-KOBETU
           ELSE
               MOVE    "F"             TO  SPA-GMN-IKKATU
               MOVE    "T"             TO  SPA-GMN-KOBETU
           END-IF
      *
           IF    ( SPA-O01-SRYYM       =   ZERO )
               PERFORM 490-CLEAR-SEC
           ELSE
               MOVE    SPA-O01-PRTKBN  TO  WRK-CMB-CD
               PERFORM 4200-PRTKBNLST-CHK-SEC
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-PRTKBN-G
               PERFORM 3101-SRYYM-EDIT-SEC
               PERFORM 3101-TEIKI-KIKAN-GET-SEC
               MOVE    SPA-O01-CHK-KIKAN01 TO  SPA-GMN-CHK-KIKAN01
               MOVE    SPA-O01-CHK-KIKAN02 TO  SPA-GMN-CHK-KIKAN02
               MOVE    SPA-O01-CHK-KIKAN03 TO  SPA-GMN-CHK-KIKAN03
               PERFORM 3101-DENPPRTYMD-EDIT-SEC
      *
               PERFORM 3101-LASTRUNDATE-EDIT-SEC
      *
           END-IF
      *
      *
           IF    ( SPA-GMN-IKKATU          =   "T" )
               MOVE    7               TO  SPA-GMN-CUR
           ELSE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
      *
           .
       350-O01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                     SECTION.
      *
           MOVE    MCP-WIDGET      TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    戻る
           WHEN    "CLICKED"       ALSO    "B01"
               MOVE    "CHANGE"    TO      MCP-PUTTYPE
               PERFORM 210-BACK
      *    クリア
           WHEN    "CLICKED"       ALSO    "B02"
               PERFORM 310-SPASET-SEC
      *    削除
           WHEN    "CLICKED"       ALSO    "B04"
               PERFORM 440-O05-SENI-SEC
      *    印刷区分
           WHEN    "CLICKED"       ALSO    "B07"
               MOVE    9           TO      SPA-GMN-CUR
      *    作成区分
           WHEN    "CLICKED"       ALSO    "B08"
               IF    ( SPA-GMN-IKKATU  =   "T" )
                   PERFORM 400-KOBETU-SEC
               ELSE
                   PERFORM 400-IKKATU-SEC
               END-IF
      *
      *    再印刷
           WHEN    "CLICKED"       ALSO    "B05"
               PERFORM 490-XA01-SENI-SEC
      *
      *    情報削除
           WHEN    "CLICKED"       ALSO    "B11"
               PERFORM 490-JYOUHOU-DEL-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B12"
               PERFORM 450-TOUROKU-SEC
      *    確定
           WHEN    "CLICKED"       ALSO    "B09"
               PERFORM 490-O04-SENI-SEC
      *    処理確認
           WHEN    "CLICKED"       ALSO    "B13"
               PERFORM 490-KAKUNIN-SEC
      *    期間１
           WHEN    "CLICKED"       ALSO    "CHK_KIKAN01"
               MOVE    1           TO  IDX-CHK
               PERFORM 400-KIKAN-SEC
      *    期間２
           WHEN    "CLICKED"       ALSO    "CHK_KIKAN02"
               MOVE    2           TO  IDX-CHK
               PERFORM 400-KIKAN-SEC
      *    期間３
           WHEN    "CLICKED"       ALSO    "CHK_KIKAN03"
               MOVE    3           TO  IDX-CHK
               PERFORM 400-KIKAN-SEC
      *    一括作成
           WHEN    "CLICKED"       ALSO    "IKKATU"
               PERFORM 400-IKKATU-SEC
      *    個別作成
           WHEN    "CLICKED"       ALSO    "KOBETU"
               PERFORM 400-KOBETU-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
           WHEN        "ACTIVATE"      ALSO    "SRYYM"
               PERFORM 4101-SRYYM-CHK-SEC
           WHEN        OTHER
      *
               MOVE    ZERO        TO  SPA-GMN-CUR
      *
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
      *
           END-EVALUATE
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
      *    印刷区分チェック処理
           PERFORM 4101-PRTKBN-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    伝票発行日チェック処理
           PERFORM 4101-DENPPRTYMD-CHK-SEC
           IF    ( SPA-ERRCD   NOT =   SPACE )
               GO  TO  410-INPUT-CHK-EXT
           END-IF
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
      *    印刷区分
           MOVE    O01-PRTKBN          TO  SPA-GMN-PRTKBN-G
      *
      *    伝票発行日
           MOVE    O01-DENPPRTYMD      TO  SPA-GMN-DENPPRTYMD
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    請求年月（一括）チェック処理
      *****************************************************************
       4101-SRYYM-CHK-SEC           SECTION.
      *
           MOVE    2                  TO  SPA-GMN-CUR
      *
           IF    ( SPA-GMN-IKKATU         =   "F" )
               IF    ( SPA-GMN-PRTKBN     =   "1" )
                   MOVE    "0007"             TO  SPA-ERRCD
                   MOVE    3                  TO  SPA-GMN-CUR
                   GO  TO  4101-SRYYM-CHK-EXT
               ELSE
                   MOVE    "T"                TO  SPA-GMN-IKKATU
                   MOVE    "F"                TO  SPA-GMN-KOBETU
               END-IF
           END-IF
      *
           MOVE   SPA-GMN-SRYYM        TO  WRK-SRYYM-MOT
           MOVE   O01-SRYYM            TO  SPA-GMN-SRYYM
      *
      *    日付画面チェックサブ
           MOVE    SPA-GMN-SRYYM       TO  WRK-SYMD
           PERFORM 800-HIZUKE-YM-CHK-SEC
           IF    ( SGDAY-RC            =   ZERO )
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-SRYYM
           ELSE
               MOVE    WRK-SRYYM-MOT   TO  SPA-GMN-SRYYM
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4101-SRYYM-CHK-EXT
           END-IF
      *
           PERFORM 3101-TEIKI-KIKAN-GET-SEC
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *
           .
       4101-SRYYM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷区分チェック処理
      *****************************************************************
       4101-PRTKBN-CHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-PRTKBN      TO  WRK-CMB-CD
           PERFORM 4200-PRTKBNLST-CHK-SEC
           IF    ( WRK-CMB-ITM         =   SPACE )
               MOVE    "0008"          TO  SPA-ERRCD
           ELSE
               MOVE    WRK-CMB-ITM     TO  SPA-GMN-PRTKBN-G
               IF    ( SPA-GMN-PRTKBN  =   "2" )
                   MOVE    7           TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-PRTKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    伝票発行日チェック処理
      *****************************************************************
       4101-DENPPRTYMD-CHK-SEC         SECTION.
      *
           IF    ( SPA-GMN-DENPPRTYMD  =   SPACE )
               MOVE    SPA-SYSYMD          TO  WRK-SYMD
               PERFORM 800-HIZUKE-YMD-CHK-SEC
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-DENPPRTYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-DENPPRTYMD
           ELSE
               MOVE    SPA-NAI-DENPPRTYMD  TO  WRK-DENPPRTYMD-MOT
      *
               MOVE    SPA-GMN-DENPPRTYMD  TO  WRK-SYMD
               PERFORM 800-HIZUKE-YMD-CHK-SEC
               IF    ( SGDAY-RC            =   ZERO )
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-DENPPRTYMD
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-DENPPRTYMD
               ELSE
                   MOVE    "0003"          TO  SPA-ERRCD
                   MOVE    WRK-DENPPRTYMD-MOT  TO  WRK-SYMD
                   PERFORM 800-HIZUKE-YMD-CHK-SEC
                   MOVE    SGDAY-OTDAY     TO  SPA-GMN-DENPPRTYMD
                   MOVE    SGDAY-SDAY      TO  SPA-NAI-DENPPRTYMD
      *
                   MOVE    8               TO  SPA-GMN-CUR
      *
               END-IF
           END-IF
      *
           .
       4101-DENPPRTYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷区分チェック処理
      *****************************************************************
       4200-PRTKBNLST-CHK-SEC          SECTION.
      *
           INITIALIZE                  WRK-CMB-ITM
      *
           PERFORM VARYING IDX-CMB FROM    1   BY  1
                   UNTIL ( IDX-CMB >   SPA-GMN-PRTKBN-MAX )
                    OR   ( WRK-CMB-ITM NOT =   SPACE )
      *
               IF    ( SPA-GMN-PRTKBNL (IDX-CMB)
                                       =   WRK-CMB-CD )
                   MOVE    SPA-GMN-PRTKBNLST (IDX-CMB)
                                       TO  WRK-CMB-ITM
               END-IF
      *
           END-PERFORM
      *
           .
       4101-PRTKBNLST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択期間チェック処理
      *****************************************************************
       4200-SELKBT-CHK-SEC             SECTION.
      *
           MOVE    ZERO            TO  CNT-SELKBT
           INITIALIZE                  WRK-SELKBT-G
      *
           PERFORM VARYING IDX-SELKBT  FROM    1   BY  1
                   UNTIL ( IDX-SELKBT  >   3 )
      *
               IF    ( SPA-GMN-CHK-KIKAN (IDX-SELKBT)
                                       =   "T" )
      *
                   COMPUTE CNT-SELKBT  =   CNT-SELKBT  +   1
      *
                   MOVE    "1"     TO   WRK-SELKBT (IDX-SELKBT)
      *
               END-IF
      *
           END-PERFORM
      *
           .
       4200-SELKBT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ジョブ管理チェック処理
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-3003-REC
               PERFORM VARYING IDX4    FROM    1   BY  1
                       UNTIL ( IDX4    >   SYS-3003-JOBID )
      *
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   MOVE    IDX4            TO  WRK-JOBID
                   PERFORM 800-ORCSJOB-SEC
                   IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
                       IF    ( JOB-ENDYMD      =   SPACE )
                           MOVE    "6666"          TO  SPA-OIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC
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
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "O01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間選択処理
      *****************************************************************
       400-KIKAN-SEC          SECTION.
      *
           MOVE    O01-CHK-KIKAN   (IDX-CHK)
                                   TO  SPA-GMN-CHK-KIKAN (IDX-CHK)
      *
           IF    ( SPA-NAI-KIKANST (IDX-CHK)   =   SPACE )
      *
               MOVE    "F"         TO  SPA-GMN-CHK-KIKAN (IDX-CHK)
      *
           ELSE
               MOVE    "T"         TO  SPA-GMN-IKKATU
               MOVE    "F"         TO  SPA-GMN-KOBETU
      *
               MOVE    ZERO        TO  SPA-GMN-CUR
           END-IF
      *
      *
           .
       400-KIKAN-EXT.
           EXIT.
      *****************************************************************
      *    一括作成選択処理
      *****************************************************************
       400-IKKATU-SEC          SECTION.
      *
           MOVE    "T"             TO  SPA-GMN-IKKATU
           MOVE    "F"             TO  SPA-GMN-KOBETU
      *
           MOVE    "F"             TO  SPA-GMN-CHK-KIKAN01
                                       SPA-GMN-CHK-KIKAN02
                                       SPA-GMN-CHK-KIKAN03
      *
           MOVE    2               TO  SPA-GMN-CUR
           .
       400-IKKATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別作成選択処理
      *****************************************************************
       400-KOBETU-SEC          SECTION.
      *
           MOVE    "T"             TO  SPA-GMN-KOBETU
           MOVE    "F"             TO  SPA-GMN-IKKATU
      *
           MOVE    "F"             TO  SPA-GMN-CHK-KIKAN01
                                       SPA-GMN-CHK-KIKAN02
                                       SPA-GMN-CHK-KIKAN03
      *
           MOVE    "1"             TO  WRK-CMB-CD
           PERFORM 4200-PRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-PRTKBN-G
      *
           MOVE    7               TO  SPA-GMN-CUR
      *
           .
       400-KOBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確認画面遷移処理
      *****************************************************************
       490-O04-SENI-SEC                SECTION.
      *
      *    画面移行用のパラメタ
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    "O04"               TO  SPA-SAKIPG
           MOVE    "O01"               TO  SPA-MOTOPG
           MOVE    "O01"               TO  SPA-O01-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-O04-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括削除画面遷移処理
      *****************************************************************
       440-O05-SENI-SEC                SECTION.
      *
      *    画面移行用のパラメタ
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    "O05"               TO  SPA-SAKIPG
           MOVE    "O01"               TO  SPA-MOTOPG
           MOVE    "O01"               TO  SPA-O01-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       440-O05-SENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-TOUROKU-SEC                 SECTION.
      *
           PERFORM 410-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  450-TOUROKU-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-PRTKBN
           WHEN    "1"
               PERFORM 4501-TEIKI-CHK-SEC
           WHEN    "2"
               PERFORM 4501-REPRINT-LAST-CHK-SEC
           WHEN    "3"
               PERFORM 4501-REPRINT-YM-CHK-SEC
           END-EVALUATE
      *
           .
       450-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求実行前チェック処理
      *****************************************************************
       4501-TEIKI-CHK-SEC              SECTION.
      *
      *    ジョブ管理チェック
           PERFORM 4501-JOBKANRICHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  4501-TEIKI-CHK-EXT
           END-IF
      *
      *    画面移行用のパラメタ変更
           PERFORM 45011-PARA-HENSYU-SEC
      *
      *    一括作成のときのチェック
           EVALUATE    SPA-GMN-IKKATU
           WHEN    "T"
               IF    ( SPA-NAI-SRYYM   =   ZERO )
                   MOVE    "0001"      TO  SPA-ERRCD
                   GO  TO  4501-TEIKI-CHK-EXT
               END-IF
      *
               IF    ( SPA-GMN-CHK-KIKAN01 =   "F" )
                AND  ( SPA-GMN-CHK-KIKAN02 =   "F" )
                AND  ( SPA-GMN-CHK-KIKAN03 =   "F" )
                   MOVE    "0020"      TO  SPA-ERRCD
                   GO  TO  4501-TEIKI-CHK-EXT
               END-IF
      *
           END-EVALUATE
      *
      *    ガイドメッセージセット
           IF    ( SPA-O01-FILEKBN     =   "1" )
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG3    TO  WRK-SRYYMH
               MOVE    "1001"          TO  SPA-OIDCD
           ELSE
               MOVE    "1003"          TO  SPA-OIDCD
           END-IF
      *
           .
       4501-TEIKI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再印刷実行前チェック処理（前回作成）
      *****************************************************************
       4501-REPRINT-LAST-CHK-SEC       SECTION.
      *
           PERFORM 900-QUERY-TEIKI-KEY-SEL-SEC
           IF    ( FLG-QTEIKI      =  ZERO )
               MOVE    "1005"      TO  SPA-OIDCD
           ELSE
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
       4501-REPRINT-LAST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再印刷実行前チェック処理（指定年月）
      *****************************************************************
       4501-REPRINT-YM-CHK-SEC         SECTION.
      *
           PERFORM 4200-SELKBT-CHK-SEC
      *
           IF    ( CNT-SELKBT      =   ZERO )
               MOVE    "0020"      TO  SPA-ERRCD
               GO   TO 4501-REPRINT-YM-CHK-EXT
           END-IF
      *
           PERFORM 900-QUERY-TEIKI-KEY11-SEL-SEC
           IF    ( FLG-QTEIKI      =  ZERO )
               MOVE    SPA-NAI-SRYYM   TO  WRK-SYMD
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 800-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG3    TO  WRK-SRYYMH
               MOVE    "1004"      TO  SPA-OIDCD
           ELSE
               MOVE    "0002"      TO  SPA-ERRCD
           END-IF
      *
           .
       4501-REPRINT-YM-CHK-EXT.
           EXIT.
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
                       IF    ( SPA-GMN-IKKATU  =   "T" )
                        AND  ( JOB-SYOKBN      =   "1" )
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
      *    更新処理
      *****************************************************************
       450-KOUSHIN-SEC                 SECTION.
      *
      *    新規分作成のとき定期請求指示コード更新
           EVALUATE SPA-GMN-IKKATU
           WHEN    "T"
      *
               PERFORM 4501-SYS5011-HEN-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  450-KOUSHIN-EXT
               END-IF
      *
               PERFORM 4501-SYS3003-DEL-SEC
      *
               PERFORM 4501-BTPARA-DEL-SEC
      *
      *        ジョブ開始セット
               MOVE    "ALL"           TO  SJOBKANRI-MODE
               PERFORM 800-ORCSJOB-SEC
      *
               MOVE    SPA-HOSPNUM         TO  TEIKIERR-HOSPNUM
               MOVE    "OR033ERR"          TO  TEIKIERR-FILE-ID
               MOVE    SPA-TERMID          TO  TEIKIERR-TERMID
               MOVE    1                   TO  TEIKIERR-SYOKBN1
      *
               MOVE    SPACE               TO  SHELLTBL
      *
               MOVE    "teiki1.sh"     TO  SHELLTBL-NAME
      *
               MOVE    SPA-NAI-SRYYM   TO  WRK-SHELL-SRYYM
               MOVE    SPA-TERMID      TO  WRK-SHELL-TERMID
               MOVE    SPA-SYSYMD      TO  WRK-SHELL-SYSYMD
      *
               MOVE    ZERO            TO  WRK-JOBID
      *
               PERFORM 800-MCNDATE-SEC
               MOVE    SMCNDATE-YMD    TO  WRK-JOBYMD
               MOVE    SMCNDATE-HMS    TO  WRK-JOBHMS
      *
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL ( IDX3    >   3 )
      *
                   IF    ( SPA-GMN-CHK-KIKAN (IDX3)    =   "T" )
      *
                       MOVE    "JBS"           TO  SJOBKANRI-MODE
                       COMPUTE WRK-JOBID   =   WRK-JOBID   +   1
                       PERFORM 800-ORCSJOB-SEC
      *
                       MOVE    IDX3    TO  WRK-KBT9
                       PERFORM 4501-BTPARA-INS-SEC
                       IF    ( SPA-ERRCD   NOT =   SPACE )
                           GO  TO  450-KOUSHIN-EXT
                       END-IF
      *
                       INITIALIZE              WRK-SHELL-KIKAN-TBL
                       MOVE    "1"         TO  WRK-SHELL-KIKAN-KBN(IDX3)
      *
                       PERFORM VARYING IDX0    FROM    1   BY  1
                               UNTIL ( IDX0    >   3 )
                           MOVE    SPA-NAI-KIKANST (IDX0)
                                           TO  WRK-SHELL-KIKAN-ST (IDX0)
                           MOVE    SPA-NAI-KIKANED (IDX0)
                                           TO  WRK-SHELL-KIKAN-ED (IDX0)
                           IF    ( SPA-GMN-CHK-KIKAN (IDX0)    =   "T" )
                               MOVE    "1" TO  WRK-SHELL-KIKAN-KBN(IDX0)
                           ELSE
                               MOVE    SPACE
                                           TO  WRK-SHELL-KIKAN-KBN(IDX0)
                           END-IF
                       END-PERFORM
      *
                       MOVE    WRK-SHELL-ARG1
                                           TO  SHELLTBL-ARG1
                       MOVE    "1"         TO  SHELLTBL-ARG2
      *
                       MOVE    WRK-SHELL-KIKAN-OCC (1)
                                           TO  SHELLTBL-ARG3
                       MOVE    WRK-SHELL-KIKAN-OCC (2)
                                           TO  SHELLTBL-ARG4
                       MOVE    WRK-SHELL-KIKAN-OCC (3)
                                           TO  SHELLTBL-ARG5
                       MOVE    SPA-NAI-DENPPRTYMD
                                           TO  SHELLTBL-ARG6
                       MOVE    WRK-JOBID
                                           TO  SHELLTBL-ARG7
                       MOVE    CONST-SHELLID
                                           TO  SHELLTBL-ARG8
                       MOVE    SPA-HOSPNUM
                                           TO  SHELLTBL-ARG9
                       MOVE    TEIKIERR    TO  SHELLTBL-ARG10
      *
                       MOVE    IDX3        TO  WRK-KBT9
                       MOVE    WRK-KBTX    TO  SHELLTBL-ARG11
                       MOVE    WRK-JOBYMD  TO  SHELLTBL-ARG12
                       MOVE    WRK-JOBHMS  TO  SHELLTBL-ARG13
      *
                       MOVE    SHELLTBL    TO  MCPDATA-REC
                       MOVE    "SHELL"     TO  MCP-FUNC
                       MOVE    "shell"     TO  MCP-TABLE
                       MOVE    "monthly"   TO  MCP-PATHNAME
      *
                       CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   END-IF
               END-PERFORM
      *
      *        ＪＯＢＩＤ更新処理
               PERFORM 45012-JOBID-UPD-SEC
               IF    ( SPA-ERRCD       NOT =   SPACE )
                   GO  TO  450-KOUSHIN-EXT
               END-IF
      *
      *        情報確認画面を表示
               PERFORM 490-KAKUNIN-SEC
           WHEN    OTHER
               PERFORM 4501-NEXT-GMN-SEC
           END-EVALUATE
      *
           .
       450-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC             SECTION.
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    CONST-SHELLID       TO  SYS-3003-TBL-KEY
           MOVE    WRK-JOBID           TO  SYS-3003-JOBID
      *
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    "4001"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *****************************************************************
      *    定期請求作成指示編集処理
      *****************************************************************
       4501-SYS5011-HEN-SEC            SECTION.
      *
           INITIALIZE                      SYS-5011-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5011-HOSPNUM
           MOVE    "5011"              TO  SYS-5011-KANRICD
           MOVE    "*"                 TO  SYS-5011-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-5011-STYUKYMD
                                           SYS-5011-EDYUKYMD
           MOVE    SYS-5011-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF  (   FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           INITIALIZE                      SYS-5011-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5011-HOSPNUM
           MOVE    "5011"              TO  SYS-5011-KANRICD
           MOVE    "*"                 TO  SYS-5011-KBNCD
           MOVE    ZERO                TO  SYS-5011-STYUKYMD
           MOVE    ALL "9"             TO  SYS-5011-EDYUKYMD
           MOVE    "1"                 TO  SYS-5011-SYOKBN
           MOVE    SPA-NAI-SRYYM       TO  SYS-5011-SRYYM
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD        TO  SYS-5011-CREYMD
                                           SYS-5011-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYS-5011-UPHMS
           MOVE    SYS-5011-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "1001"          TO  SPA-ERRCD
               DISPLAY "5011 SYSKANRI-KEY =>" SYS-5011-KEY 
           END-IF
           .
      *
       4501-SYS5011-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ実行管理削除処理
      *****************************************************************
       4501-SYS3003-DEL-SEC            SECTION.
      *
           INITIALIZE                      SYS-3003-REC
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    CONST-3003-KBNCD    TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SYS-3003-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF  (   FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBDELETE-SEC
           END-IF
      *
           .
       4501-SYS3003-DEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ削除処理
      *****************************************************************
       4501-BTPARA-DEL-SEC            SECTION.
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    CONST-SHELLID       TO  BTPARA-SHELLID
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
      *
           .
       4501-BTPARA-DEL-EXT.
           EXIT.
      *****************************************************************
      *    バッチパラメタ更新処理
      *****************************************************************
       4501-BTPARA-INS-SEC            SECTION.
      *
           INITIALIZE              BTPARA-REC
                                   TEIKI1BTPARA
           MOVE    "1"             TO  TEIKI1BT-SYOKBN
           MOVE    SPA-NAI-SRYYM   TO  TEIKI1BT-SRYYM
           MOVE    WRK-KBT9        TO  TEIKI1BT-KBT
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    CONST-SHELLID   TO  BTPARA-SHELLID
           MOVE    WRK-JOBID       TO  BTPARA-RENNUM
           MOVE    WRK-JOBID       TO  BTPARA-JOBID
           MOVE    TEIKI1BTPARA    TO  BTPARA-INFO-PARA
           MOVE    SPA-TERMID      TO  BTPARA-TERMID
           MOVE    SPA-OPID        TO  BTPARA-OPID
           PERFORM 800-MCNDATE-SEC
           MOVE    SMCNDATE-YMD    TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD    TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS    TO  BTPARA-UPHMS
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC      NOT =   ZERO )
               MOVE    "4002"      TO  SPA-ERRCD
           END-IF
      *
           .
       4501-BTPARA-INS-EXT.
           EXIT.
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-NEXT-GMN-SEC               SECTION.
      *
           IF    ( SPA-OID1-FLG        =   "OK" )
               IF    ( SPA-GMN-KOBETU  =   "T" )
                   PERFORM 4501-O02-SENI-SEC
               ELSE
                   PERFORM 4501-O03-SENI-SEC
               END-IF
           ELSE
                   PERFORM 4501-O03-SENI-SEC
           END-IF
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別指示画面へ遷移処理
      *****************************************************************
       4501-O02-SENI-SEC               SECTION.
      *
           MOVE    "O01"           TO  SPA-MOTOPG
           MOVE    "O02"           TO  SPA-SAKIPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       4501-O02-SENI-EXT.
           EXIT.
      *****************************************************************
      *    印刷指示画面へ遷移処理
      *****************************************************************
       4501-O03-SENI-SEC               SECTION.
      *
      *    画面移行用のパラメタ
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    "O01"           TO  SPA-MOTOPG
           MOVE    "O03"           TO  SPA-SAKIPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       4501-O03-SENI-EXT.
           EXIT.
      *****************************************************************
      *    画面移行用のパラメタ変更処理
      *****************************************************************
       45011-PARA-HENSYU-SEC           SECTION.
      *
           MOVE    SPA-GMN-PRTKBN          TO  SPA-O01-PRTKBN
      *
           IF    ( SPA-GMN-IKKATU          =   "T" )
               MOVE    "1"                 TO  SPA-O01-FILEKBN
           ELSE
               MOVE    "2"                 TO  SPA-O01-FILEKBN
           END-IF
      *
           MOVE    SPA-NAI-SRYYM           TO  SPA-O01-SRYYM
           MOVE    SPA-GMN-CHK-KIKAN01     TO  SPA-O01-CHK-KIKAN01
           MOVE    SPA-GMN-CHK-KIKAN02     TO  SPA-O01-CHK-KIKAN02
           MOVE    SPA-GMN-CHK-KIKAN03     TO  SPA-O01-CHK-KIKAN03
           PERFORM 4200-SELKBT-CHK-SEC
           MOVE    WRK-SELKBT-G            TO  SPA-O01-KBT-G
      *
           MOVE    SPA-NAI-DENPPRTYMD      TO  SPA-O01-DENPPRTYMD
      *
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
               MOVE    SYSKANRI-REC    TO  SYS-5010-REC
           ELSE
               INITIALIZE                  SYS-5010-REC
           END-IF
      *
           MOVE    SYS-5010-CHGWINDOWKBN   TO  SPA-O01-CHGWINDOWKBN
      *
           MOVE    SPA-O01KYOUTU           TO  SPA-WORK
      *
           .
       45011-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
      *    再印刷を選択時は表示しない
           IF    ( SPA-GMN-PRTKBN =   "2" OR "3" )
               GO  TO  490-KAKUNIN-EXT
           END-IF
      *
      *    画面移行用のパラメタ
           PERFORM 45011-PARA-HENSYU-SEC
      *
           MOVE    "O01"           TO  SPA-MOTOPG
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
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "O01"           TO  SPA-MOTOPG
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
      *    再印刷画面へ
      *****************************************************************
       490-XA01-SENI-SEC               SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-O01             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "O01"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           MOVE    CONST-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
      *
           MOVE    CONST-SHELLID       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XA01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-XA01-SENI-EXT.
           EXIT.
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-O01             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "O01"               TO  SPA-MOTOPG
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
      *    クリア処理
      *****************************************************************
       490-CLEAR-SEC                   SECTION.
      *
           MOVE    "1"             TO  WRK-CMB-CD
           PERFORM 4200-PRTKBNLST-CHK-SEC
           MOVE    WRK-CMB-ITM     TO  SPA-GMN-PRTKBN-G
      *
           MOVE    ZERO            TO  SPA-O01-SRYYM
           PERFORM 3101-SRYYM-EDIT-SEC
           PERFORM 3101-TEIKI-KIKAN-GET-SEC
      *
           PERFORM 3101-LASTRUNDATE-EDIT-SEC
      *
           .
       490-CLEAR-EXT.
           EXIT.
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
           IF    ( SPA-OIDCD2      NOT =   SPACE )
               PERFORM 530-OIDSET2-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "O01    "           TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  O01
      *
           INITIALIZE                      O01
           MOVE    SPA-GMN-PRTKBN-G    TO  O01-PRTKBN
           MOVE    SPA-GMN-PRTKBN-MAX  TO  O01-PRTKBN-COUNT
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   SPA-GMN-PRTKBN-MAX )
      *
               MOVE    SPA-GMN-PRTKBNLST (IDX2)
                                       TO  O01-PRTKBNLST (IDX2)
      *
           END-PERFORM
      *
           MOVE    WIDGET-NORMAL       TO  O01-SRYYM-STATE
           IF    ( SPA-GMN-PRTKBN =    "2" )
               MOVE    WIDGET-INSENSITIVE  TO  O01-SRYYM-STATE
           ELSE
               MOVE    "請求年月"      TO  O01-LBLSRYYM
               MOVE    SPA-GMN-SRYYM   TO  O01-SRYYM
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  O01-DENPPRTYMD-STATE
           IF    ( SPA-GMN-PRTKBN =   "2" OR "3" )
               MOVE    WIDGET-INSENSITIVE  TO  O01-DENPPRTYMD-STATE
           ELSE
               MOVE    "伝票発行日"    TO  O01-LBLDENPPRTYMD
               MOVE    SPA-GMN-DENPPRTYMD
                                       TO  O01-DENPPRTYMD
           END-IF
      *
           MOVE    SPA-GMN-LBLINFO01   TO  O01-LBLINFO01
      *
           MOVE    SPA-GMN-IKKATU      TO  O01-IKKATU
           MOVE    SPA-GMN-KOBETU      TO  O01-KOBETU
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               MOVE    SPA-GMN-CHK-KIKAN (IDX2)
                                       TO  O01-CHK-KIKAN (IDX2)
           END-PERFORM
      *
           MOVE    SPA-GMN-KIKAN01     TO  O01-KIKAN01
           MOVE    SPA-GMN-KIKAN02     TO  O01-KIKAN02
           MOVE    SPA-GMN-KIKAN03     TO  O01-KIKAN03
      *
           MOVE    WIDGET-INSENSITIVE  TO  O01-B04-STATE
           IF    ( SPA-NAI-DELKBN      =   "2" )
               MOVE    WIDGET-NORMAL   TO  O01-B04-STATE
               MOVE    "請求削除"      TO  O01-B04-LABEL
           END-IF
      *
           MOVE    WIDGET-NORMAL       TO  O01-B13-STATE
           IF    ( SPA-GMN-PRTKBN =   "2" OR "3" )
               MOVE    WIDGET-INSENSITIVE
                                       TO  O01-B13-STATE
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
           IF    ( SPA-ERRCD       =   SPACE)
            AND  ( SPA-GMN-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           PERFORM 5012-SKIP-CUR-SEC
      *
           EVALUATE    SPA-GMN-CUR
           WHEN    1
               MOVE  "SYOKBN"      TO  MCP-WIDGET
           WHEN    2
               MOVE  "SRYYM"       TO  MCP-WIDGET
           WHEN    3
               MOVE  "CHK_KIKAN01" TO  MCP-WIDGET
           WHEN    4
               MOVE  "CHK_KIKAN02" TO  MCP-WIDGET
           WHEN    5
               MOVE  "CHK_KIKAN03" TO  MCP-WIDGET
           WHEN    7
               MOVE  "B12"         TO  MCP-WIDGET
           WHEN    8
               MOVE  "DENPPRTYMD"  TO  MCP-WIDGET
           WHEN    9
               MOVE  "PRTKBN"      TO  MCP-WIDGET
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
           EVALUATE    WRK-MCP-WIDGET
           WHEN    "SYOKBN"
               MOVE    002             TO  SPA-GMN-CUR
           WHEN    "SRYYM"
               MOVE    008             TO  SPA-GMN-CUR
           WHEN    "CHK_KIKAN01"
               MOVE    004             TO  SPA-GMN-CUR
           WHEN    "CHK_KIKAN02"
               MOVE    005             TO  SPA-GMN-CUR
           WHEN    "CHK_KIKAN03"
               MOVE    007             TO  SPA-GMN-CUR
           WHEN    "DENPPRTYMD"
               MOVE    003             TO  SPA-GMN-CUR
           WHEN    "PRTKBN"
               MOVE    002             TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置変更処理
      *****************************************************************
       5012-SKIP-CUR-SEC               SECTION.
      *
           IF    ( SPA-GMN-CUR         =   2 )
               IF    ( O01-SRYYM-STATE =   WIDGET-INSENSITIVE )
                   MOVE    8       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   8 )
               IF    ( O01-DENPPRTYMD-STATE    =   WIDGET-INSENSITIVE )
                   MOVE    3       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   3 )
               IF    ( SPA-NAI-KIKANST (1) =   SPACE )
                   MOVE    7       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   4 )
               IF    ( SPA-NAI-KIKANST (2) =   SPACE )
                   MOVE    7       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF    ( SPA-GMN-CUR     =   5 )
               IF    ( SPA-NAI-KIKANST (3) =   SPACE )
                   MOVE    7       TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       5012-SKIP-CUR-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
           WHEN    "0001"
               MOVE    "診療年月入力エラー" 
                                           TO  SPA-ERRMSG
           WHEN    "0002"
               MOVE   "作成済のデータがないので再印刷は指定できません"
                                           TO  SPA-ERRMSG
           WHEN    "0003"
               MOVE    "伝票発行日入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "0004"
               MOVE    "対象のデータを選択してありません"
                                           TO  SPA-ERRMSG
           WHEN    "0005"
               MOVE    "対象のデータが選択してあります"
                                           TO  SPA-ERRMSG
           WHEN    "0007"
               MOVE    "個別作成のとき入力できません"
                                           TO  SPA-ERRMSG
           WHEN    "0008"
               MOVE    "印刷区分入力エラー"
                                           TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "定期請求指示コードが更新できません"
                                           TO  SPA-ERRMSG
           WHEN    "0010"
               MOVE    "作成済みの帳票管理ファイルがありません"
                                           TO  SPA-ERRMSG
           WHEN    "0020"
               MOVE    "対象の期間が選択されていません"
                                           TO  SPA-ERRMSG
           WHEN    "4001"
               MOVE    "システム管理の更新に失敗しました"
                                           TO  SPA-ERRMSG
           WHEN    "4002"
               MOVE    "バッチパラメタの更新に失敗しました"
                                           TO  SPA-ERRMSG
           WHEN    "7777"
               MOVE    WRK-TEIKIERR        TO  SPA-ERRMSG
           WHEN    "9999"
               MOVE    WRK-TEIKIERR        TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                   TO  OERR
           MOVE    SPA-ERRCD               TO  OERR-ERRCODE
           MOVE    SPA-ERRMSG              TO  OERR-ERRMSG
           MOVE    "B01"                   TO  MCP-WIDGET
      *
           MOVE    "O01"                   TO  SPA-MOTOPG
           MOVE    "OERR"                  TO  SPA-SAKIPG
      *
           MOVE    "NEW"                   TO  MCP-PUTTYPE
           MOVE    "OERR"                  TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                       TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-OIDSET-SEC                  SECTION.
      *
           MOVE    SPACE                TO  WRK-OIDMSG
      *
           EVALUATE    SPA-OIDCD
           WHEN    "1001"
               MOVE    WRK-SRYYMH       TO  WRK-OIDMSG
               MOVE    "分新規作成処理を行います"
                                        TO  WRK-OIDMSG (17:)
           WHEN    "1003"
               MOVE    "個別作成分の新規作成処理を行います" 
                                        TO  WRK-OIDMSG
           WHEN    "1004"
               INITIALIZE                   WRK-OIDMSG
               STRING  WRK-SRYYMH       DELIMITED  BY  SIZE
                       "分再印刷処理を行います"
                                        DELIMITED  BY  SIZE
               INTO    WRK-OIDMSG
               END-STRING
           WHEN    "1005"
               MOVE    "前回作成分再印刷処理を行います" 
                                        TO  WRK-OIDMSG
           WHEN    "6666"
               MOVE    "患者請求作成処理中です"
                                        TO  WRK-OIDMSG
           WHEN    "8888"
               MOVE    "患者請求作成処理が正常終了しました"
                                        TO  WRK-OIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  OID1
           INITIALIZE                       OID1
           MOVE    SPA-OIDCD            TO  OID1-ID1CODE
           MOVE    WRK-OIDMSG           TO  OID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "O01"                TO  SPA-MOTOPG
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
      *    エラーメッセージ表示理
      *****************************************************************
       530-OIDSET2-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-OIDMSG
      *
           EVALUATE    SPA-OIDCD2
           WHEN    "8888"
               MOVE    "定期請求作成処理が正常終了しました"
                                       TO  WRK-OIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  OID2
           INITIALIZE                      OID2
           MOVE    SPA-OIDCD2          TO  OID2-ID1CODE
           MOVE    WRK-OIDMSG          TO  OID2-ID1MSG
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "O01"               TO  SPA-MOTOPG
           MOVE    "OID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "OID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-OIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       800-HIZUKE-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENYMDG3
      *
           IF    ( WRK-SYMD        NOT =   SPACE )
      *
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
               MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
      *
           END-IF
      *
           .
       800-HIZUKE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    年月チェック処理
      *****************************************************************
       800-HIZUKE-YM-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"              USING
                                           ORCSGDAYAREA
      *
           .
       800-HIZUKE-YM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年月日チェック処理
      *****************************************************************
       800-HIZUKE-YMD-CHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SYMD            TO  SGDAY-INDAY
           CALL    "ORCSGDAY"              USING
                                           ORCSGDAYAREA
      *
           .
       800-HIZUKE-YMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
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
           MOVE    WRK-JOBID           TO  JOB-JOBID
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
      *    管理マスター読込（KEY10）
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
      *
      *****************************************************************
      *    定期請求クエリ検索処理(KEY)
      *****************************************************************
       900-QUERY-TEIKI-KEY-SEL-SEC     SECTION.
      *
           MOVE    ZERO            TO  FLG-QTEIKI
      *
           INITIALIZE                  QTEIKI-REC
           MOVE    SPA-HOSPNUM     TO  QTEIKI-HOSPNUM
           MOVE    "%"             TO  QTEIKI-SELTAIINYMD(1)
                                       QTEIKI-SELTAIINYMD(2)
           MOVE    QTEIKI-REC      TO  MCPDATA-REC
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  QTEIKI-REC
           ELSE
               MOVE    1           TO  FLG-QTEIKI
               INITIALIZE              QTEIKI-REC
           END-IF
      *
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-QUERY-TEIKI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期請求クエリ検索処理(KEY11)
      *****************************************************************
       900-QUERY-TEIKI-KEY11-SEL-SEC   SECTION.
      *
           MOVE    ZERO            TO  FLG-QTEIKI
      *
           INITIALIZE                  QTEIKI-REC
           MOVE    SPA-HOSPNUM     TO  QTEIKI-HOSPNUM
           MOVE    SPA-NAI-SRYYM   TO  QTEIKI-SRYYM
           MOVE    ZERO            TO  CNT-SELKBT
           PERFORM VARYING IDX-SELKBT  FROM    1   BY  1
                   UNTIL ( IDX-SELKBT  >   3 )
               IF    ( SPA-GMN-CHK-KIKAN (IDX-SELKBT)
                                       =   "T" )
      *
                   COMPUTE CNT-SELKBT
                       =   CNT-SELKBT  +   1
      *
                   IF    ( CNT-SELKBT  =   1 )
                       MOVE    CONST-KBT (IDX-SELKBT)
                                   TO  QTEIKI-SELKBT (1)
                                       QTEIKI-SELKBT (2)
                                       QTEIKI-SELKBT (3)
                   ELSE
                       MOVE    CONST-KBT (IDX-SELKBT)
                                   TO  QTEIKI-SELKBT (CNT-SELKBT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    "%"             TO  QTEIKI-SELTAIINYMD(1)
                                       QTEIKI-SELTAIINYMD(2)
           MOVE    QTEIKI-REC      TO  MCPDATA-REC
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  QTEIKI-REC
           ELSE
               MOVE    1           TO  FLG-QTEIKI
               INITIALIZE              QTEIKI-REC
           END-IF
      *
           MOVE    "query_teiki"   TO  MCP-TABLE
           MOVE    "key11"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-QUERY-TEIKI-KEY11-SEL-EXT.
           EXIT.
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
               INITIALIZE                  TEIKIRRK-REC
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                  SECTION.
      *
           MOVE   LOW-VALUE         TO  MCP-TABLE
                                        MCP-PATHNAME
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
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
