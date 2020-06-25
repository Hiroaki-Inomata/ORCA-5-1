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
       PROGRAM-ID.             ORCSCOMMON.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 共通ＳＰＡ設定
      *  管理者            : 
      *  09/01/26    NACL−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 04.05.00     NACL-多々納  09/12/14  エンコーディング追加
      * 04.06.01     NACL-伊藤    11/01/28  リアルタイム送信
      * 04.07.01     NACL-竹田    11/06/29  クライアント印刷情報設定
      * 04.08.00     NACL-太田    14/02/08  Ginbee対応(ユーザー登録なしを許可）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYSBASE         PIC 9(01).
           03  FLG-SYSUSER         PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX                   PIC 9(03).
           03  IDY                   PIC 9(03).
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-SYSYMD-YOBI     PIC X(01).
      *
           03  WRK-MCP-DBSTATUS    PIC X(64).
      *
           03  WRK-SYSBASE-CNT     PIC 9(03).
      *
           03  WRK-ERRMSG          PIC X(500).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    システム管理マスタ領域
      *    職員情報
           COPY  "CPSK1010.INC".
           COPY  "CPSK1050.INC".
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *    自動算定情報
           COPY  "CPSK1007.INC".
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *    排他管理情報
           COPY  "CPSK9800.INC".
      *    主科設定情報
           COPY  "CPSK2009.INC".
      *    広告情報
           COPY  "CPSK1003.INC".
      *
           COPY  "CPSK1038.INC".
      *
      *    システム基本
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システムユーザー
       01  SYSUSER-REC.
           COPY    "CPSYSUSER.INC".
      *
      *    システムカタログ
           COPY    "CPDBCTG.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      * 
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
           COPY    MCPAREA.
           COPY    COMMON-SPA.
           COPY    "CPORCSCOMMON.INC".
      *    職員情報
           COPY  "CPSK1010.INC"        REPLACING
                                       //SYS-// BY //LNK-SYS-//.
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPA-AREA
           ORCSCOMMONAREA
           LNK-SYS-1010-REC
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                     FLG-AREA
           INITIALIZE                     WRK-AREA
      *
           EVALUATE    ORCSCOMMON-PG
               WHEN    "M00"
      *            マスターメニュウー
                   PERFORM 100-SPA-INIT-SEC
               WHEN    "M01"
               WHEN    "M01N"
      *            業務メニュー
                   PERFORM 200-M01-SPAHEN-SEC
               WHEN    OTHER
      *            本院分院切替処理
                   PERFORM 300-HOSPNUM-CHANGE-SEC
           END-EVALUATE
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    マスターメニュー処理
      *****************************************************************
       100-SPA-INIT-SEC                SECTION.
      *
      *    ミドルウェア
           EVALUATE    MCP-MIDDLEWARE-NAME
           WHEN    "panda"
               MOVE    SPA-PANDA       TO  SPA-MW
           WHEN    "ginbee"
               MOVE    SPA-GINBEE      TO  SPA-MW
      *    取得できない場合はpandaとみなす
           WHEN    OTHER
               MOVE    SPA-PANDA       TO  SPA-MW
           END-EVALUATE
      *
      *    端末ＩＤ
           MOVE    MCP-TERM            TO  SPA-TERMID
           PERFORM VARYING IDX     FROM    64  BY  -1
                   UNTIL   IDX     <       1
               IF      SPA-TERMID (IDX:1)  NOT =   SPACE
                   MOVE    1                   TO  IDX 
               ELSE
                   MOVE    "Z"                 TO  SPA-TERMID (IDX:1)
               END-IF
           END-PERFORM 
      *
      *    オペレータＩＤ
      *****当モジュールをCALLする前にSPA-OPIDにセットする
      *****MOVE    MCP-USER            TO  SPA-OPID
      *
      *    医療機関基本情報編集
           PERFORM 1001-SYS1001-SPA-SEC
      *    患者番号構成管理情報・排他制御情報
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1002-SYS1009-SPA-SEC
           END-IF
      *    職員情報編集
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1003-SYS1010-SPA-SEC
           END-IF
      *    スーパーバイザー？
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-SUPERVISOR          =   "F" )
               AND   ( SYS-1010-KANRIAUTHFLG   =   "T" )
                   PERFORM 1004-SUPERVISOR-SPA-SEC
               END-IF
           END-IF
      *    主科設定情報
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1005-SYS2009-SPA-SEC
           END-IF
      *    本院・分院編集処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1007-GRPHOSPNUM-SET-SEC
           END-IF
      *    エンコード取得処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1008-DBCATLG-GET-SEC
           END-IF
      *
           .
       100-SPA-INIT-EXT.
           EXIT.
      *****************************************************************
      *    医療機関基本情報からのＳＰＡ編集処理
      *****************************************************************
       1001-SYS1001-SPA-SEC         SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
               MOVE    SYS-1001-HOSPID     TO  SPA-HOSPID
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *        病床数
               MOVE    SYS-1001-BEDSU      TO  SPA-BEDSU
      *        病床数（一般）
               IF     (SYS-1001-BEDSUIPN(1:4)  =   SPACE )  OR
                      (SYS-1001-BEDSUIPN(1:4)  NOT NUMERIC)
                   MOVE    ZERO                TO  SPA-BEDSUIPN
               ELSE
                   MOVE    SYS-1001-BEDSUIPN   TO  SPA-BEDSUIPN
               END-IF
      *
      *        データ作成フラグ
               IF     (SYS-1001-DATACREATEFLG  =   SPACE )  OR
                      (SYS-1001-DATACREATEFLG  NOT NUMERIC)
                   MOVE    ZERO            TO  SPA-DATACREATEFLG
               ELSE
                   MOVE    SYS-1001-DATACREATEFLG
                                               TO  SPA-DATACREATEFLG
               END-IF
      *        データ提出方法区分
               IF     (SYS-1001-DATATEISYUTUKBN  =   SPACE )  OR
                      (SYS-1001-DATATEISYUTUKBN  NOT NUMERIC)
                   MOVE    ZERO            TO  SPA-DATATEISYUTUKBN
               ELSE
                   MOVE    SYS-1001-DATATEISYUTUKBN
                                           TO  SPA-DATATEISYUTUKBN
               END-IF
      *        データ感染症区分
               IF     (SYS-1001-DATAINFECTIONKBN  =   SPACE )  OR
                      (SYS-1001-DATAINFECTIONKBN  NOT NUMERIC)
                   IF      SPA-DATACREATEFLG   =   1
                       MOVE    1           TO  SPA-DATAINFECTIONKBN
                   ELSE
                       MOVE    ZERO        TO  SPA-DATAINFECTIONKBN
                   END-IF
               ELSE
                   MOVE    SYS-1001-DATAINFECTIONKBN
                                           TO  SPA-DATAINFECTIONKBN
               END-IF
      *
      *        県番号
               MOVE    SYS-1001-PREFNUM    TO  SPA-PREFNUM
      *        有床区分
               MOVE    1                   TO  SPA-BEDKBN
      *        医療機関名称
               MOVE    SYS-1001-HOSPNAME   TO  ORCSCOMMON-HOSPNAME
               MOVE    SYS-1001-HOSPNAME   TO  SPA-HOSPNAME
      *
           ELSE
               MOVE    "1002"              TO  SPA-ERRCD
           END-IF
      *
      *    病床数フラグ
           IF     (SPA-BEDKBN          =   ZERO )  OR
                  (SPA-BEDSU           =   ZERO )
               MOVE    ZERO            TO  SPA-BEDFLG
           ELSE
               MOVE    1               TO  SPA-BEDFLG
           END-IF
           .
       1001-SYS1001-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号構成管理情報からのＳＰＡ編集処理
      *****************************************************************
       1002-SYS1009-SPA-SEC       SECTION.
      *
      *    患者番号構成管理情報編集
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
      *拡張構成区分
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  SPA-1009-KKCKSIATOKETA
           ELSE
               MOVE    "1005"          TO  SPA-ERRCD
           END-IF
      *
      *    患者番号構成の年切替えチェック
           IF      SYS-1009-NENTUKI        =   SPACE
               MOVE    "01"                TO  SYS-1009-NENTUKI
           END-IF
           IF     (SYS-1009-PTNUMKSIKBN    =   "2" )  AND
                  (SYS-1009-HJNKSIKBN      =   "1"
                                           OR  "2" )  AND
                  (SYS-1009-NEN        NOT =   SPA-SYSYMD (1:4)) AND
                  (SYS-1009-NENTUKI        <=  SPA-SYSYMD (5:2))
               STRING  "【患者番号構成の年切替え月になりました。"
                       "システム管理情報を更新して下さい。】"
                                           DELIMITED  BY  SIZE
                                           INTO    ORCSCOMMON-LABLMSG
               END-STRING
           ELSE
               MOVE    SPACE               TO  ORCSCOMMON-LABLMSG
           END-IF
      *
      *    排他管理情報編集
           MOVE    SPACE               TO  SYS-9800-REC
           INITIALIZE                  SYS-9800-REC
           MOVE    "9800"              TO  SYS-9800-KANRICD
           MOVE    "*"                 TO  SYS-9800-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9800-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9800-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9800-HOSPNUM
           MOVE    SYS-9800-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9800-REC
               MOVE    SYS-9800-HAITA      TO  SPA-HAITAFLG
           ELSE
               MOVE    1                   TO  SPA-HAITAFLG
           END-IF
           .
       1002-SYS1009-SPAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報からのＳＰＡ編集処理
      *****************************************************************
       1003-SYS1010-SPA-SEC       SECTION.
      *
      *    職員情報
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           STRING  SPA-OPID            DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1010-TBL
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
               ELSE
                   INITIALIZE                  SYS-1010-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-MCP-DBSTATUS
           PERFORM UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SPA-OPID            =   SYS-1010-USERID
                   MOVE    1                   TO  FLG-OK
                   MOVE    SYS-1010-REC        TO  LNK-SYS-1010-REC
                   MOVE    1                   TO  FLG-SYSKANRI
               ELSE
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 910-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    職員情報なし
           IF     FLG-OK               =   ZERO
      *
               INITIALIZE                  SYS-1010-REC
               INITIALIZE                  LNK-SYS-1010-REC
      *
               IF  ( SPA-MW            =   SPA-GINBEE )
                AND( SPA-MOTOPG        =   "API"    )
                   CONTINUE
               ELSE
                   MOVE    "1001"              TO  SPA-ERRCD
                   GO      TO      1003-SYS1010-SPA-EXT
               END-IF
      *
           END-IF
      *
           INITIALIZE                  SYS-1050-REC
           MOVE    "1050"              TO  SYS-1050-KANRICD
           MOVE    SYS-1010-KBNCD      TO  SYS-1050-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1050-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1050-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1050-HOSPNUM
           MOVE    SYS-1050-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1050-REC
           END-IF
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               MOVE    "1"             TO  SYS-1050-PRTCONF
           END-IF
      *
      *    職員コード設定
           MOVE    SYS-1010-KBNCD(1:5) TO  SPA-STAFFCD
      *    クライアント印刷情報セット
           IF      SYS-1010-CLIENT-PRT =   "1"
               MOVE  2                 TO  SPA-CLIENT-PRT-FLG
           ELSE
               MOVE  ZERO              TO  SPA-CLIENT-PRT-FLG
           END-IF
           IF      SYS-1010-DIALOGFLG  =   "1"
               MOVE  1                 TO  SPA-CLIENT-SHOWDIALOG
           ELSE
               MOVE  ZERO              TO  SPA-CLIENT-SHOWDIALOG
           END-IF
      *
           IF    ( SPA-CLIENT-PRT-FLG  =   ZERO )
            AND  ( SYS-1050-PRTCONF    =   "1" )
               MOVE    "1"             TO  SPA-PRTCONF
           END-IF
      *
      *2001.9.27 強制プロテクト（ＰＧ未作成）
      *   （マスターメニュー表示用）
      **** MOVE    "0"                 TO  LNK-SYS-1010-GSRAUTH(3:1)
           MOVE    "0"                 TO  LNK-SYS-1010-GSRAUTH(4:1)
                                           LNK-SYS-1010-GSRAUTH(4:1)
                                           LNK-SYS-1010-GSRAUTH(46:1)
      *2001.9.27 強制プロテクト（ＰＧ未作成）END
      *
           .
       1003-SYS1010-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    スーパーバイザー設定処理
      *****************************************************************
       1004-SUPERVISOR-SPA-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSBASE
           MOVE    ZERO                TO  WRK-SYSBASE-CNT
           INITIALIZE                      SYSBASE-REC
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM UNTIL   FLG-SYSBASE =   1
                   MOVE    "tbl_sysbase"   TO  MCP-TABLE 
                   MOVE    "all"           TO  MCP-PATHNAME 
                   PERFORM 910-DBFETCH-SEC
                   IF      MCP-RC          =   ZERO
                        ADD     1           TO  WRK-SYSBASE-CNT
                   ELSE
                       MOVE    1           TO  FLG-SYSBASE
                   END-IF
               END-PERFORM
           END-IF
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF      WRK-SYSBASE-CNT     =   1
               MOVE    "T"             TO  SPA-SUPERVISOR
           END-IF
           .
       1004-SUPERVISOR-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定情報設定処理
      *****************************************************************
       1005-SYS2009-SPA-SEC       SECTION.
      *
      *    主科設定情報
           INITIALIZE                      SPA-SYUKA-INFO
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                  SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-2009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-2009-REC
               MOVE    SYS-2009-SYUKA-KBN  TO  SPA-SYUKA-KBN
               MOVE    SYS-2009-SYUKA-NYUIN
                                           TO  SPA-SYUKA-NYUIN
               MOVE    SYS-2009-SYUKA-GAIRAI
                                           TO  SPA-SYUKA-GAIRAI
               MOVE    SYS-2009-SYUKA-MODE TO  SPA-SYUKA-MODE
               MOVE    SYS-2009-SYUKA-SELECT
                                           TO  SPA-SYUKA-SELECT
           ELSE
               MOVE    "0"                 TO  SPA-SYUKA-KBN
           END-IF
           .
       1005-SYS2009-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    本院・分院編集処理
      *****************************************************************
       1007-GRPHOSPNUM-SET-SEC       SECTION.
      *
      *    システム基本テーブル
           INITIALIZE                      SYSBASE-REC
           MOVE    SPA-HOSPNUM         TO  SYSBASE-HOSPNUM
      *
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSBASE-READ-SEC
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           MOVE    "tbl_sysbase"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF     (FLG-SYSBASE             =   ZERO  )  AND
                  (SYSBASE-HONBUNGRP   NOT =   ZERO  )
      *        本院・分院あり
               MOVE    SYSBASE-REC         TO  MCPDATA-REC
               MOVE    "tbl_sysbase"       TO  MCP-TABLE 
               MOVE    "key2"              TO  MCP-PATHNAME 
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sysbase"   TO  MCP-TABLE 
                   MOVE    "key2"          TO  MCP-PATHNAME 
                   PERFORM 900-SYSBASE-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYSBASE
               END-IF
               MOVE    ZERO                TO  IDX
               PERFORM UNTIL  (FLG-SYSBASE =   1 )
                         OR   (IDX         >=  9 )
                   IF     (SYSBASE-KIGEN   >=  SPA-SYSYMD )
                   AND    (SYSBASE-HOSPNUM NOT =   SPA-HOSPNUM)
                     ADD     1               TO  IDX
                     MOVE    SYSBASE-HOSPNUM TO  SPA-GRPHOSPNUM (IDX)
                     PERFORM VARYING  IDY  FROM  1  BY  1
                             UNTIL   (IDY  >   9)
                       IF    SYS-1010-GRPHOSPNUM(IDY)  IS NUMERIC
                         IF   (SYSBASE-HOSPNUM    =
                                              SYS-1010-GRPHOSPNUM(IDY))
                         AND  (SYS-1010-GRPHOSPUSERID(IDY)
                                               NOT =   SPACE)
                           MOVE    SYS-1010-GRPHOSPUSERID(IDY)
                                                  TO  SPA-GRPHOSPUSERID
                                                                  (IDX)
                           MOVE    9               TO  IDY
                         END-IF
                       ELSE
                           MOVE    SPACE           TO  SPA-GRPHOSPUSERID
                                                                  (IDX)
                       END-IF
                     END-PERFORM
                   END-IF
                   MOVE    "tbl_sysbase"   TO  MCP-TABLE 
                   MOVE    "key2"          TO  MCP-PATHNAME 
                   PERFORM 900-SYSBASE-READ-SEC
               END-PERFORM
               MOVE    "tbl_sysbase"       TO  MCP-TABLE 
               MOVE    "key2"              TO  MCP-PATHNAME 
               PERFORM 990-DBCLOSE-SEC
      *
               IF      IDX             >   ZERO
                   MOVE    1           TO  SPA-GRPHOSPKBN
               END-IF
           END-IF
      *
      *    医療機関名称編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )
                         OR   (SPA-GRPHOSPNUM(IDX) =   ZERO )
                         OR   (SPA-ERRCD       NOT =   SPACE )
               MOVE    SPACE               TO  SYS-1001-REC
               INITIALIZE                  SYS-1001-REC
               MOVE    "1001"              TO  SYS-1001-KANRICD
               MOVE    "*"                 TO  SYS-1001-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
               MOVE    SPA-GRPHOSPNUM (IDX)
                                           TO  SYS-1001-HOSPNUM
               MOVE    SYS-1001-REC        TO  SYSKANRI-REC
               PERFORM 900-SYSKANRI-KEY10-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1001-REC
                   MOVE    SYS-1001-TANHOSPNAME
                                               TO  SPA-GRPHOSPNAME(IDX)
                   IF      SYS-1001-TANHOSPNAME    =   SPACE
                       MOVE    SYS-1001-HOSPNAME
                                               TO  SPA-GRPHOSPNAME(IDX)
                   END-IF
               ELSE
                   MOVE    "1015"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           .
       1007-GRPHOSPNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    DBエンコーディング取得処理
      *****************************************************************
       1008-DBCATLG-GET-SEC        SECTION.
      *
      *    エンコーディング取得処理
           CALL    "ORCSENCODING"      USING    MCPAREA
                                                SPA-AREA
      *
           .
       1008-DBCATLG-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務メニューＳＰＡ設定処理
      *****************************************************************
       200-M01-SPAHEN-SEC       SECTION.
      *
      *    職員情報編集
           PERFORM 2002-SYS1010-SPA02-SEC
      *
      *    検査正式名称表示
           MOVE    SYS-1010-FORMALFLG  TO  SPA-FORMALFLG
           IF      SYS-1010-FORMALFLG  =   SPACE
               PERFORM 2001-SYS1038-SPA-SEC
           END-IF
           IF      SPA-FORMALFLG       =   SPACE
               MOVE    "0"             TO  SPA-FORMALFLG
           END-IF
           .
       200-M01-SPAHEN-EXT.
           EXIT.
      *****************************************************************
      *    職員情報編集処理
      *****************************************************************
       2002-SYS1010-SPA02-SEC          SECTION.
      *
      *    職員情報
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
      *    職員コード
           MOVE    SPA-STAFFCD         TO  SYS-1010-KBNCD(1:5)
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *2001.9.27 強制プロテクト（ＰＧ未作成）
      *
      **** MOVE    "0"                 TO  SYS-1010-GSRAUTH(3:1)
           MOVE    "0"                 TO  SYS-1010-GSRAUTH(4:1)
                                           SYS-1010-GSRAUTH(46:1)
      *2001.9.27 強制プロテクト（ＰＧ未作成）END
      *業務処理権限設定
      *    受付
           MOVE    SYS-1010-GSRAUTH(16:1)  TO  SPA-GSRAUTH-11
      *    患者登録
           MOVE    SYS-1010-GSRAUTH(17:1)  TO  SPA-GSRAUTH-12
      *    予約登録
           MOVE    SYS-1010-GSRAUTH(19:1)  TO  SPA-GSRAUTH-14
      *    診療行為
           MOVE    SYS-1010-GSRAUTH(21:1)  TO  SPA-GSRAUTH-21
      *    病名登録
           MOVE    SYS-1010-GSRAUTH(22:1)  TO  SPA-GSRAUTH-22
      *    収納登録
           MOVE    SYS-1010-GSRAUTH(23:1)  TO  SPA-GSRAUTH-23
      *    会計照会
           MOVE    SYS-1010-GSRAUTH(24:1)  TO  SPA-GSRAUTH-24
      *    入退院登録
           MOVE    SYS-1010-GSRAUTH(36:1)  TO  SPA-GSRAUTH-31
      *    入院会計照会
           MOVE    SYS-1010-GSRAUTH(37:1)  TO  SPA-GSRAUTH-32
      *
           MOVE    SYS-1010-REC        TO  LNK-SYS-1010-REC
      *
           .
       2002-SYS1010-SPA02-EXT.
           EXIT.
      *****************************************************************
      *    検査正式名称表示処理
      *****************************************************************
       2001-SYS1038-SPA-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
      *        検査正式名称表示
               MOVE    SYS-1038-FORMALFLG  TO  SPA-FORMALFLG
           END-IF
           .
       2001-SYS1038-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療番号変更処理
      *****************************************************************
       300-HOSPNUM-CHANGE-SEC       SECTION.
      *
      *!!  ORCSCOMMON-HOSPNUM と SPA-STAFFCD から 
      *!!  ORCSCOMMON-HOSPNUM の MCP-USER を 決定する
      *
      *****INITIALIZE                      SPA-AREA
      *    システム日付取得
           PERFORM 3001-SYSYMD-SET-SEC
           MOVE    ORCSCOMMON-HOSPNUM  TO  SPA-HOSPNUM
      *    SYSUSER から スーパーバイザー
           PERFORM 3002-SUPERVISOR-SET-SEC
      *
      *    マスターメニュウー初期設定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 100-SPA-INIT-SEC
           END-IF
      *    業務ニュウー初期設定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 200-M01-SPAHEN-SEC
           END-IF
           .
       300-HOSPNUM-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム日付取得処理
      *****************************************************************
       3001-SYSYMD-SET-SEC       SECTION.
      *
      *    システム日付セット
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
           IF      STS-DAY-RC1         =   ZERO 
               MOVE    LNK-DAY1-YMD (2:7)  TO  SPA-SYSYMDW
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYSYMD          TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               IF      STS-DAY-RC1         =   ZERO 
                   MOVE    LNK-DAY2-EDTYMD1    TO  SPA-SYSYMDWH
                   MOVE    WRK-SYSYMD          TO  SPA-SYSYMD
               END-IF
           END-IF
           IF      STS-DAY-RC1     NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
           END-IF
      *    曜日指定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYSYMD          TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY3-YOBI       TO  SPA-SYSYMD-YOBI
           END-IF
           .
       3001-SYSYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関識別番号設定処理
      *****************************************************************
       3002-SUPERVISOR-SET-SEC       SECTION.
      *
           INITIALIZE                      SYSUSER-REC
           MOVE    SPA-OPID            TO  SYSUSER-USERID
           MOVE    SYSUSER-REC         TO  MCPDATA-REC
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sysuser"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-SYSUSER-READ-SEC
           ELSE
               INITIALIZE                  SYSUSER-REC
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           MOVE    "tbl_sysuser"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSUSER         =   ZERO
      *        医療機関識別番号
               IF      SYSUSER-HOSPNUM     =   SPA-HOSPNUM
      *            スーパーバイザー
                   MOVE    SYSUSER-SUPERVISOR  TO  SPA-SUPERVISOR
               END-IF
           ELSE
               MOVE    "1002"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    SPACE               TO  SPA-NOCHK-HOSPNUM
      *
           .
       3002-SUPERVISOR-SET-EXT.
           EXIT.
      *****************************************************************
      *    システム基本チェック処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEC     SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    システム基本チェック処理
      *****************************************************************
       910-SYSKANRI-READ-SEC       SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSBASE-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
           .
       900-SYSBASE-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム基本読込処理
      *****************************************************************
       900-SYSUSER-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSUSER-REC
               MOVE    ZERO            TO  FLG-SYSUSER
           ELSE
               MOVE    1               TO  FLG-SYSUSER
           END-IF
           .
       900-SYSUSER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルFETCH処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       910-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
