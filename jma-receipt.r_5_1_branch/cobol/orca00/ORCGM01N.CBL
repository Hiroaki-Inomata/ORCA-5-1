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
       PROGRAM-ID.             ORCGM01N.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : 業務選択（Ｍ０１Ｎ）
      *  管理者            : 
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/06/15  44:総括表追加
      *  01.00.02    MCC-藤原     01/07/24  43:請求管理追加
      *  01.00.03    MCC-多々納   01/08/23  K01 -> K02       
      *  01.00.04    MCC-藤原     01/09/12  緊急情報、センター情報の
      *                                     ボタン削除
      *  01.01.01    MCC-多々納   01/09/20  JOINの修正      
      *  01.01.02    MCC-藤原     01/09/27  MVERの追加  
      *  01.01.03    MCC-藤原     01/11/05  C01 -> C02,S01 -> S02,
      *                                     J01 -> J02
      *  01.01.04    MCC-多々納   01/11/27  待ち受けＰＧ追加
      *  01.02.01    MCC-藤原     01/11/21  ORCMCPSUBへ変更  
      *  01.02.01    MCC-多々納   01/12/11  P01→P02 へ変更
      *                                     非活性の変更  
      *  01.02.03    MCC-多々納   02/02/13  デモ用項目の削除
      *  01.02.04    MCC-多々納   02/02/21  マスタ更新の追加
      *  01.02.05    MCC-藤原     02/03/11  月次統計の追加
      *  01.02.06    ひさなが     02/04/05  管理情報画面の追加
      *  01.02.07    NACL-小豆沢  02/10/07  入院対応の追加
      *  01.02.08    NACL-森脇    02/11/08  入院対応のボタン活性非活性
      *  01.02.09    NACL-多々納  02/11/11  SPA-MOTOPGをM01へ変更
      *  01.02.10    NACL-多々納  04/02/02  環境設定と外来時間外区分追加
      *  01.02.11    NACL-伊藤    04/02/05  M97の追加
      *  01.02.12    NACL-伊藤    06/02/22  MONFUNC 対応
      *  03.04.00    NACL-多々納  06/11/13  検査正式名称表示フラグ追加
      *  03.05.00    NACL-多々納  07/05/25  グループ診療対応
      *  04.04.00    NACL-森脇    08/11/20  印刷削除ボタン追加
      *  04.04.00    NACL-森脇    09/01/27  排他ＳＰＡ共通対応
      *  04.07.00    NACL-森脇    12/07/04  ユーザーＩＤ表示
      *  04.07.00    NACL-太田    13/07/16  電子お薬手帳対応
      *  04.08.00    NACL-多々納  15/05/12  背景色設定対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    プログラム更新　日レセ再起動要求メッセージ
           SELECT  RESTARTMSG-FILE
                   ASSIGN  "/tmp/jma-receipt/patch-restart-message"
                   FILE    STATUS  IS  STS-RESTARTMSG.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       FD  RESTARTMSG-FILE.
       01  RESTARTMSG-REC          PIC X(01). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M00COMMON-SPA".
      *    スパ領域
           COPY    "M01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    マスタ更新シェル領域 
           COPY    "CPCOMMONSHELLMST.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  STS-AREA.
           03  STS-RESTARTMSG      PIC X(02).
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
      *
      *    一時領域
       01  WRK-AREA.
           03  IDX                   PIC 9(03).
           03  IDJ                   PIC 9(03).
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
           03  WRK-GNO             PIC X(02).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-PRGNAME         PIC X(08).
      *
           03  WRK-MIDMSG          PIC X(80).
      *
           03  PRO-ID              PIC X(10).
           03  PID                 PIC X(10).
      *
       01  WRK-JOBKANRI-AREA.
           03  WRK-JOBKANRI-MODE   PIC X(03). 
           03  WRK-JOBKANRI-RETURN PIC S9(03).
      *
       01  CONST-AREA.
           03  CONST-HTML-PANDA    PIC X(200) VALUE
               "http://www.orca.med.or.jp/info/index.html".
           03  CONST-HTML-GINBEE   PIC X(200) VALUE
               "http://www.orca.med.or.jp/info_cloud/index.html".
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
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
           COPY  "CPSK1038.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSCOLCHK.INC".
      *
      *    COMMON-SPA 初期編集
           COPY    "CPORCSCOMMON.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
       01  ST                      PIC S9(2).
       01  ORCNET-AREA1.
           03   ACC1-KBN           PIC  X(02).
           03   ACC1-ST            PIC  X(02).
           03   ACC1-FILENAME      PIC  X(128).
      *
      *   排他制御 
           COPY    "CPORCSLOCK.INC".
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *****COPY    "MCPAREA.INC".
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-SYS-1010-REC    TO  SYS-1010-REC
           MOVE    SPA-FREE        TO  SPA-M01
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
           MOVE    SYS-1010-REC    TO  SPA-SYS-1010-REC
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M01        TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    3                   TO  SLOCK-MODE
           MOVE    ZERO                TO  SLOCK-PTID
           CALL "ORCSLOCK"             USING   MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN        NOT =   ZERO
               MOVE    "9999"          TO  SPA-ERRCD
           END-IF
      *
      ***  MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "M01N   "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-M01-AREA
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MID1"
                   PERFORM 3001-MID1-SYORI-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 320-SYS-1010-SPASET-SEC
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       3001-MID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-MIDCD
               WHEN    "0001"
                   IF      SPA-MID1-FLG        =   "OK"
      *                印刷削除処理へ
                       PERFORM 30012-PRTDEL-SYORI-SEC
                   END-IF
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   MOVE    SPACE           TO  SPA-MIDCD
               WHEN    "0101"
                   IF      SPA-MID1-FLG        =   "OK"
      *                マスタ更新処理へ
                       PERFORM 30011-MSTMNT-SYORI-SEC
                   END-IF
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   MOVE    SPACE           TO  SPA-MIDCD
      *            マスタ更新異常終了
               WHEN    "0102"
      *
      *            ジョブ管理削除
                   MOVE    "DEL"               TO  WRK-JOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1                   TO  JOB-JOBID
                   MOVE    "MSTMNT"            TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"           USING
                                               WRK-JOBKANRI-AREA
                                               JOBKANRI-REC
                                               SPA-AREA
      *
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   MOVE    SPACE           TO  SPA-MIDCD
      *        マスタ更新正常終了
               WHEN    "0104"
               WHEN    "0103"
      *
      *            ジョブ管理削除
                   MOVE    "DEL"               TO  WRK-JOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1                   TO  JOB-JOBID
                   MOVE    "MSTMNT"            TO  JOB-SHELLID
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"           USING
                                               WRK-JOBKANRI-AREA
                                               JOBKANRI-REC
                                               SPA-AREA
      *
                   MOVE    1               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-MID1-FLG
                   MOVE    SPACE           TO  SPA-MIDCD
           END-EVALUATE
           .
      *
       3001-MID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ更新処理
      *****************************************************************
       30011-MSTMNT-SYORI-SEC          SECTION.
      *
      *     ジョブ管理削除
           MOVE    "DEL"               TO  WRK-JOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1                   TO  JOB-JOBID
           MOVE    "MSTMNT"            TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       WRK-JOBKANRI-AREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *    ジョブ管理作成
           MOVE    "JBS"               TO  WRK-JOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1                   TO  JOB-JOBID
           MOVE    "MSTMNT"            TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       WRK-JOBKANRI-AREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
      *    マスタ更新処理
           MOVE    "master_upgrade.sh" TO  SHELL-SHELL
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    SHELL-NAME          TO  SHELLTBL-NAME
           MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
           MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
           MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
           MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
           MOVE    SHELLTBL            TO  MCPDATA-REC
      *
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       30011-MSTMNT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷削除処理
      *****************************************************************
       30012-PRTDEL-SYORI-SEC          SECTION.
      *
      *    印刷削除処理
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "orc_cups_del.sh"   TO  SHELLTBL-NAME
           MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
           MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
           MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
           MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
           MOVE    SHELLTBL            TO  MCPDATA-REC
      *
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
      *
       30012-PRTDEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-MID1-FLG
           MOVE    SPACE           TO  SPA-MIDCD
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       320-SYS-1010-SPASET-SEC              SECTION.
      *
      *!!
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M01"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
      *H27.5
      *    背景色設定
           PERFORM 3102-BGCOLORS-INIT-SEC
      *
      *!!
      *    職員情報
      *    MOVE    SPACE               TO  SYS-1010-REC
      *    INITIALIZE                  SYS-1010-REC
      *    MOVE    "1010"              TO  SYS-1010-KANRICD
      *    STRING  SPA-OPID            DELIMITED  BY  SPACE
      *            "%"                 DELIMITED  BY  SIZE
      *                                INTO    SYS-1010-TBL
      *    END-STRING
      *    MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
      *    MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
      *    MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    MOVE    SYS-1010-REC        TO  MCPDATA-REC
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key3"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *        MOVE    "key3"          TO  MCP-PATHNAME
      *        PERFORM 900-SYSKANRI-READ-SEC
      *        IF      FLG-SYSKANRI    =   ZERO
      *            MOVE    MCPDATA-REC     TO  SYS-1010-REC
      *        ELSE
      *            INITIALIZE              SYS-1010-REC
      *        END-IF
      *    ELSE
      *        INITIALIZE                  SYS-1010-REC
      *        MOVE    1                   TO  FLG-SYSKANRI
      *    END-IF
      *    MOVE    ZERO                TO  FLG-OK
      *    PERFORM UNTIL       FLG-SYSKANRI    =   1
      *        MOVE    MCPDATA-REC     TO  SYS-1010-REC
      *        IF      SPA-OPID            =   SYS-1010-USERID
      *            MOVE    1                   TO  FLG-OK
      *            MOVE    1                   TO  FLG-SYSKANRI
      *        ELSE
      *            MOVE    "tbl_syskanri"  TO  MCP-TABLE
      *            MOVE    "key3"          TO  MCP-PATHNAME
      *            PERFORM 900-SYSKANRI-READ-SEC
      *        END-IF
      *    END-PERFORM
      *    MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *    MOVE    "key3"              TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *    職員情報なし
      *    IF     FLG-OK               =   ZERO
      *        INITIALIZE                  SYS-1010-REC
      *    END-IF
      *
      *2001.9.27 強制プロテクト（ＰＧ未作成）
      *    MOVE    "0"                 TO  SYS-1010-GSRAUTH(3:1)
      *                                    SYS-1010-GSRAUTH(4:1)
      *************************************SYS-1010-GSRAUTH(18:1)
      *************************************SYS-1010-GSRAUTH(32:1)
      *************************************SYS-1010-GSRAUTH(41:1)
      *                                    SYS-1010-GSRAUTH(46:1)
      ************************************ SYS-1010-GSRAUTH(47:1)
      *2001.9.27 強制プロテクト（ＰＧ未作成）END
      *
      *****MOVE    SYS-1010-REC         TO  SPA-WORK
      *
      *業務処理権限設定
      *    受付
      *    MOVE    SYS-1010-GSRAUTH(16:1)  TO  SPA-GSRAUTH-11
      *    患者登録
      *    MOVE    SYS-1010-GSRAUTH(17:1)  TO  SPA-GSRAUTH-12
      *    予約登録
      *    MOVE    SYS-1010-GSRAUTH(19:1)  TO  SPA-GSRAUTH-14
      *    診療行為
      *    MOVE    SYS-1010-GSRAUTH(21:1)  TO  SPA-GSRAUTH-21
      *    病名登録
      *    MOVE    SYS-1010-GSRAUTH(22:1)  TO  SPA-GSRAUTH-22
      *    収納登録
      *    MOVE    SYS-1010-GSRAUTH(23:1)  TO  SPA-GSRAUTH-23
      *    会計照会
      *    MOVE    SYS-1010-GSRAUTH(24:1)  TO  SPA-GSRAUTH-24
      *    入退院登録
      *    MOVE    SYS-1010-GSRAUTH(36:1)  TO  SPA-GSRAUTH-31
      *    入院会計照会
      *    MOVE    SYS-1010-GSRAUTH(37:1)  TO  SPA-GSRAUTH-32
      *
      *    検査正式名称表示
      *    MOVE    SYS-1010-FORMALFLG  TO  SPA-FORMALFLG
      *    IF      SYS-1010-FORMALFLG  =   SPACE
      *        PERFORM 3201-SYS-1038-SET-SEC
      *    END-IF
      *    IF      SPA-FORMALFLG       =   SPACE
      *        MOVE    "0"             TO  SPA-FORMALFLG
      **** END-IF
      *
      *    プログラム更新日レセ再起動要求メッセージ
           MOVE    SPACE               TO  STS-RESTARTMSG
           OPEN    INPUT   RESTARTMSG-FILE
           IF      STS-RESTARTMSG      =   ZERO
                   CLOSE   RESTARTMSG-FILE
                   MOVE    ALL "0"         TO  SYS-1010-GSRAUTH
                   MOVE    "9991"          TO  SPA-ERRCD
                   PERFORM 500-SET-SCREEN
           END-IF
           .
       320-SYS-1010-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    背景色設定処理
      *****************************************************************
       3102-BGCOLORS-INIT-SEC       SECTION.
      *
      *    システム日付決定
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                      SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    WRK-SYSYMD          TO  SYS-1038-STYUKYMD
           MOVE    WRK-SYSYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1038-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1038-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
      *        背景色設定
               PERFORM 31021-BGCOLORS-HEN-SEC
           END-IF
           .
       3102-BGCOLORS-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    背景色設定処理
      *****************************************************************
       31021-BGCOLORS-HEN-SEC       SECTION.
      *
      *    時間内（標準
           MOVE    SYS-1038-BGCOLORSJ0 TO  SPA-M00-BGCOLORS
      *
           IF      WRK-SYSYMD          =   SPA-SYSYMD
      *    時間外
               IF      SPA-GAI-JIKANKBN    NOT =   "0"
                   MOVE    SPA-GAI-JIKANKBN        TO  IDJ
                   IF     (IDJ                 >=  1   AND   <=  8 )
                       MOVE    SYS-1038-BGCOLORSJ (IDJ)
                                               TO  SPA-M00-BGCOLORS
                   END-IF
               END-IF
           ELSE
      *    日付変更
               MOVE    SYS-1038-BGCOLORSDAY    TO  SPA-M00-BGCOLORS
           END-IF
           .
       31021-BGCOLORS-HEN-EXT.
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
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    システム日付設定
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-M95-SEC
      *    オンライン再印刷
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 470-REPRINT-SEC
      *    印刷削除
               WHEN    "CLICKED"       ALSO    "B07"
                   MOVE    "0001"      TO  SPA-MIDCD
      *    電子版手帳画面
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 470-MNOTE-SEC
      *    各業務移動へ
               WHEN    "CLICKED"       ALSO    ANY
                   PERFORM 400-GYOMU-SEL-SEC
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4101-KEY-ENT-SEC
           END-EVALUATE
      *
          .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       4101-KEY-ENT-SEC            SECTION.
      *
           MOVE    M01N-SELNUM        TO  SPA-GMN-GSEL
      *
           IF      SPA-GMN-GSEL    NOT =   ZERO
               IF      SYS-1010-GSRAUTH(1:1)     =  "0"
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-GSEL        TO  WRK-GNO
                   PERFORM 420-GYOMU-START-SEC
               END-IF
           END-IF
      *
           .
       4101-KEY-ENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力　処理
      *****************************************************************
       400-GYOMU-SEL-SEC            SECTION.
      *
           IF      MCP-WIDGET(1:1)     =   "G"
               MOVE   MCP-WIDGET(2:2)      TO  WRK-GNO
               PERFORM 420-GYOMU-START-SEC
           END-IF
      *
           .
       400-GYOMU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務起動　処理
      *****************************************************************
       420-GYOMU-START-SEC         SECTION.
      *
          MOVE    SPACE               TO  SPA-SAKIPG
          MOVE    SPACE               TO  SPA-MACHIPG
          EVALUATE    WRK-GNO
      * 受付
               WHEN    "11"
                   IF      SYS-1010-GSRAUTH(16:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "U02"           TO  SPA-SAKIPG
                       MOVE    "U02"           TO  SPA-MACHIPG
                   END-IF
      * 患者登録
               WHEN    "12"
                   IF      SYS-1010-GSRAUTH(17:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "P02"           TO  SPA-SAKIPG
                   END-IF
      * 患者照会
               WHEN    "13"
                   IF      SYS-1010-GSRAUTH(18:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "Q01"          TO  SPA-SAKIPG
                   END-IF
      * 予約登録
               WHEN    "14"
                   IF      SYS-1010-GSRAUTH(19:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "Y01"           TO  SPA-SAKIPG
                   END-IF 
      * 診療行為入力
               WHEN    "21"
                   IF      SYS-1010-GSRAUTH(21:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      *                MOVE    "K01"           TO  SPA-SAKIPG
                       MOVE    "K02"           TO  SPA-SAKIPG
                       MOVE    "K02"           TO  SPA-MACHIPG
                   END-IF
      * 病名登録
               WHEN    "22"
                   IF      SYS-1010-GSRAUTH(22:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      ********         MOVE    "C01"           TO  SPA-SAKIPG
                       MOVE    "C02"           TO  SPA-SAKIPG
                   END-IF 
      * 収納登録
               WHEN    "23"
                   IF      SYS-1010-GSRAUTH(23:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      ********         MOVE    "S01"           TO  SPA-SAKIPG
                       MOVE    "S02"           TO  SPA-SAKIPG
                   END-IF
      * 会計照会
               WHEN    "24"
                   IF      SYS-1010-GSRAUTH(24:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      ********         MOVE    "J01"           TO  SPA-SAKIPG
                       MOVE    "J02"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.07) LINE ADD START ----------------------------------
      *----(01.02.08) LINE ADD START ----------------------------------
      * 入退院登録
               WHEN    "31"
                   IF      SYS-1010-GSRAUTH(36:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "I01"           TO  SPA-SAKIPG
                   END-IF
      * 入院会計照会
               WHEN    "32"
                   IF      SYS-1010-GSRAUTH(37:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "I41"           TO  SPA-SAKIPG
                   END-IF
      * 入院定期請求
               WHEN    "33"
                   IF      SYS-1010-GSRAUTH(38:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "O01"           TO  SPA-SAKIPG
                   END-IF
      * 退院時仮計算
               WHEN    "34"
                   IF      SYS-1010-GSRAUTH(39:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "I51"           TO  SPA-SAKIPG
                   END-IF
      * 病床管理
      *         WHEN    "35"
      *             IF      SYS-1010-GSRAUTH(56:1)    =  "0"
      *                 MOVE    "0002"          TO  SPA-ERRCD
      *             ELSE
      *                 MOVE    "O01"           TO  SPA-SAKIPG
      *             END-IF
      * 入院定期請求
               WHEN    "36"
                   IF      SYS-1010-GSRAUTH(57:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "I20"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.07) LINE ADD END ----------------------------------
      *----(01.02.08) LINE ADD END ----------------------------------
      * データチェック
               WHEN    "41"
                   IF      SYS-1010-GSRAUTH(26:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "D01"           TO  SPA-SAKIPG
                   END-IF
      * 明細書
               WHEN    "42"
                   IF      SYS-1010-GSRAUTH(27:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "R03"           TO  SPA-SAKIPG
                   END-IF
      * 請求管理
               WHEN    "43"
                   IF      SYS-1010-GSRAUTH(28:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      *----(01.00.02) LINE MOD START ----------------------------------
      *******          MOVE    "M43"           TO  SPA-SAKIPG
                       MOVE    "H01"           TO  SPA-SAKIPG
      *----(01.00.02) LINE MOD END   ----------------------------------
                   END-IF
      * 総括表
               WHEN    "44"
                   IF      SYS-1010-GSRAUTH(29:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      * 
      *----(01.00.01) LINE MOD START ----------------------------------
      *******          MOVE    "M44"           TO  SPA-SAKIPG
                       MOVE    "T01"           TO  SPA-SAKIPG
      *----(01.00.01) LINE MOD END   ----------------------------------
                   END-IF
      * 日次統計
               WHEN    "51"
                   IF      SYS-1010-GSRAUTH(31:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      *****************MOVE    "M51"           TO  SPA-SAKIPG
                       MOVE    "L01"           TO  SPA-SAKIPG
                  END-IF
      * 月次統計
               WHEN    "52"
                   IF      SYS-1010-GSRAUTH(32:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      *****************MOVE    "M52"           TO  SPA-SAKIPG
                       MOVE    "G01"           TO  SPA-SAKIPG
                   END-IF
      *----(01.00.04) LINE DEL START ----------------------------------
      * センター情報
      *****         WHEN    "61"
      *****             IF      SYS-1010-GSRAUTH(36:1)    =  "0"
      *****                 MOVE    "0002"          TO  SPA-ERRCD
      *****             ELSE
      *****                 MOVE    "M61"           TO  SPA-SAKIPG
      *****             END-IF
      *----(01.00.04) LINE DEL END   ----------------------------------
      * データ出力
               WHEN    "71"
                 IF      SPA-MW          NOT =   SPA-PANDA
                       MOVE    "0001"          TO  SPA-ERRCD
                 ELSE
                   IF      SYS-1010-GSRAUTH(41:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "E01"           TO  SPA-SAKIPG
                   END-IF
                 END-IF
      * センターバックアップ
               WHEN    "81"
                   IF      SYS-1010-GSRAUTH(46:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "M81"           TO  SPA-SAKIPG
                   END-IF
      * 外部媒体
               WHEN    "82"
                 IF      SPA-MW          NOT =   SPA-PANDA
                       MOVE    "0001"          TO  SPA-ERRCD
                 ELSE
                   IF      SYS-1010-GSRAUTH(47:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      *                グループ診療の場合
                       IF      SPA-GRPHOSPNUM(2)   NOT =   SPACE
      *                    スーパーバイザーであれば活性
                           IF      SPA-SUPERVISOR      =   "T"
                               MOVE    "F01"           TO  SPA-SAKIPG
                           ELSE
                               MOVE    "0002"          TO  SPA-ERRCD
                           END-IF
                       ELSE
                           MOVE    "F01"           TO  SPA-SAKIPG
                       END-IF
                   END-IF
                 END-IF
      * マスタ登録処理
               WHEN    "91"
                   IF      SYS-1010-GSRAUTH(51:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "M02"           TO  SPA-SAKIPG
                   END-IF
      * マスタ更新処理
               WHEN    "92"
                   IF      SYS-1010-GSRAUTH(52:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
      *                管理者権限フラグ
                       IF      SYS-1010-KANRIAUTHFLG  =   "T"
      *修正*************************************************************
      *                     PERFORM 4201-GYOUMU92-SYORI-SEC
      *                     GO      TO      420-GYOMU-START-EXT
                           MOVE    "M96"           TO  SPA-SAKIPG
      *修正*************************************************************
                       ELSE
                           MOVE    "0002"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *----(01.00.04) LINE DEL START ----------------------------------
      * 緊急情報処理
      ***         WHEN    "I1"
      ****    DISPLAY "START"
      ****             MOVE    "I01"           TO  SPA-SAKIPG
      ****    ETC-EXEC SHELL  "netscape-communicator ../html/jmari/index.html" 0
      ****        GO  TO  420-GYOMU-START-EXT
      *----(01.00.04) LINE MOD START ----------------------------------
               WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE.
      *
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPACE               TO  SPA-MOTOPG2
               MOVE    "M01"               TO  SPA-MOTOPG
               IF      SPA-SAKIPG      NOT =  SPACE
                   EVALUATE    SPA-SAKIPG
                       WHEN    "M02"
                           CONTINUE
                       WHEN    OTHER
                           INITIALIZE                  LINKAREA
                           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
                   END-EVALUATE
      *
      *修正*************************************************************
                   MOVE     "CHANGE"           TO  MCP-PUTTYPE
      *             IF  SPA-SAKIPG   =   "M96"
      **
      *                 MOVE    SPA-M00KYOUTU       TO  SPA-WORK
      *                 MOVE    SPA-AREA            TO  SPA-COMMON
      **
      *                 MOVE     "LINK"             TO  MCP-STATUS
      *                 MOVE     SPACE              TO  MCP-EVENT
      *                 CALL     "ORCGM96"          USING
      *                                             MCPAREA
      *                                             SPAAREA
      *                                             LINKAREA
      *                                             SCRAREA
      *                 MOVE    SPA-FREE            TO  SPA-M01
      **
      *                 MOVE    SPA-COMMON          TO  SPA-AREA
      *                 MOVE    SPA-WORK            TO  SPA-M00KYOUTU
      **
      *                 MOVE    "M96"               TO  SPA-MOTOPG2
      *                 MOVE     "NEW"              TO  MCP-PUTTYPE
      *             ELSE
      *                 MOVE     "CHANGE"           TO  MCP-PUTTYPE
      *             END-IF
      *修正*************************************************************
                   MOVE     SPA-SAKIPG         TO  MCP-WINDOW
      *
                   PERFORM 900-PUT-WINDOW
      *
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
      *
       420-GYOMU-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ更新処理
      *****************************************************************
       4201-GYOUMU92-SYORI-SEC            SECTION.
      *
      *    ジョブ管理チェック処理
           MOVE    "CHK"           TO  WRK-JOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "MSTMNT"        TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   WRK-JOBKANRI-AREA
                                   JOBKANRI-REC 
                                   SPA-AREA
           IF      WRK-JOBKANRI-RETURN
                                       =   ZERO
      *        バッチ処理中にエラーの時
               IF      JOB-ERRCD(1:2)      =   "99"
                   MOVE    "0102"          TO  SPA-MIDCD
                   GO  TO  4201-GYOUMU92-SYORIT-EXT
               END-IF
               IF      JOB-ERRCD(1:2)      =   "01"
                   MOVE    "0104"          TO  SPA-MIDCD
                   GO  TO  4201-GYOUMU92-SYORIT-EXT
               END-IF
               IF      JOB-ENDYMD          =   SPACE
                   MOVE    "0092"          TO  SPA-ERRCD
                   GO  TO  4201-GYOUMU92-SYORIT-EXT
               END-IF
               IF      JOB-ENDYMD      NOT =   SPACE
                   MOVE    "0103"          TO  SPA-MIDCD
                   GO  TO  4201-GYOUMU92-SYORIT-EXT
               END-IF
           END-IF
      *
      *    確認画面へ
           MOVE    "0101"          TO  SPA-MIDCD
           .
      *
       4201-GYOUMU92-SYORIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    定点調査研究事業 収集データ送信チェック
           INITIALIZE                  ORCSCOLCHKAREA
           CALL    "ORCSCOLCHK"        USING
                                           ORCSCOLCHKAREA
                                           SPA-AREA
      *
           IF      COLCHK-RC           =   30
	           PERFORM 490-TEITEN-SEC
	   ELSE
      *
               MOVE    "M00"               TO  SPA-SAKIPG
               MOVE    "M01N"              TO  SPA-MOTOPG
      *
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
               MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
	   END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M01"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *    戻り時、画面を元へ戻す
           MOVE    SPA-M01             TO  LNK-FREE
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M01"              TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       470-REPRINT-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M01"              TO  SPA-MOTOPG
           MOVE    "XA02"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    "ONLINE"
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "XA02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-REPRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電子版手帳画面へ
      *****************************************************************
       470-MNOTE-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M01"               TO  SPA-MOTOPG
           MOVE    "XG03"              TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "XG03"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-MNOTE-EXT.
           EXIT.
      *****************************************************************
      *    システム日付変更画面へ1
      *****************************************************************
       460-M95-SEC                    SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-SAKIPG
           MOVE    SPA-M00KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGM95"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-M00KYOUTU
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "M01N"              TO  SPA-MOTOPG
           MOVE    "M95"               TO  SPA-SAKIPG
           MOVE    "M01N"              TO  SPA-M95-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "M95"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
      *
           .
       460-M95-EXT.
           EXIT.
      *
      *****************************************************************
      *    定点調査確認画面へ
      *****************************************************************
       490-TEITEN-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M01N"              TO  SPA-MOTOPG
           MOVE    "T91"               TO  SPA-SAKIPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *    戻り時、画面を元へ戻す
           MOVE    SPA-M01             TO  LNK-FREE
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "T91    "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       490-TEITEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-MIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "M01N   "           TO  MCP-WINDOW.
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
      *****MOVE    SPA-WORK            TO  SYS-1010-REC
      *
           INITIALIZE                  M01N
      *
      *----(04.07.00) LINE UPD START ----------------------------------
      *     MOVE    SPA-HOSPNAME        TO  M01N-TITLE
           MOVE    SPACE           TO  M01N-TITLE
           STRING  SPA-HOSPNAME        DELIMITED BY SPACE
                   "  ["               DELIMITED BY SIZE
                   SPA-OPID            DELIMITED BY SPACE
                   "]"                 DELIMITED BY SIZE
                                 INTO  M01N-TITLE
           END-STRING
      *----(04.07.00) LINE UPD END   ----------------------------------
           MOVE    SPA-GMN-GSEL        TO  M01N-SELNUM
      *
      *ver.4.7
           MOVE    SPA-M00-BGCOLORS    TO  M01N-BGCOLORS
      *----(01.00.04) LINE DEL START ----------------------------------
      ***     IF  ACC1-ST =  "01"
      ***         MOVE    "既に起動済み！"    TO  M01N-ALERT
      ***     ELSE
      ***         MOVE    "未読緊急情報あり！"    TO  M01N-ALERT
      ***     END-IF
      *----(01.00.04) LINE DEL START ----------------------------------
      *
           MOVE    "電子版手帳"   TO  M01N-B08-LABEL
      *
           MOVE    SPACE               TO  M01N-HOSPID
           IF      SPA-MW              =   SPA-PANDA
               MOVE    SPA-HOSPID          TO  M01N-HOSPID
               IF      SPA-NAI-CONTRACT-COND   =   1   OR  2
                   IF      SPA-NAI-KADO-LEVEL      =   5
                       MOVE    "教育版"            TO  M01N-CONTRACTMSG
                   ELSE
                       MOVE    "商用版"            TO  M01N-CONTRACTMSG
                   END-IF
                   MOVE    "red"               TO
                                               M01N-CONTRACTMSG-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務権限非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *----(01.02.07) LINE UPD START ----------------------------------
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       24 
               MOVE    WIDGET-INSENSITIVE      TO  M01N-STATE-O (IDX)
               MOVE    "T"                     TO  M01N-VISIBLE-O (IDX)
           END-PERFORM
      *----(01.02.07) LINE UPD END   ----------------------------------
      *
           MOVE    "T"                         TO  M01N-FR6-VISIBLE
      *
           IF      SYS-1010-GSRAUTH(1:1)    =  "0"
               CONTINUE
           ELSE
      *----(01.00.04) LINE DEL START ----------------------------------
      ***         IF      SYS-1010-GSRAUTH(11:1)   =  "1"
      ***             MOVE    WIDGET-NORMAL      TO  M01N-I01-STATE
      ***         END-IF
      ***         IF      SYS-1010-GSRAUTH(12:1)   =  "1"
      ***             MOVE    WIDGET-NORMAL      TO  M01N-I02-STATE
      ***         END-IF
      ***         IF      SYS-1010-GSRAUTH(13:1)   =  "1"
      ***             MOVE    WIDGET-NORMAL      TO  M01N-I03-STATE
      ***         END-IF
      *----(01.00.04) LINE DEL END   ----------------------------------
      *
           DISPLAY "SYS-1010-GSRAUTH(16:1)="  SYS-1010-GSRAUTH(16:1)
               IF      SYS-1010-GSRAUTH(16:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G11-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(17:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G12-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(18:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G13-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(19:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G14-STATE
               END-IF
      *
               IF      SYS-1010-GSRAUTH(21:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G21-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(22:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G22-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(23:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G23-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(24:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G24-STATE
               END-IF
      *----(01.02.08) LINE ADD START ----------------------------------
      *----(01.02.07) LINE ADD START ----------------------------------
               IF      SYS-1010-GSRAUTH(36:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G31-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(37:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G32-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(38:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G33-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(39:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G34-STATE
               END-IF
      *         IF      SYS-1010-GSRAUTH(56:1)   =  "1"
      *            MOVE    WIDGET-NORMAL      TO  M01N-G35-STATE
      *         END-IF
               IF      SYS-1010-GSRAUTH(57:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G36-STATE
               END-IF
      *----(01.02.07) LINE ADD END   ----------------------------------
      *----(01.02.08) LINE ADD END   ----------------------------------
      *
               IF      SYS-1010-GSRAUTH(26:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G41-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(27:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G42-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(28:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G43-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(29:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G44-STATE
               END-IF
      *
               IF      SYS-1010-GSRAUTH(31:1)  =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G51-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(32:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G52-STATE
               END-IF
      *
      *----(01.00.04) LINE DEL START ----------------------------------
      ***         IF      SYS-1010-GSRAUTH(36:1)  =  "1"
      ***             MOVE    WIDGET-NORMAL      TO  M01N-G61-STATE
      ***         END-IF
      *----(01.00.04) LINE DEL START ----------------------------------
      *
               IF      SYS-1010-GSRAUTH(41:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G71-STATE
               END-IF
      *
      ****     IF      SYS-1010-GSRAUTH(46:1)   =  "1"
      *            MOVE    WIDGET-NORMAL      TO  M01N-G81-STATE
      *****    END-IF
      *        グループ診療の場合
               IF      SPA-GRPHOSPNUM(2)   NOT =   SPACE
      *            スーパーバイザーであれば活性
                   IF      SPA-SUPERVISOR      =   "T"
                       IF      SYS-1010-GSRAUTH(47:1)   =  "1"
                           MOVE    WIDGET-NORMAL      TO  M01N-G82-STATE
                       END-IF
                   END-IF
               ELSE
                   IF      SYS-1010-GSRAUTH(47:1)   =  "1"
                       MOVE    WIDGET-NORMAL      TO  M01N-G82-STATE
                   END-IF
               END-IF
      *
               IF      SYS-1010-GSRAUTH(51:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M01N-G91-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(52:1)   =  "1"
      *             管理者権限フラグ
                   IF      SYS-1010-KANRIAUTHFLG  =   "T"
                       MOVE    WIDGET-NORMAL      TO  M01N-G92-STATE
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-MW          NOT =   SPA-PANDA )
               MOVE    "F"             TO  M01N-G71-VISIBLE
                                           M01N-G82-VISIBLE
                                           M01N-FR6-VISIBLE
               MOVE    CONST-HTML-GINBEE   TO  M01N-HTML1
           ELSE
               MOVE    CONST-HTML-PANDA    TO  M01N-HTML1
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
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "SELNUM"    TO  MCP-WIDGET
           END-EVALUATE
      *
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
                   MOVE    "入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "この業務は選択できません"
                                               TO  SPA-ERRMSG
               WHEN    "0092"
                   MOVE    "マスタ更新中です。"
                                               TO  SPA-ERRMSG
               WHEN    "9991"
                   MOVE    "日レセを再起動してください。"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M01N"               TO  SPA-MOTOPG
           MOVE    "MERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
      *
           EVALUATE    SPA-MIDCD
               WHEN    "0001"
                   MOVE
               "スプールに溜っているすべての印刷データを削除します。"
                                           TO  WRK-MIDMSG
               WHEN    "0101"
                   MOVE    "処理を開始します。"
                                           TO  WRK-MIDMSG
               WHEN    "0102"
                   MOVE    "マスタ更新は異常終了しました。"
                                           TO  WRK-MIDMSG
               WHEN    "0103"
                   MOVE    "マスタ更新は正常に終了しました。"
                                           TO  WRK-MIDMSG
               WHEN    "0104"
                   MOVE    "マスタ更新はありませんでした。"
                                           TO  WRK-MIDMSG
               WHEN    OTHER
                   MOVE    SPA-MIDCD       TO  WRK-MIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  MID1
           INITIALIZE                      MID1
           MOVE    SPA-MIDCD           TO  MID1-ID1CODE
           MOVE    WRK-MIDMSG          TO  MID1-ID1MSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "M01N"              TO  SPA-MOTOPG
           MOVE    "MID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "MID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
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
      *
           .
       910-DBSELECT-EXT.
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
           MOVE    "PUTWINDOW"         TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
