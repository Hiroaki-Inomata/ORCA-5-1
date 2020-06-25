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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGE02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＣＳＶ作成
      *  コンポーネント名  : ＣＳＶ作成（Ｅ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/09/15    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     ひさなが     02/05/10  ジョブ管理検索追加
      * 01.00.03     ひさなが     02/09/13  ディレクトリをサブプロへ
      * 01.00.04     ひさなが     03/04/08  診療年月チェックの修正
      * 01.00.05     ひさなが     03/05/12  入院関連テーブルの追加
      * 01.00.06     ひさなが     03/06/05  患者ＩＤ置換メッセージの追加
      * 03.05.01     NACL-森脇    07/06/11  グループ診療対応
      * 04.05.01     NACL-森脇    12/04/19  パッチ対応
      * 04.08.01     NACL-森脇    14/08/25  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ＣＳＶ作成条件用ファイル 
           SELECT  EIE00-FILE      ASSIGN  EIE00PARA
                                   FILE    STATUS  IS  STS-EIE00.
      *    ＣＳＶ作成条件１〜９０ファイル 
           SELECT  EIE01-FILE      ASSIGN  EIE01PARA
                                   FILE    STATUS  IS  STS-EIE01.
      *    抽出項目用ファイル
           SELECT  EIE02-FILE      ASSIGN  EIE02PARA
                                   FILE    STATUS  IS  STS-EIE02.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＣＳＶ作成条件用ファイル
       FD  EIE00-FILE.
       01  EIE00-REC               PIC X(50).
      *    ＣＳＶ作成条件１〜９０ファイル 
       FD  EIE01-FILE.
       01  EIE01-REC               PIC X(250).
      *    抽出項目用ファイル
       FD  EIE02-FILE.
       01  EIE02-REC               PIC X(001).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *----(04.08.01)--UPD-START---
      *    ＣＳＶ作成条件用ファイル 名称領域
grpsys*DD  COPY    "CPCOMMONDAT4.INC"
grpsys*DD                          REPLACING  //ORCGD//
grpsys     COPY    "CPCOMMONDAT2.INC"
grpsys                             REPLACING  //RECE01//
grpsys                             BY         //EIE00//.
grpsys*     03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    ＣＳＶ作成条件１〜９０ファイル 名称領域
grpsys*DD  COPY    "CPCOMMONDAT4.INC"
grpsys*DD                          REPLACING  //ORCGD//
grpsys*DD                          BY         //EIE01//.
grpsys     COPY    "CPCOMMONDAT2.INC"
grpsys                             REPLACING  //RECE01//
grpsys                             BY         //EIE01//.
grpsys*     03  EIE01PARA-FILE      PIC X(23).
      *----(04.08.01)--UPD-END-----
      *
      *    抽出項目用ファイル 名称領域
       01  EIE02PARA.
           03  FILLER               PIC  X(100).
       01  EIE02PARA-NAME.
           03  FILLER               PIC  X(07) VALUE
               "record/".
           03  EIE02-FILE-ID        PIC  X(33).
      *
      *    データチェック シェル領域 
           COPY    "CPCOMMONSHELL5.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  SPA-E02.
           03  SPA-E02-AREA.
               05  SPA-GMN-PAGE               PIC 9(02).
               05  SPA-GMN-MAX-A              PIC 9(03).
               05  SPA-GMN-MAX-B              PIC 9(03).
               05  SPA-GMN-CUR                PIC 9(03).
               05  SPA-GMN-KBN                PIC X(02).
               05  SPA-GMN-RUN                PIC X(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-SRYYM          PIC X(06).
                   07  SPA-GMN-TBLNM          PIC X(20).
                   07  SPA-GMN-FILE-ID        PIC X(128).
                   07  SPA-GMN-FILE-KEY       PIC X(64).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-SRYYM          PIC X(06).
                   07  SPA-NAI-MOTOPG         PIC X(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-FIL             PIC 9(01).
           03  FLG-SEL             PIC X(01).
           03  FLG-ROW1            PIC 9(01).
           03  FLG-ROW2            PIC 9(01).
      *
      *    エンドステータス領域
       01  STS-AREA.
           03  STS-EIE00           PIC X(02).
           03  STS-EIE01           PIC X(02).
           03  STS-EIE02           PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03).
           03  IDX4                PIC 9(03).
           03  IDX7                PIC 9(03).
           03  IDX8                PIC 9(03).
           03  IDX9                PIC 9(03).
           03  IDY1                PIC 9(06).
           03  IDY2                PIC 9(06).
           03  IDY3                PIC 9(06).
           03  IDY4                PIC 9(06).
           03  IDY5                PIC 9(06).
           03  IDY6                PIC 9(06).
           03  IDY7                PIC 9(05).
           03  IDY8                PIC 9(05).
           03  IDY9                PIC 9(06).
      *
      *    メッセージ領域
       01  T-MESSAGE-AREA.
           03  T-MESSAGE-01        PIC X(60)   VALUE
               "指定された条件でＣＳＶ出力中のためしばらくお待ちください
      -        "。  ".
           03  T-MESSAGE-02        PIC X(30)   VALUE
               "指定された条件でのＣＳＶ出力は".
           03  T-MESSAGE-03        PIC X(16)   VALUE
               "に終了しました。".
           03  T-MESSAGE-04        PIC X(28)   VALUE
               "抽出項目未設定テーブルあり。".
           03  T-MESSAGE-05        PIC X(42)   VALUE
               "指定された条件でＣＳＶ出力処理を行います。".
           03  T-MESSAGE-06        PIC X(42)   VALUE
               "診療年月入力誤り。".
           03  T-MESSAGE-07        PIC X(18)   VALUE
               "条件設定途中です。".
      *
      *    編集作業
       01  W-AREA.
           03  WK-SEW01                    PIC  X(02).
           03  WK-SEW02                    PIC  X(02).
           03  WK-SEW03                    PIC  X(02).
           03  WK-SEW04                    PIC  X(128).
           03  WK-SEW05                    PIC  9(05).
      *
       01  WRK-ROW.
           03  WRK-ROW1            PIC S9(9)   BINARY.
           03  WRK-ROW2            PIC S9(9)   BINARY.
      *
      *    作業テーブル名称領域
       01  WK-TBL1.
      *修正/*開始*03.06.05*********************************************
           03  WK-TBL-CHKBTN01             PIC X(01).
           03  WK-TBL-CHKMSG02             PIC X(06).
           03  WK-TBL-CHKPTMUM             PIC X(05).
           03  WK-TBL-SYSYMD               PIC X(08).
      *修正/*終了*03.06.05*********************************************
           03  WK-TBLSET1               OCCURS  200.
               05  WK-TBL-MSG              PIC X(20).
               05  WK-TBL-KBN1             PIC X(01).
               05  WK-TBL-KBN2             PIC X(01).
               05  WK-TBL-NAME             PIC X(20).
       01  WK-TBL2-S.
           03  WK-TBL2-1.
               05  WK-TBLSET0           OCCURS  900.
                   07  WK-TBL-BTN-A        PIC X(001).
                   07  WK-TBL-MSG-A        PIC X(128).
                   07  WK-TBL-BTN-B        PIC X(001).
                   07  WK-TBL-MSG-B        PIC X(128).
      *修正/*開始*03.06.05*********************************************
           03  WK-TBL-MSG-KBN              PIC X(01).
      *修正/*終了*03.06.05*********************************************
           03  WK-TBL2-2.
               05  WK-TBLSET2           OCCURS  900.
                   07  WK-TBL-MSG-C.
                       09  WK-TBL-MSG-C1   PIC X(01).
                       09  WK-TBL-MSG-C2   PIC X(29).
                       09  WK-TBL-MSG-C3   PIC X(07).
                       09  WK-TBL-MSG-C4   PIC X(13).
                       09  WK-TBL-MSG-C5   PIC X(05).
                       09  WK-TBL-MSG-C6   PIC X(05).
                       09  WK-TBL-MSG-C7   PIC X(05).
                       09  WK-TBL-MSG-C8   PIC X(05).
                       09  WK-TBL-MSG-C9   PIC X(06).
                       09  WK-TBL-MSG-C10  PIC X(52).
                   07  WK-TBL-BTN-C        PIC X(01).
       01  WK-TBL3.
           03  WK-TBL-MSG-D                PIC X(128).
           03  WK-TBL-SW-CNT               PIC 9(003).
           03  WK-TBL-SW-SW                PIC X(001).
       01  WK-ARE0.
           03  WK-SRYYM                    PIC X(006).
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理
       01  WRK-JOBKANRI-AREA.
           03  WRK-JOBKANRI-MODE           PIC X(03).
           03  WRK-JOBKANRI-RETURN         PIC S9(03).
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    ファイルリネームサブ
           COPY    "CPORCSSTRING.INC".
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *
           COPY    "MCPDATA.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
        LINKAGE                     SECTION.
      *
      *     テーブルアクセス
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "E01.INC".
           COPY    "E02.INC".
           COPY    "EERR.INC".
           COPY    "EID1.INC".
           COPY    "EID2.INC".
           COPY    "F01.INC".
           COPY    "FERR.INC".
           COPY    "FID1.INC".
           COPY    "FID2.INC".
      *
      *
       PROCEDURE                   DIVISION    USING
                                               MCPAREA
                                               SPAAREA
                                               LINKAREA
                                               SCRAREA.
      *****************************************************************
      *    主    処    理
      *****************************************************************
      *
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-E02.
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
      *
           MOVE    ZERO            TO  FLG-ROW1
                                       FLG-ROW2
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 530-RIDSET2-SEC
                   IF  SPA-GMN-RUN   =   SPACE   OR   "2"
                       PERFORM 200-ENTRY
                   END-IF
           END-EVALUATE.
      *    画面遷移がない時、画面表示へ
           IF  FLG-END    =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-E02         TO  SPA-FREE.
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
           INITIALIZE                  FLG-AREA.
      *
      *    エラー画面より
           IF  SPA-MOTOPG   =   "EERR" OR "EID1" OR "EID2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5011-MAPCUR-SEC
               IF  (SPA-GMN-RUN   =   SPACE   OR   "2")   AND
                   (SPA-GMN-KBN   =   "OK")
                   MOVE    "CHK"               TO  WRK-JOBKANRI-MODE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  JOB-JOBID
                   MOVE    "ORCBSE1"           TO  JOB-SHELLID
                   MOVE    ALL "*"             TO  JOB-TERMID
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"           USING
                                               WRK-JOBKANRI-AREA  
                                               JOBKANRI-REC
grpsys                                         SPA-AREA
                   IF  WRK-JOBKANRI-RETURN   =   ZERO
                       INITIALIZE                      WRK-JOBKANRI-AREA
                       MOVE    "DEL"               TO  WRK-JOBKANRI-MODE
                       INITIALIZE                      JOBKANRI-REC
                       MOVE    1                   TO  JOB-JOBID
                       MOVE    "ORCBSE1"           TO  JOB-SHELLID
                       MOVE    ALL "*"             TO  JOB-TERMID
                       MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                       CALL    "ORCSJOB"           USING
                                                   WRK-JOBKANRI-AREA
                                                   JOBKANRI-REC
grpsys                                             SPA-AREA
                   END-IF
                   INITIALIZE                      WRK-JOBKANRI-AREA
                   MOVE    "JBS"               TO  WRK-JOBKANRI-MODE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  JOB-JOBID
                   MOVE    "ORCBSE1"           TO  JOB-SHELLID
                   MOVE    ALL "*"             TO  JOB-TERMID
                   MOVE    "ＣＳＶ出力"        TO  JOB-SHELLMSG
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"           USING
                                               WRK-JOBKANRI-AREA
                                               JOBKANRI-REC
grpsys                                         SPA-AREA
                   INITIALIZE                      ORCSMKPASSAREA
                   MOVE    "scripts/allways/orcbse01.sh"
                                               TO  MKPASS-IN-01
                   CALL    "ORCSMKPASS"     USING  ORCSMKPASSAREA
                   MOVE    MKPASS-OT-01        TO  SHELL-NAME
                   MOVE    SPA-SYSYMD(1:6)     TO  SHELL-SRYYM
                   MOVE    SPA-TERMID          TO  SHELL-TERMID
                   MOVE    SPA-SYSYMD          TO  SHELL-SYSYMD
                   MOVE    ZERO                TO  SHELL-SYOKBN
                   MOVE    ZERO                TO  SHELL-RECEKBN
                   MOVE    SPACE               TO  SHELL-ERRFILE
                   MOVE    SHELL-NAME          TO  SHELLTBL-NAME
                   MOVE    SHELL-ARG1          TO  SHELLTBL-ARG1
                   MOVE    SHELL-ARG2          TO  SHELLTBL-ARG2
                   MOVE    SHELL-ARG3          TO  SHELLTBL-ARG3
                   MOVE    SHELL-ARG4          TO  SHELLTBL-ARG4
grpsys             MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
                   MOVE    SHELLTBL            TO  MCPDATA-REC
                   MOVE    "SHELL"             TO  MCP-FUNC
                   MOVE    "shell"             TO  MCP-TABLE
                   MOVE    "shell"             TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING
                                                   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
                   MOVE    1                   TO  FLG-END
                   MOVE    SPACE               TO  SPA-MOTOPG
                   MOVE    SPACE               TO  SPA-GMN-KBN
               END-IF
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF.
      *
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   SPA-GMN-MAX-A
               IF  E02-SELECT1 (IDX9)  =  "T"
                   MOVE    "A"              TO  FLG-SEL
                   MOVE    901              TO  IDX9
               END-IF
           END-PERFORM.
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   SPA-GMN-MAX-B
               IF  E02-SELECT2 (IDX9)  =  "T"
                   MOVE    "D"              TO  FLG-SEL
                   MOVE    901              TO  IDX9
               END-IF
           END-PERFORM.
      *
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-E02             TO  LNK-COMMON.
           MOVE    WK-TBL2-S           TO  LNK-SCRSPA.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "E02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-E02.
           INITIALIZE              WK-TBL2-1.
      *
      *    他のＬＤより
           IF  LINKAREA   NOT =   SPACE
               MOVE    LNK-KYOUTU              TO  SPA-KYOUTU
               MOVE    LNK-COMMON              TO  SPA-E02
               MOVE    LNK-SCRSPA              TO  WK-TBL2-S
           END-IF.
      *
           MOVE    ZERO                    TO  SPA-GMN-CUR.
      *
           PERFORM 310-SPASET-SEC.
      *
           IF  SPA-MOTOPG   NOT =   "XB01"
               PERFORM T-000-READCHEK-SEC
               PERFORM T-000-SETECHEK-SEC
               PERFORM T-001-READCHEK-SEC
               PERFORM T-000-SETTABLE-SEC
           END-IF.
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR.
      *
           MOVE    1                   TO  SPA-GMN-PAGE.
      *
           MOVE    1                   TO  FLG-ROW1
                                           FLG-ROW2
           .
      *
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM T-001-WRITCHEK-SEC
                   PERFORM T-000-WRITCHEK-SEC
                   PERFORM 210-BACK
               WHEN    "CLICKED"       ALSO    "B02"
                   IF  FLG-SEL   NOT =   SPACE
                       MOVE    "0004"              TO    SPA-ERRCD
                       PERFORM 500-SET-SCREEN
                   ELSE
                       PERFORM T-001-WRITCHEK-SEC
                       PERFORM T-000-WRITCHEK-SEC
                       PERFORM T-001-HANTCHEK-SEC
                       PERFORM T-000-SETECHEK-SEC
                       PERFORM T-001-READCHEK-SEC
                       PERFORM T-000-SETTABLE-SEC
                   END-IF
               WHEN    "CLICKED"       ALSO    "B03"
               WHEN    "CLICKED"       ALSO    "TBL_R"
                   PERFORM T-001-CHUSYUTU-SEC
               WHEN    "CLICKED"       ALSO    "B04"
               WHEN    "CLICKED"       ALSO    "TBL_L"
                   PERFORM T-001-TORIYAME-SEC
               WHEN    "CLICKED"       ALSO    "B05"
               WHEN    "CLICKED"       ALSO    "ALL_R"
                   PERFORM T-002-CHUSYUTU-SEC
               WHEN    "CLICKED"       ALSO    "B06"
               WHEN    "CLICKED"       ALSO    "ALL_L"
                   PERFORM T-002-TORIYAME-SEC
               WHEN    "CLICKED"       ALSO    "B07"
               WHEN    "CLICKED"       ALSO    "KOUMOKU_UP"
                   PERFORM T-002-AGARU-SEC
               WHEN    "CLICKED"       ALSO    "B08"
               WHEN    "CLICKED"       ALSO    "KOUMOKU_DOWN"
                   PERFORM T-002-SAGARU-SEC
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM T-001-WRITCHEK-SEC
                   PERFORM T-000-WRITCHEK-SEC
                   PERFORM 530-RIDSET3-SEC
               WHEN    "CLICKED"       ALSO    "B12"
                   IF  FLG-SEL   NOT =   SPACE
                       MOVE    "0004"              TO    SPA-ERRCD
                       PERFORM 500-SET-SCREEN
                   ELSE
                       PERFORM T-001-WRITCHEK-SEC
                       PERFORM T-000-WRITCHEK-SEC
                       MOVE    SPACE               TO    SPA-ERRCD
                       PERFORM VARYING IDX1  FROM  1  BY 1
                               UNTIL   IDX1  >    200
                           IF  (WK-TBL-KBN1 (IDX1)     = "*")  AND
                               (WK-TBL-KBN2 (IDX1) NOT = "*")
                               MOVE    "0002"          TO  SPA-ERRCD
      *                        MOVE    201              TO  IDX1
                           END-IF
                       END-PERFORM
                       IF  SPA-ERRCD  NOT = SPACE
                           PERFORM 500-SET-SCREEN
                       ELSE
                           PERFORM 530-RIDSET2-SEC
                       END-IF
                   END-IF
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 530-RIDSET2-SEC
               WHEN      ANY           ALSO    "CLIST1"
                   PERFORM T-000-CLST1CHK-SEC
               WHEN      ANY           ALSO    "CLIST2"
                   PERFORM T-000-CLST2CHK-SEC
               WHEN    OTHER
                   MOVE    ZERO            TO  SPA-GMN-CUR
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
               WHEN    "CHANGED"       ALSO    "SRYYM"
                   PERFORM T-000-SNENCHEK-SEC
               WHEN    "ACTIVATE"      ALSO    "FILENAME"
               WHEN    "CHANGED"       ALSO    "FILENAME"
                   PERFORM T-000-NAMECHEK-SEC
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM T-002-AGARU-SEC
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM T-002-SAGARU-SEC
               WHEN      ANY           ALSO    "CLIST1"
                   PERFORM T-000-CLST1CHK-SEC
               WHEN      ANY           ALSO    "CLIST2"
                   PERFORM T-000-CLST2CHK-SEC
               WHEN    OTHER
                   MOVE    ZERO            TO  SPA-GMN-CUR
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "E01"               TO  SPA-SAKIPG.
           MOVE    SPACE               TO  SPA-MOTOPG2.
           MOVE    "E02"               TO  SPA-MOTOPG.
      *
           MOVE    SPACE               TO  LINKAREA. 
           INITIALIZE                  SPA-KYOTUKEY.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
           MOVE    "CHANGE"            TO  MCP-PUTTYPE.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF  SPA-ERRCD   NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF.
      *
           MOVE    SPACE           TO  LINKAREA.
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "E02"           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *     MOVE    SPACE               TO  E02.
           MOVE    E02-ROW1        TO     WRK-ROW1
           MOVE    E02-ROW2        TO     WRK-ROW2
      *
           INITIALIZE              E02.
      *
           IF    ( FLG-ROW1        =      1 )
               MOVE    ZERO            TO  E02-ROW1
               MOVE    ZERO            TO  E02-ROWATTR1
           ELSE
               MOVE    WRK-ROW1        TO  E02-ROW1
               MOVE    ZERO            TO  E02-ROWATTR1
           END-IF
      *
           IF    ( FLG-ROW2        =      1 )
               MOVE    ZERO            TO  E02-ROW2
               MOVE    ZERO            TO  E02-ROWATTR2
           ELSE
               MOVE    WRK-ROW2        TO  E02-ROW2
               MOVE    ZERO            TO  E02-ROWATTR2
           END-IF
      *
           MOVE    SPA-GMN-TBLNM       TO  E02-CHKMSG000.
           MOVE    SPA-GMN-SRYYM       TO  E02-SRYYM.
           MOVE    SPA-GMN-FILE-ID     TO  E02-FILENAME.
           IF  SPA-GMN-MAX-A   NOT =   ZERO
               PERFORM VARYING IDX7  FROM  1  BY 1
                       UNTIL   IDX7  >   SPA-GMN-MAX-A
                   MOVE    WK-TBL-MSG-A (IDX7)(1:25)
                                         TO  E02-VALUE1 (IDX7)(1:25)
                   MOVE    WK-TBL-BTN-A (IDX7)
                                         TO  E02-SELECT1 (IDX7)
      *             MOVE    IDX7          TO  E02-COUNT1
               END-PERFORM
           END-IF.
           MOVE    SPA-GMN-MAX-A     TO  E02-COUNT1.
      *
           IF  SPA-GMN-MAX-B   NOT =   ZERO
               PERFORM VARYING IDX7  FROM  1  BY 1
                       UNTIL   IDX7  >   SPA-GMN-MAX-B
                   MOVE    WK-TBL-MSG-B (IDX7)(1:25)
                                         TO  E02-VALUE2 (IDX7)(1:25)
                   MOVE    WK-TBL-BTN-B (IDX7)
                                         TO  E02-SELECT2 (IDX7)
      *             MOVE    IDX7          TO  E02-COUNT2
               END-PERFORM
           END-IF.
           MOVE    SPA-GMN-MAX-B      TO  E02-COUNT2.
      *
           PERFORM 5011-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5011-MAPCUR-SEC              SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SRYYM"         TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE.
      *
       5011-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
           EVALUATE    SPA-ERRCD
               WHEN    "0002"
                   MOVE    T-MESSAGE-04        TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    T-MESSAGE-06        TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    T-MESSAGE-07        TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPACE               TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  EERR.
           MOVE    SPA-ERRCD           TO  EERR-ERRCODE.
           MOVE    SPA-ERRMSG          TO  EERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "E02"               TO  SPA-MOTOPG.
           MOVE    "EERR"              TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "EERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       530-RIDSET2-SEC              SECTION.
      *
      *    ジョブ管理チェック処理
      *
           MOVE    "CHK"              TO  WRK-JOBKANRI-MODE.
           INITIALIZE                     JOBKANRI-REC.
           MOVE    1                  TO  JOB-JOBID.
           MOVE    "ORCBSE1"          TO  JOB-SHELLID.
           MOVE    ALL "*"            TO  JOB-TERMID.
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM.
           CALL    "ORCSJOB"          USING
                                      WRK-JOBKANRI-AREA  
                                      JOBKANRI-REC
grpsys                                SPA-AREA.
           MOVE    SPACE              TO  EID2.
           MOVE    SPACE              TO  SPA-GMN-RUN.
           IF  WRK-JOBKANRI-RETURN   =   ZERO
               IF  JOB-ENDYMD            =   SPACE
                   MOVE    "7777"             TO  EID2-ID2CODE
                   MOVE    T-MESSAGE-01       TO  EID2-ID2MSG
                   MOVE    "1"                TO  SPA-GMN-RUN
               ELSE
                   IF  MCP-WIDGET             =   "B13"
                       MOVE    "8888"             TO  EID2-ID2CODE
                       MOVE    T-MESSAGE-02       TO  EID2-ID2MSG(1:30)
                       MOVE    JOB-ENDTIME(1:2)   TO  EID2-ID2MSG(31:2)
                       MOVE    "時"               TO  EID2-ID2MSG(33:2)
                       MOVE    JOB-ENDTIME(3:2)   TO  EID2-ID2MSG(35:2)
                       MOVE    "分"               TO  EID2-ID2MSG(37:2)
                       MOVE    T-MESSAGE-03       TO  EID2-ID2MSG(39:16)
                       MOVE    "2"                TO  SPA-GMN-RUN
                   END-IF
               END-IF
               IF  MCP-WIDGET   =   "B01"   OR   "B13"
                   MOVE    "E02"               TO  SPA-MOTOPG
                   MOVE    "EID2"              TO  SPA-SAKIPG
                   MOVE    "NEW"               TO  MCP-PUTTYPE
                   MOVE    "EID2"              TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF.
      *
           IF  MCP-WIDGET             =   "B12"
               IF  SPA-GMN-RUN        NOT =   "1"
                   MOVE    "1001"                TO  EID1-ID1CODE
                   MOVE    T-MESSAGE-05          TO  EID1-ID1MSG
                   MOVE    "E02"                 TO  SPA-MOTOPG
                   MOVE    "EID1"                TO  SPA-SAKIPG
                   MOVE    "NEW"                 TO  MCP-PUTTYPE
                   MOVE    "EID1"                TO  MCP-WINDOW
               ELSE
                   MOVE    "E02"                 TO  SPA-MOTOPG
                   MOVE    "EID2"                TO  SPA-SAKIPG
                   MOVE    "NEW"                 TO  MCP-PUTTYPE
                   MOVE    "EID2"                TO  MCP-WINDOW
               END-IF
               PERFORM 900-PUT-WINDOW
               MOVE    1                     TO  FLG-END
           END-IF.
      *
       530-RIDSET2-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理表示理
      *****************************************************************
       530-RIDSET3-SEC              SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "E02"               TO  SPA-MOTOPG.
           MOVE    "XB01"              TO  SPA-SAKIPG.
           INITIALIZE                  SPA-KYOTUKEY.
           INITIALIZE                  SPA-WORK.
           MOVE    "ORCBSE1"           TO  SPA-JOB-SHELLID.
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-E02             TO  LNK-COMMON.
           MOVE    WK-TBL2-S           TO  LNK-SCRSPA.
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "XB01"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END.
      *
       530-RIDSET3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
grpsys*DD  CALL   "ORCDBMAIN"      USING
grpsys*DD                              MCPAREA
grpsys*DD                              MCPDATA-REC
grpsys*DD                              SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月 確認処理
      *****************************************************************
       T-000-SNENCHEK-SEC                SECTION.
      *
           IF  (WK-TBL-MSG (IDX2)(1:6)  = "JYURRK")       OR
               (WK-TBL-MSG (IDX2)(1:8)  = "PTBYOMEI")     OR
               (WK-TBL-MSG (IDX2)(1:12) = "SRYACCT_MAIN") OR
               (WK-TBL-MSG (IDX2)(1:11) = "SRYACCT_SUB")  OR
               (WK-TBL-MSG (IDX2)(1:6)  = "SRYACT")       OR
               (WK-TBL-MSG (IDX2)(1:8)  = "WKSRYACT")     OR
               (WK-TBL-MSG (IDX2)(1:6)  = "SANTEI")       OR
               (WK-TBL-MSG (IDX2)(1:11) = "SEIKYU_MAIN")  OR
               (WK-TBL-MSG (IDX2)(1:10) = "SEIKYU_KOH")   OR
               (WK-TBL-MSG (IDX2)(1:10) = "SEIKYU_ETC")   OR
               (WK-TBL-MSG (IDX2)(1:5)  = "BD002")        OR
      *修正/*開始*03.05.12*********************************************
               (WK-TBL-MSG (IDX2)(1:9)  = "NYUINACCT")    OR
               (WK-TBL-MSG (IDX2)(1:8)  = "NYUINACT")     OR
               (WK-TBL-MSG (IDX2)(1:10) = "PTTEIKIRRK")
      *修正/*終了*03.05.12*********************************************
               MOVE    E02-SRYYM(1:6)      TO  SPA-GMN-SRYYM
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SRYYM
               MOVE    SPACE               TO  SPA-NAI-SRYYM
           END-IF.
      *
           IF  SPA-GMN-SRYYM   =  "******" OR  SPACE
               MOVE    SPACE               TO  SPA-GMN-SRYYM
               MOVE    SPACE               TO  SPA-NAI-SRYYM
           END-IF.
      *
           MOVE    ZERO                TO  SPA-GMN-CUR.
           IF  SPA-GMN-SRYYM   NOT =  SPACE
               INITIALIZE                  ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-GMN-SRYYM       TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING   ORCSGDAYAREA
               IF  SGDAY-RC   =   ZERO
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
                   MOVE    SGDAY-SDAY(1:6)     TO  SPA-NAI-SRYYM
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY1-AREA
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    "11"                TO  LNK-DAY1-IRAI
                   MOVE    SGDAY-SDAY(1:6)     TO  LNK-DAY1-YMD-X(1:6)
                   MOVE    "01"                TO  LNK-DAY1-YMD-X(7:2)
                   CALL    "ORCSDAY"             USING   STS-AREA-DAY
                                                         LNK-DAY1-AREA
                   IF  STS-DAY-RC1   NOT =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                       MOVE    01                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
                   END-IF
               ELSE
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    01                  TO  SPA-GMN-CUR
               END-IF
           END-IF.
      *
       T-000-SNENCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル名 確認処理
      *****************************************************************
       T-000-NAMECHEK-SEC                SECTION.
      *
           MOVE    E02-FILENAME        TO  SPA-GMN-FILE-ID.
      *
       T-000-NAMECHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｂ０３クリック 処理
      *****************************************************************
       T-001-CHUSYUTU-SEC                SECTION.
      *
           IF  FLG-SEL   =   "A"
               MOVE    ZERO                   TO    IDX7
               MOVE    ZERO                   TO    IDX8
      *
               PERFORM VARYING IDX9  FROM  1  BY 1
                       UNTIL   IDX9  >   SPA-GMN-MAX-A
                   IF  WK-TBL-BTN-A (IDX9)   =   "T"
                       COMPUTE  IDX7          = IDX7          + 1
                       COMPUTE  SPA-GMN-MAX-B = SPA-GMN-MAX-B + 1
                       MOVE    WK-TBL-MSG-A (IDX9)
                                     TO  WK-TBL-MSG-B (SPA-GMN-MAX-B)
                       MOVE    "F"   TO  WK-TBL-BTN-A (IDX9)
                   ELSE
                       COMPUTE  IDX8 = IDX8 + 1
                       MOVE    WK-TBL-MSG-A (IDX9)
                                     TO  WK-TBL-MSG-A (IDX8)
                   END-IF
               END-PERFORM
      *
               COMPUTE  IDX8 = SPA-GMN-MAX-A + 1
      *
               PERFORM VARYING IDX9  FROM  IDX8  BY 1
                       UNTIL   IDX9  >   900
                   MOVE    SPACE              TO  WK-TBL-MSG-A (IDX9)
                   MOVE    "F"                TO  WK-TBL-BTN-A (IDX9)
               END-PERFORM
      *
               COMPUTE  SPA-GMN-MAX-A = SPA-GMN-MAX-A - IDX7
               MOVE    SPACE               TO  FLG-SEL
      *
           END-IF.
      *
       T-001-CHUSYUTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｂ０４クリック 処理
      *****************************************************************
       T-001-TORIYAME-SEC                SECTION.
      *
           IF  FLG-SEL   =   "D"
               MOVE    ZERO                   TO    IDX7
               MOVE    ZERO                   TO    IDX8
      *
               PERFORM VARYING IDX9  FROM  1  BY 1
                       UNTIL   IDX9  >   SPA-GMN-MAX-B
                   IF  WK-TBL-BTN-B (IDX9)   =   "T"
                       COMPUTE  IDX7 = IDX7 + 1
                       MOVE    "F"       TO  WK-TBL-BTN-B (IDX9)
                   ELSE
                       COMPUTE  IDX8 = IDX8 + 1
                       MOVE    WK-TBL-MSG-B (IDX9)
                                         TO  WK-TBL-MSG-B (IDX8)
                   END-IF
               END-PERFORM
      *
               COMPUTE  IDX8 = SPA-GMN-MAX-B + 1
      *
               PERFORM VARYING IDX9  FROM  IDX8  BY 1
                       UNTIL   IDX9  >   900
                   MOVE    SPACE            TO  WK-TBL-MSG-B (IDX9)
                   MOVE    "F"              TO  WK-TBL-BTN-B (IDX9)
               END-PERFORM
      *
               COMPUTE  SPA-GMN-MAX-B = SPA-GMN-MAX-B - IDX7
               MOVE    SPACE             TO  FLG-SEL
               PERFORM T-000-SETTABLE-SEC
      *
           END-IF.
      *
       T-001-TORIYAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｂ０５クリック 処理
      *****************************************************************
       T-002-CHUSYUTU-SEC                SECTION.
      *
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   SPA-GMN-MAX-A
               COMPUTE  SPA-GMN-MAX-B = SPA-GMN-MAX-B + 1
               MOVE    WK-TBL-MSG-A (IDX9)
                                  TO  WK-TBL-MSG-B (SPA-GMN-MAX-B)
               MOVE    SPACE      TO  WK-TBL-MSG-A (IDX9)
               MOVE    "F"        TO  WK-TBL-BTN-A (IDX9)
           END-PERFORM.
      *
           MOVE    1                      TO  FLG-ROW1
                                              FLG-ROW2
      *
           MOVE    ZERO             TO  SPA-GMN-MAX-A.
           MOVE    SPACE            TO  FLG-SEL.
      *
       T-002-CHUSYUTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｂ０６クリック 処理
      *****************************************************************
       T-002-TORIYAME-SEC                SECTION.
      *
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   SPA-GMN-MAX-B
               MOVE    SPACE            TO  WK-TBL-MSG-B (IDX9)
               MOVE    "F"              TO  WK-TBL-BTN-B (IDX9)
           END-PERFORM.
      *
           MOVE    1                      TO  FLG-ROW1
                                              FLG-ROW2
      *
           MOVE    ZERO             TO  SPA-GMN-MAX-B.
           MOVE    SPACE            TO  FLG-SEL.
           PERFORM T-000-SETTABLE-SEC.
      *
       T-002-TORIYAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｂ０７クリック 処理
      *****************************************************************
       T-002-AGARU-SEC                   SECTION.
      *
           MOVE    ZERO              TO  WK-TBL-SW-CNT.
           MOVE    SPACE             TO  WK-TBL-SW-SW.
      *
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   SPA-GMN-MAX-B
               IF  WK-TBL-BTN-B (IDX9)   =   "T"
                   COMPUTE  WK-TBL-SW-CNT = WK-TBL-SW-CNT + 1
               END-IF
           END-PERFORM.
      *
           IF  WK-TBL-BTN-B (1)   =   "T"
               MOVE    "*"               TO  WK-TBL-SW-SW
           END-IF.
      *
           IF  (WK-TBL-SW-CNT  >  1)   AND
               (WK-TBL-SW-SW   =  "*")
               CONTINUE
           ELSE
               MOVE    1                 TO  IDX9
               PERFORM UNTIL   IDX9  >   SPA-GMN-MAX-B
                   IF  WK-TBL-BTN-B (IDX9)   =   "T"
                       COMPUTE  IDX7 = IDX9 - 1
                       IF  IDX7  >  ZERO
                           MOVE    WK-TBL-MSG-B (IDX9)
                                           TO  WK-TBL-MSG-D
                           MOVE    "F"     TO  WK-TBL-BTN-B (IDX9)
                           MOVE    WK-TBL-MSG-B (IDX7)
                                           TO  WK-TBL-MSG-B (IDX9)
                           MOVE    WK-TBL-MSG-D
                                           TO  WK-TBL-MSG-B (IDX7)
                           MOVE    "T"     TO  WK-TBL-BTN-B (IDX7)
                       END-IF
                   END-IF
                   COMPUTE  IDX9 = IDX9 + 1
               END-PERFORM
           END-IF.
      *
       T-002-AGARU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｂ０８クリック 処理
      *****************************************************************
       T-002-SAGARU-SEC                  SECTION.
      *
           MOVE    ZERO              TO  WK-TBL-SW-CNT.
           MOVE    SPACE             TO  WK-TBL-SW-SW.
      *
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   SPA-GMN-MAX-B
               IF  WK-TBL-BTN-B (IDX9)   =   "T"
                   COMPUTE  WK-TBL-SW-CNT = WK-TBL-SW-CNT + 1
               END-IF
           END-PERFORM.
      *
           IF  WK-TBL-BTN-B (SPA-GMN-MAX-B)   =   "T"
               MOVE    "*"               TO  WK-TBL-SW-SW
           END-IF.
      *
           IF  (WK-TBL-SW-CNT  >  1)  AND
               (WK-TBL-SW-SW   =  "*")
               CONTINUE
           ELSE
               MOVE    SPA-GMN-MAX-B     TO  IDX9
               PERFORM UNTIL   IDX9  <   1
                   IF  WK-TBL-BTN-B (IDX9)   =   "T"
                       COMPUTE  IDX7 = IDX9 + 1
                       IF  IDX7  NOT >  SPA-GMN-MAX-B
                           MOVE    WK-TBL-MSG-B (IDX9)
                                           TO  WK-TBL-MSG-D
                           MOVE    "F"     TO  WK-TBL-BTN-B (IDX9)
                           MOVE    WK-TBL-MSG-B (IDX7)
                                           TO  WK-TBL-MSG-B (IDX9)
                           MOVE    WK-TBL-MSG-D
                                           TO  WK-TBL-MSG-B (IDX7)
                           MOVE    "T"     TO  WK-TBL-BTN-B (IDX7)
                       END-IF
                   END-IF
                   COMPUTE  IDX9 = IDX9 - 1
               END-PERFORM
           END-IF.
      *
       T-002-SAGARU-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル項目クリック 処理
      *****************************************************************
       T-000-CLST1CHK-SEC                SECTION.
      *
           IF  FLG-SEL   =   " " OR "A"
               MOVE    SPACE            TO  FLG-SEL
               PERFORM VARYING IDX9  FROM  1  BY 1
                       UNTIL   IDX9  >   SPA-GMN-MAX-A
                   MOVE    E02-SELECT1 (IDX9) TO  WK-TBL-BTN-A (IDX9)
                   IF  E02-SELECT1 (IDX9)   =   "T"
                       MOVE    "A"              TO  FLG-SEL
                   END-IF
               END-PERFORM
           END-IF.
      *
       T-000-CLST1CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    抽出項目クリック 処理
      *****************************************************************
       T-000-CLST2CHK-SEC                SECTION.
      *
           IF  FLG-SEL   =   " " OR "D"
               MOVE    SPACE            TO  FLG-SEL
               PERFORM VARYING IDX9  FROM  1  BY 1
                       UNTIL   IDX9  >   SPA-GMN-MAX-B
                   MOVE    E02-SELECT2 (IDX9) TO  WK-TBL-BTN-B (IDX9)
                   IF  E02-SELECT2 (IDX9)   =   "T"
                       MOVE    "D"              TO  FLG-SEL
                   END-IF
               END-PERFORM
           END-IF.
      *
       T-000-CLST2CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル名称 セット処理
      *****************************************************************
       T-000-SETTABLE-SEC                SECTION.
      *
           IF  SPA-GMN-MAX-B   NOT =   ZERO
               PERFORM VARYING IDX9  FROM  1  BY 1
                       UNTIL   IDX9  >   SPA-GMN-MAX-B
                   PERFORM VARYING IDX7  FROM  1  BY 1
                           UNTIL   IDX7  >   900
                       IF  WK-TBL-MSG-B (IDX9)   =   WK-TBL-MSG-C (IDX7)
                           MOVE    "T"         TO  WK-TBL-BTN-C (IDX7)
                           MOVE    901         TO  IDX7
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF.
      *
           MOVE    ZERO             TO  IDX8.
           MOVE    ZERO             TO  SPA-GMN-MAX-A.
      *
           PERFORM VARYING IDX9  FROM  1  BY 1
                   UNTIL   IDX9  >   900
               EVALUATE    WK-TBL-BTN-C (IDX9)
                   WHEN    "T"
                       MOVE    "F"         TO  WK-TBL-BTN-C (IDX9)
                   WHEN    "F"
                       COMPUTE  IDX8 = IDX8 + 1
                       MOVE    IDX8        TO  SPA-GMN-MAX-A
                       MOVE    WK-TBL-MSG-C (IDX9)
                                           TO  WK-TBL-MSG-A (IDX8)
               END-EVALUATE
           END-PERFORM.
      *         
           COMPUTE  IDX8 = SPA-GMN-MAX-A + 1.
      *
           PERFORM VARYING IDX9  FROM  IDX8  BY 1
                   UNTIL   IDX9  >   900
               MOVE    SPACE            TO  WK-TBL-MSG-A (IDX9)
               MOVE    "F"              TO  WK-TBL-BTN-A (IDX9)
           END-PERFORM.
      *
       T-000-SETTABLE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ作成条件 入力処理
      *****************************************************************
       T-000-READCHEK-SEC                SECTION.
      *
grpsys*DD  MOVE    "ORCE00.CNV"          TO  EIE00PARA-FILE.
grpsys     MOVE    "ORCGE00"         TO  EIE00PARA-FILE-ID.
grpsys     MOVE    SPA-HOSPNUM       TO  EIE00PARA-HOSPNUM.
grpsys     MOVE    SPA-TERMID        TO  EIE00PARA-TERMID.
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    EIE00PARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    ALL SPACE           TO  EIE00PARA-FULLNAME
           STRING  SGETTEMP-FULLNAME   DELIMITED  BY    SPACE
                   ".dat"              DELIMITED  BY    SIZE
                                       INTO       EIE00PARA-FULLNAME
           END-STRING.
      *----(04.08.01)--ADD-END-----
           MOVE    SPACE                 TO  WK-TBL1.
           MOVE    1                     TO  IDX1.
           MOVE    ZERO                  TO  IDX2.
      *
           OPEN    INPUT                   EIE00-FILE.
      *
           IF  STS-EIE00   =   ZERO
      *修正/*開始*03.06.05*********************************************
               READ    EIE00-FILE
                 AT   END
                   MOVE    201              TO  IDX1
                 NOT  AT  END
                   IF  (EIE00-REC(3:6)   =   "する  ")  OR
                       (EIE00-REC(3:6)   =   "しない")
                       MOVE    EIE00-REC(1:1)  TO  WK-TBL-CHKBTN01
                       MOVE    EIE00-REC(3:6)  TO  WK-TBL-CHKMSG02
                       MOVE    EIE00-REC(10:5) TO  WK-TBL-CHKPTMUM
                       MOVE    EIE00-REC(16:8) TO  WK-TBL-SYSYMD
                   ELSE
                       PERFORM T-000-READCHEK1-SEC
                   END-IF
               END-READ
      *修正/*終了*03.06.05*********************************************
               PERFORM UNTIL   IDX1  >    200
                   READ    EIE00-FILE
                     AT   END
                       MOVE    201    TO  IDX1
                     NOT  AT  END
                       PERFORM T-000-READCHEK1-SEC
                   END-READ
               END-PERFORM
               CLOSE                           EIE00-FILE
           END-IF.
      *
      *修正/*開始*03.06.05*********************************************
           IF  WK-TBL-CHKBTN01   =   "T"
               MOVE    "tbl_ptnum.db"        TO  EIE02-FILE-ID
               MOVE    EIE02PARA-NAME        TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"          USING   ORCSMKPASSAREA
               MOVE    MKPASS-OT-01          TO  EIE02PARA
      *
      *        パッチ対応
               PERFORM T-000-PATCHCHEK-SEC
      *
               MOVE    1                     TO  IDX4
               MOVE    1                     TO  IDY1
               MOVE    ZERO                  TO  IDY2
               MOVE    ZERO                  TO  IDY3
               MOVE    ZERO                  TO  FLG-FIL
               MOVE    SPACE                 TO  FLG-SEL
               MOVE    SPACE                 TO  WK-TBL2-2
               MOVE    SPACE                 TO  WK-SEW01
               MOVE    SPACE                 TO  WK-SEW02
               MOVE    SPACE                 TO  WK-SEW03
               MOVE    SPACE                 TO  WK-SEW04
               MOVE    SPACE                 TO  WK-TBL-MSG-KBN
      *
               OPEN    INPUT                 EIE02-FILE
      *
               IF  STS-EIE02   =   ZERO
                   PERFORM UNTIL   FLG-FIL  =    1
                       READ    EIE02-FILE
                         AT   END
                           MOVE    1                 TO  FLG-FIL
                         NOT  AT  END
                           IF  EIE02-REC = "}"
                               MOVE    1                 TO  FLG-FIL
                           ELSE
                               PERFORM T-000-SETECHEK-01-SEC
                           END-IF
                       END-READ
                   END-PERFORM
                   CLOSE                           EIE02-FILE
               END-IF
      *
               PERFORM VARYING IDX1  FROM  1  BY 1
                       UNTIL   IDX1  >     900
                   IF  WK-TBL-MSG-C (IDX1)(1:5)   =   "PTNUM"
                       MOVE    WK-TBL-MSG-C5 (IDX1)  TO  WK-TBL-CHKPTMUM
                       MOVE    901                   TO  IDX1
                   END-IF
               END-PERFORM
           END-IF.
      *修正/*終了*03.06.05*********************************************
      *
       T-000-READCHEK-EXT.
           EXIT.
      *
      ********************
      *
       T-000-READCHEK1-SEC               SECTION.
      *
           MOVE    EIE00-REC(1:20)      TO  WK-TBL-MSG (IDX1).
           MOVE    EIE00-REC(22:1)      TO  WK-TBL-KBN1 (IDX1).
           MOVE    EIE00-REC(24:1)      TO  WK-TBL-KBN2 (IDX1).
           MOVE    EIE00-REC(26:20)     TO  WK-TBL-NAME (IDX1).
           IF      (IDX2            =   ZERO)  AND
                   (EIE00-REC(22:1) =   "*")
               MOVE    IDX1                 TO    IDX2
           END-IF.
grpsys     MOVE    "ORCGE01"         TO  EIE01PARA-FILE-ID.
grpsys     MOVE    SPA-HOSPNUM       TO  EIE01PARA-HOSPNUM.
grpsys     MOVE    SPA-TERMID        TO  EIE01PARA-TERMID.
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    EIE01PARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    ALL SPACE           TO  EIE01PARA-FULLNAME
           STRING  SGETTEMP-FULLNAME   DELIMITED  BY    SPACE
                   WK-TBL-MSG (IDX1)   DELIMITED  BY    SPACE
                   ".SLT"              DELIMITED  BY    SIZE
                                       INTO       EIE01PARA-FULLNAME
           END-STRING.
      *----(04.08.01)--ADD-END-----
      *----(04.08.01)--UPD-START---
      *     MOVE    ALL SPACE            TO  EIE01PARA-FILE.
      *     STRING  WK-TBL-MSG (IDX1)    DELIMITED  BY    SPACE
      *             ".SLT"               DELIMITED  BY    SIZE
      *                                  INTO       EIE01PARA-FILE.
      *----(04.08.01)--UPD-END-----
           OPEN    INPUT                EIE01-FILE.
           IF  STS-EIE01   =   ZERO
               READ    EIE01-FILE
                 NOT  AT  END
                   READ    EIE01-FILE
                     NOT  AT  END
                       MOVE    "*"          TO  WK-TBL-KBN2 (IDX1)
                   END-READ
               END-READ
               CLOSE                        EIE01-FILE
           END-IF.
           COMPUTE  IDX1   =   IDX1   +   1.
      *
       T-000-READCHEK1-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成条件 入力処理
      *****************************************************************
       T-001-READCHEK-SEC                SECTION.
      *
           OPEN    INPUT                   EIE01-FILE.
      *
           MOVE    ZERO                    TO  SPA-GMN-MAX-B.
           MOVE    SPACE                   TO  SPA-NAI-SRYYM
      *
           IF  STS-EIE01   =   ZERO
               READ    EIE01-FILE
                 NOT  AT  END
                   MOVE    EIE01-REC(1:6)     TO  SPA-GMN-SRYYM
                   IF  EIE01-REC(1:6) NOT =  SPACE
                       INITIALIZE             ORCSGDAYAREA
                       MOVE    "1"            TO  SGDAY-INTYPE
                       MOVE    EIE01-REC(1:6) TO  SGDAY-INDAY
                       CALL    "ORCSGDAY"  USING   ORCSGDAYAREA
                       IF  SGDAY-RC   =   ZERO
                           MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-SRYYM
                        END-IF
                   END-IF
                   MOVE    EIE01-REC(1:6)           TO  SPA-NAI-SRYYM
                   MOVE    EIE01-REC(8:128)         TO  SPA-GMN-FILE-ID
                   MOVE    EIE01-REC(137:64)        TO  SPA-GMN-FILE-KEY
                   PERFORM VARYING IDX1  FROM  1  BY 1
                           UNTIL   IDX1  >   900
                       READ    EIE01-FILE
                         AT   END
                           MOVE    901          TO  IDX1
                         NOT  AT  END
                           IF  EIE01-REC(1:8)  NOT =  "SRYYM***"  AND
      *修正/*開始*03.06.05*********************************************
      ********************                            "HOSPID**"
                                                      "HOSPNUM*"
      *修正/*終了*03.06.05*********************************************
                               MOVE    EIE01-REC TO  WK-TBL-MSG-B (IDX1)
                               MOVE    IDX1      TO  SPA-GMN-MAX-B
                           END-IF
                       END-READ
                   END-PERFORM
               END-READ
               CLOSE                           EIE01-FILE
           END-IF.
      *
       T-001-READCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶ作成条件 更新出力処理
      *****************************************************************
       T-000-WRITCHEK-SEC                SECTION.
      *
           OPEN    OUTPUT                  EIE00-FILE.
      *
      *修正/*開始*03.06.05*********************************************
           MOVE    SPACE                 TO  EIE00-REC.
           MOVE    WK-TBL-CHKBTN01       TO  EIE00-REC(1:1).
           MOVE    WK-TBL-CHKMSG02       TO  EIE00-REC(3:6).
           MOVE    WK-TBL-CHKPTMUM       TO  EIE00-REC(10:5).
           MOVE    WK-TBL-SYSYMD         TO  EIE00-REC(16:8)
           WRITE   EIE00-REC.
      *修正/*終了*03.06.05*********************************************
      *
           PERFORM VARYING IDX1  FROM  1  BY 1
                   UNTIL   IDX1  >    200
               IF  WK-TBL-MSG (IDX1)   NOT =   SPACE
                   MOVE    SPACE                   TO  EIE00-REC
                   MOVE    WK-TBL-MSG (IDX1)       TO  EIE00-REC(1:20)
                   MOVE    WK-TBL-KBN1 (IDX1)      TO  EIE00-REC(22:1)
                   MOVE    WK-TBL-KBN2 (IDX1)      TO  EIE00-REC(24:1)
                   MOVE    WK-TBL-NAME (IDX1)      TO  EIE00-REC(26:20)
                   WRITE   EIE00-REC
               END-IF
           END-PERFORM.
      *
           CLOSE                           EIE00-FILE.
      *
       T-000-WRITCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成条件 出力処理
      *****************************************************************
       T-001-WRITCHEK-SEC                SECTION.
      *
           OPEN    OUTPUT                  EIE01-FILE.
      *
           MOVE    SPACE                   TO  EIE01-REC.
           MOVE    SPA-NAI-SRYYM           TO  EIE01-REC.
           MOVE    SPA-GMN-FILE-ID         TO  EIE01-REC(8:128).
           MOVE    SPA-GMN-FILE-KEY        TO  EIE01-REC(137:64).
           WRITE   EIE01-REC.
      *
           PERFORM VARYING IDX1  FROM  1  BY 1
                   UNTIL   IDX1  >     SPA-GMN-MAX-B
               MOVE    SPACE                   TO  EIE01-REC
               MOVE    WK-TBL-MSG-B (IDX1)     TO  EIE01-REC
               WRITE   EIE01-REC
           END-PERFORM.
      *
           IF  (SPA-GMN-FILE-KEY(1:6)  = "JYURRK")       OR
               (SPA-GMN-FILE-KEY(1:8)  = "PTBYOMEI")     OR
               (SPA-GMN-FILE-KEY(1:12) = "SRYACCT_MAIN") OR
               (SPA-GMN-FILE-KEY(1:6)  = "SRYACT")       OR
               (SPA-GMN-FILE-KEY(1:8)  = "WKSRYACT")     OR
               (SPA-GMN-FILE-KEY(1:6)  = "SANTEI")       OR
               (SPA-GMN-FILE-KEY(1:11) = "SEIKYU_MAIN")  OR
               (SPA-GMN-FILE-KEY(1:10) = "SEIKYU_KOH")   OR
               (SPA-GMN-FILE-KEY(1:10) = "SEIKYU_ETC")   OR
               (SPA-GMN-FILE-KEY(1:5)  = "BD002")        OR
      *修正/*開始*03.05.12*********************************************
               (SPA-GMN-FILE-KEY(1:9)  = "NYUINACCT")    OR
               (SPA-GMN-FILE-KEY(1:8)  = "NYUINACT")     OR
               (SPA-GMN-FILE-KEY(1:10) = "PTTEIKIRRK")
      *修正/*終了*03.05.12*********************************************
               PERFORM VARYING IDX1  FROM  1  BY 1
                       UNTIL   IDX1  >     900
                   IF  WK-TBL-MSG-C (IDX1)(1:5)   =   "SRYYM"
                       MOVE    SPACE                 TO  EIE01-REC
                       MOVE    WK-TBL-MSG-C (IDX1)   TO  EIE01-REC
                       MOVE    "SRYYM***"            TO  EIE01-REC(1:8)
                       WRITE   EIE01-REC
                       MOVE    901                   TO  IDX1
                   END-IF
               END-PERFORM
           END-IF.
      *
      *修正/*開始*03.06.05*********************************************
      *     IF  (WK-TBL-CHKBTN01  = "T")      AND
      *         (WK-TBL-MSG-KBN   = "T")
      *         PERFORM VARYING IDX1  FROM  1  BY 1
      *                 UNTIL   IDX1  >     900
      *************IF  WK-TBL-MSG-C (IDX1)(1:6)   =   "HOSPID"
      *             IF  WK-TBL-MSG-C (IDX1)(1:7)   =   "HOSPNUM"
      *                 MOVE    SPACE                 TO  EIE01-REC
      *                 MOVE    WK-TBL-MSG-C (IDX1)   TO  EIE01-REC
      *****************MOVE    "HOSPID**"            TO  EIE01-REC(1:8)
      *                 MOVE    "HOSPNUM*"            TO  EIE01-REC(1:8)
      *                 WRITE   EIE01-REC
      *                 MOVE    901                   TO  IDX1
      *             END-IF
      *         END-PERFORM
      *     END-IF.
      *修正/*終了*03.06.05*********************************************
      *
           CLOSE                           EIE01-FILE.
      *
           IF  SPA-GMN-MAX-B   NOT =   ZERO
               MOVE    "*"                   TO  WK-TBL-KBN2 (IDX2)
           ELSE
               MOVE    SPACE                 TO  WK-TBL-KBN2 (IDX2)
           END-IF.
      *
       T-001-WRITCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    作成テーブル 判定処理
      *****************************************************************
       T-001-HANTCHEK-SEC                SECTION.
      *
           IF  SPA-GMN-MAX-B   NOT =   ZERO
               MOVE    "*"                   TO  WK-TBL-KBN2 (IDX2)
           END-IF.
      *
           COMPUTE   IDX3 = IDX2 + 1.
           MOVE    ZERO                  TO  IDX2.
      *
           PERFORM VARYING IDX1  FROM  IDX3  BY 1
                   UNTIL   IDX1  >    200
               IF  WK-TBL-KBN1 (IDX1)   =   "*"
                   MOVE    IDX1                  TO  IDX2
                   MOVE    201                    TO  IDX1
               END-IF
           END-PERFORM.
      *
           IF  IDX2   =   ZERO
               PERFORM VARYING IDX1  FROM  1  BY 1
                       UNTIL   IDX1  >    200
                   IF  WK-TBL-KBN1 (IDX1)   =   "*"
                       MOVE    IDX1                  TO  IDX2
                       MOVE    201                    TO  IDX1
                   END-IF
               END-PERFORM
           END-IF.
      *      
       T-001-HANITCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル項目名 設定処理
      *****************************************************************
       T-000-SETECHEK-SEC                SECTION.
      *
           MOVE    SPACE                 TO  SPA-GMN-AREA.
           MOVE    WK-TBL-MSG (IDX2)     TO  SPA-GMN-TBLNM.
grpsys     MOVE    "ORCGE01"         TO  EIE01PARA-FILE-ID.
grpsys     MOVE    SPA-HOSPNUM       TO  EIE01PARA-HOSPNUM.
grpsys     MOVE    SPA-TERMID        TO  EIE01PARA-TERMID.
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    EIE01PARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    ALL SPACE           TO  EIE01PARA-FULLNAME
           STRING  SGETTEMP-FULLNAME   DELIMITED  BY    SPACE
                   WK-TBL-MSG (IDX2)   DELIMITED  BY    SPACE
                   ".SLT"              DELIMITED  BY    SIZE
                                       INTO       EIE01PARA-FULLNAME
           END-STRING.
      *----(04.08.01)--ADD-END-----
      *----(04.08.01)--UPD-START---
      *     MOVE    ALL SPACE             TO  EIE01PARA-FILE.
      *     STRING  WK-TBL-MSG (IDX2)     DELIMITED  BY    SPACE
      *             ".SLT"                DELIMITED  BY    SIZE
      *                                   INTO       EIE01PARA-FILE.
      *----(04.08.01)--UPD-END-----
           INITIALIZE                    ORCSMKPASSAREA.
           MOVE    SPACE                 TO  EIE02-FILE-ID.
           STRING  WK-TBL-NAME (IDX2)    DELIMITED  BY  SPACE
                   ".db"                 DELIMITED  BY  SIZE
           INTO    EIE02-FILE-ID
           END-STRING
           MOVE    EIE02PARA-NAME        TO  MKPASS-IN-01.
           CALL    "ORCSMKPASS"          USING   ORCSMKPASSAREA.
           MOVE    MKPASS-OT-01          TO  EIE02PARA.
      *
      *    パッチ対応
           PERFORM T-000-PATCHCHEK-SEC
      *
           MOVE    ALL SPACE             TO  SPA-GMN-FILE-ID.
           STRING  "/tmp/"               DELIMITED  BY    SIZE
                   WK-TBL-MSG (IDX2)     DELIMITED  BY    SPACE
                   ".TXT"                DELIMITED  BY    SIZE
                                         INTO       SPA-GMN-FILE-ID
           END-STRING.
           MOVE    ALL SPACE             TO  SPA-GMN-FILE-KEY.
           STRING  WK-TBL-MSG (IDX2)     DELIMITED  BY    SPACE
                   "-ALL"                DELIMITED  BY    SIZE
                                         INTO       SPA-GMN-FILE-KEY
           END-STRING.
      *
           IF    ( WK-TBL-MSG (IDX2)     =   "JYURRK-PRV" )
               MOVE    "JYURRK-PRV-ALL3" TO  SPA-GMN-FILE-KEY
           END-IF
      *
           MOVE    1                     TO  IDX4.
           MOVE    1                     TO  IDY1.
           MOVE    ZERO                  TO  IDY2.
           MOVE    ZERO                  TO  IDY3.
           MOVE    ZERO                  TO  FLG-FIL.
           MOVE    SPACE                 TO  FLG-SEL.
           MOVE    SPACE                 TO  WK-TBL2-2.
           MOVE    SPACE                 TO  WK-SEW01.
           MOVE    SPACE                 TO  WK-SEW02.
           MOVE    SPACE                 TO  WK-SEW03.
           MOVE    SPACE                 TO  WK-SEW04.
      *修正/*開始*03.06.05*********************************************
           MOVE    SPACE                 TO  WK-TBL-MSG-KBN.
      *修正/*終了*03.06.05*********************************************
      *
           OPEN    INPUT                 EIE02-FILE.
      *
           IF  STS-EIE02   =   ZERO
               PERFORM UNTIL   FLG-FIL  =    1
                   READ    EIE02-FILE
                     AT   END
                       MOVE    1                 TO  FLG-FIL
                     NOT  AT  END
                       IF  EIE02-REC = "}"
                           MOVE    1                 TO  FLG-FIL
                       ELSE
                           PERFORM T-000-SETECHEK-01-SEC
                       END-IF
                   END-READ
               END-PERFORM
               CLOSE                           EIE02-FILE
           END-IF.
      *
       T-000-SETECHEK-EXT.
           EXIT.
      *
      ********************
      *
       T-000-SETECHEK-01-SEC             SECTION.
      *
           IF  (EIE02-REC  = X"0A")   OR
               (WK-SEW02   = "*")
               MOVE    ZERO              TO  IDY5
               MOVE    ZERO              TO  IDY6
               PERFORM VARYING IDY4 FROM   1  BY 1
                       UNTIL   IDY4  >   128
                   IF  WK-SEW04 (IDY4:1)   =   X"2C"
                       MOVE    IDY4          TO  IDY5
                   END-IF
                   IF  WK-SEW04 (IDY4:1)   =   X"29"
                       MOVE    IDY4          TO  IDY6
                       MOVE    129           TO  IDY4
                   END-IF
               END-PERFORM
               COMPUTE  IDY9   =   IDY6   +   1
               COMPUTE  IDY6   =   IDY6   -   1
               IF  ((WK-TBL-MSG-C3 (IDX4)    =   "char   ")   OR
                    (WK-TBL-MSG-C3 (IDX4)    =   "number ")   OR
                    (WK-TBL-MSG-C3 (IDX4)    =   "varchar")   OR
                    (WK-TBL-MSG-C3 (IDX4)    =   "integer"))  AND
                   (WK-TBL-MSG-C1 (IDX4) NOT =   "#")         AND
                   (WK-SEW04 (IDY9:1)        NOT =  X"2C")
      *修正/*開始*03.06.05*********************************************
                   IF  WK-TBL-MSG-C (IDX4)(1:4)   =   "PTID"
                       MOVE    "T"                   TO  WK-TBL-MSG-KBN
                   END-IF
      *修正/*終了*03.06.05*********************************************
                   IF  IDY5   =   ZERO
                       IF    ( WK-TBL-MSG-C3 (IDX4)  =   "integer")
                           MOVE    4         TO  WK-SEW05
                           MOVE    WK-SEW05  TO  WK-TBL-MSG-C5 (IDX4)
                           MOVE    ZERO      TO  WK-TBL-MSG-C7 (IDX4)
                       ELSE
                           MOVE    5         TO  IDY7
                           MOVE    IDY6      TO  IDY8
                           MOVE    ZERO      TO  WK-SEW05
                           MOVE    WK-SEW05  TO  WK-TBL-MSG-C7 (IDX4)
                           PERFORM UNTIL   IDY8  <     1
                               IF  WK-SEW04 (IDY8:1)  = X"28"
                                   MOVE  ZERO       TO  IDY8
                               ELSE
                                   MOVE  WK-SEW04 (IDY8:1)
                                                    TO WK-SEW05 (IDY7:1)
                                   COMPUTE  IDY7   =   IDY7   -   1
                                   COMPUTE  IDY8   =   IDY8   -   1
                               END-IF
                           END-PERFORM
                           MOVE    WK-SEW05   TO  WK-TBL-MSG-C5 (IDX4)
                       END-IF
                   ELSE
                       MOVE    5          TO  IDY7
                       MOVE    IDY6       TO  IDY8
                       MOVE    ZERO       TO  WK-SEW05
                       PERFORM UNTIL   IDY8  <     1
                           IF  WK-SEW04 (IDY8:1)  = X"2C"
                               MOVE  ZERO       TO  IDY8
                           ELSE
                               MOVE  WK-SEW04 (IDY8:1)
                                                TO  WK-SEW05 (IDY7:1)
                               COMPUTE  IDY7   =   IDY7   -   1
                               COMPUTE  IDY8   =   IDY8   -   1
                           END-IF
                       END-PERFORM
                       MOVE    WK-SEW05    TO  WK-TBL-MSG-C7 (IDX4)
                       COMPUTE  IDY8   =   IDY5   -   1
                       MOVE    5           TO  IDY7
                       MOVE    ZERO        TO  WK-SEW05
                       PERFORM UNTIL   IDY8  <     1
                           IF  WK-SEW04 (IDY8:1)  = X"28"
                               MOVE    ZERO     TO  IDY8
                           ELSE
                               MOVE  WK-SEW04 (IDY8:1)
                                                TO  WK-SEW05 (IDY7:1)
                               COMPUTE  IDY7   =   IDY7   -   1
                               COMPUTE  IDY8   =   IDY8   -   1
                           END-IF
                       END-PERFORM
                       MOVE    WK-SEW05    TO  WK-TBL-MSG-C5 (IDX4)
                   END-IF
                   MOVE    IDY1         TO  WK-TBL-MSG-C9 (IDX4)
                   MOVE    "F"          TO  WK-TBL-BTN-C (IDX4)
                   COMPUTE  IDX4   =   IDX4   +   1
                   COMPUTE  IDY1   =   IDY1   +   WK-SEW05
               END-IF
               MOVE    ZERO            TO  IDY2
               MOVE    ZERO            TO  IDY3
               MOVE    SPACE           TO  WK-SEW01
               MOVE    SPACE           TO  WK-SEW02
               MOVE    SPACE           TO  WK-SEW03
               MOVE    SPACE           TO  WK-SEW04
               MOVE    SPACE           TO  WK-TBL-MSG-C (IDX4)
           ELSE
               IF  (EIE02-REC   >   X"60")  AND
                   (EIE02-REC   <   X"7A")  AND
                   (WK-SEW01    =   SPACE)
                   MOVE    "*"           TO  WK-SEW01
                   MOVE    30            TO  IDY2
               END-IF
               IF  EIE02-REC   =   X"28"
                   MOVE    "*"           TO  WK-SEW03
               END-IF
               IF  EIE02-REC   =   X"3B"
                   MOVE    "*"           TO  WK-SEW02
               END-IF
               IF  (EIE02-REC   >   X"20")  AND
                   (EIE02-REC   <   X"7F")  AND
                   (WK-SEW03    =   SPACE)
                   COMPUTE  IDY2   =   IDY2   +   1
                   MOVE    EIE02-REC    TO  WK-TBL-MSG-C (IDX4)(IDY2:1)
               END-IF
               IF  (EIE02-REC   >   X"20")  AND
                   (EIE02-REC   <   X"7F")  AND
                   (WK-SEW03   =  "*")
                   COMPUTE  IDY3   =   IDY3   +   1
                   MOVE    EIE02-REC    TO  WK-SEW04 (IDY3:1)
               END-IF
           END-IF.
      *
       T-000-SETECHEK-01-EXT.
           EXIT.
      *
      **************************************************
      *    パッチ対応
      **************************************************
       T-000-PATCHCHEK-SEC         SECTION.
      *
           INITIALIZE                      ORCSSTRINGAREA
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    EIE02PARA           TO  ORCSSTR-STRING1
           MOVE    "record"            TO  ORCSSTR-STRING2
           MOVE    "patch-lib/record"  TO  ORCSSTR-STRING3
           CALL    "orcsstring"        USING   ORCSSTRINGAREA
           IF      ORCSSTR-STRING1     NOT =   SPACE
               INITIALIZE                  ORCSFILECHKAREA
               MOVE    ORCSSTR-STRING1     TO  FILECHK-NAME
               CALL    "ORCSFILECHK"       USING   ORCSFILECHKAREA
               IF      FILECHK-RC          =   0
                   MOVE    ORCSSTR-STRING1     TO  EIE02PARA
               END-IF
           END-IF.
      *
       T-000-PATCHCHEK-EXT.
           EXIT.
