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
       PROGRAM-ID.                 ORCGF01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : バックアップ
      *  コンポーネント名  : バックアップ（Ｆ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/12/03    ひさなが 　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     ひさなが     02/02/04  ジョブ管理テーブルの追加
      *                                     雛形テーブルの追加
      *                                     レセプトコメントの追加
      * 01.00.02     ひさなが     02/04/23  患者労災保険テーブルの追加
      * 01.00.03     ひさなが     02/05/10  ジョブ管理検索追加
      * 01.00.04     ひさなが     02/05/29  相互作用テーブルの追加
      *                                     機序テーブルの追加
      *                                     人名テーブルの追加
      *                                     月代り受給者番号の追加
      * 01.00.05     ひさなが     03/05/12  入院関連テーブルの追加
      * 03.05.01     NACL-森脇    07/06/11  グループ診療対応
      * 04.08.01     NACL-森脇    14/08/25  一時ファイルディレクトリ設定
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    バックアップ条件用ファイル 
           SELECT  FIF00-FILE      ASSIGN  FIF00PARA
                                   ORGANIZATION LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-FIF00.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    バックアップ条件用ファイル
       FD  FIF00-FILE.
       01  FIF00-REC.
           03  FIF00-DATA-AREA     PIC X(1001).
           03  FIF00-OLD-DEF-RED   REDEFINES   FIF00-DATA-AREA.
               05  FIF00-OLD-DEFINITION    PIC X(1001).
           03  FIF00-NEW-DEF-RED   REDEFINES   FIF00-DATA-AREA.
               05  FIF00-NEW-DEFINITION.
                   07  FIF00-FILE-ID       PIC X(500).
                   07  FIF00-CHK           PIC X(01) OCCURS    500.
                   07  FIF00-DEFKBN        PIC X(01).
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    バックアップ条件用ファイル 名称領域 
grpsys     COPY    "CPCOMMONDAT2.INC"
grpsys                             REPLACING  //RECE01//
grpsys                             BY         //FIF00//.
grpsys*     03  FILLER              PIC X(04)   VALUE   ".dat".
      *
      *    バックアップ シェル領域
           COPY    "CPCOMMONSHELLUP.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
       01  SPA-F01.
           03  SPA-F01-AREA.
               05  SPA-GMN-PAGE               PIC 9(02).
               05  SPA-GMN-MAX-A              PIC 9(03).
               05  SPA-GMN-MAX-B              PIC 9(03).
               05  SPA-GMN-CUR                PIC 9(03).
               05  SPA-GMN-KBN                PIC X(02).
               05  SPA-GMN-RUN                PIC 9(01).
               05  SPA-GMN-AREA.
                   07  SPA-GMN-FILE-ID        PIC X(128).
                   07  SPA-GMN-TBLG.
                       09  SPA-GNM-TBL2       OCCURS  120.
                           11  SPA-GMN-NO     PIC 9(03).
                           11  SPA-GMN-MSG    PIC X(30).
                           11  SPA-GMN-BTN    PIC X(01).
               05  SPA-NAI-AREA.
                   07  SPA-NAI-MOTOPG         PIC X(08).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    エンドステータス領域
       01  STS-AREA.
           03  STS-FIF00           PIC X(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                PIC 9(05).
           03  IDX1                PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03).
           03  IDX-NO              PIC 9(03).
      *
      *    メッセージ領域
       01  T-MESSAGE-AREA.
           03  T-MESSAGE-01        PIC X(60)   VALUE
               "指定された条件でバックアップ中のためしばらくお待ちくださ
      -        "い。".
           03  T-MESSAGE-02        PIC X(32)   VALUE
               "指定された条件でのバックアップは".
           03  T-MESSAGE-03        PIC X(16)   VALUE
               "に終了しました。".
           03  T-MESSAGE-04        PIC X(20)   VALUE
               "作成テーブル未設定。".
           03  T-MESSAGE-05        PIC X(44)   VALUE
               "指定された条件でバックアップ処理を行います。".
      *
       01  WRK-AREA.
           03  WRK-TBLID             PIC X(30).
      *
      *    作業テーブル名称領域
       01  WK-TBL.
           03  WK-TBLSET       OCCURS  120.
               05  WK-TBL-NO         PIC 9(03).
               05  WK-TBL-MSG-1      PIC X(30).
               05  WK-TBL-MSG-2      PIC X(30).
               05  WK-TBL-MSG-3.
                   07  WK-TBL-MSG-3-NAME   PIC X(06).
                   07  WK-TBL-MSG-3-IDX    PIC 9(03).
      *
      *    シェル用領域
           COPY    "CPSHELLTBL.INC".
      *
      *    ジョブ管理
       01  WRK-JOBKANRI-AREA.
           03  WRK-JOBKANRI-MODE       PIC X(03).
           03  WRK-JOBKANRI-RETURN     PIC S9(03).
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    フィアルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
           COPY  "CPSTGTSCM.INC".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-START---
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
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
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-F01.
      *
           MOVE    SPACE           TO  SPA-ERRCD.
           MOVE    ZERO            TO  FLG-END.
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
                   IF  SPA-GMN-RUN   =   ZERO   OR   2
                       PERFORM 200-ENTRY
                   ELSE
                       MOVE    "F01"               TO  SPA-MOTOPG
                       MOVE    "FID2"              TO  SPA-SAKIPG
                       MOVE    "NEW"               TO  MCP-PUTTYPE
                       MOVE    "FID2"              TO  MCP-WINDOW
                       PERFORM 900-PUT-WINDOW
                       MOVE    1                   TO  FLG-END
                   END-IF
           END-EVALUATE.
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF.
      *
           MOVE    SPA-AREA        TO  SPA-COMMON.
           MOVE    SPA-F01         TO  SPA-FREE.
      *
       000-PROC-SEC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "FERR" OR "FID1" OR "FID2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5011-MAPCUR-SEC
               IF  (SPA-GMN-RUN   =   ZERO   OR   2)   AND
                   (SPA-GMN-KBN   =   "OK")
                   MOVE    "CHK"               TO  WRK-JOBKANRI-MODE
                   INITIALIZE                      JOBKANRI-REC
                   MOVE    1                   TO  JOB-JOBID
                   MOVE    "ORCBSF1"           TO  JOB-SHELLID
grpsys             MOVE    SPA-TERMID          TO  JOB-TERMID
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
                       MOVE    "ORCBSF1"           TO  JOB-SHELLID
grpsys                 MOVE    SPA-TERMID          TO  JOB-TERMID
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
                   MOVE    "ORCBSF1"           TO  JOB-SHELLID
grpsys             MOVE    SPA-TERMID          TO  JOB-TERMID
                   MOVE    "媒体変換"          TO  JOB-SHELLMSG
                   MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"           USING
                                               WRK-JOBKANRI-AREA
                                               JOBKANRI-REC
grpsys                                         SPA-AREA
                   PERFORM T-000-READWRIT-SEC
                   PERFORM T-000-WRITABLE-SEC
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
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "F01"               TO  MCP-WINDOW.
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
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    LNK-SCRSPA          TO  SPA-F01-AREA
      *
           END-IF.
      *
           IF    ( SPA-MOTOPG      =   "XB01" )
               GO  TO  300-SCREEN-EXT
           END-IF
      *
           MOVE    1                       TO  SPA-GMN-CUR.
      *    テーブル名称セット
           INITIALIZE                          SPA-GMN-TBLG
                                               WK-TBL
           INITIALIZE              SPA-F01.
           
           MOVE    "/home/orca/orcadb.out" TO  SPA-GMN-FILE-ID.
      *
      *
           CALL    "ORCSTGTSCM"            USING   STGTSCM-AREA
      *
           MOVE    ZERO                    TO  SPA-GMN-MAX-A
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   STGTSCM-MAX )
      *
               IF    ( STGTSCM-NO   (IDX0) NOT =   ZERO )
                AND  ( SPA-GMN-MAX-A   <   120 )
      *
                   COMPUTE SPA-GMN-MAX-A   =   SPA-GMN-MAX-A   +   1
      *
                   MOVE    STGTSCM-NO   (IDX0) TO  WK-TBL-NO
                                                       (SPA-GMN-MAX-A)
                   MOVE    STGTSCM-NAME (IDX0) TO  WK-TBL-MSG-1
                                                       (SPA-GMN-MAX-A)
                   MOVE    STGTSCM-ID   (IDX0) TO  WK-TBL-MSG-2
                                                       (SPA-GMN-MAX-A)
                   MOVE    "CHKBTN"            TO  WK-TBL-MSG-3-NAME
                                                       (SPA-GMN-MAX-A)
                   MOVE    IDX0                TO  WK-TBL-MSG-3-IDX
                                                       (SPA-GMN-MAX-A)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 310-SPASET-SEC.
      *
           PERFORM T-000-SETTABLE-SEC.
      *
           PERFORM T-000-READCHEK-SEC.
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
           MOVE    1                   TO  SPA-GMN-CUR.
      *
           MOVE    1                   TO  SPA-GMN-PAGE.
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
                   PERFORM 210-BACK
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 530-RIDSET3-SEC
               WHEN    "CLICKED"       ALSO    "B12"
                   IF      SPA-GMN-MAX-B       =   ZERO
                       MOVE    "0001"              TO  SPA-ERRCD
                       PERFORM 500-SET-SCREEN
                   ELSE
                       PERFORM 530-RIDSET2-SEC
                   END-IF
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 530-RIDSET2-SEC
               WHEN    "ACTIVATE"      ALSO    "FILENAME"
               WHEN    "CLICKED"       ALSO    "FILENAME"
                   PERFORM T-000-NAMECHEK-SEC
               WHEN    "CLICKED"
                        ALSO    "CHKBTN001" THRU "CHKBTN120"
                   PERFORM T-000-CLICKED-SEC
               WHEN    OTHER
                   MOVE    1                   TO  SPA-GMN-CUR
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
               WHEN    "ACTIVATE"      ALSO    "FILENAME"
                   PERFORM T-000-NAMECHEK-SEC
               WHEN    "ACTIVATE"
                        ALSO    "CHKBTN001" THRU "CHKBTN120"
                   PERFORM T-000-CLICKED-SEC
               WHEN    OTHER
                   MOVE    1                   TO  SPA-GMN-CUR
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
           MOVE    "M01"               TO  SPA-SAKIPG.
           MOVE    SPACE               TO  SPA-MOTOPG2.
           MOVE    "F01"               TO  SPA-MOTOPG.
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
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF.
      *
           MOVE    SPACE           TO  LINKAREA.
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "F01"           TO  MCP-WINDOW.
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
           MOVE    SPACE                 TO  F01.
      *
           MOVE    SPA-GMN-FILE-ID       TO  F01-FILENAME.
      *
           PERFORM VARYING IDX1  FROM  1  BY 1
                   UNTIL   IDX1  >    SPA-GMN-MAX-A
               EVALUATE    SPA-GMN-MSG (IDX1)
               WHEN   "SRYACCT"
                   MOVE    "SRYACCT_MAIN, SUB"
                                            TO  F01-CHKMSG  (IDX1)
               WHEN   "SEIKYU"
                   MOVE    "SEIKYU_MAIN, SUB, KOH"
                                            TO  F01-CHKMSG  (IDX1)
               WHEN   "SYUNOU"
                   MOVE    "SYUNOU_MAIN, NYUIN"
                                            TO  F01-CHKMSG  (IDX1)
               WHEN    OTHER
                   MOVE    SPA-GMN-MSG (IDX1)
                                            TO  F01-CHKMSG  (IDX1)
               END-EVALUATE
               MOVE    SPA-GMN-BTN (IDX1)   TO  F01-CHKBTN  (IDX1)
           END-PERFORM.
      *
           MOVE    "red"                 TO  F01-LBLCAUTION-STYLE
           STRING  "※データベースの全体バックアップは"
                                             DELIMITED BY SIZE
                   " pg_dump コマンドで行ってください。"
                                             DELIMITED BY SIZE
           INTO    F01-LBLCAUTION
           END-STRING
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
           IF      MCP-WIDGET       =   "FILENAME"
               MOVE    1                              TO  SPA-GMN-CUR
           END-IF.
      *
           IF      SPA-GMN-CUR > 120
               MOVE    "B12"                          TO  MCP-WIDGET
           ELSE
               MOVE    WK-TBL-MSG-3 (SPA-GMN-CUR)     TO  MCP-WIDGET
           END-IF.
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
               WHEN    "0001"
                   MOVE    T-MESSAGE-04        TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPACE               TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  FERR.
           MOVE    SPA-ERRCD           TO  F01-ERRCODE.
           MOVE    SPA-ERRMSG          TO  F01-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "F01"               TO  SPA-MOTOPG.
           MOVE    "FERR"              TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "FERR"              TO  MCP-WINDOW.
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
           MOVE    "ORCBSF1"          TO  JOB-SHELLID.
grpsys     MOVE    SPA-TERMID         TO  JOB-TERMID.
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM.
           CALL    "ORCSJOB"          USING
                                      WRK-JOBKANRI-AREA  
                                      JOBKANRI-REC
grpsys                                SPA-AREA.
           MOVE    SPACE              TO  FID2.
           MOVE    ZERO               TO  SPA-GMN-RUN.
           IF  WRK-JOBKANRI-RETURN   =   ZERO
               IF  JOB-ENDYMD            =   SPACE
                   MOVE    "7777"             TO  FID2-ID2CODE
                   MOVE    T-MESSAGE-01       TO  FID2-ID2MSG
                   MOVE    1                  TO  SPA-GMN-RUN
               ELSE
                   IF  MCP-WIDGET             =   "B13"
                       MOVE    "8888"             TO  FID2-ID2CODE
                       MOVE    T-MESSAGE-02       TO  FID2-ID2MSG(1:32)
                       MOVE    JOB-ENDTIME(1:2)   TO  FID2-ID2MSG(33:2)
                       MOVE    "時"               TO  FID2-ID2MSG(35:2)
                       MOVE    JOB-ENDTIME(3:2)   TO  FID2-ID2MSG(37:2)
                       MOVE    "分"               TO  FID2-ID2MSG(39:2)
                       MOVE    T-MESSAGE-03       TO  FID2-ID2MSG(41:16)
                       MOVE    2                  TO  SPA-GMN-RUN
                   END-IF
               END-IF
               IF  MCP-WIDGET   =   "B01"   OR   "B13"
                   MOVE    "F01"               TO  SPA-MOTOPG
                   MOVE    "FID2"              TO  SPA-SAKIPG
                   MOVE    "NEW"               TO  MCP-PUTTYPE
                   MOVE    "FID2"              TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF.
      *
           IF  MCP-WIDGET             =   "B12"
               IF  SPA-GMN-RUN        NOT =   1
                   MOVE    "1001"                TO  FID1-ID1CODE
                   MOVE    T-MESSAGE-05          TO  FID1-ID1MSG
                   MOVE    "F01"                 TO  SPA-MOTOPG
                   MOVE    "FID1"                TO  SPA-SAKIPG
                   MOVE    "NEW"                 TO  MCP-PUTTYPE
                   MOVE    "FID1"                TO  MCP-WINDOW
               ELSE
                   MOVE    "F01"                 TO  SPA-MOTOPG
                   MOVE    "FID2"                TO  SPA-SAKIPG
                   MOVE    "NEW"                 TO  MCP-PUTTYPE
                   MOVE    "FID2"                TO  MCP-WINDOW
               END-IF
               MOVE    SPA-AREA              TO  SPA-COMMON
               MOVE    SPA-F01               TO  SPA-FREE
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
           MOVE    "F01"               TO  SPA-MOTOPG.
           MOVE    "XB01"              TO  SPA-SAKIPG.
           INITIALIZE                  SPA-KYOTUKEY.
           INITIALIZE                  SPA-WORK.
           MOVE    "ORCBSF1"           TO  SPA-JOB-SHELLID.
           MOVE    SPACE               TO  LINKAREA.
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU.
           MOVE    SPA-F01-AREA        TO  LNK-SCRSPA.
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
           CALL   "ORCDBMAIN"                   USING
                                                    MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル名確認 処理
      *****************************************************************
       T-000-NAMECHEK-SEC                SECTION.
      *
           MOVE    F01-FILENAME            TO  SPA-GMN-FILE-ID.
      *
       T-000-NAMECHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックボタンクリック 処理
      *****************************************************************
       T-000-CLICKED-SEC                 SECTION.
      *
           MOVE    121                TO  SPA-GMN-CUR.
      *
           PERFORM VARYING IDX1  FROM  1  BY 1
                   UNTIL   IDX1  >    120
      *
               IF  MCP-WIDGET  =  WK-TBL-MSG-3 (IDX1)
                   EVALUATE    SPA-GMN-BTN (IDX1)
                       WHEN    "T"
                               MOVE    "F"
                                       TO  SPA-GMN-BTN (IDX1)
                               COMPUTE  SPA-GMN-MAX-B
                                        = SPA-GMN-MAX-B - 1
                       WHEN    OTHER
                               MOVE    "T"
                                       TO  SPA-GMN-BTN (IDX1)
                               COMPUTE  SPA-GMN-MAX-B
                                        =  SPA-GMN-MAX-B  +  1
                   END-EVALUATE
                   COMPUTE  IDX2 = IDX1 + 1
                   PERFORM VARYING IDX3  FROM  IDX2  BY 1
                           UNTIL   IDX3  >    120
                           IF  SPA-GMN-MSG (IDX3)  NOT =  SPACE
                               MOVE    IDX3
                                       TO  SPA-GMN-CUR
                               MOVE    121
                                       TO  IDX3
                           END-IF
                   END-PERFORM
                   MOVE    121                          TO  IDX1
               END-IF
           END-PERFORM.
      *
       T-000-CLICKED-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル バックアップ処理
      *****************************************************************
       T-000-WRITABLE-SEC                SECTION.
      *
      *    IF      SPA-GMN-MAX-A = SPA-GMN-MAX-B
      *        INITIALIZE                    ORCSMKPASSAREA
      *        MOVE    "scripts/allways/orcbsf02.sh"
      *                                  TO  MKPASS-IN-01
      *        CALL    "ORCSMKPASS"      USING  ORCSMKPASSAREA
      *        MOVE    MKPASS-OT-01      TO  SHELL-NAME
      *        MOVE    "ALL-TABLE"       TO  SHELL-T-NAME
      *        MOVE    "ALL-TABLE"       TO  SHELL-C-NAME
      *        MOVE    "ALL-TABLE"       TO  SHELL-W-NAME
      *        MOVE    SPA-GMN-FILE-ID   TO  SHELL-O-NAME
      *        MOVE    "0"               TO  SHELL-KBEN
      *        MOVE    SHELL-NAME        TO  SHELLTBL-NAME
      *        MOVE    SHELL-ARG1        TO  SHELLTBL-ARG1
      *        MOVE    SHELL-ARG2        TO  SHELLTBL-ARG2
      *        MOVE    SHELL-ARG3        TO  SHELLTBL-ARG3
      *        MOVE    SHELL-ARG4        TO  SHELLTBL-ARG4
      *        MOVE    SHELL-ARG5        TO  SHELLTBL-ARG5
      *        MOVE    SHELL-ARG6        TO  SHELLTBL-ARG6
grpsys*        MOVE    SPA-HOSPNUM       TO  SHELLTBL-ARG7
      *        MOVE    SHELLTBL          TO  MCPDATA-REC
      *        MOVE    "SHELL"           TO  MCP-FUNC
      *        MOVE    "shell"           TO  MCP-TABLE
      *        MOVE    "shell"           TO  MCP-PATHNAME
      *        CALL    "ORCDBMAIN"       USING
      *                                      MCPAREA
      *                                      MCPDATA-REC
      *                                      SPA-AREA
      *    ELSE
               MOVE    SPACE             TO  SHELL-KBEN
               PERFORM VARYING IDX1  FROM  1  BY 1
                       UNTIL   IDX1  >    SPA-GMN-MAX-A
                   IF      SPA-GMN-BTN (IDX1) = "T"
                       EVALUATE    WK-TBL-MSG-2 (IDX1)
                       WHEN    "tbl_sryacct"
                           MOVE    "tbl_sryacct_main"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                           MOVE    "tbl_sryacct_sub"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                       WHEN    "tbl_seikyu"
                           MOVE    "tbl_seikyu_main"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                           MOVE    "tbl_seikyu_koh"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                           MOVE    "tbl_seikyu_etc"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                       WHEN    "tbl_syunou"
                           MOVE    "tbl_syunou_main"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                           MOVE    "tbl_syunou_nyuin"
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                       WHEN    OTHER
                           MOVE    WK-TBL-MSG-2 (IDX1)
                                         TO  WRK-TBLID
                           PERFORM T-000-ORCBSF01-SEC
                       END-EVALUATE
                   END-IF
               END-PERFORM
               INITIALIZE                ORCSMKPASSAREA
               MOVE    "scripts/allways/orcbsf03.sh"
                                         TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"      USING  ORCSMKPASSAREA
               MOVE    MKPASS-OT-01      TO  SHELL-NAME
               MOVE    "OUT-TABLE-WK"    TO  SHELL-T-NAME
               MOVE    "OUT-TABLE-WK"    TO  SHELL-C-NAME
               MOVE    "OUT-TABLE-WK"    TO  SHELL-W-NAME
               MOVE    SPA-GMN-FILE-ID   TO  SHELL-O-NAME
               MOVE    "1"               TO  SHELL-KBEN
               MOVE    SHELL-NAME        TO  SHELLTBL-NAME
               MOVE    SHELL-ARG1        TO  SHELLTBL-ARG1
               MOVE    SHELL-ARG2        TO  SHELLTBL-ARG2
               MOVE    SHELL-ARG3        TO  SHELLTBL-ARG3
               MOVE    SHELL-ARG4        TO  SHELLTBL-ARG4
               MOVE    SHELL-ARG5        TO  SHELLTBL-ARG5
               MOVE    SHELL-ARG6        TO  SHELLTBL-ARG6
grpsys         MOVE    SPA-HOSPNUM       TO  SHELLTBL-ARG7
               MOVE    SHELLTBL          TO  MCPDATA-REC
               MOVE    "SHELL"           TO  MCP-FUNC
               MOVE    "shell"           TO  MCP-TABLE
               MOVE    "shell"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"       USING
                                             MCPAREA
                                             MCPDATA-REC
                                             SPA-AREA
      *    END-IF
           .
      *
       T-000-WRITABLE-EXT.
           EXIT.
      *
      *****************************************************************
      *    バックアップシェル実行処理
      *****************************************************************
       T-000-ORCBSF01-SEC                SECTION.
      *
                       INITIALIZE                  ORCSMKPASSAREA
                       MOVE    "scripts/allways/orcbsf01.sh"
                                                   TO  MKPASS-IN-01
                       CALL    "ORCSMKPASS"        USING  ORCSMKPASSAREA
                       MOVE    MKPASS-OT-01        TO  SHELL-NAME
                       MOVE    WRK-TBLID           TO  SHELL-T-NAME
                       MOVE    "CNV-TABLE"         TO  SHELL-C-NAME
                       MOVE    "OUT-TABLE-WK"      TO  SHELL-W-NAME
                       MOVE    SPA-GMN-FILE-ID     TO  SHELL-O-NAME
                       IF      SHELL-KBEN = SPACE
                           MOVE    "0"                TO  SHELL-KBEN
                       ELSE
                           MOVE    "1"                TO  SHELL-KBEN
                       END-IF
                       MOVE    SHELL-NAME        TO  SHELLTBL-NAME
                       MOVE    SHELL-ARG1        TO  SHELLTBL-ARG1
                       MOVE    SHELL-ARG2        TO  SHELLTBL-ARG2
                       MOVE    SHELL-ARG3        TO  SHELLTBL-ARG3
                       MOVE    SHELL-ARG4        TO  SHELLTBL-ARG4
                       MOVE    SHELL-ARG5        TO  SHELLTBL-ARG5
                       MOVE    SHELL-ARG6        TO  SHELLTBL-ARG6
                       MOVE    SHELLTBL          TO  MCPDATA-REC
                       MOVE    "SHELL"           TO  MCP-FUNC
                       MOVE    "shell"           TO  MCP-TABLE
                       MOVE    "shell"           TO  MCP-PATHNAME
                       CALL    "ORCDBMAIN"       USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
      *
           .
      *
       T-000-ORCBSF01-EXT.
           EXIT.
      *****************************************************************
      *    テーブル名称 セット処理
      *****************************************************************
       T-000-SETTABLE-SEC                SECTION.
      *
           PERFORM VARYING IDX1  FROM  1  BY 1
                   UNTIL   IDX1  >    SPA-GMN-MAX-A
               MOVE    WK-TBL-NO    (IDX1) TO  SPA-GMN-NO  (IDX1)
               MOVE    WK-TBL-MSG-1 (IDX1) TO  SPA-GMN-MSG (IDX1)
               MOVE    "F"                 TO  SPA-GMN-BTN (IDX1)
           END-PERFORM.
      *
           COMPUTE  IDX2 = SPA-GMN-MAX-A + 1.
      *
           PERFORM VARYING IDX1  FROM  IDX2  BY 1
                   UNTIL   IDX1  >    120
               MOVE    SPACE               TO  SPA-GMN-MSG (IDX1)
               MOVE    SPACE               TO  SPA-GMN-BTN (IDX1)
           END-PERFORM.
      *
       T-000-SETTABLE-EXT.
           EXIT.
      *
      *****************************************************************
      *    バックアップ条件 入力処理
      *****************************************************************
       T-000-READCHEK-SEC                SECTION.
      *
           MOVE    ZERO              TO  SPA-GMN-MAX-B.
grpsys     MOVE    "ORCGF00"         TO  FIF00PARA-FILE-ID.
grpsys     MOVE    SPA-HOSPNUM       TO  FIF00PARA-HOSPNUM.
grpsys     MOVE    SPA-TERMID        TO  FIF00PARA-TERMID.
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    FIF00PARA-BASENAME
                                       TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    ALL SPACE           TO  FIF00PARA-FULLNAME
           STRING  SGETTEMP-FULLNAME   DELIMITED  BY    SPACE
                   ".dat"              DELIMITED  BY    SIZE
                                       INTO       FIF00PARA-FULLNAME
           END-STRING.
      *----(04.08.01)--ADD-END-----
           MOVE    1                 TO  SPA-GMN-CUR.
      *
           OPEN    INPUT                   FIF00-FILE.
      *
           IF      STS-FIF00 = ZERO
               READ    FIF00-FILE
               NOT  AT  END
                   IF    ( FIF00-DEFKBN    =   "1" )
                       PERFORM VARYING IDX1    FROM    1  BY 1
                               UNTIL ( IDX1    >   120 )
                           MOVE    SPACE       TO  SPA-GMN-BTN (IDX1)
                           MOVE    SPA-GMN-NO (IDX1)   TO    IDX-NO
                           IF   ( IDX-NO      >=   1   )
                            AND ( IDX-NO      <=   120 )
                               IF  (FIF00-CHK (IDX-NO)     = "*")
                               AND (SPA-GMN-MSG (IDX1) NOT = SPACE)
                                   MOVE    "T"    TO  SPA-GMN-BTN (IDX1)
                                   COMPUTE    SPA-GMN-MAX-B
                                          =   SPA-GMN-MAX-B + 1
                               END-IF
                           END-IF
                       END-PERFORM
                       MOVE    FIF00-FILE-ID
                                       TO  SPA-GMN-FILE-ID
                   ELSE
                       PERFORM VARYING IDX1    FROM    1  BY 1
                               UNTIL ( IDX1    >   120 )
                           MOVE    SPACE       TO  SPA-GMN-BTN (IDX1)
                           MOVE    SPA-GMN-NO (IDX1)   TO    IDX-NO
                           IF   ( IDX-NO      >=   1   )
                            AND ( IDX-NO      <=   90 )
                               IF  (FIF00-REC(IDX-NO:1)      = "*")
                               AND (SPA-GMN-MSG (IDX1) NOT = SPACE)
                                   MOVE    "T"    TO  SPA-GMN-BTN (IDX1)
                                   COMPUTE    SPA-GMN-MAX-B
                                          =   SPA-GMN-MAX-B + 1
                               END-IF
                           END-IF
                       END-PERFORM
                       MOVE    FIF00-REC(92:128)
                                       TO  SPA-GMN-FILE-ID
                   END-IF
               END-READ
               CLOSE                           FIF00-FILE
           END-IF.
      *
       T-000-READCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    バックアップ条件 出力処理
      *****************************************************************
       T-000-READWRIT-SEC                SECTION.
      *
           OPEN    OUTPUT                  FIF00-FILE.
      *
           MOVE    SPACE               TO  FIF00-REC.
      *
           PERFORM VARYING IDX1  FROM  1  BY 1
                   UNTIL   IDX1  >    SPA-GMN-MAX-A
               IF  SPA-GMN-BTN (IDX1)   =   "T"
                   MOVE    SPA-GMN-NO (IDX1)
                                       TO  IDX-NO
                   IF    ( IDX-NO      >=  1 )
                    AND  ( IDX-NO      <=  120 )
                       MOVE    "*"     TO  FIF00-CHK(IDX-NO)
                   END-IF
               END-IF
           END-PERFORM.
           MOVE    SPA-GMN-FILE-ID     TO  FIF00-FILE-ID
           MOVE    "1"                 TO  FIF00-DEFKBN
      *
           WRITE   FIF00-REC.
      *
           CLOSE                           FIF00-FILE.
      *
       T-000-READWRIT-EXT.
           EXIT.
      *
