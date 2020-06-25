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
       PROGRAM-ID.                 ORCGM96.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 業務選択
      *  コンポーネント名  : ジョブ管理検索（Ｍ９６）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/04/05    ひさなが      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    ひさなが     02/06/06  日付表示を更新日が空白なら
      *                                     新規作成日表示に変更
      *
      *  01.00.02    太田         04/02/03  MCPAREAの初期化を削除
      *  01.00.03    NACL-伊藤    04/02/05  M97の追加
      *  01.00.04    NACL-多々納  05/04/04  マスタ更新管理情報の分割
      *  01.00.05    NACL-伊藤    06/02/22  MONFUNC 対応
      *  01.00.06    NACL-伊藤    06/07/12  ライセンスマスタ対応
      *  03.04.01    NACL-森脇    07/01/24  タイマーイベント追加
      *  03.05.00    NACL-多々納  07/05/25  グループ診療対応
      *  04.03.00    NACL-太田    08/05/27  エラー処理強化
      *  04.06.00    NACL-伊藤    11/03/10  新マスター追加
      *  04.07.00    NACL-伊藤    11/10/12  新マスター追加
      *  04.07.00    NACL-伊藤    12/03/19  新マスター追加
      *  05.00.00    NACL-伊藤    17/01/03  アクセスキー取得処理追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    エラーファイル 名称領域
           COPY    "CPERRFL.INC".
      *
           COPY    "CPASGNPARA.INC"     REPLACING //ASGNPARA//
                                        BY        //MNTERRFL//.
           COPY    "CPASGNPARA.INC"     REPLACING //ASGNPARA//
                                        BY        //MLIERRFL//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    スパ領域
           COPY    "M901COMMON-SPA".
      *    スパ領域
           COPY    "M96SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-DBKANRI         PIC 9(01).
           03  FLG-MSTKANRI        PIC 9(01).
           03  FLG-JOBKANRI        PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-INIT            PIC 9(01).
           03  FLG-SJOBKANRI       PIC 9(01).
           03  FLG-SJOBKANRI1      PIC 9(01).
           03  FLG-RUNNING         PIC 9(01).
           03  FLG-MONBATCH        PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
           03  FLG-ACCESSKEY       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  ID1                 PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-MIDCD           PIC X(04).
           03  WRK-MSG             PIC X(200).
           03  WRK-ENV-OFFLINE     PIC X(03).
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
           COPY    "CPSHELLTBL.INC".
      *
       01  SHELL-AREA.
           03  SHELL-NAME          PIC X(17)  VALUE
               "master_upgrade.sh".
           03  SHELL-NAME1         PIC X(25)  VALUE
               "get_license_access_key.sh".
      *
       01  CONST-AREA.
           03  CONST-MNTJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MNTSHELLID    PIC X(08)  VALUE    "MSTMNT".
           03  CONST-MLIJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MLISHELLID    PIC X(08)  VALUE    "MSTMLI".
           03  CONST-MKYJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MKYSHELLID    PIC X(08)  VALUE    "MSTKEY".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
           COPY  "CPORCSDAY.INC".
           COPY  "CPORCSLNK.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ管理
       01  DBKANRI-REC.
           COPY    "CPDBKANRI.INC".
      *
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *
      *    ジョブ管理
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  JOBKANRI1-REC.
           COPY    "CPJOBKANRI.INC"    REPLACING  //JOB-//
                                       BY         //JOB1-//.
      *
           COPY    "CPACCESSKEY.INC".
      *
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
           COPY    "M80.INC".
           COPY    "M96.INC".
           COPY    "M97.INC".
           COPY    "MMERR.INC".
           COPY    "MMID1.INC".
           COPY    "MJOB.INC".
           COPY    "MGUIDE1.INC".
      *
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M901KYOUTU
           MOVE    SPA-FREE        TO  SPA-M96
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
           MOVE    SPA-M901KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-M96             TO  SPA-FREE
           .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "MMERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
      *
                   IF    ( SPA-MMID1CD NOT =   SPACE )
                       PERFORM 520-MIDSET-SEC
                   END-IF
      *
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   SPACE                TO  LINKAREA
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "M96"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
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
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "MMID1"
               WHEN    "MGUIDE1"
      *            確認画面より
                   PERFORM 30010-MID1-SET-SEC
               WHEN    OTHER
                   MOVE    SPA-MOTOPG          TO  SPA-NAI-MOTOPG
                   MOVE    SPACE               TO  SPA-MMID1CD
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＭＩＤ１）ＯＫ処理
      *****************************************************************
       30010-MID1-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-MIDCD
      *
           EVALUATE    SPA-MMID1CD
               WHEN    "0100"
                   IF      SPA-MMID1-FLG       =   "OK"
                       PERFORM 2011-RUNNING-JOB-CHK-SEC
                       IF    ( SPA-ERRCD       =   SPACE )
                           PERFORM 30011-DELETE-SYORI-SEC
                       END-IF
      *
                       IF  ( SPA-ERRCD     NOT =   SPACE )
                           PERFORM 510-ERRSET-SEC
                       END-IF
      *
                   END-IF
      *
               WHEN    "0102"
      *            削除処理
                   IF      SPA-MMID1-FLG       =   "OK"
                       PERFORM 2101-CANCEL-SEC
                   END-IF
               WHEN    "0202"
      *            マスタ更新処理
                   IF      SPA-MMID1-FLG       =   "OK"
      *                プログラム更新ＪＯＢ管理存在チェック
                       PERFORM 820-JOBKANRI-READ2-SEC
                       MOVE    ZERO                 TO  FLG-ERR
                       IF    ( FLG-SJOBKANRI         =   ZERO )
                           IF        ( JOB-ENDTIME   NOT =   SPACE )
                               AND   ( JOB-ENDYMD    NOT =   SPACE )
                               AND   ( JOB-ERRCD (1:2)   NOT =   "51")
                               CONTINUE
                           ELSE
                               MOVE    1             TO  FLG-ERR
                           END-IF
                       END-IF
                       IF    ( FLG-ERR                 =   1 )
                           MOVE    "0501"              TO  SPA-ERRCD
                           PERFORM 510-ERRSET-SEC
                       ELSE
                           PERFORM 30012-KOUSIN-SYORI-SEC
                       END-IF
                   END-IF
               WHEN    "0301"
      *            終了処理
                   IF      SPA-MMID1-FLG       =   "OK"
                       PERFORM 2001-BACK-SYORI-SEC
                   END-IF
               WHEN    "0401"
      *            アクセスキー取得処理
                   IF      SPA-MMID1-FLG       =   "OK"
                       PERFORM 30021-GET-ACCESSKEY-SEC
                   END-IF
               WHEN    "0402"
      *            アクセスキー取得処理
                   IF      SPA-MMID1-FLG       =   "OK"
                       MOVE    SPACE               TO  M96-B05-LABEL
                   END-IF
               WHEN    "0404"
      *            アクセスキー削除処理
                   IF      SPA-MMID1-FLG       =   "OK"
                       PERFORM 900-ACCESSKEY-DELETE-SEC
                   END-IF
               WHEN    "0410"
      *            アクセスキー取得確認
                   IF      SPA-MMID1-FLG       =   "OK"
                       PERFORM 30022-GET-ACCESSKEY-RES-SEC
                   END-IF
               WHEN    "GUD1"
                   MOVE    "0202"              TO  SPA-MMID1CD
                   PERFORM 520-MIDSET-SEC
                   GO  TO  30010-MID1-SET-EXT
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-MMID1-FLG
           IF      SPA-MMID1CD     NOT =   "0410"
               MOVE    WRK-MIDCD           TO  SPA-MMID1CD
           END-IF
      *
           .
       30010-MID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    1                   TO  FLG-INIT
           INITIALIZE                  SPA-GMN-M96-AREA
      *    最新情報編集
           PERFORM 310-DBHENSYU-SEC
      *
      *    最新から初期へ
           MOVE    SPA-GMN-STARTYMD (1)   TO  SPA-GMN-LASTYMD      (1)
           MOVE    SPA-GMN-STARTYMD (2)   TO  SPA-GMN-LASTYMD      (2)
      *
           MOVE    ZERO                TO  SPA-NAI-KADO-LEVEL
           MOVE    ZERO                TO  SPA-NAI-CONTRACT-COND
           IF      SPA-MW              =   SPA-PANDA
               PERFORM 900-ACCESSKEY-READ-SEC
               IF      FLG-ACCESSKEY       =   ZERO
                   MOVE    ACCESS-KEY-KADO-LEVEL
                                           TO  SPA-NAI-KADO-LEVEL
                   MOVE    ACCESS-KEY-CONTRACT-COND
                                           TO  SPA-NAI-CONTRACT-COND
               END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-M96-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ最新情報編集
      *****************************************************************
       310-DBHENSYU-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-TBLG3
                                       SPA-GMN-MSG-G
           MOVE   ZERO                 TO  SPA-NAI-CANCEL
      *
      *    ＤＢ管理検索処理
           INITIALIZE                  DBKANRI-REC
           MOVE    "ORCADB00"          TO  DBKANRI-KANRICD
           MOVE    DBKANRI-REC         TO  MCPDATA-REC
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 911-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_dbkanri"   TO  MCP-TABLE 
               MOVE    "key"           TO  MCP-PATHNAME 
               PERFORM 900-DBKANRI-READ-SEC
           ELSE
               INITIALIZE                      DBKANRI-REC
               MOVE    1                   TO  FLG-DBKANRI
           END-IF
           IF      FLG-DBKANRI         =   ZERO
               PERFORM 3101-LIST1-HENSYU-SEC
           END-IF
           MOVE    "tbl_dbkanri"       TO  MCP-TABLE 
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBCLOSECURSOR-SEC
      *
      *    マスタ管理検索処理(医療機関識別番号01の処理)
           INITIALIZE                  MSTKANRI-REC
grpsys     MOVE    1                   TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 911-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-MSTKANRI-READ-SEC
           ELSE
               INITIALIZE                      MSTKANRI-REC
               MOVE    1                   TO  FLG-MSTKANRI
           END-IF
           PERFORM UNTIL   FLG-MSTKANRI    =   1
               IF      MSTKANRI-KANRICD(7:2)    NUMERIC
                   PERFORM 3102-LIST3-HENSYU-SEC
               END-IF
               MOVE    "tbl_mstkanri"  TO  MCP-TABLE 
               MOVE    "all"           TO  MCP-PATHNAME 
               PERFORM 900-MSTKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
           MOVE    "all"               TO  MCP-PATHNAME 
           PERFORM 910-DBCLOSECURSOR-SEC
      *    マスタ管理検索処理(医療機関識別番号02以降の処理)
           IF      SPA-HOSPNUM         >   1
             INITIALIZE                  MSTKANRI-REC
grpsys       MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
             MOVE    MSTKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
             MOVE    "all"               TO  MCP-PATHNAME 
             PERFORM 911-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_mstkanri"  TO  MCP-TABLE 
                 MOVE    "all"           TO  MCP-PATHNAME 
                 PERFORM 900-MSTKANRI-READ-SEC
             ELSE
                 INITIALIZE                      MSTKANRI-REC
                 MOVE    1                   TO  FLG-MSTKANRI
             END-IF
             PERFORM UNTIL   FLG-MSTKANRI    =   1
                 IF      MSTKANRI-KANRICD(7:2)    NUMERIC
                     PERFORM 3102-LIST3-HENSYU-SEC
                 END-IF
                 MOVE    "tbl_mstkanri"  TO  MCP-TABLE 
                 MOVE    "all"           TO  MCP-PATHNAME 
                 PERFORM 900-MSTKANRI-READ-SEC
             END-PERFORM
             MOVE    "tbl_mstkanri"      TO  MCP-TABLE 
             MOVE    "all"               TO  MCP-PATHNAME 
             PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
      *    ＪＯＢ管理検索
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF      FLG-INIT             =   ZERO
               MOVE    SPACE                TO  SPA-GMN-MSG1
                                                SPA-GMN-MSG2
               MOVE    1                    TO  ID1
           ELSE
               MOVE    "前回処理分"         TO  SPA-GMN-MSG1
                                                SPA-GMN-MSG2
               MOVE    11                   TO  ID1
           END-IF
      *
           IF    ( FLG-SJOBKANRI       =   ZERO )
               IF    ( JOB-ENDTIME NOT =   SPACE )
                AND  ( JOB-ENDYMD  NOT =   SPACE )
                   EVALUATE    JOB-ERRCD
                   WHEN    "00"
                   WHEN    "0000"
                       IF    ( JOB-UPDCNT      >   ZERO )
                           MOVE    "マスタ更新結果に問題があります。"
                                           TO  SPA-GMN-MSG1(ID1:32)
                           MOVE    "blue"  TO  SPA-GMN-MSG1-STYLE
                       ELSE
                           MOVE    "マスタ更新は正常に終了しました。"
                                           TO  SPA-GMN-MSG1(ID1:32)
                           MOVE    "blue"  TO  SPA-GMN-MSG1-STYLE
                       END-IF
                   WHEN    "01"
                   WHEN    "8001"
                       MOVE    "更新データはありませんでした。"
                                           TO  SPA-GMN-MSG1(ID1:30)
                       MOVE    "blue"      TO  SPA-GMN-MSG1-STYLE
                       IF      JOB-YOBI(1:7)   =  "[ERROR]"
                           MOVE    JOB-YOBI(9:)  TO  SPA-GMN-MSG1PLUS
                           MOVE    "red"         TO  SPA-GMN-MSG1-STYLE
                       END-IF
                   WHEN    "98"
                   WHEN    "8002"
                       MOVE    "サーバへ接続できませんでした。"
                                           TO  SPA-GMN-MSG1(ID1:34)
                       MOVE    "blue"      TO  SPA-GMN-MSG1-STYLE
                       IF      JOB-YOBI(1:7)   =  "[ERROR]"
                           MOVE    JOB-YOBI(9:)  TO  SPA-GMN-MSG1PLUS
                           MOVE    "red"         TO  SPA-GMN-MSG1-STYLE
                       END-IF
                   WHEN    "8888"
                       MOVE    "マスタ更新は中止されました"
                                           TO  SPA-GMN-MSG1(ID1:34)
                       MOVE    "blue"       TO  SPA-GMN-MSG1-STYLE
                       MOVE    JOB-YOBI    TO  SPA-GMN-MSG1PLUS
                   WHEN    OTHER
                       MOVE    "マスタ更新は異常終了しました。"
                                           TO  SPA-GMN-MSG1(ID1:30)
                       MOVE    "red"       TO  SPA-GMN-MSG1-STYLE
                       MOVE    JOB-YOBI    TO  SPA-GMN-MSG1PLUS
                   END-EVALUATE
                   IF    ( JOB-UPDCNT      >   ZERO )
                       MOVE    SPA-GMN-MSG1 TO  WRK-MSG
                       MOVE    SPACE        TO  SPA-GMN-MSG1
                       STRING  WRK-MSG         DELIMITED   BY  SPACE
                               "【確認リストが作成されました】"
                                               DELIMITED   BY  SIZE
                       INTO    SPA-GMN-MSG1
                       END-STRING
                   END-IF
               ELSE
                   MOVE    "マスタ更新中です。"
                                           TO  SPA-GMN-MSG1(ID1:18)
                   MOVE    "blue"          TO  SPA-GMN-MSG1-STYLE
                   MOVE    JOB-SHELLMSG    TO  SPA-GMN-MSG1PLUS
                   EVALUATE    JOB-STOPFLG
                   WHEN    ZERO
                       MOVE    1           TO  SPA-NAI-CANCEL
                   WHEN    1
                       MOVE    "処理中止手続き中です"
                                           TO  SPA-GMN-MSG1PLUS
                   END-EVALUATE
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-MSG1
           END-IF
      *
      *    ライセンスマスタ
           IF    ( FLG-SJOBKANRI1          =   ZERO )
               IF    ( JOB1-ENDTIME    NOT =   SPACE )
                AND  ( JOB1-ENDYMD     NOT =   SPACE )
                   EVALUATE    JOB1-ERRCD
                   WHEN    "00"
                   WHEN    "0000"
                       IF    ( JOB1-UPDCNT      >   ZERO )
                           MOVE    "マスタ更新結果に問題があります。"
                                                TO  SPA-GMN-MSG2(ID1:32)
                           MOVE    "blue"       TO  SPA-GMN-MSG2-STYLE
                       ELSE
                           MOVE    "マスタ更新は正常に終了しました。"
                                                TO  SPA-GMN-MSG2(ID1:32)
                           MOVE    "blue"       TO  SPA-GMN-MSG2-STYLE
                       END-IF
                   WHEN    "01"
                   WHEN    "8001"
                       MOVE    "更新データはありませんでした。"
                                             TO  SPA-GMN-MSG2(ID1:30)
                       MOVE    "blue"        TO  SPA-GMN-MSG2-STYLE
                       IF      JOB1-YOBI(1:7)  =  "[ERROR]"
                           MOVE    JOB1-YOBI(9:) TO  SPA-GMN-MSG2PLUS
                           MOVE    "red"         TO  SPA-GMN-MSG2-STYLE
                       END-IF
                   WHEN    "98"
                   WHEN    "8002"
                       MOVE    "提供サーバへ接続できませんでした。"
                                             TO  SPA-GMN-MSG2(ID1:34)
                       MOVE    "blue"        TO  SPA-GMN-MSG2-STYLE
                       IF      JOB1-YOBI(1:7)  =  "[ERROR]"
                           MOVE    JOB1-YOBI(9:) TO  SPA-GMN-MSG2PLUS
                           MOVE    "red"         TO  SPA-GMN-MSG2-STYLE
                       END-IF
                   WHEN    "8888"
                       MOVE    "マスタ更新は中止されました"
                                           TO  SPA-GMN-MSG2(ID1:34)
                       MOVE    "blue"       TO  SPA-GMN-MSG2-STYLE
                       MOVE    JOB1-YOBI      TO  SPA-GMN-MSG2PLUS
                   WHEN    OTHER
                       MOVE    "マスタ更新は異常終了しました。"
                                             TO  SPA-GMN-MSG2(ID1:30)
                       MOVE    "red"         TO  SPA-GMN-MSG2-STYLE
                       MOVE    JOB1-YOBI     TO  SPA-GMN-MSG2PLUS
                   END-EVALUATE
                   IF    ( JOB1-UPDCNT      >   ZERO )
                        MOVE    SPA-GMN-MSG2 TO  WRK-MSG
                        MOVE    SPACE        TO  SPA-GMN-MSG2
                        STRING  WRK-MSG         DELIMITED   BY  SPACE
                               "【確認リストが作成されました】"
                                                DELIMITED   BY  SIZE
                        INTO    SPA-GMN-MSG2
                        END-STRING
                    END-IF
               ELSE
                   MOVE    "マスタ更新中です。"
                                             TO  SPA-GMN-MSG2(ID1:18)
                   MOVE    "blue"            TO  SPA-GMN-MSG2-STYLE
                   MOVE    JOB1-SHELLMSG     TO  SPA-GMN-MSG2PLUS
                   EVALUATE    JOB1-STOPFLG
                   WHEN    ZERO
                       MOVE    1           TO  SPA-NAI-CANCEL
                   WHEN    1
                       MOVE    "処理中止手続き中です"
                                           TO  SPA-GMN-MSG2PLUS
                   END-EVALUATE
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-GMN-MSG2
           END-IF
      *
           PERFORM 3103-JOBKANRI-HENSYU2-SEC
      *
      *----(03.04.01)--ADD-START---
           MOVE    0           TO  SPA-GMN-DURATION
      *
           IF    ( FLG-SJOBKANRI       =   ZERO )
               IF    ( JOB-ENDTIME     =   SPACE)  OR
                     ( JOB-ENDYMD      =   SPACE)
                   MOVE    5           TO  SPA-GMN-DURATION
               END-IF
           END-IF
      *
           IF    (  FLG-SJOBKANRI1     =   ZERO )
               IF    ( JOB1-ENDTIME    =   SPACE)  OR
                     ( JOB1-ENDYMD     =   SPACE)
                   MOVE    5           TO  SPA-GMN-DURATION
               END-IF
           END-IF
      *----(03.04.01)--ADD-END-----
      *
           .
       310-DBHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ管理情報編集処理
      *****************************************************************
       3101-LIST1-HENSYU-SEC         SECTION.
      *
           MOVE    1                   TO  IDX
           MOVE    DBKANRI-VERSION     TO  SPA-GMN-VERSION     (IDX)
           MOVE    DBKANRI-DBSVERSION1 TO  SPA-GMN-DBSVERSION1 (IDX)
           MOVE    DBKANRI-DBSVERSION2 TO  SPA-GMN-DBSVERSION2 (IDX)
           IF      DBKANRI-UPYMD       =   SPACE  OR  "00000000"
               MOVE    DBKANRI-CREYMD      TO  WRK-SYMD
           ELSE
               MOVE    DBKANRI-UPYMD       TO  WRK-SYMD
           END-IF
           PERFORM 700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-CREYMD   (IDX)
           .
       3101-LIST1-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    マスタ管理情報編集処理
      *****************************************************************
       3102-LIST3-HENSYU-SEC         SECTION.
      *
           MOVE    MSTKANRI-KANRICD(7:2)   TO  IDX2
           ADD     1                       TO  IDX2
      *
           IF      MSTKANRI-HOSPNUM     =   1
               ADD     1                    TO  SPA-GMN-MAX3
           END-IF
           MOVE    MSTKANRI-DBRVERSION1 TO  SPA-GMN-DBRVERSION1 (IDX2)
           MOVE    MSTKANRI-DBRVERSION2 TO  SPA-GMN-DBRVERSION2 (IDX2)
      *
           IF    ( MSTKANRI-NAME    NOT =   SPACE )
               MOVE    MSTKANRI-NAME    TO  SPA-GMN-MASTNAME(IDX2)
           ELSE
                   MOVE    MSTKANRI-KANRICD
                                         TO  SPA-GMN-MASTNAME(IDX2)
           END-IF
      *
           .
       3102-LIST3-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理情報編集処理
      *****************************************************************
       3103-JOBKANRI-HENSYU2-SEC         SECTION.
      *
               MOVE    "標準提供マスタ"
                                       TO  SPA-GMN-JOBNAME (1)
      *
               MOVE    "ライセンスマスタ"
                                       TO  SPA-GMN-JOBNAME (2)
      *
           IF    ( FLG-SJOBKANRI        =   ZERO )
      *
               MOVE    JOB-STARTYMD    TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-STARTYMD (1)
      *
               MOVE    JOB-ENDYMD      TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-ENDYMD (1)
      *
               MOVE    JOB-ENDYMD      TO  SPA-NAI-ENDYMD  (1)
               MOVE    JOB-ENDTIME     TO  SPA-NAI-ENDTIME (1)
      *
               IF    ( JOB-STARTTIME   =   SPACE )
                   MOVE    SPACE       TO  SPA-GMN-STARTTIME (1)
               ELSE
                   MOVE    ":"         TO  SPA-GMN-STARTTIME (1)(3:1)
                   MOVE    ":"         TO  SPA-GMN-STARTTIME (1)(6:1)
                   MOVE    JOB-STARTTIME(1:2)
                                       TO  SPA-GMN-STARTTIME (1)(1:2)
                   MOVE    JOB-STARTTIME(3:2)
                                       TO  SPA-GMN-STARTTIME (1)(4:2)
                   MOVE    JOB-STARTTIME(5:2)
                                       TO  SPA-GMN-STARTTIME (1)(7:2)
               END-IF
      *
               IF    ( JOB-ENDTIME    =   SPACE )
                   MOVE    SPACE       TO  SPA-GMN-ENDTIME (1)
               ELSE
                   MOVE    ":"         TO  SPA-GMN-ENDTIME (1)(3:1)
                   MOVE    ":"         TO  SPA-GMN-ENDTIME (1)(6:1)
                   MOVE    JOB-ENDTIME(1:2)
                                       TO  SPA-GMN-ENDTIME (1)(1:2)
                   MOVE    JOB-ENDTIME(3:2)
                                       TO  SPA-GMN-ENDTIME (1)(4:2)
                   MOVE    JOB-ENDTIME(5:2)
                                       TO  SPA-GMN-ENDTIME (1)(7:2)
               END-IF
           END-IF
      *
           IF    ( FLG-SJOBKANRI1       =   ZERO )
      *
               MOVE    JOB1-STARTYMD   TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-STARTYMD (2)
      *
               MOVE    JOB1-ENDYMD     TO  WRK-SYMD
               PERFORM 700-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-ENDYMD (2)
      *
               MOVE    JOB1-ENDYMD     TO  SPA-NAI-ENDYMD  (2)
               MOVE    JOB1-ENDTIME    TO  SPA-NAI-ENDTIME (2)
      *
               IF    ( JOB1-STARTTIME   =   SPACE )
                   MOVE    SPACE       TO  SPA-GMN-STARTTIME (2)
               ELSE
                   MOVE    ":"         TO  SPA-GMN-STARTTIME (2)(3:1)
                   MOVE    ":"         TO  SPA-GMN-STARTTIME (2)(6:1)
                   MOVE    JOB1-STARTTIME(1:2)
                                       TO  SPA-GMN-STARTTIME (2)(1:2)
                   MOVE    JOB1-STARTTIME(3:2)
                                       TO  SPA-GMN-STARTTIME (2)(4:2)
                   MOVE    JOB1-STARTTIME(5:2)
                                       TO  SPA-GMN-STARTTIME (2)(7:2)
               END-IF
      *
               IF    ( JOB1-ENDTIME    =   SPACE )
                   MOVE    SPACE       TO  SPA-GMN-ENDTIME (2)
               ELSE
                   MOVE    ":"         TO  SPA-GMN-ENDTIME (2)(3:1)
                   MOVE    ":"         TO  SPA-GMN-ENDTIME (2)(6:1)
                   MOVE    JOB1-ENDTIME(1:2)
                                       TO  SPA-GMN-ENDTIME (2)(1:2)
                   MOVE    JOB1-ENDTIME(3:2)
                                       TO  SPA-GMN-ENDTIME (2)(4:2)
                   MOVE    JOB1-ENDTIME(5:2)
                                       TO  SPA-GMN-ENDTIME (2)(7:2)
               END-IF
           END-IF
      *
           .
       3103-JOBKANRI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        終了
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        情報削除
               WHEN    "CLICKED"       ALSO    "B02"
                   IF      M96-B02-VISIBLE     =   "T"
      **               PERFORM 211-JYOUKYOU-SEC
                       MOVE   ZERO                 TO  SPA-NAI-CANCEL
                       PERFORM 201-DELETE-SEC
                   END-IF
      *        キー取得
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 221-GETKEY-SEC
      *        キー削除
               WHEN    "CLICKED"       ALSO    "B05C"
                   PERFORM 221-DELKEY-SEC
      *        再印刷画面
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 209-REPRINT-SEC
      *
      *        処理中止
               WHEN    "CLICKED"       ALSO    "B10"
                   IF      M96-B10-VISIBLE     =   "T"
                       PERFORM 210-CANCEL-SEC
                   END-IF
      *        情報
               WHEN    "CLICKED"   ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11"
                   IF      M96-B11-VISIBLE     =   "T"
                       PERFORM 211-JYOUKYOU-SEC
                   END-IF
      *        更新
               WHEN    "CLICKED"       ALSO    "B12"
                   IF      M96-B12-VISIBLE     =   "T"
      **               PERFORM 211-JYOUKYOU-SEC
                       MOVE   ZERO                 TO  SPA-NAI-CANCEL
                       PERFORM 212-UPDATE-SEC
                   END-IF
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除　処理
      *****************************************************************
       201-DELETE-SEC                  SECTION.
      *
           PERFORM 2011-RUNNING-JOB-CHK-SEC
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( FLG-RUNNING     =   ZERO )
                   MOVE    "0005"      TO  SPA-ERRCD
               ELSE
                   MOVE    "0100"      TO  SPA-MMID1CD
               END-IF
           END-IF
      *
           .
       201-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    実行中ジョブチェック処理
      *****************************************************************
       2011-RUNNING-JOB-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-RUNNING
      *
           PERFORM 900-MONBATCH-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-RUNNING     NOT =   ZERO )
      *
               IF    ( SHELLTBL-SHELLID    =   CONST-MNTSHELLID
                                            OR CONST-MLISHELLID )
      *
                   MOVE    1           TO  FLG-RUNNING
      *
               END-IF
      *
               PERFORM 900-MONBATCH-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           PERFORM 900-JOBKANRI-KEY4-SEL-SEC
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID  TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO  )
      *
      *        IF    ( FLG-RUNNING     =   1 )
      *         AND  ( JOB-PGID        =   "ORCXRMST2" )
      *         AND  ( JOB-STEPENDTIME =   SPACE OR ZERO )
      *            MOVE    "1104"  TO  SPA-ERRCD
      *        END-IF
      *
               IF    ( FLG-RUNNING     =   ZERO )
                   MOVE    2       TO  FLG-RUNNING
               END-IF
      *
           END-IF
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MLIJOBID  TO  JOB-JOBID
           MOVE    CONST-MLISHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO  )
      *
      *        IF    ( FLG-RUNNING     =   1 )
      *         AND  ( JOB-PGID        =   "ORCXRMST4" )
      *         AND  ( JOB-STEPENDTIME =   SPACE OR ZERO )
      *            MOVE    "1104"  TO  SPA-ERRCD
      *        END-IF
      *
               IF    ( FLG-RUNNING     =   ZERO )
                   MOVE    2       TO  FLG-RUNNING
               END-IF
      *
           END-IF
      *
           .
       2011-RUNNING-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    実行中ジョブチェック処理
      *****************************************************************
       2012-RUNNING-JOB-CHK-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-RUNNING
      *
           MOVE    "CH2"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID  TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO  )
               IF    ( JOB-ENDYMD      =  SPACE )
                   MOVE    1       TO  FLG-RUNNING
               END-IF
           END-IF
      *
           MOVE    "CH2"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MLIJOBID  TO  JOB-JOBID
           MOVE    CONST-MLISHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO  )
               IF    ( JOB-ENDYMD      =  SPACE )
                   MOVE    1       TO  FLG-RUNNING
               END-IF
           END-IF
      *
           .
       2012-RUNNING-JOB-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再印刷画面へ
      *****************************************************************
       209-REPRINT-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "M96"               TO  SPA-MOTOPG
           MOVE    "XA01"              TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    CONST-MNTSHELLID    TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XA01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       209-REPRINT-EXT.
           EXIT.
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       210-CANCEL-SEC                  SECTION.
      *
           MOVE    "0102"              TO  SPA-MMID1CD
      *
           .
       210-CANCEL-EXT.
           EXIT.
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       2101-CANCEL-SEC                  SECTION.
      *
           MOVE    "STP"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    1                   TO  JOB-STOPFLG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI1-REC
           MOVE    CONST-MLIJOBID      TO  JOB1-JOBID
           MOVE    CONST-MLISHELLID    TO  JOB1-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB1-HOSPNUM
           MOVE    1                   TO  JOB1-STOPFLG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI1-REC
                                           SPA-AREA
           PERFORM 211-JYOUKYOU-SEC
      *
           .
       2101-CANCEL-EXT.
           EXIT.
      *****************************************************************
      *    状況処理
      *****************************************************************
       211-JYOUKYOU-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-INIT
      *    最新情報編集
           PERFORM 310-DBHENSYU-SEC
           MOVE    1                   TO  SPA-M96-CUR
           .
       211-JYOUKYOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ更新　処理
      *****************************************************************
       212-UPDATE-SEC             SECTION.
      *
           MOVE    ZERO                 TO  FLG-ERR
      *
           PERFORM 2012-RUNNING-JOB-CHK-SEC
           IF    ( FLG-RUNNING NOT =   ZERO )
               MOVE    1                TO  FLG-ERR
           END-IF
      *
           IF    ( FLG-ERR                 =   1 )
               MOVE    "0203"              TO  SPA-ERRCD
           ELSE
      *        プログラム更新ＪＯＢ管理存在チェック
               PERFORM 820-JOBKANRI-READ2-SEC
      *
               MOVE    ZERO                 TO  FLG-ERR
               IF    ( FLG-SJOBKANRI         =   ZERO )
                   IF        ( JOB-ENDTIME   NOT =   SPACE )
                       AND   ( JOB-ENDYMD    NOT =   SPACE )
                       AND   ( JOB-ERRCD (1:2)   NOT =   "51")
                       CONTINUE
                   ELSE
                       MOVE    1             TO  FLG-ERR
                   END-IF
               END-IF
      *
               IF    ( FLG-ERR                 =   1 )
                   MOVE    "0501"              TO  SPA-ERRCD
               ELSE
                   ACCEPT  WRK-ENV-OFFLINE FROM ENVIRONMENT
                                               "ENV_OFFLINE"
                   IF    ( SPA-MW              =   SPA-PANDA ) AND
                         ( SPA-NAI-CONTRACT-COND
                                           NOT =   1   AND 2 ) AND
                         ( WRK-ENV-OFFLINE NOT =   "yes" )
                       MOVE    "GUD1"              TO  SPA-MMID1CD
                       PERFORM 830-GAMENN-SET1-SEC
                   ELSE
                       MOVE    "0202"              TO  SPA-MMID1CD
                   END-IF
               END-IF
           END-IF
           .
      *
       212-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー取得処理
      *****************************************************************
       221-GETKEY-SEC              SECTION.
      *
           IF      M96-B05-LABEL   NOT =   SPACE
               MOVE    "0401"              TO  SPA-MMID1CD
               MOVE    1                   TO  SPA-M96-CUR
           END-IF
           .
       221-GETKEY-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー削除処理
      *****************************************************************
       221-DELKEY-SEC              SECTION.
      *
           IF     (SPA-HOSPID(4:2) NOT =   "50")       AND
                  (SPA-MW              =   SPA-PANDA)  AND
                  (M96-B05-LABEL       =   SPACE)
               MOVE    "0404"              TO  SPA-MMID1CD
               MOVE    1                   TO  SPA-M96-CUR
           END-IF
           .
       221-GETKEY-EXT.
           EXIT.
      *****************************************************************
      *    マスタ更新処理
      *****************************************************************
       30012-KOUSIN-SYORI-SEC                  SECTION.
      *
           PERFORM 820-JOBKANRI-READ-SEC
      *
           IF      FLG-SJOBKANRI       =   ZERO
      *        ジョブ管理削除
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CONST-MNTJOBID      TO  JOB-JOBID
               MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           END-IF
      *
      *    ジョブ管理作成
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *    ライセンスマスタ
           IF      FLG-SJOBKANRI1      =   ZERO
      *        ジョブ管理削除
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI1-REC
               MOVE    CONST-MLIJOBID      TO  JOB1-JOBID
               MOVE    CONST-MLISHELLID    TO  JOB1-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB1-HOSPNUM
               CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI1-REC
                                           SPA-AREA
           END-IF
      *
      *    ジョブ管理作成
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI1-REC
           MOVE    CONST-MLIJOBID      TO  JOB1-JOBID
           MOVE    CONST-MLISHELLID    TO  JOB1-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB1-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI1-REC
                                       SPA-AREA
      *
           MOVE    SPA-HOSPNUM         TO  ERRFLPARA-HOSPNUM
           MOVE    "ORMNTERR"          TO  ERRFLPARA-FILE-ID
           MOVE    1                   TO  ERRFLPARA-SYOKBN1
           MOVE    SPA-TERMID          TO  ERRFLPARA-TERMID
           MOVE    ERRFLPARA           TO  MNTERRFL
      *
           MOVE    "ORMLIERR"          TO  ERRFLPARA-FILE-ID
           MOVE    ERRFLPARA           TO  MLIERRFL
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    MNTERRFL            TO  SGETTEMP-BASENAMES (1)
           MOVE    MLIERRFL            TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING     SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  MNTERRFL
           MOVE    SGETTEMP-ST         TO  MNTERRFL-ST
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  MLIERRFL
           MOVE    SGETTEMP-ST         TO  MLIERRFL-ST
      *
      *    最新情報編集
           MOVE    ZERO                TO  FLG-INIT
           PERFORM 310-DBHENSYU-SEC
           MOVE    1                   TO  SPA-M96-CUR
      *    マスタ更新処理
           MOVE    SPACE                TO  SHELLTBL
           MOVE    SHELL-NAME           TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM          TO  SHELLTBL-HOSPNUM
           MOVE    CONST-MNTJOBID       TO  SHELLTBL-JOBID
           MOVE    CONST-MNTSHELLID     TO  SHELLTBL-SHELLID
           MOVE    SPA-HOSPNUM          TO  SHELLTBL-ARG1
           MOVE    SPA-SUPERVISOR       TO  SHELLTBL-ARG2
           MOVE    SPA-HOSPID           TO  SHELLTBL-ARG3
           MOVE    MNTERRFL (MNTERRFL-ST:)
                                        TO  SHELLTBL-ARG4
           MOVE    CONST-MNTJOBID       TO  SHELLTBL-ARG5
           MOVE    CONST-MNTSHELLID     TO  SHELLTBL-ARG6
           MOVE    MLIERRFL (MLIERRFL-ST:)
                                        TO  SHELLTBL-ARG7
           MOVE    CONST-MLIJOBID       TO  SHELLTBL-ARG8
           MOVE    CONST-MLISHELLID     TO  SHELLTBL-ARG9
           MOVE    SPA-ACCESS-KEY-1     TO  SHELLTBL-ARG10
      *
           MOVE    SHELLTBL             TO  MCPDATA-REC
      *
           MOVE    "SHELL"              TO  MCP-FUNC
           MOVE    "shell"              TO  MCP-TABLE
           MOVE    "allways"            TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       30012-KOUSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       30011-DELETE-SYORI-SEC                  SECTION.
      *
      *    ジョブ管理削除
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID  TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID TO JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    ジョブ管理削除
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE              JOBKANRI1-REC
           MOVE    CONST-MLIJOBID  TO  JOB1-JOBID
           MOVE    CONST-MLISHELLID TO  JOB1-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB1-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI1-REC
                                   SPA-AREA
      *
           PERFORM 900-MONBATCH-SEL-SEC
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( SPA-ERRCD       NOT =   SPACE )
      *
               IF    ( SHELLTBL-SHELLID    =   CONST-MNTSHELLID
                                            OR CONST-MLISHELLID
                                                                )
                   MOVE    "shell"         TO  MCP-TABLE
                   MOVE    LOW-VALUE       TO  MCP-PATHNAME
                   PERFORM 910-DBDELETE-SEC
      *            IF    ( MCP-RC          NOT =   ZERO )
      *                MOVE    "1103"      TO  SPA-ERRCD
      *            END-IF
               END-IF
      *
               PERFORM 900-MONBATCH-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( SPA-ERRCD           =   SPACE )
               INITIALIZE              SPA-GMN-TBLG2
               INITIALIZE              SPA-GMN-MSG-G
           END-IF
      *
           .
       30011-DELETE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー取得処理
      *****************************************************************
       30021-GET-ACCESSKEY-SEC     SECTION.
      *
           PERFORM 820-JOBKANRI-READ3-SEC
      *
           IF      FLG-SJOBKANRI       =   ZERO
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               INITIALIZE  JOBKANRI-REC
               MOVE    CONST-MKYJOBID      TO  JOB-JOBID
               MOVE    CONST-MKYSHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    CONST-MKYJOBID      TO  JOB-JOBID
           MOVE    CONST-MKYSHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    SHELL-NAME1         TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG1
           MOVE    SPA-HOSPID          TO  SHELLTBL-ARG2
           MOVE    CONST-MKYJOBID      TO  SHELLTBL-ARG3
           MOVE    CONST-MKYSHELLID    TO  SHELLTBL-ARG4
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0410"          TO  SPA-MMID1CD
               PERFORM 520-MIDSET-SEC
           END-IF
           .
       30021-GET-ACCESSKEY-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー取得処理確認
      *****************************************************************
       30022-GET-ACCESSKEY-RES-SEC SECTION.
      *
           PERFORM 820-JOBKANRI-READ3-SEC
      *
           IF      FLG-SJOBKANRI       =   ZERO
               IF    ( JOB-ENDTIME     NOT =   SPACE )
               AND   ( JOB-ENDYMD      NOT =   SPACE )
                   IF      JOB-ERRCD           =   "0000"
                       MOVE    "0402"          TO  SPA-MMID1CD
                   ELSE
                       MOVE    "0403"          TO  SPA-MMID1CD
                   END-IF
               ELSE
                   MOVE    "0410"          TO  SPA-MMID1CD
               END-IF
               PERFORM 520-MIDSET-SEC
           END-IF
           .
       30022-GET-ACCESSKEY-RES-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           PERFORM 2012-RUNNING-JOB-CHK-SEC
           IF    ( FLG-RUNNING NOT =   ZERO )
               MOVE    "0301"  TO  SPA-MMID1CD
           END-IF
      *
           IF      SPA-MMID1CD         =   SPACE
               PERFORM 2001-BACK-SYORI-SEC
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       2001-BACK-SYORI-SEC             SECTION.
      *
           IF      SPA-NAI-MOTOPG(1:3) =   "M01"
               MOVE    SPA-NAI-MOTOPG      TO  SPA-SAKIPG
           ELSE
               MOVE    "M01"               TO  SPA-SAKIPG
           END-IF
           MOVE    "M96"               TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
           MOVE    "JOIN"              TO  MCP-PUTTYPE
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       2001-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"
                                       OR   SPACE
               MOVE    SPACE               TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF.
      *
       700-SEIWA-HEN-EXT.
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
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-MMID1CD     NOT =   SPACE )
               PERFORM 520-MIDSET-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    "CURRENT"           TO  MCP-PUTTYPE
               MOVE    "M96"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
           .
      *
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  M96
           INITIALIZE              M96
      *
           PERFORM VARYING   IDX   FROM   1   BY   1
                   UNTIL     IDX      >   1
               MOVE    SPA-GMN-VERSION       (IDX) TO  M96-VERSION (IDX)
               MOVE    SPA-GMN-DBSVERSION1   (IDX) TO  M96-DBSVERSION1
                                                                  (IDX)
               MOVE    SPA-GMN-DBSVERSION2   (IDX) TO  M96-DBSVERSION2
                                                                  (IDX)
               MOVE    SPA-GMN-CREYMD        (IDX) TO  M96-CREYMD (IDX)
           END-PERFORM
           MOVE    1                   TO  M96-LIST1-COUNT
           PERFORM VARYING   IDX   FROM   1   BY   1
                   UNTIL     IDX      >   2
               MOVE    SPA-GMN-JOBNAME       (IDX) TO  M96-JOBNAME
                                                                  (IDX)
               MOVE    SPA-GMN-STARTYMD      (IDX) TO  M96-STARTYMD
                                                                  (IDX)
               MOVE    SPA-GMN-STARTTIME     (IDX) TO  M96-STARTTIME
                                                                  (IDX)
               MOVE    SPA-GMN-ENDYMD        (IDX) TO  M96-ENDYMD (IDX)
               MOVE    SPA-GMN-ENDTIME       (IDX) TO  M96-ENDTIME(IDX)
               MOVE    SPA-GMN-LASTYMD       (IDX) TO  M96-LASTYMD(IDX)
           END-PERFORM
           MOVE    2                   TO  M96-LIST2-COUNT
      *
           PERFORM VARYING   IDX   FROM   1   BY   1
                   UNTIL     IDX      >   SPA-GMN-MAX3
               MOVE    SPA-GMN-MASTNAME    (IDX) TO  M96-MASTNAME (IDX)
               MOVE    SPA-GMN-DBRVERSION1 (IDX) TO  M96-DBRVERSION1
                                                                  (IDX)
               MOVE    SPA-GMN-DBRVERSION2 (IDX) TO  M96-DBRVERSION2
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-GMN-MAX3        TO  M96-LIST3-COUNT
      *
           MOVE    SPA-GMN-MSG1            TO  M96-MSG1-VALUE
           MOVE    SPA-GMN-MSG1PLUS        TO  M96-MSG1PLUS-VALUE
           MOVE    SPA-GMN-MSG1-STYLE      TO  M96-MSG1-STYLE
                                               M96-MSG1PLUS-STYLE
      *
           MOVE    SPA-GMN-MSG2            TO  M96-MSG2-VALUE
           MOVE    SPA-GMN-MSG2PLUS        TO  M96-MSG2PLUS-VALUE
           MOVE    SPA-GMN-MSG2-STYLE      TO  M96-MSG2-STYLE
                                               M96-MSG2PLUS-STYLE
      *
      *----(03.04.01)--ADD-START---
           MOVE    SPA-GMN-DURATION        TO  M96-DURATION
      *----(03.04.01)--ADD-END-----
      *
           MOVE    WIDGET-INSENSITIVE      TO  M96-B10-STATE
      *
           MOVE    "T"                     TO  M96-B02-VISIBLE
                                               M96-B10-VISIBLE
                                               M96-B11-VISIBLE
                                               M96-B12-VISIBLE
      *
           IF    ( SPA-NAI-CANCEL          =   1 )
               MOVE    WIDGET-NORMAL       TO  M96-B10-STATE
           END-IF
      *
           IF    ( SPA-MW              NOT =   SPA-PANDA )
               MOVE    "F"                 TO  M96-B02-VISIBLE
                                               M96-B10-VISIBLE
                                               M96-B11-VISIBLE
                                               M96-B12-VISIBLE
           END-IF
      *
           IF    ( SPA-HOSPID(4:2)     =   "50" )  OR
                 ( SPA-MW              =   SPA-GINBEE )
               MOVE    SPACE               TO  M96-B05-LABEL
           ELSE
               PERFORM 900-ACCESSKEY-READ-SEC
               IF      ACCESS-KEY-1        =   SPACE
                   MOVE    "キー取得"          TO  M96-B05-LABEL
               ELSE
                   MOVE    ACCESS-KEY-1        TO  SPA-ACCESS-KEY-1
                   MOVE    SPACE               TO  M96-B05-LABEL
               END-IF
           END-IF
      *
           IF      SPA-MW              =   SPA-PANDA
               IF      SPA-NAI-CONTRACT-COND       =   1   OR  2
                   IF      SPA-NAI-KADO-LEVEL      =   5
                       MOVE    "教育版"            TO  M96-CONTRACTMSG
                   ELSE
                       MOVE    "商用版"            TO  M96-CONTRACTMSG
                   END-IF
                   MOVE    "red"               TO
                                               M96-CONTRACTMSG-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
           .
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-M96-CUR
               WHEN    1
                   IF    ( SPA-MW              NOT =   SPA-PANDA )
                       MOVE    "B01"               TO  MCP-WIDGET
                   ELSE
                       MOVE    "B12"               TO  MCP-WIDGET
                   END-IF
           END-EVALUATE
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
            EVALUATE    SPA-ERRCD
                WHEN    "0001"
                    MOVE    "入力エラー"
                                                TO  SPA-ERRMSG
                WHEN    "0203"
                    MOVE    "マスタ更新は実行中です。"
                                                TO  SPA-ERRMSG
                WHEN    "0003"
                    MOVE    "対象が１００件以上あります"
                                                TO  SPA-ERRMSG
                WHEN    "0004"
                    MOVE    "更新処理が実行中の為、削除できません"
                                                TO  SPA-ERRMSG
                WHEN    "0005"
                    MOVE    "ＪＯＢ管理情報は、ありません"
                                                TO  SPA-ERRMSG
               WHEN     "1103"
               WHEN     "1104"
                   MOVE    "削除不可な処理実行中です。削除できません"
                                                TO  SPA-ERRMSG
                WHEN    "0501"
                    MOVE    "プログラム更新が実行中です。"
                                                TO  SPA-ERRMSG
                WHEN    OTHER
                    MOVE    SPACE               TO  SPA-ERRMSG
            END-EVALUATE.
      *
      *----(03.04.01)--ADD-START---
           MOVE    ZERO            TO  M96-DURATION
      *----(03.04.01)--ADD-END-----
      *
           MOVE    SPACE                TO  MMERR
           MOVE    SPA-ERRCD            TO  MMERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MMERR-ERRMSG
      *
           MOVE    "B01"               TO  MCP-WIDGET
           MOVE    "M96"               TO  SPA-MOTOPG
           MOVE    "MMERR"             TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MMERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-MIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  MMID1
           INITIALIZE                      MMID1
           EVALUATE    SPA-MMID1CD
               WHEN    "0100"
                   MOVE
                   "ＪＯＢ管理情報を削除します。よろしいですか？"
                                       TO  MMID1-ID1MSG
               WHEN    "0102"
                   MOVE    "マスタ更新を中止します。よろしいですか？"
                                       TO  MMID1-ID1MSG
               WHEN    "0202"
                   MOVE    "マスタ更新を実行します。よろしいですか？"
                                       TO  MMID1-ID1MSG
               WHEN    "0301"
                   MOVE    "マスタ更新中です。よろしいですか？"
                                       TO  MMID1-ID1MSG
               WHEN    "0401"
                   MOVE
                       "アクセスキーを取得します。よろしいですか？"
                                       TO  MMID1-ID1MSG
               WHEN    "0402"
                   MOVE
                       "アクセスキーを取得しました"
                                       TO  MMID1-ID1MSG
               WHEN    "0403"
                   MOVE    JOB-YOBI    TO  MMID1-ID1MSG
      *            MOVE
      *                "アクセスキーの取得に失敗しました"
      *                                TO  MMID1-ID1MSG
               WHEN    "0404"
                   MOVE
                       "アクセスキーを削除します。よろしいですか？"
                                       TO  MMID1-ID1MSG
               WHEN    "0410"
                   MOVE
                       "２〜３秒待ってからボタンをクリックして下さい"
                                       TO  MMID1-ID1MSG
               WHEN    OTHER
                   MOVE    SPA-MMID1CD
                                       TO  MMID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-MMID1CD           TO  MMID1-ID1CODE
      *
           MOVE    "B12"               TO  MCP-WIDGET
           EVALUATE    SPA-MMID1CD
               WHEN    "0100"
                   MOVE    "B01"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "M96"               TO  SPA-MOTOPG
           MOVE    "MMID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MMID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-MIDSET-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ-SEC     SECTION.
      *
      *    ＪＯＢ管理 
           MOVE    ZERO                TO  FLG-SJOBKANRI
                                           FLG-SJOBKANRI1
           INITIALIZE                  ORCSJOBKANRIAREA
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    NOT =   ZERO
               MOVE    1               TO  FLG-SJOBKANRI
               INITIALIZE                  JOBKANRI-REC
           END-IF
      *    日付の再編集
           IF     (JOB-ENDYMD      NOT =   SPACE)  AND
                  (JOB-ENDYMD(7:2)     =   SPACE)
               MOVE    "20"               TO  WRK-SYMD(1:2)
               MOVE    JOB-ENDYMD(1:6)    TO  WRK-SYMD(3:6)
               MOVE    WRK-SYMD           TO  JOB-ENDYMD
           END-IF
           IF     (JOB-STARTYMD    NOT =   SPACE)  AND
                  (JOB-STARTYMD(7:2)   =   SPACE)
               MOVE    "20"               TO  WRK-SYMD(1:2)
               MOVE    JOB-STARTYMD(1:6)  TO  WRK-SYMD(3:6)
               MOVE    WRK-SYMD           TO  JOB-STARTYMD
           END-IF
      *    ライセンスマスタ
           INITIALIZE                  ORCSJOBKANRIAREA
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI1-REC
           MOVE    CONST-MLIJOBID      TO  JOB1-JOBID
           MOVE    CONST-MLISHELLID    TO  JOB1-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB1-HOSPNUM
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI1-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    NOT =   ZERO
               MOVE    1               TO  FLG-SJOBKANRI1
               INITIALIZE                  JOBKANRI1-REC
           END-IF
      *    日付の再編集
           IF     (JOB1-ENDYMD     NOT =   SPACE)  AND
                  (JOB1-ENDYMD(7:2)    =   SPACE)
               MOVE    "20"               TO  WRK-SYMD(1:2)
               MOVE    JOB1-ENDYMD(1:6)   TO  WRK-SYMD(3:6)
               MOVE    WRK-SYMD           TO  JOB1-ENDYMD
           END-IF
           IF     (JOB1-STARTYMD   NOT =   SPACE)  AND
                  (JOB1-STARTYMD(7:2)  =   SPACE)
               MOVE    "20"               TO  WRK-SYMD(1:2)
               MOVE    JOB1-STARTYMD(1:6) TO  WRK-SYMD(3:6)
               MOVE    WRK-SYMD           TO  JOB1-STARTYMD
           END-IF
      *
           .
       820-JOBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ2-SEC      SECTION.
      *
      *    プログラム更新チェック
           MOVE    ZERO            TO  FLG-SJOBKANRI
           INITIALIZE                  ORCSJOBKANRIAREA
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-JOBID
           MOVE    "PRGMNT"        TO  JOB-SHELLID
      *
      *    プログラム更新処理はHOSPNUM=1のみ実行
      **** MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    1                   TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    NOT =   ZERO
               MOVE    1               TO  FLG-SJOBKANRI
               INITIALIZE                  JOBKANRI-REC
           END-IF
      *
           .
       820-JOBKANRI-READ2-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理読込
      *****************************************************************
       820-JOBKANRI-READ3-SEC      SECTION.
      *
      *    アクセスキー取得チェック
           MOVE    ZERO                TO  FLG-SJOBKANRI
           INITIALIZE  ORCSJOBKANRIAREA
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           INITIALIZE  JOBKANRI-REC
           MOVE    CONST-MKYJOBID      TO  JOB-JOBID
           MOVE    CONST-MKYSHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    NOT =   ZERO
               MOVE    1                   TO  FLG-SJOBKANRI
               INITIALIZE  JOBKANRI-REC
           END-IF
           .
       820-JOBKANRI-READ2-EXT.
           EXIT.
      *****************************************************************
      *    案内１画面
      *****************************************************************
       830-GAMENN-SET1-SEC         SECTION.
      *
           MOVE    "B12"               TO  MCP-WIDGET
           MOVE    "M96"               TO  SPA-MOTOPG
           MOVE    "MGUIDE1"           TO  SPA-SAKIPG
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "MGUIDE1"           TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END 
           .
       830-GAMENN-SET1-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ管理読込み処理
      *****************************************************************
       900-DBKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBKANRI-REC
               MOVE    ZERO            TO  FLG-DBKANRI
           ELSE
               INITIALIZE                  DBKANRI-REC
               MOVE    1               TO  FLG-DBKANRI
           END-IF
      *
           .
       900-DBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ管理読込み処理
      *****************************************************************
       900-MSTKANRI-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
               MOVE    ZERO            TO  FLG-MSTKANRI
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
      *
           .
       900-DBKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONBATCH検索処理
      *****************************************************************
       900-MONBATCH-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
      *
           INITIALIZE                      SHELLTBL
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-MONBATCH-FET-SEC
           ELSE
               MOVE    1               TO  FLG-MONBATCH
           END-IF
      *
           .
       900-MONBATCH-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONBATCH FETCH処理
      *****************************************************************
       900-MONBATCH-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    LOW-VALUE       TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SHELLTBL
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( SHELLTBL-HOSPNUM    NOT =  SPA-HOSPNUM )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-MONBATCH
               END-IF
      *
           END-PERFORM
      *
           .
       900-MONBATCH-FET-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理検索処理
      *****************************************************************
       900-JOBKANRI-KEY4-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-JOBKANRI
      *
           INITIALIZE                      JOBKANRI-REC
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 900-JOBKANRI-KEY4-FET-SEC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
               INITIALIZE                  JOBKANRI-REC
           END-IF
      *
           .
       900-JOBKANRI-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ジョブ管理FETCH処理
      *****************************************************************
       900-JOBKANRI-KEY4-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JOBKANRI
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
      *
               MOVE    MCPDATA-REC     TO  JOBKANRI-REC
           ELSE
               MOVE    1               TO  FLG-JOBKANRI
               INITIALIZE                  JOBKANRI-REC
           END-IF
      *
           .
       900-JOBKANRI-KEY4-FET-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー読込処理
      *****************************************************************
       900-ACCESSKEY-READ-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-ACCESSKEY
           INITIALIZE  ACCESS-KEY-REC
           MOVE    SPA-HOSPNUM         TO  ACCESS-KEY-HOSPNUM
           MOVE    ACCESS-KEY-REC      TO  MCPDATA-REC
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_access_key"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  ACCESS-KEY-REC
               ELSE
                   MOVE    1               TO  FLG-ACCESSKEY
                   INITIALIZE  ACCESS-KEY-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-ACCESSKEY
               INITIALIZE  ACCESS-KEY-REC
           END-IF
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME 
           PERFORM 910-DBCLOSECURSOR-SEC
           .
       900-ACCESSKEY-READ-EXT.
           EXIT.
      *****************************************************************
      *    アクセスキー削除処理
      *****************************************************************
       900-ACCESSKEY-DELETE-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-ACCESSKEY
           INITIALIZE  ACCESS-KEY-REC
           MOVE    SPA-HOSPNUM         TO  ACCESS-KEY-HOSPNUM
           MOVE    ACCESS-KEY-REC      TO  MCPDATA-REC
           MOVE    "tbl_access_key"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF      MCP-RC              =   ZERO
               MOVE    SPACE               TO  SPA-ACCESS-KEY-1
           ELSE
               MOVE    1                   TO  FLG-ACCESSKEY
               INITIALIZE  ACCESS-KEY-REC
           END-IF
           .
       900-ACCESSKEY-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    ZERO            TO  MCP-RC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       911-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
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
      *
       910-DBDELETE-EXT.
           EXIT.
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
