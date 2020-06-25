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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCSJOB.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ジョブ管理
      *  コンポーネント名  : ジョブ管理ＤＢ制御
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/01/20    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02/05/15  DELALLの追加  
      *  01.00.02    NACL-藤原    02/08/30  ステップ件数の追加
      *  01.00.03    NACL-藤原    03/02/24  頁数のチェック
      *  01.00.04    NACL-太田    03/08/11  途中経過更新処理を追加
      *  01.00.05    NACL-藤原    04/09/28  STP,CANを追加
      *  01.00.06    NACL-藤原    04/12/20  CN2を追加
      *                                     JBEのときYOBIが空白以外のとき
      *                                     だけ編集を行う
      *  01.00.07    NACL-太田    05/10/21  MONFUNC対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.05.01    NACL-竹田    10/02/27  監視ログ出力対応
      *
      *  04.07.01    NACL-藤原    12.09.07  更新件数（個別）の追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    フラグ領域
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-JOBKANRI                            PIC S9(03).
           03  FLG-MONBATCH                            PIC 9(01).
           03  FLG-LOOPEND                             PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
      *
      *
       01  WRK-AREA.
           03  WRK-SYSYMD-8.
               05  WRK-SYSYY           PIC 9(02).
               05  WRK-SYSYMD          PIC 9(06).  
           03  WRK-SYSTIME             PIC X(08).
      *シェルＩＤ
           03  WRK-SHELLID             PIC X(08).
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
       01  WRK-JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC"    REPLACING   //JOB-//
                                       BY          //WRK-JOB-//.
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCSLOCK.INC".
      *
      **************************************************************************
      *
       LINKAGE                 SECTION.
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               ORCSJOBKANRIAREA
               JOBKANRI-REC
grpsys         SPA-AREA
               .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE     SPACE              TO  WRK-SYSTIME
           ACCEPT   WRK-SYSTIME        FROM      TIME
      *
      *???
           DISPLAY "ORCSJOB ST =" SJOBKANRI-MODE "## "
                   SMCNDATE-YMD (1:4) "/"
                   SMCNDATE-YMD (5:2) "/"
                   SMCNDATE-YMD (7:2) " "
                   WRK-SYSTIME  (1:2) ":"
                   WRK-SYSTIME  (3:2) ":"
                   WRK-SYSTIME  (5:2) "."
                   WRK-SYSTIME  (7:2) " "
      *???     
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *    処理モードのチェック
           EVALUATE    SJOBKANRI-MODE
               WHEN    "JBS"
                       PERFORM 110-JBS-SEC
               WHEN    "JBE"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 120-JBE-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "STS"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 130-STS-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "STM"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 180-STM-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "STE"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 160-STE-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "DEL"
                       PERFORM 140-DEL-SEC
               WHEN    "ALL"
                       PERFORM 170-DELALL-SEC
               WHEN    "CHK"
               WHEN    "CH2"
                       PERFORM 150-CHK-SEC
               WHEN    "STP"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 190-STP-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "CAN"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 200-CANCEL-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "CA2"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 210-CANCEL2-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    OTHER
                       MOVE  10            TO  SJOBKANRI-RETURN  
                       MOVE  1             TO  FLG-END
           END-EVALUATE
      *???
           DISPLAY "ORCSJOB ED =" SJOBKANRI-RETURN "##"
      *???     
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    ジョブ開始セット　処理
      *****************************************************************
       110-JBS-SEC                 SECTION.
      *
           PERFORM 500-SYSYMD-HENSYU-SEC
           MOVE    WRK-SYSYMD-8        TO  JOB-STARTYMD
      *****ACCEPT  JOB-STARTTIME       FROM TIME
           MOVE    SMCNDATE-HMS        TO  JOB-STARTTIME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "INSERT ERR KEY = [" MCP-RC JOB-KEY "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
      *    監視ログ出力 
           CALL    "ORCSAUDIT"         USING
                                      "2"
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       ORCSLOCKAREA
                                       JOBKANRI-REC
      *
           .
       110-JBS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ジョブ終了セット　処理
      *****************************************************************
       120-JBE-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  120-JBE-EXT
           END-IF
      *    エラーコード
           IF      WRK-SHELLID(7:2)    NOT =   SPACE
               MOVE    WRK-SHELLID(7:2)    TO  JOB-ERRCD(1:2)
           END-IF
      *
      *    レコード更新
           PERFORM 500-SYSYMD-HENSYU-SEC
           MOVE    WRK-SYSYMD-8        TO  WRK-JOB-ENDYMD
      *****ACCEPT  WRK-JOB-ENDTIME     FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-ENDTIME
           IF      JOB-YOBI            =   SPACE
               CONTINUE
           ELSE
               MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           END-IF
           MOVE    JOB-ERRCD           TO  WRK-JOB-ERRCD
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       120-JBE-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    ステップ開始セット　処理
      *****************************************************************
       130-STS-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                130-STS-EXT
           END-IF
      *
      *    レコード更新
           MOVE    ZERO                TO  WRK-JOB-STEPENDTIME
           PERFORM 500-SYSYMD-HENSYU-SEC
      *****ACCEPT  WRK-JOB-STEPSTARTTIME
      *****                            FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-STEPSTARTTIME
           MOVE    JOB-PGID            TO  WRK-JOB-PGID
           MOVE    JOB-SHELLMSG        TO  WRK-JOB-SHELLMSG
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       130-STS-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    ステップ終了セット　処理
      *****************************************************************
       160-STE-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                160-STE-EXT
           END-IF
      *
      *    レコード更新
           PERFORM 500-SYSYMD-HENSYU-SEC
      *****ACCEPT  WRK-JOB-STEPENDTIME
      *****                            FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-STEPENDTIME
           IF      JOB-UPDCNT          NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT
           END-IF
           IF      JOB-UPDCNT1         NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT1
           END-IF
           IF      JOB-UPDCNT2         NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT2
           END-IF
           COMPUTE WRK-JOB-UPDCNT1     =   WRK-JOB-UPDCNT1  +
                                           JOB-UPDCNT1
           COMPUTE WRK-JOB-UPDCNT2     =   WRK-JOB-UPDCNT2  +
                                           JOB-UPDCNT2
           COMPUTE WRK-JOB-UPDCNT      =   WRK-JOB-UPDCNT  +
                                           JOB-UPDCNT                            
           COMPUTE WRK-JOB-STEPCNT     =   WRK-JOB-STEPCNT +
                                           1                            
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       160-STE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理
      *****************************************************************
       140-DEL-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                140-DEL-EXT
           END-IF
      *
      *    レコード更新
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "del11"
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       140-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    削除処理（対象のシェルＩＤを全て）
      *****************************************************************
       170-DELALL-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  ZERO              TO  SJOBKANRI-RETURN
               GO    TO                170-DELALL-EXT
           END-IF
      *
      *    レコード更新
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "del12"
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       170-DELALL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       150-CHK-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
           ELSE
               MOVE    ZERO            TO  SJOBKANRI-RETURN
               MOVE    WRK-JOBKANRI-REC
                                       TO  JOBKANRI-REC
           END-IF
      *
           IF    ( SJOBKANRI-MODE  =   "CH2" )
               PERFORM 910-MONBATCH-SEL-SEC
               IF    ( FLG-MONBATCH    =   ZERO )
                   MOVE    ZERO        TO  SJOBKANRI-RETURN
                   MOVE    SPACE       TO  JOB-ENDYMD
                                           JOB-ENDTIME
               END-IF
           END-IF
      *
           .
       150-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ステップ開始セット　処理
      *****************************************************************
       180-STM-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                180-STM-EXT
           END-IF
      *
      *    レコード更新
           IF      JOB-UPDCNT          NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT
           END-IF
           COMPUTE WRK-JOB-UPDCNT      =   WRK-JOB-UPDCNT  +
                                           JOB-UPDCNT                            
           MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           IF    ( JOB-SHELLMSG    NOT =   SPACE )
               MOVE    JOB-SHELLMSG    TO  WRK-JOB-SHELLMSG
           END-IF
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       180-STM-EXT.
           EXIT.
      * 
      *****************************************************************
      *    中止フラグセット処理
      *****************************************************************
       190-STP-SEC                 SECTION.
      *
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  190-STP-EXT
           END-IF
      *
      *    レコード更新
           IF      WRK-JOB-STOPFLG     =   2   OR  3
               CONTINUE
           ELSE
               MOVE    JOB-STOPFLG         TO  WRK-JOB-STOPFLG
               MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SJOBKANRI-RETURN
               ELSE
                   DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
                   MOVE  MCP-RC            TO  SJOBKANRI-RETURN
               END-IF
           END-IF    
      *
           .
       190-STP-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    ジョブ中止処理
      *****************************************************************
       200-CANCEL-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  200-CANCEL-EXT
           END-IF
      *
      *    レコード更新
           PERFORM 500-SYSYMD-HENSYU-SEC
           MOVE    WRK-SYSYMD-8        TO  WRK-JOB-ENDYMD
      *****ACCEPT  WRK-JOB-ENDTIME     FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-ENDTIME
           MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           MOVE    JOB-ERRCD           TO  WRK-JOB-ERRCD
           MOVE    JOB-STOPFLG         TO  WRK-JOB-STOPFLG
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       200-CANCEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ジョブ中止処理（終了日、終了時間の設定なし）
      *****************************************************************
       210-CANCEL2-SEC                 SECTION.
      *
      *    レコード存在チェック
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  210-CANCEL2-EXT
           END-IF
      *
      *    レコード更新
           MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           MOVE    JOB-ERRCD           TO  WRK-JOB-ERRCD
           MOVE    JOB-STOPFLG         TO  WRK-JOB-STOPFLG
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       210-CANCEL2-EXT.
           EXIT.
      * 
      *****************************************************************
      *    システム日付編集処理
      *****************************************************************
       500-SYSYMD-HENSYU-SEC       SECTION.
      *
      **** ACCEPT  WRK-SYSYMD          FROM DATE
      **** MOVE    20                  TO  WRK-SYSYY
      *
      *    システム日付取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD-8
      *
           .
       500-SYSYMD-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ジョブ管理マスタ読込
      *****************************************************************
       910-JOBKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  WRK-JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   MOVE    MCP-RC              TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    MCP-RC                  TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-CLOSE-SEC
      *
           .
       910-JOBKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理マスタ読込（ＫＥＹ２）
      *****************************************************************
       910-JOBKANRI-INV2-SEC         SECTION.
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  WRK-JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   MOVE    MCP-RC              TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    MCP-RC                  TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-CLOSE-SEC
      *
           .
       910-JOBKANRI-INV2-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONBATCH検索処理
      *****************************************************************
       910-MONBATCH-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
      *
           INITIALIZE                      SHELLTBL
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 910-MONBATCH-FET-SEC
           ELSE
               MOVE    1               TO  FLG-MONBATCH
           END-IF
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-MONBATCH-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONBATCH FETCH処理
      *****************************************************************
       910-MONBATCH-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    LOW-VALUE       TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SHELLTBL
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( SHELLTBL-HOSPNUM    =  SPA-HOSPNUM )
                    AND  ( SHELLTBL-SHELLID    =  JOB-SHELLID )
                    AND  ( SHELLTBL-JOBID      =  JOB-JOBID )
                       CONTINUE
                   ELSE
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
       910-MONBATCH-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
