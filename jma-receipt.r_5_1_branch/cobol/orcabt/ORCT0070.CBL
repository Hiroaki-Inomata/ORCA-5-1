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
       PROGRAM-ID.             ORCT0070.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 請求データ削除
      *  管理者            :
      *  作成日付    作業者        記述
      *  06/01/018   NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/05/31  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE       ASSIGN  TEIKIERR
                                       FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC.
           03  TEIKIERR-ERRCD          PIC X(04).
           03  TEIKIERR-WARNING        PIC 9(01).
           03  TEIKIERR-ERRMSG         PIC X(195).
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SHO                 PIC 9(05).
           03  WRK-AMARI               PIC 9(05).
           03  WRK-KUGIRI              PIC X(02).
           03  WRK-PTID                PIC 9(10).
           03  WRK-STR                 PIC X(200).
           03  WRK-LEN                 PIC 9(05).
           03  WRK-YMD                 PIC X(10).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1         PIC X(10).
               05  WRK-EDTYMD3         PIC X(22).
           03  WRK-SRYYM-MSG           PIC X(16).
           03  WRK-KBT-MSG             PIC X(04).
      *
       01  ERR-EDIT-AREA.
           03  ERR-WARNING             PIC 9(10).
           03  ERR-PTID                PIC 9(10).
           03  ERR-PTNUM               PIC X(20).
           03  ERR-ERRCD               PIC X(04).
           03  ERR-ERRMSG              PIC X(80).
           03  ERR-HEN-ERRMSG          PIC X(80).
      *
       01  CONST-AREA.
           03  CONST-SHELLID           PIC X(08)   VALUE   "TEIKI1".
      *
       01  ERR-MSG-AREA.
           03  ERR-MSG01                   PIC X(100)    VALUE
               "定期請求印刷情報の削除に失敗しました".
           03  ERR-MSG02                   PIC X(100)    VALUE
               "定期請求管理情報の削除に失敗しました".
           03  ERR-MSG03                   PIC X(100)    VALUE
               "日別収納情報の削除に失敗しました".
           03  ERR-MSG04                   PIC X(100)    VALUE
               "収納明細情報の削除に失敗しました".
           03  ERR-MSG05                   PIC X(100)    VALUE
               "収納情報の削除に失敗しました".
           03  ERR-MSG06                   PIC X(100)    VALUE
               "患者定期請求履歴情報の削除に失敗しました".
           03  ERR-MSG07                   PIC X(100)    VALUE
               "定期請求履歴情報の削除に失敗しました".
           03  ERR-MSG08                   PIC X(100)    VALUE
               "収納履歴情報の削除に失敗しました".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    請求削除処理パラメータ
           COPY    "CPTEIKI3PARA.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    収納マスター
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    日別収納
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納履歴
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    定期請求管理
       01  TEIKIKANRI-REC.
           COPY    "CPTEIKIKANRI.INC".
      *
      *    定期請求印刷
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    定期請求履歴
       01  TEIKIRRK-REC.
           COPY    "CPTEIKIRRK.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
      *
           COPY    "CPCOMMAND-PARAM.INC".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       000-PROC-EXT.
           EXIT.
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       TEIKI3PARA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                      INTO             TEIKI3P-ERRFILE
                                       TEIKI3P-HOSPNUM
                                       TEIKI3P-TERMID
                                       TEIKI3P-OPID
                                       TEIKI3P-SYSYMD
                                       TEIKI3P-JOBID
                                       TEIKI3P-SHELLID
                                       TEIKI3P-SRYYM
                                       TEIKI3P-KBT
                                       TEIKI3P-SKYSTYMD
                                       TEIKI3P-SKYEDYMD
           END-UNSTRING
      *
           MOVE    TEIKI3P-HOSPNUM     TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    TEIKI3P-SRYYM   TO  WRK-YMD
           MOVE    "01"            TO  WRK-YMD (7:)
           PERFORM 800-WAREKI-HEN-SEC
           MOVE    WRK-EDTYMD3(1:16)   TO  WRK-SRYYM-MSG
      *
           EVALUATE    TEIKI3P-KBT
           WHEN    "1"
               MOVE    "１期"          TO  WRK-KBT-MSG
           WHEN    "2"
               MOVE    "２期"          TO  WRK-KBT-MSG
           WHEN    "3"
               MOVE    "３期"          TO  WRK-KBT-MSG
           END-EVALUATE
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI3P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI3P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI3P-SHELLID TO  JOB-SHELLID
           MOVE    "ORCT0070"      TO  JOB-PGID
      *
           STRING  "請求削除（"    DELIMITED   BY  SIZE
                   WRK-SRYYM-MSG   DELIMITED   BY  SIZE
                   "　"            DELIMITED   BY  SIZE
                   WRK-KBT-MSG     DELIMITED   BY  SIZE
                   "）"            DELIMITED   BY  SIZE
           INTO    JOB-SHELLMSG
           END-STRING
      *
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    ZERO            TO  FLG-END
                                       FLG-ERR
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TEIKI3P-ERRFILE         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)  TO  TEIKIERR-FULLNAME
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2001-TEIKIRRK-DEL-SEC
      *
           PERFORM 900-PTTEIKIRRK-KEY11-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK  NOT =   ZERO )
      *
               PERFORM 2001-SYUDAY-DEL-SEC
      *
               PERFORM 2001-SYUMEI-DEL-SEC
      *
               PERFORM 2001-SYUNOU-DEL-SEC
      *
               PERFORM 2001-SYURRK-DEL-SEC
      *
               PERFORM 2001-TEIKIPRT-DEL-SEC
      *
               PERFORM 2001-PTTEIKIRRK-DEL-SEC
      *
               PERFORM 900-PTTEIKIRRK-KEY11-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期履歴削除処理
      *****************************************************************
       2001-TEIKIRRK-DEL-SEC           SECTION.
      *
           INITIALIZE                      TEIKIRRK-REC
           MOVE    TEIKI3P-HOSPNUM     TO  TEIKIRRK-HOSPNUM
           MOVE    TEIKI3P-SRYYM       TO  TEIKIRRK-SRYYM
           MOVE    TEIKI3P-KBT         TO  TEIKIRRK-KBT
           MOVE    TEIKIRRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikirrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    "0007"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-TEIKIRRK-DEL-EXT.
           EXIT.
      *****************************************************************
      *    日別収納削除処理
      *****************************************************************
       2001-SYUDAY-DEL-SEC             SECTION.
      *
           INITIALIZE                      SYUDAY-REC
           MOVE    PTTEIKIRRK-HOSPNUM  TO  SDAY-HOSPNUM
           MOVE    "1"                 TO  SDAY-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID     TO  SDAY-PTID
           MOVE    PTTEIKIRRK-DENPNUM  TO  SDAY-DENPNUM
           MOVE    SYUDAY-REC          TO  MCPDATA-REC
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0003"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-SYUDAY-DEL-EXT.
           EXIT.
      *****************************************************************
      *    収納明細削除処理
      *****************************************************************
       2001-SYUMEI-DEL-SEC             SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    PTTEIKIRRK-HOSPNUM   TO  SMEI-HOSPNUM
           MOVE    "1"                 TO  SMEI-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID     TO  SMEI-PTID
           MOVE    PTTEIKIRRK-DENPNUM  TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "del2"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0004"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-SYUMEI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    収納削除処理
      *****************************************************************
       2001-SYUNOU-DEL-SEC             SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    PTTEIKIRRK-HOSPNUM   TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID     TO  SYU-PTID
           MOVE    PTTEIKIRRK-DENPNUM  TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0005"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-SYUNOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    収納履歴処理
      *****************************************************************
       2001-SYURRK-DEL-SEC             SECTION.
      *
           INITIALIZE                      SYURRK-REC
           MOVE    PTTEIKIRRK-HOSPNUM   TO  SYURRK-HOSPNUM
           MOVE    "1"                 TO  SYURRK-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID     TO  SYURRK-PTID
           MOVE    PTTEIKIRRK-DENPNUM  TO  SYURRK-DENPNUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "del1"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0008"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-SYUNOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    定期請求印刷削除処理
      *****************************************************************
       2001-TEIKIPRT-DEL-SEC           SECTION.
      *
           INITIALIZE                      TEIKIPRT-REC
           MOVE    PTTEIKIRRK-HOSPNUM  TO  TEIKIPRT-HOSPNUM
           MOVE    PTTEIKIRRK-DENPNUM  TO  TEIKIPRT-DENPNUM
           MOVE    PTTEIKIRRK-PTID     TO  TEIKIPRT-PTID
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0001"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-TEIKIPRT-DEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴削除処理
      *****************************************************************
       2001-PTTEIKIRRK-DEL-SEC         SECTION.
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF  (   MCP-RC          NOT =   ZERO )
               MOVE    PTTEIKIRRK-PTID TO  ERR-PTID
               MOVE    "0006"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-PTTEIKIRRK-DEL-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           PERFORM 800-TEIKIERR-WRITE-SEC
      *
           MOVE    1                   TO  FLG-ERR
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    プログラム終了処理
           PERFORM 990-PROGRAM-FINISH-SEC
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI3P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI3P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI3P-SHELLID TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "*** ORCT0070 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーファイル出力処理
      *****************************************************************
       800-TEIKIERR-WRITE-SEC          SECTION.
      *
           PERFORM 800-GET-ERRMSG-SEC
      *
           IF    ( ERR-PTID        NOT =   ZERO )
               PERFORM 800-EDIT-ERRMSG-SEC
           END-IF
      *
           OPEN    OUTPUT              TEIKIERR-FILE
      *
           INITIALIZE                  TEIKIERR-REC
           MOVE    ERR-ERRCD           TO  TEIKIERR-ERRCD
           MOVE    ERR-WARNING         TO  TEIKIERR-WARNING
           MOVE    ERR-ERRMSG          TO  TEIKIERR-ERRMSG
           WRITE   TEIKIERR-REC
           CLOSE   TEIKIERR-FILE
      *
           .
       800-TEIKIERR-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ取得処理
      *****************************************************************
       800-GET-ERRMSG-SEC              SECTION.
      *
           EVALUATE    ERR-ERRCD
           WHEN    "0001"
               MOVE    ERR-MSG01       TO  ERR-ERRMSG
           WHEN    "0002"
               MOVE    ERR-MSG02       TO  ERR-ERRMSG
           WHEN    "0003"
               MOVE    ERR-MSG03       TO  ERR-ERRMSG
           WHEN    "0004"
               MOVE    ERR-MSG04       TO  ERR-ERRMSG
           WHEN    "0005"
               MOVE    ERR-MSG05       TO  ERR-ERRMSG
           WHEN    "0006"
               MOVE    ERR-MSG06       TO  ERR-ERRMSG
           WHEN    "0007"
               MOVE    ERR-MSG07       TO  ERR-ERRMSG
           WHEN    "0008"
               MOVE    ERR-MSG08       TO  ERR-ERRMSG
           END-EVALUATE
      *
           .
       800-GET-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       800-EDIT-ERRMSG-SEC             SECTION.
      *
           MOVE    SPACE               TO  ERR-HEN-ERRMSG
	                                   ERR-PTNUM
      *
      *    患者番号を取得
           MOVE    ERR-PTID            TO  WRK-PTID
           PERFORM 900-PTNUM-KEY-SEL-SEC
           IF    ( FLG-PTNUM           =   ZERO )
               MOVE    PTNUM-PTNUM     TO  ERR-PTNUM
           ELSE
               STRING "?(ID:"          DELIMITED   BY  SIZE
                      ERR-PTID         DELIMITED   BY  SIZE
                      ")"              DELIMITED   BY  SIZE
               INTO   ERR-PTNUM
               END-STRING
           END-IF
      *
           MOVE    1                   TO  WRK-LEN
           MOVE    SPACE               TO  WRK-STR
           STRING  ERR-PTNUM           DELIMITED   BY  SPACE
           INTO    WRK-STR
           WITH  POINTER WRK-LEN
           END-STRING
      *
           COMPUTE WRK-LEN =   WRK-LEN -   1
      *
           MOVE    ZERO                TO  WRK-SHO
                                           WRK-AMARI
           MOVE    SPACE               TO  WRK-KUGIRI
      *
           DIVIDE  2   INTO    WRK-LEN GIVING  WRK-SHO
           REMAINDER WRK-AMARI
           END-DIVIDE
      *
           IF    ( WRK-AMARI   =   ZERO )
               MOVE    "："            TO  WRK-KUGIRI
           ELSE
               MOVE    ":"             TO  WRK-KUGIRI
           END-IF
      *
           STRING  ERR-ERRMSG          DELIMITED   BY  SPACE
                   "［患者番号"        DELIMITED   BY  SIZE
                   WRK-KUGIRI          DELIMITED   BY  SPACE
                   ERR-PTNUM           DELIMITED   BY  SPACE
                   "］"                DELIMITED   BY  SIZE
           INTO    ERR-HEN-ERRMSG
           END-STRING
      *
           MOVE    ERR-HEN-ERRMSG      TO  ERR-ERRMSG
      *
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
      *
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
      *    患者定期請求履歴検索処理(KEY11)
      *****************************************************************
       900-PTTEIKIRRK-KEY11-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    TEIKI3P-HOSPNUM      TO  PTTEIKIRRK-HOSPNUM
           MOVE    TEIKI3P-SRYYM       TO  PTTEIKIRRK-SRYYM
           MOVE    TEIKI3P-KBT         TO  PTTEIKIRRK-KBT
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY11-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴FETCH処理(KEY11)
      *****************************************************************
       900-PTTEIKIRRK-KEY11-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY11-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスター読込（主キー）
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    TEIKI3P-HOSPNUM      TO  PTNUM-HOSPNUM
           MOVE    WRK-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    DBDELETE処理
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           IF    ( FLG-ERR         =   ZERO )
               MOVE    LOW-VALUE           TO  MCP-TABLE
                                               MCP-PATHNAME
               MOVE    "DBCOMMIT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-IF
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理
      *****************************************************************
       990-PROGRAM-FINISH-SEC          SECTION.
      *
            STOP   RUN
      *
           .
       990-PROGRAM-FINISH-EXT.
           EXIT.
