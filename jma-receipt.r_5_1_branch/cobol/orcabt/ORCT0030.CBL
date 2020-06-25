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
       PROGRAM-ID.             ORCT0030.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 収納データチェック
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.12.19  日別収納マスタ対応
      *  01.00.02    NACL-太田    04.03.03  削除対象患者請求履歴ファイル
      *                                     の編集をORCT0040に移す
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    定期請求期間情報ファイル(IN)
           SELECT  TEIKI03-FILE    ASSIGN  TEIKI03PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI03.
      *
      *
      *    請求取消対象収納ファイル(OT)
           SELECT  TEIKI04-FILE    ASSIGN   TEIKI04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TEIKI04-KEY
                                   FILE    STATUS  IS  STS-TEIKI04.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE   ASSIGN  TEIKIERR
                                   FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    定期請求期間情報
       FD  TEIKI03-FILE.
           COPY "CPTEIKI03.INC".
      *
      *    請求取消対象収納ファイル
       FD  TEIKI04-FILE.
       01  TEIKI04-REC.
           COPY "CPTEIKI01.INC"            REPLACING   //TEIKI01-//
                                           BY          //TEIKI04-//.
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC                    PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    定期請求期間情報名称領域 
           COPY    "CPCOMMONDAT3.INC"      REPLACING  //TMPFLPARA//
                                           BY         //TEIKI03PARA//.
      *
      *    請求取消対象収納ファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"      REPLACING  //TMPFLPARA//
                                           BY         //TEIKI04PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR                PIC X(02).
           03  STS-TEIKI03                 PIC X(02).
           03  STS-TEIKI04                 PIC X(02).
           03  STS-TEIKI04CL               PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-TEIKI03                 PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-SYUDAY                  PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                    PIC 9(06).
           03  CNT-OUT2                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                        PIC 9(04). 
           03  IDX2                        PIC 9(04). 
           03  IDX3                        PIC 9(04). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-STR                     PIC X(200).
           03  WRK-LEN                     PIC 9(05).
           03  WRK-SHO                     PIC 9(05).
           03  WRK-AMARI                   PIC 9(05).
           03  WRK-KUGIRI                  PIC X(02).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-SHELLMSG-G.
               05  WRK-KBTMSG              PIC X(06).
               05  WRK-MONMSG              PIC X(06).
               05  WRK-SHELLMSG            PIC X(100).
           03  WRK-YMD.
               05  WRK-YY                  PIC X(04).
               05  WRK-MM                  PIC X(02).
               05  WRK-DD                  PIC X(02).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1             PIC X(10).
               05  WRK-EDTYMD3             PIC X(22).
      *
       01  ERR-EDIT-AREA.
           03  ERR-WARNING                 PIC 9(01).
           03  ERR-PTID                    PIC 9(10).
           03  ERR-PTNUM                   PIC X(20).
           03  ERR-ERRCD                   PIC X(04).
           03  ERR-ERRMSG                  PIC X(80).
           03  ERR-HEN-ERRMSG              PIC X(80).
      *
       01  ERR-MSG-AREA.
           03  ERR-MSG01                   PIC X(100)    VALUE
               "患者定期請求履歴データの取得に失敗しました".
           03  ERR-MSG88                   PIC X(100)    VALUE
               "キャンセルしました".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    定期請求一括処理パラメータ
           COPY    "CPTEIKI1PARA.INC".
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
      *    日別収納マスタ
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    定期請求サブ
           COPY    "CPORCSTEIKI01.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    定期請求用収納作成サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
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
           INITIALIZE                  INDEX-AREA
                                       CNT-AREA
                                       WRK-AREA
                                       SPA-AREA
                                       TEIKI1PARA-AREA
                                       ERR-EDIT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    TEIKI1P-ARG1
                                               TEIKI1P-SYOKBN
                                               TEIKI1P-KIKAN-OCC (1)
                                               TEIKI1P-KIKAN-OCC (2)
                                               TEIKI1P-KIKAN-OCC (3)
                                               TEIKI1P-DENPPRTYMD
                                               TEIKI1P-JOBID
                                               TEIKI1P-SHELLID
                                               TEIKI1P-HOSPNUM
                                               TEIKI1P-ERRFILE
                                               TEIKI1P-KBT
                                               TEIKI1P-JOBYMD
                                               TEIKI1P-JOBHMS
           END-UNSTRING
      *
           MOVE    TEIKI1P-HOSPNUM TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 1001-EDIT-SHELLMSG-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID TO  JOB-SHELLID
           MOVE    "ORCT0030"      TO  JOB-PGID
           MOVE    WRK-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    ZERO            TO  FLG-END
                                       FLG-ERR
      *
           MOVE    TEIKI1P-HOSPNUM TO  TEIKI03PARA-HOSPNUM
           MOVE    "TEIKI003"      TO  TEIKI03PARA-FILE-ID
           MOVE    TEIKI1P-TERMID  TO  TEIKI03PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM TO  TEIKI04PARA-HOSPNUM
           MOVE    "TEIKI004"      TO  TEIKI04PARA-FILE-ID
           MOVE    TEIKI1P-TERMID  TO  TEIKI04PARA-TERMID
      *
      *
           INITIALIZE                  SGETTEMP-AREA
      *
           MOVE    TEIKI03PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    TEIKI04PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (2)
           MOVE    TEIKI1P-ERRFILE TO  SGETTEMP-BASENAMES (3)

           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                   TO  TEIKI03PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(2)
                                   TO  TEIKI04PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(3)
                                   TO  TEIKIERR-FULLNAME
      *
           INITIALIZE                  ORCSTEIKI01AREA
           MOVE    TEIKI1P-JOBID   TO  STEIKI01-JOBID
           MOVE    TEIKI1P-SHELLID TO  STEIKI01-SHELLID
           CALL    "ORCSTEIKI01"   USING   ORCSTEIKI01AREA
                                           SPA-AREA
           IF    ( STEIKI01-STOP   =   1 )
               MOVE    "8888"      TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           OPEN    INPUT               TEIKI03-FILE
           OPEN    OUTPUT              TEIKI04-FILE
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    シェルメッセージ編集処理
      *****************************************************************
       1001-EDIT-SHELLMSG-SEC          SECTION.
      *
           INITIALIZE                  WRK-SHELLMSG-G
      *
           IF    ( TEIKI1P-SYOKBN      =   "1" )
               MOVE    TEIKI1P-SRYYM   TO  WRK-YMD
               MOVE    "01"            TO  WRK-YMD (7:2)
               PERFORM 800-WAREKI-HEN-SEC
               MOVE    WRK-EDTYMD3 (11:6)  TO  WRK-MONMSG
           END-IF
      *
           EVALUATE    TEIKI1P-KBT
           WHEN    1
               MOVE    "１期："        TO  WRK-KBTMSG
           WHEN    2
               MOVE    "２期："        TO  WRK-KBTMSG
           WHEN    3
               MOVE    "３期："        TO  WRK-KBTMSG
           END-EVALUATE
      *
           STRING  WRK-MONMSG              DELIMITED   BY  SPACE
                   WRK-KBTMSG              DELIMITED   BY  SIZE
                   "収納データチェック"    DELIMITED   BY  SIZE
           INTO    WRK-SHELLMSG
           END-STRING
      *
           .
       1001-EDIT-SHELLMSG-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-TEIKI03
      *
           PERFORM 900-TEIKI03-READ-SEC
      *
           PERFORM UNTIL ( FLG-TEIKI03 NOT =   ZERO )
      *
      *        収納データチェック処理
               PERFORM 2001-SYUNOU-CHK-SEC
      *
               PERFORM 900-TEIKI03-READ-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納データチェック処理
      *****************************************************************
       2001-SYUNOU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
           INITIALIZE   SYUNOU-REC
           MOVE    TEIKI03-HOSPNUM      TO  SYU-HOSPNUM
           MOVE     "1"                TO  SYU-NYUGAIKBN
           MOVE    TEIKI03-PTID        TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
      *    今回定期請求期間にかかる収納データを抽出する
           PERFORM UNTIL ( FLG-SYUNOU  NOT     =   ZERO )
      *
               IF    ( TEIKI03-SKYSTYMD       <=  SYU-SKYEDYMD )
                AND  ( TEIKI03-SKYEDYMD       >=  SYU-SKYSTYMD )
                   IF    ( SYU-DENPJTIKBN  NOT =   "3" )
                    AND  ( SYU-CREATEKBN       =   "2" )
                       PERFORM 20011-TEIKI04-OUTPUT-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
               ELSE
                   MOVE    1           TO  FLG-SYUNOU
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    請求取消対象収納ファイル出力処理
      *****************************************************************
       20011-TEIKI04-OUTPUT-SEC      SECTION.
      *
           MOVE    TEIKI1P-TERMID      TO  SYU-TERMID
      *
           MOVE    SYUNOU-REC          TO  TEIKI04-SYU-DAT
      *
      *    日別収納マスタ検索処理
           PERFORM 900-SYUDAY-KEY-SEL-SEC
           IF    ( FLG-SYUDAY          =   ZERO )
               MOVE    1               TO  TEIKI04-SYUDAY-EXIST-FLG
               MOVE    SYUDAY-REC      TO  TEIKI04-SYUDAY-DAT
           ELSE
               MOVE    ZERO            TO  TEIKI04-SYUDAY-EXIST-FLG
               MOVE    SPACE           TO  TEIKI04-SYUDAY-DAT
           END-IF
      *
      *    患者定期請求履歴検索処理
           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
           IF    ( FLG-PTTEIKIRRK  NOT =   ZERO )
               MOVE    SYU-PTID        TO  ERR-PTID
               MOVE    "0001"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    SYU-HOSPNUM          TO  TEIKI04-HOSPNUM
           MOVE    SYU-PTID            TO  TEIKI04-PTID
           MOVE    PTTEIKIRRK-SAKKBN   TO  TEIKI04-SAKKBN
           MOVE    PTTEIKIRRK-SKYSTYMD TO  TEIKI04-SKYSTYMD
           MOVE    PTTEIKIRRK-SKYEDYMD TO  TEIKI04-SKYEDYMD
           MOVE    SYU-HKNCOMBINUM     TO  TEIKI04-HKNCOMBINUM
      *
           WRITE   TEIKI04-REC
      *
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
           .
       20011-TEIKI04-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
      *
           PERFORM 800-GET-ERRMSG-SEC
      *
           IF    ( ERR-PTID        NOT =   ZERO )
               PERFORM 800-EDIT-ERRMSG-SEC
           END-IF
      *
           OPEN    OUTPUT              TEIKIERR-FILE
      *
           MOVE    ERR-ERRMSG      TO  TEIKIERR-REC
           WRITE   TEIKIERR-REC
           CLOSE   TEIKIERR-FILE
      *
           INITIALIZE                  JOBKANRI-REC
           IF    ( ERR-WARNING     =   1 )
      *        ジョブ管理終了処理
               MOVE    "JBE"       TO  SJOBKANRI-MODE
           ELSE
      *        ジョブ管理キャンセル処理
               MOVE    "CAN"       TO  SJOBKANRI-MODE
               MOVE    2           TO  JOB-STOPFLG
           END-IF
      *
           MOVE    TEIKI1P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID
                                   TO  JOB-SHELLID
           MOVE    ERR-ERRMSG      TO  JOB-YOBI
           MOVE    ERR-ERRCD       TO  JOB-ERRCD
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    1               TO  FLG-ERR
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
           CLOSE   TEIKI03-FILE
                   TEIKI04-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TEIKI01 (ORCT0030) CNT " CNT-OUT1 
           DISPLAY "*** ORCT0030 END ***"
      ***  ACCEPT  WRK-IN FROM CONSOLE 
           .
       300-TERM-EXT.
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
      *    エラーメッセージ取得処理
      *****************************************************************
       800-GET-ERRMSG-SEC              SECTION.
      *
           EVALUATE    ERR-ERRCD
           WHEN    "0001"
               MOVE    ERR-MSG01       TO  ERR-ERRMSG
           WHEN    "8888"
               MOVE    ERR-MSG88       TO  ERR-ERRMSG
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
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    日別収納マスタ検索処理
      *****************************************************************
       900-SYUDAY-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUDAY
      *
           INITIALIZE                      SYUDAY-REC
           MOVE    SYU-HOSPNUM          TO  SDAY-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SDAY-NYUGAIKBN
           MOVE    SYU-PTID            TO  SDAY-PTID
           MOVE    SYU-DENPNUM         TO  SDAY-DENPNUM
           MOVE    SYUDAY-REC          TO  MCPDATA-REC
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUDAY-REC
           ELSE
               MOVE    1               TO  FLG-SYUDAY
           END-IF
      *
           MOVE    "tbl_syuday"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUDAY-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                      PTTEIKIRRK-REC
      *
           MOVE    SYU-HOSPNUM          TO  PTTEIKIRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTTEIKIRRK-PTID
           MOVE    SYU-DENPNUM         TO  PTTEIKIRRK-DENPNUM
      *
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    定期請求期間情報読込
      *****************************************************************
       900-TEIKI03-READ-SEC            SECTION.
      *
           READ    TEIKI03-FILE
           AT  END
               MOVE    1               TO  FLG-TEIKI03
           END-READ
      *
           .
       900-TEIKI03-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込（主キー）
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    TEIKI1P-HOSPNUM     TO  PTNUM-HOSPNUM
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
      *    ＤＢ検索処理（FHETCHは行わない)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
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
