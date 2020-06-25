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
       PROGRAM-ID.             ORCT0060.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 収納データ突合
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.12.19  日別収納マスタ対応
      *  01.00.02    NACL-太田    04.03.30  訂正更新用データ作成追加
      *  03.05.00    NACL-太田    07/05/31  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    定期請求収納ファイル(IN)
           SELECT  TEIKI01-FILE    ASSIGN   TEIKI01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TEIKI01-KEY
                                   FILE    STATUS  IS  STS-TEIKI01.
      *
      *    請求取消対象収納ファイル(IN)
           SELECT  TEIKI04-FILE    ASSIGN   TEIKI04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TEIKI04-KEY
                                   FILE    STATUS  IS  STS-TEIKI04.
      *
      *    定期請求収納ファイル(OT) <-- 新規追加用
           SELECT  TEIKI06-FILE    ASSIGN   TEIKI06PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI06.
      *
      *    請求取消対象収納ファイル(OT)
           SELECT  TEIKI07-FILE    ASSIGN   TEIKI07PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI07.
      *
      *    単独更新用日別収納ファイル(OT)
           SELECT  TEIKI08-FILE    ASSIGN   TEIKI08PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI08.
      *
      *    定期請求収納ファイル(OT) <-- 訂正更新用
           SELECT  TEIKI09-FILE    ASSIGN   TEIKI09PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI09.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE   ASSIGN  TEIKIERR
                                   FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    定期請求収納ファイル(IN)
       FD  TEIKI01-FILE.
       01  TEIKI01-REC.
           COPY  "CPTEIKI01.INC".
      *
      *    請求取消対象収納ファイル(IN)
       FD  TEIKI04-FILE.
       01  TEIKI04-REC.
           COPY  "CPTEIKI01.INC"       REPLACING   //TEIKI01-//
                                       BY          //TEIKI04-//.
      *
      *    定期請求収納ファイル(OT) <-- 新規追加用
       FD  TEIKI06-FILE.
       01  TEIKI06-REC.
           COPY  "CPTEIKI01.INC"       REPLACING   //TEIKI01-//
                                       BY          //TEIKI06-//.
      *
      *    請求取消対象収納ファイル(OT)
       FD  TEIKI07-FILE.
       01  TEIKI07-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //TEIKI07-//.
      *
      *    単独更新用日別収納ファイル(OT)
       FD  TEIKI08-FILE.
       01  TEIKI08-REC.
      *    更新区分（１：更新（追加）／２：削除）
         02    TEIKI08-UPD-FLG         PIC 9(01).
         02    TEIKI08-SYUDAY-DAT.
           COPY    "CPSYUDAY.INC"      REPLACING   //SDAY-//
                                       BY          //TEIKI08-SDAY-//.
      *
      *    定期請求収納ファイル(OT) <-- 訂正更新用
       FD  TEIKI09-FILE.
       01  TEIKI09-REC.
           COPY  "CPTEIKI01.INC"       REPLACING   //TEIKI01-//
                                       BY          //TEIKI09-//.
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC                PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    定期請求収納ファイル名称領域(IN)
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI01PARA//.
      *
      *    請求取消対象収納ファイル名称領域 (IN)
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI04PARA//.
      *
      *    定期請求収納ファイル(OT) <-- 新規追加用
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI06PARA//.
      *
      *    定期請求収納ファイル名称領域 (OT)
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI07PARA//.
      *
      *    単独更新用日別収納ファイル(OT)
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI08PARA//.
      *
      *    定期請求収納ファイル(OT) <-- 訂正更新用
           COPY    "CPCOMMONDAT3.INC"  REPLACING  //TMPFLPARA//
                                       BY         //TEIKI09PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR            PIC X(02).
           03  STS-TEIKI01             PIC X(02).
           03  STS-TEIKI04             PIC X(02).
           03  STS-TEIKI06             PIC X(02).
           03  STS-TEIKI07             PIC X(02).
           03  STS-TEIKI08             PIC X(02).
           03  STS-TEIKI09             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                PIC 9(06).
           03  CNT-OUT2                PIC 9(06).
           03  CNT-OUT3                PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                    PIC 9(04). 
           03  IDX2                    PIC 9(04). 
           03  IDX3                    PIC 9(04). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-TEIKIERR            PIC X(200).
           03  WRK-PTID                PIC 9(10).
           03  WRK-GEN-SKYMONEY        PIC S9(09).
           03  WRK-GEN-CHOSEI          PIC S9(09).
           03  WRK-GEN-CHOSEI1         PIC S9(09).
           03  WRK-GEN-CHOSEI2         PIC S9(09).
           03  WRK-SAGAKU              PIC S9(09).
           03  WRK-STR                 PIC X(200).
           03  WRK-LEN                 PIC 9(05).
           03  WRK-SHO                 PIC 9(05).
           03  WRK-AMARI               PIC 9(05).
           03  WRK-KUGIRI              PIC X(02).
           03  WRK-YMD.
               05  WRK-YY              PIC X(04).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1         PIC X(10).
               05  WRK-EDTYMD3         PIC X(22).
           03  WRK-SHELLMSG-G.
               05  WRK-KBTMSG              PIC X(06).
               05  WRK-MONMSG              PIC X(06).
               05  WRK-SHELLMSG            PIC X(100).
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    定期請求一括処理パラメータ
           COPY    "CPTEIKI1PARA.INC".
      *
       01  ZOUSYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //ZOUSYU-//.
      *
       01  ZOUSYUDAY-REC.
           COPY    "CPSYUDAY.INC"      REPLACING   //SDAY-//
                                       BY          //ZOUSDAY-//.
      *
       01  GENSYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING   //SYU-//
                                       BY          //GENSYU-//.
      *
       01  GENSYUDAY-REC.
           COPY    "CPSYUDAY.INC"      REPLACING   //SDAY-//
                                       BY          //GENSDAY-//.
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
               "定期請求管理情報が取得できませんでした。".
           03  ERR-MSG02                   PIC X(100)    VALUE
               "処理対象のデータがありませんでした".
           03  ERR-MSG88                   PIC X(100)    VALUE
               "キャンセルしました".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5010.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
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
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
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
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
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
           MOVE    TEIKI1P-HOSPNUM     TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 1001-EDIT-SHELLMSG-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    TEIKI1P-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID       TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID     TO  JOB-SHELLID
           MOVE    "ORCT0060"          TO  JOB-PGID
           MOVE    WRK-SHELLMSG        TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    ZERO                TO  FLG-END
                                           FLG-ERR
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI01PARA-HOSPNUM
           MOVE    "TEIKI001"          TO  TEIKI01PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI01PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI04PARA-HOSPNUM
           MOVE    "TEIKI004"          TO  TEIKI04PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI04PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI06PARA-HOSPNUM
           MOVE    "TEIKI006"          TO  TEIKI06PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI06PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI07PARA-HOSPNUM
           MOVE    "TEIKI007"          TO  TEIKI07PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI07PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI08PARA-HOSPNUM
           MOVE    "TEIKI008"          TO  TEIKI08PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI08PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM     TO  TEIKI09PARA-HOSPNUM
           MOVE    "TEIKI009"          TO  TEIKI09PARA-FILE-ID
           MOVE    TEIKI1P-TERMID      TO  TEIKI09PARA-TERMID
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    TEIKI01PARA-BASENAME    TO  SGETTEMP-BASENAMES (1)
           MOVE    TEIKI04PARA-BASENAME    TO  SGETTEMP-BASENAMES (2)
           MOVE    TEIKI06PARA-BASENAME    TO  SGETTEMP-BASENAMES (3)
           MOVE    TEIKI07PARA-BASENAME    TO  SGETTEMP-BASENAMES (4)
           MOVE    TEIKI08PARA-BASENAME    TO  SGETTEMP-BASENAMES (5)
           MOVE    TEIKI09PARA-BASENAME    TO  SGETTEMP-BASENAMES (6)
           MOVE    TEIKI1P-ERRFILE         TO  SGETTEMP-BASENAMES (7)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1) TO   TEIKI01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2) TO   TEIKI04PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3) TO   TEIKI06PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (4) TO   TEIKI07PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (5) TO   TEIKI08PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (6) TO   TEIKI09PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (7) TO   TEIKIERR-FULLNAME
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
           OPEN    INPUT               TEIKI01-FILE
           OPEN    INPUT               TEIKI04-FILE
           OPEN    OUTPUT              TEIKI06-FILE
           OPEN    OUTPUT              TEIKI07-FILE
           OPEN    OUTPUT              TEIKI08-FILE
           OPEN    OUTPUT              TEIKI09-FILE
      *
      *    定期請求管理情報
           INITIALIZE                  SYS-5010-REC
           MOVE    "5010"              TO  SYS-5010-KANRICD
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    TEIKI1P-SYSYMD      TO  SYS-5010-STYUKYMD
                                           SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5010-REC
           ELSE
               MOVE    "0001"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
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
           STRING  WRK-MONMSG          DELIMITED   BY  SPACE
                   WRK-KBTMSG          DELIMITED   BY  SIZE
                   "収納データ突合"    DELIMITED   BY  SIZE
           INTO    WRK-SHELLMSG
           END-STRING
      *
           .
       1001-EDIT-SHELLMSG-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 910-TEIKI01-READ-SEC
           PERFORM 910-TEIKI04-READ-SEC
      *
      *
           PERFORM UNTIL ( TEIKI01-MAIN-KEY =   HIGH-VALUE )
                   AND   ( TEIKI04-MAIN-KEY =   HIGH-VALUE )
      *
               EVALUATE    TRUE
               WHEN    TEIKI01-MAIN-KEY     <   TEIKI04-MAIN-KEY
                   PERFORM 2001-TEIKI06-WRITE-SEC
                   PERFORM 910-TEIKI01-READ-SEC
               WHEN    TEIKI01-MAIN-KEY     >   TEIKI04-MAIN-KEY
                   PERFORM 2001-TEIKI07-WRITE-SEC
                   PERFORM 910-TEIKI04-READ-SEC
               WHEN    TEIKI01-MAIN-KEY     =   TEIKI04-MAIN-KEY
      *            同一収納チェック
                   PERFORM 2001-DOUITU-SYUNOU-CHK-SEC
                   PERFORM 910-TEIKI01-READ-SEC
                   PERFORM 910-TEIKI04-READ-SEC
               END-EVALUATE
      *
           END-PERFORM
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一収納チェック
      *****************************************************************
       2001-DOUITU-SYUNOU-CHK-SEC      SECTION.
      *
           MOVE    TEIKI01-SYU-DAT     TO  ZOUSYUNOU-REC
           MOVE    TEIKI01-SYUDAY-DAT  TO  ZOUSYUDAY-REC
      *
           MOVE    TEIKI04-SYU-DAT     TO  GENSYUNOU-REC
           MOVE    TEIKI04-SYUDAY-DAT  TO  GENSYUDAY-REC
      *
           INITIALIZE                  ZOUSYU-DENPNUM
                                       ZOUSYU-DENPLASTNUM
                                       ZOUSYU-DENPJTIKBN
                                       ZOUSYU-CREATEKBN
                                       ZOUSYU-DENPPRTYMD
                                       ZOUSYU-NYUKIN-TOTAL
                                       ZOUSYU-NYUKIN-KAISU
                                       ZOUSYU-NYUIN-RRKNUM
                                       ZOUSYU-DAYINFFLG
                                       ZOUSYU-TERMID
                                       ZOUSYU-OPID
                                       ZOUSYU-CREYMD
                                       ZOUSYU-UPYMD
                                       ZOUSYU-UPHMS
                                       ZOUSYU-UPSRYYMD
                                       ZOUSYU-SELKOHID
      *
           INITIALIZE                  GENSYU-DENPNUM
                                       GENSYU-DENPLASTNUM
                                       GENSYU-DENPJTIKBN
                                       GENSYU-CREATEKBN
                                       GENSYU-DENPPRTYMD
                                       GENSYU-NYUKIN-TOTAL
                                       GENSYU-NYUKIN-KAISU
                                       GENSYU-NYUIN-RRKNUM
                                       GENSYU-DAYINFFLG
                                       GENSYU-TERMID
                                       GENSYU-OPID
                                       GENSYU-CREYMD
                                       GENSYU-UPYMD
                                       GENSYU-UPHMS
                                       GENSYU-UPSRYYMD
                                       GENSYU-SELKOHID
      *
           COMPUTE GENSYU-SKYMONEY
               =   GENSYU-SKYMONEY -   GENSYU-CHOSEI
      *
           MOVE    ZERO            TO  GENSYU-CHOSEI
                                       GENSYU-CHOSEI1
                                       GENSYU-CHOSEI2
      *
           IF    ( ZOUSYUNOU-REC   NOT =   GENSYUNOU-REC )
      *
               EVALUATE    SYS-5010-SAISYORIKBN
               WHEN    "2"
                   PERFORM 2001-TEIKI09-WRITE-SEC
               WHEN    OTHER
                   PERFORM 2001-TEIKI06-WRITE-SEC
                   PERFORM 2001-TEIKI07-WRITE-SEC
               END-EVALUATE
           ELSE
      *        単独更新用日別収納ファイル編集処理
               PERFORM 2001-TEIKI08-HEN-SEC
           END-IF
      *
           .
       2001-DOUITU-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    定期請求収納ファイル出力処理
      *****************************************************************
       2001-TEIKI06-WRITE-SEC          SECTION.
      *
           INITIALIZE                  TEIKI06-REC
      *
           MOVE    TEIKI01-KEY         TO  TEIKI06-KEY
      *
           MOVE    TEIKI01-SAKKBN      TO  TEIKI06-SAKKBN
           MOVE    TEIKI01-KBT         TO  TEIKI06-KBT
      *
           MOVE    TEIKI01-SYU-DAT     TO  TEIKI06-SYU-DAT
           MOVE    TEIKI01-SYUDAY-EXIST-FLG
                                       TO  TEIKI06-SYUDAY-EXIST-FLG
           MOVE    TEIKI01-SYUDAY-DAT
                                       TO  TEIKI06-SYUDAY-DAT
           WRITE   TEIKI06-REC
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
           .
       2001-TEIKI06-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    請求取消対象収納ファイル出力処理
      *****************************************************************
       2001-TEIKI07-WRITE-SEC          SECTION.
      *
           MOVE    TEIKI04-SYU-DAT     TO  TEIKI07-REC
           WRITE   TEIKI07-REC
           COMPUTE CNT-OUT2    =   CNT-OUT2    +   1
      *
           .
       2001-TEIKI07-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    定期請求収納ファイル出力処理
      *****************************************************************
       2001-TEIKI09-WRITE-SEC          SECTION.
      *
           MOVE    TEIKI01-SYU-DAT     TO  ZOUSYUNOU-REC
           MOVE    TEIKI04-SYU-DAT     TO  GENSYUNOU-REC
      *
           MOVE    GENSYU-CHOSEI       TO  WRK-GEN-CHOSEI
           MOVE    GENSYU-CHOSEI1      TO  WRK-GEN-CHOSEI1
           MOVE    GENSYU-CHOSEI2      TO  WRK-GEN-CHOSEI2
      *
           COMPUTE WRK-GEN-SKYMONEY
               =   GENSYU-SKYMONEY     -   WRK-GEN-CHOSEI
      *
           COMPUTE WRK-SAGAKU
               =   WRK-GEN-SKYMONEY
               -   ZOUSYU-SKYMONEY
      *
           COMPUTE TEIKI09-TSI-SKYTEN
               =   ZOUSYU-TOTAL-HKNTEN
               -   GENSYU-TOTAL-HKNTEN
      *
           COMPUTE TEIKI09-TSI-SKYMONEY-TAX-SAI
               =   ZOUSYU-SKYMONEY-TAX-SAI
               -   GENSYU-SKYMONEY-TAX-SAI
      *
           IF    ( WRK-SAGAKU          >   ZERO )
               IF    ( WRK-SAGAKU      >   GENSYU-SKYMONEY )
                   COMPUTE ZOUSYU-CHOSEI
                       =   ZOUSYU-SKYMONEY
                       *   -1
                   MOVE    ZERO            TO  ZOUSYU-SKYMONEY
                   IF    ( GENSYU-CHOSEI2  =   ZERO )
      *
                       COMPUTE ZOUSYU-CHOSEI2
                           =   GENSYU-CHOSEI2
      *
                       COMPUTE ZOUSYU-CHOSEI1
                           =   GENSYU-CHOSEI1
                           -   GENSYU-CHOSEI
                           +   ZOUSYU-CHOSEI
                   ELSE
                       COMPUTE ZOUSYU-CHOSEI1
                           =   GENSYU-CHOSEI1
      *
                       COMPUTE ZOUSYU-CHOSEI2
                           =   GENSYU-CHOSEI2
                           -   GENSYU-CHOSEI
                           +   ZOUSYU-CHOSEI
                   END-IF
               ELSE
                   COMPUTE ZOUSYU-SKYMONEY
                       =   GENSYU-SKYMONEY
                       -   WRK-SAGAKU
      *
                   MOVE    WRK-GEN-CHOSEI  TO  ZOUSYU-CHOSEI
                   MOVE    WRK-GEN-CHOSEI1 TO  ZOUSYU-CHOSEI1
                   MOVE    WRK-GEN-CHOSEI2 TO  ZOUSYU-CHOSEI2
               END-IF
           ELSE
      *
               COMPUTE ZOUSYU-SKYMONEY
                   =   ZOUSYU-SKYMONEY
                   +   WRK-GEN-CHOSEI
      *
               MOVE    WRK-GEN-CHOSEI  TO  ZOUSYU-CHOSEI
               MOVE    WRK-GEN-CHOSEI1 TO  ZOUSYU-CHOSEI1
               MOVE    WRK-GEN-CHOSEI2 TO  ZOUSYU-CHOSEI2
      *
           END-IF
      *
           COMPUTE TEIKI09-TSI-SKYMONEY
               =   ZOUSYU-SKYMONEY
               -   GENSYU-SKYMONEY
      *
           MOVE    GENSYU-DENPNUM      TO  ZOUSYU-DENPNUM
      *
           MOVE    GENSYU-NYUKIN-KAISU
                                       TO  ZOUSYU-NYUKIN-KAISU
           MOVE    GENSYU-NYUKIN-TOTAL
                                       TO  ZOUSYU-NYUKIN-TOTAL
      *
           MOVE    GENSYU-CREYMD       TO  ZOUSYU-CREYMD
      *
           MOVE    TEIKI01-KEY         TO  TEIKI09-KEY
           MOVE    TEIKI01-SAKKBN      TO  TEIKI09-SAKKBN
           MOVE    TEIKI01-KBT         TO  TEIKI09-KBT
      *
           MOVE    ZOUSYUNOU-REC       TO  TEIKI09-SYU-DAT
           MOVE    TEIKI01-SYUDAY-EXIST-FLG
                                       TO  TEIKI09-SYUDAY-EXIST-FLG
           MOVE    TEIKI01-SYUDAY-DAT
                                       TO  TEIKI09-SYUDAY-DAT
           WRITE   TEIKI09-REC
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
           .
       2001-TEIKI09-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    単独更新用日別ファイル出力処理
      *****************************************************************
       2001-TEIKI08-HEN-SEC            SECTION.
      *
           MOVE    TEIKI01-SYU-DAT     TO  ZOUSYUNOU-REC
           MOVE    TEIKI01-SYUDAY-DAT  TO  ZOUSYUDAY-REC
      *
           MOVE    TEIKI04-SYU-DAT     TO  GENSYUNOU-REC
           MOVE    TEIKI04-SYUDAY-DAT  TO  GENSYUDAY-REC
      *
           EVALUATE TEIKI01-SYUDAY-EXIST-FLG
                                       ALSO    TEIKI04-SYUDAY-EXIST-FLG
      *    新日別と旧日別がともに存在し、内容が異なる場合、
      *    旧日別の内容を新日別の内容で置き換える
           WHEN    1       ALSO    1
               IF    ( ZOUSDAY-DAY-AREA    NOT =   GENSDAY-DAY-AREA )
                   INITIALIZE              TEIKI08-REC
                   MOVE    1       TO      TEIKI08-UPD-FLG
                   MOVE    ZOUSYUDAY-REC
                                   TO      TEIKI08-SYUDAY-DAT
                   MOVE    GENSYU-DENPNUM
                                   TO      TEIKI08-SDAY-DENPNUM
                   WRITE   TEIKI08-REC
                   COMPUTE CNT-OUT3    =   CNT-OUT3    +   1
               END-IF
      *
      *    新日別は存在するが旧日別が存在しない場合、
      *    新日別を追加する
           WHEN    1       ALSO    ZERO
               INITIALIZE              TEIKI08-REC
               MOVE    1       TO      TEIKI08-UPD-FLG
               MOVE    ZOUSYUDAY-REC
                               TO      TEIKI08-SYUDAY-DAT
               MOVE    GENSYU-DENPNUM
                               TO      TEIKI08-SDAY-DENPNUM
               WRITE   TEIKI08-REC
               COMPUTE CNT-OUT3    =   CNT-OUT3    +   1
      *
      *    新日別は存在せず旧日別が存在する場合、
      *    旧日別を削除する
           WHEN    ZERO    ALSO    1
               INITIALIZE              TEIKI08-REC
               MOVE    2       TO      TEIKI08-UPD-FLG
               MOVE    GENSYUDAY-REC
                               TO      TEIKI08-SYUDAY-DAT
               WRITE   TEIKI08-REC
               COMPUTE CNT-OUT3    =   CNT-OUT3    +   1
           END-EVALUATE
      *
           .
       2001-TEIKI08-HEN-EXT.
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
           CLOSE   TEIKI01-FILE
                   TEIKI04-FILE
                   TEIKI06-FILE
                   TEIKI07-FILE
                   TEIKI08-FILE
                   TEIKI09-FILE
      *
           IF    ( CNT-OUT1            =   ZERO )
            AND  ( CNT-OUT3            =   ZERO )
               MOVE    1           TO  ERR-WARNING
               MOVE    "0002"      TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID   TO  JOB-JOBID
           MOVE    TEIKI1P-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TEIKI01 (ORCT0060) CNT " CNT-OUT1 
           DISPLAY "*** ORCT0060 END ***"
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
           WHEN    "0002"
               MOVE    ERR-MSG02       TO  ERR-ERRMSG
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
           MOVE    ERR-HEN-ERRMSG      TO  ERR-ERRMSG
           PERFORM 500-ERR-HENSYU-SEC
      *
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
      *****************************************************************
      *    定期請求収納ファイル読込
      *****************************************************************
       910-TEIKI01-READ-SEC            SECTION.
      *
           READ    TEIKI01-FILE        NEXT
           AT  END
               MOVE    HIGH-VALUE      TO  TEIKI01-MAIN-KEY
           END-READ
      *
           .
       910-TEIKI01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求取消対象収納ファイル読込
      *****************************************************************
       910-TEIKI04-READ-SEC            SECTION.
      *
           READ    TEIKI04-FILE        NEXT
           AT  END
               MOVE    HIGH-VALUE      TO  TEIKI04-MAIN-KEY
           END-READ
      *
           .
       910-TEIKI04-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    TEIKI1P-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
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
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
      *
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
