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
       PROGRAM-ID.                 ORCXRMST2MIGV51.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : マスタ更新処理
      *  コンポーネント名  : 更新プログラム実行処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/02/22    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-太田     02/03/18  点数マスタレイアウト変更
      *                                     対応
      * 02.05.00     NACL-竹田    05/04/05  バージョン依存緩和処理追加
      * 03.00.00     NACL-伊藤    06/05/26  MONFUNC対応
      * 04.03.00     NACL-太田    08/05/22  異常処理系強化
      * 05.01.00     NACL-伊藤    18/11/27  V510マスター分離対応特別版
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：パラメータファイル
           SELECT  IN-FILE         ASSIGN  PGEXIST01
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *
      *    エラーファイル
           SELECT  MSTERR-FILE     ASSIGN  RECEERR
                                   FILE    STATUS IS  STS-MSTERR.
      *
      *    ＰＲＦファイル（マスタデータ別）
           SELECT  MSTPRF-FILE     ASSIGN  MSTPRFFILE
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS IS  STS-MSTPRFFILE.
      *
      *    ＰＲＦファイル
           SELECT  PRF-FILE        ASSIGN  PARA-PRFFILE
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS IS  STS-PRFFILE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力ファイル
       FD  IN-FILE.
       01  IN-REC.
           03  IN-FIL              PIC X(01).
      *
      *    エラーファイル
       FD  MSTERR-FILE.
       01  MSTERR-REC              PIC X(200).
      *
       FD  MSTPRF-FILE.
       01  MSTPRF-REC              PIC X(1000).
      *
       FD  PRF-FILE.
       01  PRF-REC                 PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "HCMSL55.INC".
      *
           COPY    "CPRECEERR.INC".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  PGEXIST01               PIC X(256).
           03  MSTPRFFILE              PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-IN                  PIC X(02).
           03  STS-MSTERR              PIC X(02).
           03  STS-OUT                 PIC X(02).
           03  STS-PRFFILE             PIC X(02).
           03  STS-MSTPRFFILE          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-IN                  PIC 9(01).
           03  FLG-MSTKANRI            PIC 9(01).
           03  FLG-END                 PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-PRF                 PIC 9(01).
           03  FLG-MSTPRF              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-PAGE                PIC 9(06).
           03  CNT-LINE                PIC 9(06).
           03  CNT-MSTPRF              PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
      *
           03  WRK-IN                  PIC X(01). 
      *
           03  WRK-UPFILE              PIC X(100).
           03  WRK-UPFILE2             PIC X(100).
           03  WRK-MSTERR              PIC X(200).
           03  WRK-MSTNAME             PIC X(60).
           03  WRK-ZZZZZ9              PIC ZZZZZ9.
      *
           03  WRK-HOMEDIR             PIC X(128). 
           03  WRK-UPPG.
               05  WRK-UPPG-PARA       PIC X(10).
               05  WRK-UPPG-MIG        PIC X(06).
      *
       01  PARA-AREA.
           03  PARA-MAIN.
               05  PARA-HOSPNUM              PIC 9(02).
               05  PARA-DBFILE               PIC X(13).
               05  PARA-MASTER-KBN           PIC X(02).
               05  PARA-SEPARATE             PIC X(01).
               05  PARA-DBRVERVION           PIC X(21).
               05  PARA-UPPG                 PIC X(10).
               05  PARA-UPFILE               PIC X(100).
           03  PARA-MSTCOUNT                 PIC 9(06).
           03  PARA-MSTSEQ                   PIC 9(06).
           03  PARA-MSTCNTMSG                PIC X(20).
           03  PARA-COUNT                    PIC 9(06).
           03  PARA-ERRFILE                  PIC X(100).
           03  PARA-JOBID                    PIC  9(07).
           03  PARA-SHELLID                  PIC  X(08).
           03  PARA-PRFFILE                  PIC X(100).
      *
       01  PRF-HEAD-AREA.
           03  PRF-HNAME                      PIC  X(60).
           03  PRF-HFL1                       PIC  X(01).
           03  PRF-HDBRVERVION                PIC  X(21).
           03  PRF-HFL2                       PIC  X(01).
           03  PRF-HDATE                      PIC  X(10).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR                 PIC  X(100)   VALUE
               "/var/lib/jma-receipt/master/".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    マスタ管理情報
       01  MSTKANRI-REC.
           COPY    "CPMSTKANRI.INC".
      *
      *    システム管理
           COPY    "CPSK1031.INC".
      *
      *    ジョブ管理情報
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
           COPY    "CPMUPLINK.INC".
      *
           COPY    "CPHOMEDIR.INC".
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル存在チェックサブ
           COPY    "CPORCSACCESS.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
       01  PARA-REC                PIC   X(1000).
      *
       PROCEDURE                       DIVISION
           USING
           PARA-REC.
      *
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           MOVE    ZERO                TO  RETURN-CODE
      *
           EXIT    PROGRAM
      *
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-AREA
           MOVE    ZERO            TO  CNT-AREA
           MOVE    ZERO            TO  IDX-AREA
           INITIALIZE                  WRK-AREA
                                       SPA-AREA
                                       PARA-AREA
      *
           MOVE    SPACE           TO  RECEERR
      *
           UNSTRING    PARA-REC    DELIMITED   BY  ","
           INTO                    PARA-MAIN
                                   PARA-MSTCOUNT
                                   PARA-MSTSEQ
                                   PARA-MSTCNTMSG
                                   PARA-COUNT
                                   PARA-ERRFILE
                                   PARA-JOBID
                                   PARA-SHELLID
                                   PARA-PRFFILE
           END-UNSTRING
      *
           MOVE    PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 800-MCNDATE-SEC
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 800-MSTNAME-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    PARA-ERRFILE    TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    PARA-JOBID      TO  JOB-JOBID
           MOVE    PARA-SHELLID    TO  JOB-SHELLID
           PERFORM 800-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF  ( JOB-STOPFLG       =   1    )
                   PERFORM 500-CAN-HENSYU-SEC
               END-IF
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    PARA-HOSPNUM    TO  JOB-HOSPNUM
               MOVE    PARA-JOBID      TO  JOB-JOBID
               MOVE    PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCXRMST2MIGV51"
                                       TO  JOB-PGID
               MOVE    PARA-COUNT      TO  WRK-ZZZZZ9
               STRING  "マスタ更新中 " DELIMITED   BY  SIZE
                       "【"            DELIMITED   BY  SIZE
                       WRK-MSTNAME     DELIMITED   SPACE
                       WRK-ZZZZZ9      DELIMITED   SIZE
                       "件"            DELIMITED   SIZE
                       "】"            DELIMITED   BY  SIZE
                       PARA-MSTCNTMSG  DELIMITED   BY  SPACE
               INTO    JOB-SHELLMSG
               END-STRING
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    SPACE               TO  WRK-UPFILE
           STRING  PARA-UPFILE         DELIMITED  BY  "*"
                                       INTO    WRK-UPFILE
           END-STRING
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1     BY  1
                   UNTIL       IDX     >   100
               IF      WRK-UPFILE(IDX:1)    =   "/"
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
      *
           ADD     1                   TO  IDY
           MOVE    SPACE               TO  WRK-UPFILE2
           MOVE    WRK-UPFILE(IDY:)    TO  WRK-UPFILE2
      *
           MOVE    SPACE               TO  MSTPRFFILE
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/orca-mstlog/"     DELIMITED  BY  SIZE
                   WRK-UPFILE2         DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
               INTO    MSTPRFFILE
           END-STRING
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    更新プログラム存在チェック
           PERFORM 2001-PGEXIST-CHECK
      *
           IF    ( FLG-END             =   ZERO )
               PERFORM 2001-EXEC-UPPG-SEC
           END-IF
      *
           IF    ( FLG-END             =   ZERO )
               PERFORM 2001-UPDATE-MSTKANRI-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新プログラム実行処理
      *****************************************************************
       2001-EXEC-UPPG-SEC              SECTION.
      *
           INITIALIZE                      LNK-AREA
           MOVE    PARA-DBFILE         TO  LNK-DBFILE
           MOVE    WRK-UPFILE2         TO  LNK-UPFILE
           MOVE    PARA-DBRVERVION     TO  LNK-DBRVERSION
           MOVE    PARA-HOSPNUM        TO  LNK-HOSPNUM
           MOVE    SMCNDATE-YMD        TO  LNK-MNTYMD
           MOVE    SMCNDATE-HMS        TO  LNK-MNTHMS
           MOVE    PARA-COUNT          TO  LNK-COUNT
      *
      *    更新プログラム実行
           CALL    WRK-UPPG            USING
                                       LNK-AREA
      *
           PERFORM 20011-EDIT-PRF-SEC
      *
           IF    ( LNK-RC          NOT =   ZERO )
               MOVE    SPACE           TO  WRK-MSTERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-EXEC-UPPG-EXT.
           EXIT.
      *****************************************************************
      *    PRFファイル編集処理
      *****************************************************************
       20011-EDIT-PRF-SEC              SECTION.
      *
           OPEN    INPUT               MSTPRF-FILE
           OPEN    EXTEND              PRF-FILE
           PERFORM 900-MSTPRF-READ-SEC
           PERFORM 900-MSTPRF-READ-SEC
      *
           MOVE    ZERO            TO  CNT-MSTPRF
      *
           PERFORM UNTIL ( FLG-MSTPRF  =   1 )
      *
               COMPUTE CNT-MSTPRF  =   CNT-MSTPRF  +   1
               IF    ( CNT-MSTPRF  =   1 )
                   PERFORM 200111-EDIT-PRF-HEAD-SEC
               END-IF
      *
               MOVE    MSTPRF-REC  TO  PRF-REC
               WRITE   PRF-REC
      *
               PERFORM 900-MSTPRF-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   MSTPRF-FILE
                   PRF-FILE
      *
           .
       20011-EDIT-PRF-EXT.
           EXIT.
      *****************************************************************
      *    PRFファイルHEAD編集処理
      *****************************************************************
       200111-EDIT-PRF-HEAD-SEC        SECTION.
      *
           INITIALIZE          PRF-HEAD-AREA
           STRING  "【"            DELIMITED   BY  SIZE 
                   WRK-MSTNAME     DELIMITED   SPACE
                   "】"            DELIMITED   BY  SIZE
           INTO    PRF-HNAME
           END-STRING
      *
           MOVE    PARA-DBRVERVION     TO  PRF-HDBRVERVION
           MOVE    SMCNDATE-YMD (1:4)  TO  PRF-HDATE(1:4)
           MOVE    "."                 TO  PRF-HDATE(5:1)
           MOVE    SMCNDATE-YMD (5:2)  TO  PRF-HDATE(6:2)
           MOVE    "."                 TO  PRF-HDATE(8:1)
           MOVE    SMCNDATE-YMD (7:2)  TO  PRF-HDATE(9:2)
      *
           MOVE    PRF-HEAD-AREA       TO  PRF-REC
           WRITE   PRF-REC
      *
           .
       200111-EDIT-PRF-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    マスタ更新処理
      *****************************************************************
       2001-UPDATE-MSTKANRI-SEC        SECTION.
      *
           PERFORM 900-MSTKANRI-KEY-SEL-SEC
      *
      *    マスタ管理マスタ更新
           IF      FLG-MSTKANRI        =   ZERO
               MOVE    PARA-DBRVERVION TO  MSTKANRI-DBRVERSION1
               MOVE    SMCNDATE-YMD    TO  MSTKANRI-UPYMD
               MOVE    SMCNDATE-HMS    TO  MSTKANRI-UPHMS
               MOVE    MSTKANRI-REC    TO  MCPDATA-REC
               MOVE    "tbl_mstkanri_org"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         PERFORM 910-DBUPDATE-SEC
               IF    ( MCP-RC          NOT =   ZERO )
                   DISPLAY "*(ORCXRMST2MIGV51)* DBUPDATE ERR:" MCP-RC
                   MOVE    "マスター管理の更新に失敗しました"
                                       TO  WRK-MSTERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           ELSE
               PERFORM 920-DBABORT-SEC
               MOVE    "更新対象のマスタデータが存在しません"
                                       TO  WRK-MSTERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-UPDATE-MSTKANRI-EXT.
           EXIT.
      *****************************************************************
      *    更新プログラム存在チェック
      *****************************************************************
       2001-PGEXIST-CHECK              SECTION.
      *
           MOVE    SPACE           TO  WRK-UPPG
           MOVE    PARA-UPPG       TO  WRK-UPPG-PARA
           MOVE    "MIGV51"        TO  WRK-UPPG-MIG
      *
           MOVE    SPACE           TO  PGEXIST01
           STRING  WRK-UPPG            DELIMITED  BY  SPACE
                   ".so"               DELIMITED  BY  SIZE
                                       INTO   PGEXIST01
           END-STRING
      *
           INITIALIZE                  SACCESS-AREA
           MOVE    "X"             TO  SACCESS-MODE
           MOVE    PGEXIST01       TO  SACCESS-IN-FILENAME
           CALL    "ORCSACCESS"    USING
                                   SACCESS-AREA
      *
           IF    ( SACCESS-RC      NOT =  ZERO )
               DISPLAY "*(ORCXRMST2MIGV51)* MODULE NOT FOUND:" WRK-UPPG
               MOVE    SPACE           TO  WRK-MSTERR
               STRING  "更新プログラムが存在しません:"
                                       DELIMITED   BY  SIZE
                       WRK-UPPG        DELIMITED   BY  SIZE
               INTO    WRK-MSTERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       2001-PGEXIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＲＦ印刷処理
      *****************************************************************
       250-PRINT-PRF-SEC               SECTION.
      *
           OPEN    INPUT   PRF-FILE
      *
           PERFORM 900-PRF-READ-SEC
      *
           MOVE    ZERO                TO  CNT-PAGE
      *
           PERFORM UNTIL ( FLG-PRF     NOT =   ZERO )
      *
               INITIALIZE              HCMSL55
      *
               COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
               MOVE    CNT-PAGE        TO  WRK-ZZZZZ9
               STRING  "標準提供マスタ確認リスト  "
                                       DELIMITED   BY  SIZE
                       "P-"            DELIMITED   BY  SIZE
                       WRK-ZZZZZ9      DELIMITED   BY  SIZE
               INTO    HCMSL55-MOJI (1)
               END-STRING
               MOVE    ALL "-"         TO  HCMSL55-MOJI (2)
               MOVE    2               TO  CNT-LINE
      *
               PERFORM UNTIL ( FLG-PRF     NOT =   ZERO )
                        OR   ( CNT-LINE       >=  53 )
      *
                   COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
                   MOVE    PRF-REC         TO  HCMSL55-MOJI (CNT-LINE)
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"             TO  KANACHK-SYORI
                   MOVE    HCMSL55-MOJI (CNT-LINE)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   MOVE    KANACHK-OUT-INPUT
                                           TO  HCMSL55-MOJI (CNT-LINE)
      *
                   PERFORM 900-PRF-READ-SEC
      *
               END-PERFORM
      *
               PERFORM 260-PRINT-SEC
      *
           END-PERFORM
      *
           CLOSE   PRF-FILE
      *
           .
       250-PRINT-PRF-EXT.
           EXIT.
      *****************************************************************
      *    印刷処理
      *****************************************************************
       260-PRINT-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SMCNDATE-YMD        TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF    ( ORCSPRTNM-RC    NOT =   ZERO )
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    1               TO  SPRT-RENNUM
           MOVE    PARA-SHELLID    TO  SPRT-TBL-KEY
           MOVE    SMCNDATE-YMD    TO  SPRT-TBL-GROUP (1:8)
           MOVE    SMCNDATE-HMS    TO  SPRT-TBL-GROUP (9:)
           MOVE    SMCNDATE-YMD (1:6)
                                   TO  SPRT-SRYYM
           MOVE    SMCNDATE-YMD    TO  SPRT-SKYYMD
           MOVE    "master_upgrade.sh"
                                   TO  SPRT-SHELLID
           MOVE    1               TO  SPRT-SHORI-RENNUM
           MOVE    ZERO            TO  SPRT-PRIORITY
           MOVE   "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "標準提供マスタ更新確認リスト"
                                   TO  SPRT-TITLE
           MOVE    HCMSL55         TO  SPRT-PRTDATA
           MOVE    ZERO            TO  SPRT-TERMID
           MOVE    SYS-1031-PRTNM (6)
                                   TO  SPRT-PRTNM
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
grpsys                                 SPA-AREA
           IF    ( SPRT-RETURN     =   ZERO )
               CONTINUE
           ELSE
               DISPLAY "印刷ＤＢに更新できませんでした"
               MOVE    1           TO  FLG-END
           END-IF
      *
      *
           .
       260-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブキャンセル処理
      *****************************************************************
       500-CAN-HENSYU-SEC              SECTION.
      *
           MOVE    "処理が中止されました"
                                       TO  WRK-MSTERR
      *
           OPEN    INPUT   MSTERR-FILE
           IF    ( STS-MSTERR          =   ZERO )
               CLOSE   MSTERR-FILE
           ELSE
               OPEN    OUTPUT              MSTERR-FILE
      *
               MOVE    WRK-MSTERR      TO  MSTERR-REC
               WRITE   MSTERR-REC
               CLOSE   MSTERR-FILE
      *
               PERFORM 600-EDIT-PRF-SEC
      *
      *        ジョブ管理開始処理
               MOVE    "CAN"           TO  SJOBKANRI-MODE
               INITIALIZE                   JOBKANRI-REC
               MOVE    PARA-JOBID      TO  JOB-JOBID
               MOVE    PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "8888"          TO  JOB-ERRCD
               MOVE    WRK-MSTERR      TO  JOB-YOBI
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    2               TO  JOB-STOPFLG
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-CAN-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT   MSTERR-FILE
           IF    ( STS-MSTERR          =   ZERO )
               CLOSE   MSTERR-FILE
           ELSE
               OPEN    OUTPUT              MSTERR-FILE
      *
               INITIALIZE          MSTERR-REC
               STRING  WRK-MSTERR      DELIMITED   BY  SPACE
                       "【"            DELIMITED   BY  SIZE
                       WRK-MSTNAME     DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
               INTO    MSTERR-REC
               END-STRING
      *
               MOVE    MSTERR-REC       TO  WRK-MSTERR
      *
               WRITE   MSTERR-REC
               CLOSE   MSTERR-FILE
      *
               PERFORM 600-EDIT-PRF-SEC
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"            TO  SJOBKANRI-MODE
               INITIALIZE                   JOBKANRI-REC
               MOVE    PARA-JOBID       TO  JOB-JOBID
               STRING  "99"             DELIMITED  BY  SIZE
                       PARA-MASTER-KBN  DELIMITED  BY  SIZE
               INTO    JOB-ERRCD
               END-STRING
               MOVE    PARA-SHELLID    TO  JOB-SHELLID
               MOVE    WRK-MSTERR      TO  JOB-YOBI
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    PRFファイル編集処理
      *****************************************************************
       600-EDIT-PRF-SEC                SECTION.
      *
           PERFORM 900-DBCOMMIT-SEC
           PERFORM 900-DBSTART-SEC
      *
           PERFORM 250-PRINT-PRF-SEC
      *
           IF    ( CNT-PAGE            >   ZERO )
      *
               MOVE    "STM"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-MSTERR      TO  JOB-YOBI
               MOVE    PARA-JOBID      TO  JOB-JOBID
               MOVE    PARA-SHELLID    TO  JOB-SHELLID
               MOVE    "ORCXRMST2MIGV51"
                                       TO  JOB-PGID
               MOVE    CNT-PAGE        TO  JOB-UPDCNT
               MOVE    PARA-HOSPNUM    TO JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           .
       600-EDIT-PRF-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( FLG-END         =    ZERO )
               IF    ( PARA-MSTCOUNT   =    PARA-MSTSEQ )
                   PERFORM 600-EDIT-PRF-SEC
               END-IF
           END-IF
      *
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    PARA-HOSPNUM    TO  JOB-HOSPNUM
           MOVE    PARA-JOBID      TO  JOB-JOBID
           MOVE    PARA-SHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
      *    DB DISCONNECT
           PERFORM 900-DBDISCONNECT-SEC
      *
      *    出力ファイル　終了処理
      **** CLOSE                       IN-FILE
      *
      *    DISPLAY "*(ORCXRMST2MIGV51)* INPUT  /I   CNT[" CNT-IN  "]"
      *    ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ名称編集処理
      *****************************************************************
       800-MSTNAME-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-MSTNAME
      *
           PERFORM 900-MSTKANRI-KEY-SEL-SEC
           IF    ( FLG-MSTKANRI    =   ZERO )
               MOVE    MSTKANRI-NAME           TO  WRK-MSTNAME
           ELSE
               MOVE    PARA-MASTER-KBN         TO  WRK-MSTNAME
           END-IF
      *
           .
       800-MSTNAME-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
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
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    入力ファイル読込み処理
      *****************************************************************
      *900-IN-READ-SEC          SECTION.
      *
      *   READ    IN-FILE
      *       AT  END
      *           MOVE    1               TO  FLG-IN
      *       NOT AT  END
      *           MOVE    ZERO            TO  FLG-IN
      *           ADD     1               TO  CNT-IN
      *   END-READ
      *
      *   .
      *900-IN-READ-EXT.
      *   EXIT.
      *****************************************************************
      *    マスタ管理検索処理
      *****************************************************************
       900-MSTKANRI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-MSTKANRI
      *
      *    マスタ管理マスタ検索
           INITIALIZE                  MSTKANRI-REC
           MOVE    "ORCADB"            TO  MSTKANRI-KANRICD
           MOVE    PARA-MASTER-KBN     TO  MSTKANRI-KANRICD(7:2)
           MOVE    SPA-HOSPNUM         TO  MSTKANRI-HOSPNUM
           MOVE    MSTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  MSTKANRI-REC
           ELSE
               INITIALIZE                  MSTKANRI-REC
               MOVE    1               TO  FLG-MSTKANRI
           END-IF
      *
           MOVE    "tbl_mstkanri_org"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-MSTKANRI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    マスタデータ別ＰＲＦ読込処理
      *****************************************************************
       900-MSTPRF-READ-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-MSTPRF
      *
           READ    MSTPRF-FILE
           AT  END
               MOVE    1           TO  FLG-MSTPRF
           END-READ
      *
           .
       900-MSTPRF-READ-EXT.
           EXIT.
      *****************************************************************
      *    ＰＲＦ読込処理
      *****************************************************************
       900-PRF-READ-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-PRF
      *
           READ    PRF-FILE
           AT  END
               MOVE    1           TO  FLG-PRF
           END-READ
      *
           .
       900-PRF-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
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
       910-DBFETCH-SEC             SECTION.
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
      *    テーブル更新処理
      *****************************************************************
       910-DBUPDATE-SEC            SECTION.
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
      *    テーブル更新処理
      *****************************************************************
       910-INSERT-SEC              SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢアボート処理
      *****************************************************************
       920-DBABORT-SEC                 SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    1               TO  JOB-HOSPNUM
           MOVE    "tbl_jobkanri"  TO  MCP-TABLE
           MOVE    "abort"         TO  MCP-PATHNAME
           MOVE    JOBKANRI-REC    TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
      *
           .
       920-DBABORT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
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
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                 SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBSTART-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBCOMMIT-EXT.
           EXIT.
