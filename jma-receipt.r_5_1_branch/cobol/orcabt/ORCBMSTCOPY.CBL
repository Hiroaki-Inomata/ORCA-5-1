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
       PROGRAM-ID.             ORCBMSTCOPY.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 月次
      *  コンポーネント名  : 本院分院データ複写
      *  管理者            : 
      *  作成日付    作業者        記述
      *  09/01/30    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.01.00    NACL-小豆沢  19/03/28  点数マスタ分離対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *
           COPY    "CPRECEERR.INC".
      *
      *    一覧
           COPY    "HCMSL55.INC".
      *
      *
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSBASE                 PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-TENSU                   PIC 9(01).
           03  FLG-SRYCDCHG                PIC 9(01).
           03  FLG-INPUTCD                 PIC 9(01).
           03  FLG-INPUTSET                PIC 9(01).
           03  FLG-INIT-CHK                PIC 9(01).
           03  FLG-TENSU-ARI               PIC 9(01).
           03  FLG-INPUTCD-ARI             PIC 9(01).
           03  FLG-INPUTSET-ARI            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-SYORIKBN                   PIC X(01).
           03  WRK-PARA-COPYHOSPNUM                PIC 9(02).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRNO                           PIC 9(2).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-PTID                            PIC 9(10).
           03  WRK-SRYYM                           PIC X(06).
           03  WRK-MOTOHOSPNAME                    PIC X(10).
           03  WRK-SAKIHOSPNAME                    PIC X(10).
           03  WRK-MOTOGRP                         PIC 9(02).
           03  WRK-SAKIGRP                         PIC 9(02).
           03  WRK-NAIYO                           PIC X(100).
           03  WRK-INPUTCD                         PIC X(20).
      *
           03  WRK-TAIHI-AREA                      PIC X(2000).
      *
           03  WRK-ERRCD                           PIC X(04).
      *
       01  LST-AREA.
           03  LST-HEAD1.
               05                      PIC X(40)   VALUE   " ".
               05                      PIC X(40)   VALUE   
                             "本院分院マスター複写結果リスト（複写元：".
               05  LST-HEAD-MOTOHOSP   PIC X(10).
               05                      PIC X(10)   VALUE   
                             "  複写先：".
               05  LST-HEAD-SAKIHOSP   PIC X(10).
               05                      PIC X(02)   VALUE   "）".
               05                      PIC X(08)   VALUE   SPACE.
               05  LST-HEAD-PAGE       PIC Z,ZZ9.
               05                      PIC X(05)   VALUE   "頁".
           03  LST-HEAD.
               05                      PIC X(04)   VALUE   "番号".
               05                      PIC X(02)   VALUE   SPACE.
               05                      PIC X(20)   VALUE   "複写コード".
               05                      PIC X(01)   VALUE   SPACE.
               05                      PIC X(30)   VALUE   "名称".
               05                      PIC X(06)   VALUE   SPACE.
               05                      PIC X(52)   VALUE   "処理結果".
      *
           03  LST-MEISAI-G.
               05  LST-MEISAI-OCC      OCCURS      50.
                   07  LST-NO          PIC Z(04).
                   07  LST-FIL1        PIC X(02)   VALUE SPACE.
                   07  LST-CD          PIC X(20).
                   07  LST-FIL2        PIC X(01)   VALUE SPACE.
                   07  LST-NAME1       PIC X(30).
                   07  LST-FIL3        PIC X(06)   VALUE SPACE.
                   07  LST-NAIYO       PIC X(52).
      *
       01  CONST-AREA.
           03  CONST-LST-HEAD-LINE     PIC 9(05)   VALUE   2.
           03  CONST-LST-MEISAI-MAX    PIC 9(05)   VALUE   50.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム基本マスタ
       01  SYSBASE-REC.
           COPY    "CPSYSBASE.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *    点数ユーザーマスタ
           COPY    "CPTENSU.INC".
      *
      *    入力コード
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セット
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    印刷ＤＢ制御サブ
           COPY    "CPORCSPRT.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    FLG-INIT-CHK       =     ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  PARA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
                                       LNK-PRTKANRI-TBL-KEY
                                       LNK-PRTKANRI-TBL-GROUP
                                       LNK-PRTKANRI-SHORI-RENNUM
                                       LNK-PRTKANRI-SRYYM
                                       LNK-PRTKANRI-SKYYMD
                                       LNK-PRTKANRI-SHELLID
                                       LNK-PRTKANRI-PRIORITY
                                       LNK-PRTKANRI-TERMID
                                       LNK-PRTKANRI-OPID
                                       LNK-PRTKANRI-PRTNM
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-SYORIKBN
                                       WRK-PARA-COPYHOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBMSTCOPY"       TO  JOB-PGID
              EVALUATE    WRK-PARA-SYORIKBN
                  WHEN    "1"
                      MOVE    "マスタ複写（入力コード）"
                                       TO  JOB-SHELLMSG
                  WHEN    "2"
                      MOVE    "マスタ複写（点数マスタ）"
                                       TO  JOB-SHELLMSG
                  WHEN    "3"
                      MOVE    "マスタ複写（入力セット）"
                                       TO  JOB-SHELLMSG
                  WHEN    OTHER
                      MOVE    "マスタ複写"
                                       TO  JOB-SHELLMSG
              END-EVALUATE
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           IF    ( WRK-PARA-SYORIKBN   =   "1" OR "2" OR "3" )
               CONTINUE
           ELSE
               MOVE    "処理区分が誤りです"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-INIT-CHK
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    自院の医療機関名称編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1001-STYUKYMD
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC         =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-TANHOSPNAME
                                       TO  WRK-SAKIHOSPNAME
                   ELSE
                         MOVE  "医療機関情報が取得できません"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1               TO  FLG-INIT-CHK
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-INIT-CHK
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    FLG-INIT-CHK   NOT =    ZERO
               GO    TO   100-INIT-EXT
           END-IF
      *
           IF    SPA-HOSPNUM    =   WRK-PARA-COPYHOSPNUM
               MOVE    "複写元は他院を指定してください"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-INIT-CHK
           END-IF
      *
           IF    FLG-INIT-CHK   NOT =    ZERO
               GO    TO   100-INIT-EXT
           END-IF
      *
      *    複写元の医療機関名称編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1001-STYUKYMD
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-COPYHOSPNUM TO SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC         =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-TANHOSPNAME
                                       TO  WRK-MOTOHOSPNAME
                   ELSE
                   MOVE  "複写元の医療機関情報が取得できません"
                                       TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1           TO  FLG-INIT-CHK
                   END-IF
           ELSE
               MOVE    "複写元の医療機関情報が取得できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-INIT-CHK
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    FLG-INIT-CHK   NOT =    ZERO
               GO    TO   100-INIT-EXT
           END-IF
      *
      *    本院分院チェック
           INITIALIZE                      SYSBASE-REC
           MOVE    SPA-HOSPNUM         TO  SYSBASE-HOSPNUM
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SYSBASE-SELECT-SEC
           IF      FLG-SYSBASE         =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM     910-SYSBASE-READ-SEC
                   IF    FLG-SYSBASE   =   ZERO
                       IF   SYSBASE-HONBUNKBN    =   "0"
                         MOVE  "本院分院でないため処理できません"
                                       TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         MOVE    1     TO  FLG-INIT-CHK
                       ELSE
                         MOVE    SYSBASE-HONBUNGRP
                                       TO  WRK-SAKIGRP
                       END-IF
                   ELSE
                       MOVE  "システム基本情報が取得できません"
                                           TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1         TO  FLG-INIT-CHK
                   END-IF
           ELSE
               MOVE    "システム基本情報が取得できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-INIT-CHK
           END-IF
      *
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    FLG-INIT-CHK   NOT =    ZERO
               GO    TO   100-INIT-EXT
           END-IF
      *
      *    複写元医療機関チェック
           INITIALIZE                      SYSBASE-REC
           MOVE    WRK-PARA-COPYHOSPNUM TO SYSBASE-HOSPNUM
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SYSBASE-SELECT-SEC
           IF      FLG-SYSBASE         =   ZERO
               MOVE    "tbl_sysbase"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM     910-SYSBASE-READ-SEC
                   IF    FLG-SYSBASE   =   ZERO
                       IF   SYSBASE-HONBUNKBN    =   "0"
                     MOVE  "複写元が本院分院でないため処理できません"
                                       TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         MOVE    1     TO  FLG-INIT-CHK
                       ELSE
                         MOVE    SYSBASE-HONBUNGRP
                                       TO  WRK-MOTOGRP
                           IF   WRK-MOTOGRP  NOT = WRK-SAKIGRP
                           MOVE  "複写元と本院分院の関係にありません"
                                           TO  WRK-RECEERR
                               PERFORM 500-ERR-HENSYU-SEC
                               MOVE    1   TO  FLG-INIT-CHK
                           END-IF
                       END-IF
                   ELSE
                       MOVE
                      "複写元のシステム基本情報が取得できません。"
                                         TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    1         TO  FLG-INIT-CHK
                   END-IF
           ELSE
               MOVE "複写元のシステム基本情報が取得できません"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1               TO  FLG-INIT-CHK
           END-IF
      *
           MOVE    "tbl_sysbase"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE   WRK-PARA-SYORIKBN
               WHEN   "1"
                   PERFORM   2001-INPUTCD-SEC
               WHEN   "2"
                   PERFORM   2001-TENSU-SEC
               WHEN   "3"
                   PERFORM   2001-INPUTSET-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数ユーザーマスター複写処理
      *****************************************************************
       2001-TENSU-SEC                SECTION.
      *
      *    帳票初期処理
           PERFORM 2001-HCMSL55-INIT-SEC
      *
      *    複写対象検索
           MOVE    SPACE                TO  TNS-TENSU-REC
           INITIALIZE                       TNS-TENSU-REC
           MOVE    WRK-PARA-COPYHOSPNUM TO  TNS-HOSPNUM
           MOVE    SMCNDATE-YMD         TO  TNS-YUKOSTYMD
                                            TNS-YUKOEDYMD
           MOVE    "tbl_tensu_user"     TO  MCP-TABLE
           MOVE    "key27"              TO  MCP-PATHNAME
           PERFORM   910-TENSU-SELECT-SEC
           IF      FLG-TENSU            =   ZERO
               MOVE    "tbl_tensu_user" TO  MCP-TABLE
               MOVE    "key27"          TO  MCP-PATHNAME
               PERFORM  910-TENSU-READ-SEC
               PERFORM  UNTIL  FLG-TENSU    =    1
      *                点数ユーザーマスター作成処理
                   PERFORM     2001-TENSU-INS-SEC
      *
                   MOVE    "tbl_tensu_user"
                                        TO  MCP-TABLE
                   MOVE    "key27"      TO  MCP-PATHNAME
                   PERFORM  910-TENSU-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_tensu_user"     TO  MCP-TABLE
           MOVE    "key27"              TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF    ( CNT-LINE            >   ZERO )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2001-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数ユーザーマスター作成処理
      *****************************************************************
       2001-TENSU-INS-SEC          SECTION.
      *
           MOVE   TNS-TENSU-REC TO  WRK-TAIHI-AREA
      *
      *       自院の点数ユーザーマスタ存在チェック
           PERFORM    2001-TENSU-CHK-SEC
      *
           IF         FLG-TENSU-ARI    =    ZERO
               MOVE    WRK-TAIHI-AREA  TO   TNS-TENSU-REC
               MOVE    SPA-HOSPNUM     TO   TNS-HOSPNUM
               MOVE    TNS-TENSU-REC   TO   MCPDATA-REC
               MOVE    "tbl_tensu_user"
                                       TO   MCP-TABLE
               MOVE    "key"           TO   MCP-PATHNAME
               MOVE    "DBINSERT"      TO   MCP-FUNC
               PERFORM     900-ORCDBMAIN-SEC
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-TENSU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数ユーザーマスタ自院存在チェック
      *****************************************************************
       2001-TENSU-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU-ARI
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-TAIHI-AREA      TO  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-TENSU-SELECT-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    "tbl_tensu_user"
                                       TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-TENSU-READ-SEC
               IF  FLG-TENSU           =   ZERO
                   MOVE    1           TO  FLG-TENSU-ARI
               END-IF
           END-IF
      *
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-TENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード複写処理
      *****************************************************************
       2001-INPUTCD-SEC                SECTION.
      *
      *    帳票初期処理
           PERFORM 2001-HCMSL55-INIT-SEC
      *
      *    複写対象検索
           MOVE    SPACE                TO  INPUTCD-REC
           INITIALIZE                       INPUTCD-REC
           MOVE    WRK-PARA-COPYHOSPNUM TO  ICD-HOSPNUM
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "all"                TO  MCP-PATHNAME
           PERFORM   910-INPUTCD-SELECT-SEC
           IF      FLG-INPUTCD            =   ZERO
               MOVE    "tbl_inputcd"    TO  MCP-TABLE
               MOVE    "all"            TO  MCP-PATHNAME
               PERFORM  910-INPUTCD-READ-SEC
               PERFORM  UNTIL  FLG-INPUTCD  =    1
                  IF   (ICD-CREYMD      =  SPACE)     OR
                       (ICD-CREYMD      = "00000000") OR
                       (ICD-INPUTCD(1:1) = "P")       OR
                       (ICD-INPUTCD(1:1) = "S")
                     CONTINUE
                  ELSE
      *                   入力コード作成処理
                     PERFORM     2001-INPUTCD-INS-SEC
                  END-IF
      *
                  MOVE    "tbl_inputcd"  TO  MCP-TABLE
                  MOVE    "all"          TO  MCP-PATHNAME
                  PERFORM  910-INPUTCD-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "all"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF    ( CNT-LINE            >   ZERO )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2001-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード作成処理
      *****************************************************************
       2001-INPUTCD-INS-SEC          SECTION.
      *
           MOVE   INPUTCD-REC          TO  WRK-TAIHI-AREA
      *
      *       自院の入力コード存在チェック
           PERFORM    2001-INPUTCD-CHK-SEC
      *
           IF         FLG-INPUTCD-ARI  =    ZERO
               MOVE    WRK-TAIHI-AREA  TO   INPUTCD-REC
               MOVE    SPA-HOSPNUM     TO   ICD-HOSPNUM
               MOVE    INPUTCD-REC     TO   MCPDATA-REC
               MOVE    "tbl_inputcd"   TO   MCP-TABLE
               MOVE    "key"           TO   MCP-PATHNAME
               MOVE    "DBINSERT"      TO   MCP-FUNC
               PERFORM     900-ORCDBMAIN-SEC
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-INPUTCD-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード自院存在チェック
      *****************************************************************
       2001-INPUTCD-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTCD-ARI
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    WRK-TAIHI-AREA      TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-INPUTCD-SELECT-SEC
           IF      FLG-INPUTCD         =   ZERO
               MOVE    "tbl_inputcd"   TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-INPUTCD-READ-SEC
               IF  FLG-INPUTCD         =   ZERO
                   MOVE    1           TO  FLG-INPUTCD-ARI
               END-IF
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット複写処理
      *    （処理対象セットの検索は入力コードテーブルより行う）
      *****************************************************************
       2001-INPUTSET-SEC                SECTION.
      *
      *    帳票初期処理
           PERFORM 2001-HCMSL55-INIT-SEC
      *
      *    複写対象検索
           MOVE    SPACE                TO  INPUTCD-REC
           INITIALIZE                       INPUTCD-REC
           MOVE    WRK-PARA-COPYHOSPNUM TO  ICD-HOSPNUM
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "all"                TO  MCP-PATHNAME
           PERFORM   910-INPUTCD-SELECT-SEC
           IF      FLG-INPUTCD            =   ZERO
               MOVE    "tbl_inputcd"    TO  MCP-TABLE
               MOVE    "all"            TO  MCP-PATHNAME
               PERFORM  910-INPUTCD-READ-SEC
               PERFORM  UNTIL  FLG-INPUTCD  =    1
                  IF   (ICD-INPUTCD(1:1) = "P")       OR
                       (ICD-INPUTCD(1:1) = "S")
      *                   入力セット作成処理
                     PERFORM     2001-INPUTSET-INS-SEC
                  END-IF
      *
                  MOVE    "tbl_inputcd"  TO  MCP-TABLE
                  MOVE    "all"          TO  MCP-PATHNAME
                  PERFORM  910-INPUTCD-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_inputcd"        TO  MCP-TABLE
           MOVE    "all"                TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           IF    ( CNT-LINE            >   ZERO )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2001-INPUTSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット作成処理
      *****************************************************************
       2001-INPUTSET-INS-SEC          SECTION.
      *
           MOVE   INPUTCD-REC          TO  WRK-TAIHI-AREA
      *
      *       自院の入力セット存在チェック
           PERFORM    2001-INPUTSET-CHK-SEC
      *
           IF         FLG-INPUTSET-ARI =   ZERO
               MOVE    WRK-TAIHI-AREA  TO  INPUTCD-REC
               MOVE    ICD-INPUTCD     TO  WRK-INPUTCD
               MOVE    SPA-HOSPNUM     TO  ICD-HOSPNUM
               MOVE    INPUTCD-REC     TO  MCPDATA-REC
               MOVE    "tbl_inputcd"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               MOVE    "DBINSERT"      TO  MCP-FUNC
               PERFORM     900-ORCDBMAIN-SEC
      *            入力セットテーブルの複写
               MOVE    SPACE               TO  INPUTSET-REC
               INITIALIZE                      INPUTSET-REC
               MOVE    WRK-PARA-COPYHOSPNUM TO ISET-HOSPNUM
               MOVE    WRK-INPUTCD         TO  ISET-SETCD
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM   910-INPUTSET-SELECT-SEC
               IF      FLG-INPUTSET        =   ZERO
                   MOVE    "tbl_inputset"     TO  MCP-TABLE
                   MOVE    "key6"             TO  MCP-PATHNAME
                   PERFORM  910-INPUTSET-READ-SEC
                   PERFORM  UNTIL  FLG-INPUTSET  =    1
      *                     入力セット作成処理
                       MOVE   INPUTSET-REC    TO  WRK-TAIHI-AREA
                       MOVE    SPACE          TO  INPUTSET-REC
                       INITIALIZE                 INPUTSET-REC
                       MOVE   WRK-TAIHI-AREA  TO  INPUTSET-REC
                       MOVE   SPA-HOSPNUM     TO  ISET-HOSPNUM
                       MOVE   INPUTSET-REC    TO  MCPDATA-REC
                       MOVE   "tbl_inputset"  TO  MCP-TABLE
                       MOVE   "key"           TO  MCP-PATHNAME
                       MOVE   "DBINSERT"      TO  MCP-FUNC
                       PERFORM     900-ORCDBMAIN-SEC
      *
                       MOVE   "tbl_inputset"  TO  MCP-TABLE
                       MOVE   "key6"          TO  MCP-PATHNAME
                       PERFORM  910-INPUTSET-READ-SEC
                   END-PERFORM
               END-IF
      *
               MOVE    "tbl_inputset"       TO  MCP-TABLE
               MOVE    "key6"                TO  MCP-PATHNAME
               PERFORM   900-CLOSE-SEC
      *
           END-IF
      *
           PERFORM     2002-HCMSL55-HEN-SEC
      *
           .
       2001-INPUTSET-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット自院存在チェック
      *****************************************************************
       2001-INPUTSET-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-INPUTSET-ARI
      *
           MOVE    SPACE               TO  INPUTCD-REC
           INITIALIZE                      INPUTCD-REC
           MOVE    WRK-TAIHI-AREA      TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-INPUTCD-SELECT-SEC
           IF      FLG-INPUTCD         =   ZERO
               MOVE    "tbl_inputcd"   TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-INPUTCD-READ-SEC
               IF  FLG-INPUTCD         =   ZERO
                   MOVE    1           TO  FLG-INPUTSET-ARI
               END-IF
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2001-INPUTSET-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票初期処理
      *****************************************************************
       2001-HCMSL55-INIT-SEC           SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
                                           CNT-PAGE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2001-HCMSL55-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-HCMSL55-HEN-SEC            SECTION.
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           ADD     1                   TO  CNT-JOB-UPDCNT
           MOVE    CNT-JOB-UPDCNT      TO  LST-NO (CNT-LINE)
           EVALUATE   WRK-PARA-SYORIKBN
               WHEN   "1"
                   MOVE    ICD-INPUTCD TO  LST-CD (CNT-LINE)
                   MOVE    SPACE       TO  TNS-TENSU-REC
                   INITIALIZE          TNS-TENSU-REC
                   MOVE    WRK-PARA-HOSPNUM
                                       TO  TNS-HOSPNUM
                   MOVE    ICD-SRYCD   TO  TNS-SRYCD
                   MOVE    SMCNDATE-YMD
                                       TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
                   MOVE    "tbl_tensu" TO  MCP-TABLE
                   MOVE    "key"       TO  MCP-PATHNAME
                   PERFORM   910-TENSU-SELECT-SEC
                   IF      FLG-TENSU   =   ZERO
                       MOVE    "tbl_tensu"    TO  MCP-TABLE
                       MOVE    "key"          TO  MCP-PATHNAME
                       PERFORM  910-TENSU-READ-SEC
                       IF    FLG-TENSU    =    ZERO
                          INSPECT TNS-NAME REPLACING
                                           ALL "  "  BY  "　"
                          MOVE    TNS-NAME  TO  LST-NAME1 (CNT-LINE)
                       ELSE
                          MOVE  "現在有効な点数マスタ無し　　　"
                                       TO  LST-NAME1 (CNT-LINE)
                          IF ICD-SRYCD(1:1)  = "P" 
                             MOVE "入力セット　　　　　　　　　　"
                                       TO  LST-NAME1 (CNT-LINE)
                          END-IF
                          IF ICD-SRYCD(1:1)  = "S" 
                             MOVE "約束セット　　　　　　　　　　"
                                       TO  LST-NAME1 (CNT-LINE)
                          END-IF
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_tensu"
                                          TO  MCP-TABLE
                   MOVE    "key"          TO  MCP-PATHNAME
                   PERFORM   900-CLOSE-SEC
               WHEN   "2"
                   MOVE    TNS-SRYCD   TO  LST-CD (CNT-LINE)
                   INSPECT TNS-NAME REPLACING  ALL "  "  BY  "　"
                   MOVE    TNS-NAME    TO  LST-NAME1 (CNT-LINE)
               WHEN   "3"
                   MOVE    ICD-INPUTCD TO  LST-CD (CNT-LINE)
      *                 全角チェック
                   INITIALIZE             ORCSKANACHKAREA
                   MOVE    "1"            TO  KANACHK-SYORI
                   MOVE    ICD-DSPNAME    TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
                   IF      KANACHK-RC     =   ZERO
                       MOVE   ICD-DSPNAME TO  LST-NAME1 (CNT-LINE)
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           IF      FLG-INPUTCD-ARI     =   1
               MOVE  "2001"            TO  WRK-ERRCD
           END-IF
      *
           IF      FLG-TENSU-ARI       =   1
               MOVE  "2002"            TO  WRK-ERRCD
           END-IF
      *
           IF      FLG-INPUTSET-ARI     =   1
               MOVE  "2003"            TO  WRK-ERRCD
           END-IF
      *
           EVALUATE    WRK-ERRCD
               WHEN    SPACE
                  EVALUATE   WRK-PARA-SYORIKBN
                      WHEN   "1"
                          MOVE  "入力コードを複写しました"
                                       TO  WRK-NAIYO
                      WHEN   "2"
                          MOVE  "点数マスタを複写しました"
                                       TO  WRK-NAIYO
                      WHEN   "3"
                          MOVE  "入力セットを複写しました"
                                       TO  WRK-NAIYO
                  END-EVALUATE
               WHEN    "2001"
                   MOVE  "入力コードが存在しています"
                                       TO  WRK-NAIYO
               WHEN    "2002"
                   MOVE  "点数マスタが存在しています"
                                       TO  WRK-NAIYO
               WHEN    "2003"
                   MOVE  "入力セットが存在しています"
                                       TO  WRK-NAIYO
           END-EVALUATE
           MOVE    WRK-NAIYO           TO  LST-NAIYO (CNT-LINE)
      *
           IF    ( CNT-LINE            >=  CONST-LST-MEISAI-MAX )
               PERFORM 2002-HCMSL55-PRT-SEC
           END-IF
      *
           .
       2002-HCMSL55-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       2002-HCMSL55-PRT-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           MOVE    CNT-PAGE            TO  LST-HEAD-PAGE
           IF      WRK-MOTOHOSPNAME    NOT =   SPACE
              MOVE    WRK-MOTOHOSPNAME TO  LST-HEAD-MOTOHOSP
           ELSE
              MOVE    WRK-PARA-COPYHOSPNUM
                                       TO  LST-HEAD-MOTOHOSP
           END-IF
           IF      WRK-SAKIHOSPNAME    NOT =   SPACE
              MOVE    WRK-SAKIHOSPNAME TO  LST-HEAD-SAKIHOSP
           ELSE
              MOVE    WRK-PARA-HOSPNUM
                                       TO  LST-HEAD-SAKIHOSP
           END-IF
           MOVE    LST-HEAD1           TO  HCMSL55-MOJI (1)
           MOVE    LST-HEAD            TO  HCMSL55-MOJI (2)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   CONST-LST-MEISAI-MAX )
      *
               MOVE    LST-MEISAI-OCC (IDX0)
                                       TO  HCMSL55-MOJI
                                          (IDX0 + CONST-LST-HEAD-LINE)
      *
           END-PERFORM
      *
           PERFORM 290-PRINT-SEC
      *
           MOVE    ZERO                    TO  CNT-LINE
      *
           INITIALIZE                      LST-MEISAI-G
      *
           .
       2002-HCMSL55-PRT-EXT.
           EXIT.
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       290-PRINT-SEC                   SECTION.
      *
           INITIALIZE                  ORCSPRTAREA
           MOVE    "INS"           TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                   TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SPRT-PRIORITY
           MOVE   "HCMSL55.red"    TO  SPRT-PRTID
           MOVE    "本院分院マスター複写"
                                    TO  SPRT-TITLE
           MOVE    HCMSL55          TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID
                                    TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID
                                    TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM
                                    TO  SPRT-PRTNM
           MOVE    "1"              TO  SPRT-SITEKBN
           CALL    "ORCSPRT"            USING
                                        ORCSPRTAREA
                                        SPA-AREA
           IF      SPRT-RETURN          =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       290-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ジョブ管理終了処理
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスター読込
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム基本マスター検索
      *****************************************************************
       910-SYSBASE-SELECT-SEC        SECTION.
      *
           MOVE    SYSBASE-REC         TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               MOVE    1               TO  FLG-SYSBASE
           END-IF
      *
           .
       910-SYSBASE-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム基本マスター読込
      *****************************************************************
       910-SYSBASE-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSBASE-REC
               MOVE    ZERO            TO  FLG-SYSBASE
           ELSE
               INITIALIZE                  SYSBASE-REC
               MOVE    1               TO  FLG-SYSBASE
           END-IF
      *
           .
       910-SYSBASE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数ユーザーマスター検索
      *****************************************************************
       910-TENSU-SELECT-SEC        SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数ユーザーマスター読込
      *****************************************************************
       910-TENSU-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード検索
      *****************************************************************
       910-INPUTCD-SELECT-SEC        SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-INPUTCD
           ELSE
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           .
       910-INPUTCD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード読込
      *****************************************************************
       910-INPUTCD-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTCD-REC
               MOVE    ZERO            TO  FLG-INPUTCD
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1               TO  FLG-INPUTCD
           END-IF
      *
           .
       910-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット検索
      *****************************************************************
       910-INPUTSET-SELECT-SEC        SECTION.
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-INPUTSET
           ELSE
               MOVE    1               TO  FLG-INPUTSET
           END-IF
      *
           .
       910-INPUTSET-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット読込
      *****************************************************************
       910-INPUTSET-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  INPUTSET-REC
               MOVE    ZERO            TO  FLG-INPUTSET
           ELSE
               INITIALIZE                  INPUTSET-REC
               MOVE    1               TO  FLG-INPUTSET
           END-IF
      *
           .
       910-INPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *      
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
