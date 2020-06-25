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
       PROGRAM-ID.             ORCBPTNUMCHG.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 随時
      *  コンポーネント名  : 患者番号桁数変更
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/09/13    NACL-小豆沢   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-小豆沢  09/11/04  分娩管理テーブル追加
      *  04.05.00    NACL-小豆沢  09/11/05  自由構成にも対応（数字型のみ）
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    チェックマスタ
           SELECT  CHK-FILE        ASSIGN   BPTNUMCHGPARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-CHK.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    チェックマスタ
       FD  CHK-FILE.
       01  CHK-REC.
           03   CHK-KEY                    PIC X(200).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    チェックマスタファイル
           COPY    "CPCOMMONDAT2.INC"
                                           REPLACING   //RECE01//
                                           BY          //BPTNUMCHG//.
      *
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
           03  STS-CHK                     PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
           03  FLG-KOHSKY                  PIC 9(01).
           03  FLG-SEIKYU                  PIC 9(01).
           03  FLG-ORDER                   PIC 9(01).
           03  FLG-RECEPRT                 PIC 9(01).
           03  FLG-TEIKIPRT                PIC 9(01).
           03  FLG-CHKMST                  PIC 9(01).
           03  FLG-RECEPRT-SET             PIC 9(01).
           03  FLG-BUNBEN                  PIC 9(01).
      *
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
           03  CNT-COMMIT                  PIC 9(06).
           03  CNT-CHKMST                  PIC 9(06).
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
           03  IDX                                 PIC 9(05).
      *
      *    パラメタ領域
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
           03  WRK-PARA-HOSPNUM                    PIC 9(02).
           03  WRK-PARA-KBN                        PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DBPATH                          PIC X(100).
           03  WRK-ERRNO                           PIC 9(2).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-HOSPNUM                         PIC 9(02).
           03  WRK-KETA                            PIC 9(02).
           03  WRK-KETA2                           PIC 9(02).
           03  WRK-PTNUM                           PIC X(20).
           03  WRK-PTNUM2                          PIC X(20).
           03  WRK-KENSHOU-KETA                    PIC 9(02).
           03  WRK-KENSHOU-KETA2                   PIC 9(02).
           03  WRK-ADD-KETA                        PIC 9(02).
           03  WRK-ST-KETA                         PIC 9(02).
           03  WRK-MAE-KETA                        PIC 9(02).
           03  WRK-ATO-KETA                        PIC 9(02).
           03  WRK-ATO-KETA2                       PIC 9(02).
           03  WRK-TBLNAME                         PIC X(100).
           03  WRK-SHELLMSG                        PIC X(100).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
           COPY    "CPSK1009.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    公費請求
           COPY    "CPKOHSKY.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    入院オーダ
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    レセプト明細
           COPY    "CPRCF008.INC".
      *
      *    定期請求印刷
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    分娩管理
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
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
           PERFORM 200-MAIN-SEC
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
                                       WRK-PARA-KBN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO   SPA-HOSPNUM
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCPTNUMCHG"       TO  JOB-PGID
           EVALUATE   WRK-PARA-KBN
               WHEN   "1"
                 MOVE    "患者番号桁数変更処理（１桁追加）"
                                       TO  JOB-SHELLMSG
               WHEN   "2"
                 MOVE    "患者番号桁数変更処理（２桁追加）"
                                       TO  JOB-SHELLMSG
               WHEN   "3"
                 MOVE    "患者番号桁数変更処理（３桁追加）"
                                       TO  JOB-SHELLMSG
               WHEN   "4"
                 MOVE    "患者番号桁数変更処理（４桁追加）"
                                       TO  JOB-SHELLMSG
               WHEN   "5"
                 MOVE    "患者番号桁数変更処理（５桁追加）"
                                       TO  JOB-SHELLMSG
               WHEN   OTHER
                 MOVE    "患者番号桁数変更処理"
                                       TO  JOB-SHELLMSG
           END-EVALUATE
      *
           MOVE    JOB-SHELLMSG        TO  WRK-SHELLMSG
      *
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    3                   TO  JOB-STOPFLG
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           MOVE    WRK-PARA-HOSPNUM    TO  BPTNUMCHGPARA-HOSPNUM
           MOVE    "BCHKS01"           TO  BPTNUMCHGPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  BPTNUMCHGPARA-TERMID
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    BPTNUMCHGPARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  BPTNUMCHGPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  RECEERR
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    追加桁数チェック
           IF    WRK-PARA-KBN    =   "1" OR "2" OR "3" OR "4" OR "5"
               CONTINUE
           ELSE
               IF  WRK-PARA-KBN    =   "6" OR "7" OR "8" OR "9"
                   MOVE  "追加桁数は５桁までを指定してください。"
                                          TO  WRK-RECEERR
               ELSE
                   MOVE  "追加桁数が誤りです。"
                                          TO  WRK-RECEERR
               END-IF
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
      *    医療機関ＩＤ編集
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1001-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
                   IF    FLG-SYSKANRI  =   ZERO
                         MOVE    SYS-1001-HOSPNUM  TO  WRK-HOSPNUM
                   ELSE
                         MOVE  "医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                         PERFORM 500-ERR-HENSYU-SEC
                         GO  TO  100-INIT-EXT
                   END-IF
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    患者番号構成管理情報の取得
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1009-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD(7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI  =   ZERO
                     IF  (SYS-1009-PTNUMKSIKBN  =  "1")  AND
                         (SYS-1009-JIYKSIKBN    =  "0"  OR  "1")
                           MOVE "自由構成の英数字型は処理できません。"
                                          TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                     ELSE
                           PERFORM   1001-KETASU-CHK-SEC
                     END-IF
               ELSE
                   MOVE "患者番号構成管理情報が取得できません。"
                                       TO  WRK-RECEERR
                     PERFORM 500-ERR-HENSYU-SEC
                     GO  TO  100-INIT-EXT
               END-IF
           ELSE
               MOVE    "患者番号構成管理情報が取得できません。"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  100-INIT-EXT
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    桁数チェック処理
      *****************************************************************
       1001-KETASU-CHK-SEC                SECTION.
      *
           MOVE     ZERO      TO    WRK-KETA
      *
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *       自由構成
              WHEN    "1"
                  EVALUATE    WRK-PARA-KBN
                     WHEN    "1"
                       COMPUTE WRK-KETA = SYS-1009-JIYKSIKETA + 1
                     WHEN    "2"
                       COMPUTE WRK-KETA = SYS-1009-JIYKSIKETA + 2
                     WHEN    "3"
                       COMPUTE WRK-KETA = SYS-1009-JIYKSIKETA + 3
                     WHEN    "4"
                       COMPUTE WRK-KETA = SYS-1009-JIYKSIKETA + 4
                     WHEN    "5"
                       COMPUTE WRK-KETA = SYS-1009-JIYKSIKETA + 5
                  END-EVALUATE
      *       標準構成
              WHEN    "2"
                  EVALUATE    WRK-PARA-KBN
                     WHEN    "1"
                       COMPUTE WRK-KETA = SYS-1009-HJNKSIRENNUMKETA + 1
                     WHEN    "2"
                       COMPUTE WRK-KETA = SYS-1009-HJNKSIRENNUMKETA + 2
                     WHEN    "3"
                       COMPUTE WRK-KETA = SYS-1009-HJNKSIRENNUMKETA + 3
                     WHEN    "4"
                       COMPUTE WRK-KETA = SYS-1009-HJNKSIRENNUMKETA + 4
                     WHEN    "5"
                       COMPUTE WRK-KETA = SYS-1009-HJNKSIRENNUMKETA + 5
                  END-EVALUATE
      *       拡張構成
              WHEN    "3"
                  EVALUATE    WRK-PARA-KBN
                     WHEN    "1"
                       COMPUTE WRK-KETA = SYS-1009-KKCKSIRENNUMKETA + 1
                     WHEN    "2"
                       COMPUTE WRK-KETA = SYS-1009-KKCKSIRENNUMKETA + 2
                     WHEN    "3"
                       COMPUTE WRK-KETA = SYS-1009-KKCKSIRENNUMKETA + 3
                     WHEN    "4"
                       COMPUTE WRK-KETA = SYS-1009-KKCKSIRENNUMKETA + 4
                     WHEN    "5"
                       COMPUTE WRK-KETA = SYS-1009-KKCKSIRENNUMKETA + 5
                  END-EVALUATE
           END-EVALUATE
      *
           EVALUATE    SYS-1009-PTNUMKSIKBN
      *       自由構成
              WHEN    "1"
               IF     WRK-KETA     >     20
                   MOVE    "桁数が２０桁を超えています。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *       標準構成
              WHEN    "2"
      *       拡張構成
              WHEN    "3"
               IF     WRK-KETA     <=     10
                   IF    SYS-1009-PTNUMKSIKBN    =   "3"
                       MOVE    ZERO       TO    WRK-KETA2
                       COMPUTE   WRK-KETA2    =    WRK-KETA   +
                                               SYS-1009-KKCKSIMAEKETA +
                                               SYS-1009-KKCKSIATOKETA
                       IF   WRK-KETA2    <=   20
                           CONTINUE
                       ELSE
                       MOVE "拡張構成の総桁数が２０桁を超えています。"
                                           TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                       END-IF
                   END-IF
               ELSE
                   MOVE    "連番号桁数が１０桁を超えています。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-EVALUATE
           .
       1001-KETASU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    患者番号取り込み（患者番号テーブル）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-PTNUM
           INITIALIZE                      PTNUM-REC
           MOVE    SYS-1001-HOSPNUM    TO  PTNUM-HOSPNUM
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-PTNUM-SELECT-SEC
           IF      FLG-PTNUM       =   ZERO
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-PTNUM-READ-SEC
               PERFORM  UNTIL  FLG-PTNUM    =    1
                   MOVE  PTNUM-KEY     TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_ptnum"        TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-PTNUM-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    患者番号テーブル更新
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-PTNUM  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-PTNUM
               INITIALIZE                      PTNUM-REC
               MOVE    CHK-KEY             TO  PTNUM-KEY
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-PTNUM-SELECT-SEC
               IF      FLG-PTNUM           =   ZERO
                   MOVE    "tbl_ptnum"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-PTNUM-READ-SEC
                   MOVE       "tbl_ptnum"  TO  MCP-TABLE
                   MOVE       "key"        TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
                   MOVE   PTNUM-PTNUM      TO  WRK-PTNUM
                   PERFORM    2001-PTNUM-CHG-SEC
                   MOVE   WRK-PTNUM        TO  PTNUM-PTNUM
                   MOVE   PTNUM-REC        TO  MCPDATA-REC
                   MOVE  "tbl_ptnum"       TO  MCP-TABLE
                   MOVE  "key"             TO  MCP-PATHNAME
                   MOVE  "DBUPDATE"        TO  MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "PTNUM UPDATE MCP-RC = " MCP-RC
                   END-IF
                   ADD    1                TO  CNT-COMMIT
                   IF     CNT-COMMIT       =   100
                       MOVE    ZERO        TO  CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
      *
           CLOSE                           CHK-FILE
      *
      *    患者番号取り込み（公費請求テーブル）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-KOHSKY
           INITIALIZE                      KOHSKY-REC
           MOVE    SYS-1001-HOSPNUM    TO  KOHSKY-HOSPNUM
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-KOHSKY-SELECT-SEC
           IF      FLG-KOHSKY          =   ZERO
               MOVE    "tbl_kohsky"    TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-KOHSKY-READ-SEC
               PERFORM  UNTIL  FLG-KOHSKY    =    1
                   MOVE  KOHSKY-KEY    TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_kohsky"       TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-KOHSKY-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    公費請求テーブル更新
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-KOHSKY  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-KOHSKY
               INITIALIZE                      KOHSKY-REC
               MOVE    CHK-KEY             TO  KOHSKY-KEY
               MOVE    "tbl_kohsky"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-KOHSKY-SELECT-SEC
               IF      FLG-KOHSKY          =   ZERO
                   MOVE    "tbl_kohsky"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-KOHSKY-READ-SEC
                   MOVE       "tbl_kohsky" TO  MCP-TABLE
                   MOVE       "key"        TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
                   MOVE   KOHSKY-PTNUM     TO   WRK-PTNUM
                   PERFORM    2001-PTNUM-CHG-SEC
                   MOVE   WRK-PTNUM        TO   KOHSKY-PTNUM
                   MOVE   KOHSKY-REC       TO   MCPDATA-REC
                   MOVE  "tbl_kohsky"      TO   MCP-TABLE
                   MOVE  "key"             TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"        TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "KOHSKY UPDATE MCP-RC = " MCP-RC
                   END-IF
                   ADD    1                TO   CNT-COMMIT
                   IF     CNT-COMMIT       =    100
                       MOVE    ZERO        TO   CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
      *
           CLOSE                           CHK-FILE
      *
      *    患者番号取り込み（請求管理テーブル）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-SEIKYU
           INITIALIZE                      RECE10-REC
           MOVE    SYS-1001-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-SEIKYU-SELECT-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-SEIKYU-READ-SEC
               PERFORM  UNTIL  FLG-SEIKYU    =    1
                   MOVE  RECE10-KEY    TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_seikyu"       TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-SEIKYU-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    請求管理テーブル更新
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-SEIKYU  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-SEIKYU
               INITIALIZE                      RECE10-REC
               MOVE    CHK-KEY             TO  RECE10-KEY
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SEIKYU-SELECT-SEC
               IF      FLG-SEIKYU          =   ZERO
                   MOVE    "tbl_seikyu"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-SEIKYU-READ-SEC
                   MOVE       "tbl_seikyu" TO  MCP-TABLE
                   MOVE       "key"        TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
                   MOVE   RECE10-PTNUM     TO   WRK-PTNUM
                   PERFORM    2001-PTNUM-CHG-SEC
                   MOVE   WRK-PTNUM        TO   RECE10-PTNUM
                   MOVE   RECE10-REC       TO   MCPDATA-REC
                   MOVE  "tbl_seikyu"      TO   MCP-TABLE
                   MOVE  "key"             TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"        TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "SEIKYU UPDATE MCP-RC = " MCP-RC
                   END-IF
                   ADD    1                TO   CNT-COMMIT
                   IF     CNT-COMMIT       =    100
                       MOVE    ZERO        TO   CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
      *
           CLOSE                           CHK-FILE
      *
      *    患者番号取り込み（入院オーダテーブル）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-ORDER
           INITIALIZE                      ORDER-REC
           MOVE    SYS-1001-HOSPNUM    TO  ORDER-HOSPNUM
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-ORDER-SELECT-SEC
           IF      FLG-ORDER           =   ZERO
               MOVE    "tbl_order"     TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-ORDER-READ-SEC
               PERFORM  UNTIL  FLG-ORDER    =    1
                   MOVE  ORDER-KEY     TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_order"        TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-ORDER-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    入院オーダテーブル更新
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-ORDER  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-ORDER
               INITIALIZE                      ORDER-REC
               MOVE    CHK-KEY             TO  ORDER-KEY
               MOVE    "tbl_order"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-ORDER-SELECT-SEC
               IF      FLG-ORDER           =   ZERO
                   MOVE    "tbl_order"     TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-ORDER-READ-SEC
                   MOVE       "tbl_order"  TO  MCP-TABLE
                   MOVE       "key"        TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
                   MOVE   ORDER-PTNUM      TO   WRK-PTNUM
                   PERFORM    2001-PTNUM-CHG-SEC
                   MOVE   WRK-PTNUM        TO   ORDER-PTNUM
                   MOVE   ORDER-REC        TO   MCPDATA-REC
                   MOVE  "tbl_order"       TO   MCP-TABLE
                   MOVE  "key"             TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"        TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "ORDER UPDATE MCP-RC = " MCP-RC
                   END-IF
                   ADD    1                TO   CNT-COMMIT
                   IF     CNT-COMMIT       =    100
                       MOVE    ZERO        TO   CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
      *
           CLOSE                           CHK-FILE
      *
      *    患者番号取り込み（レセプト明細テーブル）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-RECEPRT
           INITIALIZE                      RECE08-REC
           MOVE    SYS-1001-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-RECEPRT-SELECT-SEC
           IF      FLG-RECEPRT         =   ZERO
               MOVE    "tbl_receprt"   TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-RECEPRT-READ-SEC
               PERFORM  UNTIL  FLG-RECEPRT   =    1
                   MOVE  RECE08-KEY    TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_receprt"      TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-RECEPRT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    レセプト明細テーブル更新
           MOVE    1                   TO  FLG-RECEPRT-SET
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-RECEPRT  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-RECEPRT
               INITIALIZE                      RECE08-REC
               MOVE    CHK-KEY             TO  RECE08-KEY
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key42"             TO  MCP-PATHNAME
               PERFORM 910-RECEPRT-SELECT-SEC
               IF      FLG-RECEPRT         =   ZERO
                   MOVE    "tbl_receprt"   TO  MCP-TABLE
                   MOVE    "key42"         TO  MCP-PATHNAME
                   PERFORM 910-RECEPRT-READ-SEC
                   MOVE       "tbl_receprt" TO  MCP-TABLE
                   MOVE       "key42"       TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
      *
      *           (DELETE)
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receprt"       TO  MCP-TABLE
                   MOVE    "del4"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC     NOT =   ZERO
                        DISPLAY  "RECEPRT DELETE MCP-RC = " MCP-RC
                   END-IF
      *           (INSERT)
                   MOVE    "tbl_receprt"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE     RECE08-PTNUM       TO   WRK-PTNUM
                   PERFORM  2001-PTNUM-CHG-SEC
                   MOVE     WRK-PTNUM           TO   RECE08-PTNUM
                   MOVE     RECE08-REC          TO  MCPDATA-REC
                   PERFORM  900-ORCDBMAIN-SEC
                   IF       MCP-RC     NOT =   ZERO
                        DISPLAY  "RECEPRT INSERT MCP-RC = " MCP-RC
                   END-IF
      *
                   ADD    1                TO   CNT-COMMIT
                   IF     CNT-COMMIT       =    100
                       MOVE    ZERO        TO   CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
           MOVE    ZERO                TO  FLG-RECEPRT-SET
      *
           CLOSE                           CHK-FILE
      *
      *
      *    患者番号取り込み（定期請求印刷情報）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-TEIKIPRT
           INITIALIZE                      TEIKIPRT-REC
           MOVE    SYS-1001-HOSPNUM    TO  TEIKIPRT-HOSPNUM
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-TEIKIPRT-SELECT-SEC
           IF      FLG-TEIKIPRT        =   ZERO
               MOVE    "tbl_teikiprt"  TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-TEIKIPRT-READ-SEC
               PERFORM  UNTIL  FLG-TEIKIPRT  =    1
                   MOVE  TEIKIPRT-KEY  TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_teikiprt"     TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-TEIKIPRT-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    定期請求印刷情報更新
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-TEIKIPRT  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-TEIKIPRT
               INITIALIZE                      TEIKIPRT-REC
               MOVE    CHK-KEY             TO  TEIKIPRT-KEY
               MOVE    "tbl_teikiprt"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-TEIKIPRT-SELECT-SEC
               IF      FLG-TEIKIPRT        =   ZERO
                   MOVE    "tbl_teikiprt"  TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-TEIKIPRT-READ-SEC
                   MOVE       "tbl_teikiprt" TO  MCP-TABLE
                   MOVE       "key"        TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
                   MOVE   TEIKIPRT-PTNUM   TO   WRK-PTNUM
                   PERFORM    2001-PTNUM-CHG-SEC
                   MOVE   WRK-PTNUM        TO   TEIKIPRT-PTNUM
                   MOVE   TEIKIPRT-REC     TO   MCPDATA-REC
                   MOVE  "tbl_teikiprt"    TO   MCP-TABLE
                   MOVE  "key"             TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"        TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "TEIKIPRT UPDATE MCP-RC = " MCP-RC
                   END-IF
                   ADD    1                TO   CNT-COMMIT
                   IF     CNT-COMMIT       =    100
                       MOVE    ZERO        TO   CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
      *
           CLOSE                           CHK-FILE
      *
      *
      *    患者番号取り込み（分娩管理テーブル）
           OPEN    OUTPUT                  CHK-FILE
           MOVE    ZERO                TO  FLG-BUNBEN
           INITIALIZE                      BUNBEN-REC
           MOVE    SYS-1001-HOSPNUM    TO  BUNBEN-HOSPNUM
           MOVE    "tbl_bunben"        TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 910-BUNBEN-SELECT-SEC
           IF      FLG-BUNBEN          =   ZERO
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "all"           TO  MCP-PATHNAME
               PERFORM 910-BUNBEN-READ-SEC
               PERFORM  UNTIL  FLG-BUNBEN  =    1
                   MOVE  BUNBEN-KEY    TO   CHK-KEY
                   WRITE   CHK-REC
                   MOVE    "tbl_bunben"       TO   MCP-TABLE
                   MOVE    "all"              TO   MCP-PATHNAME
                   PERFORM 910-BUNBEN-READ-SEC
               END-PERFORM
           END-IF
           MOVE    "tbl_bunben"        TO  MCP-TABLE
           MOVE    "all"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE                           CHK-FILE
      *
      *    分娩管理テーブル更新
           OPEN    INPUT                   CHK-FILE
           DISPLAY "TBL-BUNBEN  UPDATE START"
           PERFORM    900-CHK-READ-SEC
           PERFORM  UNTIL  ( FLG-CHKMST  NOT =  ZERO )
               MOVE    ZERO                TO  FLG-BUNBEN
               INITIALIZE                      BUNBEN-REC
               MOVE    CHK-KEY             TO  BUNBEN-KEY
               MOVE    "tbl_bunben"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-BUNBEN-SELECT-SEC
               IF      FLG-BUNBEN          =   ZERO
                   MOVE    "tbl_bunben"    TO  MCP-TABLE
                   MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM 910-BUNBEN-READ-SEC
                   MOVE       "tbl_bunben" TO  MCP-TABLE
                   MOVE       "key"        TO  MCP-PATHNAME
                   PERFORM    900-CLOSE-SEC
                   MOVE   BUNBEN-PTNUM     TO   WRK-PTNUM
                   PERFORM    2001-PTNUM-CHG-SEC
                   MOVE   WRK-PTNUM        TO   BUNBEN-PTNUM
                   MOVE   BUNBEN-REC       TO   MCPDATA-REC
                   MOVE  "tbl_bunben"      TO   MCP-TABLE
                   MOVE  "key"             TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"        TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "BUNBEN UPDATE MCP-RC = " MCP-RC
                   END-IF
                   ADD    1                TO   CNT-COMMIT
                   IF     CNT-COMMIT       =    100
                       MOVE    ZERO        TO   CNT-COMMIT
                       PERFORM   900-DBCOMMIT-SEC
                       PERFORM   900-DBSTART-SEC
                   END-IF
               END-IF
               PERFORM    900-CHK-READ-SEC
           END-PERFORM
      *
           CLOSE                           CHK-FILE
      *
      *    ファイル削除
           PERFORM 800-FILE-DEL-SEC
      *
      *
      *    システム管理（患者番号構成管理情報）の連番号桁数更新
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-1009-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1009-STYUKYMD(7:2)
           MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-SYSKANRI-SELECT-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM     910-SYSKANRI-READ-SEC
               IF    FLG-SYSKANRI  =   ZERO
                   EVALUATE  SYS-1009-PTNUMKSIKBN
                       WHEN  "1"
                          MOVE  WRK-KETA  TO  SYS-1009-JIYKSIKETA
                       WHEN  "2"
                          MOVE  WRK-KETA  TO  SYS-1009-HJNKSIRENNUMKETA
                       WHEN  "3"
                          MOVE  WRK-KETA  TO  SYS-1009-KKCKSIRENNUMKETA
                   END-EVALUATE
                   MOVE   SYS-1009-REC    TO   MCPDATA-REC
                   MOVE  "tbl_syskanri"   TO   MCP-TABLE
                   MOVE  "key"            TO   MCP-PATHNAME
                   MOVE  "DBUPDATE"       TO   MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF   MCP-RC   NOT =  ZERO
                        DISPLAY  "SYS1009 UPDATE MCP-RC = " MCP-RC
                   END-IF
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    ステップ途中処理
           PERFORM 800-MCNDATE-SEC
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  "患者番号桁数変更終了 "
                                       DELIMITED BY SIZE
                   SMCNDATE-HMS(1:2)   DELIMITED BY SIZE
                   "."                 DELIMITED BY SIZE
                   SMCNDATE-HMS(3:2)   DELIMITED BY SIZE
                   "."                 DELIMITED BY SIZE
                   SMCNDATE-HMS(5:2)   DELIMITED BY SIZE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-SHELLMSG        TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号置換え処理
      *****************************************************************
       2001-PTNUM-CHG-SEC          SECTION.
      *
           MOVE        SPACE           TO  WRK-PTNUM2
      *
      *    追加桁数のセット
           EVALUATE    WRK-PARA-KBN
               WHEN    "1"
                   MOVE    1     TO    WRK-ADD-KETA
               WHEN    "2"
                   MOVE    2     TO    WRK-ADD-KETA
               WHEN    "3"
                   MOVE    3     TO    WRK-ADD-KETA
               WHEN    "4"
                   MOVE    4     TO    WRK-ADD-KETA
               WHEN    "5"
                   MOVE    5     TO    WRK-ADD-KETA
           END-EVALUATE
      *
           EVALUATE  SYS-1009-PTNUMKSIKBN
      *       自由構成
              WHEN  "1"
                  EVALUATE    WRK-PARA-KBN
                      WHEN    "1"
                          MOVE   WRK-PTNUM    TO  WRK-PTNUM2(2:19)
                          MOVE   "0"          TO  WRK-PTNUM2(1:1)
                      WHEN    "2"
                          MOVE   WRK-PTNUM    TO  WRK-PTNUM2(3:18)
                          MOVE   "00"         TO  WRK-PTNUM2(1:2)
                      WHEN    "3"
                          MOVE   WRK-PTNUM    TO  WRK-PTNUM2(4:17)
                          MOVE   "000"        TO  WRK-PTNUM2(1:3)
                      WHEN    "4"
                          MOVE   WRK-PTNUM    TO  WRK-PTNUM2(5:16)
                          MOVE   "0000"       TO  WRK-PTNUM2(1:4)
                      WHEN    "5"
                          MOVE   WRK-PTNUM    TO  WRK-PTNUM2(6:15)
                          MOVE   "00000"      TO  WRK-PTNUM2(1:5)
                      END-EVALUATE
      *       標準構成
              WHEN  "2"
                  EVALUATE   SYS-1009-HJNKSIKBN
      *               年＋連番号＋検証番号
                      WHEN   "1"
                          EVALUATE    SYS-1009-HJNKSINENKBN
      *                       和暦
                              WHEN    "1"
                                MOVE   WRK-PTNUM(1:3) TO WRK-PTNUM2(1:3)
                                COMPUTE   WRK-KENSHOU-KETA
                                     =   3  +  
                                         SYS-1009-HJNKSIRENNUMKETA  +
                                         1
                                COMPUTE   WRK-KENSHOU-KETA2
                                     =   3  +  
                                         SYS-1009-HJNKSIRENNUMKETA  +
                                         WRK-ADD-KETA    +
                                         1
                                MOVE   WRK-PTNUM(WRK-KENSHOU-KETA:1)
                                     TO WRK-PTNUM2(WRK-KENSHOU-KETA2:1)
                                MOVE  ALL"0"   TO   WRK-PTNUM2
                                                  (4:WRK-ADD-KETA)
                                COMPUTE  WRK-ST-KETA   =
                                         3   +
                                         WRK-ADD-KETA   +
                                         1
                                MOVE  WRK-PTNUM
                                          (4:SYS-1009-HJNKSIRENNUMKETA)
                                                TO
                                      WRK-PTNUM2
                                          (WRK-ST-KETA:
                                             SYS-1009-HJNKSIRENNUMKETA)
      *                       西暦
                              WHEN    "2"
                                MOVE   WRK-PTNUM(1:4) TO WRK-PTNUM2(1:4)
                                COMPUTE   WRK-KENSHOU-KETA
                                     =   4  +  
                                         SYS-1009-HJNKSIRENNUMKETA  +
                                         1
                                COMPUTE   WRK-KENSHOU-KETA2
                                     =   4  +  
                                         SYS-1009-HJNKSIRENNUMKETA  +
                                         WRK-ADD-KETA    +
                                         1
                                MOVE   WRK-PTNUM(WRK-KENSHOU-KETA:1)
                                     TO WRK-PTNUM2(WRK-KENSHOU-KETA2:1)
                                MOVE  ALL"0"   TO   WRK-PTNUM2
                                                  (5:WRK-ADD-KETA)
                                COMPUTE  WRK-ST-KETA   =
                                         4   +
                                         WRK-ADD-KETA   +
                                         1
                                MOVE  WRK-PTNUM
                                          (5:SYS-1009-HJNKSIRENNUMKETA)
                                                TO
                                      WRK-PTNUM2
                                          (WRK-ST-KETA:
                                             SYS-1009-HJNKSIRENNUMKETA)
                          END-EVALUATE
      *               年＋連番号
                      WHEN   "2"
                          EVALUATE    SYS-1009-HJNKSINENKBN
      *                       和暦
                              WHEN    "1"
                                MOVE   WRK-PTNUM(1:3) TO WRK-PTNUM2(1:3)
                                MOVE  ALL"0"   TO   WRK-PTNUM2
                                                  (4:WRK-ADD-KETA)
                                COMPUTE  WRK-ST-KETA   =
                                         3   +
                                         WRK-ADD-KETA   +
                                         1
                                MOVE  WRK-PTNUM
                                          (4:SYS-1009-HJNKSIRENNUMKETA)
                                                TO
                                      WRK-PTNUM2
                                          (WRK-ST-KETA:
                                             SYS-1009-HJNKSIRENNUMKETA)
      *                       西暦
                              WHEN    "2"
                                MOVE   WRK-PTNUM(1:4) TO WRK-PTNUM2(1:4)
                                MOVE  ALL"0"   TO   WRK-PTNUM2
                                                  (5:WRK-ADD-KETA)
                                COMPUTE  WRK-ST-KETA   =
                                         4   +
                                         WRK-ADD-KETA   +
                                         1
                                MOVE  WRK-PTNUM
                                          (5:SYS-1009-HJNKSIRENNUMKETA)
                                                TO
                                      WRK-PTNUM2
                                          (WRK-ST-KETA:
                                             SYS-1009-HJNKSIRENNUMKETA)
                          END-EVALUATE
      *               連番号＋検証番号
                      WHEN   "3"
                          COMPUTE   WRK-KENSHOU-KETA
                               =   SYS-1009-HJNKSIRENNUMKETA  +
                                   1
                          COMPUTE   WRK-KENSHOU-KETA2
                               =   SYS-1009-HJNKSIRENNUMKETA  +
                                   WRK-ADD-KETA    +
                                   1
                          MOVE   WRK-PTNUM(WRK-KENSHOU-KETA:1)
                               TO WRK-PTNUM2(WRK-KENSHOU-KETA2:1)
                          MOVE  ALL"0"   TO   WRK-PTNUM2
                                           (1:WRK-ADD-KETA)
                          COMPUTE  WRK-ST-KETA   =
                                   WRK-ADD-KETA   +
                                   1
                          MOVE  WRK-PTNUM
                                   (1:SYS-1009-HJNKSIRENNUMKETA)
                                          TO
                                WRK-PTNUM2
                                   (WRK-ST-KETA:
                                       SYS-1009-HJNKSIRENNUMKETA)
      *               連番号のみ
                      WHEN   "4"
                          EVALUATE    WRK-PARA-KBN
                              WHEN    "1"
                                MOVE   WRK-PTNUM    TO  WRK-PTNUM2(2:9)
                                MOVE   "0"          TO  WRK-PTNUM2(1:1)
                              WHEN    "2"
                                MOVE   WRK-PTNUM    TO  WRK-PTNUM2(3:8)
                                MOVE   "00"         TO  WRK-PTNUM2(1:2)
                              WHEN    "3"
                                MOVE   WRK-PTNUM    TO  WRK-PTNUM2(4:7)
                                MOVE   "000"        TO  WRK-PTNUM2(1:3)
                              WHEN    "4"
                                MOVE   WRK-PTNUM    TO  WRK-PTNUM2(5:6)
                                MOVE   "0000"       TO  WRK-PTNUM2(1:4)
                              WHEN    "5"
                                MOVE   WRK-PTNUM    TO  WRK-PTNUM2(6:5)
                                MOVE   "00000"      TO  WRK-PTNUM2(1:5)
                          END-EVALUATE
                   END-EVALUATE
      *       拡張構成
              WHEN  "3"
                  EVALUATE   SYS-1009-KKCKSIKBN
      *               フリー＋連番号＋フリー
                      WHEN   "1"
                          MOVE   WRK-PTNUM(1:SYS-1009-KKCKSIMAEKETA)
                             TO  WRK-PTNUM2(1:SYS-1009-KKCKSIMAEKETA)
                          COMPUTE   WRK-MAE-KETA   =
                                    SYS-1009-KKCKSIMAEKETA  +   1
                          COMPUTE   WRK-ATO-KETA   =
                                    SYS-1009-KKCKSIMAEKETA  +
                                    SYS-1009-KKCKSIRENNUMKETA  +
                                    1
                          COMPUTE   WRK-ATO-KETA2  =
                                    SYS-1009-KKCKSIMAEKETA  +
                                    WRK-ADD-KETA            +
                                    SYS-1009-KKCKSIRENNUMKETA  +
                                    1
                          MOVE   WRK-PTNUM
                                 (WRK-ATO-KETA:SYS-1009-KKCKSIATOKETA)
                                    TO
                                 WRK-PTNUM2
                                 (WRK-ATO-KETA2:SYS-1009-KKCKSIATOKETA)
                          MOVE  ALL"0"   TO   WRK-PTNUM2
                                           (WRK-MAE-KETA:WRK-ADD-KETA)
                          COMPUTE  WRK-ST-KETA   =
                                   SYS-1009-KKCKSIMAEKETA   +
                                   WRK-ADD-KETA   +
                                   1
                          MOVE  WRK-PTNUM
                                (WRK-MAE-KETA:SYS-1009-KKCKSIRENNUMKETA)
                                       TO
                                 WRK-PTNUM2
                                (WRK-ST-KETA:SYS-1009-KKCKSIRENNUMKETA)
      *               フリー＋連番号
                      WHEN   "2"
                          MOVE   WRK-PTNUM(1:SYS-1009-KKCKSIMAEKETA)
                             TO  WRK-PTNUM2(1:SYS-1009-KKCKSIMAEKETA)
                          COMPUTE   WRK-MAE-KETA   =
                                    SYS-1009-KKCKSIMAEKETA  +   1
                          MOVE  ALL"0"   TO   WRK-PTNUM2
                                           (WRK-MAE-KETA:WRK-ADD-KETA)
                          COMPUTE  WRK-ST-KETA   =
                                   SYS-1009-KKCKSIMAEKETA   +
                                   WRK-ADD-KETA   +
                                   1
                          MOVE  WRK-PTNUM
                                (WRK-MAE-KETA:SYS-1009-KKCKSIRENNUMKETA)
                                       TO
                                 WRK-PTNUM2
                                (WRK-ST-KETA:SYS-1009-KKCKSIRENNUMKETA)
      *               連番号＋フリー
                      WHEN   "3"
                          COMPUTE   WRK-ATO-KETA   =
                                    SYS-1009-KKCKSIMAEKETA  +
                                    SYS-1009-KKCKSIRENNUMKETA  +
                                    1
                          COMPUTE   WRK-ATO-KETA2  =
                                    SYS-1009-KKCKSIMAEKETA  +
                                    WRK-ADD-KETA            +
                                    SYS-1009-KKCKSIRENNUMKETA  +
                                    1
                          MOVE   WRK-PTNUM
                                 (WRK-ATO-KETA:SYS-1009-KKCKSIATOKETA)
                                    TO
                                 WRK-PTNUM2
                                 (WRK-ATO-KETA2:SYS-1009-KKCKSIATOKETA)
                          MOVE  ALL"0"   TO   WRK-PTNUM2
                                           (1:WRK-ADD-KETA)
                          COMPUTE  WRK-ST-KETA   =
                                   WRK-ADD-KETA   +
                                   1
                          MOVE  WRK-PTNUM
                                (1:SYS-1009-KKCKSIRENNUMKETA)
                                       TO
                                 WRK-PTNUM2
                                (WRK-ST-KETA:SYS-1009-KKCKSIRENNUMKETA)
                  END-EVALUATE
           END-EVALUATE
      *
           IF      FLG-RECEPRT-SET    =    1
               PERFORM   VARYING   IDX     FROM  20  BY  -1
                 UNTIL  (WRK-PTNUM2(IDX:1)  NOT =  SPACE)  OR
                        (IDX       =   1)
                     MOVE  "Z"     TO   WRK-PTNUM2(IDX:1)
               END-PERFORM
           END-IF
      *
           DISPLAY "PTNUM CHANGE " WRK-PTNUM " -> " WRK-PTNUM2
      *
           MOVE    WRK-PTNUM2          TO  WRK-PTNUM
      *
           .
       2001-PTNUM-CHG-EXT.
           EXIT.
      *
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
      *    ファイル削除処理
      *****************************************************************
       800-FILE-DEL-SEC            SECTION.
      *
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    BPTNUMCHGPARA   TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       800-FILE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    マシン日付取得サブ
      *****************************************************************
       800-MCNDATE-SEC                 SECTION.
      *
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           .
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    処理経過編集処理
      *****************************************************************
       800-SYORI-KEIKA-SEC         SECTION.
      *
           PERFORM 800-MCNDATE-SEC
      *
           MOVE    SPACE           TO  WRK-TBLNAME
           EVALUATE    MCP-TABLE
           WHEN    "tbl_ptnum"
               MOVE    "患者番号"
                                   TO  WRK-TBLNAME
           WHEN    "tbl_kohsky"
               MOVE    "地方公費請求書"
                                   TO  WRK-TBLNAME
           WHEN    "tbl_seikyu"
               MOVE    "請求管理"
                                   TO  WRK-TBLNAME
           WHEN    "tbl_order"
               MOVE    "入院オーダ"
                                   TO  WRK-TBLNAME
           WHEN    "tbl_receprt"
               MOVE    "レセプト明細書"
                                   TO  WRK-TBLNAME
           WHEN    "tbl_teikiprt"
               MOVE    "定期請求印刷情報"
                                   TO  WRK-TBLNAME
           WHEN    "tbl_bunben"
               MOVE    "分娩管理"
                                   TO  WRK-TBLNAME
           END-EVALUATE
      *
      *    ステップ途中処理
           MOVE    "STM"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           STRING  WRK-TBLNAME         DELIMITED BY SPACE
                   "テーブル更新中 "
                                       DELIMITED BY SIZE
                   SMCNDATE-HMS(1:2)   DELIMITED BY SIZE
                   "."                 DELIMITED BY SIZE
                   SMCNDATE-HMS(3:2)   DELIMITED BY SIZE
                   "."                 DELIMITED BY SIZE
                   SMCNDATE-HMS(5:2)   DELIMITED BY SIZE
           INTO    JOB-YOBI
           END-STRING
           MOVE    WRK-SHELLMSG        TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           DISPLAY JOB-SHELLMSG
      *
           .
       800-SYORI-KEIKA-EXT.
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
      *    システム管理マスター検索
      *****************************************************************
       910-SYSKANRI-SELECT-SEC        SECTION.
      *
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-SELECT-EXT.
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
                                               SYS-1009-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                      SYS-1001-REC
                                               SYS-1009-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター検索
      *****************************************************************
       910-PTNUM-SELECT-SEC        SECTION.
      *
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-PTNUM
           ELSE
               MOVE    1                  TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスター読込
      *****************************************************************
       910-PTNUM-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  PTNUM-REC
               MOVE    ZERO               TO  FLG-PTNUM
           ELSE
               INITIALIZE                     PTNUM-REC
               MOVE    1                  TO  FLG-PTNUM
           END-IF
      *
           .
       910-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスター検索
      *****************************************************************
       910-KOHSKY-SELECT-SEC        SECTION.
      *
           MOVE    KOHSKY-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-KOHSKY
           ELSE
               MOVE    1                  TO  FLG-KOHSKY
           END-IF
      *
           .
       910-KOHSKY-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスター読込
      *****************************************************************
       910-KOHSKY-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  KOHSKY-REC
               MOVE    ZERO               TO  FLG-KOHSKY
           ELSE
               INITIALIZE                     KOHSKY-REC
               MOVE    1                  TO  FLG-KOHSKY
           END-IF
      *
           .
       910-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスター検索
      *****************************************************************
       910-SEIKYU-SELECT-SEC        SECTION.
      *
           MOVE    RECE10-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SEIKYU
           ELSE
               MOVE    1                  TO  FLG-SEIKYU
           END-IF
      *
           .
       910-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスター読込
      *****************************************************************
       910-SEIKYU-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  RECE10-REC
               MOVE    ZERO               TO  FLG-SEIKYU
           ELSE
               INITIALIZE                     RECE10-REC
               MOVE    1                  TO  FLG-SEIKYU
           END-IF
      *
           .
       910-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院オーダ検索
      *****************************************************************
       910-ORDER-SELECT-SEC        SECTION.
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-ORDER
           ELSE
               MOVE    1               TO  FLG-ORDER
           END-IF
      *
           .
       910-ORDER-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院オーダ読込
      *****************************************************************
       910-ORDER-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  ORDER-REC
               MOVE    ZERO               TO  FLG-ORDER
           ELSE
               INITIALIZE                     ORDER-REC
               MOVE    1                  TO  FLG-ORDER
           END-IF
      *
           .
       910-ORDER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細検索
      *****************************************************************
       910-RECEPRT-SELECT-SEC        SECTION.
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECEPRT
           ELSE
               MOVE    1               TO  FLG-RECEPRT
           END-IF
      *
           .
       910-RECEPRT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細読込
      *****************************************************************
       910-RECEPRT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  RECE08-REC
               MOVE    ZERO               TO  FLG-RECEPRT
           ELSE
               INITIALIZE                     RECE08-REC
               MOVE    1                  TO  FLG-RECEPRT
           END-IF
      *
           .
       910-RECEPRT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求印刷情報検索
      *****************************************************************
       910-TEIKIPRT-SELECT-SEC        SECTION.
      *
           MOVE    TEIKIPRT-REC       TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-TEIKIPRT
           ELSE
               MOVE    1               TO  FLG-TEIKIPRT
           END-IF
      *
           .
       910-TEIKIPRT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    定期請求印刷情報読込
      *****************************************************************
       910-TEIKIPRT-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  TEIKIPRT-REC
               MOVE    ZERO               TO  FLG-TEIKIPRT
           ELSE
               INITIALIZE                     TEIKIPRT-REC
               MOVE    1                  TO  FLG-TEIKIPRT
           END-IF
      *
           .
       910-TEIKIPRT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩管理検索
      *****************************************************************
       910-BUNBEN-SELECT-SEC          SECTION.
      *
           MOVE    BUNBEN-REC          TO  MCPDATA-REC
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-BUNBEN
           ELSE
               MOVE    1               TO  FLG-BUNBEN
           END-IF
      *
           .
       910-BUNBEN-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    分娩管理読込
      *****************************************************************
       910-BUNBEN-READ-SEC        SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC        TO  BUNBEN-REC
               MOVE    ZERO               TO  FLG-BUNBEN
           ELSE
               INITIALIZE                     BUNBEN-REC
               MOVE    1                  TO  FLG-BUNBEN
           END-IF
      *
           .
       910-BUNBEN-READ-EXT.
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
       900-DBCOMMIT-SEC              SECTION.
      *
           PERFORM 800-SYORI-KEIKA-SEC
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
       900-DBSTART-SEC              SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBSTART-EXT.
           EXIT.
      *****************************************************************
      *    チェックマスタファイル読込み処理
      *****************************************************************
       900-CHK-READ-SEC                SECTION.
      *
           MOVE        ZERO            TO  FLG-CHKMST
           READ    CHK-FILE
           AT  END
               MOVE    1               TO  FLG-CHKMST
           END-READ
      *
           IF    ( FLG-CHKMST          =   ZERO )
               COMPUTE CNT-CHKMST  =   CNT-CHKMST  +   1
           END-IF
      *
           .
       900-CHK-READ-EXT.
           EXIT.
