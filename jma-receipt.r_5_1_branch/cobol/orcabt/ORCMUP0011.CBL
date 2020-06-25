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
       PROGRAM-ID.                 ORCMUP0011.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ＤＢ作成処理
      *  コンポーネント名  : 保険番号マスタ更新処理
      *                    : 
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/09/05    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-太田    03/01/08  保険番号マスタレイアウト変更
      *                                     対応
      * 01.00.02     NACL-伊藤    03/02/13  保険番号マスタレイアウト変更
      *                                     対応−法別番号チェック区分
      * 01.00.03     NACL-太田    04/02/09  保険番号マスタレイアウト変更
      *                                     対応
      * 01.00.03     NACL-小豆沢  06/04/03  MONFUNC対応
      * 04.03.00     NACL-太田    08/05/28  エラー処理強化対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    入力：保険番号マスタ更新ファイル
           SELECT  IN-FILE         ASSIGN
                                   ASS-IN
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-IN.
      *    出力：エラーファイル
           SELECT  ERR-FILE        ASSIGN
                                   ASS-ERR
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-ERR.
      *    出力：プルーフファイル
           SELECT  PLF-FILE        ASSIGN
                                   ASS-PLF
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PLF.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    入力：保険番号マスタ更新ファイル
       FD  IN-FILE.
      *---(01.00.01) LINE UPD START -------------------------------------
      *    COPY    "CPHKNNUMWK.INC".
       01  IN-REC                      PIC X(10000).
      *---(01.00.01) LINE UPD END   -------------------------------------
      *
      *    出力：エラーファイル
       FD  ERR-FILE.
           COPY    "CPHKNNUMWK.INC"       REPLACING
                                       //HKNWK-// BY //ERR-HKNWK-//.
      *
      *    出力：プルーフファイル
       FD  PLF-FILE.
       01  PLF-REC                 PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
           COPY    "CPHKNNUMWK.INC".
      *
      *    旧レイアウト
           COPY    "CPHKNNUMWKV01.INC".
           COPY    "CPHKNNUMWKV02.INC".
      *
      *    ファイル指定領域
       01  ASS-AREA.
           03  ASS-IN                  PIC X(256).
           03  ASS-ERR                 PIC X(256).
           03  ASS-PLF                 PIC X(256).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-IN                  PIC X(02).
           03  STS-ERR                 PIC X(02).
           03  STS-PLF                 PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PARA                PIC 9(01).
           03  FLG-IN                  PIC 9(01).
           03  FLG-PLF                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PARA                PIC 9(06).
           03  CNT-IN                  PIC 9(06).
           03  CNT-INS                 PIC 9(06).
           03  CNT-UPD                 PIC 9(06).
           03  CNT-DEL                 PIC 9(06).
           03  CNT-ERR                 PIC 9(06).
           03  CNT-PLF                 PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *
       01  SYS-DATE.
           03  SYS-YY                  PIC 9(02).
           03  SYS-MM                  PIC 9(02).
           03  SYS-DD                  PIC 9(02).
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
      *　　主キー更新用項目
           03  WRK-TNSUP-KEY.
               05  WRK-TNSUP-HKNNUM         PIC  X(09).
               05  WRK-TNSUP-TEKSTYMD     PIC  X(08).
               05  WRK-TNSUP-TEKEDYMD     PIC  X(08).
      *
           03  WRK-ERRCD               PIC 9(01). 
      *
           03  WRK-IN                  PIC X(01). 
           03  WRK-HOMEDIR             PIC X(128).
      *
      *    出力ファイル領域
       01  MES-AREA.
           03  MES-TITLE1.
               05  FILLER              PIC X(26) VALUE
                                       "【PGID:ORCMUP0011.CBL】".
               05  MES-TITLE1-YY       PIC X(04) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-MM       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(01) VALUE ".".
               05  MES-TITLE1-DD       PIC X(02) VALUE SPACE.
               05  FILLER              PIC X(02) VALUE SPACE.
               05  MES-TITLE1-HOSPNUM  PIC 9(02).
      *
       01  WRK-MSG-AREA.
           03  WRK-MSG-01              PIC X(30).
           03  FILLER                  PIC X(01)   VALUE   SPACE.
           03  WRK-HKNNUM              PIC X(03).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-PAYKBN              PIC X(02).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-TEKSTYMD            PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-TEKEDYMD            PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-TEKEDYMD2           PIC X(08).
           03  FILLER                  PIC X(01)   VALUE   ",".
           03  WRK-NAME                PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-HOMEDIR           PIC X(28)  VALUE
               "/var/lib/jma-receipt/master/".
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
           COPY    "ENUM-VALUE".
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    ファイル定義
      *****************************************************************
      *
      *
      *    保険番号マスタ−
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    システム管理マスタ
           COPY    "CPSK1001.INC".
      *
      *    ジョブ管理情報
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    共通パラメタ
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPMUPLINK.INC".
      *
       PROCEDURE                       DIVISION
           USING
           LNK-AREA.
      *
      ****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL ( FLG-IN      =   1 )
                    OR   ( LNK-RC  NOT =   ZERO )
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
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
      *
           MOVE    LNK-HOSPNUM     TO  SPA-HOSPNUM
      *
      *    DB OPEN
      *    PERFORM 100-DBOPEN-SEC
      *    IF      MCP-RC      NOT =   ZERO
      *        DISPLAY "DBOPEN ERR"
      *    END-IF.
      * 
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ZERO                TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE    
               DISPLAY "SYS 001 ERR"
           END-IF                                     
      *
      *    ファイル情報
           PERFORM 101-ASSIN-GET-SEC
      *
      *    入力　初期処理
           OPEN    INPUT           IN-FILE
           IF  STS-IN          NOT =   ZERO
               MOVE    1           TO  FLG-IN
               DISPLAY "*(ORCMUP0011)* IN-FILE OPN STS["
                                       STS-IN "]"
           END-IF
      *
      *    出力ファイル　初期処理
           PERFORM 2700-OUTLST-INIT-SEC
      *
      *    保険番号マスタ更新ファイル読込み処理
           IF      FLG-IN              =   ZERO
               PERFORM 900-IN-KENSAKU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル情報取得
      *****************************************************************
       101-ASSIN-GET-SEC                SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
      *
           IF      SGETTEMP-MW         =   SPA-PANDA
               MOVE    CONST-HOMEDIR       TO  WRK-HOMEDIR
           ELSE
               MOVE    SGETTEMP-TEMPDIR    TO  WRK-HOMEDIR
           END-IF
      *
      *    入力ファイル
           MOVE    SPACE               TO  ASS-IN
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-DBFILE          DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                                       INTO    ASS-IN
           END-STRING
      *
      *    エラーファイル
           MOVE    SPACE               TO  ASS-ERR
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".ERR"              DELIMITED  BY  SIZE
                                       INTO    ASS-ERR
           END-STRING
      *
      *    プルーフファイル
           MOVE    SPACE               TO  ASS-PLF
           STRING  WRK-HOMEDIR         DELIMITED  BY  SPACE
                   SPA-HOSPNUM         DELIMITED  BY  SIZE
                   "/"                 DELIMITED  BY  SIZE
                   "orca-mstlog/"      DELIMITED  BY  SIZE
                   LNK-UPFILE          DELIMITED  BY  SPACE
                   ".PRF"              DELIMITED  BY  SIZE
                                       INTO    ASS-PLF
           END-STRING
      *
           .
       101-ASSIN-GET-EXT.
           EXIT.
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    ZERO            TO  WRK-ERRCD
      *
           EVALUATE    HKNWK-SYORIKBN
      *        追加
               WHEN    "I"
                   PERFORM 2010-INSERT-SYORI-SEC
      *        更新
               WHEN    "U"
                   PERFORM 2020-UPDATE-SYORI-SEC
      *        削除
               WHEN    "D"
                   PERFORM 2030-DELETE-SYORI-SEC
           END-EVALUATE
      *
      *    保険番号マスタ更新ファイル読込み処理
           PERFORM 900-IN-KENSAKU-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加  処理
      *****************************************************************
       2010-INSERT-SYORI-SEC               SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNWK-HKNNUM        TO  HKN-HKNNUM
           MOVE    HKNWK-PAYKBN        TO  HKN-PAYKBN
           MOVE    HKNWK-TEKSTYMD      TO  HKN-TEKSTYMD
                                           HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
      *    単純追加
           IF      HKNWK-TEKEDYMD2    =   SPACE
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *        追加エラー
               IF      FLG-HKNNUM           =   ZERO
                   MOVE    5               TO  WRK-ERRCD
                   PERFORM 2040-ERR-SYORI-SEC
                   GO      TO      2010-INSERT-SYORI-EXT
               END-IF
      *
      *        ＤＢ追加
               PERFORM 20101-HKNNUM-INS-SEC
           ELSE
      *
      *    追加更新
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM UNTIL   (FLG-HKNNUM      =   1  )  OR
                               (FLG-SYORI       =   1  )
                   IF     (HKN-TEKSTYMD   <=  HKNWK-TEKEDYMD2 ) AND
                          (HKN-TEKEDYMD   >   HKNWK-TEKEDYMD2 )
      *                更新
                       MOVE    HKNWK-TEKEDYMD2    TO  HKN-TEKEDYMD
                       PERFORM 20102-HKNNUM-UPD-SEC
      *
      *                ＤＢ追加
      ***************  PERFORM 20101-HKNNUM-INS-SEC
                       MOVE    1                   TO  FLG-SYORI
                   ELSE
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM                     910-HKNNUM-READ-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
      *******  IF      FLG-SYORI           =   ZERO
      *
      *            ＤＢ追加
                   PERFORM 20101-HKNNUM-INS-SEC
      *******  END-IF
           END-IF
           .
       2010-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加  処理
      *****************************************************************
       20101-HKNNUM-INS-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
      *****MOVE    HKNWK-KEY           TO  HKN-KEY
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNWK-HKNNUM        TO  HKN-HKNNUM
           MOVE    HKNWK-TEKSTYMD      TO  HKN-TEKSTYMD
           MOVE    HKNWK-PAYKBN        TO  HKN-PAYKBN
           MOVE    HKNWK-TEKEDYMD      TO  HKN-TEKEDYMD
           MOVE    HKNWK-REC (55:)     TO  HKNNUM-REC(24:)
      *    保険番号マスタの確認
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM                     910-HKNNUM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           IF     (FLG-HKNNUM         =   ZERO )  AND
                  (HKN-TEKSTYMD       =   HKNWK-TEKSTYMD) AND
                  (HKN-TEKEDYMD       =   HKNWK-TEKEDYMD)
               MOVE    5           TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
               GO      TO      20101-HKNNUM-INS-EXT
           END-IF
      *
      *    保険番号マスタ追加
           INITIALIZE                  HKNNUM-REC
      *****MOVE    HKNWK-KEY           TO  HKN-KEY
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNWK-HKNNUM        TO  HKN-HKNNUM
           MOVE    HKNWK-TEKSTYMD      TO  HKN-TEKSTYMD
           MOVE    HKNWK-PAYKBN        TO  HKN-PAYKBN
           MOVE    HKNWK-TEKEDYMD      TO  HKN-TEKEDYMD
           MOVE    HKNWK-REC(55:)      TO  HKNNUM-REC(24:)
           MOVE    LNK-MNTYMD          TO  HKN-CREYMD
                                           HKN-UPYMD
           MOVE    LNK-MNTHMS          TO  HKN-UPHMS
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    1               TO  LNK-RC
               MOVE    1               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           ELSE
               ADD     1               TO  CNT-INS
           END-IF
           .
       20101-HKNNUM-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新  処理
      *****************************************************************
       20102-HKNNUM-UPD-SEC           SECTION.
      *
           MOVE    LNK-MNTYMD          TO  HKN-UPYMD
           MOVE    LNK-MNTHMS          TO  HKN-UPHMS
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    2               TO  LNK-RC
               MOVE    2               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20102-HKNNUM-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ削除  処理
      *****************************************************************
       20103-HKNNUM-DEL-SEC           SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               MOVE    3               TO  LNK-RC
               MOVE    4               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       20103-HKNNUM-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新  処理
      *****************************************************************
       2020-UPDATE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNWK-HKNNUM        TO  HKN-HKNNUM
           MOVE    HKNWK-PAYKBN        TO  HKN-PAYKBN    
           MOVE    HKNWK-TEKSTYMD      TO  HKN-TEKSTYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
      *    通常更新
           IF      HKNWK-TEKEDYMD2     =   SPACE
               PERFORM UNTIL   (FLG-HKNNUM      =   1  )  OR
                               (FLG-SYORI       =   1  )
                   IF     (HKN-TEKSTYMD   <=  HKNWK-TEKSTYMD ) AND
                          (HKN-TEKEDYMD   >=  HKNWK-TEKSTYMD )
      *                更新
      *****************MOVE    HKNWK-KEY           TO  HKN-KEY
                       MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                       MOVE    HKNWK-HKNNUM        TO  HKN-HKNNUM
                       MOVE    HKNWK-TEKSTYMD      TO  HKN-TEKSTYMD
                       MOVE    HKNWK-PAYKBN        TO  HKN-PAYKBN    
                       MOVE    HKNWK-TEKEDYMD      TO  HKN-TEKEDYMD
                       MOVE    HKNWK-REC(55:)      TO  HKNNUM-REC(24:)
                       PERFORM 20102-HKNNUM-UPD-SEC
                       ADD     1                   TO  CNT-UPD
      *
                       MOVE    1                   TO  FLG-SYORI
                   ELSE
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM                     910-HKNNUM-READ-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           ELSE
      *
      *    期限更新
               PERFORM UNTIL   (FLG-HKNNUM      =   1  )  OR
                               (FLG-SYORI       =   1  )
                   IF     (HKN-TEKSTYMD   <=  HKNWK-TEKEDYMD2 ) AND
                          (HKN-TEKEDYMD   >   HKNWK-TEKEDYMD2 )
      *                更新
                       MOVE    HKNWK-TEKEDYMD2     TO  HKN-TEKEDYMD
                       PERFORM 20102-HKNNUM-UPD-SEC
                       ADD     1                   TO  CNT-UPD
      *
                       MOVE    1                   TO  FLG-SYORI
                   ELSE
                       MOVE    "tbl_hknnum"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM                     910-HKNNUM-READ-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
           END-IF
      *
      *    更新エラー
           IF      FLG-SYORI           =   ZERO
               MOVE    3               TO  WRK-ERRCD
               PERFORM 2040-ERR-SYORI-SEC
           END-IF
           .
       2020-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除  処理
      *****************************************************************
       2030-DELETE-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNWK-HKNNUM        TO  HKN-HKNNUM
           MOVE    HKNWK-PAYKBN        TO  HKN-PAYKBN    
           MOVE    HKNWK-TEKSTYMD      TO  HKN-TEKSTYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM                     910-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
      *    削除処理
           IF      FLG-HKNNUM          =   ZERO
                   PERFORM 20103-HKNNUM-DEL-SEC
                   ADD     1           TO  CNT-DEL
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2030-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー処理
      *****************************************************************
       2040-ERR-SYORI-SEC            SECTION.
      *
      *    エラー出力
           MOVE    HKNWK-REC          TO  ERR-HKNWK-REC
           WRITE                          ERR-HKNWK-REC
           ADD     1                  TO  CNT-ERR
      *
      *    エラーメッセージ出力
           PERFORM 20401-PLF-OUT-SEC
      *
           .
       2040-ERR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ出力処理
      *****************************************************************
       20401-PLF-OUT-SEC            SECTION.
      *
           MOVE    HKNWK-HKNNUM      TO  WRK-HKNNUM
           MOVE    HKNWK-PAYKBN      TO  WRK-PAYKBN  
           MOVE    HKNWK-TEKSTYMD    TO  WRK-TEKSTYMD
           MOVE    HKNWK-TEKEDYMD    TO  WRK-TEKEDYMD
           MOVE    HKNWK-TEKEDYMD2   TO  WRK-TEKEDYMD2
           MOVE    HKNWK-SEIDONAME   TO  WRK-NAME
           EVALUATE    WRK-ERRCD
               WHEN    1
                   MOVE    "追加エラー"       TO  WRK-MSG-01
               WHEN    2
                   MOVE    "更新エラー"       TO  WRK-MSG-01
               WHEN    3
                   MOVE    "更新対象なし"   TO  WRK-MSG-01
               WHEN    4
                   MOVE    "削除エラー"       TO  WRK-MSG-01
               WHEN    5
                   MOVE    "保険番号登録済み"
                                               TO  WRK-MSG-01
           END-EVALUATE
      *
           MOVE    WRK-MSG-AREA       TO  PLF-REC
           WRITE                          PLF-REC
           ADD     1                  TO  CNT-PLF
      *
           .
       20401-PLF-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力ファイル　初期処理
      *****************************************************************
       2700-OUTLST-INIT-SEC            SECTION.
      *
           OPEN    OUTPUT              ERR-FILE
           IF  STS-ERR             NOT =   ZERO
               DISPLAY "*(ORCMUP0011)* OUTFILE OPN STS[" STS-ERR "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
      *
      *    エラーレコード出力ファイル　オープン
           OPEN    OUTPUT              PLF-FILE
           IF  STS-PLF             NOT =   ZERO
               DISPLAY "*(ORCMUP0011)* PLFFILE OPN STS[" STS-PLF "]"
               GO      TO              2700-OUTLST-INIT-EXT
           END-IF
           ACCEPT  SYS-DATE            FROM    DATE
           COMPUTE WRK-SYY             =   SYS-YY    +   2000
           MOVE    WRK-SYY             TO  MES-TITLE1-YY
           MOVE    SYS-MM              TO  MES-TITLE1-MM
           MOVE    SYS-DD              TO  MES-TITLE1-DD
           MOVE    SPA-HOSPNUM         TO  MES-TITLE1-HOSPNUM
           MOVE    MES-TITLE1          TO  PLF-REC
           WRITE   PLF-REC
      *
           .
       2700-OUTLST-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    DB DISCONNECT
      *    PERFORM 900-DBDISCONNECT-SEC
      *
      *    出力ファイル　終了処理
           CLOSE                       IN-FILE
           CLOSE                       ERR-FILE
           CLOSE                       PLF-FILE
      *
           IF    ( LNK-RC              =   ZERO )
               IF    ( CNT-IN      NOT =   LNK-COUNT )
      *
                   MOVE    99              TO  LNK-RC
      *
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    1               TO  JOB-HOSPNUM
                   MOVE    "tbl_jobkanri"  TO  MCP-TABLE
                   MOVE    "abort"         TO  MCP-PATHNAME
                   MOVE    JOBKANRI-REC    TO  MCPDATA-REC
                   MOVE    "DBSELECT"      TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
               END-IF
      *
           END-IF
      *
           DISPLAY "*(ORCMUP0011)* INPUT  /I   CNT[" CNT-IN  "]"
           DISPLAY "*(ORCMUP0011)*  INSERT     CNT[" CNT-INS "]"
           DISPLAY "*(ORCMUP0011)*  UPDATE     CNT[" CNT-UPD "]"
           DISPLAY "*(ORCMUP0011)*  DELETE     CNT[" CNT-DEL "]"
           DISPLAY "*(ORCMUP0011)* ERR    /O   CNT[" CNT-ERR "]"
           DISPLAY "*(ORCMUP0011)* PLF    /O   CNT[" CNT-PLF "]"
           DISPLAY "*(ORCMUP0011)* RC             [" LNK-RC "]"
      **** ACCEPT  WRK-IN
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ更新ファイル読込み処理
      *****************************************************************
       900-IN-KENSAKU-SEC          SECTION.
      *
          READ    IN-FILE
              AT  END
                  MOVE    1               TO  FLG-IN
              NOT AT  END
                  MOVE    ZERO            TO  FLG-IN
                  ADD     1               TO  CNT-IN
          END-READ
      *
      *---(01.00.01) LINE ADD START -------------------------------------
          IF   ( FLG-IN            =     ZERO )
               PERFORM 900-IN-DATA-SET-SEC
          END-IF
      *---(01.00.01) LINE ADD END   -------------------------------------
      *
          .
       900-IN-KENSAKU-EXT.
          EXIT.
      *
      *---(01.00.01) LINE ADD START -------------------------------------
      *****************************************************************
      *    保険番号マスタ変更データ設定処理
      *****************************************************************
       900-IN-DATA-SET-SEC         SECTION.
      *
           EVALUATE    TRUE
           WHEN    LNK-DBRVERSION (3:6)    <   "010003"
               PERFORM 900-HKNWK-V01-SET-SEC
      *
           WHEN    LNK-DBRVERSION (3:6)    <   "020100"
               PERFORM 900-HKNWK-V02-SET-SEC
      *
           WHEN    OTHER
               MOVE    IN-REC      TO  HKNWK-REC
           END-EVALUATE
      *
          .
       900-IN-DATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更後レイアウト移送処理
      *****************************************************************
       900-HKNWK-V01-SET-SEC           SECTION.
      *
           MOVE    IN-REC              TO  HKNWKV01-REC
      *
           INITIALIZE  HKNWK-REC
      *
      *処理区分
           MOVE    HKNWKV01-SYORIKBN   TO  HKNWK-SYORIKBN
      *キー
      *医療機関ＩＤ
           MOVE    HKNWKV01-HOSPID     TO  HKNWK-HOSPID
      *保険番号
           MOVE    HKNWKV01-HKNNUM     TO  HKNWK-HKNNUM
      *適用開始年月日
           MOVE    HKNWKV01-TEKSTYMD   TO  HKNWK-TEKSTYMD
      *支払区分
           MOVE    HKNWKV01-PAYKBN     TO  HKNWK-PAYKBN
      *適用終了年月日
           MOVE    HKNWKV01-TEKEDYMD   TO  HKNWK-TEKEDYMD
      *適用終了年月日２
           MOVE    HKNWKV01-TEKEDYMD2  TO  HKNWK-TEKEDYMD2
      *法別番号
           MOVE    HKNWKV01-HBTNUM     TO  HKNWK-HBTNUM
      *制度名
           MOVE    HKNWKV01-SEIDONAME  TO  HKNWK-SEIDONAME
      *短縮制度名
           MOVE    HKNWKV01-TANSEIDONAME
                                       TO  HKNWK-TANSEIDONAME
      *保険公費種別区分
           MOVE    HKNWKV01-HKNKOHSBTKBN
                                       TO  HKNWK-HKNKOHSBTKBN
      *---(01.00.02) LINE UPD START -------------------------------------
      *法別番号チェック区分
           IF      HKNWK-HBTNUM        =   SPACE
               MOVE    "0"             TO  HKNWK-HBTNUMCHKKBN
           ELSE
               MOVE    "1"             TO  HKNWK-HBTNUMCHKKBN
           END-IF
      *---(01.00.02) LINE UPD END   -------------------------------------
      *検証番号チェック区分
           MOVE    HKNWKV01-KENSNUMCHKKBN
                                       TO  HKNWK-KENSNUMCHKKBN
      *受給者検証番号チェック区分
           MOVE    HKNWKV01-JKYSKENSNUMCHKKBN
                                       TO  HKNWK-JKYSKENSNUMCHKKBN
      *公費主保区分
           MOVE    HKNWKV01-HKNKOHKBN  TO  HKNWK-HKNKOHKBN
      *限定保険番号
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               MOVE    HKNWKV01-GENHKNNUM (IDX)
                                       TO  HKNWK-GENHKNNUM (IDX)
           END-PERFORM
      *条件−年齢開始
           MOVE   HKNWKV01-JOKEN-STYEAR
                                       TO  HKNWK-JOKEN-STYEAR
      *条件−年齢終了
           MOVE   HKNWKV01-JOKEN-EDYEAR
                                       TO  HKNWK-JOKEN-EDYEAR
      *点数単価
           MOVE   HKNWKV01-TENTNK      TO  HKNWK-TENTNK
      *レセプト請求区分
           MOVE   HKNWKV01-RECESKYKBN  TO  HKNWK-RECESKYKBN
      *
      *レセプト１円単位表示区分
           MOVE   "1"                  TO  HKNWK-RECEFTNDSPKBN
      *
      * 
      *レセプト患者負担額判定表示区分 (02.01.00 追加 )
           MOVE   "0"                  TO  HKNWK-RECEJGNDSPKBN
      *
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
      *
      *  外来−負担区分
               MOVE   HKNWKV01-GAIFTNKBN (IDX)
                                       TO  HKNWK-GAIFTNKBN (IDX)
      *  外来−内容
      *    外来−回−負担割合
               MOVE   HKNWKV01-GAIKFTNRATE (IDX)
                                       TO  HKNWK-GAIKFTNRATE (IDX)
      *    外来−回−固定額
               MOVE   HKNWKV01-GAIKKOTGAK (IDX)
                                       TO  HKNWK-GAIKKOTGAK (IDX)
      *    外来−回−上限額
               MOVE   HKNWKV01-GAIKJGNGAK (IDX)
                                       TO  HKNWK-GAIKJGNGAK (IDX)
      *    外来−日−上限額
               MOVE   HKNWKV01-GAIDJGNGAK (IDX)
                                       TO  HKNWK-GAIDJGNGAK (IDX)
      *    外来−日−上限回数
               MOVE   HKNWKV01-GAIDJGNCNT (IDX)
                                       TO  HKNWK-GAIDJGNCNT (IDX)
      *    外来−月−院内−上限額
               MOVE   HKNWKV01-GAIMNAIJGNGAK (IDX)
                                       TO  HKNWK-GAIMNAIJGNGAK (IDX)
      *    外来−月−院外−上限額
               MOVE   HKNWKV01-GAIMGAIJGNGAK (IDX)
                                       TO  HKNWK-GAIMGAIJGNGAK (IDX)
      *    外来−月−上限回数
               MOVE   HKNWKV01-GAIMJGNCNT (IDX)
                                       TO  HKNWK-GAIMJGNCNT (IDX)
      *    外来−薬剤負担区分
               MOVE   HKNWKV01-GAIYKZFTNKBN (IDX)
                                       TO  HKNWK-GAIYKZFTNKBN (IDX)
      *  入院−負担区分
               MOVE   HKNWKV01-NYUFTNKBN (IDX)
                                       TO  HKNWK-NYUFTNKBN (IDX)
      *  入院−内容
      *    入院−回−負担割合
               MOVE   HKNWKV01-NYUKFTNRATE (IDX)
                                       TO  HKNWK-NYUKFTNRATE (IDX)
      *    入院−回−固定額
               MOVE   HKNWKV01-NYUKKOTGAK (IDX)
                                       TO  HKNWK-NYUKKOTGAK (IDX)
      *    入院−回−上限額
               MOVE   HKNWKV01-NYUKJGNGAK (IDX)
                                       TO  HKNWK-NYUKJGNGAK (IDX)
      *    入院−日−上限額
               MOVE   HKNWKV01-NYUDJGNGAK (IDX)
                                       TO  HKNWK-NYUDJGNGAK (IDX)
      *    入院−日−上限回数
               MOVE   HKNWKV01-NYUDJGNCNT (IDX)
                                       TO  HKNWK-NYUDJGNCNT (IDX)
      *    入院−月−上限額
               MOVE   HKNWKV01-NYUMJGNGAK (IDX)
                                       TO  HKNWK-NYUMJGNGAK (IDX)
      *    入院−月−上限回数
               MOVE   HKNWKV01-NYUMJGNCNT (IDX)
                                       TO  HKNWK-NYUMJGNCNT (IDX)
      *    入院−食事療養区分
               MOVE   HKNWKV01-NYUSKJRYYKBN (IDX)
                                       TO  HKNWK-NYUSKJRYYKBN (IDX)
      *    入院−食事負担助成額 (02.01.00 追加 )
               MOVE   ZERO             TO  HKNWK-NYUSKJJYOGAK  (IDX)
           END-PERFORM
      *　　端末ＩＤ
           MOVE   HKNWKV01-TERMID      TO  HKNWK-TERMID
      *　　オペーレータＩＤ
           MOVE   HKNWKV01-OPID        TO  HKNWK-OPID
      *　　作成年月日
           MOVE   HKNWKV01-CREYMD      TO  HKNWK-CREYMD
      *　　更新年月日
           MOVE   HKNWKV01-UPYMD       TO  HKNWK-UPYMD
      *　　更新時間
           MOVE   HKNWKV01-UPHMS       TO  HKNWK-UPHMS
      *
          .
       900-HKNWK-V01-SET-EXT.
           EXIT.
      *
      *---(01.00.01) LINE ADD END   -------------------------------------
      *****************************************************************
      *    変更後レイアウト移送処理
      *****************************************************************
       900-HKNWK-V02-SET-SEC           SECTION.
      *
           MOVE    IN-REC              TO  HKNWKV02-REC
      *
           INITIALIZE  HKNWK-REC
      *
      *処理区分
           MOVE    HKNWKV02-SYORIKBN   TO  HKNWK-SYORIKBN
      *キー
      *医療機関ＩＤ
           MOVE    HKNWKV02-HOSPID     TO  HKNWK-HOSPID
      *保険番号
           MOVE    HKNWKV02-HKNNUM     TO  HKNWK-HKNNUM
      *適用開始年月日
           MOVE    HKNWKV02-TEKSTYMD   TO  HKNWK-TEKSTYMD
      *支払区分
           MOVE    HKNWKV02-PAYKBN     TO  HKNWK-PAYKBN
      *適用終了年月日
           MOVE    HKNWKV02-TEKEDYMD   TO  HKNWK-TEKEDYMD
      *適用終了年月日２
           MOVE    HKNWKV02-TEKEDYMD2  TO  HKNWK-TEKEDYMD2
      *法別番号
           MOVE    HKNWKV02-HBTNUM     TO  HKNWK-HBTNUM
      *制度名
           MOVE    HKNWKV02-SEIDONAME  TO  HKNWK-SEIDONAME
      *短縮制度名
           MOVE    HKNWKV02-TANSEIDONAME
                                       TO  HKNWK-TANSEIDONAME
      *保険公費種別区分
           MOVE    HKNWKV02-HKNKOHSBTKBN
                                       TO  HKNWK-HKNKOHSBTKBN
      *法別番号チェック区分
           MOVE    HKNWKV02-HBTNUMCHKKBN
                                       TO  HKNWK-HBTNUMCHKKBN
      *検証番号チェック区分
           MOVE    HKNWKV02-KENSNUMCHKKBN
                                       TO  HKNWK-KENSNUMCHKKBN
      *受給者検証番号チェック区分
           MOVE    HKNWKV02-JKYSKENSNUMCHKKBN
                                       TO  HKNWK-JKYSKENSNUMCHKKBN
      *公費主保区分
           MOVE    HKNWKV02-HKNKOHKBN  TO  HKNWK-HKNKOHKBN
      *限定保険番号
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 3
               MOVE    HKNWKV02-GENHKNNUM (IDX)
                                       TO  HKNWK-GENHKNNUM (IDX)
           END-PERFORM
      *条件−年齢開始
           MOVE   HKNWKV02-JOKEN-STYEAR
                                       TO  HKNWK-JOKEN-STYEAR
      *条件−年齢終了
           MOVE   HKNWKV02-JOKEN-EDYEAR
                                       TO  HKNWK-JOKEN-EDYEAR
      *点数単価
           MOVE   HKNWKV02-TENTNK      TO  HKNWK-TENTNK
      *レセプト請求区分
           MOVE   HKNWKV02-RECESKYKBN  TO  HKNWK-RECESKYKBN
      *
      *レセプト１円単位表示区分
           MOVE   HKNWKV02-RECEFTNDSPKBN
                                       TO  HKNWK-RECEFTNDSPKBN
      *
      *レセプト患者負担額判定表示区分 (02.01.00 追加 )
           MOVE   "0"                  TO  HKNWK-RECEJGNDSPKBN
      *
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 4
      *
      *  外来−負担区分
               MOVE   HKNWKV02-GAIFTNKBN (IDX)
                                       TO  HKNWK-GAIFTNKBN (IDX)
      *  外来−内容
      *    外来−回−負担割合
               MOVE   HKNWKV02-GAIKFTNRATE (IDX)
                                       TO  HKNWK-GAIKFTNRATE (IDX)
      *    外来−回−固定額
               MOVE   HKNWKV02-GAIKKOTGAK (IDX)
                                       TO  HKNWK-GAIKKOTGAK (IDX)
      *    外来−回−上限額
               MOVE   HKNWKV02-GAIKJGNGAK (IDX)
                                       TO  HKNWK-GAIKJGNGAK (IDX)
      *    外来−日−上限額
               MOVE   HKNWKV02-GAIDJGNGAK (IDX)
                                       TO  HKNWK-GAIDJGNGAK (IDX)
      *    外来−日−上限回数
               MOVE   HKNWKV02-GAIDJGNCNT (IDX)
                                       TO  HKNWK-GAIDJGNCNT (IDX)
      *    外来−月−院内−上限額
               MOVE   HKNWKV02-GAIMNAIJGNGAK (IDX)
                                       TO  HKNWK-GAIMNAIJGNGAK (IDX)
      *    外来−月−院外−上限額
               MOVE   HKNWKV02-GAIMGAIJGNGAK (IDX)
                                       TO  HKNWK-GAIMGAIJGNGAK (IDX)
      *    外来−月−上限回数
               MOVE   HKNWKV02-GAIMJGNCNT (IDX)
                                       TO  HKNWK-GAIMJGNCNT (IDX)
      *    外来−薬剤負担区分
               MOVE   HKNWKV02-GAIYKZFTNKBN (IDX)
                                       TO  HKNWK-GAIYKZFTNKBN (IDX)
      *  入院−負担区分
               MOVE   HKNWKV02-NYUFTNKBN (IDX)
                                       TO  HKNWK-NYUFTNKBN (IDX)
      *  入院−内容
      *    入院−回−負担割合
               MOVE   HKNWKV02-NYUKFTNRATE (IDX)
                                       TO  HKNWK-NYUKFTNRATE (IDX)
      *    入院−回−固定額
               MOVE   HKNWKV02-NYUKKOTGAK (IDX)
                                       TO  HKNWK-NYUKKOTGAK (IDX)
      *    入院−回−上限額
               MOVE   HKNWKV02-NYUKJGNGAK (IDX)
                                       TO  HKNWK-NYUKJGNGAK (IDX)
      *    入院−日−上限額
               MOVE   HKNWKV02-NYUDJGNGAK (IDX)
                                       TO  HKNWK-NYUDJGNGAK (IDX)
      *    入院−日−上限回数
               MOVE   HKNWKV02-NYUDJGNCNT (IDX)
                                       TO  HKNWK-NYUDJGNCNT (IDX)
      *    入院−月−上限額
               MOVE   HKNWKV02-NYUMJGNGAK (IDX)
                                       TO  HKNWK-NYUMJGNGAK (IDX)
      *    入院−月−上限回数
               MOVE   HKNWKV02-NYUMJGNCNT (IDX)
                                       TO  HKNWK-NYUMJGNCNT (IDX)
      *    入院−食事療養区分
               MOVE   HKNWKV02-NYUSKJRYYKBN (IDX)
                                       TO  HKNWK-NYUSKJRYYKBN (IDX)
      *    入院−食事負担助成額 (02.01.00 追加 )
               MOVE   ZERO             TO  HKNWK-NYUSKJJYOGAK  (IDX)
           END-PERFORM
      *　　端末ＩＤ
           MOVE   HKNWKV02-TERMID      TO  HKNWK-TERMID
      *　　オペーレータＩＤ
           MOVE   HKNWKV02-OPID        TO  HKNWK-OPID
      *　　作成年月日
           MOVE   HKNWKV02-CREYMD      TO  HKNWK-CREYMD
      *　　更新年月日
           MOVE   HKNWKV02-UPYMD       TO  HKNWK-UPYMD
      *　　更新時間
           MOVE   HKNWKV02-UPHMS       TO  HKNWK-UPHMS
      *
          .
       900-HKNWK-V02-SET-EXT.
           EXIT.
      *
      *---(01.00.01) LINE ADD END   -------------------------------------
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       910-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE                      HKNNUM-REC
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           .
       910-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスタ読み込み
      *****************************************************************
       800-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM    900-CLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
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
