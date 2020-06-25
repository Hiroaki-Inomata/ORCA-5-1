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
       PROGRAM-ID.             ORCBINF1.
      *****************************************************************
      *  ★修正時には必ずプログラム識別番号をリセットすること
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定点調査
      *  コンポーネント名  : 感染症サーベイランス報告データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/11/30    NACL−伊藤    新規作成
      *  11/01/31    NACL−伊藤    新レイアウト対応
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-伊藤    10/ 2/ 8  数字編集小数点見直し
      *  04.05.00    NACL-伊藤    10/10/26  判定条件見直し
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    感染症データ
           SELECT  INFEC-FILE      ASSIGN  WRK-PARA-INFECFILE
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-INFEC.
      *
      *    エラーファイル
           SELECT  ERR-FILE        ASSIGN  WRK-PARA-ERRFILE
                                   FILE    STATUS  IS  STS-ERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    感染症データ
       FD  INFEC-FILE.
       01  INFEC-REC               PIC X(3000).
      *
      *    エラーファイル
       FD  ERR-FILE.
       01  ERR-REC                 PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *01  PRGSKBNO                PIC X(08)  VALUE "20091218".
      *01  PRGSKBNO                PIC X(08)  VALUE "20101026".
      *01  PRGSKBNO                PIC X(08)  VALUE "20101125".
      *    レイアウト変更（リアルタイム導入）
      *01  PRGSKBNO                PIC X(08)  VALUE "20110201".
      *    レイアウト変更（データ作成区分）
      *01  PRGSKBNO                PIC X(08)  VALUE "20110520".
      *    レイアウト変更（属性区分）
      *01  PRGSKBNO                PIC X(08)  VALUE "20120706".
      *    データ作成区分に空白がセットされる修正
       01  PRGSKBNO                PIC X(08)  VALUE "20130425".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-INFEC                       PIC X(02).
           03  STS-ERR                         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-SYSKANRI                    PIC 9(01).
           03  FLG-PTINF                       PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-SRYACCT                     PIC 9(01).
           03  FLG-JYURRK                      PIC 9(01).
           03  FLG-PTBYOMEI                    PIC 9(01).
           03  FLG-INFECTION                   PIC 9(01).
           03  FLG-SRYACCT-READ                PIC 9(01).
           03  FLG-P-REC                       PIC 9(01).
           03  FLG-TBL-INFECT                  PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-TENSUPLUS                   PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                          PIC 9(06).
           03  CNT-INFEC                       PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDT                             PIC 9(05).
           03  IDU                             PIC 9(05).
           03  IDV                             PIC 9(05).
           03  IDW                             PIC 9(05).
           03  IDX                             PIC 9(05).
           03  IDY                             PIC 9(05).
           03  IDY1                            PIC 9(05).
           03  IDZ                             PIC 9(05).
      *
       01  KEY-AREA                            VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID                  PIC 9(10).
           03  KEY-OLD.
               05  KEY-O-PTID                  PIC 9(10).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-SRYYMD             PIC X(08).
               05  WRK-PARA-NYUGAIKBN          PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-INFECFILE          PIC X(128).
               05  WRK-PARA-ERRFILE            PIC X(128).
               05  WRK-PARA-VERSION            PIC X(100).
      *
           03  WRK-HENSYU-AREA.
               05  WRK-SRYYMD                  PIC 9(08).
               05  WRK-SRYKBN                  PIC X(02).
               05  WRK-SRYCD                   PIC X(09).
               05  WRK-SURYO                   PIC 9(05)V9(05).
               05  WRK-ZAIKAISU                PIC 9(03).
               05  WRK-CDKBN                   PIC 9(01).
               05  WRK-BYOMEICD                PIC X(07).
               05  WRK-UTAGAIFLG               PIC X(01).
               05  WRK-BYOSRYYMD               PIC 9(08).
               05  WRK-TENKIKBN                PIC X(01).
               05  WRK-TENKIYMD                PIC 9(08).
               05  WRK-DLTFLG                  PIC X(01).
               05  WRK-BIRTHDAY                PIC 9(08).
               05  WRK-BIRTHDAY-R          REDEFINES   WRK-BIRTHDAY.
                   07  WRK-BIRTHDAY-YM         PIC 9(06).
                   07  WRK-BIRTHDAY-D          PIC 9(02).
               05  WRK-ATTR                    PIC 9(02).
      *
           03  WRK-SAVE-AREA.
               05  WRK-S-KHNBYOMEICD           PIC X(07).
               05  WRK-S-SRYYMD                PIC 9(08).
      *
      *    出力データ編集用
           03  WRK-REC                         PIC X(3000).
           03  WRK-REC-LENGTH                  PIC 9(04).
           03  WRK-DATA-HENSYU-AREA.
               05  WRK-DATA                    PIC X(200).
               05  WRK-NUM                     PIC ZZZZZZZZZ9.99999.
               05  WRK-NAGASA                  PIC 9(03).
               05  WRK-ST                      PIC 9(02).
               05  WRK-SYOSUU                  PIC 9(01).
               05  WRK-END                     PIC 9(01).
      *
           03  WRK-RECKBN                      PIC 9(01).
           03  WRK-INFECERR                    PIC X(200).
           03  WRK-ERRMSG                      PIC X(300).
           03  WRK-SRYACT-MAX                  PIC 9(03).
      *
       01  TBL-INFECT-AREA.
           03  TBL-INFECT-OCC      OCCURS  100  INDEXED  I-S.
               05  TBL-INFECT-DISEASE          PIC 9(02).
               05  TBL-INFECT-SCD              PIC X(09).
               05  TBL-INFECT-YCD              PIC X(09).
               05  TBL-INFECT-BCD              PIC X(07).
               05  TBL-INFECT-BSRYYMD          PIC 9(08).
               05  TBL-INFECT-TENKIKBN         PIC X(01).
               05  TBL-INFECT-TENKIYMD         PIC 9(08).
           03  TBL-INFECT-MAX             PIC 9(03).
      *
       01  TBL-DISEASE-AREA.
           03  TBL-DISEASE-OCC     OCCURS   99  INDEXED  I-D.
               05  TBL-DISEASE            PIC 9(02).
           03  TBL-DISEASE-MAX            PIC 9(02).
      *
      * 2012.7 テーブル数 100->10000 へ増加
       01  TBL-JUDGMENT-AREA.
           03  TBL-JUDGMENT-B-OCC  OCCURS  10000  INDEXED  I-J-B.
               05  TBL-JUDGMENT-BYOMEICD  PIC X(07).
           03  TBL-JUDGMENT-B-MAX         PIC 9(05).
           03  TBL-JUDGMENT-S-OCC  OCCURS  10000  INDEXED  I-J-S.
               05  TBL-JUDGMENT-SRYCD     PIC X(09).
               05  TBL-JUDGMENT-CDKBN     PIC 9(01).
           03  TBL-JUDGMENT-S-MAX         PIC 9(05).
      *
       01  TBL-SRYACT-AREA.
      *    診療種別区分
           03  TBL-SRY-SRYSYUKBN           PIC  X(03).
      *    診療区分
           03  TBL-SRY-SRYKBN              PIC  X(02).
           03  TBL-SRYACT-AREA-G   OCCURS  100.
      *        診療コード
               05  TBL-SRY-SRYCD           PIC  X(09).
      *        数量
               05  TBL-SRY-SRYSURYO        PIC  9(05)V9(05).
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    医療機関情報
           COPY    "CPSK1002.INC".
      *
      *    帳票編集区分情報
           COPY    "CPSK1030.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    感染症プルーフ
       01  INFECTION-REC.
           COPY    "CPINFECTION.INC".
      *
      *    判定
       01  INFJUDGMENT-REC.
           COPY    "CPINFJUDGMENT.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE                   DIVISION
                                   USING   COMMAND-PARAM.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL  (FLG-END =   1)  OR
                                          (FLG-JYURRK =   1)
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SRYYMD
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-INFECFILE
                                               WRK-PARA-ERRFILE
                                               WRK-PARA-VERSION
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           PERFORM 900-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBINF1"          TO  JOB-PGID
           MOVE    "感染症データ作成"  TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    医療機関基本情報1001
           INITIALIZE                  SYS-1001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    WRK-PARA-SRYYMD     TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "医療機関情報が取得できませんでした。"
                                           TO  WRK-INFECERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           IF      SYS-1001-DATAINFECTIONKBN   =   SPACE
               IF      SYS-1001-DATACREATEFLG  =   "1"
                   MOVE    "1"         TO  SYS-1001-DATAINFECTIONKBN
               ELSE
                   MOVE    "0"         TO  SYS-1001-DATAINFECTIONKBN
               END-IF
           END-IF
      *
      *    医療機関基本情報1002
           INITIALIZE                      SYS-1002-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1002-HOSPNUM
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    WRK-PARA-SRYYMD     TO  SYS-1002-STYUKYMD
                                           SYS-1002-EDYUKYMD
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               INITIALIZE                      SYS-1002-REC
           END-IF
      *
      *    帳票編集区分情報1030
           INITIALIZE                      SYS-1030-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    WRK-PARA-SRYYMD     TO  SYS-1030-STYUKYMD
                                           SYS-1030-EDYUKYMD
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1030-REC
           ELSE
               INITIALIZE                  SYS-1030-REC
           END-IF
      *
           OPEN    OUTPUT              INFEC-FILE
      *
      *    受診履歴読み込み
           INITIALIZE                  JYURRK-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JYURRK-HOSPNUM
           MOVE    WRK-PARA-SRYYMD     TO  JYURRK-SRYYMD
           MOVE    WRK-PARA-SRYYMD     TO  JYURRK-UPSRYYMD
           MOVE    WRK-PARA-NYUGAIKBN  TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
      *
      *    判定テーブルから病原体区分のサマリをテーブルに取得
           PERFORM 900-DISEASE-SET-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-INIT-SYORI-SEC
           PERFORM UNTIL  (FLG-JYURRK         =   1)
                      OR  (KEY-NEW        NOT =   KEY-OLD )
                      OR  (PTINF-TSTPTNUMKBN  =   "1")
               PERFORM 2002-HENSYU-SYORI-SEC
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者毎の編集処理
      *****************************************************************
       2001-INIT-SYORI-SEC         SECTION.
      *
           INITIALIZE                      WRK-HENSYU-AREA
           INITIALIZE                      WRK-SAVE-AREA
           INITIALIZE                      TBL-INFECT-AREA
           MOVE    WRK-PARA-SRYYMD     TO  WRK-SRYYMD
           MOVE    ZERO                TO  FLG-P-REC
      *    テスト患者のときは読み飛ばす
           MOVE    JYURRK-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    JYURRK-PTID         TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *    テスト患者の時読み飛ばし
           IF     (FLG-PTINF           =   ZERO)  AND
                  (PTINF-TSTPTNUMKBN   =   "1" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM UNTIL ( FLG-JYURRK      =   1 )
                       OR    ( KEY-NEW     NOT =   KEY-OLD )
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key21"             TO  MCP-PATHNAME
                   PERFORM 900-JYURRK-READ-SEC
               END-PERFORM
           ELSE
               MOVE    KEY-NEW             TO  KEY-OLD
           END-IF
           .
       2001-INIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       2002-HENSYU-SYORI-SEC       SECTION.
      *
           MOVE    KEY-NEW             TO  KEY-OLD
           PERFORM UNTIL ( FLG-JYURRK      =   1 )
                   OR    ( KEY-NEW     NOT =   KEY-OLD )
               PERFORM 20021-INFECTION-SEC
           END-PERFORM
           .
       2002-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    編集処理
      *****************************************************************
       20021-INFECTION-SEC         SECTION.
      *
      *    受診履歴の内容判定
           PERFORM UNTIL   ( FLG-JYURRK    =   1 )  OR
                           ( KEY-NEW   NOT =   KEY-OLD )
               PERFORM 200211-SRYCD-HEN-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key21"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           END-PERFORM
      *
           PERFORM VARYING I-D     FROM  1  BY  1
                   UNTIL   I-D  >  TBL-DISEASE-MAX
      *        判定テーブルから判定条件をテーブルに取得
               PERFORM 900-JUDGMENT-SET-SEC
      *        傷病名の内容判定
               PERFORM 20022-BYOMEI-HENSYU-SEC
           END-PERFORM
      *
           PERFORM VARYING IDT  FROM  1  BY  1
                   UNTIL  (IDT  >   TBL-INFECT-MAX)  OR
                          (IDT  >   100)
      *        感染症プルーフ読み込み
               MOVE    IDT                 TO  I-S
               PERFORM 2004-INFECTION-READ-SEC
               IF      FLG-INFECTION       =   1
                   PERFORM 2004-INFECTION-INSERT-SEC
               ELSE
                   PERFORM 2004-INFECTION-UPDATE-SEC
               END-IF
           END-PERFORM
           .
       20021-INFECTION-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       200211-SRYCD-HEN-SEC        SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15)  OR
                              (JYURRK-ZAINUM(IDX)  =   ZERO)
      *
               MOVE    ZERO                TO  FLG-SRYACCT-READ
               MOVE    ZERO                TO  IDY
               INITIALIZE                      TBL-SRYACT-AREA
               MOVE    SPACE               TO  SRYACT-REC
               INITIALIZE                      SRYACT-REC
               MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  SRY-PTID
               MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
               MOVE    JYURRK-ZAINUM(IDX)  TO  SRY-ZAINUM
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               ELSE
                   INITIALIZE                  SRYACT-REC
                   MOVE    1                   TO  FLG-SRYACT
               END-IF
               PERFORM UNTIL FLG-SRYACT    =   1
                   PERFORM 20011-SRYACT-SAVE-SEC
                   MOVE    "tbl_sryact"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SRYACT-READ-SEC
               END-PERFORM
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               PERFORM VARYING I-D     FROM  1  BY  1
                       UNTIL   I-D  >  TBL-DISEASE-MAX
      *            判定テーブルから判定条件をテーブルに取得
                   PERFORM 900-JUDGMENT-SET-SEC
                   PERFORM 20012-SRYCD-JUDGE-SEC
               END-PERFORM
           END-PERFORM
           .
       200211-SRYCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       20011-SRYACT-SAVE-SEC       SECTION.
      *
           IF      IDY                 =   ZERO
               MOVE    SRY-SRYSYUKBN       TO  TBL-SRY-SRYSYUKBN
               MOVE    SRY-SRYKBN          TO  TBL-SRY-SRYKBN
           END-IF
      *
           PERFORM VARYING IDY1  FROM  1  BY  1
                   UNTIL  (IDY1            >   5)
                      OR  (SRY-SRYCD (IDY1) =   SPACE)
               COMPUTE IDY                 =   IDY  +  1
               IF      IDY                 >   100
                   DISPLAY "TBL-SRYACT-AREA TABLE OVER !!"
               END-IF
               MOVE    SRY-SRYCD (IDY1)    TO  TBL-SRY-SRYCD (IDY)
               MOVE    SRY-SRYSURYO (IDY1) TO  TBL-SRY-SRYSURYO (IDY)
           END-PERFORM
           MOVE    IDY                 TO  WRK-SRYACT-MAX
           .
       20011-SRYACT-SAVE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為判定処理
      *****************************************************************
       20012-SRYCD-JUDGE-SEC       SECTION.
      *
           PERFORM VARYING IDY  FROM  1  BY  1
                   UNTIL  (IDY             >   WRK-SRYACT-MAX)
                      OR  (TBL-SRY-SRYCD (IDY) =   SPACE)
               PERFORM VARYING I-J-S  FROM  1  BY  1
                       UNTIL   I-J-S  >  TBL-JUDGMENT-S-MAX
                   IF      TBL-SRY-SRYCD (IDY) =   TBL-JUDGMENT-SRYCD
                                                   (I-J-S)
                       MOVE    TBL-JUDGMENT-CDKBN (I-J-S)
                                                   TO  WRK-CDKBN
                       MOVE    TBL-SRY-SRYKBN      TO  WRK-SRYKBN
                       MOVE    TBL-SRY-SRYCD (IDY) TO  WRK-SRYCD
                       MOVE    TBL-SRY-SRYSURYO (IDY)
                                                   TO  WRK-SURYO
                       IF      FLG-SRYACCT-READ    =   ZERO
                           PERFORM 900-SRYACCT-READ-SEC
                           MOVE    1               TO  FLG-SRYACCT-READ
                           IF      FLG-SRYACCT         =   ZERO
                               MOVE    ACCT-ZAIKAISU   TO  WRK-ZAIKAISU
                           ELSE
                               MOVE    ZERO            TO  WRK-ZAIKAISU
                           END-IF
                       END-IF
      *
                       IF     (WRK-CDKBN           =   2)  AND
                              (WRK-SRYKBN          =   "21" OR "22"
                                                            OR "23")
                           PERFORM 20013-SYOHO-HENSYU-SEC
                       ELSE
                           MOVE    ZERO                TO  WRK-ATTR
                       END-IF
      *
                       MOVE    2                   TO  WRK-RECKBN
                       PERFORM 2003-OUT-SYORI-SEC
                       PERFORM 2003-TBL-INFECT-SEC
                       MOVE    TBL-JUDGMENT-S-MAX  TO  I-J-S
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       20012-SRYCD-JUDGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内・院外別、銘柄名・一般名記載決定
      *****************************************************************
       20013-SYOHO-HENSYU-SEC          SECTION.
      *
      *    院内、院外の区別
           IF     (ACCT-ZAITEN     NOT =   ZERO)  OR
                  (TBL-SRY-SRYSYUKBN   =   "213" OR "223" OR "233")
      *        院内処方
               MOVE    1                   TO  WRK-ATTR
           ELSE
      *        院外処方（銘柄名）
               MOVE    2                   TO  WRK-ATTR
           END-IF
      *
           IF      WRK-ATTR            =   1
               GO  TO  20013-SYOHO-HENSYU-EXT
           END-IF
      *
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数附加情報検索
           IF     (WRK-SRYCD(1:1)      =   "6" )  AND
                  (FLG-TENSU           =   ZERO)
               PERFORM 900-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
      *
           IF     (WRK-SRYCD(1:1)      =   "6" )  AND
                  (FLG-TENSU           =   ZERO)
      *        システム管理の変更可によって一般名記載とする
               IF     (TNSP-IPN-KISAIKBN       =   ZERO    )  AND
                      (SYS-1030-KOUHATUKA      =   "1"     )  AND
                      (TNS-KOUHATUKBN          =   1  OR  2
                                                      OR  7)
                   MOVE    1               TO  TNSP-IPN-KISAIKBN
               END-IF
           END-IF
      *
      *    システム予約により銘柄名へ変更か調べる
           PERFORM 20013-SYOHO-HENSYU-2-SEC
      *
           IF      TNSP-IPN-KISAIKBN   =   1  OR  3
               MOVE    3                   TO  WRK-ATTR
           END-IF
           .
       20013-SYOHO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為データを読みシステム予約マスタが入力されていないか
      *    調べる
      *****************************************************************
       20013-SYOHO-HENSYU-2-SEC    SECTION.
      *
           COMPUTE IDY1                =   IDY  +  1
      *
           IF      TBL-SRY-SRYCD (IDY1)    =   SPACE
               GO  TO  20013-SYOHO-HENSYU-2-EXT
           END-IF
      *
           EVALUATE    TBL-SRY-SRYCD (IDY1)
               WHEN    "099209903"
      *            後発医薬品へ変更不可
                   MOVE    ZERO                TO  TNSP-IPN-KISAIKBN
      *        WHEN    "099209905"
      *            含量規格変更不可
      *        WHEN    "099209906"
      *            剤型変更不可
               WHEN    "099209907"
      *            銘柄名記載
                   MOVE    ZERO                TO  TNSP-IPN-KISAIKBN
               WHEN    "099209908"
      *            一般名記載
                   MOVE    1                   TO  TNSP-IPN-KISAIKBN
               WHEN    OTHER
                   CONTINUE
           END-EVALUATE
           .
       20013-SYOHO-HENSYU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名情報編集処理
      *****************************************************************
       20022-BYOMEI-HENSYU-SEC     SECTION.
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    PTINF-PTID          TO  PTBYO-PTID
           MOVE    WRK-PARA-SRYYMD     TO  PTBYO-UPYMD
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key44"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
      *
           PERFORM UNTIL    FLG-PTBYOMEI   =   1
               PERFORM 200221-BYOMEI-HENSYU2-SEC
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key44"             TO  MCP-PATHNAME
               PERFORM 900-PTBYO-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key44"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       20022-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名編集処理２
      *****************************************************************
       200221-BYOMEI-HENSYU2-SEC   SECTION.
      *
      *    基本病名コード
           IF      PTBYO-KHNBYOMEICD   =   SPACE
               GO  TO  200221-BYOMEI-HENSYU2-EXT
           END-IF
      *
      *    診療開始日
      *    診療開始日は該当日と同一の場合を対象とするよう変更
      **     IF      PTBYO-SRYYMD    NOT =   WRK-PARA-SRYYMD
      **         GO  TO  200221-BYOMEI-HENSYU2-EXT
      **     END-IF
      *
           PERFORM VARYING     I-J-B   FROM    1   BY  1
                   UNTIL      (I-J-B   >  TBL-JUDGMENT-B-MAX)
               IF      PTBYO-KHNBYOMEICD   =   TBL-JUDGMENT-BYOMEICD
                                               (I-J-B)
                   IF      PTBYO-KHNBYOMEICD   =   WRK-S-KHNBYOMEICD
                     AND   PTBYO-SRYYMD        =   WRK-S-SRYYMD
                     AND   PTBYO-DLTFLG        =   1
                           CONTINUE
                   ELSE
                       MOVE    PTBYO-KHNBYOMEICD   TO  WRK-S-KHNBYOMEICD
                       MOVE    PTBYO-SRYYMD        TO  WRK-S-SRYYMD
                       MOVE    PTBYO-KHNBYOMEICD   TO  WRK-BYOMEICD
                       MOVE    PTBYO-UTAGAIFLG     TO  WRK-UTAGAIFLG
                       MOVE    PTBYO-SRYYMD        TO  WRK-BYOSRYYMD
                       MOVE    PTBYO-SRYYMD        TO  WRK-SRYYMD
                       MOVE    PTBYO-TENKIKBN      TO  WRK-TENKIKBN
                       MOVE    PTBYO-TENKIYMD      TO  WRK-TENKIYMD
                       MOVE    PTBYO-DLTFLG        TO  WRK-DLTFLG
                       MOVE    1                   TO  WRK-RECKBN
                       PERFORM 2003-OUT-SYORI-SEC
                       PERFORM 2003-TBL-INFECT-SEC
                       MOVE    TBL-JUDGMENT-B-MAX  TO  I-J-B
                   END-IF
               END-IF
           END-PERFORM
           .
       200221-BYOMEI-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為判定処理
      *****************************************************************
       2003-TBL-INFECT-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-TBL-INFECT
      *
           PERFORM VARYING IDT  FROM  1  BY  1
                   UNTIL  (IDT  >   TBL-INFECT-MAX)  OR
                          (IDT  >   100)
               IF      TBL-DISEASE (I-D)   =   TBL-INFECT-DISEASE (IDT)
                   MOVE    1                   TO  FLG-TBL-INFECT
                   MOVE    IDT                 TO  I-S
                   MOVE    100                 TO  IDT
               END-IF
           END-PERFORM
      *
           IF      FLG-TBL-INFECT      =   ZERO
               IF      TBL-INFECT-MAX      =   100
                   GO  TO  2003-TBL-INFECT-EXT
               END-IF
               COMPUTE TBL-INFECT-MAX      =   TBL-INFECT-MAX  +  1
               MOVE    TBL-INFECT-MAX      TO  I-S
               MOVE    TBL-DISEASE (I-D)   TO  TBL-INFECT-DISEASE (I-S)
           END-IF
      *
           IF      WRK-RECKBN          =   1
               IF      TBL-INFECT-BCD (I-S)    =   SPACE
                   MOVE    WRK-BYOMEICD        TO  TBL-INFECT-BCD (I-S)
                   MOVE    WRK-BYOSRYYMD
                                       TO  TBL-INFECT-BSRYYMD (I-S)
                   MOVE    WRK-TENKIKBN
                                       TO  TBL-INFECT-TENKIKBN (I-S)
                   MOVE    WRK-TENKIYMD
                                       TO  TBL-INFECT-TENKIYMD (I-S)
               END-IF
           END-IF
      *
           IF      WRK-RECKBN          =   2
               IF      WRK-CDKBN           =   2
                   IF      TBL-INFECT-YCD (I-S)    =   SPACE
                       MOVE    WRK-SRYCD       TO  TBL-INFECT-YCD (I-S)
                   END-IF
               END-IF
               IF      WRK-CDKBN           =   3
                   IF      TBL-INFECT-SCD (I-S)    =   SPACE
                       MOVE    WRK-SRYCD       TO  TBL-INFECT-SCD (I-S)
                   END-IF
               END-IF
           END-IF
           .
       2003-TBL-INFECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       2003-OUT-SYORI-SEC          SECTION.
      *
           IF      CNT-INFEC           =   ZERO
               PERFORM 400-I-REC-SEC
           END-IF
      *
           IF      FLG-P-REC           =   ZERO
               PERFORM 400-P-REC-SEC
               MOVE    1                   TO  FLG-P-REC
           END-IF
      *
           IF      WRK-RECKBN          =   1
               PERFORM 400-B-REC-SEC
           END-IF
      *
           IF      WRK-RECKBN          =   2
               PERFORM 400-SY-REC-SEC
           END-IF
      *
      *    IF      STS-INFEC           =   "00"
      *        CONTINUE
      *    ELSE
      *        MOVE    SPACE               TO  WRK-INFECERR
      *        STRING "感染症データ 書き込みエラー STS="
      *                                        DELIMITED  BY  SIZE
      *                STS-INFEC               DELIMITED  BY  SIZE
      *                                        INTO    WRK-INFECERR
      *        END-STRING
      *        PERFORM 500-ERR-HENSYU-SEC
      *        PERFORM 500-COBABORT-SEC
      *    END-IF
           .
       2003-OUT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ処理
      *****************************************************************
       2004-INFECTION-READ-SEC     SECTION.
      *
           INITIALIZE                      INFECTION-REC
           MOVE    SPA-HOSPNUM         TO  INFECT-HOSPNUM
           MOVE    SMCNDATE-YMD        TO  INFECT-SENDYMD
           MOVE    PTINF-PTID          TO  INFECT-PTID
           MOVE    TBL-INFECT-DISEASE (I-S)
                                       TO  INFECT-DISEASE
           MOVE    WRK-PARA-SRYYMD     TO  INFECT-SRYYMD
      *
           MOVE    INFECTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_infection"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-INFECTION-READ-SEC
           ELSE
               INITIALIZE                      INFECTION-REC
               MOVE    1                   TO  FLG-INFECTION
           END-IF
      *
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       2004-INFECTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ処理
      *****************************************************************
       2004-INFECTION-INSERT-SEC   SECTION.
      *
           INITIALIZE                      INFECTION-REC
           MOVE    SPA-HOSPNUM         TO  INFECT-HOSPNUM
           MOVE    SMCNDATE-YMD        TO  INFECT-SENDYMD
           MOVE    PTINF-PTID          TO  INFECT-PTID
           MOVE    TBL-INFECT-DISEASE (I-S)
                                       TO  INFECT-DISEASE
           MOVE    WRK-PARA-SRYYMD     TO  INFECT-SRYYMD
      *
           MOVE    TBL-INFECT-SCD (I-S)    TO  INFECT-SRYCD
           MOVE    TBL-INFECT-YCD (I-S)    TO  INFECT-MEDSRYCD
           MOVE    TBL-INFECT-BCD (I-S)    TO  INFECT-BYOMEICD
           IF      TBL-INFECT-BCD (I-S)
                                   NOT =   SPACE
               MOVE    TBL-INFECT-BSRYYMD (I-S)
                                           TO  INFECT-BYOSRYYMD
               MOVE    TBL-INFECT-TENKIKBN (I-S)
                                           TO  INFECT-TENKIKBN
               MOVE    TBL-INFECT-TENKIYMD (I-S)
                                           TO  INFECT-TENKIYMD
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  INFECT-CREYMD
           MOVE    SPACE               TO  INFECT-UPYMD
           MOVE    SMCNDATE-HMS        TO  INFECT-UPHMS
      *
           MOVE    INFECTION-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCBINF1 INFECTION INS ERR:"  MCP-RC  "."
           END-IF
           .
       2004-INFECTION-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症プルーフ処理
      *****************************************************************
       2004-INFECTION-UPDATE-SEC   SECTION.
      *
      *    INITIALIZE                      INFECTION-REC
      *    MOVE    SPA-HOSPNUM         TO  INFECT-HOSPNUM
      *    MOVE    SMCNDATE-YMD        TO  INFECT-SENDYMD
      *    MOVE    PTINF-PTID          TO  INFECT-PTID
      *    MOVE    TBL-DISEASE (I-D)   TO  INFECT-DISEASE
      *    MOVE    WRK-SRYYMD          TO  INFECT-SRYYMD
      *
      *    病名
           IF      TBL-INFECT-BCD (I-S)
                                       NOT =   SPACE
               MOVE    TBL-INFECT-BCD (I-S)
                                           TO  INFECT-BYOMEICD
               MOVE    TBL-INFECT-BSRYYMD (I-S)
                                           TO  INFECT-BYOSRYYMD
               MOVE    TBL-INFECT-TENKIKBN (I-S)
                                           TO  INFECT-TENKIKBN
               MOVE    TBL-INFECT-TENKIYMD (I-S)
                                           TO  INFECT-TENKIYMD
           END-IF
      *    診療行為
           MOVE    TBL-INFECT-SCD (I-S)    TO  INFECT-SRYCD
           MOVE    TBL-INFECT-YCD (I-S)    TO  INFECT-MEDSRYCD
      *
           MOVE    SMCNDATE-YMD        TO  INFECT-CREYMD
           MOVE    SPACE               TO  INFECT-UPYMD
           MOVE    SMCNDATE-HMS        TO  INFECT-UPHMS
      *
           MOVE    INFECTION-REC       TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_infection"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "ORCBINF2 INFECTION INS ERR:"  MCP-RC  "."
           END-IF
           .
       2004-INFECTION-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key21"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE   INFEC-FILE
      *
      **   IF      CNT-INFEC           =   ZERO
      **       MOVE    "処理対象のデータがありませんでした"
      **                                   TO  WRK-INFECERR
      **       PERFORM 500-ERR-HENSYU-SEC
      **   END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "ORCBINF1 OUTPUT CNT : " CNT-INFEC
           DISPLAY "*** ORCBINF1 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（医療機関情報）作成処理
      *****************************************************************
       400-I-REC-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           MOVE    "I"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    都道府県
           MOVE    SYS-1001-PREFNUM    TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    データ作成年月日（マシン日付）
           MOVE    SMCNDATE-YMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    データ作成時間（マシン時間）
           MOVE    SMCNDATE-HMS        TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    医療機関ＩＤ
           MOVE    SYS-1001-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    プログラム識別番号
           MOVE    PRGSKBNO            TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    郵便番号
           MOVE    SYS-1002-POST       TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    日レセバージョン
           MOVE    WRK-PARA-VERSION    TO  WRK-DATA
           MOVE    100                 TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    データ作成区分
           MOVE    SYS-1001-DATAINFECTIONKBN
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 4002-MOJI-HENSYU-SEC
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-I-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（属性情報）作成処理
      *****************************************************************
       400-P-REC-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           MOVE    "P"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    性別
           MOVE    PTINF-SEX           TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-BIRTHDAY
           MOVE    WRK-BIRTHDAY-YM     TO  WRK-NUM
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    郵便番号
           MOVE    PTINF-HOME-POST     TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    更新識別コード
           MOVE    PTINF-PTID          TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 4003-NUM-HENSYU-SEC
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-P-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（病名）作成処理
      *****************************************************************
       400-B-REC-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           MOVE    "B"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    病原体区分
           MOVE    TBL-DISEASE (I-D)   TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    傷病名コード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    疑いフラグ
           MOVE    WRK-UTAGAIFLG       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    診療開始日
           IF      WRK-BYOSRYYMD       =   0
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 4002-MOJI-HENSYU-SEC
           ELSE
               MOVE    WRK-BYOSRYYMD       TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 4003-NUM-HENSYU-SEC
           END-IF
      *    転帰区分
           MOVE    WRK-TENKIKBN        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    転帰日
           IF      WRK-TENKIYMD        =   0
               MOVE    SPACE               TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 4002-MOJI-HENSYU-SEC
           ELSE
               MOVE    WRK-TENKIYMD        TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 4003-NUM-HENSYU-SEC
           END-IF
      *    削除フラグ
           MOVE    WRK-DLTFLG          TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 4002-MOJI-HENSYU-SEC
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-B-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータ（診療行為・医薬品）作成処理
      *****************************************************************
       400-SY-REC-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           MOVE    ZERO                TO  WRK-END
      *    レコード識別
           IF      WRK-CDKBN           =   2
               MOVE    "Y"                 TO  WRK-DATA
           ELSE
               IF      WRK-CDKBN           =   3
                   MOVE    "S"                 TO  WRK-DATA
               ELSE
                   MOVE    WRK-CDKBN           TO  WRK-DATA
               END-IF
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    病原体区分
           MOVE    TBL-DISEASE (I-D)   TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    診療日
           MOVE    WRK-SRYYMD          TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    診療行為区分
           MOVE    WRK-SRYKBN          TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    コード
           MOVE    WRK-SRYCD           TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    数量
           MOVE    WRK-SURYO           TO  WRK-NUM
           MOVE    11                  TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    剤回数
           MOVE    WRK-ZAIKAISU        TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    処理区分
           MOVE    ZERO                TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4003-NUM-HENSYU-SEC
      *    入外区分
           MOVE    WRK-PARA-NYUGAIKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 4002-MOJI-HENSYU-SEC
      *    各種区分
           MOVE    WRK-ATTR            TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 4003-NUM-HENSYU-SEC
      *
           PERFORM 4001-FILE-WRITE-SEC
           .
       400-SY-REC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       4001-FILE-WRITE-SEC         SECTION.
      *
           MOVE    WRK-REC             TO  INFEC-REC
           WRITE   INFEC-REC
           ADD     1                   TO  CNT-INFEC
           .
       4001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       4002-MOJI-HENSYU-SEC        SECTION.
      *
           IF      WRK-DATA            =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDW FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW <       1
                   IF      WRK-DATA (IDW:1)    NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH)
                                               DELIMITED   BY  SIZE
                                   WRK-DATA(1:IDW)
                                               DELIMITED   BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW                 TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END             =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       4002-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
      *****************************************************************
       4003-NUM-HENSYU-SEC         SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
      *
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  -   IDW
                        IF      WRK-SYOSUU       >   ZERO
                          COMPUTE IDU     =    11  + WRK-SYOSUU
                          COMPUTE IDZ     =    IDU - IDW  +  1
                          PERFORM VARYING IDV FROM   IDU  BY  -1
                                  UNTIL   IDV <      12
                            IF  WRK-NUM(IDV:1)   =   "0"
                                COMPUTE IDZ      =   IDZ  -  1
                                IF  IDV              =    12
                                    COMPUTE IDZ      =    IDZ  -  1
                                END-IF
                            ELSE
                                MOVE    11       TO  IDV
                            END-IF
                          END-PERFORM
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                        END-STRING
                        COMPUTE WRK-REC-LENGTH   =   WRK-REC-LENGTH   +
                                                     IDZ
                        MOVE    10               TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
           .
       4003-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   ERR-FILE
           IF      STS-ERR             =   ZERO
               CLOSE   ERR-FILE
           ELSE
               OPEN    OUTPUT              ERR-FILE
               MOVE    WRK-INFECERR        TO  ERR-REC
               WRITE   ERR-REC
               CLOSE   ERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-INFECERR        TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBINF1 "         DELIMITED  BY  SIZE
                   WRK-INFECERR        DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病原体区分サマリ処理
      *****************************************************************
       900-DISEASE-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  I-D
           MOVE    ZERO                TO  TBL-DISEASE-MAX
      *
           MOVE    SPACE               TO  MCPDATA-REC
           MOVE    "tbl_infection_judgment"
                                       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM UNTIL  MCP-RC   NOT =   ZERO
               MOVE    "tbl_infection_judgment"
                                           TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INFJUDGMENT-REC
                   COMPUTE I-D                 =   I-D  +  1
                   MOVE    INFJUD-DISEASE      TO  TBL-DISEASE (I-D)
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_infection_judgment"
                                       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    I-D                 TO  TBL-DISEASE-MAX
           .
       900-DISEASE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    判定取得処理
      *****************************************************************
       900-JUDGMENT-SET-SEC        SECTION.
      *
           INITIALIZE                      TBL-JUDGMENT-AREA
           MOVE    ZERO                TO  I-J-B
           MOVE    ZERO                TO  I-J-S
      *
           INITIALIZE                      INFJUDGMENT-REC
           MOVE    TBL-DISEASE (I-D)   TO  INFJUD-DISEASE
           MOVE    INFJUDGMENT-REC     TO  MCPDATA-REC
           MOVE    "tbl_infection_judgment"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
      *
           PERFORM UNTIL  MCP-RC   NOT =   ZERO
               MOVE    "tbl_infection_judgment"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INFJUDGMENT-REC
      * 2012.7 テーブルオーバーの対処
                   EVALUATE    INFJUD-CODE-KBN
                       WHEN    5
                           MOVE    1               TO  INFJUD-CODE-KBN
                       WHEN    6
                           MOVE    2               TO  INFJUD-CODE-KBN
                       WHEN    7
                           MOVE    3               TO  INFJUD-CODE-KBN
                       WHEN    OTHER
                           CONTINUE
                   END-EVALUATE
      *
                   IF      INFJUD-CODE-KBN     =   1
                       COMPUTE I-J-B               =   I-J-B  +  1
                       MOVE    INFJUD-CODE         TO
                                       TBL-JUDGMENT-BYOMEICD (I-J-B)
                   END-IF
                   IF      INFJUD-CODE-KBN     =   2  OR  3
                       COMPUTE I-J-S               =   I-J-S  +  1
                       MOVE    INFJUD-CODE         TO
                                       TBL-JUDGMENT-SRYCD (I-J-S)
                       MOVE    INFJUD-CODE-KBN     TO
                                       TBL-JUDGMENT-CDKBN (I-J-S)
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_infection_judgment"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    I-J-B               TO  TBL-JUDGMENT-B-MAX
           MOVE    I-J-S               TO  TBL-JUDGMENT-S-MAX
           .
       900-JUDGMENT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスタＲＥＡＤ
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-JYURRK
               MOVE    MCPDATA-REC         TO  JYURRK-REC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-PTID         TO  KEY-N-PTID
           END-IF
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       900-PTBYO-READ-SEC          SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTBYOMEI
               MOVE    MCPDATA-REC         TO  PTBYOMEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           .
       900-PTBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計読み込み処理
      *****************************************************************
       900-SRYACCT-READ-SEC        SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM(IDX)  TO  ACCT-ZAINUM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SRYACCT
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症読込
      *****************************************************************
       900-INFECTION-READ-SEC      SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-INFECTION
               MOVE    MCPDATA-REC         TO  INFECTION-REC
           ELSE
               MOVE    1                   TO  FLG-INFECTION
           END-IF
           .
       900-INFECTION-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTINF
                   MOVE    MCPDATA-REC         TO  PTINF-REC
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
                                           TNS-YUKOEDYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSU
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                      TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       900-TENSUPLUS-READ-SEC      SECTION.
      *
           INITIALIZE                      TENSUPLUS-REC
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    WRK-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-SRYYMD          TO  TNSP-YUKOSTYMD
                                           TNSP-YUKOEDYMD
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-TENSUPLUS
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
      *
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC            SECTION.
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
       900-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC           SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢオープン処理
      *****************************************************************
       900-DBOPEN-SEC              SECTION.
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
       900-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
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
