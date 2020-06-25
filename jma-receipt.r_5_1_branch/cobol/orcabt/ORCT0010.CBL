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
       PROGRAM-ID.             ORCT0010.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 定期請求
      *  コンポーネント名  : 収納データ作成
      *  管理者            :
      *  作成日付    作業者        記述
      *  02/01/07    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.04.02  負担金計算サブ引数修正
      *  01.00.02    NACL-太田    03.12.19  負担金計算サブ引数修正
      *                                     （日別収納マスタ）
      *  01.00.03    NACL-太田    04.03.03  患者請求履歴ファイルの編集
      *                                     をORCT0040に移す
      *  03.05.00    NACL-太田    07/05/30  グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    定期請求収納ファイル(OT)
           SELECT  TEIKI01-FILE    ASSIGN   TEIKI01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TEIKI01-KEY
                                   FILE    STATUS  IS  STS-TEIKI01.
      *
      *    定期請求期間情報ファイル(OT)
           SELECT  TEIKI03-FILE    ASSIGN  TEIKI03PARA
                   ORGANIZATION    LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-TEIKI03.
      *
      *    エラーファイル
           SELECT  TEIKIERR-FILE   ASSIGN  TEIKIERR
                                   FILE    STATUS  IS  STS-TEIKIERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    定期請求収納ファイル
       FD  TEIKI01-FILE.
       01  TEIKI01-REC.
           COPY    "CPTEIKI01.INC".
      *
      *    定期請求期間情報
       FD  TEIKI03-FILE.
           COPY "CPTEIKI03.INC".
      *
      *    エラーファイル
       FD  TEIKIERR-FILE.
       01  TEIKIERR-REC                    PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    定期請求収納ファイル名称領域 
           COPY    "CPCOMMONDAT3.INC"      REPLACING  //TMPFLPARA//
                                           BY         //TEIKI01PARA//.
      *
      *    定期請求期間情報名称領域 
           COPY    "CPCOMMONDAT3.INC"      REPLACING  //TMPFLPARA//
                                           BY         //TEIKI03PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //TEIKIERR//.
      *
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TEIKIERR                PIC X(02).
           03  STS-TEIKI01                 PIC X(02).
           03  STS-TEIKI03                 PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ERR                     PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
           03  FLG-PTNYUINRRK2             PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SYS5012                 PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-ORCSDAY                 PIC 9(01).
           03  FLG-NYUINKA                 PIC 9(01).
           03  FLG-PTTEIKIRRK              PIC 9(01).
           03  FLG-GETUMATUSITEI           PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
           03  FLG-PTNUM                   PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT1                    PIC 9(06).
           03  CNT-OUT2                    PIC 9(06).
           03  CNT-OUT3                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX1                        PIC 9(04). 
           03  IDX2                        PIC 9(04). 
           03  IDX3                        PIC 9(04). 
           03  IDX10                       PIC 9(04). 
           03  IDX11                       PIC 9(04). 
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-KIKAN-TBL.
               05  WRK-KIKAN-MAX           PIC 9(04).
               05  WRK-KIKAN-OCC           OCCURS 3.
                   07  WRK-KIKANST         PIC X(08).
                   07  WRK-KIKANED         PIC X(08).
                   07  WRK-KIKANSAKKBN     PIC 9(01).
                   07  WRK-KIKANKBT        PIC 9(01).
      *
           03  WRK-TEIKISTYMD              PIC X(08).
           03  WRK-TEIKIEDYMD              PIC X(08).
           03  WRK-TEIKISAKKBN             PIC X(01).
           03  WRK-TEIKIKBT                PIC X(01).
           03  WRK-TEIKIGETUMATU           PIC X(01).
      *
           03  WRK-YMD.
               05  WRK-YY                  PIC X(04).
               05  WRK-MM                  PIC X(02).
               05  WRK-DD                  PIC X(02).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1         PIC X(10).
               05  WRK-EDTYMD3         PIC X(22).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-NYUINKA                 PIC X(02).
           03  WRK-BTUNUM                  PIC X(02).
           03  WRK-BRMNUM                  PIC X(08).
           03  WRK-DRCD                    PIC X(04).
           03  WRK-SEIKYUSTYMD             PIC X(08).
           03  WRK-SEIKYUEDYMD             PIC X(08).
           03  WRK-CNT                     PIC 9(05).
           03  WRK-SAKKBN                  PIC X(01).
           03  WRK-KBT                     PIC X(01).
           03  WRK-STR                     PIC X(200).
           03  WRK-LEN                     PIC 9(05).
           03  WRK-SHO                     PIC 9(05).
           03  WRK-AMARI                   PIC 9(05).
           03  WRK-KUGIRI                  PIC X(02).
           03  WRK-PTID                    PIC 9(10).
           03  WRK-NYUKIN-HOHO             PIC X(02).
           03  WRK-NYUKIN-JYOTAI           PIC X(01).
           03  WRK-NYUKIN-KBN              PIC X(01).
           03  WRK-SHELLMSG-G.
               05  WRK-KBTMSG              PIC X(06).
               05  WRK-MONMSG              PIC X(06).
               05  WRK-SHELLMSG            PIC X(100).
           03  WRK-NYUINYMD                PIC X(08).
      *
       01  SRH-AREA.
           03  SRH-IDX                     PIC 9(05).
           03  SRH-1041-DAT.
               05  SRH-1041-JYOTAI         PIC X(01).
               05  SRH-1041-MAX            PIC 9(05).
               05  SRH-1041-OCC            OCCURS  99.
                   07  SRH-1041-KBNCDL     PIC X(02).
                   07  SRH-1041-JYOTAIL    PIC X(01).
      *
       01  ERR-EDIT-AREA.
           03  ERR-WARNING                 PIC 9(01).
           03  ERR-PTID                    PIC 9(10).
           03  ERR-PTNUM                   PIC X(20).
           03  ERR-ERRCD.
               05  ERR-ERRCD-NUM           PIC 9(04).
           03  ERR-ERRMSG                  PIC X(80).
           03  ERR-HEN-ERRMSG              PIC X(80).
      *
       01  ERR-MSG-AREA.
           03  ERR-MSG01                   PIC X(100)    VALUE
               "入院履歴の取得に失敗しました".
           03  ERR-MSG02                   PIC X(100)    VALUE
               "定期請求管理情報の取得に失敗しました".
           03  ERR-MSG03                   PIC X(100)    VALUE
               "患者情報の取得に失敗しました".
           03  ERR-MSG04                   PIC X(100)    VALUE
               "通算日数の取得に失敗しました".
           03  ERR-MSG06                   PIC X(100)    VALUE
               "収納データの取得に失敗しました".
           03  ERR-MSG07                   PIC X(100)    VALUE
               "入院履歴の取得に失敗しました".
           03  ERR-MSG08                   PIC X(100)    VALUE
               "定期期間の取得に失敗しました".
           03  ERR-MSG09                   PIC X(100)    VALUE
               "処理対象のデータがありませんでした".
           03  ERR-MSG10                   PIC X(100)    VALUE
               "定期請求管理情報の取得に失敗しました".
           03  ERR-MSG88                   PIC X(100)    VALUE
               "キャンセルしました".
      *
       01  CONST-AREA.
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
      *
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      *    定期請求一括処理パラメータ
           COPY    "CPTEIKI1PARA.INC".
      *
      *    ワーク患者入院履歴
       01  WKPTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING   //PTNYUINRRK-//
                                       BY          //WKPTNYUINRR-//.
      *
           COPY    "CPSK5010.INC"      REPLACING   //SYS-//
                                       BY          //WKSYS-//.
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
      *    入金方法
           COPY  "CPSK1041.INC".
      *    請求管理
           COPY    "CPSK5010.INC".
      *    定期請求作成指示コード
           COPY    "CPSK5011.INC".
      *    定期請求作成指示（個別指示）コード
           COPY    "CPSK5012.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者定期請求履歴
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    定期請求サブ
           COPY    "CPORCSTEIKI01.INC".
      *
       01  LNKNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-// 
                                       BY         //LNKNRRK-//.
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
      *    負担金計算サブエラーメッセージ
           COPY    "CPORCS02MSG.INC".
      *
      *    同日入院チェックサブ
           COPY    "CPORCSDNTCHK.INC".
      *
      *    分娩入院チェックサブ
           COPY  "CPORCSBUNBENCHK.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                         SECTION.
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
           MOVE    TEIKI1P-HOSPNUM  TO  SPA-HOSPNUM
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
           MOVE    TEIKI1P-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCT0010"      TO  JOB-PGID
           MOVE    WRK-SHELLMSG    TO  JOB-SHELLMSG
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    ZERO            TO  FLG-END
                                       FLG-ERR
      *
           MOVE    TEIKI1P-HOSPNUM TO  TEIKI01PARA-HOSPNUM
           MOVE    "TEIKI001"      TO  TEIKI01PARA-FILE-ID
           MOVE    TEIKI1P-TERMID  TO  TEIKI01PARA-TERMID
      *
           MOVE    TEIKI1P-HOSPNUM TO  TEIKI03PARA-HOSPNUM
           MOVE    "TEIKI003"      TO  TEIKI03PARA-FILE-ID
           MOVE    TEIKI1P-TERMID  TO  TEIKI03PARA-TERMID
      *
           INITIALIZE                  SGETTEMP-AREA
      *
           MOVE    TEIKI01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES(1)
           MOVE    TEIKI03PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES(2)
      *
           MOVE    TEIKI1P-ERRFILE TO  SGETTEMP-BASENAMES(3)
      *
           CALL    "ORCSGETTEMP"   USING
                                   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES(1)
                                   TO  TEIKI01PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES(2)
                                   TO  TEIKI03PARA-FULLNAME
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
           OPEN    OUTPUT              TEIKI01-FILE
           OPEN    OUTPUT              TEIKI03-FILE
      *
      *    定期請求管理情報
           INITIALIZE                  SYS-5010-REC
           MOVE    TEIKI1P-HOSPNUM TO  SYS-5010-HOSPNUM
           MOVE    "5010"          TO  SYS-5010-KANRICD
           MOVE    "*"             TO  SYS-5010-KBNCD
           MOVE    TEIKI1P-SYSYMD  TO  SYS-5010-STYUKYMD
                                       SYS-5010-EDYUKYMD
           MOVE    SYS-5010-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC TO SYS-5010-REC
           ELSE
               MOVE    "0002"      TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           PERFORM 1001-SRH-1041-HEN-SEC
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
                   "収納データ作成"    DELIMITED   BY  SIZE
           INTO    WRK-SHELLMSG
           END-STRING
      *
           .
       1001-EDIT-SHELLMSG-EXT.
           EXIT.
      *****************************************************************
      *    入金方法
      *****************************************************************
       1001-SRH-1041-HEN-SEC               SECTION.
      *
      *    入金方法
           MOVE    ZERO            TO  SRH-1041-MAX
      *
           INITIALIZE                  SYS-1041-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1041-HOSPNUM
           MOVE    "1041"          TO  SYS-1041-KANRICD
           MOVE    TEIKI1P-SYSYMD  TO  SYS-1041-STYUKYMD
                                       SYS-1041-EDYUKYMD
           MOVE    SYS-1041-REC    TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC TO  SYS-1041-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                    OR   ( SRH-1041-MAX    >=  99 )
      *
               COMPUTE SRH-1041-MAX    =   SRH-1041-MAX    +   1
      *
               MOVE    SYS-1041-KBNCD  TO  SRH-1041-KBNCDL
                                                        (SRH-1041-MAX)
      *
               IF    ( SYS-1041-NYKN-JYOTAI-T  =   SPACE )
                   MOVE    "3"     TO  SRH-1041-JYOTAIL (SRH-1041-MAX)
               ELSE
                   MOVE    SYS-1041-NYKN-JYOTAI-T
                                   TO  SRH-1041-JYOTAIL (SRH-1041-MAX)
               END-IF
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               IF    ( FLG-SYSKANRI        =   ZERO )
                   MOVE    SYSKANRI-REC TO  SYS-1041-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-SRH-1041-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           EVALUATE    TEIKI1P-SYOKBN
           WHEN    "1"
               PERFORM 2001-IKKATU-SYORI-SEC
           WHEN    "2"
               PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
      *
           MOVE    1               TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC           SECTION.
      *
           INITIALIZE    WRK-KIKAN-TBL
           MOVE    ZERO            TO  WRK-CNT
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   3 )
               IF    ( TEIKI1P-KIKAN-KBN (IDX10)  =   "1" )
                AND  ( TEIKI1P-KBT =      IDX10 )
      *
                   COMPUTE WRK-CNT =   WRK-CNT +   1
                   MOVE    WRK-CNT TO  WRK-KIKAN-MAX
      *
                   MOVE    TEIKI1P-KIKAN-ST (IDX10)
                                   TO  WRK-KIKANST (WRK-CNT)
                   MOVE    TEIKI1P-KIKAN-ED (IDX10)
                                   TO  WRK-KIKANED (WRK-CNT)
                   MOVE    IDX10   TO  WRK-KIKANSAKKBN (WRK-CNT)
                   MOVE    IDX10   TO  WRK-KIKANKBT    (WRK-CNT)
               END-IF
      *
           END-PERFORM
      *
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   WRK-KIKAN-MAX )
      *
               MOVE    WRK-KIKANST (IDX10)
                                   TO  WRK-TEIKISTYMD
               MOVE    WRK-KIKANED (IDX10)
                                   TO  WRK-TEIKIEDYMD
               MOVE    WRK-KIKANSAKKBN (IDX10)
                                   TO  WRK-TEIKISAKKBN
               MOVE    WRK-KIKANKBT    (IDX10)
                                   TO  WRK-TEIKIKBT
      *
      *        一括処理（メイン）
               PERFORM 20011-IKKATU-SYORI-MAIN-SEC
      *
           END-PERFORM
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    一括処理（メイン）
      *****************************************************************
       20011-IKKATU-SYORI-MAIN-SEC     SECTION.
      *
      *    入院履歴読み込み
           MOVE    ZERO            TO  FLG-PTNYUINRRK
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    TEIKI1P-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-TEIKIEDYMD  TO  PTNYUINRRK-NYUINYMD
           MOVE    WRK-TEIKIEDYMD  TO  PTNYUINRRK-TAIINYMD
           MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE              PTNYUINRRK-REC
               MOVE    1           TO  FLG-PTNYUINRRK
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  =   1 )
      *
      *        歴番号の最新の入院歴に読み直す
               PERFORM 900-PTNYUINRRK-KEY4-SEL-SEC
               IF    ( FLG-PTNYUINRRK2     NOT =   ZERO )
                   MOVE    PTNYUINRRK-PTID TO  ERR-PTID
                   MOVE    "0001"          TO  ERR-ERRCD
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
      *        処理対象チェック処理
               PERFORM 20011-TAISYO-CHK-SEC
               IF    ( FLG-TAISYO  =   1 )
                   MOVE    SPACE           TO  WRK-NYUINYMD
                   IF    ( PTNYUINRRK-NYUINYMD <   CONST-H180401 )
                       PERFORM 800-ORCSDNTCHK-SEC
                       IF    ( SDNTCHK-STYMD   NOT =   ZERO )
                           MOVE    SDNTCHK-STYMD
                                           TO  WRK-NYUINYMD
                       END-IF
                   ELSE
                       MOVE    PTNYUINRRK-NYUINYMD
                                           TO  WRK-NYUINYMD
                   END-IF
                   IF    ( WRK-NYUINYMD    NOT =   SPACE )
                       PERFORM 20011-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
               MOVE    "key14"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
               ELSE
                   INITIALIZE              PTNYUINRRK-REC
                   MOVE    1           TO  FLG-PTNYUINRRK
               END-IF
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
           MOVE    "key14"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-IKKATU-SYORI-MAIN-EXT.
           EXIT. 
      *****************************************************************
      *   処理対象チェック処理
      *****************************************************************
       20011-TAISYO-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-TAISYO
           MOVE    SPACE           TO  WRK-SAKKBN
           MOVE    ZERO            TO  FLG-GETUMATUSITEI
      *
           IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN
                                           =   "3" )
               GO  TO  20011-TAISYO-CHK-EXT
           END-IF
      *
      *    既に退院している患者は対象外とする
           IF    ( PTNYUINRRK-TAIINYMD
                               NOT =   "99999999" )
               GO  TO  20011-TAISYO-CHK-EXT
           END-IF
      *
      *    患者定期請求履歴読み込み処理
           PERFORM 900-PTTEIKIRRK-KEY2-SEL-SEC
           IF    ( FLG-PTTEIKIRRK  =   ZERO )
               IF    ( PTTEIKIRRK-SAKKBN   =   "9" )
                   MOVE    1       TO  FLG-GETUMATUSITEI
               END-IF
           END-IF
      *
           IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN
                                           =   "2" )
               MOVE    1           TO  FLG-GETUMATUSITEI
           END-IF
      *
           IF    ( FLG-GETUMATUSITEI       =   1 )
      *
      *        今回処理が月末日を含む場合は処理対象とする
               MOVE    TEIKI1P-SRYYM  TO  WRK-YMD
               PERFORM 20021-GETUMATU-GET-SEC
               IF    ( WRK-YMD             =   WRK-TEIKIEDYMD )
                   MOVE    1           TO  FLG-TAISYO
               END-IF
           ELSE
               MOVE    1               TO  FLG-TAISYO
           END-IF
      *
           IF    ( FLG-TAISYO              =   ZERO )
               GO  TO  20011-TAISYO-CHK-EXT
           END-IF
      *
           IF    ( FLG-GETUMATUSITEI       =   ZERO )
               MOVE    WRK-TEIKISAKKBN TO  WRK-SAKKBN
           ELSE
               MOVE    "9"             TO  WRK-SAKKBN
           END-IF
      *
           MOVE    WRK-TEIKIKBT        TO  WRK-KBT
      *
           .
       20011-TAISYO-CHK-EXT.
           EXIT. 
      *****************************************************************
      *   編集処理
      *****************************************************************
       20011-HENSYU-SEC                SECTION.
      *
      *    患者情報読み込み
           MOVE    TEIKI1P-HOSPNUM      TO  PTINF-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTINF-PTID
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    PTNYUINRRK-PTID TO  ERR-PTID
               MOVE    "0003"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    テスト患者のときは読み飛ばす
           IF    ( PTINF-TSTPTNUMKBN   =   "1" )
               GO  TO  20011-HENSYU-EXT
           END-IF
      *
      *    入金方法取得
           PERFORM 200111-NYUKIN-HOHO-HEN-SEC
      *
      *    収納データ取得処理
           PERFORM 20011-SYUNOU-GET-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >    10 )
                   OR    ( LNK-SYU-SKYSTYMD (IDX1) =   SPACE )
      *
      *        収納データを定期収納ファイルに出力
               PERFORM 20011-TEIKI01-OUTPUT-SEC
      *
           END-PERFORM
      *
      *    患者請求期間情報を患者請求期間情報ファイルに出力
           PERFORM 20011-TEIKI03-OUTPUT-SEC
      *
           .
       20011-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    入金方法編集処理
      *****************************************************************
       200111-NYUKIN-HOHO-HEN-SEC      SECTION.
      *
           MOVE    "01"            TO  WRK-NYUKIN-HOHO
           MOVE    "3"             TO  WRK-NYUKIN-JYOTAI
           MOVE    SPACE           TO  WRK-NYUKIN-KBN
      *
           IF    ( PTINF-NYUKIN-HOHO   NOT =   SPACE )
               MOVE    PTINF-NYUKIN-HOHO
                                   TO  WRK-NYUKIN-HOHO
           END-IF
      *
           MOVE    SPACE           TO  SRH-1041-JYOTAI
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-1041-MAX )
                    OR   ( SRH-1041-JYOTAI NOT =   SPACE )
      *
               IF    ( WRK-NYUKIN-HOHO =   SRH-1041-KBNCDL (SRH-IDX))
                   MOVE    SRH-1041-JYOTAIL (SRH-IDX)
                                   TO  SRH-1041-JYOTAI
               END-IF
      *
           END-PERFORM
      *
           IF    ( SRH-1041-JYOTAI NOT =   SPACE )
               MOVE    SRH-1041-JYOTAI TO  WRK-NYUKIN-JYOTAI
           END-IF
      *
           EVALUATE    WRK-NYUKIN-JYOTAI
           WHEN    "1"
               MOVE    "2"             TO  WRK-NYUKIN-KBN
           WHEN    "2"
               MOVE    "1"             TO  WRK-NYUKIN-KBN
           WHEN    "3"
               MOVE    SYS-5010-NYUKINKBN
                                       TO  WRK-NYUKIN-KBN
           WHEN    OTHER
               MOVE    SYS-5010-NYUKINKBN
                                       TO  WRK-NYUKIN-KBN
           END-EVALUATE
           
      *
           .
       200111-NYUKIN-HOHO-HEN-EXT.
           EXIT. 
      *****************************************************************
      *    収納データ取得処理
      *****************************************************************
       20011-SYUNOU-GET-SEC            SECTION.
      *
      * ----   請求開始日を取得 -----
           MOVE    SPACE           TO  WRK-SEIKYUSTYMD
           IF    ( WRK-SAKKBN      =   "9" )
               IF    ( TEIKI1P-SYOKBN =   "1" )
                   MOVE    TEIKI1P-SRYYM (1 : 6)
                                   TO  WRK-YMD (1 : 6)
                   MOVE    "01"    TO  WRK-YMD (7 : 2)
               ELSE
                   MOVE    SYS-5012-SRYYM
                                   TO  WRK-YMD (1 : 6)
                   MOVE    "01"    TO  WRK-YMD (7 : 2)
               END-IF
           ELSE
               MOVE    WRK-TEIKISTYMD
                                   TO  WRK-YMD
           END-IF
      *
      *
           IF  ( WRK-NYUINYMD      >   WRK-YMD )
               MOVE    WRK-NYUINYMD
                                   TO  WRK-SEIKYUSTYMD
           ELSE
               MOVE    WRK-YMD     TO  WRK-SEIKYUSTYMD
           END-IF
      * ------------------------------
           MOVE    WRK-TEIKIEDYMD  TO  WRK-SEIKYUEDYMD
      *
      *    通算日数を求める
           MOVE    PTNYUINRRK-NYUINYMD
                                   TO  WRK-STYMD
      *    請求開始日を設定
           MOVE    WRK-SEIKYUSTYMD
                                   TO  WRK-EDYMD
           PERFORM 800-KIKAN-CALC-SEC
           IF    ( FLG-ORCSDAY     NOT =   ZERO )
               MOVE    PTNYUINRRK-PTID TO  ERR-PTID
               MOVE    "0004"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           MOVE    SPACE           TO  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
                                       S02MSG-AREA
      *
           INITIALIZE                  LNK-ORCS02-REC
                                       LNK-SYUNOU-REC-T
                                       LNK-SYUDAY-REC-T
                                       S02MSG-AREA
      *
           INITIALIZE                  SBUNBENCHK-AREA
           MOVE    PTNYUINRRK-PTID TO  SBUNBENCHK-PTID
           MOVE    PTNYUINRRK-RRKNUM
                                   TO  SBUNBENCHK-RRKNUM
           CALL    "ORCSBUNBENCHK" USING
                                       SBUNBENCHK-AREA
                                       SPA-AREA
      *
           INITIALIZE                  SS006-AREA
           MOVE    PTNYUINRRK-PTID     TO  SPA-PTID
           MOVE    WRK-SEIKYUSTYMD     TO  SS006-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD     TO  SS006-SKYEDYMD
           MOVE    "2"                 TO  SS006-SKYKBN
           CALL    "ORCSS006"      USING
                                   SS006-AREA
                                   SPA-AREA
      *
           MOVE    TEIKI1P-HOSPNUM  TO  LNK-S02-HOSPNUM
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    PTNYUINRRK-PTID TO  LNK-S02-PTID
           MOVE    WRK-NISSU1      TO  LNK-S02-NYUIN-TSUSAN-NISSU
      *    請求開始日を設定
           MOVE    WRK-SEIKYUSTYMD
                                   TO  LNK-S02-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD
                                   TO  LNK-S02-SKYEDYMD
           IF    ( WRK-SEIKYUSTYMD <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ST  TO  LNK-S02-DOJITUFLG-ST
               MOVE    SS006-TIMEKBN-ST    TO  LNK-S02-TIMEKBN-ST
           END-IF
           IF    ( WRK-SEIKYUEDYMD <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ED  TO  LNK-S02-DOJITUFLG-ED
               MOVE    SS006-TIMEKBN-ED    TO  LNK-S02-TIMEKBN-ED
           END-IF
           MOVE    "A"             TO  LNK-S02-TEIKI-KBN
           MOVE    TEIKI1P-SYSYMD TO  LNK-S02-SYSYMD
           IF    ( SBUNBENCHK-BUNBEN   =   ZERO )
               MOVE    "2"         TO  LNK-S02-SYORI-KBN
           ELSE
               MOVE    "9"         TO  LNK-S02-SYORI-KBN
           END-IF
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
           IF    ( LNK-S02-RC      NOT = ZERO )
      *
               INITIALIZE              S02MSG-AREA
               MOVE    LNK-S02-RC  TO  S02MSG-S02-RC
               CALL    "ORCS02MSG"     USING
                                       S02MSG-AREA
                                       SPA-AREA
      *
               IF    ( S02MSG-MSG (1)  =   SPACE )
                   MOVE    "0006"  TO  ERR-ERRCD
               ELSE
                   COMPUTE ERR-ERRCD-NUM
                       =   3000
                       +   LNK-S02-RC
               END-IF
      *
               MOVE    PTNYUINRRK-PTID TO  ERR-PTID
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           PERFORM VARYING IDX1   FROM    1   BY  1
                   UNTIL ( IDX1   >   10 )
      *
               DISPLAY "LNK-SYU-SKYMONEY (" IDX1 " ) = "
                        LNK-SYU-SKYMONEY ( IDX1 )
      *
           END-PERFORM
      *
           .
       20011-SYUNOU-GET-EXT.
           EXIT. 
      *****************************************************************
      *    定期収納ファイル出力処理
      *****************************************************************
       20011-TEIKI01-OUTPUT-SEC      SECTION.
      *
           INITIALIZE                  TEIKI01-REC
      *
      *    収納情報編集処理
           PERFORM 20011-TEIKI01-SYUNOU-HEN-SEC
      *
      *    日別収納情報編集処理
           PERFORM 20011-TEIKI01-SYUDAY-HEN-SEC
      *
           MOVE    SYU-HOSPNUM          TO  TEIKI01-HOSPNUM
           MOVE    SYU-PTID            TO  TEIKI01-PTID
           MOVE    WRK-SAKKBN          TO  TEIKI01-SAKKBN
           MOVE    WRK-KBT             TO  TEIKI01-KBT
           MOVE    WRK-SEIKYUSTYMD     TO  TEIKI01-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD     TO  TEIKI01-SKYEDYMD
           MOVE    SYU-HKNCOMBINUM     TO  TEIKI01-HKNCOMBINUM
      *
           WRITE   TEIKI01-REC
      *
           COMPUTE CNT-OUT1    =   CNT-OUT1    +   1
      *
           .
       20011-TEIKI01-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    収納情報編集処理
      *****************************************************************
       20011-TEIKI01-SYUNOU-HEN-SEC    SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDX1)
                                       TO  SYUNOU-REC
      *
           MOVE    TEIKI1P-DENPPRTYMD  TO  SYU-DENPPRTYMD
      *
           MOVE    WRK-SEIKYUSTYMD     TO  SYU-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD     TO  SYU-SKYEDYMD
      *
           MOVE    PTNYUINRRK-REC      TO  WKPTNYUINRRK-REC
      *    請求時最終履歴取得処理
           PERFORM 20011-SEILASTRRK-SET-SEC
           MOVE    WKPTNYUINRRK-REC    TO  PTNYUINRRK-REC
      *
           MOVE    WRK-NYUINKA         TO  SYU-SRYKA
           MOVE    WRK-BRMNUM          TO  SYU-ROOMNUM
           MOVE    WRK-BTUNUM          TO  SYU-BYOTONUM
           MOVE    WRK-DRCD            TO  SYU-DRCD
      *
           IF    ( WRK-NYUKIN-KBN      =   "2" )
      *
               MOVE    SYU-SKYMONEY    TO  SYU-NYUKIN-TOTAL
           END-IF
      *
           IF    ( SYU-NYUKIN-TOTAL    >   ZERO )
               MOVE    1               TO  SYU-NYUKIN-KAISU
           END-IF
      *
           IF    ( SYU-SKYMONEY        =   ZERO )
               MOVE    SPACE           TO  SYU-DENPJTIKBN
           ELSE
               IF    ( SYU-SKYMONEY   <=  SYU-NYUKIN-TOTAL )
                   MOVE    "1"         TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"         TO  SYU-DENPJTIKBN
               END-IF
           END-IF
      *
           MOVE    PTNYUINRRK-RRKNUM   TO  SYU-NYUIN-RRKNUM
           IF    ( SYS-5010-SKYSUMKBN  =   "1" )
               MOVE    1               TO  SYU-GRP-HAKHOUFLG
           ELSE
               MOVE    ZERO            TO  SYU-GRP-HAKHOUFLG
           END-IF
      *
           MOVE    TEIKI1P-TERMID      TO  SYU-TERMID
      *
           MOVE    SYUNOU-REC          TO  TEIKI01-SYU-DAT
      *
           .
       20011-TEIKI01-SYUNOU-HEN-EXT.
           EXIT. 
      *****************************************************************
      *    日別収納情報編集処理
      *****************************************************************
       20011-TEIKI01-SYUDAY-HEN-SEC    SECTION.
      *
           MOVE    LNK-SYUDAY-REC (IDX1)
                                       TO  SYUDAY-REC
      *
           MOVE    TEIKI1P-TERMID      TO  SDAY-TERMID
      *
           IF    ( SDAY-DAYINFFLG      =   "1" )
               MOVE    1               TO  TEIKI01-SYUDAY-EXIST-FLG
               MOVE    SYUDAY-REC      TO  TEIKI01-SYUDAY-DAT
           ELSE
               MOVE    ZERO            TO  TEIKI01-SYUDAY-EXIST-FLG
               MOVE    SPACE           TO  TEIKI01-SYUDAY-DAT
           END-IF
      *
           .
       20011-TEIKI01-SYUDAY-HEN-EXT.
           EXIT. 
      *****************************************************************
      *    患者請求期間情報ファイル出力処理
      *****************************************************************
       20011-TEIKI03-OUTPUT-SEC        SECTION.
      *
           INITIALIZE                  TEIKI03-REC
           MOVE    PTNYUINRRK-HOSPNUM   TO  TEIKI03-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  TEIKI03-PTID
           MOVE    WRK-SEIKYUSTYMD     TO  TEIKI03-SKYSTYMD
           MOVE    WRK-SEIKYUEDYMD     TO  TEIKI03-SKYEDYMD
      *
           WRITE   TEIKI03-REC
      *
           COMPUTE CNT-OUT3    =   CNT-OUT3    +   1
      *
           .
       20011-TEIKI03-OUTPUT-EXT.
           EXIT. 
      *****************************************************************
      *    請求時最終履歴取得処理
      *****************************************************************
       20011-SEILASTRRK-SET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINKA
           MOVE    SPACE               TO  WRK-NYUINKA
                                           WRK-BTUNUM
                                           WRK-BRMNUM
                                           WRK-DRCD
      *
      *    入院履歴マスタ検索
           MOVE    ZERO                TO  FLG-PTNYUINRRK2
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK2
           END-IF
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK2 NOT =   ZERO )
                    OR   ( FLG-NYUINKA =   1 )
      *
               IF    ( PTNYUINRRK-TENNYUYMD
                                       <=  SYU-SKYEDYMD )
                AND  ( PTNYUINRRK-TENSTUYMD
                                       >=  SYU-SKYEDYMD )
                   MOVE    1           TO  FLG-NYUINKA
                   MOVE    PTNYUINRRK-NYUINKA
                                       TO  WRK-NYUINKA
                   MOVE    PTNYUINRRK-BTUNUM
                                       TO  WRK-BTUNUM
                   MOVE    PTNYUINRRK-BRMNUM
                                       TO  WRK-BRMNUM
                   MOVE    PTNYUINRRK-DRCD1 (2 : 4)
                                       TO  WRK-DRCD
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               ELSE
                   INITIALIZE                  PTNYUINRRK-REC
                   MOVE    1               TO  FLG-PTNYUINRRK2
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( FLG-NYUINKA         =   ZERO )
               MOVE    PTNYUINRRK-PTID TO  ERR-PTID
               MOVE    "0007"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20011-SEILASTRRK-SET-EXT.
           EXIT.
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-SYS5012
      *
      *    定期請求指示（個別指示）コード読み込み
           INITIALIZE                      SYS-5012-REC
           MOVE    TEIKI1P-HOSPNUM     TO  SYS-5012-HOSPNUM
           MOVE    "5012"              TO  SYS-5012-KANRICD
           MOVE    TEIKI1P-SYSYMD      TO  SYS-5012-STYUKYMD
                                           SYS-5012-EDYUKYMD
           MOVE    SYS-5012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5012-REC
           ELSE
               MOVE    1               TO  FLG-SYS5012
           END-IF
      *
           PERFORM UNTIL ( FLG-SYS5012     =   1 )
      *
               PERFORM 20021-KOBETU-HENSYU-SEC
      *
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC             SECTION.
      *
      *    請求期間取得処理
           MOVE    SYS-5010-REC    TO      WKSYS-5010-REC
           PERFORM 20021-SEIKYU-KIKAN-GET-SEC
           MOVE    WKSYS-5010-REC  TO      SYS-5010-REC
      *
           PERFORM VARYING IDX10   FROM    1   BY  1
                   UNTIL ( IDX10   >   WRK-KIKAN-MAX )
      *
               MOVE    WRK-KIKANST (IDX10)
                                       TO  WRK-TEIKISTYMD
               MOVE    WRK-KIKANED (IDX10)
                                       TO  WRK-TEIKIEDYMD
               MOVE    WRK-KIKANSAKKBN (IDX10)
                                       TO  WRK-SAKKBN
               MOVE    WRK-KIKANKBT    (IDX10)
                                       TO  WRK-KBT
      *        入院履歴読み込み
      *
               IF    ( WRK-KBT         =   TEIKI1P-KBT )
      *
                   MOVE    ZERO            TO  FLG-PTNYUINRRK
      *
                   INITIALIZE              PTNYUINRRK-REC
                   MOVE    TEIKI1P-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
                   MOVE    SYS-5012-PTID   TO  PTNYUINRRK-PTID
                   MOVE    WRK-TEIKIEDYMD  TO  PTNYUINRRK-NYUINYMD
                   MOVE    WRK-TEIKIEDYMD  TO  PTNYUINRRK-TAIINYMD
                   MOVE    PTNYUINRRK-REC  TO  MCPDATA-REC
                   MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF    ( MCP-RC          =   ZERO )
                       MOVE    MCPDATA-REC TO  PTNYUINRRK-REC
                   ELSE
                       INITIALIZE              PTNYUINRRK-REC
                       MOVE    1           TO  FLG-PTNYUINRRK
                   END-IF
      *
      *            カーソルクロース
                   MOVE    "tbl_ptnyuinrrk" TO MCP-TABLE
                   MOVE    "key10"         TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
      *
                   IF    ( FLG-PTNYUINRRK  =   ZERO )
                       MOVE    SPACE           TO  WRK-NYUINYMD
                       IF    ( PTNYUINRRK-NYUINYMD <   CONST-H180401 )
                           PERFORM 800-ORCSDNTCHK-SEC
                           IF    ( SDNTCHK-STYMD   NOT =   ZERO )
                               MOVE    SDNTCHK-STYMD
                                           TO  WRK-NYUINYMD
                           END-IF
                       ELSE
                           MOVE    PTNYUINRRK-NYUINYMD
                                           TO  WRK-NYUINYMD
                       END-IF
                       IF    ( PTNYUINRRK-TEIKI-SEIKYUKBN
                                                   =   "1" OR "2" )
                        AND  ( WRK-NYUINYMD    NOT =   SPACE )
                           PERFORM 20011-HENSYU-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-5012-REC
           ELSE
               MOVE    1               TO  FLG-SYS5012
           END-IF
      *
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求期間取得処理
      *****************************************************************
       20021-SEIKYU-KIKAN-GET-SEC      SECTION.
      *
           INITIALIZE  WRK-KIKAN-TBL
           MOVE        ZERO            TO  WRK-CNT
      *
      *    請求期間編集
           INITIALIZE                      SYS-5010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-5010-HOSPNUM
           MOVE    "5010"              TO  SYS-5010-KANRICD
           MOVE    "*"                 TO  SYS-5010-KBNCD
           MOVE    SYS-5012-SRYYM      TO  SYS-5010-STYUKYMD
           MOVE    "01"                TO  SYS-5010-STYUKYMD(7:2)
           MOVE    SYS-5010-STYUKYMD   TO  SYS-5010-EDYUKYMD
      *
           MOVE    SYS-5010-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           IF    ( FLG-SYSKANRI        =   ZERO )
               MOVE    SYSKANRI-REC        TO  SYS-5010-REC
           ELSE
               MOVE    "0010"              TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    作成区分が“９”の場合、月末時のみ指定
           IF    ( SYS-5012-SAKKBN     =   "9" )
               MOVE    1               TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM  (1 : 6)
                                       TO  WRK-KIKANST (1)(1 : 6)
               MOVE    "01"            TO  WRK-KIKANST (1)(7 : 2)
               MOVE    SYS-5012-SRYYM      TO  WRK-YMD
               PERFORM 20021-GETUMATU-GET-SEC
               MOVE    WRK-YMD         TO  WRK-KIKANED (1)
               MOVE    9               TO  WRK-KIKANSAKKBN (1)
               MOVE    SYS-5012-KBT    TO  WRK-KIKANKBT (1)
      *
               GO  TO  20021-SEIKYU-KIKAN-GET-EXT
           END-IF
      *
           IF    ( SYS-5012-SAKKBN =   "0" OR  "1" )
               COMPUTE WRK-CNT =   WRK-CNT +   1
               MOVE    WRK-CNT TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM  (1 : 6)
                                       TO  WRK-KIKANST (WRK-CNT)(1 : 6)
               MOVE    "01"            TO  WRK-KIKANST (WRK-CNT)(7 : 2)
               IF    ( SYS-5010-SEIKYU-DAY1    =   99 )
                   MOVE    SYS-5012-SRYYM      TO  WRK-YMD
                   PERFORM 20021-GETUMATU-GET-SEC
                   MOVE    WRK-YMD     TO  WRK-KIKANED (WRK-CNT)
               ELSE
                   MOVE    SYS-5012-SRYYM (1 : 6)
                                       TO  WRK-KIKANED (WRK-CNT)(1 : 6)
                   MOVE    SYS-5010-SEIKYU-DAY1
                                       TO  WRK-KIKANED (WRK-CNT)(7 : 2)
               END-IF
               MOVE    1               TO  WRK-KIKANSAKKBN (WRK-CNT)
               MOVE    1               TO  WRK-KIKANKBT    (WRK-CNT)
           END-IF
      *
           IF    ( SYS-5012-SAKKBN =   "0" OR  "2" )
               COMPUTE WRK-CNT =   WRK-CNT +   1
               MOVE    WRK-CNT TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM (1 : 6)  TO  WRK-YMD (1 : 6)
               MOVE    SYS-5010-SEIKYU-DAY1    TO  WRK-YMD (7 : 2)
               PERFORM 20021-YOKUZITU-GET-SEC
               MOVE    WRK-YMD         TO  WRK-KIKANST (WRK-CNT)
      *
               IF    ( SYS-5010-SEIKYU-DAY2    =   99 )
                   MOVE    SYS-5012-SRYYM      TO  WRK-YMD
                   PERFORM 20021-GETUMATU-GET-SEC
                   MOVE    WRK-YMD     TO  WRK-KIKANED (WRK-CNT)
               ELSE
                   MOVE    SYS-5012-SRYYM (1 : 6)
                                       TO  WRK-KIKANED (WRK-CNT)(1 : 6)
                   MOVE    SYS-5010-SEIKYU-DAY2
                                       TO  WRK-KIKANED (WRK-CNT)(7 : 2)
               END-IF
               MOVE    2               TO  WRK-KIKANSAKKBN (WRK-CNT)
               MOVE    2               TO  WRK-KIKANKBT    (WRK-CNT)
           END-IF
      *
           IF    ( SYS-5012-SAKKBN =   "0" OR  "3" )
               COMPUTE WRK-CNT =   WRK-CNT +   1
               MOVE    WRK-CNT TO  WRK-KIKAN-MAX
               MOVE    SYS-5012-SRYYM (1 : 6)  TO  WRK-YMD (1 : 6)
               MOVE    SYS-5010-SEIKYU-DAY2    TO  WRK-YMD (7 : 2)
               PERFORM 20021-YOKUZITU-GET-SEC
               MOVE    WRK-YMD         TO  WRK-KIKANST (WRK-CNT)
      *
               IF    ( SYS-5010-SEIKYU-DAY3    =   99 )
                   MOVE    SYS-5012-SRYYM      TO  WRK-YMD
                   PERFORM 20021-GETUMATU-GET-SEC
                   MOVE    WRK-YMD     TO  WRK-KIKANED (WRK-CNT)
               ELSE
                   MOVE    SYS-5012-SRYYM (1 : 6)
                                       TO  WRK-KIKANED (WRK-CNT)(1 : 6)
                   MOVE    SYS-5010-SEIKYU-DAY3
                                       TO  WRK-KIKANED (WRK-CNT)(7 : 2)
               END-IF
               MOVE    3               TO  WRK-KIKANSAKKBN (WRK-CNT)
               MOVE    3               TO  WRK-KIKANKBT    (WRK-CNT)
           END-IF
      *
           IF    ( WRK-KIKAN-MAX   <=  ZERO )
               MOVE    "0008"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       20021-SEIKYU-KIKAN-GET-EXT.
            EXIT.
      *****************************************************************
      *    月末日取得処理
      *****************************************************************
       20021-GETUMATU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-YMD (1 : 6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-YMD
      *
           .
       20021-GETUMATU-GET-EXT.
            EXIT.
      *****************************************************************
      *    翌日取得処理
      *****************************************************************
       20021-YOKUZITU-GET-SEC      SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-YMD
      *
           .
       20021-YOKUZITU-GET-EXT.
            EXIT.
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
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
                   TEIKI03-FILE
      *
           IF    ( CNT-OUT1            =   ZERO )
               MOVE    1               TO  ERR-WARNING
               MOVE    "0009"          TO  ERR-ERRCD
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    TEIKI1P-HOSPNUM TO  JOB-HOSPNUM
           MOVE    TEIKI1P-JOBID  TO   JOB-JOBID
           MOVE    TEIKI1P-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA  
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TEIKI01 (ORCT0010) CNT " CNT-OUT1 
           DISPLAY "*** ORCT0010 END ***"
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
      *    期間算出処理
      *****************************************************************
       800-KIKAN-CALC-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSDAY
                                           WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF      ( STS-DAY-RC1   NOT =   ZERO )
               MOVE    1           TO  FLG-ORCSDAY
               GO  TO              800-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       800-KIKAN-CALC-EXT.
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
           WHEN    "0006"
               MOVE    ERR-MSG06       TO  ERR-ERRMSG
           WHEN    "0007"
               MOVE    ERR-MSG07       TO  ERR-ERRMSG
           WHEN    "0008"
               MOVE    ERR-MSG08       TO  ERR-ERRMSG
           WHEN    "0009"
               MOVE    ERR-MSG09       TO  ERR-ERRMSG
           WHEN    "0010"
               MOVE    ERR-MSG10       TO  ERR-ERRMSG
           WHEN    "3000"      THRU    "3999"
               MOVE    S02MSG-MSG (1)  TO  ERR-ERRMSG
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
      *
           .
       800-EDIT-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院チェックサブ
      *****************************************************************
       800-ORCSDNTCHK-SEC              SECTION.
      *
           INITIALIZE                  SDNTCHK-AREA
           MOVE    PTNYUINRRK-REC      TO      LNKNRRK-REC
           MOVE    WRK-TEIKIEDYMD      TO      SDNTCHK-KJNYMD
           CALL    "ORCSDNTCHK"        USING   SDNTCHK-AREA
                                               LNKNRRK-REC
      *
           .
       800-ORCSDNTCHK-EXT.
           EXIT.
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
      *
      *****************************************************************
      *    患者定期請求履歴マスタ　ＲＥＡＤ
      *****************************************************************
       900-PTTEIKIRRK-KEY2-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
           INITIALIZE                  PTTEIKIRRK-REC
      *
           MOVE    PTNYUINRRK-HOSPNUM   TO  PTTEIKIRRK-HOSPNUM
           MOVE    PTNYUINRRK-PTID     TO  PTTEIKIRRK-PTID
           MOVE    TEIKI1P-SRYYM       TO  PTTEIKIRRK-SRYYM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE                  PTTEIKIRRK-REC
               MOVE    1               TO  FLG-PTTEIKIRRK
           END-IF
      *
      *    カーソルクロース
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       900-PTNYUINRRK-KEY4-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK2
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK2
           END-IF
      *
      *    カーソルクロース
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者番号マスター読込（主キー）
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    TEIKI1P-HOSPNUM      TO  PTNUM-HOSPNUM
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
      *
      *****************************************************************
      *    システム管理マスタ読込(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    TEIKI1P-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
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
