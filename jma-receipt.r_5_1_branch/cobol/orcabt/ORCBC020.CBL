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
       PROGRAM-ID.             ORCBC020.
      *****************************************************************
      *  ★修正時には必ずプログラム識別番号をリセットすること
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : データ収集
      *  コンポーネント名  : ＣＳＶデータ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/09/12    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    NACL-伊藤    08/ 4/28  Dカレンダーレコード追加
      *  04.05.01    NACL-伊藤    10/01/28  小数点５桁対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    収集データ（病院基本情報）
           SELECT  COLDAT91-FILE   ASSIGN  COLDAT91
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT91-KEY
                                   FILE    STATUS  IS  STS-COLDAT91.
      *
           SELECT  COLDAT911-FILE  ASSIGN  COLDAT91
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  COLDAT911-KEY
                                   FILE    STATUS  IS  STS-COLDAT91.
      *
      *    収集データ（患者基本情報）
           SELECT  COLDAT01-FILE   ASSIGN  COLDAT01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT01-KEY
                                   FILE    STATUS  IS  STS-COLDAT01.
      *
      *    収集データ（診療情報）
           SELECT  COLDAT02-FILE   ASSIGN  COLDAT02
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT02-KEY
                                   FILE    STATUS  IS  STS-COLDAT02.
      *
      *    収集データ（病名情報）
           SELECT  COLDAT03-FILE   ASSIGN  COLDAT03
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  COLDAT03-KEY
                                   FILE    STATUS  IS  STS-COLDAT03.
      *
      *    ＣＳＶデータ
           SELECT  COLDAT04-FILE   ASSIGN  WRK-PARA-COLFILENM
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-COLDAT04.
      *
      *    エラーファイル
           SELECT  COLDATERR-FILE  ASSIGN  WRK-PARA-ERRFILENM
                                   FILE    STATUS  IS  STS-COLDATERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    病院基本情報データ
       FD  COLDAT91-FILE.
       01  COLDAT91-REC. 
           COPY    "CPCDF901.INC".
      *
       FD  COLDAT911-FILE.
       01  COLDAT911-REC. 
           COPY    "CPCDF901.INC"  REPLACING  //COLDAT91//
                                   BY         //COLDAT911//.
      *
      *    患者基本情報データ
       FD  COLDAT01-FILE.
       01  COLDAT01-REC. 
           COPY    "CPCDF001.INC".
      *
      *    診療情報データ
       FD  COLDAT02-FILE.
       01  COLDAT02-REC. 
           COPY    "CPCDF002.INC".
      *
      *    病名情報データ
       FD  COLDAT03-FILE.
       01  COLDAT03-REC. 
           COPY    "CPCDF003.INC".
      *
      *    ＣＳＶデータ
       FD  COLDAT04-FILE.
       01  COLDAT04-REC            PIC X(3000). 
      *
      *    エラーファイル
       FD  COLDATERR-FILE.
       01  COLDATERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *01  PRGSKBNO                PIC X(08)  VALUE "20070809".
      *01  PRGSKBNO                PIC X(08)  VALUE "20080428".
      *01  PRGSKBNO                PIC X(08)  VALUE "20090327".
       01  PRGSKBNO                PIC X(08)  VALUE "20100128".
      *
      *    病院基本情報データ
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT91//.
      *
      *    患者基本情報データ
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT01//.
      *
      *    診療情報データ
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT02//.
      *
      *    病名情報データ
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //COLDAT03//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-COLDAT91                    PIC X(02).
           03  STS-COLDAT01                    PIC X(02).
           03  STS-COLDAT02                    PIC X(02).
           03  STS-COLDAT03                    PIC X(02).
           03  STS-COLDAT04                    PIC X(02).
           03  STS-COLDATERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
           03  FLG-COLDAT91                    PIC 9(01).
           03  FLG-COLDAT02                    PIC 9(01).
           03  FLG-COLDAT03                    PIC 9(01).
           03  FLG-KOIHEAD                     PIC 9(01).
           03  FLG-SANSO                       PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-COLDAT91                    PIC 9(06).
           03  CNT-COLDAT01                    PIC 9(06).
           03  CNT-COLDAT04                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03).
           03  IDY                             PIC 9(03).
           03  IDZ                             PIC 9(03).
           03  IDU                             PIC 9(03).
           03  IDV                             PIC 9(03).
           03  IDW                             PIC 9(03).
           03  IDX1                            PIC 9(03).
           03  IDX2                            PIC 9(03).
           03  IDX3                            PIC 9(03).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(16).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-SRYYM              PIC X(06).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-COLFILENM          PIC X(100).
               05  WRK-PARA-ERRFILENM          PIC X(100).
      *
           03  WRK-COLDATERR               PIC X(200). 
      * 
           03  WRK-YMD.
               05  WRK-YY              PIC 9(04).
               05  WRK-MM              PIC 9(02).
               05  WRK-DD              PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-KOH-MAX             PIC 9(03).
      *
      *     右ずめ編集用
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-REC                 PIC X(3000).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(200).
               05  WRK-NUM             PIC ZZZZZZZZZ9.99999.
               05  WRK-NAGASA          PIC 9(03).
               05  WRK-ST              PIC 9(02).
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *
      *    ワーク主保険情報
           03  WRK-HKN-AREA.
      *        主保険−保険ＩＤ
               05  WRK-HKN-HKNID       PIC  9(10).
      *        主保険−記号
               05  WRK-HKN-KIGO        PIC  X(80).
      *        主保険−番号
               05  WRK-HKN-NUM         PIC  X(80).
      *        被保険者名
               05  WRK-HKN-HIHKNJANAME
                                       PIC  X(100).
      *        資格取得年月日
               05  WRK-HKN-SKKGETYMD   PIC  X(08).
      *        適用開始年月日
               05  WRK-HKN-TEKSTYMD    PIC  X(08).
      *        適用終了年月日
               05  WRK-HKN-TEKEDYMD    PIC  X(08).
      *        主保険−継続区分
               05  WRK-HKN-CONTKBN     PIC  X(01).
      *        主保険−補助区分
               05  WRK-HKN-HOJOKBN     PIC X(01).
      *        主保険−本人家族区分
               05  WRK-HKN-HONKZKKBN   PIC  X(01).
      * 
      *    ワーク公費情報
           03  WRK-KOHI-AREA.
      *        公費ＩＤ
               07  WRK-KOHID          PIC  9(10).
      *        公費保険番号
               07  WRK-KOHHKNNUM      PIC  X(03).
      *        公費支払区分
               07  WRK-KOHPAYKBN      PIC  X(02).
      *        公費−法別番号
               07  WRK-KOHHBTNUM      PIC  X(02).
      *        公費−負担者番号
               07  WRK-KOHFTNJANUM    PIC  X(08).
      *        公費−受給者番号
               07  WRK-KOHJKYSNUM     PIC  X(20).
      *        公費−レセプト請求区分
               07  WRK-RECESKYKBN     PIC  X(01).
      *        適用開始年月日
               07  WRK-KOHTEKSTYMD    PIC  X(08).
      *        適用終了年月日
               07  WRK-KOHTEKEDYMD    PIC  X(08).
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *    生活保護保険番号
           03  WRK-CONS-SEIHO-HKNNUM   PIC X(03)   VALUE   "012".
      *    自費保険番号
           03  WRK-CONS-JIHI-HKNNUM    PIC X(03)   VALUE   "980".
      *    労災保険番号
           03  WRK-CONS-ROUSAI-HKNNUM  PIC X(03)   VALUE   "971".
      *    自賠責保険番号
           03  WRK-CONS-JIBAI-HKNNUM   PIC X(03)   VALUE   "973".
      *    原爆保険番号
           03  WRK-CONS-GENBAKU-HKNNUM PIC X(03)   VALUE   "019".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    酸素
           COPY    "CMSANSOTBL.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
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
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
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
                                       WRK-AREA
                                       FLG-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-SRYYM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-COLFILENM
                                               WRK-PARA-ERRFILENM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    "ORCBC020"      TO  JOB-PGID
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           STRING  LNK-DAY2-EDTYMD3(5:12)       DELIMITED BY SIZE
                   "分収集データ作成（ＣＳＶ）" DELIMITED BY SIZE
                                 INTO  JOB-SHELLMSG
           END-STRING
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT91-HOSPNUM
           MOVE    "COLDT910"          TO  COLDAT91-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT91-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT01-HOSPNUM
           MOVE    "COLDT010"          TO  COLDAT01-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT01-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT02-HOSPNUM
           MOVE    "COLDT020"          TO  COLDAT02-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT02-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  COLDAT03-HOSPNUM
           MOVE    "COLDT030"          TO  COLDAT03-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  COLDAT03-TERMID
      *
           OPEN    I-O                 COLDAT91-FILE
                                       COLDAT01-FILE
                                       COLDAT02-FILE
                                       COLDAT03-FILE
      *
           PERFORM 900-COLDAT91-READ-SEC
           IF      CNT-COLDAT91        >   ZERO
               OPEN    OUTPUT              COLDAT04-FILE
           ELSE
               MOVE    1               TO  FLG-END
           END-IF
           PERFORM UNTIL        FLG-COLDAT91    =   1
                   PERFORM 1001-FILE-HENSYU-SEC
                   PERFORM 900-COLDAT91-READ-SEC
           END-PERFORM
      *
           PERFORM 900-COLDAT01-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶファイル（病院基本）処理
      *****************************************************************
       1001-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
           EVALUATE  COLDAT91-RECKBN
             WHEN    "1"
      *        レコード識別
               MOVE    "I"               TO  WRK-DATA
               MOVE    1                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        都道府県
               MOVE    COLDAT91-PREFNUM  TO  WRK-NUM
               MOVE    2                 TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        医療機関種別
               MOVE    COLDAT91-HOSPSBT  TO  WRK-NUM
               MOVE    1                 TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        旧総合病院
               MOVE    COLDAT91-HOSPSBT1 TO  WRK-NUM
               MOVE    1                 TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        病床数（許可）
               MOVE    COLDAT91-BEDSU    TO  WRK-NUM
               MOVE    4                 TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        病床数（一般）
               MOVE    COLDAT91-BEDSUIPN TO  WRK-NUM
               MOVE    4                 TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        入外区分
               MOVE    COLDAT91-NYUGAIKBN    TO  WRK-DATA
               MOVE    1                     TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        データ年月
               MOVE    COLDAT91-DATAYM   TO  WRK-DATA
               MOVE    6                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        医療機関ＩＤ
               MOVE    COLDAT91-HOSPID   TO  WRK-DATA
               MOVE    24                TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        プログラム識別番号（抽出）
               MOVE    COLDAT91-PRGSKBNO TO  WRK-DATA
               MOVE    8                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        プログラム識別番号（ＣＳＶ）
               MOVE    PRGSKBNO          TO  WRK-DATA
               MOVE    8                 TO  WRK-NAGASA
               MOVE    1                 TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
             WHEN    "2"
      *        レコード識別
               MOVE    "K"             TO  WRK-DATA
               MOVE    1               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        診療科コード
               MOVE    COLDAT91-SRYKA  TO  WRK-DATA
               MOVE    2               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        レセ電診療科コード
               MOVE    COLDAT91-RECEKA TO  WRK-DATA
               MOVE    2               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        診療科名
               MOVE    COLDAT91-SRYKANAME  TO  WRK-DATA
               MOVE    40                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
             WHEN    "3"
      *        レコード識別
               MOVE    "T"             TO  WRK-DATA
               MOVE    1               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        施設基準コード
               MOVE    COLDAT91-SSTKIJUNCD TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
             WHEN    "4"
      *        レコード識別
               MOVE    "N"             TO  WRK-DATA
               MOVE    1               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        保険番号
               MOVE    COLDAT91-HKNNUM TO  WRK-DATA
               MOVE    3               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        支払区分
               MOVE    COLDAT91-PAYKBN TO  WRK-DATA
               MOVE    2               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        法別番号
               MOVE    COLDAT91-HBTNUM TO  WRK-DATA
               MOVE    2               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        制度名
               MOVE    COLDAT91-SEIDONAME  TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        保険公費種別区分
               MOVE    COLDAT91-HKNKOHSBTKBN TO  WRK-DATA
               MOVE    1                     TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        公費主保区分
               MOVE    COLDAT91-HKNKOHKBN  TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        条件−年齢開始
               MOVE    COLDAT91-JOKEN-STYEAR TO  WRK-NUM
               MOVE    3                     TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        条件−年齢終了
               MOVE    COLDAT91-JOKEN-EDYEAR TO  WRK-NUM
               MOVE    3                     TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        点数単価
               MOVE    COLDAT91-TENTNK TO  WRK-NUM
               MOVE    3               TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        レセプト請求区分
               MOVE    COLDAT91-RECESKYKBN TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
           END-EVALUATE
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       1001-FILE-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-FILE-HENSYU-SEC
           PERFORM 20011-FILE-HENSYU-SEC
           PERFORM 2002-FILE-HENSYU-SEC
      *
      *    診療情報（１件目先読み）
           INITIALIZE                          COLDAT02-REC
           MOVE    COLDAT01-KEY            TO  COLDAT02-KEY1
      *
           START   COLDAT02-FILE
                   KEY IS    >=   COLDAT02-KEY
                   INVALID
                       MOVE    1           TO  FLG-COLDAT02
                   NOT INVALID
                       PERFORM 900-COLDAT02-READ-SEC
           END-START
      *
      *    病名情報
           INITIALIZE                          COLDAT03-REC
           MOVE    COLDAT01-HOSPID         TO  COLDAT03-HOSPID
           MOVE    COLDAT01-SRYYM          TO  COLDAT03-SRYYM
           MOVE    COLDAT01-NYUGAIKBN      TO  COLDAT03-NYUGAIKBN
           MOVE    COLDAT01-PTID           TO  COLDAT03-PTID
           MOVE    COLDAT01-RECEKA         TO  COLDAT03-RECEKA
      *
           START   COLDAT03-FILE
                   KEY IS    >=   COLDAT03-KEY
                   INVALID
                       MOVE    1           TO  FLG-COLDAT03
                   NOT INVALID
                       PERFORM 900-COLDAT03-READ-SEC
           END-START
      *
           PERFORM     UNTIL   FLG-COLDAT03    =   1
                       OR      COLDAT01-KEY (1:43)
                                           NOT =   COLDAT03-KEY (1:43)
                   PERFORM 2003-FILE-HENSYU-SEC
                   PERFORM 900-COLDAT03-READ-SEC
           END-PERFORM
      *
      *    診療情報
           PERFORM     UNTIL   FLG-COLDAT02        =   1
                       OR      COLDAT01-KEY    NOT =   COLDAT02-KEY1
                   PERFORM 2004-FILE-HENSYU-SEC
                   PERFORM 900-COLDAT02-READ-SEC
           END-PERFORM
      *
           PERFORM 900-COLDAT01-READ-SEC
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶファイル（患者基本）処理
      *****************************************************************
       2001-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    レコード識別
           MOVE    "P"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    審査支払機関
           MOVE    COLDAT01-TEISYUTUSAKI
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レセプト種別
           MOVE    COLDAT01-RECESYUBETU
                                       TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月
           MOVE    WRK-PARA-SRYYM      TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    性別
           MOVE    COLDAT01-SEX        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
           MOVE    COLDAT01-BIRTHDAY   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    入院年月日
           MOVE    COLDAT01-NYUINYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    退院年月日
           MOVE    COLDAT01-TAIINYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    患者ＩＤ
           MOVE    SPACE               TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    郵便番号
           MOVE    COLDAT01-HOME-POST      TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           MOVE    1                       TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2001-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶファイル（保険組合せ）処理
      *****************************************************************
       20011-FILE-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    レコード識別
           MOVE    "G"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険番号
           MOVE    COLDAT01-HKNNUM     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険番号（老人保健）
           MOVE    COLDAT01-RJNHKNNUM  TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    実日数（保険）
           IF        COLDAT01-HKNJNISSU    =   0
             MOVE    SPACE                 TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    COLDAT01-HKNJNISSU    TO  WRK-NUM
             MOVE    2                     TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *    合計点数（保険）
           IF        COLDAT01-HKNTOTALTENSU  =   0
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    COLDAT01-HKNTOTALTENSU  TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *    食事療養回数（保険）
           IF        COLDAT01-HKNSKJNISSU  =   0
             MOVE    SPACE                 TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    COLDAT01-HKNSKJNISSU  TO  WRK-NUM
             MOVE    2                     TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *    食事療養費（保険）
           IF        COLDAT01-HKNSKJRYOYOHI  =   0
             MOVE    SPACE                   TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    COLDAT01-HKNSKJRYOYOHI  TO  WRK-NUM
             MOVE    8                       TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *    食事療養費標準負担額（保険）
           IF        COLDAT01-HKNSKJFTN    =   0
             MOVE    SPACE                 TO  WRK-DATA
             MOVE    1                     TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    COLDAT01-HKNSKJFTN    TO  WRK-NUM
             MOVE    8                     TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *公費情報
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
      *      公費番号
             MOVE    COLDAT01-TKOHHKNNUM (IDX1)    TO  WRK-DATA
             MOVE    3                             TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      実日数
             IF        COLDAT01-TKOHJNISSU (IDX1)  =   0
               MOVE    SPACE                       TO  WRK-DATA
               MOVE    1                           TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
             ELSE
               MOVE    COLDAT01-TKOHJNISSU (IDX1)  TO  WRK-NUM
               MOVE    2                           TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
             END-IF
      *      合計点数
             IF        COLDAT01-TKOHTOTALTENSU (IDX1)  =   0
               MOVE    SPACE                       TO  WRK-DATA
               MOVE    1                           TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
             ELSE
               MOVE    COLDAT01-TKOHTOTALTENSU (IDX1)
                                                   TO  WRK-NUM
               MOVE    8                           TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
             END-IF
      *      食事療養回数
             IF        COLDAT01-TKOHSKJNISSU (IDX1)  =   0
               MOVE    SPACE                       TO  WRK-DATA
               MOVE    1                           TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
             ELSE
               MOVE    COLDAT01-TKOHSKJNISSU (IDX1)
                                                   TO  WRK-NUM
               MOVE    2                           TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
             END-IF
      *      食事療養費
             IF        COLDAT01-TKOHSKJRYOYOHI (IDX1)  =   0
               MOVE    SPACE                       TO  WRK-DATA
               MOVE    1                           TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
             ELSE
               MOVE    COLDAT01-TKOHSKJRYOYOHI (IDX1)
                                                   TO  WRK-NUM
               MOVE    8                           TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
             END-IF
      *      食事療養費標準負担額
             IF        COLDAT01-TKOHSKJFTN (IDX1)  =   0
               MOVE    SPACE                       TO  WRK-DATA
               MOVE    1                           TO  WRK-NAGASA
               IF      IDX1                        =   4
                       MOVE    1                   TO  WRK-END
               END-IF
               PERFORM 5004-MOJI-HENSYU-SEC
             ELSE
               MOVE    COLDAT01-TKOHSKJFTN (IDX1)  TO  WRK-NUM
               MOVE    8                           TO  WRK-NAGASA
               IF      IDX1                        =   4
                       MOVE    1                   TO  WRK-END
               END-IF
               PERFORM 5005-NUM-HENSYU-SEC
             END-IF
           END-PERFORM
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20011-FILE-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶファイル（保険組合せ）処理
      *****************************************************************
       2002-FILE-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
                   OR      COLDAT01-HKNCOMBI (IDX)  =   ZERO 
      *
             MOVE    SPACE               TO  WRK-REC
             MOVE    ZERO                TO  WRK-REC-LENGTH
      *      レコード識別
             MOVE    "H"                 TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      保険組合せ
             MOVE    COLDAT01-HKNCOMBI (IDX)   TO  WRK-NUM
             MOVE    4                         TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *      保険番号
             MOVE    COLDAT01-HKNNUM     TO  WRK-DATA
             MOVE    3                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      本人家族区分
             MOVE    COLDAT01-HONKZKKBN  TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      補助区分
             MOVE    COLDAT01-HOJOKBN    TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *
      *公費情報
      *
             MOVE    ZERO                TO  WRK-KOH-MAX
      *
             IF      COLDAT01-KOHID      >   ZERO
               MOVE    COLDAT01-RJNHKNNUM  TO  WRK-KOHHKNNUM
               MOVE    COLDAT01-KOHID      TO  WRK-KOHID
               MOVE    COLDAT01-RJNPAYKBN  TO  WRK-KOHPAYKBN
               MOVE    COLDAT01-RJNTEKSTYMD
                                           TO  WRK-KOHTEKSTYMD
               MOVE    COLDAT01-RJNTEKEDYMD
                                           TO  WRK-KOHTEKEDYMD
               PERFORM 20021-KOH-HENSYU-SEC
             END-IF
      *
             PERFORM VARYING IDX1    FROM    1   BY  1
                     UNTIL   IDX1    >       4
                     OR      WRK-KOH-MAX     =   4 
               INITIALIZE                  WRK-KOHI-AREA
               IF      COLDAT01-KOHIDHC (IDX IDX1)  >   ZERO
                   PERFORM VARYING IDX2    FROM    1   BY  1
                           UNTIL   IDX2    >       10
                           OR      COLDAT01-TKOHID (IDX2)  =   ZERO
                       IF      COLDAT01-KOHIDHC (IDX IDX1)  =
                                               COLDAT01-TKOHID (IDX2)
                           MOVE    COLDAT01-KOHI-TBL (IDX2)
                                                       TO  WRK-KOHI-AREA
                           PERFORM 20021-KOH-HENSYU-SEC
                           MOVE    10                  TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
             END-PERFORM
      *
             PERFORM VARYING IDX1    FROM    1   BY  1
                     UNTIL   IDX1    >       4
                     OR      WRK-KOH-MAX     =   4 
               INITIALIZE                  WRK-KOHI-AREA
               PERFORM 20021-KOH-HENSYU-SEC
             END-PERFORM
      *
      *労災情報
      *
      *      保険区分
             MOVE    COLDAT01-HKNKBN         TO  WRK-DATA
             MOVE    1                       TO  WRK-NAGASA
             MOVE    1                       TO  WRK-END
             PERFORM 5004-MOJI-HENSYU-SEC
      *
             PERFORM 5001-FILE-WRITE-SEC
           END-PERFORM
           .
       2002-FILE-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶファイル（基本データ）公費情報編集処理
      *****************************************************************
       20021-KOH-HENSYU-SEC        SECTION.
      *
      *    公費保険番号
           MOVE    WRK-KOHHKNNUM       TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           ADD     1                   TO  WRK-KOH-MAX
           .
       20021-KOH-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶファイル（病名データ）処理
      *****************************************************************
       2003-FILE-HENSYU-SEC         SECTION.
      *
      *    労災・自賠責のときは保険組合せでチェック
           EVALUATE    COLDAT01-HKNNUM
               WHEN    WRK-CONS-ROUSAI-HKNNUM
               WHEN    WRK-CONS-JIBAI-HKNNUM
                   IF      COLDAT03-HKNCOMBI   =   ZERO
                       GO  TO  2003-FILE-HENSYU-EXT
                   END-IF
      *
                   MOVE    ZERO            TO  IDZ  
                   PERFORM VARYING IDX     FROM    1   BY  1
                           UNTIL   IDX     >       10
                           OR      COLDAT01-HKNCOMBI (IDX)   =   ZERO
                       IF      COLDAT03-HKNCOMBI   =
                                            COLDAT01-HKNCOMBI (IDX)
                           MOVE    IDX             TO  IDZ
                           MOVE    10              TO  IDX
                       END-IF
                   END-PERFORM
                   IF      IDZ            =   ZERO
                       GO  TO  2003-FILE-HENSYU-EXT
                   END-IF
           END-EVALUATE
      *
      *病名情報
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    レコード識別
           MOVE    "B"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療科
           MOVE    COLDAT03-SRYKA      TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療開始日
           MOVE    COLDAT03-SRYYMD     TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    転帰区分
           MOVE    COLDAT03-TENKIKBN   TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    転帰日付
           MOVE    COLDAT03-TENKIYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    主病名フラグ
           MOVE    COLDAT03-SYUBYOFLG  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    疑いフラグ
           MOVE    COLDAT03-UTAGAIFLG  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険病名フラグ
           MOVE    COLDAT03-HKNBYOMEIFLG
                                       TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険組合せ
           IF      COLDAT03-HKNCOMBI   =   0
             MOVE    SPACE             TO  WRK-DATA
             MOVE    1                 TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
             MOVE    COLDAT03-HKNCOMBI TO  WRK-NUM
             MOVE    4                 TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
           END-IF
      *    病名
           MOVE    COLDAT03-BYOMEI     TO  WRK-DATA
           MOVE    200                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    病名コード数
           MOVE    COLDAT03-BYOMEICDSU TO  WRK-NUM
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    病名コード
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       21
               MOVE    COLDAT03-BYOMEICD  (IDX1)   TO  WRK-DATA
               MOVE    7                           TO  WRK-NAGASA
               IF      IDX1        =       21
                   MOVE    1           TO  WRK-END
               END-IF
               PERFORM 5004-MOJI-HENSYU-SEC
           END-PERFORM
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       2003-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶファイル（診療データ）処理
      *****************************************************************
       2004-FILE-HENSYU-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *
      *    用法コードは記録しない
           IF      COLDAT02-SRYCD(1:3) =   "001"
                   GO    TO    2004-FILE-HENSYU-EXT
           END-IF
      *
      *    先頭がコメントレコードの場合は点数回数の記録が特殊
           IF      COLDAT02-SEQNO      =   1
               MOVE  ZERO              TO  FLG-KOIHEAD
           END-IF
      *
      *    レコード識別
           EVALUATE  COLDAT02-SRYCD (1:1)
               WHEN  "6"
                   MOVE    "Y"         TO  WRK-DATA
                   IF  COLDAT02-JIHOKBN    =  "1"
                       MOVE    "Q"     TO  WRK-DATA
                   END-IF
               WHEN  "7"
                   MOVE    "Z"         TO  WRK-DATA
                   IF  COLDAT02-JIHOKBN    =  "1"
                       MOVE    "R"     TO  WRK-DATA
                   END-IF
               WHEN  "8"
                   MOVE    "C"         TO  WRK-DATA
                   IF  COLDAT02-JIHOKBN    =  "1"
                       MOVE    "U"     TO  WRK-DATA
                   END-IF
               WHEN  OTHER
                   MOVE    "S"         TO  WRK-DATA
                   IF  COLDAT02-JIHOKBN    =  "1"
                       MOVE    "O"     TO  WRK-DATA
                   END-IF
           END-EVALUATE
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    診療情報
      *
           IF      COLDAT02-SEQNO      =   1
      *      診療科
             MOVE    COLDAT02-SRYKA      TO  WRK-DATA
             MOVE    2                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      保険組合せ
             MOVE    COLDAT02-HKNCOMBI   TO  WRK-NUM
             MOVE    4                   TO  WRK-NAGASA
             PERFORM 5005-NUM-HENSYU-SEC
      *      診療行為区分
             MOVE    COLDAT02-SRYKBN     TO  WRK-DATA
             MOVE    2                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      診療種別区分
             MOVE    COLDAT02-SRYSYUKBN  TO  WRK-DATA
             MOVE    3                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
      *      診療科
             MOVE    SPACE               TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      保険組合せ
             MOVE    SPACE               TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      診療行為区分
             MOVE    SPACE               TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
      *      診療種別区分
             MOVE    SPACE               TO  WRK-DATA
             MOVE    1                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
      *    診療行為コード
           MOVE    COLDAT02-SRYCD      TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           IF      WRK-REC(1:1)    NOT =   "C"
      *      数量
             MOVE    COLDAT02-SRYSURYO   TO  WRK-NUM
             MOVE    5                   TO  WRK-NAGASA
             MOVE    5                   TO  WRK-SYOSUU
             PERFORM 5005-NUM-HENSYU-SEC
             IF      FLG-KOIHEAD       =   ZERO
      *        剤点数
               MOVE  COLDAT02-ZAITEN     TO  WRK-NUM
               MOVE  8                   TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        剤回数
               MOVE  COLDAT02-ZAIKAISU   TO  WRK-NUM
               MOVE  7                   TO  WRK-NAGASA
               IF    WRK-REC(1:1)        =   "Y"
                 MOVE    1               TO  WRK-END
               END-IF
               PERFORM 5005-NUM-HENSYU-SEC
               IF     (WRK-REC(1:1)    NOT =   "S")  AND
                      (WRK-REC(1:1)    NOT =   "O")
                   MOVE    1                 TO  FLG-KOIHEAD
               END-IF
             ELSE
      *        剤点数
               MOVE  SPACE               TO  WRK-DATA
               MOVE  1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        剤回数
               MOVE  SPACE               TO  WRK-DATA
               MOVE  1                   TO  WRK-NAGASA
               IF    WRK-REC(1:1)        =   "Y"
                 MOVE    1               TO  WRK-END
               END-IF
               PERFORM 5004-MOJI-HENSYU-SEC
             END-IF
           END-IF
      *
           IF      WRK-REC(1:1)        =   "S"  OR  "O"
             IF      FLG-KOIHEAD       =   ZERO
      *        手技点数１
               IF    COLDAT02-SYUTEN1    =   0
                 MOVE  SPACE             TO  WRK-DATA
                 MOVE  1                 TO  WRK-NAGASA
                 PERFORM 5004-MOJI-HENSYU-SEC
               ELSE
                 MOVE  COLDAT02-SYUTEN1  TO  WRK-NUM
                 MOVE  7                 TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
               END-IF
      *        薬剤点数
               IF    COLDAT02-YKZTEN     =   0
                 MOVE  SPACE             TO  WRK-DATA
                 MOVE  1                 TO  WRK-NAGASA
                 PERFORM 5004-MOJI-HENSYU-SEC
               ELSE
                 MOVE  COLDAT02-YKZTEN   TO  WRK-NUM
                 MOVE  7                 TO  WRK-NAGASA
                 PERFORM 5005-NUM-HENSYU-SEC
               END-IF
      *        器材点数
               IF    COLDAT02-KIZAITEN   =   0
                 MOVE  SPACE             TO  WRK-DATA
                 MOVE  1                 TO  WRK-NAGASA
                 MOVE  1                 TO  WRK-END
                 PERFORM 5004-MOJI-HENSYU-SEC
               ELSE
                 MOVE  COLDAT02-KIZAITEN TO  WRK-NUM
                 MOVE  7                 TO  WRK-NAGASA
                 IF    COLDAT02-SRYCD (1:3)  >= "099"
                   MOVE  1               TO  WRK-END
                 END-IF
                 PERFORM 5005-NUM-HENSYU-SEC
               END-IF
               MOVE  1                   TO  FLG-KOIHEAD
             ELSE
      *        手技点数１
               MOVE  SPACE               TO  WRK-DATA
               MOVE  1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        薬剤点数
               MOVE  SPACE               TO  WRK-DATA
               MOVE  1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        器材点数
               MOVE  SPACE               TO  WRK-DATA
               MOVE  1                   TO  WRK-NAGASA
               IF    COLDAT02-SRYCD (1:3)  >= "099"
                 MOVE  1                 TO  WRK-END
               END-IF
               PERFORM 5004-MOJI-HENSYU-SEC
             END-IF
           END-IF
      *
           IF      WRK-REC(1:1)        =   "Z"
      *
      *    単位コード
           MOVE    COLDAT02-TANICD     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    単価
           PERFORM 20041-SANSO-CHECK-SEC
           IF     (COLDAT02-TENSIKIBETU =  2)  OR
                  (FLG-SANSO            =  1)
               MOVE    COLDAT02-TEN    TO  WRK-NUM
               MOVE    9               TO  WRK-NAGASA
               MOVE    2               TO  WRK-SYOSUU
               PERFORM 5005-NUM-HENSYU-SEC
           ELSE
               MOVE    SPACE           TO  WRK-DATA
               MOVE    1               TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
      *
           END-IF
      *
           IF      (WRK-REC(1:1)       =   "Z" OR "C")
             OR  (((WRK-REC(1:1)       =   "S")
             OR    (WRK-REC(1:1)       =   "O"))
             AND   (COLDAT02-SRYCD (1:3) < "099"))
      *
      *      名称
             MOVE    COLDAT02-NAME       TO  WRK-DATA
             MOVE    40                  TO  WRK-NAGASA
             MOVE    1                   TO  WRK-END
             PERFORM 5004-MOJI-HENSYU-SEC
      *
           END-IF
      *
           PERFORM 5001-FILE-WRITE-SEC
      *
           IF      COLDAT02-SEQNO      =   1
             PERFORM 20042-CALENDAR-MAKE-SEC
           END-IF
           .
       2004-FILE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    酸素チェック
      *****************************************************************
       20041-SANSO-CHECK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SANSO
           IF      COLDAT02-SRYKBN     =   "14" OR "40" OR "50"
                                        OR "60" OR "70" OR "80"
               SET    TBL-SANSO        TO  1
               SEARCH TBL-SANSO-OCC  VARYING  TBL-SANSO
                   AT END
                      MOVE  ZERO       TO  FLG-SANSO
                   WHEN    COLDAT02-SRYCD  =  TBL-SANSO-SRYCD
                                                  (TBL-SANSO)
                      MOVE  1          TO  FLG-SANSO
               END-SEARCH
           END-IF
           .
       20041-SANSO-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カレンダーレコード作成
      *****************************************************************
       20042-CALENDAR-MAKE-SEC     SECTION.
      *
           MOVE    "D,"                TO  WRK-REC
           MOVE    2                   TO  WRK-REC-LENGTH
      *
           PERFORM VARYING    IDX3     FROM  1  BY  1
                   UNTIL      IDX3     >     31
               IF    IDX3              =   31
                   MOVE    1                   TO  WRK-END
               END-IF
               IF      COLDAT02-KAISU(IDX3)    =   ZERO
                   MOVE    SPACE               TO  WRK-DATA
                   MOVE    1                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
               ELSE
                   MOVE    COLDAT02-KAISU(IDX3)
                                               TO  WRK-NUM
                   MOVE    3                   TO  WRK-NAGASA
                   PERFORM 5005-NUM-HENSYU-SEC
               END-IF
           END-PERFORM
      *
           PERFORM 5001-FILE-WRITE-SEC
           .
       20042-CALENDAR-MAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  COLDAT04-REC
           WRITE   COLDAT04-REC
           ADD     1                   TO  CNT-COLDAT04
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    右ずめ編集処理
      *****************************************************************
       5003-DATA-RIGHT-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE          TO  WRK-HENSYU
      *
           IF      WRK-HENKAN     =   SPACE
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    WRK-LENGTH  BY  -1
                       UNTIL   IDX <       1
                   IF      WRK-HENKAN (IDX:1)  NOT =   SPACE
                       COMPUTE IDZ     =   WRK-LENGTH  -   IDX +   1
                       MOVE    WRK-HENKAN (1:IDX)
                                           TO  WRK-HENSYU (IDZ:)
                       MOVE    WRK-HENSYU  TO  WRK-HENKAN
                       MOVE    1           TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5003-DATA-RIGHT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（文字）
      *****************************************************************
       5004-MOJI-HENSYU-SEC          SECTION.
      *
           IF      WRK-DATA        =   SPACE
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
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
               ADD     1                   TO  WRK-REC-LENGTH
           END-IF
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5004-MOJI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.99999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
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
      *
           INITIALIZE                      WRK-HENSYU-AREA
           .
       5005-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   COLDATERR-FILE
           IF      STS-COLDATERR         =   ZERO
               CLOSE   COLDATERR-FILE
           ELSE
               OPEN    OUTPUT              COLDATERR-FILE
      *
               MOVE    WRK-COLDATERR         TO  COLDATERR-REC
               WRITE   COLDATERR-REC
               CLOSE   COLDATERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-COLDATERR   TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           END-IF
      *
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   COLDAT91-FILE
                   COLDAT01-FILE
                   COLDAT02-FILE
                   COLDAT03-FILE
           IF      CNT-COLDAT91        >   ZERO
               CLOSE   COLDAT04-FILE
           END-IF
      *
      **   IF      CNT-COLDAT01           =   ZERO  
      **       MOVE    "処理対象のデータがありませんでした"
      **                                   TO  WRK-COLDATERR
      **       PERFORM 500-ERR-HENSYU-SEC
      **   END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "COLDAT91  CNT-IN :" CNT-COLDAT91
           DISPLAY "COLDAT01  CNT-IN :" CNT-COLDAT01
           DISPLAY "COLDAT04  CNT-OUT:" CNT-COLDAT04
           DISPLAY "*** ORCBC020 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データ（病院基本情報）読込
      *****************************************************************
       900-COLDAT91-READ-SEC             SECTION.
      *
           READ    COLDAT91-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-COLDAT91
               NOT AT  END
                   ADD     1         TO  CNT-COLDAT91
           END-READ
           .
       900-COLDAT91-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データ（患者基本情報）読込
      *****************************************************************
       900-COLDAT01-READ-SEC             SECTION.
      *
           READ    COLDAT01-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-END
               NOT AT  END
                   ADD     1         TO  CNT-COLDAT01
           END-READ
           .
       900-COLDAT01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データ（診療情報）読込
      *****************************************************************
       900-COLDAT02-READ-SEC             SECTION.
      *
           READ    COLDAT02-FILE   NEXT
               AT  END
                   MOVE    1           TO  FLG-COLDAT02
               NOT AT  END
                   MOVE    ZERO        TO  FLG-COLDAT02
           END-READ
           .
       900-COLDAT02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収集データ（病名情報）読込
      *****************************************************************
       900-COLDAT03-READ-SEC             SECTION.
      *
           READ    COLDAT03-FILE   NEXT
               AT  END
                   MOVE    1           TO  FLG-COLDAT03
               NOT AT  END
                   MOVE    ZERO        TO  FLG-COLDAT03
           END-READ
           .
       900-COLDAT03-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
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
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBCLOSE-EXT.
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
