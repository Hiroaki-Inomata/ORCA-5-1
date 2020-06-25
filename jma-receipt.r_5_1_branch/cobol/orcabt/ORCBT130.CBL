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
       PROGRAM-ID.             ORCBT130.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 日次統計ＣＳＶデータ作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/03/31    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-藤原    07/09/19  クライアント保存対応
      *
      *  04.05.01    NACL-藤原    10/02/12  拡張漢字対応
      *  04.05.02    NACL-藤原    12/06/22  処理結果画面への件数表示
      *  04.05.03    NACL-藤原    12/07/02  住所欄に番地方書を追加
      *
      *  04.06.01    NACL-藤原    10/06/21  日本語化（見出し）対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    日次統計データ（基本情報）
           SELECT  TOUKEI04-FILE   ASSIGN  TOUKEI04PARA
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI04.
      *
      *    ＣＳＶデータ（診療情報） 
           SELECT  TOUKEI05-FILE   ASSIGN  TOUKEI05PARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-TOUKEI05.
      *    エラーファイル
           SELECT  TOUKEIERR-FILE  ASSIGN  TOUKEIERR
                                   FILE    STATUS  IS  STS-TOUKEIERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    日次統計データ（基本情報）
       FD  TOUKEI04-FILE.
       01  TOUKEI04-REC. 
           COPY    "CPTCF004.INC".
      *
      *    ＣＳＶデータ（診療情報）
       FD  TOUKEI05-FILE.
       01  TOUKEI05-REC            PIC X(3000). 
      *
      *    エラーファイル
       FD  TOUKEIERR-FILE.
       01  TOUKEIERR-REC           PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    日次統計データ（基本情報）
      *
           COPY    "CPCOMMONDAT5.INC"
                                   REPLACING  //TEIKI01PARA//
                                   BY         //TOUKEI04PARA//.
      *
      *    ＣＳＶデータ（診療情報）
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //TOUKEI05PARA//.
      *01  TOUKEI05PARA.
      *****03  FILLER                  PIC  X(05) VALUE "/tmp/".
       01  TOUKEI05PARA-BASENAME.
           03  TOUKEI05PARA-HOSPNUM    PIC  9(02).
           03  TOUKEI05PARA-FILE-ID    PIC  X(12) VALUE "toukei11.dat".
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //TOUKEIERR//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-TOUKEI04                    PIC X(02).
           03  STS-TOUKEI05                    PIC X(02).
           03  STS-TOUKEIERR                   PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-TOUKEI04                    PIC 9(06).
           03  CNT-TOUKEI05                    PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                             PIC 9(03). 
           03  IDY                             PIC 9(03). 
           03  IDZ                             PIC 9(03).
           03  IDW                             PIC 9(03).
           03  IDX1                            PIC 9(03). 
           03  IDX2                            PIC 9(03). 
           03  IDX3                            PIC 9(03). 
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-TERMID             PIC X(32).
               05  WRK-PARA-KOJINKBN           PIC X(01).
               05  WRK-PARA-JOBID              PIC 9(07).
               05  WRK-PARA-SHELLID            PIC X(08).
               05  WRK-PARA-HOSPNUM            PIC 9(02).
               05  WRK-PARA-FILENM1            PIC X(150).
               05  WRK-PARA-FILE1              PIC X(20).
               05  WRK-PARA-FILENM1-FOLDER     PIC X(150).
      *
           03  WRK-TOUKEIERR           PIC X(200). 
           03  WRK-RENNUM              PIC 9(03).
      *
      *     右づめ編集用     
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-REC                 PIC X(3000).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(400).
               05  WRK-NUM             PIC ZZZZZZZZZ9.999. 
               05  WRK-NAGASA          PIC 9(03).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(12)   VALUE   "医療機関ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "医療機関コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "診療年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "レコード識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセプト種別コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "レセ電診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険組合せ番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "漢字氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "性別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "生年月日".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "基本料区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "時間外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "往診区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "紹介区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "入外中外来識別".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "医師コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "入退院区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "病棟番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "病室番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "外泊区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "食事区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "退院事由".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
               "前日診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(22)   VALUE
               "前日レセ電診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "前日病棟番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "前日病室番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "伝票番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "伝票状態区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "伝票作成区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "保険負担金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険適用外金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "保険適用外税額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "自費金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "自費税額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "食事負担金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "消費税額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "室料差額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "室料差額税額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "請求金額".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "保険ＩＤ".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "保険者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "本人家族区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "補助区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "継続区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分１".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分２".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分３".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費番号４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "公費ＩＤ４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "支払区分４".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "労災保険区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(14)   VALUE   "自賠責請求区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "郵便番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(18)   VALUE
               "地方公共団体コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "住所".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "電話番号".
      *  
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI              PIC X(01)   VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO              PIC X(01)   VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    クライアント保存ＤＢ制御サブ
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
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
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-TERMID
                                               WRK-PARA-KOJINKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               TOUKEIERR
                                               WRK-PARA-FILENM1
                                               WRK-PARA-FILE1
                                               WRK-PARA-FILENM1-FOLDER
           END-UNSTRING 
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *     
           MOVE    "TOKEI110"          TO  TOUKEI04PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  TOUKEI04PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI04PARA-HOSPNUM
      *
           MOVE    WRK-PARA-HOSPNUM    TO  TOUKEI05PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOUKEIERR           TO  SGETTEMP-BASENAMES  (1)
           MOVE    TOUKEI04PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    TOUKEI05PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOUKEIERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  TOUKEI04PARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  TOUKEI05PARA
      *
           MOVE   SGETTEMP-TEMPDIR     TO  WRK-SGETTEMP-TEMPDIR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBT130"      TO  JOB-PGID
           MOVE    "ＣＳＶデータ作成"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           OPEN    INPUT               TOUKEI04-FILE
                   OUTPUT              TOUKEI05-FILE
      *
           PERFORM 900-TOUKEI04-READ-SEC   
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
           PERFORM 2002-CSV-HEAD-HENSYU-SEC
      *     
      *    ＣＳＶファイル（診療データ）処理
           PERFORM 2001-FILE-HENSYU-SEC
      *
           PERFORM 900-TOUKEI04-READ-SEC   
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶファイル（診療データ）処理
      *****************************************************************
       2001-FILE-HENSYU-SEC         SECTION.
      *
      *    基本情報取得
      *
           MOVE    SPACE               TO  WRK-REC
           MOVE    ZERO                TO  WRK-REC-LENGTH
      *    医療機関ＩＤ      
           MOVE    TOUKEI04-HOSPID     TO  WRK-DATA
           MOVE    24                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    医療機関コード
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-HOSPCD     TO  WRK-DATA
               MOVE    7                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療年月
           MOVE    TOUKEI04-SRYYMD     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    レコード識別
           MOVE    TOUKEI04-RECSKB     TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    レセプト種別
           MOVE    TOUKEI04-RECESYUBETU
                                       TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    入外区分
           MOVE    TOUKEI04-NYUGAIKBN  TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    診療科
           MOVE    TOUKEI04-SRYKA      TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レセ電診療科
           MOVE    TOUKEI04-RECEKA     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    保険組合せ
           MOVE    TOUKEI04-HKNCOMBINUM
                                       TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *
      *    患者情報
      *  
      *    患者番号
           MOVE    TOUKEI04-PTNUM      TO  WRK-DATA
           MOVE    20                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    患者ＩＤ
           MOVE    TOUKEI04-PTID       TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC  
      *    カナ氏名
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-KANANAME   TO  WRK-DATA
               MOVE    50                  TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    氏名
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-NAME       TO  WRK-DATA
               MOVE    100                 TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    性別
           MOVE    TOUKEI04-SEX        TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    生年月日
           MOVE    TOUKEI04-BIRTHDAY   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *    外来情報
      *
      *    基本料区分
           MOVE    TOUKEI04-KIHONKBN   TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    時間外区分
           MOVE    TOUKEI04-TIMEKBN    TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    往診区分
           MOVE    TOUKEI04-OSINKBN    TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    紹介区分
           MOVE    TOUKEI04-SHOUKAIKBN TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    入院中外来識別
           MOVE    TOUKEI04-NYUHAISKB  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    医師コード
           MOVE    TOUKEI04-DRCD       TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *
      *    入院情報
      *
      *    入退院区分（１：当日入院　２：当日退院　３：入院中）
           MOVE    TOUKEI04-NYUIN-KBN  TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    病棟番号
           MOVE    TOUKEI04-BTUNUM     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    病室番号
           MOVE    TOUKEI04-BRMNUM     TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    外泊区分
           MOVE    TOUKEI04-GAIHAKU-KBN
                                       TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    食事区分
           MOVE    TOUKEI04-SKJ-KBN    TO  WRK-NUM
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    退院事由
           MOVE    TOUKEI04-TAIINCD    TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    前日診療科
           MOVE    TOUKEI04-MAE-SRYKA  TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    前日レセ電診療科
           MOVE    TOUKEI04-MAE-RECEKA TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    前日病棟番号
           MOVE    TOUKEI04-MAE-BTUNUM TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    前日病室番号
           MOVE    TOUKEI04-MAE-BRMNUM
                                       TO  WRK-DATA
           MOVE    6                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *   
      *    収納情報情報
      *
      *    伝票番号
           MOVE    TOUKEI04-DENPNUM    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    伝票状態区分
           MOVE    TOUKEI04-DENPJTIKBN TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    伝票作成区分
           MOVE    TOUKEI04-CREATEKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC    
      *    保険点数
           MOVE    TOUKEI04-HKNTEN     TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    保険負担金額
           MOVE    TOUKEI04-HKNMONEY   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    保険適用外金額
           MOVE    TOUKEI04-TGMONEY    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    保険適用外金税額
           MOVE    TOUKEI04-TGMONEY-TAX
                                       TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    自費金額
           MOVE    TOUKEI04-JIHI       TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    自費税額
           MOVE    TOUKEI04-JIHI-TAX   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    食事負担金額
           MOVE    TOUKEI04-SKJ        TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    消費税額
           MOVE    TOUKEI04-SKJ-TAX    TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    室料差額
           MOVE    TOUKEI04-RMSAGAKU   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    室料差額税額
           MOVE    TOUKEI04-RMSAGAKU-TAX
                                       TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *    請求金額
           MOVE    TOUKEI04-SKYMONEY   TO  WRK-NUM
           MOVE    7                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC    
      *
      *保険情報
      * 
      *    保険番号
           MOVE    TOUKEI04-HKNNUM     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    保険ＩＤ
           MOVE    TOUKEI04-HKNID      TO  WRK-NUM
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *    保険者番号
           MOVE    TOUKEI04-HKNJANUM   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    本人家族区分
           MOVE    TOUKEI04-HONKZKKBN  TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC  
      *    補助区分
           MOVE    TOUKEI04-HOJOKBN    TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    継続区分
           MOVE    TOUKEI04-CONTKBN    TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    
      *公費情報
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       4
      *        公費−公費番号
               MOVE    TOUKEI04-KOH-HKNNUM (IDX1)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC  
      *        公費−公費ＩＤ
               MOVE    TOUKEI04-KOH-ID     (IDX1)
                                           TO  WRK-NUM
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        公費−支払区分
               MOVE    TOUKEI04-KOH-PAYKBN  (IDX1)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-PERFORM
      *
      *労災情報
      *    
      *    労災−労災保険区分
           MOVE    TOUKEI04-RSIHKNKBN      TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    労災−自賠責請求区分
           MOVE    TOUKEI04-JIBAISEIKBN    TO  WRK-DATA
           MOVE    1                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
      *患者基本情報
      *
      *    郵便番号
           MOVE    TOUKEI04-HOME-POST      TO  WRK-DATA
           MOVE    7                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    地方公共団体コード
           MOVE    TOUKEI04-LPUBCD         TO  WRK-DATA
           MOVE    5                       TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    住所
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-HOME-ADRS      TO  WRK-DATA
               MOVE    400                     TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    電話番号
           IF      WRK-PARA-KOJINKBN   =   "1"
               MOVE    TOUKEI04-HOME-TEL       TO  WRK-DATA
               MOVE    15                      TO  WRK-NAGASA
           END-IF
           MOVE    1                       TO  WRK-END 
           PERFORM 5004-MOJI-HENSYU-SEC
      * 
           PERFORM 5001-FILE-WRITE-SEC
           .
       2001-FILE-SYORI-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ＣＳＶ（ＨＥＡＤレコード）編集処理
      *****************************************************************
       2002-CSV-HEAD-HENSYU-SEC                SECTION.
      *
           IF      CNT-TOUKEI05        >   ZERO
               GO  TO  2002-CSV-HEAD-HENSYU-EXT
           END-IF
      *
           INITIALIZE                  WRK-REC
                                       WRK-REC-LENGTH
      *
           STRING  CSV-HEAD-01         DELIMITED  BY  SIZE
                   WRK-KAIGYO          DELIMITED  BY  SIZE
                   INTO                WRK-REC
           END-STRING
      *
           MOVE    WRK-REC             TO  TOUKEI05-REC  
           WRITE   TOUKEI05-REC 
           .
       2002-CSV-HEAD-HENSYU-EXT.
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
           MOVE    WRK-REC             TO  TOUKEI05-REC  
           WRITE   TOUKEI05-REC 
           ADD     1                   TO  CNT-TOUKEI05
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
      *
      *****************************************************************
      *    出力レコード処理（数字）ZZZZZZZZZ9.999の編集
      *****************************************************************
       5005-NUM-HENSYU-SEC          SECTION.
      *
           IF      WRK-NAGASA          =   ZERO
               CONTINUE
           ELSE                   
      *        開始位置
               COMPUTE WRK-ST  =   11      -   WRK-NAGASA
               IF      WRK-SYOSUU          >   ZERO
                   COMPUTE WRK-ST  =   WRK-ST  +   WRK-SYOSUU
                                               +   1
               END-IF                                
      *     
               PERFORM VARYING IDW FROM    WRK-ST    BY  1
                       UNTIL   IDW >       10
                   IF      WRK-NUM (IDW:1)   NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW       
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF     
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ                  
                       MOVE    10                TO  IDW
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
           OPEN    INPUT   TOUKEIERR-FILE
           IF      STS-TOUKEIERR         =   ZERO
               CLOSE   TOUKEIERR-FILE
           ELSE
               OPEN    OUTPUT              TOUKEIERR-FILE
      *
               MOVE    WRK-TOUKEIERR         TO  TOUKEIERR-REC
               WRITE   TOUKEIERR-REC
               CLOSE   TOUKEIERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-TOUKEIERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
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
           CLOSE   TOUKEI04-FILE
                   TOUKEI05-FILE
      *
           IF      CNT-TOUKEI04           =   ZERO  
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ファイル保存情報更新
               PERFORM 3001-FILE-INFO-INSERT-SEC
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           COMPUTE JOB-UPDCNT  =   JOB-UPDCNT      +
                                   CNT-TOUKEI05
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "TOUKEI04  CNT-IN :" CNT-TOUKEI04
           DISPLAY "TOUKEI05  CNT-OUT:" CNT-TOUKEI05
           DISPLAY "*** ORCBT130 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル保存情報更新処理
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           MOVE    ZERO            TO  WRK-RENNUM
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
      *
           IF    ( WRK-PARA-FILE1      =   "6"             )
      *****AND   ( CNT-TOUKEI04        >   ZERO            )
               ADD     1               TO  WRK-RENNUM
      *
               MOVE    "toukei2.sh"    TO  SFILESV-SHELLID  (WRK-RENNUM)
               MOVE    1               TO  SFILESV-SHORI-RENNUM
                                                            (WRK-RENNUM)
               MOVE    WRK-RENNUM      TO  SFILESV-RENNUM   (WRK-RENNUM)
      *********MOVE    WRK-PARA-SRYYM  TO  SFILESV-SRYYM    (WRK-RENNUM)
      *********MOVE    "/tmp/"         TO  SFILESV-FOR-FOLDER
               MOVE    WRK-SGETTEMP-TEMPDIR
                                       TO  SFILESV-FOR-FOLDER
                                                            (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM1
                                       TO  SFILESV-FOR-DATA (WRK-RENNUM)
      *********MOVE    WRK-PARA-FILENM1-FOLDER
      *********                        TO  SFILESV-TO-FOLDER
      *********                                             (WRK-RENNUM)
               MOVE    WRK-PARA-FILENM1
                                       TO  SFILESV-TO-DATA  (WRK-RENNUM)
               MOVE    CNT-TOUKEI05    TO  SFILESV-CNT-ALL  (WRK-RENNUM)
               MOVE    "日次統計データ"
                                       TO  SFILESV-TITLE    (WRK-RENNUM)
               MOVE    "1"             TO  SFILESV-DATA-TYPE(WRK-RENNUM)
           END-IF
      *
           IF      WRK-RENNUM          >   ZERO   
               CALL    "ORCSFILESV"    USING
                                           ORCSFILESVAREA
                                           SPA-AREA
           END-IF
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日次統計データ（基本情報）読込
      *****************************************************************
       900-TOUKEI04-READ-SEC             SECTION.
      *
           READ    TOUKEI04-FILE
               AT  END
                   MOVE    1         TO  FLG-END
               NOT AT  END
                   ADD     1         TO  CNT-TOUKEI04
           END-READ
           .
       900-TOUKEI04-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
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
      *****************************************************************
      *    ＤＢオープン処理
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
      *    ＤＢクローズ処理
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
       900-DBDISCONNECT-EXT.
           EXIT.
