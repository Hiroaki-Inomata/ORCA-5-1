      *******************************************************************
      * ★WARNING 仮の元号対応あり
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
       PROGRAM-ID.             ORCBM550.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : オンライン請求(受付、請求確定データ取り込み)
      *  管理者            :
      *  作成日付    作業者        記述
      *  08/09/01    NACL-竹田     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/12/10  返戻対応
      *  04.04.02    NACL-藤原    10/12/10  返戻ファイルの保存用ファイル名
      *                                     の変更
      *
      *  04.06.01    NACL-藤原    10/12/20  保険者からの返戻対応
      *  04.06.02    NACL-藤原    11/02/14  OpenCOBOL 1.0.0 対応
      *  04.06.03    NACL-藤原    12/06/19  取り込み時のレコード長変更
      *  04.06.03    NACL-藤原    12/08/10  取り込み時の条件変更
      *  04.06.04    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *                                     receipt_kbn 追加 
      *  04.07.02    NACL-藤原    13/10/07  労災レセ電対応
      *  04.07.03    NACL-藤原    14/02/12  データ識別が数字であれば全て
      *                                     取り込むようにする
      *  04.07.04    NACL-藤原    15/05/12  患者番号からＩＤが取得できない
      *                                     ときのエラー処理
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-藤原    16/01/15  労災レセ電の請求情報への
      *                                     更新日設定
      *  04.08.03    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.04    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *  04.08.05    NACL-藤原    18/03/28  平成３０年４月改正対応
      *
      *  05.00.01    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    取り込みデータファイル
           SELECT  PARA-FILE       ASSIGN  ASS-PARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-PARA.
      *    エラーファイル
           SELECT  CSVERR-FILE     ASSIGN  CSVERR
                                   FILE    STATUS  IS  STS-CSVERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    取り込みデータファイル
       FD  PARA-FILE.
       01  PARA-REC.
           03  PARA-DATA           PIC X(3000).
      *
      *    エラーファイル
       FD  CSVERR-FILE.
       01  CSVERR-REC              PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //CSVERR//.
      *
       01  LNK-TIMEOFDAY       PIC X(100).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA                PIC X(02).
           03  STS-CSV                 PIC X(02).
           03  STS-LST                 PIC X(02).
           03  STS-CSVERR              PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ONRECE-K            PIC 9(01).
           03  FLG-ONRECE-B            PIC 9(01).
           03  FLG-HENREI-K            PIC 9(01).
           03  FLG-HENREI-B            PIC 9(01).
      *
           03  FLG-SIKIBETU-HR         PIC 9(01).
           03  FLG-SIKIBETU-RC         PIC 9(01).
      *
           03  FLG-DATAEND             PIC 9(01).
           03  FLG-MATCH               PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-AREA1.
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DELI1           PIC 9(01).
               05  FLG-DELI2           PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
           03  FLG-PTID-ERR            PIC 9(01).
      *
      *    カウンタ
       01  CNT-AREA.
           03  READ-CNT                PIC 9(05).
           03  INS-K-CNT               PIC 9(05).
           03  INS-B-CNT               PIC 9(05).
      *    ＣＳＶ項目数
           03  SPLIT-CNT               PIC 9(03).
           03  SPLIT-CNT1              PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(05).
           03  IDY                     PIC 9(05).
           03  IDZ                     PIC 9(03).
           03  IDW                     PIC 9(03).
           03  IDE                     PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDYY                    PIC 9(05).
           03  IDZZ                    PIC 9(05).
      *
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX3                    PIC 9(05).
      *
      *    取り込みファイル名
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //ASS-PARA//.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM    PIC 9(02).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-FILE-OCC   OCCURS  15.
                   07  WRK-PARA-FILE   PIC X(1024).
                   07  WRK-PARA-BASEFILE
                                       PIC X(1024).
               05  WRK-PARA-HOSPCD-OCC OCCURS  15.
                   07  WRK-PARA-HOSPCD PIC X(07).
      *
           03  WRK-CSVERR              PIC X(200). 
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
      * 
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTID-SRYYM          PIC 9(06).
      *     
           03  WRK-ERRCD               PIC X(03).
           03  WRK-HKNJANUM            PIC X(08).
           03  WRK-RECORD-SIKIBETU     PIC X(02).
      *
           03  WRK-ERR-RECEIPT-NO-X.
               05  WRK-ERR-RECEIPT-NO  PIC 9(05).
      *
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-KOUMOKU-NO          PIC 9(04).
      *
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-CNT          PIC 9(04).
               05  WRK-WK              PIC 9(04).
      *
           03  WRK-FILE-TMP            PIC X(1024).
      *
           03  WRK-PARA-REC.
               05  WRK-PARA-X          PIC X(01)  OCCURS   3000.
           03  WRK-PARA-REC1           PIC X(3000).
           03  WRK-PARA-REC2           PIC X(3000).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
           03  WRK-RIREKIKANRI-CHK.
               05  WRK-SIKIBETU        PIC X(03).
               05  WRK-DATA            PIC X(500).
               05  WRK-RIREKIKANRI-FLG PIC 9(01).
      *
           03  WRK-SAIGAIKBN           PIC X(01).
           03  WRK-CHOHYOKBN           PIC X(01).
      *
           03  WRK-SEIKYU-DATA.
               05  WRK-SEIKYU-SKYYM    PIC X(05).
               05  WRK-SEIKYU-SRYYM    PIC X(05).
               05  FILLER              PIC X(14).
      *
           03  WRK-SEIKYU-DATA-R.
               05  WRK-SEIKYU-CREYMD   PIC X(08).
               05  WRK-SEIKYU-CREHMS   PIC X(06).
               05  WRK-SEIKYU-DISASTER PIC X(02).
      *
       01  CNT-RESEDEN.
           03  PATIENT-CNT             PIC 9(05).
           03  NOPATIENT-CNT           PIC 9(05).
           03  KO-B-CNT                PIC 9(05).
           03  UKE-CNT                 PIC 9(03).
           03  SEI-CNT                 PIC 9(03).
      *
       01  WRK-RESEDEN.
           03  WRK-TEISYUTUSAKI        PIC X(1).
           03  WRK-SRYYM               PIC X(6).
           03  WRK-SKYYMD              PIC X(8).
           03  WRK-UKEYMD              PIC X(8).
           03  WRK-RECV-FLG            PIC 9(1).
           03  WRK-AMOUNT-FLG          PIC 9(1).
           03  WRK-CHG-FLG             PIC 9(1).
           03  WRK-MOJI                PIC X(500).
           03  WRK-MOJI-IN             PIC X(500).
           03  CK-RECV-FLG             PIC 9(1).
           03  CK-AMOUNT-FLG           PIC 9(1).
           03  HIT-K                   PIC 9(3).
      *
      *    患者番号構成管理情報退避用領域
       01  TBL-SYS1009-T.
           03  TBL-SYS1009-OCC         OCCURS  10.
               05  TBL-SYS1009         PIC X(610).
           03  TBL-SYS1009-MAX         PIC 9(02).
      *
       01  WRK-ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC" REPLACING   //ONRECE-B-//
                                       BY          //WRK-ONRECE-B-//.
      *
       01  WRK-HENREI-K-REC.
           COPY    "CPHENREI-KANRI.INC"
                                       REPLACING   //HENREI-K-// 
                                       BY          //WRK-HENREI-K-//.
      *
       01  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC" REPLACING   //HENREI-B-//
                                       BY          //WRK-HENREI-B-//.
      *
       01  TABLE-AREA.
         02  TBL-ONRECE-K-REC          OCCURS  1000.
           COPY    "CPONRECE-KANRI.INC"
                                       REPLACING   //ONRECE-K-// 
                                       BY          //TBL-K-//.
           02  TBL-ONRECE-K-MAX        PIC 9(05).
      *
         02  TBL-ONRECE-B-REC          OCCURS  50000.
           COPY    "CPONRECE-BODY.INC" REPLACING   //ONRECE-B-//
                                       BY          //TBL-B-//.
           03  TBL-ACCTP-HKNCOMBI      PIC X(04).
         02  TBL-ONRECE-B-MAX          PIC 9(05).
      *
         02  TBL-HENREI-B-REC          OCCURS  50000.
           COPY    "CPHENREI-BODY.INC" REPLACING   //HENREI-B-//
                                       BY          //TBL-HENREI-B-//.
         02  TBL-HENREI-B-MAX          PIC 9(05).
     *
       01  WRK-CONST-AREA.
           03  WRK-CONS-ONRECE-K-MAX   PIC 9(05)   VALUE   1000.
           03  WRK-CONS-ONRECE-B-MAX   PIC 9(05)   VALUE   50000.
           03  WRK-CONS-HENREI-B-MAX   PIC 9(05)   VALUE   50000.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  ONRECE-K-REC.
           COPY    "CPONRECE-KANRI.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
       01  HENREI-K-REC.
           COPY    "CPHENREI-KANRI.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    コード変換サブ
           COPY    "CPORCSJISTOEUC.INC".
      *
           COPY    "CPORCSZENHANCNV.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           03  FILLER              PIC X(400).
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
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               CSVERR
                                               WRK-PARA-BASEFILE (1)
                                               WRK-PARA-BASEFILE (2)
                                               WRK-PARA-BASEFILE (3)
                                               WRK-PARA-BASEFILE (4)
                                               WRK-PARA-BASEFILE (5)
                                               WRK-PARA-BASEFILE (6)
                                               WRK-PARA-BASEFILE (7)
                                               WRK-PARA-BASEFILE (8)
                                               WRK-PARA-BASEFILE (9)
                                               WRK-PARA-BASEFILE (10)
                                               WRK-PARA-BASEFILE (11)
                                               WRK-PARA-BASEFILE (12)
                                               WRK-PARA-BASEFILE (13)
                                               WRK-PARA-BASEFILE (14)
                                               WRK-PARA-BASEFILE (15)
                                               WRK-PARA-HOSPCD (1)
                                               WRK-PARA-HOSPCD (2)
                                               WRK-PARA-HOSPCD (3)
                                               WRK-PARA-HOSPCD (4)
                                               WRK-PARA-HOSPCD (5)
                                               WRK-PARA-HOSPCD (6)
                                               WRK-PARA-HOSPCD (7)
                                               WRK-PARA-HOSPCD (8)
                                               WRK-PARA-HOSPCD (9)
                                               WRK-PARA-HOSPCD (10)
                                               WRK-PARA-HOSPCD (11)
                                               WRK-PARA-HOSPCD (12)
                                               WRK-PARA-HOSPCD (13)
                                               WRK-PARA-HOSPCD (14)
                                               WRK-PARA-HOSPCD (15)
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    CSVERR          TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  CSVERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  CSVERR
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       15
                   OR      WRK-PARA-BASEFILE(IDXX) =   SPACE
               INITIALIZE                  SGETTEMP-AREA
               MOVE    WRK-PARA-BASEFILE(IDXX) 
                                       TO  SGETTEMP-BASENAME
               CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
               MOVE    SPACE           TO  WRK-PARA-FILE(IDXX)
               MOVE    SGETTEMP-FULLNAME
                                       TO  WRK-PARA-FILE(IDXX)
      *
               DISPLAY "file=(" IDXX ") " WRK-PARA-FILE (IDXX)
           END-PERFORM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM550"      TO  JOB-PGID
           MOVE    "オンライン請求（データ取り込み）"
                                   TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    患者番号構成管理情報編集
           INITIALIZE                      TBL-SYS1009-T
      *
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM UNTIL ( FLG-SYSKANRI    =    1  )
                       OR    ( TBL-SYS1009-MAX >=  100 )
                   ADD     1                 TO  TBL-SYS1009-MAX
                   MOVE    MCPDATA-REC       TO  TBL-SYS1009
                                                     (TBL-SYS1009-MAX)
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
      *
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      TBL-SYS1009-MAX     =   ZERO
               MOVE    "患者番号構成管理情報が取得できませんでした。"
                                         TO  WRK-CSVERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       15
                   OR      WRK-PARA-FILE(IDXX) =   SPACE
                   OR      FLG-END             =   1
               MOVE  WRK-PARA-FILE(IDXX)   TO  ASS-PARA
               MOVE  WRK-PARA-BASEFILE(IDXX)
                                           TO  WRK-FILE-TMP
      *
               DISPLAY "file shori st = " IDXX " " ASS-PARA
               DISPLAY "     basefile ="  WRK-FILE-TMP
               DISPLAY "     hospcd   ="  WRK-PARA-HOSPCD (IDXX) 
      *
               MOVE    ZERO                TO  FLG-PARA
                                               FLG-PTID-ERR
               MOVE    ZERO                TO  READ-CNT
                                               INS-K-CNT
                                               INS-B-CNT
               MOVE    ZERO                TO  WRK-KOUMOKU-NO
               MOVE    ZERO                TO  WRK-MOVE-AREA
               MOVE    ZERO                TO  IDZZ
               INITIALIZE                      WRK-POINT-AREA
               INITIALIZE                      CNT-RESEDEN
               INITIALIZE                      WRK-RESEDEN
               INITIALIZE                      TABLE-AREA
                                               WRK-HENREI-K-REC  
                                               WRK-HENREI-B-REC
                                               FLG-SIKIBETU-RC
                                               FLG-SIKIBETU-HR
               MOVE    ZERO                TO  WRK-ERR-RECEIPT-NO
      *
               OPEN    INPUT               PARA-FILE
               IF      STS-PARA        NOT =   ZERO
                   CALL "coblog" USING   "file open err " WRK-FILE-TMP
                   STRING  "ファイルオープンエラー FILE="
                                                   DELIMITED  BY  SIZE
                       WRK-FILE-TMP                DELIMITED  BY  SPACE
                       " STS="                     DELIMITED  BY  SIZE
                       STS-PARA                    DELIMITED  BY  SIZE
                                           INTO    WRK-CSVERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               EVALUATE    WRK-FILE-TMP(1:6)
                   WHEN    "RIhasp"
                   WHEN    "RIuasp"
                   WHEN    "RIfixf"
      *                データ取り込み処理
                       PERFORM 2001-PARA-HENSYU-SEC
               END-EVALUATE
               EVALUATE    WRK-FILE-TMP(5:8)
                   WHEN    "RECEIPTC"
      *                データ取り込み処理（返戻ファイル）
                       PERFORM 2002-PARA-HENSYU-SEC
               END-EVALUATE
               EVALUATE    WRK-FILE-TMP(5:6)
                   WHEN    "RRECEC"
      *                データ取り込み処理（労災返戻ファイル）
                       PERFORM 2003-PARA-HENSYU-SEC
               END-EVALUATE
           END-PERFORM
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ取り込み処理
      *****************************************************************
       2001-PARA-HENSYU-SEC             SECTION.
      *
      *    取り込みデータファイル読み込み処理
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM             UNTIL   FLG-PARA    =   1
                               OR      FLG-END     =   1
      *
      *        文字コード変換
               MOVE    PARA-DATA           TO  SJISTOEUC-INDATA
               INITIALIZE                      SJISTOEUC-OUTDATA
               MOVE    10000               TO  SJISTOEUC-INLEN
               MOVE    10000               TO  SJISTOEUC-OUTLEN
               CALL    "orcsjistoeuc"      USING
                                           ORCSJISTOEUC
               MOVE    SJISTOEUC-OUTDATA   TO  WRK-PARA-REC
      *
               EVALUATE    WRK-FILE-TMP (1:6)
                   WHEN    "RIhasp"
                   WHEN    "RIuasp"
      *                受付
                       EVALUATE    WRK-PARA-REC (1:2)
                           WHEN    "受" 
                           WHEN    "項" 
      *                        帳票名、項目名
                               CONTINUE
                           WHEN    "平" 
                           WHEN    "令" 
      *                        ヘッダ
                               ADD     1               TO  IDZZ
                               MOVE    9               TO  SPLIT-CNT
                               MOVE    10              TO  SPLIT-CNT1
                               PERFORM 20011-PARA-SPLIT-SEC
                               MOVE  ZERO              TO  IDZZ
                           WHEN  OTHER
      *                        明細
                               ADD     1               TO  IDZZ
                               MOVE    22              TO  SPLIT-CNT
                               MOVE    23              TO  SPLIT-CNT1
                               PERFORM 20011-PARA-SPLIT-SEC
                       END-EVALUATE
                   WHEN    "RIfixf"
      *                請求確認
                       EVALUATE    WRK-PARA-REC (1:1)
                           WHEN    "1" 
      *                        ヘッダ
                               ADD     1               TO  IDZZ
                               MOVE    10              TO  SPLIT-CNT
                               MOVE    11              TO  SPLIT-CNT1
                               PERFORM 20011-PARA-SPLIT-SEC
                               MOVE  ZERO              TO  IDZZ
                           WHEN    "2" 
      *                        明細
                               ADD     1               TO  IDZZ
                               MOVE    31              TO  SPLIT-CNT
                               MOVE    32              TO  SPLIT-CNT1
                               PERFORM 20011-PARA-SPLIT-SEC
                       END-EVALUATE
               END-EVALUATE
      *
      *        取り込みデータファイル読み込み処理
               PERFORM 900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE   PARA-FILE
      *
           IF      FLG-END             =   ZERO
      *        ＤＢ更新処理
               PERFORM 20011-DB-UPDATE-SEC
           END-IF
      *
           DISPLAY "IN   CNT = " READ-CNT
      *
           .
       2001-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ取り込み処理（返戻ファイル）
      *****************************************************************
       2002-PARA-HENSYU-SEC             SECTION.
      *
           EVALUATE    WRK-FILE-TMP (5:)
               WHEN    "RECEIPTC.HEN"
                   MOVE    "0"         TO  WRK-HENREI-K-TEISYUTUSAKI2
               WHEN    "RECEIPTC.SAH"
                   MOVE    "1"         TO  WRK-HENREI-K-TEISYUTUSAKI2
           END-EVALUATE
      *
           MOVE    WRK-PARA-HOSPCD (IDXX)  TO  WRK-HENREI-K-HOSPCD
      *
      *    取り込みデータファイル読み込み処理
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM             UNTIL   FLG-PARA    =   1
                               OR      FLG-END     =   1
      *
      *        文字コード変換
               MOVE  PARA-DATA             TO  SJISTOEUC-INDATA
               INITIALIZE                      SJISTOEUC-OUTDATA
               MOVE    10000               TO  SJISTOEUC-INLEN
               MOVE    10000               TO  SJISTOEUC-OUTLEN
               CALL    "orcsjistoeuc"      USING
                                           ORCSJISTOEUC
               IF      SJISTOEUC-RETURN    =   0
                   CONTINUE
               ELSE
                   DISPLAY "SJISTOEUC-RETURN =" SJISTOEUC-RETURN
                   DISPLAY "SJISTOEUC-INDATA=" SJISTOEUC-INDATA (1:5)
               END-IF
               MOVE    SJISTOEUC-OUTDATA   TO  WRK-PARA-REC
      *
               UNSTRING  WRK-PARA-REC      DELIMITED  BY  X"0D"  OR
                                                          X"0A"
                                           INTO    WRK-PARA-REC1
                                                   WRK-PARA-REC2
               END-UNSTRING
               MOVE    WRK-PARA-REC1       TO  WRK-PARA-REC
      *
               INITIALIZE                      WRK-RIREKIKANRI-CHK
      * 
               EVALUATE    WRK-PARA-REC (1:2)
                                           ALSO    WRK-FILE-TMP(5:12) 
                   WHEN    "HI"            ALSO    ANY
                       MOVE    ZERO            TO  IDZZ
                       MOVE    8               TO  SPLIT-CNT
                       MOVE    9               TO  SPLIT-CNT1
                       PERFORM 20011-PARA-SPLIT-SEC
                   WHEN    "RE"            ALSO     ANY
                       IF      IDZZ            >   ZERO
      *                    ＤＢ更新処理（返戻ファイル）
                           PERFORM 20021-DB-UPDATE-SEC
      *                    MOVE    ZERO        TO  IDZZ
                           INITIALIZE              TABLE-AREA
                                                   WRK-HENREI-B-REC
                                                   FLG-SIKIBETU-HR
                                                   FLG-SIKIBETU-RC
                       END-IF
                       MOVE    1               TO  IDZZ
                       MOVE    21              TO  SPLIT-CNT
                       MOVE    22              TO  SPLIT-CNT1
                       PERFORM 20011-PARA-SPLIT-SEC
                   WHEN    "HO"            ALSO     ANY
                       ADD     1               TO  IDZZ
                       MOVE    4               TO  SPLIT-CNT
                       MOVE    5               TO  SPLIT-CNT1
                       PERFORM 20011-PARA-SPLIT-SEC
                   WHEN    "KO"            ALSO     ANY
                       ADD     1               TO  IDZZ
                       MOVE    4               TO  SPLIT-CNT
                       MOVE    5               TO  SPLIT-CNT1
                       PERFORM 20011-PARA-SPLIT-SEC
                   WHEN    "HR"            ALSO     ANY
                       ADD     1               TO  IDZZ
                       MOVE    14              TO  SPLIT-CNT
                       MOVE    15              TO  SPLIT-CNT1
                       PERFORM 20011-PARA-SPLIT-SEC
                   WHEN    "HG"            ALSO     ANY
                       ADD     1               TO  IDZZ
                       MOVE    4               TO  SPLIT-CNT
                       MOVE    5               TO  SPLIT-CNT1
                       PERFORM 20011-PARA-SPLIT-SEC
                   WHEN    OTHER
      *??              IF      WRK-FILE-TMP(5:12)  =   "RECEIPTC.SAH"
      *                    データ識別が数字のとき履歴管理ブロックとして取り込み
                           UNSTRING    WRK-PARA-REC (1:10)
                                                   DELIMITED  BY  ","
                                                   INTO    WRK-SIKIBETU
                                                           WRK-DATA
                           END-UNSTRING
                           INITIALIZE              ORCSNUMAREA
                           MOVE    WRK-SIKIBETU    TO  SNUM-INX
                           CALL    "ORCSNUM"       USING
                                                   ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         <=  2     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    1               TO
                                                   WRK-RIREKIKANRI-FLG
      *
                               ADD     1               TO  IDZZ
                               MOVE    5               TO  SPLIT-CNT
                               MOVE    6               TO  SPLIT-CNT1
                               PERFORM 20011-PARA-SPLIT-SEC
                           ELSE
                               ADD     1               TO  IDZZ
                               MOVE    1               TO  SPLIT-CNT
                               MOVE    2               TO  SPLIT-CNT1
                               PERFORM 20011-PARA-SPLIT-SEC
                           END-IF
      *??              ELSE
      *??                  ADD     1               TO  IDZZ
      *??                  MOVE    1               TO  SPLIT-CNT
      *??                  MOVE    2               TO  SPLIT-CNT1
      *??                  PERFORM 20011-PARA-SPLIT-SEC
      *??              END-IF
               END-EVALUATE
      *
      *        取り込みデータファイル読み込み処理
               PERFORM 900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE   PARA-FILE
      *
           IF      FLG-PTID-ERR        =   9
               PERFORM 2009-DB-DELETE-SEC
           END-IF
      *
           IF      IDZZ            >   ZERO
      *        ＤＢ更新処理（返戻ファイル）
               PERFORM 20021-DB-UPDATE-SEC
           END-IF
      *
           DISPLAY "IN   CNT = " READ-CNT
           DISPLAY "OUT  CNT = " INS-K-CNT
           DISPLAY "     CNT = " INS-B-CNT
      *
           .
       2002-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ取り込み処理（労災・返戻ファイル）
      *****************************************************************
       2003-PARA-HENSYU-SEC             SECTION.
      *??
           DISPLAY "2003=" WRK-FILE-TMP (5:)
      *??
      *
           EVALUATE    WRK-FILE-TMP (5:)
               WHEN    "RRECEC.HEN"
                   MOVE    "0"         TO  WRK-HENREI-K-TEISYUTUSAKI2
           END-EVALUATE
      *
           MOVE    WRK-PARA-HOSPCD (IDXX)  TO  WRK-HENREI-K-HOSPCD
      *
           MOVE    ZERO            TO  IDZZ
      *
      *    取り込みデータファイル読み込み処理
           PERFORM 900-PARA-READ-SEC
      *
           PERFORM             UNTIL   FLG-PARA    =   1
                               OR      FLG-END     =   1
      *
      *        文字コード変換
               MOVE  PARA-DATA             TO  SJISTOEUC-INDATA
               INITIALIZE                      SJISTOEUC-OUTDATA
               MOVE    10000               TO  SJISTOEUC-INLEN
               MOVE    10000               TO  SJISTOEUC-OUTLEN
               CALL    "orcsjistoeuc"      USING
                                           ORCSJISTOEUC
               IF      SJISTOEUC-RETURN    =   0
                   CONTINUE
               ELSE
                   DISPLAY "SJISTOEUC-RETURN =" SJISTOEUC-RETURN
                   DISPLAY "SJISTOEUC-INDATA=" SJISTOEUC-INDATA (1:5)
               END-IF
               MOVE    SJISTOEUC-OUTDATA   TO  WRK-PARA-REC
      *
               UNSTRING  WRK-PARA-REC      DELIMITED  BY  X"0D"  OR
                                                          X"0A"
                                           INTO    WRK-PARA-REC1
                                                   WRK-PARA-REC2
               END-UNSTRING
               MOVE    WRK-PARA-REC1       TO  WRK-PARA-REC
      *
               INITIALIZE                      WRK-RIREKIKANRI-CHK
      * 
               IF      WRK-PARA-REC (1:1)  =   X"1A"
                   CONTINUE
               ELSE
                   EVALUATE    WRK-PARA-REC (1:2)
                                           ALSO    WRK-FILE-TMP(5:12) 
                       WHEN    "RE"            ALSO     ANY
                           IF      IDZZ            >   ZERO
      *                        ＤＢ更新処理（労災・返戻ファイル）
                               PERFORM 20031-DB-UPDATE-SEC
      *                        MOVE    ZERO        TO  IDZZ
                               INITIALIZE              TABLE-AREA
                                                       WRK-HENREI-B-REC
                           END-IF
                           MOVE    ZERO            TO  IDZZ
                           MOVE    1               TO  IDZZ
                           MOVE    21              TO  SPLIT-CNT
                           MOVE    22              TO  SPLIT-CNT1
                           PERFORM 20011-PARA-SPLIT-SEC
                       WHEN    "RR"            ALSO     ANY
                           ADD     1               TO  IDZZ
                           MOVE    6               TO  SPLIT-CNT
                           MOVE    7               TO  SPLIT-CNT1
                           PERFORM 20011-PARA-SPLIT-SEC
                       WHEN    OTHER
                           ADD     1               TO  IDZZ
                           MOVE    1               TO  SPLIT-CNT
                           MOVE    2               TO  SPLIT-CNT1
                           PERFORM 20011-PARA-SPLIT-SEC
                   END-EVALUATE
               END-IF
      *
      *        取り込みデータファイル読み込み処理
               PERFORM 900-PARA-READ-SEC
           END-PERFORM
      *
           CLOSE   PARA-FILE
      *
           IF      FLG-PTID-ERR        =   9
               PERFORM 2009-DB-DELETE-SEC
           END-IF
      *
           IF      IDZZ            >   ZERO
      *        ＤＢ更新処理（労災・返戻ファイル）
               PERFORM 20031-DB-UPDATE-SEC
           END-IF
      *
           DISPLAY "IN   CNT = " READ-CNT
           DISPLAY "OUT  CNT = " INS-K-CNT
           DISPLAY "     CNT = " INS-B-CNT
      *
           .
       2003-PARA-HENSYU-EXT.
           EXIT.
      *
     *****************************************************************
      *    データカンマ分離処理
      *****************************************************************
       20011-PARA-SPLIT-SEC          SECTION.
      *
           MOVE    ZERO            TO  WRK-POINT-AREA
           MOVE    ZERO            TO  IDE
           MOVE    ZERO            TO  FLG-DATAEND
           MOVE    ZERO            TO  WRK-KOUMOKU-NO
      *
      *    患者ＣＳＶファイルに存在する項目数分、繰り返す
      *    またはレコード終了まで
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX           >   SPLIT-CNT)
                   OR      ( WRK-PO-ED     >=  3000 )
                   OR      ( FLG-END       =   1    )
      *
               MOVE    ZERO            TO  FLG-AREA1
               COMPUTE WRK-PO-ST   =   WRK-PO-ED   +   1
      *
               IF    ( WRK-PARA-REC (WRK-PO-ST:)
                                       =   SPACE )
               OR    ( FLG-DATAEND     =   1     )
                   MOVE    1               TO  FLG-DATAEND
                   MOVE    WRK-PO-ST       TO  WRK-PO-ED
               ELSE
                   ADD     1               TO  WRK-KOUMOKU-NO
      *            項目終了文字後のカンマ位置を取得(WRK-PO-ED)
                   PERFORM 200111-CHAR-END-GET-SEC
               END-IF
      *        項目共通処理
               PERFORM 200112-KOUMOKU-KYOTU-SEC
      *        項目別処理
               IF      FLG-DATAEND     =   ZERO
                   PERFORM 200113-KOUMOKU-BETU-SEC
               END-IF
           END-PERFORM
      *
           .
       20011-PARA-SPLIT-EXT.
           EXIT.
      *****************************************************************
      *    対象項目の文字終了位置を取得
      *****************************************************************
       200111-CHAR-END-GET-SEC        SECTION.
      *
           MOVE    WRK-PO-ST           TO  WRK-PO-ED
      *
           PERFORM VARYING   IDYY   FROM    WRK-PO-ST   BY  1
                   UNTIL   ( IDYY           >=  3000 )
                   OR      ( FLG-COMMA     =   1    )
      *        カンマの位置
               IF    ( WRK-PARA-X (IDYY)     =   "," )  OR
                     ((IDX             =   SPLIT-CNT  OR  
                                           SPLIT-CNT1) AND
                     ( WRK-PARA-X (IDYY)     =   SPACE))
                   MOVE    IDYY         TO  WRK-PO-ED
                   MOVE    1           TO  FLG-COMMA
               ELSE
      *            カンマが足りない場合の終了位置決定
                   IF      WRK-PARA-X (IDYY) NOT =   SPACE
                       MOVE    IDYY         TO  WRK-PO-ED
                   END-IF
               END-IF
      *        デリミタ(")存在するかどうか
               IF      WRK-PARA-X (IDYY)         =   """"
                   IF      FLG-DELI1       =   ZERO
                       MOVE    1       TO  FLG-DELI1
                   ELSE
                       MOVE    1       TO  FLG-DELI2
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200111-CHAR-END-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目共通処理
      *****************************************************************
       200112-KOUMOKU-KYOTU-SEC       SECTION.
      *
           MOVE    ZERO            TO  WRK-MOVE-AREA
      *
      *    データが省略されている場合、FLG-NODATAに1をセット
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK      =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK          =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  200112-KOUMOKU-KYOTU-EXT
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               IF      WRK-PO-ST       =   WRK-PO-ED
                   MOVE    1           TO  FLG-NODATA
                   GO  TO  200112-KOUMOKU-KYOTU-EXT
               END-IF
           END-IF
      *
      *    転送開始位置・転送桁数を算出
           IF      FLG-DELI1           =   1
      *        デリミタ(")を含んでいる場合
               COMPUTE WRK-MO-ST       =   WRK-PO-ST   +   1
               IF      FLG-DELI2       =   1
                   COMPUTE WRK-MO-CNT  =   WRK-PO-ED
                                       -  (WRK-PO-ST   +   2)
               ELSE
                   COMPUTE WRK-MO-CNT  =  (WRK-PO-ED   -   1)
                                       -   WRK-PO-ST
               END-IF
           ELSE
      *        デリミタ(")を含んでいない場合
               MOVE    WRK-PO-ST       TO  WRK-MO-ST
               COMPUTE WRK-MO-CNT      =   WRK-PO-ED  -  WRK-PO-ST
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0D" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           IF    ( WRK-MO-CNT          =   1 ) AND
                 ( WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT) =   X"0A" )
               MOVE  1                 TO  FLG-NODATA
           END-IF
      *
           .
       200112-KOUMOKU-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目別処理
      *****************************************************************
       200113-KOUMOKU-BETU-SEC        SECTION.
      *
           EVALUATE    WRK-FILE-TMP(1:6)
               WHEN    "RIhasp"
               WHEN    "RIuasp"
                   PERFORM 2001131-HASP-KOUMOKU-BETU-SEC
               WHEN    "RIfixf"
                   PERFORM 2001132-FIXF-KOUMOKU-BETU-SEC
           END-EVALUATE
           EVALUATE    WRK-FILE-TMP(5:8)
               WHEN    "RECEIPTC"
                   PERFORM 2001133-RECEIP-KOUMOKU-BETU-SEC
           END-EVALUATE
           EVALUATE    WRK-FILE-TMP(5:6)
               WHEN    "RRECEC"
                   PERFORM 2001134-RRECEC-KOUMOKU-BETU-SEC
           END-EVALUATE
      *
           .
       200113-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付項目別処理
      *****************************************************************
       2001131-HASP-KOUMOKU-BETU-SEC     SECTION.
      *
           EVALUATE    WRK-PARA-REC(1:2)
      *        帳票名、項目名
               WHEN    "受" 
               WHEN    "項" 
                   CONTINUE
      *        ヘッダ
               WHEN    "平"
               WHEN    "令"
                   EVALUATE    WRK-KOUMOKU-NO
      *                １：診療年月
                       WHEN    1
                           MOVE     SPA-HOSPNUM
                                               TO  TBL-B-HOSPNUM (IDZZ)
                           MOVE    WRK-PARA-HOSPCD (IDXX)
                                               TO  TBL-B-HOSPCD  (IDZZ)
      *
                           MOVE     WRK-FILE-TMP(7:1)
                                               TO  WRK-TEISYUTUSAKI
                                                   TBL-B-TEISYUTUSAKI
                                                                 (IDZZ)
      *
                           MOVE    1           TO  TBL-B-RECV-FLG(IDZZ)
                                                   WRK-RECV-FLG
      *
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    16          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "1"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-SYMD (1:6)
                                                   TO  WRK-SRYYM
                                                       TBL-B-SRYYM(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "診療年月エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                ２：送信年月日
                       WHEN    2
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    22          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "1"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-SYMD
                                                   TO  TBL-B-UKEYMD
                                                               (IDZZ)
                                                       WRK-UKEYMD
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "送信年月日エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                ９： 送信回数
                       WHEN    9
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    6           TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "2"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF    ( LNK-ZENHANCNV-RC        =   ZERO )
                           AND   ( LNK-ZENHANCNV-OT-NUM    >=  1   AND
                                                           <=  999  )
                               MOVE    LNK-ZENHANCNV-OT-NUM
                                                   TO  UKE-CNT
                                                       TBL-B-UCNT (IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "送信回数エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                   END-EVALUATE        
      *        明細
               WHEN    OTHER
                   EVALUATE    WRK-KOUMOKU-NO
      *                ２： レセプト番号
                       WHEN    2
      *                    エラー分件数
                           ADD  1              TO  NOPATIENT-CNT
      *
                           MOVE    SPA-HOSPNUM TO  TBL-B-HOSPNUM(IDZZ)
                           MOVE    WRK-PARA-HOSPCD (IDXX)
                                               TO  TBL-B-HOSPCD (IDZZ)
      *
                           MOVE    WRK-TEISYUTUSAKI
                                               TO
                                               TBL-B-TEISYUTUSAKI(IDZZ)
                           MOVE    WRK-SRYYM   TO  TBL-B-SRYYM(IDZZ)
      *
                           MOVE  WRK-UKEYMD    TO  TBL-B-UKEYMD(IDZZ)
      *
                           MOVE  UKE-CNT       TO  TBL-B-UCNT(IDZZ)
      *
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    12          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    "2"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF    ( LNK-ZENHANCNV-RC    =   ZERO      )
                           AND   ( LNK-ZENHANCNV-OT-NUM   >=  1   AND
                                                          <=  999999 )
                               MOVE    LNK-ZENHANCNV-OT-NUM 
                                                   TO  TBL-B-RECEIPT-NO
                                                               (IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "レセプト番号エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                ３： 患者番号
                       WHEN    3
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    40          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    "3"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-DATA 
                                               (1:LNK-ZENHANCNV-OT-LEN)
                                                   TO  TBL-B-PTNUM(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "患者番号エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                ６： 診療年月
                       WHEN    6
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    14          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "1"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-SYMD (1:6)
                                               TO  TBL-B-SRYYM-B(IDZZ)
                                                   WRK-PTID-SRYYM
                               MOVE    TBL-B-PTNUM(IDZZ)
                                               TO  WRK-PTNUM 
      *                        診療年月からPTIDを編集 
                               PERFORM 500-PTID-HENSYU-SEC
                               IF      SPTNUM-RC   =   00
                                   MOVE    SPTNUM-PTID TO
                                                       TBL-B-PTID (IDZZ)
                               ELSE
                                   DISPLAY "ORCSPTNUM ERR " SPTNUM-RC
                               END-IF
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "診療年月エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                ７： 入外区分
                       WHEN    7
                           EVALUATE    WRK-PARA-REC
                                           (WRK-MO-ST:WRK-MO-CNT)
                               WHEN    "入院外"
                                   MOVE    "2" TO  TBL-B-NYUGAIKBN(IDZZ)
                               WHEN    "入　院"
                                   MOVE    "1" TO  TBL-B-NYUGAIKBN(IDZZ)
                               WHEN    OTHER
                                   MOVE    SPACE       TO  WRK-CSVERR
                                   STRING  "入外区分エラー DATA="
                                                   DELIMITED  BY  SIZE
                                           WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                                   END-STRING
                                   PERFORM 500-ERR-HENSYU-SEC
                                   PERFORM 500-COBABORT-SEC
                           END-EVALUATE
      *                ８： 患者氏名
                       WHEN    8
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-NAME (IDZZ)
      *                ９：生年月日
                       WHEN    9
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    20          TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "1"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-SYMD
                                               TO  TBL-B-BIRTHDAY(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "生年月日エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                １０： エラーコード
                       WHEN    10
                           INITIALIZE          ORCSZENHANCNVAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  LNK-ZENHANCNV-IN-DATA
                           MOVE    8           TO  LNK-ZENHANCNV-IN-LEN
                           MOVE    WRK-MO-CNT  TO  LNK-ZENHANCNV-IN-LEN1
                           MOVE    "3"         TO  LNK-ZENHANCNV-SYORI
                           CALL    "ORCSZENHANCNV" USING
                                                   ORCSZENHANCNVAREA
                           IF      LNK-ZENHANCNV-RC    =   ZERO
                               MOVE    LNK-ZENHANCNV-OT-DATA 
                                               (1:LNK-ZENHANCNV-OT-LEN)
                                                   TO
                                               TBL-B-U-ERR-CODE(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "エラーコードエラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                １１：エラーメッセージ１
                       WHEN    11
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-U-ERR-MSG-1(IDZZ)
                           END-IF
      *                １２：エラーメッセージ２
                       WHEN    12
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-U-ERR-MSG-2(IDZZ)
                           END-IF
      *                １３： 診療識別
                       WHEN    13
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-U-ERR-SRYKBN(IDZZ)
                           END-IF
      *                １４： エラー事項
                       WHEN    14
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-U-ERR-INFO(IDZZ)
                           END-IF
      *                １７：請求情報
                       WHEN    17
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-SEIKYU-INFO (IDZZ)
                           END-IF
      *                １８：返戻再請求
                       WHEN    18
                           IF      WRK-MO-CNT  >   ZERO
                               IF      WRK-PARA-REC 
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                       =   "１"
                                   MOVE    "1"         TO
                                               TBL-B-U-HENREI-FLG (IDZZ)
                               END-IF
                           END-IF
      *                １９：予備１
                       WHEN    19
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-U-YOBI1     (IDZZ)
                           END-IF
      *                ２０：予備２
                       WHEN    20
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-U-YOBI2     (IDZZ)
                           END-IF
                   END-EVALUATE
           END-EVALUATE
      *
           .
       2001131-HASP-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確定項目別処理
      *****************************************************************
       2001132-FIXF-KOUMOKU-BETU-SEC     SECTION.
      *
           EVALUATE    WRK-PARA-REC(1:1)
      *        ヘッダ
               WHEN    "1" 
                   EVALUATE    WRK-KOUMOKU-NO
      *                ２： 請求年月の前月 -> 診療年月
                       WHEN    2
                           MOVE    SPA-HOSPNUM TO  TBL-B-HOSPNUM(IDZZ)
                           MOVE    WRK-PARA-HOSPCD (IDXX)
                                               TO  TBL-B-HOSPCD (IDZZ)
      *
                           INITIALIZE              ORCSGDAYAREA
                           MOVE    "1"         TO  SGDAY-INTYPE
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"  USING
                                               ORCSGDAYAREA
                           IF      SGDAY-RC    =   ZERO
                               INITIALIZE          STS-AREA-DAY
                               INITIALIZE          LNK-DAY6-AREA
                               MOVE    "61"    TO  LNK-DAY6-IRAI
                               MOVE    SGDAY-SDAY (1:6)
                                               TO  LNK-DAY6-KIJUN (1:6)
                               MOVE    "01"    TO  LNK-DAY6-KIJUN (7:2)
                               MOVE    "2"     TO  LNK-DAY6-ZOGENPTN
                               MOVE    -1      TO  LNK-DAY6-ZOGEN
                               CALL    "ORCSDAY"   USING
                                                   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                               MOVE    LNK-DAY6-KEISAN(1:6)
                                                   TO  WRK-SRYYM
                                                       TBL-B-SRYYM(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "請求年月エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF 
                       WHEN    3
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-SKYYMD
                                                   TBL-B-SKYYMD(IDZZ)
                       WHEN    4
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               =   "1" OR  "2"
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-TEISYUTUSAKI
                                                   TBL-B-TEISYUTUSAKI
                                                           (IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "審査支払機関コードエラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                           MOVE    1           TO  TBL-B-AMOUNT-FLG
                                                           (IDZZ)
                                                   WRK-AMOUNT-FLG
                       WHEN    9
                           INITIALIZE          ORCSNUMAREA
                            MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"    USING
                                               ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         <=  3     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    SNUM-OUTNUM TO  TBL-B-UCNT(IDZZ)
                                                        UKE-CNT
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "送信回数エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                       WHEN   10
                           INITIALIZE          ORCSNUMAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"    USING
                                               ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         >=  1   AND
                                                       <=  3     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    SNUM-OUTNUM TO  TBL-B-SCNT(IDZZ)
                                                       SEI-CNT
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "請求確定回数エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                   END-EVALUATE
      *        個人情報
               WHEN     "2" 
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    2
                           MOVE ZERO       TO  KO-B-CNT
      *
                           MOVE    SPA-HOSPNUM
                                               TO  TBL-B-HOSPNUM (IDZZ)
                           MOVE    WRK-PARA-HOSPCD (IDXX)
                                               TO  TBL-B-HOSPCD (IDZZ)
                           MOVE    WRK-TEISYUTUSAKI
                                               TO  TBL-B-TEISYUTUSAKI
                                                                 (IDZZ)
                           MOVE    WRK-SKYYMD  TO  TBL-B-SKYYMD(IDZZ)
                           MOVE    1           TO  TBL-B-AMOUNT-FLG
                                                                 (IDZZ)
                           MOVE    WRK-SRYYM   TO  TBL-B-SRYYM(IDZZ)
                           MOVE    UKE-CNT     TO  TBL-B-UCNT(IDZZ)
                           MOVE    SEI-CNT     TO  TBL-B-SCNT(IDZZ)
      *
                           INITIALIZE          ORCSNUMAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"    USING
                                               ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         <=  6     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    SNUM-OUTNUM TO  TBL-B-RECEIPT-NO
                                                                 (IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "レセプト番号エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                       WHEN    3 
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-RECESYUBETU
                                                                 (IDZZ)
                           IF      TBL-B-RECESYUBETU(IDZZ)(4:1)  =
                                       "0" OR "2" OR "4" OR "6" OR "8"
                               MOVE    "2"     TO  TBL-B-NYUGAIKBN(IDZZ)
                           ELSE
                               MOVE    "1"     TO  TBL-B-NYUGAIKBN(IDZZ)
                           END-IF
                       WHEN    4 
                           INITIALIZE                      ORCSGDAYAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SGDAY-INDAY
                           MOVE    "1"         TO  SGDAY-INTYPE
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-SDAY (1:6)
                                               TO  TBL-B-SRYYM-B(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "診療年月エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                              (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                       WHEN    5 
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-NAME(IDZZ)
                       WHEN    6 
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-SEX(IDZZ)
                       WHEN    7 
                           INITIALIZE                      ORCSGDAYAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-SDAY
                                               TO  TBL-B-BIRTHDAY(IDZZ)
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "生年月日エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                              (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                       WHEN    8 
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-PTNUM(IDZZ)
                                                   WRK-PTNUM
                           MOVE    TBL-B-SRYYM-B(IDZZ)
                                               TO  WRK-PTID-SRYYM
                           PERFORM 500-PTID-HENSYU-SEC
                           IF      SPTNUM-RC   =   00
                               MOVE    SPTNUM-PTID TO  TBL-B-PTID (IDZZ)
                           ELSE
                               DISPLAY "ORCSPTNUM ERR " SPTNUM-RC
                           END-IF
                       WHEN    9
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HKNJANUM
                               PERFORM VARYING IDW FROM    1   BY  1
                                       UNTIL   IDW >       8
                                   IF      WRK-HKNJANUM (IDW:1)
                                                   NOT =   SPACE
                                       MOVE    WRK-HKNJANUM (IDW:)  
                                               TO  TBL-B-HKNJANUM(IDZZ)
                                       MOVE    8   TO  IDW
                                   END-IF
                               END-PERFORM
                           END-IF
                       WHEN   10
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  TBL-B-KIGO(IDZZ)
                           END-IF
                       WHEN   11
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  TBL-B-NUM(IDZZ)
                           END-IF
                       WHEN   12
                           IF      WRK-MO-CNT  >   ZERO
                               INITIALIZE          ORCSNUMAREA
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  SNUM-INX
                               CALL    "ORCSNUM"    USING
                                                   ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                                   MOVE    SNUM-OUTNUM TO
                                               TBL-B-H-TTL-TEN(IDZZ)
                               END-IF
                           END-IF
                       WHEN   17
                           ADD  1          TO  KO-B-CNT
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-FTNNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   18
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-JKYNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   19
                           IF      WRK-MO-CNT  >   ZERO
                               INITIALIZE          ORCSNUMAREA
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                               CALL    "ORCSNUM"   USING
                                                   ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                                   MOVE    SNUM-OUTNUM TO
                                       TBL-B-K-TTL-TEN(IDZZ KO-B-CNT)
                               END-IF
                           END-IF
                       WHEN   20
                           ADD  1          TO  KO-B-CNT
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-FTNNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   21
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-JKYNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   22
                           IF      WRK-MO-CNT  >   ZERO
                               INITIALIZE          ORCSNUMAREA
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                               CALL    "ORCSNUM"   USING
                                                   ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                                   MOVE    SNUM-OUTNUM TO
                                       TBL-B-K-TTL-TEN(IDZZ KO-B-CNT)
                               END-IF
                           END-IF
                       WHEN   23
                           ADD  1          TO  KO-B-CNT
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-FTNNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   24
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-JKYNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   25
                           IF      WRK-MO-CNT  >   ZERO
                               INITIALIZE          ORCSNUMAREA
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                               CALL    "ORCSNUM"   USING
                                                   ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                                   MOVE    SNUM-OUTNUM TO
                                       TBL-B-K-TTL-TEN(IDZZ KO-B-CNT)
                               END-IF
                           END-IF
                       WHEN   26
                           ADD  1          TO  KO-B-CNT
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-FTNNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   27
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  TBL-B-JKYNUM
                                                       (IDZZ KO-B-CNT)
                           END-IF
                       WHEN   28
                           INITIALIZE          ORCSNUMAREA
                           IF      WRK-MO-CNT  >   ZERO
                               INITIALIZE          ORCSNUMAREA
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                               CALL    "ORCSNUM"   USING
                                                   ORCSNUMAREA
                               IF    ( SNUM-RC             =   ZERO  )
                               AND   ( SNUM-MINKBN         =   SPACE )
                               AND   ( SNUM-SYOKBN         =   SPACE )
                                   MOVE    SNUM-OUTNUM TO
                                       TBL-B-K-TTL-TEN(IDZZ KO-B-CNT)
                               END-IF
                           END-IF
                       WHEN   29
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-KAKUTEI-KBN(IDZZ)
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           =   "1"
                               ADD  1          TO  PATIENT-CNT
                           ELSE
                               ADD  1          TO  NOPATIENT-CNT
                           END-IF
                       WHEN   30
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-ERR-KBN(IDZZ)
                           END-IF
                       WHEN   31
                           IF      WRK-MO-CNT  >   ZERO
                               MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                           TO  TBL-B-SEIKYU-INFO (IDZZ)
                           END-IF
                   END-EVALUATE
           END-EVALUATE
      *
           .
       2001132-FIXF-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻ファイル項目別処理
      *****************************************************************
       2001133-RECEIP-KOUMOKU-BETU-SEC       SECTION.
      *??
      **   IF      WRK-MO-ST           =   ZERO
      **        MOVE    0                  TO  TBL-HENREI-B-SELECT-FLG
      **                                                       (IDZZ)
      **        MOVE    SPACE              TO  TBL-HENREI-B-HENREI-INFO1
      **                                                       (IDZZ)
      **        MOVE    WRK-PARA-REC       TO  TBL-HENREI-B-HENREI-INFO2
      **                                                       (IDZZ)
      **       GO  TO   2001133-RECEIP-KOUMOKU-BETU-EXT
      **   END-IF
           DISPLAY "WRK-PARA-REC=" WRK-PARA-REC(1:2) "#" WRK-KOUMOKU-NO
      *??
      *
           EVALUATE    WRK-PARA-REC(1:2)
               WHEN    "HI"
                   EVALUATE    WRK-KOUMOKU-NO
      *                審査支払機関コード
                       WHEN    2
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   =   "1" OR  "2"
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                       TO
                                               WRK-HENREI-K-TEISYUTUSAKI
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "審査支払機関コードエラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
      *                請求年月
                       WHEN    3
                           INITIALIZE                      ORCSGDAYAREA
                           MOVE    "1"                 TO  SGDAY-INTYPE
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"          USING
                                                       ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-SDAY (1:6)    TO
                                                   WRK-HENREI-K-SYORIYM
                           ELSE
                               MOVE    SPACE       TO  WRK-CSVERR
                               STRING  "請求年月エラー DATA="
                                                   DELIMITED  BY  SIZE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                               END-STRING
                               PERFORM 500-ERR-HENSYU-SEC
                               PERFORM 500-COBABORT-SEC
                           END-IF
                   END-EVALUATE
               WHEN    "HR"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
      *
                           MOVE    1               TO  FLG-SIKIBETU-HR
      *                処理年月
                       WHEN    2
                           INITIALIZE                      ORCSGDAYAREA
                           MOVE    "1"                 TO  SGDAY-INTYPE
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"          USING
                                                       ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               CONTINUE
                           ELSE
                               MOVE    "W01"           TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                返戻区分
                       WHEN    3
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   =   "1" OR  "2" OR
                                                       "3" OR  "4" OR
                                                       "5" OR  "6" OR
                                                       "7" OR  "8" OR
                                                       "9"
                               CONTINUE
                           ELSE
                               MOVE    "W02"           TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                返戻事由コード
                       WHEN    5
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   1    )
                           AND   ( KANACHK-MAX     <=  5    )
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                                   WRK-HENREI-B-HR-CODE
                           ELSE
                               MOVE    "W03"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                返戻理由
                       WHEN    6
                           INITIALIZE                  ORCSKANACHKAREA
                           MOVE    "1"             TO  KANACHK-SYORI
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                           CALL    "ORCSKANACHK"   USING
                                                   ORCSKANACHKAREA
                           IF    ( KANACHK-RC      =   ZERO )
                           AND   ( KANACHK-RC2     =   ZERO )
                           AND   ( KANACHK-SYUBETU =   2    )
                           AND   ( KANACHK-MAX     <=  1000 )
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                                   WRK-HENREI-B-HR-MSG
                           ELSE
                               MOVE    "W04"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF    
      *                増減点連絡書年月
                       WHEN    9
                           IF      WRK-MO-CNT      >   ZERO
                               INITIALIZE                  ORCSGDAYAREA
                               MOVE    "1"             TO  SGDAY-INTYPE
                               MOVE    WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                       TO  SGDAY-INDAY
                               CALL    "ORCSGDAY"      USING
                                                       ORCSGDAYAREA
                               IF      SGDAY-RC        =   ZERO
                                   MOVE    SGDAY-SDAY (1:6)
                                                       TO  WRK-SRYYM
                               ELSE
                                   MOVE    "W05"       TO  WRK-ERRCD
                                   PERFORM 500-ERRCD-HENSYU-SEC
                               END-IF
                           END-IF
      *                検索番号
                       WHEN    10
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               NUMERIC
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                               WRK-HENREI-B-SEARCH-NO
                           ELSE
                               MOVE    "E01"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                複数レセプト検索番号
                       WHEN    11
                           IF      WRK-MO-CNT      >   ZERO
                               IF      WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   NUMERIC
                                   CONTINUE
                               ELSE
                                   MOVE    "W07"       TO  WRK-ERRCD
                                   PERFORM 500-ERRCD-HENSYU-SEC
                               END-IF
                           END-IF
                   END-EVALUATE
               WHEN    "HG"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
      *                総件数
                       WHEN    2
                           INITIALIZE          ORCSNUMAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"   USING   ORCSNUMAREA
                           IF    ( SNUM-RC         =   ZERO   )
                           AND   ( SNUM-MINKBN     =   SPACE  )
                           AND   ( SNUM-SYOKBN     =   SPACE  )
                           AND   ( SNUM-SEISUU     <=  6      )
                           AND   ( SNUM-OUTNUM     >   ZERO   )
                               MOVE    SNUM-OUTNUM TO
                                               WRK-HENREI-K-RECV-GO-CNT
                           ELSE
                               MOVE    "W08"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                総合計点数
                       WHEN    3
                           INITIALIZE          ORCSNUMAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"   USING   ORCSNUMAREA
                           IF    ( SNUM-RC         =   ZERO   )
                           AND   ( SNUM-MINKBN     =   SPACE  )
                           AND   ( SNUM-SYOKBN     =   SPACE  )
                           AND   ( SNUM-SEISUU     <=  10     )
                           AND   ( SNUM-OUTNUM     >   ZERO   )
                               MOVE    SNUM-OUTNUM TO
                                               WRK-HENREI-K-RECV-GO-TEN
                           ELSE
                               MOVE    "W09"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                マルチボリューム識別情報
                       WHEN    4
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               NOT =   "99"
                               MOVE    "W10"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
                   END-EVALUATE
               WHEN    "RE"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           MOVE ZERO               TO  KO-B-CNT
                           ADD  1                  TO  PATIENT-CNT
      *
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
      *                レセプト番号
                       WHEN    2
                           INITIALIZE          ORCSNUMAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                           CALL    "ORCSNUM"    USING
                                               ORCSNUMAREA
                           IF    ( SNUM-RC             =   ZERO  )
                           AND   ( SNUM-MINKBN         =   SPACE )
                           AND   ( SNUM-SYOKBN         =   SPACE )
                           AND   ( SNUM-SEISUU         <=  6     )
                           AND   ( SNUM-OUTNUM         >   ZERO  )
                               MOVE    SNUM-OUTEDT (10:6)
                                                       TO
                                               WRK-HENREI-B-RECEIPT-NO
                           ELSE
                               DISPLAY "RE RECEIPT-NO ERR=" 
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
      *
                               MOVE    "E"             TO
                                           WRK-HENREI-B-RECEIPT-NO(1:1)
                               ADD     1               TO
                                           WRK-ERR-RECEIPT-NO
                               MOVE    WRK-ERR-RECEIPT-NO-X
                                                       TO
                                           WRK-HENREI-B-RECEIPT-NO(2:5)
                               MOVE    "W40"           TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                レセプト種別
                       WHEN    3
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO
                                               WRK-HENREI-B-RECESYUBETU
                           IF      WRK-HENREI-B-RECESYUBETU(4:1)  =
                                       "0" OR "2" OR "4" OR "6" OR "8"
                               MOVE    "2" TO  WRK-HENREI-B-NYUGAIKBN
                           ELSE
                               MOVE    "1" TO  WRK-HENREI-B-NYUGAIKBN
                           END-IF
      *                診療年月
                       WHEN    4
                           INITIALIZE                      ORCSGDAYAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SGDAY-INDAY
                           MOVE    "1"         TO  SGDAY-INTYPE
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-SDAY (1:6)
                                               TO  WRK-HENREI-B-SRYYM
                                                   WRK-PTID-SRYYM
                           ELSE
                               MOVE    "W41"   TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                氏名
                       WHEN    5
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-NAME
                       WHEN    6
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-SEX
                       WHEN    7
                           INITIALIZE              ORCSGDAYAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-SDAY
                                               TO  WRK-HENREI-B-BIRTHDAY
                           ELSE
                               MOVE    "W42"   TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                特記事項（０７、０８）
                       WHEN    12
                           PERFORM VARYING IDX1    FROM    1   BY  2
                                   UNTIL   IDX1    >       WRK-MO-CNT
                               COMPUTE IDX2    =   WRK-MO-ST   +   IDX1
                                                               -   1
                               IF      WRK-PARA-REC (IDX2:2)   =   "07"
                                   MOVE    "1"         TO
                                               WRK-HENREI-B-RECEIPT-KBN
                                   MOVE    WRK-MO-CNT  TO  IDX1
                               END-IF
                               IF      WRK-PARA-REC (IDX2:2)   =   "08"
                                   MOVE    "2"         TO
                                               WRK-HENREI-B-RECEIPT-KBN
                                   MOVE    WRK-MO-CNT  TO  IDX1
                               END-IF
                               IF      WRK-PARA-REC (IDX2:2)   =   "38"
                                   MOVE    "3"         TO
                                               WRK-HENREI-B-RECEIPT-KBN
                                   MOVE    WRK-MO-CNT  TO  IDX1
                               END-IF
                               IF      WRK-PARA-REC (IDX2:2)   =   "39"
                                   MOVE    "4"         TO
                                               WRK-HENREI-B-RECEIPT-KBN
                                   MOVE    WRK-MO-CNT  TO  IDX1
                               END-IF
                           END-PERFORM    
                       WHEN    14
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-PTNUM
                                                   WRK-PTNUM
                           PERFORM 500-PTID-HENSYU-SEC
                           IF      SPTNUM-RC   =   00
                               MOVE    SPTNUM-PTID TO  WRK-HENREI-B-PTID
                           ELSE
                               MOVE    "W43"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                請求情報（災害区分）
                       WHEN    21
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-SEIKYU-DATA-R
                           IF      WRK-SEIKYU-DISASTER =   SPACE
                               MOVE    "00"    TO  WRK-HENREI-B-DISASTER
                           ELSE
                               MOVE    WRK-SEIKYU-DISASTER
                                               TO  WRK-HENREI-B-DISASTER
                           END-IF
                   END-EVALUATE
               WHEN    "HO"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
                       WHEN    2
                           IF      WRK-MO-CNT      >   ZERO
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HKNJANUM
                               PERFORM VARYING IDW FROM    1   BY  1
                                       UNTIL   IDW >       8
                                   IF      WRK-HKNJANUM (IDW:1)
                                                       NOT =   SPACE
                                       MOVE    WRK-HKNJANUM (IDW:)
                                               TO  WRK-HENREI-B-HKNJANUM
                                       MOVE    8   TO  IDW
                                   END-IF
                               END-PERFORM
                           END-IF
                       WHEN   3
                           IF      WRK-MO-CNT      >   ZERO
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-KIGO
                           END-IF
                       WHEN   4
                           IF      WRK-MO-CNT      >   ZERO
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-NUM
                           END-IF
                   END-EVALUATE
               WHEN    "KO"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
                       WHEN   2
                           ADD  1                  TO  KO-B-CNT
                           MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                           WRK-HENREI-B-FTNNUM(KO-B-CNT)
                       WHEN   3
                           MOVE WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                           WRK-HENREI-B-JKYNUM(KO-B-CNT)
                    END-EVALUATE
               WHEN    "CO"
               WHEN    "KH"
               WHEN    "SY"
               WHEN    "SI"
               WHEN    "IY"
               WHEN    "TO"
               WHEN    "NI"
               WHEN    "SJ"
                   MOVE    0               TO  TBL-HENREI-B-SELECT-FLG
                                                            (IDZZ)
                   MOVE    SPACE           TO  TBL-HENREI-B-HENREI-INFO1
                                                            (IDZZ)
                   MOVE    WRK-PARA-REC    TO  TBL-HENREI-B-HENREI-INFO2
                                                            (IDZZ)
               WHEN    OTHER
                   IF      WRK-RIREKIKANRI-FLG =   1
      *              履歴管理ブロック
                     EVALUATE    WRK-KOUMOKU-NO
      *                  データ識別
                         WHEN    1
                             MOVE    1   TO  TBL-HENREI-B-SELECT-FLG
                                                            (IDZZ)
      *                  行番号
                         WHEN    2
                             INITIALIZE          ORCSNUMAREA
                             MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SNUM-INX
                             CALL    "ORCSNUM"   USING   ORCSNUMAREA
                             IF    ( SNUM-RC         =   ZERO   )
                             AND   ( SNUM-MINKBN     =   SPACE  )
                             AND   ( SNUM-SYOKBN     =   SPACE  )
                             AND   ( SNUM-SEISUU     <=  5      )
      *********************  AND   ( SNUM-OUTNUM     >   ZERO   )
                                 CONTINUE
                             ELSE
                                 MOVE    "W20"       TO  WRK-ERRCD
                                 PERFORM 500-ERRCD-HENSYU-SEC
                             END-IF
      *                  枝番号
                         WHEN    3
                             INITIALIZE          ORCSNUMAREA
                             MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                 TO  SNUM-INX
                             CALL    "ORCSNUM"   USING   ORCSNUMAREA
                             IF    ( SNUM-RC         =   ZERO   )
                             AND   ( SNUM-MINKBN     =   SPACE  )
                             AND   ( SNUM-SYOKBN     =   SPACE  )
                             AND   ( SNUM-SEISUU     <=  3      )
      *********************  AND   ( SNUM-OUTNUM     >   ZERO   )
                                 CONTINUE
                             ELSE
                                 MOVE    "W21"       TO  WRK-ERRCD
                                 PERFORM 500-ERRCD-HENSYU-SEC
                             END-IF
      *                  レコード識別情報
                         WHEN    4
                             MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                 TO  WRK-RECORD-SIKIBETU
                             IF      WRK-RECORD-SIKIBETU
                                                 =   "RC"
                                MOVE    1           TO  FLG-SIKIBETU-RC
                             END-IF
      *
                             COMPUTE IDYY    =   WRK-MO-ST   -   1
                             MOVE    WRK-PARA-REC (1:IDYY)
                                           TO  TBL-HENREI-B-HENREI-INFO1
                                                            (IDZZ)
                             MOVE    WRK-PARA-REC (WRK-MO-ST:)
                                           TO  TBL-HENREI-B-HENREI-INFO2
                                                            (IDZZ)
      *                  履歴請求データ
                         WHEN    5
                             IF      WRK-RECORD-SIKIBETU
                                               =   "RC"
      *                          管理情報（チェックデータ）
                                 INITIALIZE              ORCSKANACHKAREA
                                 MOVE    "1"         TO  KANACHK-SYORI
                                 MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  KANACHK-MAE-INPUT
                                 CALL    "ORCSKANACHK"   USING
                                                       ORCSKANACHKAREA
                                 IF    ( KANACHK-RC      =   ZERO )
                                 AND   ( KANACHK-RC2     =   ZERO )
                                 AND   ( KANACHK-SYUBETU =   1    )
                                 AND   ( KANACHK-MAX     <=  100  )
                                     MOVE
                                       WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-CHECK-NO
                                 ELSE
                                     MOVE    "E02"       TO  WRK-ERRCD
                                     PERFORM 500-ERRCD-HENSYU-SEC
                                 END-IF
                             END-IF
                     END-EVALUATE
                   ELSE
                     MOVE    0             TO  TBL-HENREI-B-SELECT-FLG
                                                            (IDZZ)
                     MOVE    SPACE         TO  TBL-HENREI-B-HENREI-INFO1
                                                            (IDZZ)
                     MOVE    WRK-PARA-REC  TO  TBL-HENREI-B-HENREI-INFO2
                                                            (IDZZ)
      *
                     MOVE    "E99"         TO  WRK-ERRCD
                     PERFORM 500-ERRCD-HENSYU-SEC
                   END-IF
           END-EVALUATE
      *
           .
       2001133-RECEIP-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    労災・返戻ファイル項目別処理
      *****************************************************************
       2001134-RRECEC-KOUMOKU-BETU-SEC     SECTION.
      *
      *??
      *     DISPLAY "WRK-KOUMOKU-NO=" WRK-KOUMOKU-NO "#"
      *              WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
      *??
      *??
      **   IF      WRK-MO-ST           =   ZERO
      **       MOVE    0               TO  TBL-HENREI-B-SELECT-FLG
      **                                                   (IDZZ)
      **       MOVE    SPACE           TO  TBL-HENREI-B-HENREI-INFO1
      **                                                   (IDZZ)
      **       MOVE    WRK-PARA-REC    TO  TBL-HENREI-B-HENREI-INFO2
      **                                                   (IDZZ)
      **       GO  TO   2001134-RRECEC-KOUMOKU-BETU-EXT
      **   END-IF
      *??
           EVALUATE    WRK-PARA-REC(1:2)
               WHEN    "RE"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           ADD  1                  TO  PATIENT-CNT
                           MOVE    SPACE           TO  WRK-SAIGAIKBN
                                                       WRK-CHOHYOKBN 
      *
                           MOVE    "3"             TO
                                       WRK-HENREI-K-TEISYUTUSAKI
      *
                           MOVE    PATIENT-CNT     TO
                                               WRK-HENREI-B-RECEIPT-NO
                           MOVE    "00"            TO
                                               WRK-HENREI-B-DISASTER
      *
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
      *                氏名
                       WHEN    5
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-NAME
                       WHEN    6
                           MOVE  WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-SEX
                       WHEN    7
                           INITIALIZE              ORCSGDAYAREA
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  SGDAY-INDAY
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC            =   ZERO
                               MOVE    SGDAY-SDAY
                                               TO  WRK-HENREI-B-BIRTHDAY
                           ELSE
                               MOVE    "W42"   TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                患者番号
                       WHEN    14
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               TO  WRK-HENREI-B-PTNUM
                                                   WRK-PTNUM
                           PERFORM 500-PTID-HENSYU-SEC
                           IF      SPTNUM-RC   =   00
                               MOVE    SPTNUM-PTID TO  WRK-HENREI-B-PTID
                           ELSE
                               MOVE    "W43"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                電算処理受付番号
                       WHEN    19
                           IF      WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                               NUMERIC
                               MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO
                                               WRK-HENREI-B-SEARCH-NO
                           ELSE
                               MOVE    "E01"       TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
      *                請求情報
                       WHEN    21
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  WRK-SEIKYU-DATA
                           IF      PATIENT-CNT     =   1
      *                        請求年月
                               INITIALIZE              ORCSGDAYAREA
                               MOVE    "1"             TO  SGDAY-INTYPE
                               MOVE
                                   WRK-SEIKYU-SKYYM    TO  SGDAY-INDAY
                               CALL    "ORCSGDAY"      USING
                                                       ORCSGDAYAREA
                               IF      SGDAY-RC        =   ZERO
                                   MOVE    SGDAY-SDAY (1:6)    TO
                                                   WRK-HENREI-K-SYORIYM
                               ELSE
                                   MOVE    SPACE       TO  WRK-CSVERR
                                   STRING  "請求年月エラー DATA="
                                                   DELIMITED  BY  SIZE
                                           WRK-PARA-REC
                                               (WRK-MO-ST:WRK-MO-CNT)
                                                   DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                                   END-STRING
                                   PERFORM 500-ERR-HENSYU-SEC
                                   PERFORM 500-COBABORT-SEC
                               END-IF
                           END-IF
      *                    診療年月
                           INITIALIZE              ORCSGDAYAREA
                           MOVE    WRK-SEIKYU-SRYYM
                                                   TO  SGDAY-INDAY
                           MOVE    "1"             TO  SGDAY-INTYPE
                           CALL    "ORCSGDAY"      USING
                                                   ORCSGDAYAREA
                           IF      SGDAY-RC        =   ZERO
                               MOVE    SGDAY-SDAY (1:6)
                                               TO  WRK-HENREI-B-SRYYM
                                                   WRK-PTID-SRYYM
                           ELSE
                               MOVE    "W41"   TO  WRK-ERRCD
                               PERFORM 500-ERRCD-HENSYU-SEC
                           END-IF
                   END-EVALUATE
               WHEN    "RR"
                   EVALUATE    WRK-KOUMOKU-NO
                       WHEN    1
                           MOVE    0               TO  
                                       TBL-HENREI-B-SELECT-FLG   (IDZZ)
                           MOVE    SPACE           TO
                                       TBL-HENREI-B-HENREI-INFO1 (IDZZ)
                           MOVE    WRK-PARA-REC    TO
                                       TBL-HENREI-B-HENREI-INFO2 (IDZZ)
      *                業務災害・通勤災害の区分
                       WHEN    3
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  WRK-SAIGAIKBN
      *                帳票種別
                       WHEN    4
                           MOVE    WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  WRK-CHOHYOKBN
                           IF      WRK-CHOHYOKBN   =   "2" OR  "4"
                               MOVE    "1" TO  WRK-HENREI-B-NYUGAIKBN
                           ELSE
                               MOVE    "2" TO  WRK-HENREI-B-NYUGAIKBN
                           END-IF
                           EVALUATE    WRK-CHOHYOKBN
                                               ALSO    WRK-SAIGAIKBN
                               WHEN    "2"     ALSO    "1"   
                                   MOVE    "7081"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "2"     ALSO    "3"
                                   MOVE    "7082"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "3"     ALSO    "1"
                                   MOVE    "7101"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "3"     ALSO    "3"
                                   MOVE    "7102"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "4"     ALSO    "1"   
                                   MOVE    "7071"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "4"     ALSO    "3"
                                   MOVE    "7072"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "5"     ALSO    "1"
                                   MOVE    "7091"  TO
                                               WRK-HENREI-B-RECESYUBETU
                               WHEN    "5"     ALSO    "3"
                                   MOVE    "7092"  TO
                                               WRK-HENREI-B-RECESYUBETU
                           END-EVALUATE
      *                年金証書番号
                       WHEN    5
                           IF      WRK-MO-CNT      >   ZERO
                               IF   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   NOT =   SPACE
                                   MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  WRK-HENREI-B-KIGO
                               END-IF
                           END-IF
      *                労働保険番号
                       WHEN    6
                           IF      WRK-MO-CNT      >   ZERO
                               IF   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   NOT =   SPACE
                                   MOVE
                                   WRK-PARA-REC (WRK-MO-ST:WRK-MO-CNT)
                                                   TO  WRK-HENREI-B-KIGO
                               END-IF
                           END-IF
                   END-EVALUATE
               WHEN    "SY"
               WHEN    "RI"
               WHEN    "IY"
               WHEN    "TO"
               WHEN    "CO"
               WHEN    "SJ"
                   MOVE    0               TO  TBL-HENREI-B-SELECT-FLG
                                                            (IDZZ)
                   MOVE    SPACE           TO  TBL-HENREI-B-HENREI-INFO1
                                                            (IDZZ)
                   MOVE    WRK-PARA-REC    TO  TBL-HENREI-B-HENREI-INFO2
                                                            (IDZZ)
               WHEN    OTHER
                   MOVE    0               TO  TBL-HENREI-B-SELECT-FLG
                                                            (IDZZ)
                   MOVE    SPACE           TO  TBL-HENREI-B-HENREI-INFO1
                                                            (IDZZ)
                   MOVE    WRK-PARA-REC    TO  TBL-HENREI-B-HENREI-INFO2
                                                            (IDZZ)
      *
                   MOVE    "E99"           TO  WRK-ERRCD
                   PERFORM 500-ERRCD-HENSYU-SEC
           END-EVALUATE
      *
           .
       2001134-RRECEC-KOUMOKU-BETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新処理
      *****************************************************************
       20011-DB-UPDATE-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    管理ファイル読み込み（診療年月、提出先）
           MOVE    ZERO            TO  FLG-MATCH
           MOVE    ZERO            TO  TBL-ONRECE-K-MAX
      *
           INITIALIZE                  ONRECE-K-REC
           MOVE   SPA-HOSPNUM      TO  ONRECE-K-HOSPNUM
           MOVE   WRK-TEISYUTUSAKI TO  ONRECE-K-TEISYUTUSAKI
           MOVE   WRK-SRYYM        TO  ONRECE-K-SRYYM
           PERFORM 900-ONRECE-KANRIANRI-SELECT-SEC
      *
           PERFORM             UNTIL   FLG-ONRECE-K    =   1
      *        該当の医療機関コードのときだけ対象
               IF      ONRECE-K-HOSPCD =   SPACE
                   MOVE    ONRECE-K-HOSPNUM    TO  ONRECE-B-HOSPNUM
                   MOVE    ONRECE-K-TEISYUTUSAKI
                                               TO  ONRECE-B-TEISYUTUSAKI
                   MOVE    ONRECE-K-SRYYM      TO  ONRECE-B-SRYYM
                   MOVE    ONRECE-K-HOSPCD     TO  ONRECE-B-HOSPCD
                   MOVE    ONRECE-K-UCNT       TO  ONRECE-B-UCNT
                   MOVE    ONRECE-K-SCNT       TO  ONRECE-B-SCNT
                   MOVE    ONRECE-K-RENNUM     TO  ONRECE-B-RENNUM
                   MOVE    "key8"              TO  WRK-MCP-PATHNAME
                   PERFORM 900-ONRECE-B-SELECT-SEC
                   PERFORM 900-ONRECE-B-CLOSE-SEC
                   IF      FLG-ONRECE-B        =   ZERO
                       INITIALIZE                  SYS-1001-REC
                       MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                       MOVE    "1001"              TO  SYS-1001-KANRICD
                       MOVE    "*"                 TO  SYS-1001-KBNCD
                       MOVE    ONRECE-B-SRYYM-B    TO  SYS-1001-STYUKYMD
                                                               (1:6)
                       MOVE    "01"                TO  SYS-1001-STYUKYMD
                                                               (7:2)
                       MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
                       MOVE    SYS-1001-REC        TO  MCPDATA-REC
                       PERFORM 900-SYSKANRI-INV-SEC
                       IF      FLG-SYSKANRI        =   ZERO
                           MOVE    MCPDATA-REC         TO  SYS-1001-REC
                           IF      WRK-PARA-HOSPCD (IDXX)
                                                   =   SYS-1001-HOSPCD
                               ADD 1               TO  TBL-ONRECE-K-MAX
                               IF      TBL-ONRECE-K-MAX    >
                                                   WRK-CONS-ONRECE-K-MAX
                                   MOVE
                                   "*** ORCBM550 TABLE OVER  ONRECE-K"
                                                       TO  WRK-CSVERR
                                   PERFORM 500-ERR-HENSYU-SEC
                                   PERFORM 500-COBABORT-SEC
                               END-IF
                               MOVE ONRECE-K-REC   TO  TBL-ONRECE-K-REC
                                                   (TBL-ONRECE-K-MAX)
                           END-IF
                       END-IF
                   END-IF 
               ELSE
                   IF      WRK-PARA-HOSPCD (IDXX)
                                       =   ONRECE-K-HOSPCD
                       ADD  1                  TO  TBL-ONRECE-K-MAX
                       IF      TBL-ONRECE-K-MAX    >
                                                   WRK-CONS-ONRECE-K-MAX
                           MOVE    "*** ORCBM550 TABLE OVER  ONRECE-K"
                                                       TO  WRK-CSVERR
                           PERFORM 500-ERR-HENSYU-SEC
                           PERFORM 500-COBABORT-SEC
                       END-IF
                       MOVE ONRECE-K-REC       TO  TBL-ONRECE-K-REC
                                                   (TBL-ONRECE-K-MAX)
                   END-IF
               END-IF
      * 
               PERFORM 900-ONRECE-KANRI-FETCH-SEC
           END-PERFORM
           PERFORM 900-ONRECE-KANRI-CLOSE-SEC
      *
      *??
           DISPLAY "TBL-ONRECE-K-MAX=" TBL-ONRECE-K-MAX "#"
      *??
      *    管理データなしのとき
           IF      TBL-ONRECE-K-MAX       =   ZERO
      *        ＤＢ追加処理 
               PERFORM  200111-DB-INSERT-SEC
           ELSE
      *
      *        チェック処理
               PERFORM VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX           >   TBL-ONRECE-K-MAX )
                       OR      ( FLG-MATCH     =   1                )
                   PERFORM 200113-ONRECE-K-CHK-SEC
               END-PERFORM
      *??
           DISPLAY "FLG-MATCH=" FLG-MATCH "#"
      *??
      *
               IF      FLG-MATCH           =  ZERO
      *            ＤＢ追加処理 
                   PERFORM 200111-DB-INSERT-SEC
               END-IF
               IF      FLG-MATCH           =  1
                   EVALUATE    WRK-FILE-TMP (1:6)
                       WHEN    "RIhasp"
                       WHEN    "RIuasp"
      *                    受付更新処理
                           PERFORM 200114-HASP-MOD-SEC
                       WHEN    "RIfixf"
      *                    請求確定更新処理
                           PERFORM 200115-FIXF-MOD-SEC
                   END-EVALUATE
               END-IF
           END-IF    
      *
           .
       20011-DB-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ追加処理 
      *****************************************************************
       200111-DB-INSERT-SEC         SECTION.
      *
      *    管理データ項目設定処理
           PERFORM 200112-ONRECE-K-SET-SEC
      *
      *    処理年月日の設定
           EVALUATE    WRK-FILE-TMP (1:6)
               WHEN    "RIhasp"
               WHEN    "RIuasp"
                   MOVE    ONRECE-K-UKEYMD TO  ONRECE-K-SYORIYMD
               WHEN    "RIfixf"
                   MOVE    ONRECE-K-SKYYMD TO  ONRECE-K-SYORIYMD
           END-EVALUATE
      *
           PERFORM 900-ONRECE-KANRI-INSERT-SEC
      *
           PERFORM VARYING   IDZZ  FROM    1   BY  1
                   UNTIL   ( IDZZ          >   WRK-CONS-ONRECE-B-MAX )
                   OR      ( TBL-B-HOSPNUM(IDZZ)
                                           =   ZERO                 )
               MOVE    TBL-ONRECE-B-REC(IDZZ)  TO  ONRECE-B-REC
               MOVE    ONRECE-K-SYORIYMD       TO  ONRECE-B-SYORIYMD
               EVALUATE    WRK-FILE-TMP (1:6)
                   WHEN    "RIhasp"
                   WHEN    "RIuasp"
                       MOVE    "2"             TO  ONRECE-B-KAKUTEI-KBN
               END-EVALUATE
               PERFORM 900-ONRECE-B-INSERT-SEC
           END-PERFORM
      *
           .
       200111-DB-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理データ項目設定処理
      *****************************************************************
       200112-ONRECE-K-SET-SEC        SECTION.
      *
           INITIALIZE                      ONRECE-K-REC
           MOVE   SPA-HOSPNUM          TO  ONRECE-K-HOSPNUM
           MOVE   WRK-TEISYUTUSAKI     TO  ONRECE-K-TEISYUTUSAKI
           MOVE   WRK-SRYYM            TO  ONRECE-K-SRYYM
           MOVE   WRK-PARA-HOSPCD (IDXX)
                                       TO  ONRECE-K-HOSPCD 
           MOVE   WRK-SKYYMD           TO  ONRECE-K-SKYYMD
           MOVE   WRK-UKEYMD           TO  ONRECE-K-UKEYMD
           MOVE   WRK-RECV-FLG         TO  ONRECE-K-RECV-FLG
           MOVE   WRK-AMOUNT-FLG       TO  ONRECE-K-AMOUNT-FLG
           MOVE   UKE-CNT              TO  ONRECE-K-UCNT
           MOVE   SEI-CNT              TO  ONRECE-K-SCNT
      *
           MOVE   IDZZ                 TO  ONRECE-K-SEND-TOTAL-CNT
           MOVE   PATIENT-CNT          TO  ONRECE-K-SEND-PATIENT-CNT
           MOVE   NOPATIENT-CNT        TO  ONRECE-K-NOSEND-PATIENT-CNT
      *
           .
       200112-ONRECE-K-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理データチェック処理
      *****************************************************************
       200113-ONRECE-K-CHK-SEC         SECTION.
      *
           EVALUATE    WRK-FILE-TMP(1:6)  
               WHEN    "RIhasp"
               WHEN    "RIuasp"
                   IF    ( TBL-K-HOSPNUM(IDX)  =   SPA-HOSPNUM      )
                   AND   ( TBL-K-TEISYUTUSAKI(IDX) 
                                               =   WRK-TEISYUTUSAKI )
                   AND   ( TBL-K-SRYYM (IDX)   =   WRK-SRYYM        )
                   AND   ( TBL-K-UCNT  (IDX)   =   UKE-CNT          )
                       IF      TBL-K-UKEYMD(IDX)   =   SPACE
                           MOVE    1                   TO  FLG-MATCH
                           MOVE  IDX                   TO  HIT-K
                           MOVE    TBL-ONRECE-K-MAX    TO  IDX
                       ELSE
                           IF      TBL-K-UKEYMD(IDX)   =   WRK-UKEYMD
      *********************    PERFORM 2001131-HASP-CHK-SEC      
                               DISPLAY "RIhasp perfect hit OK !!"
                               MOVE    2                   TO  FLG-MATCH
                               MOVE    TBL-ONRECE-K-MAX    TO  IDX
                           ELSE
      *********************    PERFORM 2001131-HASP-CHK-SEC      
                               DISPLAY "RIhasp ukeymd unmatych !!"
                               MOVE    2                   TO  FLG-MATCH
                               MOVE    TBL-ONRECE-K-MAX    TO  IDX
                           END-IF
                       END-IF
                   END-IF
               WHEN    "RIfixf"
                   IF    ( TBL-K-HOSPNUM(IDX)  =   SPA-HOSPNUM      )
                   AND   ( TBL-K-TEISYUTUSAKI(IDX) 
                                               =   WRK-TEISYUTUSAKI )
                   AND   ( TBL-K-SRYYM (IDX)   =   WRK-SRYYM        )
      *                 
      *****************IF    ( TBL-K-UCNT(IDX) NOT =   UKE-CNT )
      *****************AND   ( TBL-K-SCNT(IDX)     =   SEI-CNT )
      *****************    DISPLAY "RIfixf ucnt unmatch !! "
      *****************    MOVE    ZERO                TO  FLG-MATCH
      *****************    MOVE    TBL-ONRECE-K-MAX    TO  IDX
      *****************END-IF
      *                 
                       IF    ( TBL-K-UCNT(IDX)     =   UKE-CNT     )
                       AND   ( TBL-K-SCNT(IDX)     =   SEI-CNT     )
                       AND   ( TBL-K-SKYYMD(IDX)   =   WRK-SKYYMD  ) 
                       AND   ( TBL-K-SEND-PATIENT-CNT(IDX)
                                                   =   PATIENT-CNT )
                           MOVE  2                     TO  FLG-MATCH
                           MOVE  TBL-ONRECE-K-MAX      TO  IDX
                       END-IF    
      *                 
                       IF    ( TBL-K-UCNT(IDX)     =   UKE-CNT    )
                       AND   ( TBL-K-SCNT(IDX)     =   ZERO       )
      *****************IF    ( TBL-K-UKEYMD(IDX)   <=  WRK-SKYYMD )
                           MOVE    1                   TO  FLG-MATCH
                           MOVE  IDX                   TO  HIT-K
                           MOVE    TBL-ONRECE-K-MAX    TO  IDX
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *
           .
       200113-ONRECE-K-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細データチェック処理
      *****************************************************************
       2001131-HASP-CHK-SEC    SECTION.
      *
           PERFORM VARYING IDYY    FROM    1   BY  1
                   UNTIL   IDYY            >   WRK-CONS-ONRECE-B-MAX
                   OR      TBL-B-HOSPNUM(IDYY)
                                           =   ZERO
               MOVE    TBL-K-HOSPNUM(IDX)  TO  ONRECE-B-HOSPNUM
               MOVE    TBL-K-TEISYUTUSAKI(IDX)
                                           TO  ONRECE-B-TEISYUTUSAKI
               MOVE    TBL-K-SRYYM(IDX)    TO  ONRECE-B-SRYYM
               MOVE    TBL-K-HOSPCD(IDX)   TO  ONRECE-B-HOSPCD
               MOVE    TBL-K-UCNT(IDX)     TO  ONRECE-B-UCNT
               MOVE    TBL-K-SCNT(IDX)     TO  ONRECE-B-SCNT
               MOVE    TBL-K-RENNUM(IDX)   TO  ONRECE-B-RENNUM
               MOVE    TBL-B-RECEIPT-NO(IDYY)
                                           TO  ONRECE-B-RECEIPT-NO
               MOVE    "key"               TO  WRK-MCP-PATHNAME
               PERFORM 900-ONRECE-B-SELECT-SEC
               IF      FLG-ONRECE-B        =   ZERO
                   IF    ( TBL-B-NYUGAIKBN(IDYY)
                                               =   ONRECE-B-NYUGAIKBN )
                   AND   ( TBL-B-SRYYM-B(IDYY) =   ONRECE-B-SRYYM-B   )
                   AND   ( TBL-B-NAME   (IDYY) =   ONRECE-B-NAME      )
                   AND   ( TBL-B-BIRTHDAY(IDYY)
                                               =   ONRECE-B-BIRTHDAY  )
                       IF      TBL-B-PTNUM(IDYY)
                                               NOT =   SPACE
                           IF    ( TBL-B-PTNUM(IDYY)   =
                                                       ONRECE-B-PTNUM )
                           AND   ( TBL-B-PTID (IDYY)   =
                                                       ONRECE-B-PTID  ) 
                               MOVE  1                     TO  FLG-MATCH            
                               DISPLAY "hasp perfect match !!    " 
                           ELSE
                               MOVE  ZERO                  TO  FLG-MATCH            
                               MOVE  WRK-CONS-ONRECE-B-MAX TO  IDYY
                               DISPLAY "hasp no match !!    " 
                           END-IF
                       ELSE
                           DISPLAY "hasp perfect match !!    " 
                       END-IF
                   ELSE
                       MOVE  ZERO              TO  FLG-MATCH            
                       MOVE  WRK-CONS-ONRECE-B-MAX
                                               TO  IDYY
                   END-IF
               ELSE
                   MOVE  ZERO              TO  FLG-MATCH            
                   MOVE  WRK-CONS-ONRECE-B-MAX
                                           TO  IDYY
               END-IF
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-ONRECE-B-CLOSE-SEC
           END-PERFORM
      *
           .
       2001131-HASP-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付更新処理
      *****************************************************************
       200114-HASP-MOD-SEC         SECTION.
      *
      *    管理レコード更新
           MOVE    TBL-ONRECE-K-REC (HIT-K)
                                       TO  ONRECE-K-REC
           MOVE    UKE-CNT             TO  ONRECE-K-UCNT
           MOVE    WRK-UKEYMD          TO  ONRECE-K-UKEYMD
           MOVE    WRK-RECV-FLG        TO  ONRECE-K-RECV-FLG
      *
           MOVE    TBL-K-UCNT (HIT-K)  TO  ONRECE-K-UCNT-V
           MOVE    TBL-K-SCNT (HIT-K)  TO  ONRECE-K-SCNT-V
           PERFORM 900-ONRECE-KANRI-UPDATE-SEC
      *
      *    明細レコード更新（エラー分）
           PERFORM VARYING   IDZZ  FROM    1   BY  1
                   UNTIL     IDZZ          >   WRK-CONS-ONRECE-B-MAX
                   OR        TBL-B-HOSPNUM(IDZZ)
                                           =   ZERO
               MOVE    TBL-ONRECE-B-REC(IDZZ)
                                           TO  ONRECE-B-REC
               MOVE    TBL-K-UCNT  (HIT-K) TO  ONRECE-B-UCNT
               MOVE    TBL-K-SCNT  (HIT-K) TO  ONRECE-B-SCNT
               MOVE    TBL-K-RENNUM(HIT-K) TO  ONRECE-B-RENNUM
               MOVE    "key"               TO  WRK-MCP-PATHNAME
               PERFORM 900-ONRECE-B-SELECT-SEC
               PERFORM 900-ONRECE-B-CLOSE-SEC
      *
               IF      FLG-ONRECE-B    =   ZERO
                   IF    ( TBL-B-NYUGAIKBN (IDZZ)
                                           =   ONRECE-B-NYUGAIKBN )
                   AND   ( TBL-B-SRYYM-B (IDZZ)
                                           =   ONRECE-B-SRYYM-B   )
                   AND   ( TBL-B-NAME    (IDZZ)
                                           =   ONRECE-B-NAME      )
                   AND   ( TBL-B-BIRTHDAY (IDZZ)
                                           =   ONRECE-B-BIRTHDAY  )
                       IF      TBL-B-PTNUM(IDYY)
                                               NOT =   SPACE
                           IF    ( TBL-B-PTNUM (IDZZ)  =
                                                       ONRECE-B-PTNUM )
                           AND   ( TBL-B-PTID  (IDZZ)  =
                                                       ONRECE-B-PTID  )
                               CONTINUE
                           ELSE
                               DISPLAY "hasp PTNUM no match !! PTNUM="
                                           TBL-B-PTNUM (IDZZ) " PTID="
                                           TBL-B-PTID  (IDZZ) 
                           END-IF
                       END-IF
                   ELSE
                       DISPLAY "hasp no match !! NYUGAIKBN="
                                   TBL-B-NYUGAIKBN (IDZZ) " SRYYM="
                                   TBL-B-SRYYM-B   (IDZZ) " NAME="
                                   TBL-B-NAME      (IDZZ) " BIRTHDAY="
                                   TBL-B-BIRTHDAY  (IDZZ) 
                   END-IF
      *
                   MOVE    TBL-ONRECE-B-REC(IDZZ)  
                                       TO  WRK-ONRECE-B-REC
                   MOVE    UKE-CNT     TO  ONRECE-K-UCNT
                   MOVE    WRK-ONRECE-B-UKEYMD
                                       TO  ONRECE-B-UKEYMD
                   MOVE    WRK-ONRECE-B-RECV-FLG
                                       TO  ONRECE-B-RECV-FLG
                   MOVE    WRK-ONRECE-B-U-ERR-CODE
                                       TO  ONRECE-B-U-ERR-CODE
                   MOVE    WRK-ONRECE-B-U-ERR-MSG-1
                                       TO  ONRECE-B-U-ERR-MSG-1
                   MOVE    WRK-ONRECE-B-U-ERR-MSG-2
                                       TO  ONRECE-B-U-ERR-MSG-2
                   MOVE    WRK-ONRECE-B-U-ERR-SRYKBN
                                       TO  ONRECE-B-U-ERR-SRYKBN
                   MOVE    WRK-ONRECE-B-U-ERR-INFO
                                       TO  ONRECE-B-U-ERR-INFO
                   MOVE    WRK-ONRECE-B-U-TEN-NG
                                       TO  ONRECE-B-U-TEN-NG
                   MOVE    WRK-ONRECE-B-U-TEN-OK
                                       TO  ONRECE-B-U-TEN-OK
                   MOVE    WRK-ONRECE-B-U-HENREI-FLG
                                       TO  ONRECE-B-U-HENREI-FLG
                   MOVE    TBL-K-UCNT(HIT-K)   
                                       TO  ONRECE-B-UCNT-V
                   MOVE    TBL-K-SCNT(HIT-K)   
                                       TO  ONRECE-B-SCNT-V
                   MOVE    "upd1"      TO  WRK-MCP-PATHNAME
                   PERFORM 900-ONRECE-B-UPDATE-SEC
               ELSE
                   DISPLAY "hasp no fetch !! UCNT="
                           TBL-K-UCNT  (HIT-K) " SCNT="
                           TBL-K-SCNT  (HIT-K) " RENNUM="
                           TBL-K-RENNUM(HIT-K) " RECEIPT-NO="
                           TBL-B-RECEIPT-NO (IDZZ)
               END-IF
           END-PERFORM
      *
      *    明細レコード更新
           MOVE    TBL-ONRECE-B-REC (1)
                                       TO  ONRECE-B-REC
           MOVE    UKE-CNT             TO  ONRECE-B-UCNT
           MOVE    WRK-UKEYMD          TO  ONRECE-B-UKEYMD
           MOVE    WRK-RECV-FLG        TO  ONRECE-B-RECV-FLG
           MOVE    TBL-K-RENNUM(HIT-K) TO  ONRECE-B-RENNUM
           MOVE    TBL-K-UCNT  (HIT-K) TO  ONRECE-B-UCNT-V
           MOVE    TBL-K-SCNT  (HIT-K) TO  ONRECE-B-SCNT-V
           MOVE    "upd2"              TO  WRK-MCP-PATHNAME
           PERFORM 900-ONRECE-B-UPDATE-SEC
      *
           .
       200114-HASP-MOD-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求確定更新処理
      *****************************************************************
       200115-FIXF-MOD-SEC         SECTION.
      *
      *    管理レコード更新
           MOVE   TBL-ONRECE-K-REC (HIT-K)
                                       TO  ONRECE-K-REC
           MOVE   SEI-CNT              TO  ONRECE-K-SCNT
           MOVE   WRK-SKYYMD           TO  ONRECE-K-SKYYMD
                                           ONRECE-K-SYORIYMD
           MOVE   WRK-AMOUNT-FLG       TO  ONRECE-K-AMOUNT-FLG
           MOVE   PATIENT-CNT          TO  ONRECE-K-SEND-PATIENT-CNT
           MOVE   IDZZ                 TO  ONRECE-K-SEND-TOTAL-CNT
           MOVE   NOPATIENT-CNT        TO  ONRECE-K-NOSEND-PATIENT-CNT
      *
           MOVE    TBL-K-UCNT(HIT-K)   TO  ONRECE-K-UCNT-V
           MOVE    TBL-K-SCNT(HIT-K)   TO  ONRECE-K-SCNT-V
           PERFORM 900-ONRECE-KANRI-UPDATE-SEC
      *
           PERFORM VARYING   IDZZ  FROM    1   BY  1
                   UNTIL   ( IDZZ          >   WRK-CONS-ONRECE-B-MAX )
                   OR      ( TBL-B-HOSPNUM(IDZZ)     =   ZERO        )
               MOVE    TBL-ONRECE-B-REC(IDZZ)
                                           TO  ONRECE-B-REC
               MOVE    TBL-K-UCNT  (HIT-K) TO  ONRECE-B-UCNT
               MOVE    TBL-K-SCNT  (HIT-K) TO  ONRECE-B-SCNT
               MOVE    TBL-K-RENNUM(HIT-K) TO  ONRECE-B-RENNUM
               MOVE    TBL-B-RECEIPT-NO (IDZZ)
                                           TO  ONRECE-B-RECEIPT-NO
               MOVE    "key"               TO  WRK-MCP-PATHNAME
               PERFORM 900-ONRECE-B-SELECT-SEC
               PERFORM 900-ONRECE-B-CLOSE-SEC
      *
               IF      FLG-ONRECE-B    =   ZERO
                   IF      TBL-B-KAKUTEI-KBN (IDZZ)    =  "2"
                       IF    ( TBL-B-NYUGAIKBN (IDZZ)
                                               =   ONRECE-B-NYUGAIKBN )
                       AND   ( TBL-B-SRYYM-B (IDZZ)
                                               =   ONRECE-B-SRYYM-B   )
                       AND   ( TBL-B-NAME    (IDZZ)
                                               =   ONRECE-B-NAME      )
                       AND   ( TBL-B-BIRTHDAY (IDZZ)
                                               =   ONRECE-B-BIRTHDAY  )
                           IF      TBL-B-PTNUM(IDYY)
                                                   NOT =   SPACE
                               IF    ( TBL-B-PTNUM (IDZZ)  =
                                                       ONRECE-B-PTNUM )
                               AND   ( TBL-B-PTID  (IDZZ)  =
                                                       ONRECE-B-PTID  )
                                   CONTINUE
                               ELSE
                                   DISPLAY
                                       "fixf PTNUM no match !! PTNUM="
                                           TBL-B-PTNUM (IDZZ) " PTID="
                                           TBL-B-PTID  (IDZZ) 
                               END-IF
                           END-IF
                       ELSE
                           DISPLAY "fixf no match !! NYUGAIKBN="
                                   TBL-B-NYUGAIKBN (IDZZ) " SRYYM="
                                   TBL-B-SRYYM-B   (IDZZ) " NAME="
                                   TBL-B-NAME      (IDZZ) " BIRTHDAY="
                                   TBL-B-BIRTHDAY  (IDZZ) 
                       END-IF
                   ELSE
                       DISPLAY "fixf noerr fetch !! "
                   END-IF
               ELSE
                   IF      TBL-B-KAKUTEI-KBN (IDZZ)    =  "2"
                       DISPLAY "fixf err no fetch !! "
                   END-IF
               END-IF
      *
               IF      FLG-ONRECE-B    =   ZERO
                   IF      TBL-B-KAKUTEI-KBN (IDZZ)    =  "2"
                       MOVE    ONRECE-B-REC    TO  WRK-ONRECE-B-REC
                       MOVE    TBL-ONRECE-B-REC(IDZZ)  
                                               TO  ONRECE-B-REC
                       MOVE    SEI-CNT         TO  ONRECE-K-SCNT
                       MOVE    WRK-ONRECE-B-SYORIYMD
                                               TO  ONRECE-B-SYORIYMD
                       MOVE    WRK-ONRECE-B-UKEYMD
                                               TO  ONRECE-B-UKEYMD
                       MOVE    WRK-ONRECE-B-RECV-FLG
                                               TO  ONRECE-B-RECV-FLG
                       MOVE    WRK-ONRECE-B-U-ERR-CODE
                                               TO  ONRECE-B-U-ERR-CODE
                       MOVE    WRK-ONRECE-B-U-ERR-MSG-1
                                               TO  ONRECE-B-U-ERR-MSG-1
                       MOVE    WRK-ONRECE-B-U-ERR-MSG-2
                                               TO  ONRECE-B-U-ERR-MSG-2
                       MOVE    WRK-ONRECE-B-U-ERR-SRYKBN
                                               TO  ONRECE-B-U-ERR-SRYKBN
                       MOVE    WRK-ONRECE-B-U-ERR-INFO
                                               TO  ONRECE-B-U-ERR-INFO
                       MOVE    WRK-ONRECE-B-U-TEN-NG
                                               TO  ONRECE-B-U-TEN-NG
                       MOVE    WRK-ONRECE-B-U-TEN-OK
                                               TO  ONRECE-B-U-TEN-OK
                       MOVE    WRK-ONRECE-B-U-HENREI-FLG
                                               TO  ONRECE-B-U-HENREI-FLG
                       MOVE    TBL-K-UCNT(HIT-K)   
                                               TO  ONRECE-B-UCNT-V
                       MOVE    TBL-K-SCNT(HIT-K)   
                                               TO  ONRECE-B-SCNT-V
                       MOVE    "upd1"      TO  WRK-MCP-PATHNAME
                       PERFORM 900-ONRECE-B-UPDATE-SEC
                   END-IF
               ELSE    
                   MOVE    TBL-ONRECE-B-REC (IDZZ) TO  ONRECE-B-REC
                   MOVE    TBL-K-UCNT     (HIT-K)  TO  ONRECE-B-UCNT
                   MOVE    SEI-CNT                 TO  ONRECE-B-SCNT
                   MOVE    TBL-K-RENNUM   (HIT-K)  TO  ONRECE-B-RENNUM
                   MOVE    TBL-K-UKEYMD   (HIT-K)  TO  ONRECE-B-UKEYMD
                   MOVE    TBL-K-RECV-FLG (HIT-K)  TO  ONRECE-B-RECV-FLG
                   MOVE    WRK-SKYYMD              TO  ONRECE-B-SYORIYMD
                   PERFORM 900-ONRECE-B-INSERT-SEC
               END-IF
           END-PERFORM
           .
      *
       200115-FIXF-MOD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新処理（返戻ファイル）
      *****************************************************************
       20021-DB-UPDATE-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    返戻明細ファイル更新
      *
           IF      FLG-SIKIBETU-HR     =   ZERO
               MOVE    "E10"               TO  TBL-HENREI-B-ERRCD(1)
           ELSE
               IF      FLG-SIKIBETU-RC     =   ZERO
                   MOVE    "E11"               TO  TBL-HENREI-B-ERRCD(1)
               END-IF
           END-IF
      *
           DISPLAY "HENREI-B-SRYYM=" WRK-HENREI-B-SRYYM
                   " NYUGAIKBN="     WRK-HENREI-B-NYUGAIKBN
                   " RECESYUBETU="   WRK-HENREI-B-RECESYUBETU
                   " PTNUM="         WRK-HENREI-B-PTNUM
                   " COUNT="         IDZZ
      *
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL     IDX           >   IDZZ
               IF      TBL-HENREI-B-ERRCD (IDX)
                                       NOT =   SPACE
                   DISPLAY "ERRCD=" IDX " "  TBL-HENREI-B-ERRCD (IDX)
               END-IF
      *
               MOVE    WRK-HENREI-B-REC        TO  HENREI-B-REC
               MOVE    WRK-PARA-HOSPNUM        TO  HENREI-B-HOSPNUM
               MOVE    WRK-HENREI-K-TEISYUTUSAKI
                                               TO  HENREI-B-TEISYUTUSAKI
               MOVE    WRK-HENREI-K-TEISYUTUSAKI2
                                               TO
                                               HENREI-B-TEISYUTUSAKI2
               MOVE    WRK-HENREI-K-SYORIYM    TO  HENREI-B-SYORIYM
               MOVE    WRK-HENREI-K-HOSPCD     TO  HENREI-B-HOSPCD
               MOVE    IDX                     TO  HENREI-B-PT-RENNUM
               MOVE    TBL-HENREI-B-SELECT-FLG   (IDX)
                                               TO  HENREI-B-SELECT-FLG
               MOVE    TBL-HENREI-B-HENREI-INFO1 (IDX)
                                               TO  HENREI-B-HENREI-INFO1  
               MOVE    TBL-HENREI-B-HENREI-INFO2 (IDX)
                                               TO  HENREI-B-HENREI-INFO2  
               MOVE    TBL-HENREI-B-ERRCD        (IDX)
                                               TO  HENREI-B-ERRCD
               MOVE    SMCNDATE-YMD            TO  HENREI-B-CREYMD
      *
               MOVE    HENREI-B-REC            TO  MCPDATA-REC
      *
               MOVE   "tbl_henrei_body"        TO  MCP-TABLE
               MOVE   "key"                    TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   ADD     1                   TO  INS-B-CNT
               ELSE 
                   INITIALIZE          ONRECE-K-REC
                   CALL "coblog" USING "abort insert tbl_henrei_body"
      *
                   MOVE    SPACE               TO  WRK-CSVERR
                   MOVE    MCP-RC              TO  WRK-MCP-RC
                   STRING  "返戻明細ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-PERFORM
      *
      *    返戻管理ファイル更新
           IF      FLG-PARA            =   1
               DISPLAY "HENREI-K-TEISYUTUSAKI ="
                                           WRK-HENREI-K-TEISYUTUSAKI
                       " TEISYUTUSAKI2="   WRK-HENREI-K-TEISYUTUSAKI2
                       " SYORIYM="         WRK-HENREI-K-SYORIYM
                       " HOSPCD="          WRK-HENREI-K-HOSPCD
                       " RECV-TOTAL-CNT="  INS-B-CNT
                       " RECV-PATIENT-CNT="  PATIENT-CNT
      *
               MOVE    WRK-HENREI-K-REC    TO  HENREI-K-REC
               MOVE    INS-B-CNT           TO  HENREI-K-RECV-TOTAL-CNT
               MOVE    PATIENT-CNT         TO  HENREI-K-RECV-PATIENT-CNT
               MOVE    WRK-PARA-HOSPNUM    TO  HENREI-K-HOSPNUM
               MOVE    SMCNDATE-YMD        TO  HENREI-K-CREYMD
               MOVE    HENREI-K-REC        TO  MCPDATA-REC
      *
               MOVE   "tbl_henrei_kanri"   TO  MCP-TABLE
               MOVE   "key"                TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   ADD     1                   TO  INS-K-CNT
               ELSE 
                   CALL "coblog" USING "abort insert tbl_henrei_kanri"
      *
                   MOVE    SPACE               TO  WRK-CSVERR
                   MOVE    MCP-RC              TO  WRK-MCP-RC
                   STRING  "返戻管理ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-IF
      *
           .
       20021-DB-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ更新処理（労災・返戻ファイル）
      *****************************************************************
       20031-DB-UPDATE-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    返戻明細ファイル更新
      *
           DISPLAY "HENREI-B-SRYYM=" WRK-HENREI-B-SRYYM
                   " NYUGAIKBN="     WRK-HENREI-B-NYUGAIKBN
                   " RECESYUBETU="   WRK-HENREI-B-RECESYUBETU
                   " PTNUM="         WRK-HENREI-B-PTNUM
                   " COUNT="         IDZZ
      *
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL     IDX           >   IDZZ
               MOVE    WRK-HENREI-B-REC        TO  HENREI-B-REC
               MOVE    WRK-PARA-HOSPNUM        TO  HENREI-B-HOSPNUM
               MOVE    WRK-HENREI-K-TEISYUTUSAKI
                                               TO  HENREI-B-TEISYUTUSAKI
               MOVE    WRK-HENREI-K-TEISYUTUSAKI2
                                               TO
                                               HENREI-B-TEISYUTUSAKI2
               MOVE    WRK-HENREI-K-SYORIYM    TO  HENREI-B-SYORIYM
               MOVE    WRK-HENREI-K-HOSPCD     TO  HENREI-B-HOSPCD
               MOVE    IDX                     TO  HENREI-B-PT-RENNUM
               MOVE    TBL-HENREI-B-SELECT-FLG   (IDX)
                                               TO  HENREI-B-SELECT-FLG
               MOVE    TBL-HENREI-B-HENREI-INFO1 (IDX)
                                               TO  HENREI-B-HENREI-INFO1  
               MOVE    TBL-HENREI-B-HENREI-INFO2 (IDX)
                                               TO  HENREI-B-HENREI-INFO2  
               MOVE    TBL-HENREI-B-ERRCD        (IDX)
                                               TO  HENREI-B-ERRCD
               MOVE    SMCNDATE-YMD            TO  HENREI-B-CREYMD
      *
               MOVE    HENREI-B-REC            TO  MCPDATA-REC
      *
               MOVE   "tbl_henrei_body"        TO  MCP-TABLE
               MOVE   "key"                    TO  MCP-PATHNAME
               MOVE    "DBINSERT"              TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   ADD     1                   TO  INS-B-CNT
               ELSE 
                   INITIALIZE          ONRECE-K-REC
                   CALL "coblog" USING "abort insert tbl_henrei_body"
      *
                   MOVE    SPACE               TO  WRK-CSVERR
                   MOVE    MCP-RC              TO  WRK-MCP-RC
                   STRING  "返戻明細ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-PERFORM
      *
      *    返戻管理ファイル更新
           IF      FLG-PARA            =   1
               DISPLAY "HENREI-K-TEISYUTUSAKI ="
                                           WRK-HENREI-K-TEISYUTUSAKI
                       " TEISYUTUSAKI2="   WRK-HENREI-K-TEISYUTUSAKI2
                       " SYORIYM="         WRK-HENREI-K-SYORIYM
                       " HOSPCD="          WRK-HENREI-K-HOSPCD
                       " RECV-TOTAL-CNT="  INS-B-CNT
                       " RECV-PATIENT-CNT="  PATIENT-CNT
      *
               MOVE    WRK-HENREI-K-REC    TO  HENREI-K-REC
               MOVE    INS-B-CNT           TO  HENREI-K-RECV-TOTAL-CNT
               MOVE    PATIENT-CNT         TO  HENREI-K-RECV-PATIENT-CNT
               MOVE    WRK-PARA-HOSPNUM    TO  HENREI-K-HOSPNUM
               MOVE    SMCNDATE-YMD        TO  HENREI-K-CREYMD
               MOVE    HENREI-K-REC        TO  MCPDATA-REC
      *
               MOVE   "tbl_henrei_kanri"   TO  MCP-TABLE
               MOVE   "key"                TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   ADD     1                   TO  INS-K-CNT
               ELSE 
                   CALL "coblog" USING "abort insert tbl_henrei_kanri"
      *
                   MOVE    SPACE               TO  WRK-CSVERR
                   MOVE    MCP-RC              TO  WRK-MCP-RC
                   STRING  "返戻管理ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
           END-IF
           .
       20031-DB-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       2009-DB-DELETE-SEC                  SECTION.
      *
           INITIALIZE                          HENREI-B-REC
           MOVE    WRK-PARA-HOSPNUM        TO  HENREI-B-HOSPNUM
           MOVE    WRK-HENREI-K-TEISYUTUSAKI
                                           TO  HENREI-B-TEISYUTUSAKI
           MOVE    WRK-HENREI-K-TEISYUTUSAKI2
                                           TO  HENREI-B-TEISYUTUSAKI2
           MOVE    WRK-HENREI-K-SYORIYM    TO  HENREI-B-SYORIYM
           MOVE    WRK-HENREI-K-HOSPCD     TO  HENREI-B-HOSPCD
           MOVE    HENREI-B-REC            TO  MCPDATA-REC
      *
           MOVE   "tbl_henrei_body"        TO  MCP-TABLE
           MOVE   "del1"                   TO  MCP-PATHNAME
           MOVE    "DBDELETE"              TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    SPACE                   TO  WRK-CSVERR
           MOVE    "全ての患者情報の取得に失敗しました"
                                           TO    WRK-CSVERR
           PERFORM 500-ERR-HENSYU-SEC
           PERFORM 500-COBABORT-SEC
           .
       2009-DB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   CSVERR-FILE
           IF      STS-CSVERR         =   ZERO
               CLOSE   CSVERR-FILE
           ELSE
               OPEN    OUTPUT              CSVERR-FILE
      *
               MOVE    WRK-CSVERR         TO  CSVERR-REC
               WRITE   CSVERR-REC
               CLOSE   CSVERR-FILE
      *         
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CSVERR      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           CALL    "coblog"            USING   WRK-CSVERR
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー時終了処理
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM550 "         DELIMITED  BY  SIZE
                   WRK-CSVERR          DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号構成情報取得処理
      *****************************************************************
       500-PTID-HENSYU-SEC            SECTION.
      *
      *    患者番号構成情報取得
           MOVE    ZERO               TO  FLG-SET
      *
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       TBL-SYS1009-MAX
               MOVE    TBL-SYS1009 (IDZ)   TO  SYS-1009-REC
               IF    ( SYS-1009-STYUKYMD (1:6)
                                       <=  WRK-PTID-SRYYM          )
               AND   ( WRK-PTID-SRYYM  <=  SYS-1009-EDYUKYMD (1:6) )
                   MOVE     1                   TO  FLG-SET
                   MOVE    TBL-SYS1009-MAX      TO  IDZ
               END-IF
           END-PERFORM
           IF     FLG-SET              =   ZERO
               INITIALIZE                  SYS-1009-REC
               CALL "coblog" USING "患者番号構成情報が取得できません"
           END-IF
      *
           MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  SPA-1009-PTNUMKSIKBN
           MOVE    SYS-1009-HJNKSIKBN  TO  SPA-1009-HJNKSIKBN
           MOVE    SYS-1009-HJNKSINENKBN
                                       TO  SPA-1009-HJNKSINENKBN
           MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  SPA-1009-HJNKSIRENNUMKBN
           MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  SPA-1009-HJNKSIRENNUMKETA
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    WRK-PTNUM           TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           IF      SPTNUM-RC           =   00
               MOVE    1                   TO  FLG-PTID-ERR
               DISPLAY "PTNUM=" WRK-PTNUM
           ELSE
               IF      FLG-PTID-ERR        =   ZERO
                   MOVE    9                   TO  FLG-PTID-ERR
               END-IF
               DISPLAY "PTNUM=" WRK-PTNUM " PTID NOT FOUND!!"
           END-IF
           .
       500-PTID-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーコード編集処理
      *****************************************************************
       500-ERRCD-HENSYU-SEC            SECTION.
      *
           IF      TBL-HENREI-B-ERRCD (IDZZ)
                                       =   SPACE
               MOVE    WRK-ERRCD           TO  TBL-HENREI-B-ERRCD (IDZZ)
           END-IF
           .
       500-ERRCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "*** ORCBM550 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    取り込みデータファイル読み込み処理
      *****************************************************************
       900-PARA-READ-SEC           SECTION.
      *
           MOVE    SPACE            TO  PARA-REC
      *
           READ    PARA-FILE
               AT  END
                   MOVE    1            TO  FLG-PARA
               NOT AT  END
                   IF      STS-PARA        NOT =   ZERO
                       CALL "coblog" USING   "file read err "
                                                   WRK-FILE-TMP
                       STRING  "ファイル読み込みエラー FILE="
                                                   DELIMITED  BY  SIZE
                           WRK-FILE-TMP            DELIMITED  BY  SPACE
                           " STS="                 DELIMITED  BY  SIZE
                           STS-PARA                DELIMITED  BY  SIZE
                                                   INTO    WRK-CSVERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1            TO  READ-CNT
                   MOVE    ZERO         TO  FLG-PARA
      *
           END-READ
          .
       900-PARA-READ-EXT.
          EXIT.
      *
      *****************************************************************
      *    管理ファイルＳＥＬＥＣＴ処理
      *****************************************************************
       900-ONRECE-KANRIANRI-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
           MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-KANRI-FETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-ONRECE-K
               INITIALIZE                      ONRECE-K-REC
           END-IF
           .
       900-ONRECE-KANRIANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルFETCH処理
      *****************************************************************
       900-ONRECE-KANRI-FETCH-SEC      SECTION.
      *
           MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-ONRECE-K
               MOVE   MCPDATA-REC          TO  ONRECE-K-REC
           ELSE
               INITIALIZE                      ONRECE-K-REC
               MOVE   1                    TO  FLG-ONRECE-K
           END-IF
           .
       900-ONRECE-KANRI-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルUPDATE処理
      *****************************************************************
       900-ONRECE-KANRI-UPDATE-SEC     SECTION.
      *??
           DISPLAY "K UPDATE=" ONRECE-K-UCNT "#" 
                               ONRECE-K-SCNT "#"
                               ONRECE-K-RENNUM "#" 
      *??
      *
           MOVE    SMCNDATE-YMD        TO  ONRECE-K-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONRECE-K-UPHMS
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
      *
           MOVE    "upd1"              TO  MCP-PATHNAME
           MOVE   "tbl_onrece_kanri"   TO  MCP-TABLE
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               CALL "coblog" USING "abort update"
      *
               MOVE    SPACE               TO  WRK-CSVERR
               MOVE    MCP-RC              TO  WRK-MCP-RC
               STRING  "管理ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-ONRECE-KANRI-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルINSERT処理
      *****************************************************************
       900-ONRECE-KANRI-INSERT-SEC     SECTION.
      *??
           DISPLAY "K INSERT=" ONRECE-K-UCNT "#" 
                               ONRECE-K-SCNT "#"
                               ONRECE-K-RENNUM "#" 
      *??
      *
           MOVE    SMCNDATE-YMD        TO  ONRECE-K-CREYMD
      *
           MOVE    ONRECE-K-REC        TO  MCPDATA-REC
      *
           MOVE   "tbl_onrece_kanri"   TO  MCP-TABLE
           MOVE   "key"                TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
                   INITIALIZE          ONRECE-K-REC
                   CALL "coblog" USING "abort insert"
      *
               MOVE    SPACE               TO  WRK-CSVERR
               MOVE    MCP-RC              TO  WRK-MCP-RC
               STRING  "管理ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-ONRECE-KANRI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    BODYファイルSELECT　処理
      *****************************************************************
       900-ONRECE-B-SELECT-SEC     SECTION.
      *
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_onrece_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-ONRECE-B-FETCH-SEC
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-B-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    BODYファイルFETCH処理
      *****************************************************************
       900-ONRECE-B-FETCH-SEC      SECTION.
      *
           MOVE   "tbl_onrece_body"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  ONRECE-B-REC
               MOVE  ZERO                  TO  FLG-ONRECE-B
           ELSE
               INITIALIZE                      ONRECE-B-REC
               MOVE  1                     TO  FLG-ONRECE-B
           END-IF
           .
       900-ONRECE-B-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    BODYファイルINSERT　処理
      *****************************************************************
       900-ONRECE-B-INSERT-SEC     SECTION.
      *??
           DISPLAY "B INSERT=" ONRECE-B-UCNT "#" 
                               ONRECE-B-SCNT "#"
                               ONRECE-B-RENNUM "#" 
                               ONRECE-B-RECEIPT-NO "#" 
      *??
      *
           MOVE    SMCNDATE-YMD        TO  ONRECE-B-CREYMD
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           MOVE   "tbl_onrece_body"    TO  MCP-TABLE
           MOVE   "key"                TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               CALL "coblog" USING "abort insert"
      *
               MOVE    SPACE               TO  WRK-CSVERR
               MOVE    MCP-RC              TO  WRK-MCP-RC
               STRING  "明細ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-ONRECE-B-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    BODYファイルUPDATE　処理
      *****************************************************************
       900-ONRECE-B-UPDATE-SEC     SECTION.
      *
      *??
           DISPLAY "B UPDATE=" ONRECE-B-UCNT "#" 
                               ONRECE-B-SCNT "#"
                               ONRECE-B-RENNUM "#" 
                               ONRECE-B-RECEIPT-NO "#" 
      *??
           MOVE    SMCNDATE-YMD        TO  ONRECE-B-UPYMD
           MOVE    SMCNDATE-HMS        TO  ONRECE-B-UPHMS
           MOVE    ONRECE-B-REC        TO  MCPDATA-REC
      *
           MOVE   "tbl_onrece_body"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               CALL    "coblog" USING "abort update"
      *
               MOVE    SPACE               TO  WRK-CSVERR
               MOVE    MCP-RC              TO  WRK-MCP-RC
               STRING  "明細ＤＢ更新エラー MCP-RC="
                                               DELIMITED  BY  SIZE
                           WRK-MCP-RC          DELIMITED  BY  SIZE
                                               INTO    WRK-CSVERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       900-ONRECE-B-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理ファイルＣＬＯＳＥ処理
      *****************************************************************
       900-ONRECE-KANRI-CLOSE-SEC      SECTION.
      *
           MOVE    "tbl_onrece_kanri"  TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ONRECE-KANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細ファイルＣＬＯＳＥ処理
      *****************************************************************
       900-ONRECE-B-CLOSE-SEC      SECTION.
      *
           MOVE   "tbl_onrece_body"    TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-ONRECE-B-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタＲＥＡＤ
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
      ****     CALL    "cobabort"          USING   WRK-ERRMSG
               
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
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    ＤＢクローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
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
