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
       PROGRAM-ID.             ORCR1200.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 公害レセプト（入院）
      *                      基本情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.04.01    NACL-藤原    09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *
      *  04.05.01    NACL-藤原    11/11/18  他保険での同日再入院時の保険
      *                                     履歴対応
      *  04.05.02    NACL-藤原    11/11/22  同日再入院対応
      *
      *  04.07.01    NACL-藤原    14/02/07  入院会計の保険組合せが全て
      *                                     公害以外のときの一括処理の修正
      *  04.07.02    NACL-門脇    14/03/28  平成２６年４月改正対応
      *  04.07.03    NACL-藤原    15/01/24  同日再入院時の入退院履歴の記録
      *                                     を修正
      *                                    （入院日と退院日が同じとき）
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    17/03/31  同日再入院の入退院履歴のチェック
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    基本情報ファイル
           SELECT  RECE200-FILE    ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
      *    入退院履歴情報ファイル
           SELECT  RECE250-FILE     ASSIGN  RECE250PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE250-KEY
                                   FILE    STATUS  IS  STS-RECE250.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC. 
           COPY    "CPRCF200.INC".
      *
       FD  RECE250-FILE.
       01  RECE250-REC. 
           COPY    "CPRCF250.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
      *     入退院履歴情報ファイル 名称領域
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE250//.
      *
      *    エラーファイル名称領域
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名称領域
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE200         PIC X(02).
           03  STS-RECE250         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-RECE200         PIC 9(01).
           03  FLG-RECE250         PIC 9(01). 
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-PTNYUINRRK12    PIC 9(01).
           03  FLG-PTNYUINRRK22    PIC 9(01).
           03  FLG-PTNYUINRRK23    PIC 9(01).
      *
           03  FLG-SET             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN-CANCEL       PIC 9(06).
           03  CNT-RECE200         PIC 9(06).
           03  CNT-RECE250         PIC 9(06). 
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDX1                PIC 9(03).
           03  IDX2                PIC 9(03).
           03  IDX3                PIC 9(03). 
           03  IDX4                PIC 9(02).
           03  IDX5                PIC 9(02).
           03  IDY1                PIC 9(03).
           03  IDY2                PIC 9(03).
           03  IDW                 PIC 9(02).
           03  IDZ                 PIC 9(02).
           03  IDXX                PIC 9(03).
           03  TBL-MAX             PIC 9(03).
           03  SYSKAN-IDX          PIC 9(03).
      *
       01  KEY-AREA                    VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-N-PTID      PIC 9(10).
               05  KEY-N-HKNCOMBI  PIC 9(04).
               05  KEY-N-SRYKA     PIC X(02).
           03  KEY-OLD.
               05  KEY-O-PTID      PIC 9(10).
               05  KEY-O-HKNCOMBI  PIC 9(04).
               05  KEY-O-SRYKA     PIC X(02).
      *
       01  KEY-AREA1                   VALUE   LOW-VALUE.
           03  KEY-NEW1.
               05  KEY-N1-PTID     PIC 9(10).
               05  KEY-N1-HKNCOMBI PIC 9(04).
               05  KEY-N1-SRYKA    PIC X(02).
           03  KEY-OLD1.
               05  KEY-O1-PTID     PIC 9(10).
               05  KEY-O1-HKNCOMBI PIC 9(04).
               05  KEY-O1-SRYKA    PIC X(02).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM      PIC X(06).
               05  WRK-PARA-TERMID     PIC X(16).
               05  WRK-PARA-SYSYMD     PIC X(08).
               05  WRK-PARA-SYOKBN     PIC X(01).
               05  WRK-PARA-RECEKBN    PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-PTID       PIC X(10).
               05  WRK-PARA-SRYYM-O    PIC X(06).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
           03  WRK-PARA-PRTKBN         PIC X(01).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-NYUINACCT-MCP-TABLE PIC X(64).
           03  WRK-NYUINACCT-MCP-PATHNAME
                                       PIC X(64).
           03  WRK-SRYACCT-MCP-TABLE   PIC X(64).
           03  WRK-SRYACCT-MCP-PATHNAME
                                       PIC X(64).
      *
           03  WRK-PRTSEQ              PIC 9(03).
      *
           03  WRK-STYMD               PIC X(08).
           03  WRK-EDYMD               PIC X(08).
      *
           03  WRK-ERR-AREA.
               05  WRK-UPDKBN          PIC X(01).
               05  WRK-TEISEIKBN       PIC X(01).
               05  WRK-ROUSAIKBN       PIC X(01).
               05  WRK-ERR-DAY         PIC 9(02).
               05  WRK-ERR-HKNCOMBINUM PIC 9(04).
               05  WRK-ERR-ERRKBN      PIC 9(01).
               05  WRK-ERR-TEKSTYMD    PIC 9(08).
               05  WRK-ERR-TEKEDYMD    PIC 9(08).
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
           03  WRK-SRYYMD.
               05  WRK-SRYYYMM                         PIC  9(06).
               05  WRK-SRYDD                           PIC  9(02).
           03  WK-PTNYUINRRK-TENSTUYMD                 PIC  X(08).
           03  WK-PTNYUINRRK-TENSTUYMD2                PIC  X(08).
           03  WK-PTNYUINRRK-TENNYUYMD                 PIC  X(08).
      *
           03  WRK-SYUKA-SRYYM         PIC X(06).
           03  WRK-SYUKA-PTID          PIC 9(10).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-DBKBN               PIC X(01).
      *              
      *    診療回数テーブル（入院会計用 １：基本 ２：組合せ）
           03  WRK-DAY-AREA.
               05  WRK-DAY-TBL         OCCURS  2.
                   07  WRK-DAY         PIC 9(03)
                                           OCCURS   31.
      *
       01  WRK-ERRMSG-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
           03  WRK-MCP-RC          PIC 9(9).
           03  WRK-ERR-FILE-STS    PIC X(02).
           03  WRK-ERR-FILE-MSG    PIC X(100).
           03  WRK-ERR-FILE-MSG1   PIC X(17).
           03  WRK-ERR-FLG         PIC 9(01).
      *    エラーファイル名称領域
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    病棟区分テーブル                                             
       01  WRK-BYOTOKBN-TABLE.                                          
           03  WRK-BYOTOKBN-T            OCCURS  4.                     
               05  WRK-BYOTOKBN          PIC X(02).                     
      *                                                                 
       01  WRK-BYOTOKBN-DATA             PIC X(02).                     
      *    初回番号(最新の入院歴分)                                                                 
       01  WRK-SHONUM                    PIC 9(03).                     
      *
      *    ワーク入退院履歴
       01  WRK-RECE250-REC. 
           COPY    "CPRCF250.INC"  REPLACING  //RECE250//
                                   BY         //WRK-RECE250//.
      *
      *    ワーク入院履歴
       01  WRK-PTNRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //WRK-PTNRRK-//.
      *     
      *    ワーク保険組合せ（入院会計用） 
       01  WRK-HKNCOMBI-TABLE.
           02  WRK-HKNCOMBI-REC        OCCURS  10. 
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //WRK-COMB-//.
           03  WRK-HKNCOMBI-SYOKBN     PIC X(01).   
           03  WRK-HKNCOMBI-ERR-DAY    PIC 9(02).   
      *
       01  WRK-RECE200-REC.
           COPY    "CPRCF200.INC"  REPLACING   //RECE200//
                                   BY          //WRK-RECE200//.
      *
       01  TABLE-RECE200-AREA.
           02  TBL-RECE200-OCC     OCCURS  100.
           COPY    "CPRCF200.INC"  REPLACING   //RECE200//
                                   BY          //TBL-RECE200//.
       01  TBL-RECE200-MAX         PIC 9(03)   VALUE   200.
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  100.
               05  TBL-SYS1001     PIC X(610).
      *
       01  WRK-CONS-AREA.
      *    公害保険番号 
           03  WRK-CONS-KOUGAI-HKNNUM
                                   PIC X(03)   VALUE   "975". 
      *
           COPY    "MCPAREA".
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
      *    医療機関情報
           COPY  "CPSK1001.INC".
      *
      *    患者番号構成管理情報
           COPY    "CPSK1009.INC".
      *    レセプト作成指示コード
           COPY    "CPSK2001.INC".
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
      *    病棟情報                                                     
           COPY    "CPSK5001.INC".                                      
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    旧姓履歴情報
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    保険算定用年齢・割合計算サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    入院料未算定日取得サブ
           COPY    "CPORCSNYUINDAY.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(400).
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
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
                                       TABLE-RECE200-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM=" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM  =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN =" WRK-PARA-PRTKBN
      *
           MOVE    WRK-PARA-SRYYM      TO  SYS-2001-SRYYM
           MOVE    WRK-PARA-SYOKBN     TO  SYS-2001-SYOKBN 
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE700"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE200PARA
      *
           MOVE    "RECE750"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE250PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ＤＢオープン処理
               PERFORM 100-DBOPEN-SEC
      *
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1200"          TO  JOB-PGID
               MOVE    "対象患者抽出"      TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               IF   WRK-PARA-SRYYM    NOT =  ZERO
      *            医療機関ＩＤ編集
                   INITIALIZE                  SYS-1001-REC
                   MOVE    "1001"              TO  SYS-1001-KANRICD
                   MOVE    "*"                 TO  SYS-1001-KBNCD
                   MOVE    WRK-PARA-SRYYM      TO 
                                               SYS-1001-STYUKYMD (1:6)
                   MOVE    "01"                TO
                                               SYS-1001-STYUKYMD (7:2)
                   MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD 
                   MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC        TO  MCPDATA-REC
                   PERFORM 910-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    MCPDATA-REC         TO  SYS-1001-REC
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               ELSE
                   MOVE     ZERO               TO  SYSKAN-IDX
                   MOVE     SPACE              TO  TBL-SYS1001-T
                   INITIALIZE                      TBL-SYS1001-T
      *
                   INITIALIZE                      SYS-1001-REC
                   MOVE    "1001"              TO  SYS-1001-KANRICD
                   MOVE    "*"                 TO  SYS-1001-KBNCD
                   MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
                   MOVE    SYS-1001-REC        TO  MCPDATA-REC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                       PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                                 OR    (SYSKAN-IDX   >=  100)
      *
                           MOVE    MCPDATA-REC   TO  SYS-1001-REC
                           ADD     1             TO  SYSKAN-IDX
                           MOVE    SYS-1001-REC  TO
                                                 TBL-SYS1001(SYSKAN-IDX)
      *
                           MOVE    "tbl_syskanri"      TO  MCP-TABLE
                           MOVE    "key8"              TO  MCP-PATHNAME
                           PERFORM 900-SYSKANRI-READ-SEC
                       END-PERFORM
                   ELSE
                       MOVE    "医療機関情報が取得できませんでした。"
                                                 TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
               END-IF
      *
      *        患者番号構成管理情報編集
               INITIALIZE                  SYS-1009-REC
               MOVE    "1009"              TO  SYS-1009-KANRICD
               MOVE    "*"                 TO  SYS-1009-KBNCD
               MOVE    WRK-PARA-SRYYM      TO  SYS-1009-STYUKYMD  (1:6)
               MOVE    "01"                TO  SYS-1009-STYUKYMD  (7:2)
               MOVE    SYS-1009-STYUKYMD   TO  SYS-1009-EDYUKYMD  
               MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
               MOVE    SYS-1009-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1009-REC
                   MOVE    SYS-1009-PTNUMKSIKBN
                                           TO  SPA-1009-PTNUMKSIKBN
                   MOVE    SYS-1009-HJNKSIKBN  
                                           TO  SPA-1009-HJNKSIKBN
                   MOVE    SYS-1009-HJNKSINENKBN 
                                           TO  SPA-1009-HJNKSINENKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKBN
                                           TO  SPA-1009-HJNKSIRENNUMKBN
                   MOVE    SYS-1009-HJNKSIRENNUMKETA
                                           TO  SPA-1009-HJNKSIRENNUMKETA
               ELSE
                   MOVE    "患者番号構成管理情報が取得できませんでした"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
      *
               IF        WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            パラメタＤＢ読み込み
                   INITIALIZE                  BTPARA-REC
                   MOVE    WRK-PARA-SHELLID
                                           TO  BTPARA-SHELLID
                   MOVE    WRK-PARA-JOBID  TO  BTPARA-JOBID
                   PERFORM 900-BTPARA-READ-SEC
                   IF      FLG-BTPARA      NOT =   ZERO
                       MOVE    "パラメタ情報が取得できませんでした。"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF
               END-IF
           END-IF
      *
      *    ファイルＯＰＥＮ処理
           OPEN    OUTPUT              RECE200-FILE
           CLOSE                       RECE200-FILE
           OPEN    OUTPUT              RECE250-FILE
           CLOSE                       RECE250-FILE
      *
           OPEN    I-O                 RECE200-FILE  
           OPEN    I-O                 RECE250-FILE
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   PERFORM 2001-IKKATU-SYORI-SEC
               WHEN    "2"
                   PERFORM 2002-KOBETU-SYORI-SEC
           END-EVALUATE
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    レセプト一括作成処理
      *****************************************************************
       2001-IKKATU-SYORI-SEC               SECTION.
      *
      *    処理中止設定処理
           PERFORM 500-CANCEL-HENSYU-SEC
      *
      *    診療年月編集
           MOVE    WRK-PARA-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    WRK-PARA-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
           IF      FLG-END             =   ZERO 
      *        入院会計読み込み
               MOVE    "2"                 TO  WRK-DBKBN 
      *
               MOVE    LOW-VALUE           TO  KEY-AREA
      *     
               INITIALIZE                  NYUINACCT-REC
               MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
               MOVE    SYS-2001-SRYYM      TO  NACCT-SRYYM
               MOVE    "1"                 TO  NACCT-NYUGAIKBN
               MOVE    NYUINACCT-REC       TO  MCPDATA-REC
               MOVE    "tbl_nyuinacct"     TO  WRK-NYUINACCT-MCP-TABLE
               MOVE    "key13"             TO
                                           WRK-NYUINACCT-MCP-PATHNAME
      *
               MOVE    WRK-NYUINACCT-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    WRK-NYUINACCT-MCP-TABLE
                                               TO  MCP-TABLE
                   MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               ELSE
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    1                   TO  FLG-NYUINACCT
               END-IF
      *
               PERFORM UNTIL       FLG-NYUINACCT   =   1
                       OR          FLG-END         =   1
                   PERFORM 20012-IKKATU-HENSYU-SEC
               END-PERFORM
      *
               MOVE    WRK-NYUINACCT-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2001-IKKATU-SYORI-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    一括編集処理
      *****************************************************************
       20012-IKKATU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  TABLE-RECE200-AREA
      *
      *    患者情報取得
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID      TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
      *****MOVE    SYS-2001-SRYYM  TO  WRK-SYUKA-SRYYM
      *****MOVE    KEY-N-PTID      TO  WRK-SYUKA-PTID
      *****PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "NYUINACCT PTID=" KEY-N-PTID 
                   " TSTPTNUMKBN=" PTINF-TSTPTNUMKBN
      *****        " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    テスト患者は読み飛ばす
           IF   ( PTINF-TSTPTNUMKBN    =   "1"  )
      *****OR   ( SYUKA-MISEIKYU-FLG   =   "01" )
               MOVE    KEY-NEW             TO  KEY-OLD
               PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                               OR      FLG-END         =   1
                               OR      KEY-N-PTID  NOT =   KEY-O-PTID
                   MOVE    WRK-NYUINACCT-MCP-TABLE
                                               TO  MCP-TABLE
                   MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-NYUINACCT-READ-SEC
               END-PERFORM
           ELSE
      *        同日再入院情報取得
               PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *       入院会計編集処理
               PERFORM 200121-NYUINACCT-HENSYU-SEC
      *
      *        診療会計処理
               IF    ( FLG-END             =   ZERO ) 
      *********AND   ( TBL-RECE200-HOSPNUM (1)
      *********                            >   ZERO )
                   MOVE    "1"                 TO  WRK-DBKBN
      *
                   INITIALIZE                      SRYACCT-REC
                   MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    KEY-O-PTID          TO  ACCT-PTID
                   MOVE    SYS-2001-SRYYM      TO  ACCT-SRYYM
                   MOVE    "1"                 TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO
                                               WRK-SRYACCT-MCP-TABLE
                   IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                       MOVE    "key61"         TO
                                               WRK-SRYACCT-MCP-PATHNAME
                   ELSE
                       MOVE    "key37"         TO
                                               WRK-SRYACCT-MCP-PATHNAME
                   END-IF
                   PERFORM 200122-SRYACCT-HENSYU-SEC
      *
      *            基本ファイル出力処理
                   PERFORM 200123-KIHON-WRITE-SEC
               END-IF
           END-IF
           .
       20012-IKKATU-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計編集処理
      *****************************************************************
       200121-NYUINACCT-HENSYU-SEC              SECTION.
      *     
           MOVE    KEY-NEW             TO  KEY-OLD
           INITIALIZE                      WRK-DAY-AREA
                                           WRK-HKNCOMBI-TABLE
      *
           PERFORM         UNTIL   FLG-NYUINACCT   =   1 
                           OR      FLG-END         =   1      
                           OR      KEY-NEW     NOT =   KEY-OLD
      *        剤識別区分別テーブル編集
               EVALUATE    NACCT-ZAISKBKBN
                   WHEN    "1"
                       MOVE    1               TO  IDX
                   WHEN    "5"
                       MOVE    2               TO  IDX
               END-EVALUATE
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL   IDX1  >        31
                   IF      NACCT-DAY  (IDX1)       >   ZERO
                       MOVE    NACCT-DAY (IDX1)    TO
                                               WRK-DAY (IDX IDX1)
                   END-IF
               END-PERFORM            
               MOVE    WRK-NYUINACCT-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           END-PERFORM
      *
           IF      FLG-END             =   ZERO
      *        同日再入院のチェック
               PERFORM 500-ORCSNYUINDAY-CHECK-SEC
      *
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       31
                   IF    ((WRK-DAY (1 IDX)        >   ZERO) OR
                          (LNK-NYUINDAY-KBN(IDX)  =   "1" ))  AND
                         ( WRK-DAY (2 IDX)        >   ZERO )
      *                入院会計チェック処理
                       PERFORM 2001211-NYUINACCT-CHK-SEC
                   END-IF
               END-PERFORM        
      *
      *        入院会計から保険組合せを追加
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL   IDX2    >       10
                       OR      WRK-COMB-HKNCOMBINUM (IDX2) =   ZERO
                   IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "1"
                       CONTINUE
                   ELSE    
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    WRK-HKNCOMBI-REC (IDX2)
                                                   TO  HKNCOMBI-REC    
                       IF      WRK-HKNCOMBI-SYOKBN (IDX2)  =   "2"
                           MOVE    "1"         TO  WRK-TEISEIKBN
                           IF      WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                       =   ZERO
                               MOVE    1           TO  WRK-ERR-ERRKBN
                           ELSE
                               MOVE    WRK-HKNCOMBI-ERR-DAY (IDX2)
                                                   TO  WRK-ERR-DAY
                               MOVE    6           TO  WRK-ERR-ERRKBN
                           END-IF
                           MOVE    WRK-COMB-HKNCOMBINUM (IDX2)  TO
                                               WRK-ERR-HKNCOMBINUM
                       END-IF        
      *                基本ファイル編集処理
                       PERFORM 2001212-KIHON-HENSYU-SEC
                   END-IF 
               END-PERFORM
           END-IF    
           .
       200121-NYUINACCT-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       2001211-NYUINACCT-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >       10
      *
      *        保険組合せ追加
               IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                   INITIALIZE                  HKNCOMBI-REC
                   MOVE    SYS-1001-HOSPNUM TO  COMB-HOSPNUM
                   MOVE    KEY-O-PTID      TO  COMB-PTID
                   MOVE    WRK-DAY (2 IDX) TO
                                           COMB-HKNCOMBINUM 
                   MOVE    HKNCOMBI-REC    TO  MCPDATA-REC
                   PERFORM 910-HKNCOMBI-INV-SEC
      *            （対象外の組合せは１をセット）
                   IF      FLG-HKNCOMBI    =   ZERO
                       MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
      *                医保以外読み飛ばし
                       IF      COMB-HKNNUM     =
                                           WRK-CONS-KOUGAI-HKNNUM
                           CONTINUE
                       ELSE
                           MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                       END-IF
                   ELSE
                       IF      COMB-DELKBN         =   "1"
                           MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
      *                    公害以外読み飛ばし
                           IF      COMB-HKNNUM     =
                                               WRK-CONS-KOUGAI-HKNNUM
                               MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                           ELSE
                               MOVE    "1"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       ELSE
                           MOVE    WRK-DAY (2 IDX) TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                           MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                       END-IF
                   END-IF
               END-IF
      *
      *        適用範囲かチェック（範囲外のとき２をセット）
               IF      WRK-DAY (2 IDX)     =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                   IF      WRK-HKNCOMBI-SYOKBN (IDX1)   NOT =   SPACE
      *                公害以外のとき初期化する
      ****             IF      WRK-HKNCOMBI-SYOKBN (IDX1)  =   "1"
      ****                 MOVE    ZERO            TO  WRK-DAY (2 IDX)
      ****             END-IF
                       CONTINUE  
                   ELSE     
                       MOVE    WRK-STYMD       TO  WRK-YMD
                       MOVE    IDX             TO  WRK-DD
                       IF      WRK-COMB-TEKSTYMD (IDX1)
                                               <=  WRK-YMD
                       AND     WRK-COMB-TEKEDYMD (IDX1)
                                               >=  WRK-YMD
                           CONTINUE
                       ELSE   
                           MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           MOVE    IDX         TO
                                           WRK-HKNCOMBI-ERR-DAY (IDX1)
                       END-IF
                   END-IF
                   MOVE    10              TO  IDX1
               END-IF            
           END-PERFORM
           .
       2001211-NYUINACCT-CHK-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    基本ファイル編集処理
      *****************************************************************
       2001212-KIHON-HENSYU-SEC           SECTION.
      *     
           INITIALIZE                      WRK-RECE200-REC
           MOVE    SPA-HOSPNUM         TO  WRK-RECE200-HOSPNUM
           MOVE    WRK-STYMD (1:6)     TO  WRK-RECE200-SRYYM
           MOVE    "1"                 TO  WRK-RECE200-NYUGAIKBN
           MOVE    COMB-PTID           TO  WRK-RECE200-PTID
           MOVE    SPACE               TO  WRK-RECE200-RECEKA
           MOVE    COMB-HKNID          TO  WRK-RECE200-HKNID
           MOVE    COMB-TEKSTYMD       TO  WRK-RECE200-TEKSTYMD
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       TBL-RECE200-MAX
               IF      TBL-RECE200-HOSPNUM (IDXX) =   ZERO
                   PERFORM 20012121-RECE200-WRITE-SEC
                   MOVE    TBL-RECE200-MAX     TO  IDXX
               ELSE
                   IF      TBL-RECE200-KEY (IDXX) =
                                               WRK-RECE200-KEY
                       PERFORM 20012122-RECE200-REWRITE-SEC
                       MOVE    TBL-RECE200-MAX     TO  IDXX
                   END-IF
               END-IF
           END-PERFORM
           .
       2001212-KIHON-HENSYU-EXT.
           EXIT. 
      *            
      *****************************************************************
      *    基本ファイル編集（新規）処理
      *****************************************************************
       20012121-RECE200-WRITE-SEC           SECTION.
      *
           MOVE    COMB-HKNNUM         TO  WRK-RECE200-HKNNUM
           MOVE    COMB-HKNCOMBINUM    TO  WRK-RECE200-HKNCOMBI
           IF      WRK-DBKBN           =   "1" 
               MOVE    ACCT-SRYKA          TO  WRK-RECE200-SRYKA (1)
           ELSE
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                         UNTIL     IDX3    >       31
                   IF    ((WRK-DAY (1 IDX3)        >   ZERO) OR
                          (LNK-NYUINDAY-KBN(IDX3)  =   "1" ))  AND
                         ( WRK-DAY (2 IDX3)        >   ZERO )
                       MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE200-DAY(IDX3)
                   END-IF
               END-PERFORM
           END-IF    
      *    
      *    主保険情報
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           MOVE    COMB-TEKSTYMD       TO  PTHKN-TEKSTYMD
                                           PTHKN-TEKEDYMD
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           PERFORM 910-PTHKNINF-INV-SEC
           MOVE    PTHKN-KIGO          TO  WRK-RECE200-KIGO
           MOVE    PTHKN-NUM           TO  WRK-RECE200-NUM
      *
      *    患者情報
           MOVE   PTINF-KANANAME       TO  WRK-RECE200-KANANAME
           MOVE   PTINF-NAME           TO  WRK-RECE200-NAME
           MOVE   PTINF-SEX            TO  WRK-RECE200-SEX
           MOVE   PTINF-BIRTHDAY       TO  WRK-RECE200-BIRTHDAY
           MOVE   PTINF-DEATHKBN       TO  WRK-RECE200-DEATHKBN
           MOVE   PTINF-TSTPTNUMKBN    TO  WRK-RECE200-TSTPTNUMKBN
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WRK-RECE200-PTID    TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  WRK-RECE200-PTNUM
      *    年齢
           INITIALIZE                      ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    WRK-RECE200-PTID    TO  AGECHK-PTID
           MOVE    WRK-STYMD           TO  AGECHK-SRYYMD
           MOVE    WRK-RECE200-BIRTHDAY
                                       TO  AGECHK-BIRTHDAY
           CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA 
           IF      AGECHK-RC           =   ZERO
               MOVE  AGECHK-NENREI         TO  WRK-RECE200-AGE
           END-IF
      *    旧姓履歴
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    WRK-RECE200-PTID    TO  KYUSEI-PTID
           MOVE    WRK-EDYMD           TO  KYUSEI-CHGYMD
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           PERFORM 900-KYUSEIRRK-READ-SEC
           IF       FLG-KYUSEIRRK      =   ZERO
               MOVE    KYUSEI-NAME         TO  WRK-RECE200-NAME
               MOVE    KYUSEI-KANANAME     TO  WRK-RECE200-KANANAME
           END-IF
      *
      *    印刷順（個別作成時の個別指定のみ）
           IF    ( WRK-PARA-SYOKBN     =   "2"              )
           AND   ( BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                   OR  "05" )
               ADD     1                   TO  WRK-PRTSEQ
               MOVE    WRK-PRTSEQ          TO  WRK-RECE200-PRTSEQ
           END-IF
      *    未請求フラグ
      *****MOVE    SYUKA-MISEIKYU-FLG  TO  WRK-RECE200-MISEIKYU-FLG
      *    レセプト対象年月日情報
           MOVE    WRK-RECE200-TEKSTYMD
                                       TO  WRK-RECE200-TEKSTYMD-X
           MOVE    COMB-TEKEDYMD       TO  WRK-RECE200-TEKEDYMD-X
      *
      *    レセプト種別不明エラー情報
           MOVE    WRK-TEISEIKBN       TO  WRK-RECE200-TEISEIKBN
           IF      WRK-TEISEIKBN       =   "1"
               MOVE    WRK-ERR-ERRKBN      TO  WRK-RECE200-ERR-ERRKBN
               MOVE    WRK-ERR-DAY         TO  WRK-RECE200-ERR-DAY
               MOVE    WRK-ERR-HKNCOMBINUM 
                                           TO
                                           WRK-RECE200-ERR-HKNCOMBINUM
               MOVE    WRK-ERR-TEKSTYMD    TO  WRK-RECE200-ERR-TEKSTYMD
               MOVE    WRK-ERR-TEKEDYMD    TO  WRK-RECE200-ERR-TEKEDYMD
           END-IF 
      *
      *    病院・診療所区分
           MOVE    SYS-1001-HOSPSBT TO  WRK-RECE200-HOSPSBT
      *        
      *    入院料情報取得フラグ
           MOVE    1               TO  WRK-RECE200-DOUJITSU-FLG
      *    日再入院（入院分）の保険組合せ、食事開始時間区分
           PERFORM VARYING IDX3    FROM    1   BY  1
                   UNTIL   IDX3    >       31
               MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX3)
                                           TO
                                   WRK-RECE200-DOUJITSU-HKNCOMBI (IDX3)
               MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN   (IDX3)
                                           TO
                                   WRK-RECE200-DOUJITSU-SKJKBN   (IDX3)
           END-PERFORM
      *    入院区分
           MOVE    LNK-NYUINDAY-TBL    TO  WRK-RECE200-NYUINDAY-TBL
      *
           MOVE    WRK-RECE200-REC     TO  TBL-RECE200-OCC (IDXX)
      *
           .
       20012121-RECE200-WRITE-EXT.
           EXIT.
      *            
      *****************************************************************
      *    基本ファイル作成（更新）処理
      *****************************************************************
       20012122-RECE200-REWRITE-SEC            SECTION.
      *
           MOVE    TBL-RECE200-OCC (IDXX)  TO  WRK-RECE200-REC
      *
           IF      WRK-DBKBN           =   "1" 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       20
                   IF      WRK-RECE200-SRYKA (IDX) =   SPACE
                       MOVE    ACCT-SRYKA  TO  WRK-RECE200-SRYKA (IDX)
                       MOVE    20          TO  IDX
                   ELSE
                       IF      WRK-RECE200-SRYKA (IDX) =   ACCT-SRYKA
                           MOVE    20              TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           ELSE
               PERFORM   VARYING   IDX3    FROM    1   BY  1
                         UNTIL     IDX3    >       31
                   IF    ((WRK-DAY (1 IDX3)        >   ZERO) OR
                          (LNK-NYUINDAY-KBN(IDX3)  =   "1" ))  AND
                         ( WRK-DAY (2 IDX3)        >   ZERO )
                       MOVE   WRK-DAY(2 IDX3)  TO  WRK-RECE200-DAY(IDX3)
                   END-IF
               END-PERFORM
           END-IF    
      *        
           IF      WRK-RECE200-DOUJITSU-FLG =   ZERO
      *        入院料情報取得フラグ
               MOVE    1               TO  WRK-RECE200-DOUJITSU-FLG
      *        同日再入院（入院分）の保険組合せ、食事開始時間区分
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       31
                   MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDX3)
                                               TO
                                   WRK-RECE200-DOUJITSU-HKNCOMBI (IDX3)
                   MOVE    LNK-NYUINDAY-DOUJITSU-SKJKBN   (IDX3)
                                               TO
                                   WRK-RECE200-DOUJITSU-SKJKBN   (IDX3)
               END-PERFORM
      *        入院区分
               MOVE    LNK-NYUINDAY-TBL    TO  WRK-RECE200-NYUINDAY-TBL
           END-IF
      *
           IF      WRK-TEISEIKBN           =   "1"
               IF      WRK-RECE200-TEISEIKBN    =   SPACE
                   MOVE    WRK-ERR-ERRKBN  TO  WRK-RECE200-ERR-ERRKBN
                   MOVE    WRK-ERR-DAY     TO  WRK-RECE200-ERR-DAY
                   MOVE    WRK-ERR-HKNCOMBINUM TO 
                                           WRK-RECE200-ERR-HKNCOMBINUM
                   MOVE    WRK-ERR-TEKSTYMD
                                           TO  WRK-RECE200-ERR-TEKSTYMD
                   MOVE    WRK-ERR-TEKEDYMD
                                           TO  WRK-RECE200-ERR-TEKEDYMD
               END-IF            
               MOVE    "1"                 TO  WRK-RECE200-TEISEIKBN     
           END-IF          
      *
           MOVE    WRK-RECE200-REC     TO  TBL-RECE200-OCC (IDXX)
           .
       20012122-RECE200-REWRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本ファイル出力処理
      *****************************************************************
       200123-KIHON-WRITE-SEC              SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       TBL-RECE200-MAX
                   OR      TBL-RECE200-HOSPNUM (IDXX)  =   ZERO
               MOVE    TBL-RECE200-OCC (IDXX)      TO  WRK-RECE200-REC
      *
      *        入退院履歴より情報取得
               PERFORM 240-PTNYUINRRK-SYORI-SEC
               MOVE    WRK-RECE200-REC             TO  RECE200-REC
      *
      *        レセプト種別
               PERFORM 2001231-RECESYUBETU-SET-SEC
      *
               DISPLAY "RECE200 PTNUM=" RECE200-PTNUM
                       " RECESYUBETU="  RECE200-RECESYUBETU 
      *
               WRITE   RECE200-REC
      *
               IF      STS-RECE200         =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-ERRMSG-AREA
                   INITIALIZE                      WRK-ERRMSG-AREA
      *
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE200PARA         TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE200         TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
      *
               ADD     1                    TO  CNT-RECE200
           END-PERFORM
           .
       200123-KIHON-WRITE-EXT.
      *
      *****************************************************************
      *    レセプト種別セット処理
      *****************************************************************
       2001231-RECESYUBETU-SET-SEC         SECTION.
      *
           IF      RECE200-TEISEIKBN   =   "1"
               MOVE    ZERO                TO  RECE200-RECESYUBETU
           ELSE
               MOVE    7210                TO  RECE200-RECESYUBETU
           END-IF
           .
        2001231-RECESYUBETU-SET-EXT.
           EXIT.
      *            
      *****************************************************************
      *    診療会計編集処理
      *****************************************************************
       200122-SRYACCT-HENSYU-SEC              SECTION.
      *
           MOVE    WRK-SRYACCT-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-SRYACCT-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-SRYACCT-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-SRYACCT-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL       FLG-SRYACCT     =   1
                   OR          FLG-END         =   1 
               PERFORM 2001221-SRYACCT-CHEKCK-SEC
           END-PERFORM
      *
           MOVE    WRK-SRYACCT-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-SRYACCT-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       200122-SRYACCT-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       2001221-SRYACCT-CHEKCK-SEC                SECTION.
      *
           MOVE    KEY-NEW1            TO  KEY-OLD1
      *
           PERFORM             UNTIL   FLG-SRYACCT     =   1
                               OR      FLG-END         =   1
               MOVE    ACCT-HKNCOMBI       TO  KEY-N1-HKNCOMBI
               MOVE    ACCT-PTID           TO  KEY-N1-PTID 
               MOVE    ACCT-SRYKA          TO  KEY-N1-SRYKA
      *        保険組合せ読込み
               INITIALIZE                  HKNCOMBI-REC
               MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
               MOVE    ACCT-PTID           TO  COMB-PTID
               MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM 
               MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
               PERFORM  910-HKNCOMBI-INV-SEC
      *
      *        未訂正分があるか
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
      *
               IF      FLG-HKNCOMBI        =   ZERO
                   IF      COMB-HKNNUM     =  WRK-CONS-KOUGAI-HKNNUM
                       IF      COMB-TEKSTYMD   <=  WRK-STYMD
                       AND     COMB-TEKEDYMD   >=  WRK-EDYMD
                           MOVE    SPACE           TO  WRK-UPDKBN
                       ELSE
                           MOVE    "1"             TO  WRK-UPDKBN
                       END-IF
                   ELSE 
                       MOVE    "1"             TO  WRK-ROUSAIKBN
                   END-IF    
               ELSE
                   MOVE    "1"                 TO  WRK-TEISEIKBN        
                   MOVE    1                   TO  WRK-ERR-ERRKBN
                   MOVE    ACCT-HKNCOMBI       TO  WRK-ERR-HKNCOMBINUM        
               END-IF    
      *
               MOVE    KEY-NEW1            TO  KEY-OLD1
               PERFORM         UNTIL   FLG-SRYACCT     =   1 
                               OR      FLG-END         =   1      
                               OR      KEY-NEW1    NOT =   KEY-OLD1
                   IF      WRK-ROUSAIKBN       =   SPACE
                       PERFORM 20012212-SRYSCCT-FILE-SYORI-SEC
                   END-IF 
      *
                   IF      WRK-UPDKBN          =   SPACE                   
      *                同じ患者ＩＤ・診療科・保険組合せは読み飛ばす
                       MOVE    KEY-NEW1            TO  KEY-OLD1
                       PERFORM     UNTIL   FLG-SRYACCT     =   1 
                                   OR      FLG-END         =   1      
                                   OR      KEY-NEW1    NOT =   KEY-OLD1
                           MOVE    WRK-SRYACCT-MCP-TABLE
                                                       TO  MCP-TABLE
                           MOVE    WRK-SRYACCT-MCP-PATHNAME
                                                       TO  MCP-PATHNAME
                           PERFORM 900-SRYACCT-READ-SEC
                        END-PERFORM
                   ELSE
                       MOVE    WRK-SRYACCT-MCP-TABLE
                                                   TO  MCP-TABLE
                       MOVE    WRK-SRYACCT-MCP-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-SRYACCT-READ-SEC
                   END-IF
               END-PERFORM    
           END-PERFORM         
      *
           .
       2001221-SRYACCT-CHEKCK-EXT.
           EXIT.
      *            
      *****************************************************************
      *    診療会計ファイル編集処理
      *****************************************************************
       20012212-SRYSCCT-FILE-SYORI-SEC              SECTION.
      *                  
           IF      WRK-UPDKBN          =   "1"
      *        保険の訂正がされているか
               IF      COMB-TEKSTYMD(1:6)  =   WRK-STYMD (1:6)
                   MOVE    COMB-TEKSTYMD       TO  WRK-YMD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX             >=  WRK-DD
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    2               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N1-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKSTYMD(1:6)  >   WRK-STYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    3               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N1-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKSTYMD   TO  WRK-ERR-TEKSTYMD    
                   END-IF
               END-IF      
               IF      COMB-TEKEDYMD(1:6)  =   WRK-EDYMD (1:6)
                   MOVE    COMB-TEKEDYMD       TO  WRK-YMD
                   ADD     1                   TO  WRK-DD
                   PERFORM VARYING IDX FROM    WRK-DD  BY  1
                           UNTIL   IDX             >   31
                           OR      WRK-TEISEIKBN   =   "1"
                       IF      ACCT-DAY (1 IDX)    >   ZERO
                           MOVE    "1"             TO  WRK-TEISEIKBN
                           MOVE    IDX             TO  WRK-ERR-DAY
                           MOVE    4               TO  WRK-ERR-ERRKBN
                           MOVE    KEY-N1-HKNCOMBI  TO
                                                   WRK-ERR-HKNCOMBINUM
                           MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD
                       END-IF
                   END-PERFORM
               ELSE
                   IF      COMB-TEKEDYMD(1:6)  <   WRK-EDYMD (1:6)
                       MOVE    "1"             TO  WRK-TEISEIKBN
                       MOVE    5               TO  WRK-ERR-ERRKBN
                       MOVE    KEY-N1-HKNCOMBI  TO
                                               WRK-ERR-HKNCOMBINUM
                       MOVE    COMB-TEKEDYMD   TO  WRK-ERR-TEKEDYMD
                   END-IF    
               END-IF    
           END-IF
      *
      *    基本ファイル編集処理
           PERFORM 2001212-KIHON-HENSYU-SEC
           .
       20012212-SRYSCCT-FILE-SYORI-EXT.
           EXIT. 
      * 
      *****************************************************************
      *    個別処理
      *****************************************************************
       2002-KOBETU-SYORI-SEC               SECTION.
      *
           IF      WRK-PARA-SHELLID        =   "RECEPTX"
               PERFORM 20021-KOBETU-REAL-SEC
               MOVE    "1"   TO  PTINF-TSTPTNUMKBN
               GO  TO    2002-KOBETU-SYORI-EXT
           END-IF
      *
      *    レセプト指示（個別指示）コード読み込み
           INITIALIZE                      SYS-2002-REC
           MOVE    "2002"              TO  SYS-2002-KANRICD
           MOVE    ZERO                TO  SYS-2002-STYUKYMD
                                           SYS-2002-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2002-HOSPNUM
           MOVE    SYS-2002-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM 20021-KOBETU-HENSYU-SEC
                              UNTIL       FLG-SYSKANRI    =   1
                              OR          FLG-END         =   1
      *   
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
      *
       2002-KOBETU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-REAL-SEC              SECTION.
      *
           INITIALIZE                  TABLE-RECE200-AREA
      *
           MOVE    WRK-PARA-PTID    TO  SYS-2002-PTID
           MOVE    WRK-PARA-SRYYM-O TO  SYS-2002-SRYYM
           MOVE    "1"              TO  SYS-2002-NYUGAIKBN
      *
      **     MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
      **     MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
      **     PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      **             " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      *    該当診療年月の医療機関情報取得
           MOVE  ZERO           TO  FLG-SET
           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
           PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                    UNTIL  ( SYSKAN-IDX              >  100  )
                     OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                     OR    ( FLG-SET                 =   1   )
                MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                    MOVE     1       TO    FLG-SET
                END-IF
           END-PERFORM
           IF     FLG-SET              =   ZERO
               STRING   "医療機関情報が取得できませんでした。診療年月＝"
                                           DELIMITED  BY  SIZE
                        SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO       TO      20021-KOBETU-REAL-EXT
           END-IF
      *
      *    患者情報取得
           MOVE    SYS-1001-HOSPNUM    TO  PTINF-HOSPNUM
           MOVE    SYS-2002-PTID       TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    "1"                 TO  PTINF-TSTPTNUMKBN
      *
      *    診療年月編集
           MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD
      *
      *    同日再入院情報取得
           PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *    診療会計読み込み
           MOVE    "1"                 TO  WRK-DBKBN 
      *   
           INITIALIZE                  SRYACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  ACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
           MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  WRK-SRYACCT-MCP-TABLE
           IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
               MOVE    "key61"             TO  WRK-SRYACCT-MCP-PATHNAME
           ELSE
               MOVE    "key37"             TO  WRK-SRYACCT-MCP-PATHNAME
           END-IF
           PERFORM 200122-SRYACCT-HENSYU-SEC
      *
      *    入院会計読み込み
           MOVE    "2"                 TO  WRK-DBKBN 
      *
           MOVE    LOW-VALUE           TO  KEY-AREA
      *     
           INITIALIZE                  NYUINACCT-REC
           MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
           MOVE    SYS-2002-PTID       TO  NACCT-PTID
           MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  WRK-NYUINACCT-MCP-TABLE
           MOVE    "key14"             TO  WRK-NYUINACCT-MCP-PATHNAME
           MOVE    WRK-NYUINACCT-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-NYUINACCT-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF
      *
           PERFORM UNTIL       FLG-NYUINACCT     =   1
                   OR          FLG-END           =   1
               PERFORM 200121-NYUINACCT-HENSYU-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    WRK-NYUINACCT-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    基本ファイル出力処理
           PERFORM 200123-KIHON-WRITE-SEC
      *
           .
       20021-KOBETU-REAL-EXT.
           EXIT.
      *     
      *****************************************************************
      *    個別編集処理
      *****************************************************************
       20021-KOBETU-HENSYU-SEC              SECTION.
      *
           INITIALIZE                  TABLE-RECE200-AREA
      *
           MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
      ***  MOVE    SYS-2002-SRYYM      TO  WRK-SYUKA-SRYYM
      ***  MOVE    SYS-2002-PTID       TO  WRK-SYUKA-PTID
      ***  PERFORM 800-MISEIKYU-CHECK-SEC
      *
           DISPLAY "SYS PTID=" SYS-2002-PTID
                   " SRYYM="   SYS-2002-SRYYM 
      **           " MISEIKYU-FLG=" SYUKA-MISEIKYU-FLG
      *
      ***  未請求設定時は読み飛ばす
      ***  IF    SYUKA-MISEIKYU-FLG  =   "01"
      ***      CONTINUE
      ***  ELSE
      *
      *        医療機関ＩＤ編集
               MOVE  ZERO           TO  FLG-SET
               MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
               PERFORM  VARYING  SYSKAN-IDX  FROM  1  BY  1
                        UNTIL  ( SYSKAN-IDX              >  100  )
                         OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                         OR    ( FLG-SET                 =   1   )
      *
                    MOVE  TBL-SYS1001(SYSKAN-IDX)  TO  SYS-1001-REC
                    IF   ( SYS-1001-STYUKYMD  <=  WRK-SYS-SRYYMD    )
                    AND  ( WRK-SYS-SRYYMD     <=  SYS-1001-EDYUKYMD )
                        MOVE     1       TO    FLG-SET
                    END-IF
      *
               END-PERFORM
               IF     FLG-SET  =  ZERO
                   STRING
                       "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                            SYS-2002-SRYYM     DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   GO       TO      20021-KOBETU-HENSYU-EXT
               END-IF
      *
      *        患者情報取得
               MOVE    SYS-1001-HOSPNUM    TO  PTINF-HOSPNUM
               MOVE    SYS-2002-PTID       TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
      *
      *        診療年月編集
               MOVE    SYS-2002-SRYYM      TO  WRK-STYMD (1:6)
               MOVE    "01"                TO  WRK-STYMD (7:2)
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY7-AREA
               MOVE    "71"                TO  LNK-DAY7-IRAI
               MOVE    SYS-2002-SRYYM      TO  LNK-DAY7-YM
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
               MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
      *
      *        処理中止設定処理
               PERFORM 500-CANCEL-HENSYU-SEC
      *
               IF      FLG-END             =   ZERO
      *            同日再入院情報取得
                   PERFORM 500-ORCSNYUINDAY-CALL-SEC
      *
      *            診療会計読み込み
                   MOVE    "1"                 TO  WRK-DBKBN 
      *      
                   INITIALIZE                  SRYACCT-REC
                   MOVE    SYS-1001-HOSPNUM    TO  ACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  ACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  ACCT-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  ACCT-NYUGAIKBN
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  WRK-SRYACCT-MCP-TABLE
                   IF      WRK-PARA-PRTKBN     =   "4" OR  "5" 
                       MOVE    "key61"         TO
                                               WRK-SRYACCT-MCP-PATHNAME
                   ELSE
                       MOVE    "key37"         TO
                                               WRK-SRYACCT-MCP-PATHNAME
                   END-IF
                   PERFORM 200122-SRYACCT-HENSYU-SEC
               END-IF 
      *
               IF      FLG-END             =   ZERO
      *            入院会計読み込み
                   MOVE    "2"                 TO  WRK-DBKBN 
      *
                   MOVE    LOW-VALUE           TO  KEY-AREA
      *     
                   INITIALIZE                  NYUINACCT-REC
                   MOVE    SYS-1001-HOSPNUM    TO  NACCT-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  NACCT-PTID
                   MOVE    SYS-2002-SRYYM      TO  NACCT-SRYYM
                   MOVE    NYUINACCT-REC       TO  MCPDATA-REC
                   MOVE    "tbl_nyuinacct"     TO
                                           WRK-NYUINACCT-MCP-TABLE
                   MOVE    "key14"             TO
                                           WRK-NYUINACCT-MCP-PATHNAME
                   MOVE    WRK-NYUINACCT-MCP-TABLE
                                               TO  MCP-TABLE
                   MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM   900-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    WRK-NYUINACCT-MCP-TABLE
                                                   TO  MCP-TABLE
                       MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                                   TO  MCP-PATHNAME
                       PERFORM 900-NYUINACCT-READ-SEC
                   ELSE
                       INITIALIZE                  SRYACCT-REC
                       MOVE    1                   TO  FLG-NYUINACCT
                   END-IF
      *
                   PERFORM UNTIL       FLG-NYUINACCT     =   1
                           OR          FLG-END           =   1
                       PERFORM 200121-NYUINACCT-HENSYU-SEC
                   END-PERFORM
      *
                   MOVE    WRK-NYUINACCT-MCP-TABLE
                                               TO  MCP-TABLE
                   MOVE    WRK-NYUINACCT-MCP-PATHNAME
                                               TO  MCP-PATHNAME
                   PERFORM 900-CLOSE-SEC
      *
      *            基本ファイル出力処理
                   PERFORM 200123-KIHON-WRITE-SEC
               END-IF
      ***  END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-SYSKANRI-READ-SEC
           .
       20021-KOBETU-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴より情報取得処理
      *****************************************************************
       240-PTNYUINRRK-SYORI-SEC                   SECTION.
      *
           INITIALIZE                      WRK-BYOTOKBN-TABLE
                                           WRK-SHONUM
           MOVE      SPACE             TO  WK-PTNYUINRRK-TENSTUYMD2
           MOVE      SPACE             TO  WK-PTNYUINRRK-TENNYUYMD
      *
      *    診療月内の入退院情報
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    WRK-RECE200-HOSPNUM TO  PTNYUINRRK-HOSPNUM
           MOVE    WRK-RECE200-PTID    TO  PTNYUINRRK-PTID
           MOVE    WRK-RECE200-SRYYM   TO  PTNYUINRRK-NYUINYMD (1:6)
           MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
           MOVE    WRK-RECE200-SRYYM   TO  PTNYUINRRK-TAIINYMD (1:6)
           MOVE    "01"                TO  PTNYUINRRK-TAIINYMD (7:2)
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK23
           END-IF
           PERFORM                 UNTIL   FLG-PTNYUINRRK23    =   1
             IF    ( PTNYUINRRK-TENNYUYMD  >   WRK-RECE200-TEKEDYMD-X )
             AND   ( WRK-RECE200-TEKEDYMD-X
                                       NOT =   SPACE              )
                 CONTINUE
             ELSE
               IF      PTNYUINRRK-NYUINYMD (1:6)
                                               <   WRK-RECE200-SRYYM
                   MOVE    1                   TO  IDX4
               ELSE
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX4
               END-IF 
               IF      PTNYUINRRK-TAIINYMD (1:6)
                                               >   WRK-RECE200-SRYYM
                   MOVE    31                  TO  IDX5
               ELSE
                   MOVE    PTNYUINRRK-TAIINYMD TO  WRK-SRYYMD
                   MOVE    WRK-SRYDD           TO  IDX5
               END-IF
      *        保険組合せのチェック（会計に対象の組合せがないときは全て）
               IF      WRK-RECE200-DAY-AREA    =   ALL "0"
                   MOVE    "1"                 TO
                                               WRK-RECE200-NYUINRYOFLG
                   PERFORM 2401-PTNYUINRRK-HENSYU-SEC
               ELSE
                   PERFORM VARYING IDX FROM    IDX4    BY  1
                           UNTIL   IDX >       IDX5
                       IF    ( IDX             =   IDX4 )
                       AND   ( WRK-RECE200-DOUJITSU-HKNCOMBI (IDX)
                                               =
                                               PTNYUINRRK-HKNCOMBINUM )
      *                    同日再入院のとき再入院分の保険組合せをチェック
                           IF      WRK-RECE200-DOUJITSU-HKNCOMBI (IDX)
                                               =   WRK-RECE200-HKNCOMBI
                               PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                               MOVE   IDX5                 TO  IDX
                           END-IF
                       ELSE
                           IF      WRK-RECE200-DAY (IDX)
                                               =   WRK-RECE200-HKNCOMBI
                               PERFORM 2401-PTNYUINRRK-HENSYU-SEC
                               MOVE   IDX5                 TO  IDX
                           END-IF
                       END-IF 
                   END-PERFORM
               END-IF
             END-IF          
      *                 
             PERFORM 900-PTNYUINRRK-KEY23-READ-SEC
           END-PERFORM        
      *                 
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    診療科情報が未編集のとき
           IF      WRK-RECE200-SRYKA (1)   =   SPACE
               MOVE    WRK-RECE200-LASTSRYKA
                                           TO  WRK-RECE200-SRYKA (1)
           END-IF
      *                                                                
      *    病棟区分並替え
      *                                                                
           PERFORM    VARYING   IDX3     FROM    1   BY  1             
                  UNTIL  ( IDX3                   >   4    )           
                    OR   ( WRK-BYOTOKBN(IDX3)     =  SPACE )           
             COMPUTE    IDX4  =  IDX3  +  1                            
             PERFORM  VARYING  IDX5  FROM  IDX4  BY  1                 
                        UNTIL  ( IDX5                >   4    )        
                          OR   ( WRK-BYOTOKBN(IDX5)  =  SPACE )        
                IF       WRK-BYOTOKBN(IDX3)  >                         
                                           WRK-BYOTOKBN(IDX5)          
                   MOVE  WRK-BYOTOKBN(IDX3) TO  WRK-BYOTOKBN-DATA      
                   MOVE  WRK-BYOTOKBN(IDX5) TO  WRK-BYOTOKBN(IDX3)     
                   MOVE  WRK-BYOTOKBN-DATA  TO  WRK-BYOTOKBN(IDX5)     
                END-IF                                                 
             END-PERFORM                                               
           END-PERFORM                                                 
      *
           MOVE    ZERO                    TO  IDX4                                                                
           PERFORM VARYING IDX3    FROM    1   BY  1             
                   UNTIL   IDX3    >       4
               EVALUATE    WRK-BYOTOKBN(IDX3)
                   WHEN    "01"                                            
                   WHEN    "02"                                            
                   WHEN    "07"
                       ADD     1       TO  IDX4                                           
                       MOVE    WRK-BYOTOKBN (IDX3)
                                       TO  WRK-RECE200-BYOTOKBN (IDX4)  
                END-EVALUATE
           END-PERFORM                                                 
      *                                                                
           .
       240-PTNYUINRRK-SYORI-EXT.
           EXIT.
      * 
      *****************************************************************
      *    入退院履歴情報編集処理
      *****************************************************************
       2401-PTNYUINRRK-HENSYU-SEC           SECTION.
      *
      *    病棟区分セット
           PERFORM    24011-BYOTOKBN-SET-SEC
      *
      *    診療科情報のセット
           PERFORM VARYING IDW FROM    1   BY  1
                   UNTIL   IDW >       20
                   OR      PTNYUINRRK-NYUINKA
                                           =   WRK-RECE200-SRYKA (IDW)
               IF      WRK-RECE200-SRYKA (IDW)  =   SPACE  
                   IF      WRK-RECE200-HKNCOMBI =
                                               PTNYUINRRK-HKNCOMBINUM
                       MOVE    PTNYUINRRK-NYUINKA  
                                           TO  WRK-RECE200-SRYKA (IDW)
                   END-IF
                   MOVE    20          TO  IDW
               END-IF
           END-PERFORM
      *
      *    入院年月日・最終科
           IF      WRK-RECE200-LASTSRYKA   =   SPACE
               MOVE    PTNYUINRRK-NYUINYMD TO  WRK-RECE200-NYUINYMD
               MOVE    PTNYUINRRK-NYUINKA  TO  WRK-RECE200-LASTSRYKA
               MOVE    PTNYUINRRK-BTUNUM   TO  WRK-RECE200-LASTBTUNUM
               MOVE    PTNYUINRRK-BRMNUM   TO  WRK-RECE200-LASTBRMNUM
               MOVE    PTNYUINRRK-SHONUM   TO  WRK-SHONUM
           END-IF
      *
      *    入退院履歴情報
           PERFORM 24012-RECE250-WRITE-SEC
      *
      *    診療月内の入退院情報の履歴
           IF      FLG-RECE250          =   1                            
               MOVE    PTNYUINRRK-REC      TO  WRK-PTNRRK-REC
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    WRK-RECE200-HOSPNUM TO  PTNYUINRRK-HOSPNUM
               MOVE    WRK-RECE200-PTID    TO  PTNYUINRRK-PTID
               MOVE    WRK-PTNRRK-SHONUM   TO  PTNYUINRRK-SHONUM
               MOVE    WRK-RECE200-SRYYM   TO  PTNYUINRRK-NYUINYMD (1:6)
               MOVE    "31"                TO  PTNYUINRRK-NYUINYMD (7:2)
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTNYUINRRK22 
               END-IF
      *
      *        入院年月日
               IF    ( PTNYUINRRK-NYUINYMD <   WRK-RECE200-NYUINYMD )
               AND   ( PTNYUINRRK-SHONUM   =   WRK-SHONUM           )
                   MOVE    PTNYUINRRK-NYUINYMD TO  WRK-RECE200-NYUINYMD
      *
                   IF    ( PTNYUINRRK-JTIKBN        =   "5" )
                   AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
                       MOVE    "1"              TO  WRK-RECE200-TAINFLG
                   END-IF
               END-IF
      *
               PERFORM             UNTIL   FLG-PTNYUINRRK22 =  1
                   PERFORM 24012-RECE250-WRITE-SEC
                   PERFORM 900-PTNYUINRRK-KEY22-READ-SEC
               END-PERFORM
      *                 
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key51"             TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       2401-PTNYUINRRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病棟区分セット                                              
      *****************************************************************
       24011-BYOTOKBN-SET-SEC                SECTION.
      *
      *    異動による転室等の場合は転出日を前日に置き換える
           MOVE    SPACE               TO  WK-PTNYUINRRK-TENSTUYMD
           IF      PTNYUINRRK-TENSTUYMD
                                       =  PTNYUINRRK-TAIINYMD
               MOVE  PTNYUINRRK-TENSTUYMD TO  WK-PTNYUINRRK-TENSTUYMD
      *
               IF  ( WK-PTNYUINRRK-TENSTUYMD2     NOT =  SPACE   )
               AND ( WK-PTNYUINRRK-TENSTUYMD(1:6) NOT = "999999" )
               AND ( WK-PTNYUINRRK-TENSTUYMD  =
                                           WK-PTNYUINRRK-TENSTUYMD2)
                   INITIALIZE                    STS-AREA-DAY
                   INITIALIZE                    LNK-DAY6-AREA
                   MOVE    "61"              TO  LNK-DAY6-IRAI
                   MOVE   PTNYUINRRK-TENSTUYMD
                                             TO  LNK-DAY6-KIJUN
                   MOVE    "1"               TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1                TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"           USING   STS-AREA-DAY
                                                     LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN TO WK-PTNYUINRRK-TENSTUYMD
               END-IF
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"            TO  LNK-DAY6-IRAI
               MOVE   PTNYUINRRK-TENSTUYMD
                                       TO  LNK-DAY6-KIJUN
               MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
               MOVE    -1              TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"         USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN TO WK-PTNYUINRRK-TENSTUYMD
           END-IF
      *
      *    病棟情報                                           
           INITIALIZE                      SYS-5001-REC     
           MOVE    "5001"              TO  SYS-5001-KANRICD      
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD        
           MOVE    PTNYUINRRK-TENNYUYMD
                                       TO  SYS-5001-STYUKYMD
           MOVE    SYS-STYUKYMD        TO  SYS-5001-EDYUKYMD        
           MOVE    SYS-5001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO               
               MOVE  MCPDATA-REC           TO  SYS-5001-REC
      *
               IF    ( WK-PTNYUINRRK-TENSTUYMD (1:6)
                                           <   WRK-RECE200-SRYYM )
               OR    ( PTNYUINRRK-TENNYUYMD (1:6)
                                           >   WRK-RECE200-SRYYM )
               OR    ((PTNYUINRRK-TENNYUYMD
                                           =
                                       WK-PTNYUINRRK-TENNYUYMD) AND
                      (WK-PTNYUINRRK-TENNYUYMD
                                       NOT =   SPACE       )        )
                   CONTINUE
               ELSE 
                   IF      SYS-5001-RECEDSPKBN =  SPACE
                       CONTINUE
                   ELSE
                       PERFORM VARYING IDX3    FROM    1   BY  1
                               UNTIL   IDX3    >       4
                               OR      WRK-BYOTOKBN (IDX3)
                                               =   SYS-5001-RECEDSPKBN
                           IF      WRK-BYOTOKBN (IDX3) =   SPACE
                               MOVE    SYS-5001-RECEDSPKBN
                                               TO   WRK-BYOTOKBN (IDX3)
                               MOVE    4           TO  IDX3
                           END-IF
                       END-PERFORM
                  END-IF
                  MOVE   WK-PTNYUINRRK-TENSTUYMD  TO
                                            WK-PTNYUINRRK-TENSTUYMD2
                  MOVE   PTNYUINRRK-TENNYUYMD     TO
                                            WK-PTNYUINRRK-TENNYUYMD
               END-IF
      *
      *        病棟番号・種別
               MOVE    WRK-RECE200-SRYYM        TO  WRK-SRYYYMM
               PERFORM VARYING IDX3    FROM    1   BY  1
                       UNTIL   IDX3    >       31
                   IF      WRK-RECE200-BYOTONUM(IDX3)   =  SPACE
                       MOVE    IDX3            TO  WRK-SRYDD
                       IF       PTNYUINRRK-TENNYUYMD  <=  WRK-SRYYMD
                       AND      PTNYUINRRK-TENSTUYMD  >=  WRK-SRYYMD
                           MOVE  SYS-5001-BTU-NUM
                                               TO
                                         WRK-RECE200-BYOTONUM    (IDX3)
                           MOVE  SYS-5001-BTU-SBT
                                               TO
                                         WRK-RECE200-BYOTOSYUBETU(IDX3)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .                                                           
       24011-BYOTOKBN-SET-EXT.                   
           EXIT.                                                       
      *
      *****************************************************************
      *    入退院履歴出力処理
      *****************************************************************
       24012-RECE250-WRITE-SEC         SECTION.
      *
      *    他院歴の履歴は記載しない
           IF    ( PTNYUINRRK-JTIKBN        =   "5" )
           AND   ( PTNYUINRRK-TAINRELKBN    =   "1" )
               GO  TO  24012-RECE250-WRITE-EXT
           END-IF
      *
           MOVE    WRK-RECE200-KEY     TO  RECE250-KEY1
           MOVE    PTNYUINRRK-NYUINYMD TO  RECE250-NYUINYMD
           IF      PTNYUINRRK-NYUINYMD =   PTNYUINRRK-TAIINYMD
               MOVE    0                   TO  RECE250-NYUINYMD-SEQNO
           ELSE 
               IF      PTNYUINRRK-TAIINYMD =   "99999999"
                   MOVE    9                   TO RECE250-NYUINYMD-SEQNO
               ELSE
                   MOVE    1                   TO RECE250-NYUINYMD-SEQNO
               END-IF
           END-IF
           MOVE    RECE250-REC         TO  WRK-RECE250-REC          
           PERFORM 900-RECE250-READ-SEC
           IF      FLG-RECE250         =   1
               MOVE    WRK-RECE250-REC     TO  RECE250-REC 
               MOVE    PTNYUINRRK-TAIINYMD TO  RECE250-TAIINYMD
               MOVE    PTNYUINRRK-NYUINCHUKBN
                                           TO  RECE250-NYUINCHUKBN
               WRITE   RECE250-REC
      *
               IF      STS-RECE250         =   "00"
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  WRK-ERRMSG-AREA
                   INITIALIZE                      WRK-ERRMSG-AREA
      *
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    RECE250PARA         TO  WRK-ERR-FILE-NM
                   MOVE    STS-RECE250         TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
      *
               ADD     1                   TO  CNT-RECE250
           END-IF    
           .
       24012-RECE250-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *            ジョブ管理終了処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM   900-CALL-ORCSJOB-SEC
               END-IF
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
       500-COBABORT-SEC                SECTION.
      *
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルエラー処理
      *****************************************************************
       500-FILE-ERR-ABORT-SEC           SECTION.
      *
           EVALUATE    WRK-ERR-FLG
               WHEN    1
                   MOVE    "ファイルＯＰＥＮエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file open err" TO  WRK-ERR-FILE-MSG1
               WHEN    2
                   MOVE    "ファイル読み込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file read err" TO  WRK-ERR-FILE-MSG1
               WHEN    3
                   MOVE    "ファイル書き込みエラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file write err"
                                           TO  WRK-ERR-FILE-MSG1
               WHEN    4
                   MOVE    "ファイル更新エラー　FILE="
                                           TO  WRK-ERR-FILE-MSG
                   MOVE    "file rewrite err"
                                           TO  WRK-ERR-FILE-MSG1
           END-EVALUATE
      *
           STRING  WRK-ERR-FILE-MSG        DELIMITED   BY  SPACE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   " STS="                 DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
           END-STRING
           PERFORM 500-ERR-HENSYU-SEC
      *
           STRING  "ORCR1200 "             DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
      *
           .
       500-FILE-ERR-ABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               CONTINUE
           ELSE
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
               IF    ( SJOBKANRI-RETURN
                                       =   ZERO )
               AND   ( JOB-STOPFLG     =   1    )
                   MOVE    "処理が中止されました"
                                           TO  WRK-RECEERR
      *
                   OPEN    INPUT   RECEERR-FILE
                   IF      STS-RECEERR         =   ZERO
                       CLOSE   RECEERR-FILE
                   ELSE
                       OPEN    OUTPUT              RECEERR-FILE
      *    
                       MOVE    WRK-RECEERR         TO  RECEERR-REC
                       WRITE   RECEERR-REC
                       CLOSE   RECEERR-FILE
      *         
      *                ジョブ管理終了処理
                       MOVE    "CAN"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    WRK-RECEERR     TO  JOB-YOBI
                       MOVE    "8888"          TO  JOB-ERRCD
                       MOVE    2               TO  JOB-STOPFLG
                       PERFORM   900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE200-FILE
                   RECE250-FILE
      *
           IF      CNT-RECE200            =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               PERFORM   900-CALL-ORCSJOB-SEC
      *
               PERFORM 900-DBDISCONNECT-SEC
           END-IF
      *     
           DISPLAY "RECE200  CNT-OUT :" CNT-RECE200
           DISPLAY "*** ORCR1200 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院情報取得処理
      *****************************************************************
       500-ORCSNYUINDAY-CALL-SEC      SECTION.
      *
           INITIALIZE                      ORCSNYUINDAY-AREA
           MOVE    SYS-1001-HOSPNUM    TO  LNK-NYUINDAY-HOSPNUM
           EVALUATE    WRK-PARA-SYOKBN
               WHEN    "1"
                   MOVE    SYS-2001-SRYYM  TO  LNK-NYUINDAY-SANTEIYM
                   MOVE    KEY-N-PTID      TO  LNK-NYUINDAY-PTID
               WHEN    "2"
                   MOVE    SYS-2002-SRYYM  TO  LNK-NYUINDAY-SANTEIYM
                   MOVE    SYS-2002-PTID   TO  LNK-NYUINDAY-PTID
           END-EVALUATE
           CALL    "ORCSNYUINDAY"      USING   ORCSNYUINDAY-AREA
                                               SPA-AREA
           IF      LNK-NYUINDAY-RC >   0
               DISPLAY "LNK-NYUINDAY-RC=" LNK-NYUINDAY-RC
           END-IF
           .
       500-ORCSNYUINDAY-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再入院チェック処理
      *****************************************************************
       500-ORCSNYUINDAY-CHECK-SEC          SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
             IF      LNK-NYUINDAY-DOUJITSU-KBN (IDXX)
                                           =   "1"
      *
               PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL   IDX1    >   10
      *
      *            保険組合せ追加
                   IF      WRK-COMB-HKNCOMBINUM (IDX1) =   ZERO
                       INITIALIZE                      HKNCOMBI-REC
                       MOVE    SYS-1001-HOSPNUM    TO  COMB-HOSPNUM
                       MOVE    KEY-O-PTID          TO  COMB-PTID
                       MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                                   TO  COMB-HKNCOMBINUM 
                       MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
                       PERFORM  910-HKNCOMBI-INV-SEC
      *                （対象外の組合せは１をセット）
                       IF      FLG-HKNCOMBI    =   ZERO
                           MOVE    HKNCOMBI-REC    TO
                                           WRK-HKNCOMBI-REC  (IDX1)
      *                    医保以外読み飛ばし
                           IF      COMB-HKNNUM     =
                                               WRK-CONS-KOUGAI-HKNNUM
                               CONTINUE
                           ELSE
                               MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                           END-IF
                       ELSE
                           IF      COMB-DELKBN         =   "1"
                               MOVE    HKNCOMBI-REC    TO
                                               WRK-HKNCOMBI-REC  (IDX1)
      *                        医保以外読み飛ばし
                               IF      COMB-HKNNUM     =
                                               WRK-CONS-KOUGAI-HKNNUM
                                   MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                               ELSE
                                   MOVE    "1"         TO
                                           WRK-HKNCOMBI-SYOKBN (IDX1)
                               END-IF
                           ELSE
                               MOVE    LNK-NYUINDAY-DOUJITSU-HKNCOMBI
                                                                (IDXX)
                                                   TO
                                           WRK-COMB-HKNCOMBINUM (IDX1)
                               MOVE    "2"             TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                           END-IF
                       END-IF
                   END-IF
      *
      *            適用範囲かチェック（範囲外のとき２をセット）
                   IF      LNK-NYUINDAY-DOUJITSU-HKNCOMBI (IDXX)
                                           =
                                       WRK-COMB-HKNCOMBINUM (IDX1)
                       IF      WRK-HKNCOMBI-SYOKBN (IDX1)
                                           NOT =   SPACE
                           CONTINUE                    
                       ELSE     
                           MOVE    WRK-STYMD       TO  WRK-YMD
                           MOVE    IDXX            TO  WRK-DD
                           IF      WRK-COMB-TEKSTYMD (IDX1)
                                                   <=  WRK-YMD
                           AND     WRK-COMB-TEKEDYMD (IDX1)
                                                   >=  WRK-YMD
                               CONTINUE
                           ELSE   
                               MOVE    "2"         TO
                                           WRK-HKNCOMBI-SYOKBN  (IDX1)
                               MOVE    IDXX        TO
                                           WRK-HKNCOMBI-ERR-DAY (IDX1)
                           END-IF
                       END-IF
                       MOVE    10              TO  IDX1
                   END-IF            
               END-PERFORM
             END-IF
           END-PERFORM
      *
           .
       500-ORCSNYUINDAY-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求フラグチェック処理
      *****************************************************************
       800-MISEIKYU-CHECK-SEC          SECTION.
      *
           INITIALIZE                      SYUKA-REC
           MOVE    SYS-1001-HOSPNUM    TO  SYUKA-HOSPNUM
           MOVE    WRK-SYUKA-PTID      TO  SYUKA-PTID
           MOVE    WRK-SYUKA-SRYYM     TO  SYUKA-SRYYM
           MOVE    "1"                 TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"              TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           MOVE    "tbl_syuka"         TO  WRK-MCP-TABLE
           MOVE    "key15"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           .
       800-MISEIKYU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           MOVE    SGETTEMP-TEMPDIR    TO  TEMPFLPARA
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  TEMPFLPARA (SGETTEMP-ST:)
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
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
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ　ＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SRYACCT
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           IF      FLG-SRYACCT         =   ZERO
               MOVE    ACCT-PTID           TO  KEY-N1-PTID 
               MOVE    ACCT-HKNCOMBI       TO  KEY-N1-HKNCOMBI
               MOVE    ACCT-SRYKA          TO  KEY-N1-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院会計マスタＲＥＡＤ
      *****************************************************************
       900-NYUINACCT-READ-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-NYUINACCT
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           IF      FLG-NYUINACCT       =   ZERO
               MOVE    NACCT-PTID          TO  KEY-N-PTID 
      *********MOVE    NACCT-HKNCOMBI      TO  KEY-N-HKNCOMBI
      *********MOVE    NACCT-SRYKA         TO  KEY-N-SRYKA
               MOVE    SPACE               TO  KEY-N-SRYKA
      *
               ADD     1                   TO  CNT-IN-CANCEL
               IF      CNT-IN-CANCEL       =   50
                   MOVE    ZERO                TO  CNT-IN-CANCEL
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
           END-IF
           .
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスタ読込
      *****************************************************************
       910-HKNCOMBI-INV-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNCOMBI
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   IF      COMB-DELKBN         =   "1"
                       MOVE    1                   TO  FLG-HKNCOMBI
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-HKNCOMBI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報マスタ読込
      *****************************************************************
       910-PTHKNINF-INV-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PTHKNINF
                   MOVE    MCPDATA-REC         TO  PTHKNINF-REC
                   IF      PTHKN-SAKJOKBN      =   "1"
                       MOVE    1                   TO  FLG-PTHKNINF
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-PTHKNINF-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *  
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
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
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       900-KYUSEIRRK-READ-SEC          SECTION.
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  KYUSEIRRK-REC
                   MOVE    ZERO                TO  FLG-KYUSEIRRK
               ELSE
                   MOVE    1                   TO  FLG-KYUSEIRRK
               END-IF
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KYUSEIRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
           PERFORM    900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM    900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM    900-DBFETCH-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    ZERO                 TO  FLG-SYUKA
                   MOVE    MCPDATA-REC          TO  SYUKA-REC
               ELSE
                   MOVE    1                    TO  FLG-SYUKA
                   INITIALIZE                       SYUKA-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２２）
      *****************************************************************
       900-PTNYUINRRK-KEY22-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key51"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK22
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
               IF      PTNYUINRRK-JTIKBN    =   "5"
                   IF      PTNYUINRRK-TAINRELKBN    =   "1"
                       CONTINUE
                   ELSE
                       GO  TO  900-PTNYUINRRK-KEY22-READ-SEC
                   END-IF
               END-IF
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK22
           END-IF
           .
       900-PTNYUINRRK-KEY22-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴読込（ＫＥＹ２３）
      *****************************************************************
       900-PTNYUINRRK-KEY23-READ-SEC    SECTION.
      *    
           MOVE    "tbl_ptnyuinrrk"     TO  MCP-TABLE
           MOVE    "key23"              TO  MCP-PATHNAME
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-PTNYUINRRK23
               MOVE    MCPDATA-REC          TO  PTNYUINRRK-REC
           ELSE
               MOVE    1                    TO  FLG-PTNYUINRRK23
           END-IF
           .
       900-PTNYUINRRK-KEY23-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴読込
      *****************************************************************
       900-RECE250-READ-SEC            SECTION.
      *
           READ    RECE250-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE250
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE250
           END-READ
           .
       900-RECE250-READ-EXT.
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
               MOVE    SPACE               TO  WRK-ERRMSG-AREA
               INITIALIZE                      WRK-ERRMSG-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ読み込みエラー　TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCR1200 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
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
      *    クローズ処理
      *****************************************************************
       900-CLOSE-SEC                  SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"     USING   MCPAREA
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
           PERFORM    900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
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
           PERFORM    900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM    900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.
